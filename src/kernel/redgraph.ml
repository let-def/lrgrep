(* MIT License
 *
 * Copyright (c) 2025 Frédéric Bour
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 *
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *)

(** Reduction graph construction and analysis

    This module builds and manipulates a graph of viable reductions in an LR(1)
    parser. The reduction graph represents the structure of possible reductions
    as paths through the automaton, enabling efficient lookahead analysis and
    priority computation.

    Proceeds in three steps:
    - Compute the closure ϵ-reductions (reductions that do not consume any
      input token) for each LR(1) state.
      This analysis is local (it does not depend on the stack, only on the
      LR(1) state), and forms a tree of possible sequences of ϵ-reductions,
      ending with optional "pending", non-ϵ, reductions that need to consume
      states from the stack to proceed.
      This closure is represented by stack_tree's and reduction_closure's, and
      simplifies and speeds up later analyses.

    - Build a target trie that maps reduction targets (e.g., a nonterminal to
      reduce) to the goto transitions where they can occur, enabling reverse
      lookup from user-specified patterns to graph nodes.

    - Construct a graph whose edges are labelled by LR(1) states and which map
      an LR(1) stack suffix to the (sequences of) reductions applicable to
      this configuration.
      The paths of this graph enumerate all the stack suffixes that can be
      consumed by reducing. The process is repeated as long as a reduction is
      applicable, thus a right recursion [A → α A] translates to a cycle.
      The process also keeps track of lookahead symbols permitting each
      reduction to strictly simulate the behavior of an LR(1) automaton that
      possibly went through conflict resolution.

    But to recognize a reduction pattern, we have to do the reverse mapping:
    the user provides the target of a reduction (e.g. I want to reduce an
    expression), and we need to find the paths that can reach this target.
    So we introduce a "target" abstraction to which a reduction pattern
    translates to, a reverse index [target_trie] to go from a pattern to a
    set of targets, and we associate to each node of the graph the reachable
    targets.

    Architecture:

    - The graph nodes ("cells") represent configurations of (LR state, reduction
      position, lookahead set). These are the vertices of the reduction graph.

    - Edges represent transitions: moving from one reduction position to another
      via goto transitions.

    - The graph is minimization-aware: Valmari's algorithm is used to minimize
      the graph while preserving the reachability structure needed for
      computation of minimal costs.

    Key data structures:

    - 'g stack_tree: Represents the tree of possible reduction stacks for a
      given LR state. Each node contains:
      - [next]: Subtrees reachable after performing a reduction
      - [reductions]: Pending non ϵ-reductions at each node, grouped by depth

    - 'g reduction_closure: Complete ϵ-reductions information for an LR state
      - [accepting], [failing]: Lookaheads that cause acceptance/failure
      - [stacks]: Stack trees of ϵ-reductions
      - [all_stacks], [all_reductions]: flattened ϵ-stacks and ϵ-reductions

    - 'g target_trie: Trie for indexing reduction targets reached by sequences of LR(1) states.
      E.g. if there is a goto transition `s0 -> s1` labelled `expression`, there will be
        a path `s0 -> s1` labelled `expression target` in the trie.
      - [sub]: Child nodes for each LR state
      - [immediates]: States from which the reductions are immediate (ϵ-reductions by definition)
      - [targets]: Targets reached by the current prefix.

    - 'g graph: The minimized reduction graph, where each cell contains the
      reductions applicable at that position, and each step contains the
      transitions from that cell.

    Tricky implementation details:

    - The reduction graph is used to compute lookahead-dependent reduction
      sequences. Each cell represents either a (state, lookahead) configuration,
      or an intermediate step in a reduction sequence given by a triple (state,
      depth, lookahead) (a non-deterministic transition which applies if
      `state` is `depth` states deep in the stack).

    - The [group_reductions] function groups items being reduced by their depth
      in the stack, enabling efficient processing of nested reductions.

    - The [index_targets] function creates a trie where each path corresponds
      to a sequence of goto transitions leading to a target state. The trie
      nodes mark "immediate" targets (directly reachable via reductions) and
      track transitions via goto.

    - The reduction graph construction uses a stream-based approach for
      accessing predecessors, implemented via [get_stream] to avoid
      recomputing them.

    - The minimization via Valmari's algorithm preserves the reachability
      structure needed for cost computation while reducing state space.

    - The [step] type represents positions in the reduction graph, and
      [cells_steps] maps each cell to its step index for efficient cost
      computation.

    - The [filter_reductions] function updates reduction lookahead sets when
      the lookahead domain is restricted to preserve LR(1) behaviors.

    - The [follow] function returns either an [Advance] (move to next step)
      or a [Switch] (transition to different goto targets), enabling the
      parser to navigate the reduction graph.
*)

open Fix.Indexing
open Utils
open Misc
open Info

(*let printf_debug = false*)

(* Merge reduction steps: combine reductions at the same depth, recursing on deeper levels. *)
let rec merge_reduction_step map acc = function
  | [] -> (map, acc)
  | [] :: rrs ->
    merge_reduction_step map acc rrs
  | (r :: rs) :: rrs ->
    let acc = if list_is_empty rs then acc else rs :: acc in
    let augment _ a b = Some (IndexSet.union a b) in
    let map = IndexMap.union augment r map in
    merge_reduction_step map acc rrs

(* Recursively merge all reduction depth layers, dropping empty ones. *)
let rec merge_reductions = function
  | [] -> []
  | rrs ->
    let r, rrs' = merge_reduction_step IndexMap.empty [] rrs in
    match merge_reductions rrs' with
    | [] when IndexMap.is_empty r -> []
    | rs -> r :: rs

(* Step 1: pre-compute closure of ϵ-reductions *)

(* Group items being reduced by their depth (reductions with zero, one, two producers, ...). *)
let group_reductions g = function
  | [] -> []
  | items ->
    let rec group depth acc = function
      | [] -> [acc]
      | (it, la) :: rest when depth = Item.position g it ->
        let lhs = Production.lhs g (Item.production g it) in
        group depth (IndexMap.update lhs (union_update la) acc) rest
      | otherwise ->
        acc :: group (depth + 1) IndexMap.empty otherwise
    in
    let compare_items (it1, _) (it2, _) =
      Int.compare (Item.position g it1) (Item.position g it2)
    in
    group 0 IndexMap.empty (List.sort compare_items items)

(* Verify that a list of reduction maps is non-empty at the deepest level. *)
let rec validate = function
  | [] -> true
  | [x] -> not (IndexMap.is_empty x)
  | _ :: xs -> validate xs

type 'g stack_tree = {
  next: ('g lr1 index list * 'g terminal indexset * 'g stack_tree) list;
  reductions: ('g nonterminal, 'g terminal indexset) indexmap list;
}

(* Fold over all reduction maps in a stack tree, traversing all branches. *)
let fold_stack_reductions f stacks acc =
  let rec aux acc {next; reductions} =
    let acc = f reductions acc in
    List.fold_left aux_next acc next
  and aux_next acc (_, _, stacks') =
    aux acc stacks'
  in
  aux acc stacks

type 'g reduction_closure = {
  accepting: 'g terminal indexset;
  failing: 'g terminal indexset;
  stacks: 'g stack_tree;
  all_stacks: ('g lr1 index list * 'g terminal indexset) list;
  all_reductions: ('g nonterminal, 'g terminal indexset) indexmap list;
}

type ('g, 'n) reduction_closures = ('n, 'g reduction_closure) vector

(* Add the intersection of [set] and [la] to the reference [r]. *)
let add_subset g r set la =
  r := IndexSet.union (Terminal.intersect g set la) !r

(* Close ϵ-reductions of each LR(1) states *)
let close_lr1_reductions (type g) (g : g grammar) : (g lr1, g reduction_closure) vector =
  Vector.init (Lr1.cardinal g) @@ fun lr1 ->
  let accepting = ref IndexSet.empty in
  let failing = ref IndexSet.empty in
  let group_stacks (items, next) =
    let reductions = group_reductions g items in
    assert (validate reductions);
    {reductions; next}
  in
  let rec pop lookahead acc (item : g item index) = function
    | [] -> ((item, lookahead) :: fst acc, snd acc)
    | hd :: tl as stack ->
      match Item.prev g item with
      | Some item' -> pop lookahead acc item' tl
      | None ->
        let lhs = Production.lhs g (Item.production g item) in
        let stack = Transition.find_goto_target g hd lhs :: stack in
        let stacks = group_stacks (reduce lookahead ([],[]) stack) in
        (fst acc, (stack, lookahead, stacks) :: snd acc)
  and reduce lookahead acc stack =
    let lr1 = List.hd stack in
    add_subset g failing (Lr1.reject g lr1) lookahead;
    add_subset g accepting (Lr1.shift_on g lr1) lookahead;
    IndexSet.fold begin fun red acc ->
      match Terminal.intersect g (Reduction.lookaheads g red) lookahead with
      | la when IndexSet.is_empty la -> acc
      | la ->
        pop la acc (Item.last g (Reduction.production g red)) stack
    end (Reduction.from_lr1 g lr1) acc
  in
  let stacks = group_stacks (reduce (Terminal.all g) ([],[]) [lr1]) in
  let failing = !failing in
  let accepting = !accepting in
  let rec all_stacks la acc {next; _} =
    List.fold_left (fun acc (stack,la',stacks) ->
        let la = IndexSet.inter la la' in
        if IndexSet.is_empty la then acc else
          all_stacks la ((stack, la) :: acc) stacks
      ) acc next
  in
  let all_stacks = all_stacks (Terminal.all g) [([lr1],Terminal.all g)] stacks in
  let all_reductions =
    merge_reductions (fold_stack_reductions List.cons stacks [])
  in
  assert (validate all_reductions);
  {accepting; failing; stacks; all_stacks; all_reductions}

(* Filter reduction lookahead sets to a restricted domain [la].
   Preserves sharing when no filtering is needed. *)
let rec filter_reductions g la = function
  | [] -> []
  | r :: rs as rrs ->
    let filtered = ref false in
    let r' =
      IndexMap.filter_map (fun _ la' ->
          let la'' = Terminal.intersect g la la' in
          if la' != la'' then filtered := true;
          if IndexSet.is_empty la'' then None else Some la''
        ) r
    in
    let rs' = filter_reductions g la rs in
    if rs == rs' && not !filtered
    then rrs
    else r' :: rs'

(* Reduction targets indexation *)

module Target = Unsafe_cardinal()
type 'g target = 'g Target.t
type 'g targets = ('g target, 'g terminal indexset) indexmap

type 'g target_trie = {
  mutable sub: ('g lr1, 'g target_trie) indexmap;
  mutable immediates: 'g lr1 indexset;
  mutable targets: ('g lr1, 'g target index) indexmap;
}

let index_targets (type g) (g : g grammar) rc
  : g target_trie * (g goto_transition, g targets) vector
  =
  (* Index sources of goto transitions *)
  let goto_sources = Vector.make (Lr1.cardinal g) IndexSet.empty in
  Index.rev_iter (Transition.goto g) begin fun gt ->
    let tr = (Transition.of_goto g gt) in
    goto_sources.@(Transition.target g tr) <- IndexSet.add gt
  end;
  (* Allocate target identifiers *)
  let module Gen = Gensym() in
  let open Target.Eq(struct
      type t = g
      include Gen
    end) in
  let Refl = eq in
  (* Targets by goto transition *)
  let by_goto = Vector.make (Transition.goto g) IndexMap.empty in
  (* Manage trie nodes *)
  let fresh_node () = {
    sub = IndexMap.empty;
    immediates = IndexSet.empty;
    targets = IndexMap.empty;
  } in
  let get_child (node, lr1) =
    match IndexMap.find_opt lr1 node.sub with
    | Some node' -> node'
    | None ->
      let node' = fresh_node () in
      node.sub <- IndexMap.add lr1 node' node.sub;
      node'
  in
  let root = fresh_node () in
  root.immediates <- Lr1.all g;
  let rec follow_path = function
    | [] -> assert false
    | [lr1] -> (root, lr1)
    | lr1 :: path -> (get_child (follow_path path), lr1)
  in
  (* Construct target trie *)
  Index.rev_iter (Lr1.cardinal g) begin fun tgt ->
    (* For each LR(1), there are three sources of reduction targets:
       - stacks directly reachable from this state,
         these are marked as "immediate" in the trie
       - goto transitions reaching this target (found using the goto_sources)
       - composition of both
    *)
    let rec visit_stacks acc {next; reductions=_} =
      List.fold_left begin fun acc (stack, la, sub') ->
        let acc = (follow_path (List.rev stack), la) :: acc in
        visit_stacks acc sub'
      end acc next
    in
    let roots = visit_stacks [] rc.:(tgt).stacks in
    (* 1. Register immediates *)
    List.iter
      (fun ((node, lr1), _) ->
         node.immediates <- IndexSet.add lr1 node.immediates)
      roots;
    (* Goto sources *)
    let sources = goto_sources.:(tgt) in
    if IndexSet.is_not_empty sources then
      (* Prepend all goto transitions (by construction, rc stacks already end with tgt) *)
      let roots =
        (get_child (root, tgt), Terminal.all g) ::
        List.map (fun (root, la) -> (get_child root, la)) roots
      in
      List.iter begin fun (root, la) ->
        IndexSet.iter begin fun gt ->
          let src = Transition.source g (Transition.of_goto g gt) in
          let index = match IndexMap.find_opt src root.targets with
            | Some index -> index
            | None ->
              let index = Gen.fresh () in
              root.targets <- IndexMap.add src index root.targets;
              index
          in
          by_goto.@(gt) <- IndexMap.add index la
        end sources;
      end roots
  end;
  stopwatch 2 "indexed %d targets" (cardinal Gen.n);
  (* Done *)
  (root, by_goto)


(* Graph construction *)

module Step = Unsafe_cardinal()
type 'g step = 'g Step.t

(* Stream accessor for lazy predecessor lists.
   Materializes the stream up to index [i] on demand. *)
let get_stream ?(initial=0) stream =
  let s = ref stream in
  let d = ref initial in
  fun i ->
    assert (i >= !d);
    while i > !d do
      s := Lazy.force (!s).lnext;
      incr d;
    done;
    (!s).lvalue

type 'g transition = {
  reached: 'g target indexset;
  reachable: 'g target indexset;
  step: 'g step index;
}

type 'g graph = ('g step, ('g lr1, 'g transition list) indexmap) vector

let make (type g)
    (g : g grammar)
    (rc : (g, g lr1) reduction_closures)
    (targets : (g goto_transition, g targets) vector)
  : g graph =
  let open IndexBuffer in
  let module Cells = Gensym() in
  let module Links = Gen.Make() in
  let cells : (Cells.n, g lr1 indexset) Dyn.t = Dyn.make IndexSet.empty in
  let open struct type label = g lr1 index * g target indexset * int * Cells.n index * Cells.n index * g lr1 indexset end in
  let links : (Links.n, label) Gen.t = Links.get_generator () in
  let table = Vector.make (Nonterminal.cardinal g) IndexSet.Map.empty in
  let get_cell nt la =
    let map0 = table.:(nt) in
    match IndexSet.Map.find_opt la map0 with
    | Some index -> index
    | None ->
      let index = Cells.fresh () in
      table.:(nt) <- IndexSet.Map.add la index map0;
      index
  in
  let initial = Cells.fresh () in
  let sink = Cells.fresh () in
  let rec explore_cell cell nt la src =
    let gt = Transition.find_goto g src nt in
    let reached =
      IndexMap.deflate targets.:(gt)
        (fun _ la' -> not (IndexSet.disjoint la la'));
    in
    let predecessors = get_stream (Lr1.predecessors g src) in
    let tgt = Transition.target g (Transition.of_goto g gt) in
    explore_transitions cell src reached la predecessors
      rc.:(tgt).all_reductions

  and explore_transitions cell0 src reached la0 predecessors reductions =
    let result = ref [] in
    List.iteri begin fun depth goto ->
      IndexMap.iter begin fun nt la ->
        let la = IndexSet.inter la0 la in
        if IndexSet.is_not_empty la then (
          let cell = get_cell nt la in
          let states = predecessors depth in
          let done_ = Dyn.get cells cell in
          let todo = IndexSet.diff states done_ in
          push result (src, reached, depth, cell0, cell, states);
          if IndexSet.is_not_empty todo then (
            Dyn.set cells cell (IndexSet.union todo done_);
            IndexSet.rev_iter (explore_cell cell nt la) todo;
          )
        );
      end goto
    end reductions;
    match !result with
    | [] -> ignore (Gen.add links (src, reached, 0, cell0, sink, IndexSet.empty));
    | result -> List.iter (fun tr -> ignore (Gen.add links tr)) result
  in
  Index.iter (Lr1.cardinal g) begin fun lr1 ->
    let predecessors = get_stream ~initial:(-1) (Lr1.predecessors g lr1) in
    explore_transitions initial lr1 IndexSet.empty (Terminal.regular g) predecessors
      rc.:(lr1).all_reductions
  end;
  stopwatch 2 "raw redgraph: %d cells, %d links" (cardinal Cells.n) (cardinal Links.n);
  let module Min = Valmari.Minimize(struct
      type t = label
      let compare
          (lr1, targets1, depth1, _src1, _dst1, states1)
          (lr2, targets2, depth2, _src2, _dst2, states2)
        =
        let c = Index.compare lr1 lr2 in
        if c <> 0 then c else
          let c = Int.compare depth1 depth2 in
          if c <> 0 then c else
            let c = IndexSet.compare targets1 targets2 in
            if c <> 0 then c else
              let c = IndexSet.compare states1 states2 in
              c
    end)(struct
      type states = Cells.n
      let states = Cells.n

      type transitions = Links.n
      let transitions = Links.n

      let source tr = let (_,_,_,x,_,_) = Gen.get links tr in x
      let target tr = let (_,_,_,_,x,_) = Gen.get links tr in x
      let label tr = Gen.get links tr

      let initials f = f initial
      let finals f = Index.iter Cells.n f
      let refinements f =
        f (fun ~add -> add initial);
        f (fun ~add -> add sink)
    end)
  in
  let initial = Option.get (Min.transport_state initial) in
  let sink = Option.get (Min.transport_state sink) in
  stopwatch 2 "minimized redgraph: %d cells, %d links"
    (cardinal Min.states) (cardinal Min.transitions);
  let cells_outgoing = Vector.make Min.states IndexMap.empty in
  let cells_depth = Vector.make Min.states 0 in
  Index.rev_iter Min.transitions begin fun tr ->
    let source = Min.source tr in
    let target = Min.target tr in
    let lr, _, depth, _, _, _ = Min.label tr in
    cells_outgoing.@(source) <- IndexMap.update lr (add_update tr);
    cells_depth.@(target) <- Int.max depth
  end;
  stopwatch 2 "redgraph: indexed transitions";
  let succ f tr =
    let (_, _, _, _, _, states) = Min.label tr in
    let outgoing = cells_outgoing.:(Min.target tr) in
    IndexSet.rev_iter (fun src -> IndexSet.iter f (IndexMap.find src outgoing))
      states
  in
  let reachable = Vector.init Min.transitions (fun tr ->
      let acc = ref IndexSet.empty in
      succ (fun tr' ->
          let (_, targets, _, _, _, _) = Min.label tr' in
          acc := IndexSet.union targets !acc
        ) tr;
      !acc
    ) in
  Tarjan.close_relation succ reachable;
  stopwatch 2 "redgraph: reachability closure";
  let module Steps = Step.Const(struct
      type t = g
      let cardinal =
        Vector.fold_left (+) (Vector.length_as_int cells_depth - 1) cells_depth
      let () = stopwatch 2 "redgraph: %d steps" cardinal
    end) in
  let enum = Index.enumerate Steps.n in
  let step_zero = enum () in
  let cells_steps =
    Vector.mapi (fun cell depth ->
        if cell = initial || cell = sink then
          step_zero
        else (
          for _ = 0 to depth - 1 do
            ignore (enum ())
          done;
          enum ()
        )
      ) cells_depth
  in
  let steps = Vector.make Steps.n IndexMap.empty in
  Vector.rev_iteri begin fun cell step ->
    steps.:(step) <- IndexMap.map begin fun trs ->
        List.map (fun tr ->
            let (_, reached, depth, _, _, _) = Min.label tr in
            let reachable = reachable.:(tr) in
            let target = cells_steps.:(Min.target tr) in
            let step = Index.of_int Steps.n (Index.to_int target - depth) in
            {reached; reachable; step}
          ) (IndexSet.elements trs)
      end cells_outgoing.:(cell)
  end cells_steps;
  steps

type 'g action =
  | Advance of 'g step index
  | Switch of ('g lr1, 'g transition list) indexmap

(* Get initial transitions for a given LR(1) state from the graph entry point. *)
let initial (type g) (gr : g graph) (lr1 : g lr1 index) =
  match IndexMap.find_opt lr1 (Vector.as_array gr).(0) with
  | None -> []
  | Some l -> l

(* Follow a step in the reduction graph.
   Step 0 returns an empty switch; empty maps advance; non-empty maps switch. *)
let follow gr step =
  match (step : _ index :> int) with
  | 0 -> Switch IndexMap.empty
  | step' ->
    let map = gr.:(step) in
    if IndexMap.is_empty map then
      Advance (Index.of_int (Vector.length gr) (step' + 1))
    else
      Switch map
