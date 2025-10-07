(* MIT License:

   Copyright (c) 2025 Frédéric Bour

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
 *)

(** This module is responsible for computing viable reductions in a LR(1) parser
    generator. It generates a graph of states, where each state represents a
    configuration of the parser, including the top of the stack, the rest of the
    stack, and the current lookahead set. The module also computes transitions
    between these states based on possible reductions and goto actions.
*)

open Fix.Indexing
open Utils
open Misc
open Info

let printf_debug = false

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

type 'g reduction_closure = {
  failing: 'g terminal indexset;
  reductions: ('g nonterminal, 'g terminal indexset) indexmap list;
  stacks: ('g lr1 index list * 'g terminal indexset) list;
}

let add_failing g r reject la =
  r := IndexSet.union (Terminal.intersect g reject la) !r

(* Close ϵ-reductions of each LR(1) states *)
let close_lr1_reductions (type g) (g : g grammar) : (g lr1, g reduction_closure) vector =
  Vector.init (Lr1.cardinal g) @@ fun lr1 ->
  let failing = ref IndexSet.empty in
  let rec pop lookahead acc (item : g item index) = function
    | [] ->
      let items, stacks = acc in
      ((item, lookahead) :: items, stacks)
    | hd :: tl as stack ->
      match Item.prev g item with
      | Some item' -> pop lookahead acc item' tl
      | None ->
        let lhs = Production.lhs g (Item.production g item) in
        let stack = Transition.find_goto_target g hd lhs :: stack in
        let items, stacks = acc in
        let acc = (items, (stack, lookahead) :: stacks) in
        reduce lookahead acc stack
  and reduce lookahead acc stack =
    let lr1 = List.hd stack in
    add_failing g failing (Lr1.reject g lr1) lookahead;
    IndexSet.fold begin fun red acc ->
      match Terminal.intersect g (Reduction.lookaheads g red) lookahead with
      | la when IndexSet.is_empty la -> acc
      | la ->
        pop la acc (Item.last g (Reduction.production g red)) stack
    end (Reduction.from_lr1 g lr1) acc
  in
  let items, stacks = reduce (Terminal.all g) ([],[]) [lr1] in
  let reductions = group_reductions g items in
  let failing = !failing in
  {failing; reductions; stacks}

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

let rec filter_stacks g la acc = function
  | [] -> acc
  | (x, la') :: xs ->
    let la' = Terminal.intersect g la la' in
    let acc =
      if IndexSet.is_empty la'
      then acc
      else (x, la') :: acc
    in
    filter_stacks g la' acc xs

let rec merge_reduction_step map acc = function
  | [] -> (map, acc)
  | [] :: _ -> assert false
  | (r :: rs) :: rrs ->
    let acc = if List.is_empty rs then acc else rs :: acc in
    let augment _ a b = Some (IndexSet.union a b) in
    let map = IndexMap.union augment r map in
    merge_reduction_step map acc rrs

let rec merge_reductions = function
  | [] -> []
  | rrs ->
    let r, rrs' = merge_reduction_step IndexMap.empty [] rrs in
    r :: merge_reductions rrs'

(* Close reductions of goto transitions *)
let close_goto_reductions (type g) (g : g grammar) rcs
  : (g goto_transition, g reduction_closure) vector
  =
  let sentinel = {failing = IndexSet.empty; reductions = []; stacks = []} in
  let table = Vector.make (Transition.goto g) sentinel in
  Index.rev_iter (Transition.goto g) begin fun gt ->
    if printf_debug then
      Printf.printf "## Closing %s\n"
        (Transition.to_string g (Transition.of_goto g gt));
    let tr = Transition.of_goto g gt in
    let src = Transition.source g tr in
    let tgt = Transition.target g tr in
    let stacks = ref [] in
    let reductions = ref [] in
    let push_reductions = function
      | [] -> ()
      | rs -> push reductions rs
    in
    let failing = ref IndexSet.empty in
    let rec visit_target tgt la =
      let rc = rcs.:(tgt) in
      if printf_debug then
        Printf.printf "- reaching target %s @ %s\n"
          (Lr1.to_string g tgt)
          (Terminal.lookaheads_to_string g la);
      add_failing g failing rc.failing la;
      if printf_debug then
        Printf.printf "importing %d stacks\n" (List.length rc.stacks);
      stacks := ([tgt], la) :: filter_stacks g la !stacks rc.stacks;
      match filter_reductions g la rc.reductions with
      | [] -> ()
      | r :: rs ->
        push_reductions rs;
        if printf_debug then
          Printf.printf "importing %d reductions\n" (List.length rs);
        IndexMap.iter visit_nt r

    and visit_nt nt la =
      let gt' = Transition.find_goto g src nt in
      if true || Index.compare gt' gt <= 0 then
        visit_target (Transition.target g (Transition.of_goto g gt')) la
      else
        let rc = table.:(gt') in
        add_failing g failing rc.failing la;
        stacks := filter_stacks g la !stacks rc.stacks;
        push_reductions (filter_reductions g la rc.reductions)
    in
    visit_target tgt (Terminal.all g);
    let failing = !failing in
    let stacks = !stacks in
    let reductions = merge_reductions !reductions in
    table.:(gt) <- {failing; reductions; stacks}
  end;
  flush stdout;
  table

let dump_closure ?(failing=false) g print_label vector =
  Vector.iteri begin fun st def ->
    let has_failing = failing && not (IndexSet.is_empty def.failing) in
    let has_reductions = not (List.is_empty def.reductions) in
    let has_stacks = not (List.is_empty def.stacks) in
    if has_failing || has_reductions || has_stacks then
      Printf.fprintf stdout "%s:\n" (print_label st);
    if has_failing then
      Printf.fprintf stdout "- failing: %s\n"
        (string_of_indexset ~index:(Terminal.to_string g) def.failing);
    if has_reductions then (
      Printf.fprintf stdout "- reductions:\n";
      List.iter (fun map ->
          let first = ref true in
          IndexMap.iter (fun nt la ->
              if !first then
                (Printf.fprintf stdout "  - "; first := false)
              else
                Printf.fprintf stdout "    ";
              Printf.fprintf stdout "%s @ %s\n"
                (Nonterminal.to_string g nt)
                (Terminal.lookaheads_to_string g la);
            ) map
        ) def.reductions
    );
    if has_stacks then (
      Printf.fprintf stdout "- stacks:\n";
      List.iter (fun (stack, la) ->
          Printf.fprintf stdout "  - %s @ %s\n"
            (Lr1.list_to_string g stack)
            (Terminal.lookaheads_to_string g la)
        ) def.stacks
    );
  end vector

(* Reduction targets indexation *)

module Target = Unsafe_cardinal()
type 'g target = 'g Target.t

type 'g trie = {
  mutable sub: ('g lr1, 'g trie) indexmap;
  mutable immediates: 'g lr1 indexset;
  mutable targets: ('g lr1, 'g target index) indexmap;
}

let index_targets (type g) (g : g grammar) rc
    : g trie * (g goto_transition, (g target index * g terminal indexset) list) vector
  =
  (* Index sources of goto transitions *)
  let goto_sources = Vector.make (Lr1.cardinal g) IndexSet.empty in
  Index.rev_iter (Transition.goto g) begin fun gt ->
    goto_sources.@(Transition.target g (Transition.of_goto g gt)) <-
      IndexSet.add gt 
  end;
  (* Allocate target identifiers *)
  let module Gen = Gensym() in
  let open Target.Eq(struct
      type t = g
      include Gen
    end) in
  let Refl = eq in
  (* Targets by goto transition *) 
  let by_goto = Vector.make (Transition.goto g) [] in
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
    let roots = List.map (fun (stack, la) -> follow_path stack, la) rc.:(tgt).stacks in
    (* 1. Register immediates *)
    List.iter
      (fun ((node, lr1), _) ->
         node.immediates <- IndexSet.add lr1 node.immediates)
      roots;
    (* Goto sources *)
    let sources = goto_sources.:(tgt) in
    if not (IndexSet.is_empty sources) then
      (* Prepend all goto transitions (by construction, rc stacks already end with tgt) *)
      let roots =
        (get_child (root, tgt), Terminal.all g) ::
        List.map (fun (root, la) -> (get_child root, la)) roots
      in
      IndexSet.iter begin fun gt ->
        List.iter
          (fun (root, la) ->
             let index = Gen.fresh () in
             by_goto.@(gt) <- List.cons (index, la);
             let src = Transition.source g (Transition.of_goto g gt) in
             root.targets <- IndexMap.add src index root.targets
          ) roots
      end sources
  end;
  (* Done *)
  (root, by_goto)


(* Graph construction *)
    
module Node = Unsafe_cardinal()
type ('g, 's) node = ('g * 's) Node.t

module Step = Unsafe_cardinal()
type ('g, 's) step = ('g * 's) Step.t

type 'g node_desc = {
  lr1: 'g lr1 index;
  lookaheads: 'g terminal indexset;
}

type 'g pre_step = ('g lr1, 'g target indexset * ('g, 'g lr1) node indexset) indexmap
let prepare (type g) (g : g grammar) rc targets
  : ((g, g lr1) node, g node_desc * (g lr1, g pre_step list) indexmap) vector *
    (g lr1, g pre_step list) vector
  =
  let module Nodes = IndexBuffer.Gen.Make() in
  let nodes = Nodes.get_generator () in
  let table = Vector.make (Lr1.cardinal g) IndexSet.Map.empty in
  let get_memoize la lr1 ~f =
    let map0 = table.:(lr1) in
    match IndexSet.Map.find_opt la map0 with
    | Some index -> index
    | None ->
      let r = IndexBuffer.Gen.reserve nodes in
      let i = IndexBuffer.Gen.index r in
      table.:(lr1) <- IndexSet.Map.add la i map0;
      IndexBuffer.Gen.commit nodes r (f la lr1);
      i
  in
  let rec visit_reductions la lr1s = function
    | [] -> []
    | nts :: next ->
      let lazy lr1s = lr1s in
      let curr =
        IndexMap.inflate begin fun lr1 ->
          let tgts, nodes =
            IndexMap.fold begin fun nt la (tgts, acc) ->
              let gt = Transition.find_goto g lr1 nt in
              let tgts = List.fold_left (fun acc (target, la') ->
                  if IndexSet.disjoint la la'
                  then acc
                  else IndexSet.add target acc
                ) tgts targets.:(gt)
              in
              let tgt = Transition.target g (Transition.of_goto g gt) in
              (tgts, get_memoize la tgt ~f:visit_state :: acc)
            end nts (IndexSet.empty, [])
          in
          (tgts, IndexSet.of_list (List.rev nodes))
        end lr1s.lvalue
      in
      let next = visit_reductions la lr1s.lnext next in
      match next with
      | [] when IndexMap.is_empty curr -> []
      | _ -> curr :: next
  and visit_state lookaheads lr1 =
    let reductions = rc.:(lr1).reductions in
    let lr1s = (Lazy.force (Lr1.predecessors g lr1).lnext).lvalue in
    let transitions =
      IndexMap.inflate (fun lr1 ->
          let lr1s = Lr1.predecessors g lr1 in
          visit_reductions lookaheads (lazy lr1s) reductions 
        ) lr1s
    in
    ({lr1; lookaheads}, transitions)
  in
  let initials =
    Vector.init (Lr1.cardinal g) (fun lr1 ->
        let lr1s = (Lr1.predecessors g lr1).lnext in
        visit_reductions (Terminal.all g) lr1s rc.:(lr1).reductions
      )
  in
  let nodes = IndexBuffer.Gen.freeze nodes in
  stopwatch 2 "viable2: %d nodes\n" (Vector.length_as_int nodes);
  let open Node.Eq(struct
      type t = g * g lr1
      include Nodes
    end ) in
  let Refl = eq in
  (nodes, initials)

type ('g, 's) step_desc = {
  next: ('g, 's) step index;
  reachable: 'g target indexset;
  goto: ('g lr1, 'g target indexset * ('g, 's) node indexset) indexmap;
}

type ('g, 's) graph = {
  nodes: (('g,'s) node, 'g node_desc * ('g lr1, ('g,'s) step index) indexmap) vector;
  initials: ('g lr1, ('g,'s) step index) vector;
  steps: (('g,'s) step, ('g, 's) step_desc) vector;
}

let small_steps (type g)
    ((gr_nodes : ((g, g lr1) node, g node_desc * (g lr1, g pre_step list) indexmap) vector),
     (gr_initials : (g lr1, g pre_step list) vector))
  : (g, g lr1) graph
  =
  let flatten_targets map acc =
    IndexMap.fold (fun _ (tgts, _) acc -> IndexSet.union tgts acc) map acc
  in
  let flatten_nodes map acc =
    IndexMap.fold (fun _ (_, nodes) acc -> IndexSet.union nodes acc) map acc
  in
  let flatten_maplist ~f (_, mtrs) =
    IndexMap.fold (fun _ trs acc ->
        IndexSet.union (List.fold_right f trs IndexSet.empty) acc
      ) mtrs IndexSet.empty
  in
  (* Compute the set reachable states (closure of successors). *)
  let successors = Vector.map (flatten_maplist ~f:flatten_nodes) gr_nodes in
  let reachable_from = Vector.map (flatten_maplist ~f:flatten_targets) gr_nodes in
  stopwatch 2 "prepared big-step successors";
  Tarjan.close_relation (Vector.get successors) reachable_from;
  stopwatch 2 "closed the big-step successors";
  (* Implement small steps with sharing *)
  let module NMap = Map.Make(struct
      type t = g pre_step
      let compare (a : t) (b : t) =
        IndexMap.compare (compare_pair IndexSet.compare IndexSet.compare) a b
    end) in
  let open IndexBuffer in
  let module Steps = Gen.Make() in
  let open Step.Eq(struct
      type t = g * g lr1 
      include Steps
    end) in
  let Refl = eq in
  let steps = Steps.get_generator () in
  let zero = Gen.add steps NMap.empty in
  let index = Dyn.make {
      next = zero;
      goto = IndexMap.empty;
      reachable = IndexSet.empty;
    }
  in
  let get goto next =
    let map = Gen.get steps next in
    match NMap.find_opt goto map with
    | Some step -> step
    | None ->
      let step = Gen.add steps NMap.empty in
      Gen.set steps next (NMap.add goto step map);
      (* Compute reachability for all steps of a reduction *)
      let successors = flatten_nodes goto IndexSet.empty in
      let reachable = flatten_targets goto IndexSet.empty in
      let reachable = IndexSet.union reachable (IndexSet.bind successors (Vector.get reachable_from)) in
      let reachable = IndexSet.union reachable (Dyn.get index next).reachable in
      Dyn.set index step {next; goto; reachable};
      step
  in
  let pack_transitions trs = List.fold_right get trs zero in
  let add_reachables (node, transitions) = (node, IndexMap.map pack_transitions transitions) in
  let nodes = Vector.map add_reachables gr_nodes in
  let initials = Vector.map pack_transitions gr_initials in
  let steps = Dyn.contents index Steps.n in
  stopwatch 2 "closed the small-step successors (%d elements)"
    (Vector.length_as_int steps);
  {nodes; initials; steps}

let make g rc targets = small_steps (prepare g rc targets)

let dump_dot _oc _g (*grc*) _graph =
  failwith "TODO"
  (*let p fmt = Printf.kfprintf (fun oc -> output_char oc '\n') oc fmt in
  p "digraph {";
  p "  rankdir=LR;";
  p "  node[shape=rect];";
  let pnode i = Printf.sprintf "node%d" (i : _ index :> int) in
  let rec follow_step from step =
    if Index.to_int step > 0 then (
      IndexMap.iter (fun label (_, nodes) ->
          let lr1 = Lr1.to_string g label in
          IndexSet.iter (fun node ->
              p "  %s -> %s [label=%S];"
                from
                (pnode node)
                lr1)
            nodes
        ) graph.steps.:(step).goto;
      let next = graph.steps.:(step).next in
      if Index.to_int next > 0 then (
        let pstep = Printf.sprintf "step%d" (Index.to_int next) in
        p "  %s[shape=plain,label=\"<step-%d>\"];" pstep (Index.to_int next);
        p "  %s -> %s [dir=none, label=\"_\"];" from pstep;
        follow_step pstep next;
      );
    )
  in
  (* Vector.iteri (fun lr1 steps ->
       if not (IndexSet.is_empty steps) then (
         let node = Printf.sprintf "start%d" (lr1 : _ index :> int) in
         p "  %s[label = %S];" node (Lr1.to_string g lr1);
         IndexSet.iter (follow_step node) steps
       )
     ) graph.initials; *)
  Vector.iteri begin fun i (desc, step) ->
    (*let gotos = string_of_indexset ~index:(fun gt ->
        Printf.sprintf "(%s) = [%s]"
          (Transition.to_string g (Transition.of_goto g gt))
          (string_concat_map "; "
             (fun (lr1s, _) -> Lr1.list_to_string g lr1s)
             grc.:(gt).stacks)
      ) node.gotos in*)
    p "  %s[label=\"%s @ <%d lookaheads>\"];" (pnode i)
      (Lr1.to_string g desc.lr1)
      (IndexSet.cardinal desc.lookaheads);
    follow_step (pnode i) step
  end graph.nodes;
    p "}"*)
