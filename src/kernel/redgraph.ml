(* MIT License

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

type ('g, 'n) reduction_closures = ('n, 'g reduction_closure) vector

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
    let acc = if list_is_empty rs then acc else rs :: acc in
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
    let has_reductions = not (list_is_empty def.reductions) in
    let has_stacks = not (list_is_empty def.stacks) in
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

let make (type g) (g : g grammar) rc targets : g graph =
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
    explore_transitions cell src reached la predecessors rc.:(tgt).reductions

  and explore_transitions cell0 src reached la0 predecessors reductions =
    let result = ref [] in
    List.iteri begin fun depth goto ->
      IndexMap.iter begin fun nt la ->
        let la = IndexSet.inter la0 la in
        if not (IndexSet.is_empty la) then (
          let cell = get_cell nt la in
          let states = predecessors depth in
          let done_ = Dyn.get cells cell in
          let todo = IndexSet.diff states done_ in
          push result (src, reached, depth, cell0, cell, states);
          if not (IndexSet.is_empty todo) then (
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
      rc.:(lr1).reductions
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

let initial (type g) (gr : g graph) (lr1 : g lr1 index) =
  match IndexMap.find_opt lr1 (Vector.as_array gr).(0) with
  | None -> []
  | Some l -> l

let follow gr step =
  match (step : _ index :> int) with
  | 0 -> Switch IndexMap.empty
  | step' ->
    let map = gr.:(step) in
    if IndexMap.is_empty map then
      Advance (Index.of_int (Vector.length gr) (step' + 1))
    else
      Switch map
