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

(* Step 1: pre-compute closure of ϵ-reductions *)

type ('g, 'a) with_lookahead = 'a * 'g terminal indexset

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

type 'g reduce_closure = {
  failing: 'g terminal indexset;
  reductions: ('g nonterminal, 'g terminal indexset) indexmap list;
  stacks: ('g, 'g lr1 index list) with_lookahead list;
}

let add_failing g r reject la =
  r := IndexSet.union (Terminal.intersect g reject la) !r

(* Close ϵ-reductions of each LR(1) states *)
let reduce_closures (type g) (g : g grammar) : (g lr1, g reduce_closure) vector =
  Vector.init (Lr1.cardinal g) @@ fun lr1 ->
  let failing = ref IndexSet.empty in
  let rec pop lookahead acc (item : g item index) = function
    | [] -> let items, stacks = acc in ((item, lookahead) :: items, stacks)
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
      let lookahead = Terminal.intersect g (Reduction.lookaheads g red) lookahead in
      if IndexSet.is_empty lookahead
      then acc
      else pop lookahead acc (Item.last g (Reduction.production g red)) stack
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
let goto_reduce_closures (type g) (g : g grammar) rcs
  : (g goto_transition, g reduce_closure) vector
  =
  let sentinel = {failing = IndexSet.empty; reductions = []; stacks = []} in
  let table = Vector.make (Transition.goto g) sentinel in
  Index.rev_iter (Transition.goto g) begin fun gt ->
    let tr = Transition.of_goto g gt in
    let src = Transition.source g tr in
    let tgt = Transition.target g tr in
    let stacks = ref [] in
    let reductions = ref [] in
    let failing = ref IndexSet.empty in
    let rec visit_target tgt la =
      let rc = rcs.:(tgt) in
      add_failing g failing rc.failing la;
      stacks := filter_stacks g la !stacks rc.stacks;
      match filter_reductions g la rc.reductions with
      | [] -> ()
      | r :: rs ->
        if not (List.is_empty rs) then
          push reductions rs;
        let visit nt la = visit_goto (Transition.find_goto g src nt) la in
        IndexMap.iter visit r
    and visit_goto gt' la =
      if Index.compare gt' gt <= 0 then
        visit_target (Transition.target g (Transition.of_goto g gt')) la
      else
        let rc = table.:(gt') in
        add_failing g failing rc.failing la;
        stacks := filter_stacks g la !stacks rc.stacks;
        let rs = filter_reductions g la rc.reductions in
        if not (List.is_empty rs) then
          push reductions rs
    in
    visit_target tgt (Terminal.all g);
    let failing = !failing in
    let stacks = !stacks in
    let reductions = merge_reductions !reductions in
    table.:(gt) <- {failing; reductions; stacks}
  end;
  table

module Viable_nodes = Unsafe_cardinal()
type 'g viable_nodes = 'g Viable_nodes.t

let viable2 (type g stack) (g : g grammar)
    (stacks: stack cardinal)
    (lr1_of: stack index -> g lr1 index)
    (predecessors: stack index -> stack indexset lazy_stream)
     rc grc
  : (g viable_nodes,
     stack index * g nonterminal indexset *
     g terminal indexset * g viable_nodes indexset list)
      vector *
    (stack, g viable_nodes indexset list) vector
  =
  let module Nodes = IndexBuffer.Gen.Make() in
  let nodes = Nodes.get_generator () in
  let table = Vector.make stacks IndexSet.Map.empty in
  let get_memoize lrc nts la ~f =
    let map0 = table.:(lrc) in
    let map1 = Option.value (IndexSet.Map.find_opt nts map0) ~default:IndexSet.Map.empty in
    match IndexSet.Map.find_opt la map1 with
    | Some index -> index
    | None ->
      let r = IndexBuffer.Gen.reserve nodes in
      let i = IndexBuffer.Gen.index r in
      table.:(lrc) <- IndexSet.Map.add nts (IndexSet.Map.add la i map1) map0;
      IndexBuffer.Gen.commit nodes r (f ());
      i
  in
  let rec visit_reductions la lrcs = function
    | [] -> []
    | nts :: next ->
      let lazy lrcs = lrcs.lnext in
      let curr =
        let by_la =
          IndexMap.fold (fun nt la' acc ->
              let la' = IndexSet.inter la la' in
              cons_if
                (not (IndexSet.is_empty la'))
                (la', nt) acc
            ) nts []
          |> IndexRefine.annotated_partition
        in
        List.fold_left (fun acc (la, nts) ->
            let nts = IndexSet.of_list nts in
            IndexSet.fold (fun lrc acc -> visit_gotos lrc nts la :: acc) lrcs.lvalue acc
          ) [] by_la
      in
      let next = visit_reductions la lrcs next in
      match curr, next with
      | [], [] -> []
      | _ -> IndexSet.of_list curr :: next
  and visit_gotos lrc nts la : Nodes.n index =
    get_memoize lrc nts la ~f:begin fun () ->
      let reductions =
        IndexSet.fold begin fun nt acc ->
          match grc.:(Transition.find_goto g (lr1_of lrc) nt).reductions with
          | [] -> acc
          | x -> x :: acc
        end nts []
      in
      let transitions =
        visit_reductions la (predecessors lrc) (merge_reductions reductions)
      in
      (lrc, nts, la, transitions)
    end
  in
  let initials = Vector.init stacks @@ fun lrc ->
    let lr1 = lr1_of lrc in
    visit_reductions (Terminal.all g) (predecessors lrc) rc.:(lr1).reductions
  in
  let nodes = IndexBuffer.Gen.freeze nodes in
  stopwatch 2 "viable2: %d nodes\n" (Vector.length_as_int nodes);
  let open Viable_nodes.Eq(struct
      type t = g
      include Nodes
    end ) in
  let Refl = eq in
  (nodes, initials)

module Viable_steps = Unsafe_cardinal()
type 'g viable_steps = 'g Viable_steps.t

type ('g, 'node, 'step) step = {
  next: 'step index;
  reachable: 'node indexset;
  goto: 'node indexset;
}

let small_steps (type g stack)
    (nodes : (g viable_nodes, stack index * g nonterminal indexset *
                              g terminal indexset * g viable_nodes indexset list) vector)
    (initials : (stack, g viable_nodes indexset list) vector)
  : (g viable_nodes, stack index * g nonterminal indexset *
                     g terminal indexset * g viable_steps index) vector *
    (stack, g viable_steps index) vector *
    (g viable_steps, (g viable_nodes, g viable_steps index) IndexSet.Map.t) vector
  =
  (* Compute the set reachable states (closure of successors). *)
  let reachable_from =
    Vector.map
      (fun (_, _, _, transitions) ->
         List.fold_right IndexSet.union transitions IndexSet.empty
      ) nodes
  in
  stopwatch 2 "prepared big-step successors";
  Tarjan.close_relation reachable_from;
  stopwatch 2 "closed the big-step successors";
  (* Implement small steps with sharing *)
  let open IndexBuffer in
  let module Steps = Gen.Make() in
  let steps = Steps.get_generator () in
  let zero = Gen.add steps IndexSet.Map.empty in
  let index = Dyn.make {
      next = zero;
      goto = IndexSet.empty;
      reachable = IndexSet.empty;
    }
  in
  let get goto next =
    let map = Gen.get steps next in
    match IndexSet.Map.find_opt goto map with
    | Some step -> step
    | None ->
      let step = Gen.add steps IndexSet.Map.empty in
      Gen.set steps next (IndexSet.Map.add goto step map);
      (* Compute reachability for all steps of a reduction *)
      let reachable = IndexSet.bind goto (Vector.get reachable_from) in
      let reachable = IndexSet.union reachable (Dyn.get index next).reachable in
      Dyn.set index step {next; goto; reachable};
      step
  in
  let pack_transitions trs = List.fold_right get trs zero in
  let add_reachables (lr1, nts, la, trs) = (lr1, nts, la, pack_transitions trs) in
  let transitions = Vector.map add_reachables nodes in
  let initials = Vector.map pack_transitions initials in
  let steps = Gen.freeze steps in
  stopwatch 2 "closed the small-step successors (%d elements)"
    (Vector.length_as_int steps);
  let open Viable_steps.Eq(struct
      type t = g
      include Steps
    end) in
  let Refl = eq in
  (transitions, initials, steps)
