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

module Viable = Unsafe_cardinal()
type 'g viable = 'g Viable.t

(* A goto transition, which includes the target state,
   the set of lookahead symbols that permitted to follow it,
   the source state (different for inner/epsilon-reductions, and outer ones),
   and the reduction to be performed. *)
type ('g, 'a) goto_transition = {
  target: 'g viable index;
  lookahead: 'g terminal indexset;
  source: 'a;
  reduction: 'g reduction index;
}

(* A step in the reduction process, which includes the set of reachable states and a list
   of goto candidates. *)
type ('g, 'a) reduction_step = {
  reachable: 'g viable indexset;
  goto_transitions: ('g, 'a) goto_transition list;
}

(* Transitions within the same state (inner) and transitions to other states (outer). *)
type 'g inner_transitions = ('g, unit) reduction_step list
type 'g outer_transitions = ('g, 'g lr1 indexset) reduction_step list

type 'g transitions = {
  inner: 'g inner_transitions;
  outer: 'g outer_transitions;
}

(* A configuration of a reduction simulation state,
   including the top of the stack, the rest of the stack,
   and the lookahead symbols that permitted to reach it. *)
type 'g config = {
  top: 'g lr1 index;
  rest: 'g lr1 index list;
  lookahead: 'g terminal indexset;
}

type 'g t = {
  initial: ('g lr1, 'g outer_transitions) vector;
  config: ('g viable, 'g config) vector;
  reachable_from: ('g viable, 'g viable indexset) vector;
  transitions: ('g viable, 'g transitions) vector;
}

(* Group reductions by their depth (visit epsilon reductions first, then reductions with one producer, two producers, etc). *)
let group_reductions (type g) (g : g grammar) =
  let rec group depth : g reduction index list -> g reduction index list list =
    function
    | [] -> []
    | red :: rest when depth = Production.length g (Reduction.production g red) ->
      begin match group depth rest with
        | [] -> [[red]]
        | reds :: tail ->
          (red :: reds) :: tail
      end
    | otherwise ->
      [] :: group (depth + 1) otherwise
  in
  Vector.init (Lr1.cardinal g)
    (fun lr1 -> group 0 (IndexSet.elements (Reduction.from_lr1 g lr1)))

let make (type g) (g : g grammar) : g t =
  stopwatch 2 "constructing viable reduction graph";
  let open Info in
  let module States = IndexBuffer.Gen.Make() in
  let module VEq = Viable.Eq(struct type t = g include States end) in
  let Refl : (States.n, g viable) eq = Obj.magic () in
  (* Get the generator for state indices. *)
  let states = States.get_generator () in
  (* A hashtable to store configurations and their corresponding state indices. *)
  let nodes = Hashtbl.create 7 in
  let reductions = group_reductions g in
  (* Create states by visiting configurations and their outgoing transitions. *)
  let rec visit_config config =
    match Hashtbl.find_opt nodes config with
    | Some state -> state
    | None ->
      let reservation = IndexBuffer.Gen.reserve states in
      let index = IndexBuffer.Gen.index reservation in
      Hashtbl.add nodes config index;
      IndexBuffer.Gen.commit states reservation (config, visit_transitions config);
      index
  (* Visit all outgoing transitions of a given configuration. *)
  and visit_transitions config =
    visit_inner config reductions.:(config.top)
  (* Follow inner transitions for a given configuration and list of reductions. *)
  and visit_inner config = function
    | [] -> ([], [])
    | gotos :: next ->
      let (inner, outer) = match config.rest with
        | top :: rest ->
          visit_inner {config with top; rest} next
        | [] ->
          ([], visit_outers config.lookahead (Lr1.predecessors g config.top) next)
      in
      let process_goto red =
        let prod = Reduction.production g red in
        let lookahead = Terminal.intersect g (Reduction.lookaheads g red) config.lookahead in
        if IndexSet.is_empty lookahead then
          None
        else
          let top = Transition.find_goto_target g config.top (Production.lhs g prod) in
          let rest = config.top :: config.rest in
          let target = visit_config {top; rest; lookahead} in
          Some {target; lookahead; source=(); reduction=red}
      in
      (List.filter_map process_goto gotos :: inner, outer)
  (* Follow outer transitions for a given lookahead set,
     set of LR(1) states, and list of reductions. *)
  and visit_outers lookahead lr1_states = function
    | [] -> []
    | gotos :: next ->
      visit_outer lookahead lr1_states.lnext gotos next
  (* Helper function for visiting a single outer transition. *)
  and visit_outer lookahead (lazy lr1_states) gotos next =
    let next = visit_outers lookahead lr1_states next in
    let process_goto acc red =
      let lookahead =
        Terminal.intersect g lookahead (Reduction.lookaheads g red)
      in
      if IndexSet.is_empty lookahead then acc
      else
        let lhs = Production.lhs g (Reduction.production g red) in
        let process_target source acc =
          (source, Transition.find_goto_target g source lhs) :: acc
        in
        IndexSet.fold process_target lr1_states.lvalue []
        |> List.sort (fun (s1,t1) (s2,t2) ->
             let c = Index.compare t1 t2 in
             if c <> 0 then c else Index.compare s1 s2)
        |> merge_group
           ~equal:Index.equal
           ~group:(fun top sources ->
             let config = {top; rest = []; lookahead} in
             let source = IndexSet.of_list sources in
             let target = visit_config config in
             {source; target; lookahead; reduction=red}
           )
    in
    let gotos = List.fold_left process_goto [] gotos in
    gotos :: next
  in
  (* Compute the initial set of transitions for each LR(1) state. *)
  let initial = Vector.init (Lr1.cardinal g) (fun lr1 ->
      match reductions.:(lr1) with
      | [] -> []
      | gotos :: next ->
        visit_outer (Terminal.regular g) (Lazy.from_val (Lr1.predecessors g lr1)) gotos next
    )
  in
  let states = IndexBuffer.Gen.freeze states in
  stopwatch 2 "constructed viable reduction graph with %d nodes" (cardinal States.n);
  (* Compute the set reachable states (closure of successors). *)
  let reachable_from =
    let add_target acc step = IndexSet.add step.target acc in
    let add_targets acc l =
      List.fold_left (List.fold_left add_target) acc l
    in
    Vector.mapi
      (fun self (_, (inner, outer)) ->
         add_targets (add_targets (IndexSet.singleton self) inner) outer)
      states
  in
  Tarjan.close_relation reachable_from;
  stopwatch 2 "closed the big-step successors";
  (* Compute reachability for all steps of a reduction *)
  let rec process_steps = function
    | [] -> []
    | step :: steps ->
      let steps = process_steps steps in
      let acc = match steps with
        | [] -> IndexSet.empty
        | x :: _ -> x.reachable
      in
      let reachable =
        let add_reach acc c = IndexSet.union acc reachable_from.:(c.target) in
        List.fold_left add_reach acc step
      in
      {reachable; goto_transitions=step} :: steps
  in
  let make_reduction_step (_, (inner, outer)) =
    {inner = process_steps inner; outer = process_steps outer}
  in
  let initial = Vector.map process_steps initial in
  let config = Vector.map fst states in
  let transitions = Vector.map make_reduction_step states in
  stopwatch 2 "closed the small-step successors";
  {initial; reachable_from; config; transitions}

let get_stack vr state =
  let {top; rest; _} = vr.config.:(state) in
  List.rev (top :: rest)

(* [string_of_stack st] is a string representing the suffix of the stacks
   recognized by when reaching state [st]. *)
let string_of_stack g vr state =
  string_concat_map " " (Lr1.to_string g) (get_stack vr state)
