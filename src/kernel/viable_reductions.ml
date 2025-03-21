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

module type S = sig
  module Info : Info.S
  open Info

  include CARDINAL

  (* A goto transition, which includes the target state,
     the set of lookahead symbols that permitted to follow it,
     the source state (different for inner/epsilon-reductions, and outer ones),
     and the reduction to be performed. *)
  type 'a goto_transition = {
    target: n index;
    lookahead: Terminal.set;
    source: 'a;
    reduction: Reduction.t;
  }

  (* A step in the reduction process, which includes the set of reachable states and a list
     of goto candidates. *)
  type 'a reduction_step = {
    reachable: n indexset;
    goto_transitions: 'a goto_transition list;
  }

  (* Transitions within the same state (inner) and transitions to other states (outer). *)
  type inner_transitions = unit reduction_step list
  type outer_transitions = Lr1.set reduction_step list
  type transitions = {
    inner: inner_transitions;
    outer: outer_transitions;
  }

  (* A configuration of a reduction simulation state,
     including the top of the stack, the rest of the stack,
     and the lookahead symbols that permitted to reach it. *)
  type config = {
    top: Lr1.t;
    rest: Lr1.t list;
    lookahead: Terminal.set;
  }

  (* The initial set of transitions simulating reductions
     for each LR(1) state. *)
  val initial : (Lr1.n, outer_transitions) vector

  (* Retrieve the configuration of a state. *)
  val get_config : n index -> config

  (* Retrieve the stack of a state. *)
  val get_stack : n index -> Lr1.t list

  (* Retrieve the transitions of a state. *)
  val get_transitions : n index -> transitions

  (* Retrieve the states reachable from a state. *)
  val reachable : n index -> n indexset

  (* [string_of_stack st] is a string representing the suffix of the stacks
     recognized by when reaching state [st]. *)
  val string_of_stack : n index -> string
end

module Make(Info : Info.S)() : S with module Info := Info =
struct
  open Info

  (* Start a stopwatch to measure the time taken by the module's execution. *)
  let time = Stopwatch.enter Stopwatch.main "Viable_reductions.Make"

  include IndexBuffer.Gen.Make()

  (* A configuration of a reduction simulation state,
     including the top of the stack, the rest of the stack,
     and the lookahead symbols that permitted to reach it. *)
  type config = {
    top: Lr1.t;
    rest: Lr1.t list;
    lookahead: Terminal.set;
  }

  (* Representation of a goto transition, which includes the target state,
     the set of lookahead symbols that permitted to follow it,
     the source state (different for inner/epsilon-reductions, and outer ones),
     and the reduction to be performed. *)
  type 'a goto_transition = {
    target: n index;
    lookahead: Terminal.set;
    source: 'a;
    reduction: Reduction.t;
  }

  (* A step in the reduction process, which includes the set of reachable states and a list
     of goto candidates. *)
  type 'a reduction_step = {
    reachable: n indexset;
    goto_transitions: 'a goto_transition list;
  }

  (* Transitions that do not consume stack symbols (inner)
     and transitions that depend on symbols deeper in the stack (outer). *)
  type inner_transitions = unit reduction_step list
  type outer_transitions = Lr1.set reduction_step list
  type transitions = {
    inner: inner_transitions;
    outer: outer_transitions;
  }

  (* Group reductions by their depth (visit epsilon reductions first, then reductions with one producer, two producers, etc). *)
  let reductions =
    let rec group depth
      : Reduction.t list -> Reduction.set list =
      function
      | [] -> []
      | red :: rest when depth = Production.length (Reduction.production red) ->
        begin match group depth rest with
          | [] -> [IndexSet.singleton red]
          | reds :: tail ->
            IndexSet.add red reds :: tail
          end
      | otherwise ->
        IndexSet.empty :: group (depth + 1) otherwise
    in
    Vector.init Lr1.n
      (fun lr1 -> group 0 (IndexSet.elements (Reduction.from_lr1 lr1)))

  (* Get the generator for state indices. *)
  let states = get_generator ()

  (* A hashtable to store configurations and their corresponding state indices. *)
  let nodes = Hashtbl.create 7

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
    visit_inner config (Vector.get reductions config.top)

  (* Follow inner transitions for a given configuration and list of reductions. *)
  and visit_inner config = function
    | [] -> ([], [])
    | gotos :: next ->
      let (inner, outer) = match config.rest with
        | top :: rest ->
          visit_inner {config with top; rest} next
        | [] ->
          ([], visit_outers config.lookahead (IndexSet.singleton config.top) next)
      in
      let process_goto red =
        let prod = Reduction.production red in
        let lookahead = Terminal.intersect (Reduction.lookaheads red) config.lookahead in
        if IndexSet.is_empty lookahead then
          None
        else
          let top = Transition.find_goto_target config.top (Production.lhs prod) in
          let rest = config.top :: config.rest in
          let target = visit_config {top; rest; lookahead} in
          Some {target; lookahead; source=(); reduction=red}
      in
      (List.filter_map process_goto (IndexSet.elements gotos) :: inner, outer)

  (* Follow outer transitions for a given lookahead set,
     set of LR(1) states, and list of reductions. *)
  and visit_outers lookahead lr1_states = function
    | [] -> []
    | gotos :: next ->
      let lr1_states = indexset_bind lr1_states Lr1.predecessors in
      visit_outer lookahead lr1_states gotos next

  (* Helper function for visiting a single outer transition. *)
  and visit_outer lookahead lr1_states gotos next =
    let next = visit_outers lookahead lr1_states next in
    let process_goto red acc =
      let lookahead =
        Terminal.intersect lookahead (Reduction.lookaheads red)
      in
      if IndexSet.is_empty lookahead then acc
      else
        let lhs = Production.lhs (Reduction.production red) in
        let process_target source acc =
          let target_lhs = Transition.find_goto_target source lhs in
          IndexMap.update target_lhs (function
            | Some (sources, target) ->
              Some (IndexSet.add source sources, target)
            | None ->
              let config = {
                top = target_lhs;
                rest = [];
                lookahead;
              } in
              Some (IndexSet.singleton source, visit_config config)
          ) acc
        in
        let by_target = IndexSet.fold process_target lr1_states IndexMap.empty in
        let add_target _ (source, target) acc =
          {source; target; lookahead; reduction=red} :: acc
        in
        IndexMap.fold add_target by_target acc
    in
    let gotos = IndexSet.fold process_goto gotos [] in
    gotos :: next

  (* Compute the initial set of transitions for each LR(1) state. *)
  let initial = Vector.init Lr1.n (fun lr1 ->
      match Vector.get reductions lr1 with
      | [] -> []
      | gotos :: next ->
        visit_outer Terminal.regular (IndexSet.singleton lr1) gotos next
    )

  let states = IndexBuffer.Gen.freeze states

  (* Compute the set reachable states (closure of successors). *)
  let reachable =
    let reachable =
      let add_target acc step = IndexSet.add step.target acc in
      let add_targets acc l =
        List.fold_left (List.fold_left add_target) acc l
      in
      Vector.mapi
        (fun self (_stack, (inner, outer)) ->
          add_targets (add_targets (IndexSet.singleton self) inner) outer)
        states
    in
    close_relation reachable;
    reachable

  (* Follow all steps of a reduction *)
  let rec process_steps = function
    | [] -> []
    | step :: steps ->
      let steps = process_steps steps in
      let acc = match steps with
        | [] -> IndexSet.empty
        | x :: _ -> x.reachable
      in
      let reachable =
        List.fold_left (fun acc candidate ->
            IndexSet.union acc
              (Vector.get reachable candidate.target))
          acc step
      in
      {reachable; goto_transitions=step} :: steps

  (* Create a reduction step by exploring inner and outer transitions. *)
  let make_reduction_step (inner, outer) =
    {inner = process_steps inner; outer = process_steps outer}

  (* Compute the initial set of transitions by processing all reductions applicable to states. *)
  let initial = Vector.map process_steps initial

  (* Compute the final definitions of states, with stack suffixes and processed transitions. *)
  let states = Vector.map (fun (stack, steps) -> (stack, make_reduction_step steps)) states

  (* The set of states reachable from a state. *)
  let reachable = Vector.get reachable

  (* The configuration and transitions of a state. *)
  let get_def = Vector.get states
  let get_config st = fst (get_def st)
  let get_transitions st = snd (get_def st)

  (* The stack suffix of a given state. *)
  let get_stack st =
    let config = get_config st in
    config.top :: config.rest

  (* [string_of_stack st] is a string representing the suffix of the stacks
     recognized by when reaching state [st]. *)
  let string_of_stack state =
    let {top; rest; _} = get_config state in
    let states = List.rev (top :: rest) in
    string_concat_map " " Lr1.to_string states

  (* Log the number of nodes in the viable reductions graph. *)
  let () =
    Stopwatch.step time "Viable reductions graph has %d nodes" (cardinal n);
    Stopwatch.leave time
end
