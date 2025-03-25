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

module Make2(Info : Info.S)() = struct
  open Info

  module ENFA = struct
    (** Define states of the reduction ϵ-NFA.
        - [Initial]: initial state.
        - [Suffix]: represents a stack suffix with a set of lookaheads.
        - [Reduce]: represents a state during reduction with a stack suffix, set of
                    lookaheads, the nonterminal to `goto` at the end of the
                    reduction, and the number of symbols that remain to consume.

        Compared to the description in the thesis, the main differences are:
        - The stack suffix is represented by a single list with the current state
          as the first element. Thus, what is described in the document as
          [s . s'] is represented by a list [List.rev (s :: s')].
        - During reduction, we do not care about the actual symbols to remove
          (written α) but only by the number of symbols, thus we only store |α| as
          an [int].
          Furthermore, to improve performance, we want to explore multiple
          reductions in a single pass. So [Reduce] takes a list of the form
          [(pop, nt) :: rest] representing the sequence of actions to take: after
          popping [pop] elements, follow goto transition labelled [nt], and
          continue with list [rest].
    *)
    type state =
      | Initial
      | Suffix of Lr1.t list * Terminal.set
      | Reduce of Lr1.t list * (int * Nonterminal.t * Terminal.set) list

    (** A transition of the ϵ-NFA is represented as a pair of a label and a target state.
        The label is either [Some <lr1_state>] for a labelled transition or [None]
        for an ϵ-transition. *)
    type transition = Lr1.t option * state

    let elements_map set f =
      IndexSet.fold_right (fun acc x -> f x :: acc) [] set

    let delta_cons length lhs lookaheads = function
      | [] -> [(length, lhs, lookaheads)]
      | (length', lhs', lookaheads') :: rest ->
        let delta = length' - length in
        assert (delta >= 0);
        if delta = 0 && Index.equal lhs lhs' then
          (length, lhs, IndexSet.union lookaheads lookaheads') :: rest
        else
          (length, lhs, lookaheads) :: (delta, lhs', lookaheads') :: rest

    let reduction_list lr1 lookaheads =
      (* Reductions are ordered such that the shortest ones comes first, so by
         folding from the right we can construct a correct list of reductions
         delta. *)
      IndexSet.fold_right
        (fun acc red ->
           let lookaheads =
             IndexSet.inter (Reduction.lookaheads red) lookaheads
           in
           (* Skip this reduction if no lookahead permits to take it *)
           if IndexSet.is_empty lookaheads then
             acc
           else
             let prod = Reduction.production red in
             delta_cons
               (Production.length prod)
               (Production.lhs prod)
               lookaheads
               acc
        )
        [] (Reduction.from_lr1 lr1)

    (** The transition function of the ϵ-NFA *)
    let rec transitions : state -> transition list = function
      | Initial ->
        (* Visit all initial states *)
        elements_map Lr1.all
          (fun lr1 -> (Some lr1, Suffix ([lr1], Terminal.all)))
      | Suffix (stack, lookaheads) ->
        (* The current state of the LR automaton is at the top of the stack *)
        let current = List.hd stack in
        (* Visit all reductions applicable to the top state *)
        (match reduction_list current lookaheads with
         | [] -> []
         | actions -> transitions (Reduce (stack, actions)))
      (* End of a reduction sequence *)
      | Reduce (_, []) -> []
      (* End of a reduction *)
      | Reduce (stack, (0, lhs, lookaheads) :: rest) ->
        (* The current state of the LR automaton is at the top of the stack *)
        let current = List.hd stack in
        (* Find the target of the goto transition labelled [lhs] *)
        let target = Transition.find_goto_target current lhs in
        (None, Suffix (target :: stack, lookaheads)) ::
        (* Continue with remaining actions *)
        transitions (Reduce (stack, rest))
      (* Reducing an empty suffix: consume one more symbol of the stack *)
      | Reduce ([current], (n, lhs, lookaheads) :: rest) ->
        let actions' = (n - 1, lhs, lookaheads) :: rest in
        (* Look at possible predecessors *)
        elements_map (Lr1.predecessors current)
          (fun lr1 -> (Some lr1, Reduce ([lr1], actions')))
      (* Reducing from the current suffix *)
      | Reduce (stack, (n, lhs, lookaheads) :: rest) ->
        transitions (Reduce (List.tl stack, (n - 1, lhs, lookaheads) :: rest))
  end

  (* The "wildcard" NFA, with special transitions that should be taken for all
     input symbols.
     It keeps track of the suffixes produced by following goto transitions to
     enable recognition of reduce patterns. *)
  module WNFA = struct
    (** Explicit representation focusing on suffixes.

        A suffix [{stack; lookaheads}] abstracts the set of LR configurations
        whose stacks are suffixed by [stack] and looking ahead at a symbol
        in [lookaheads]. *)
    type suffix = {
      stack: Lr1.t list;
      lookaheads: Terminal.set;
      child: suffix list;
    }

    type 'a desc = {
      suffixes: suffix list;
      transitions: 'a Lr1.map;
      wildcards: 'a list;
    }

    let desc_empty = {suffixes = []; transitions = IndexMap.empty; wildcards = []}

    let desc_map f {suffixes; transitions; wildcards} = {
      suffixes;
      transitions = IndexMap.map f transitions;
      wildcards = List.map f wildcards
    }

    (** Determinize states and close over ϵ-transitions, accumulating visited
        suffixes on the way while preserving the tree structure (such that we can
        tell which suffix allowed reaching another one via ϵ-transitions). *)
    let rec fold_transition (desc : _ desc) (label, state) =
      match label with
      | None ->
        fold_transitions desc state
      | Some lr1 ->
        let update = function
          | None -> Some [state]
          | Some states -> Some (state :: states)
        in
        begin match state with
          | ENFA.Reduce (_, (n, _, _) :: _) when n > 0 ->
            {desc with wildcards = [state] :: desc.wildcards}
          | _ ->
            {desc with transitions = IndexMap.update lr1 update desc.transitions}
        end

    and fold_transitions (desc : _ desc) (state : ENFA.state) : _ desc =
      let transitions = ENFA.transitions state in
      match state with
      | Suffix (stack, lookaheads) ->
        let suffixes = desc.suffixes in
        let desc =
          List.fold_left fold_transition {desc with suffixes = []} transitions
        in
        let suffix = {stack; lookaheads; child = desc.suffixes} in
        {desc with suffixes = suffix :: suffixes}
      | _ -> List.fold_left fold_transition desc transitions

    include IndexBuffer.Gen.Make()

    (** Construct the DFA *)
    let initial, states =
      (* Generator for DFA states *)
      let states = get_generator () in
      (* Hashtable to memoize visited states. *)
      let table = Hashtbl.create 7 in
      (* Recursive function to visit and process states *)
      let rec visit nstates =
        let nstates = List.sort_uniq compare nstates in
        match Hashtbl.find_opt table nstates with
        | Some index -> index
        | None ->
          let slot = IndexBuffer.Gen.reserve states in
          Hashtbl.add table nstates (IndexBuffer.Gen.index slot);
          let desc = List.fold_left fold_transitions desc_empty nstates in
          let desc = desc_map visit desc in
          IndexBuffer.Gen.commit states slot desc;
          IndexBuffer.Gen.index slot
      in
      (* Start exploration from the initial state *)
      let initial = visit [ENFA.Initial] in
      (* Freeze the DFA states *)
      let states = IndexBuffer.Gen.freeze states in
      (initial, states)
  end

end


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
