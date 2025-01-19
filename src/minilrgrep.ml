open Utils
open Fix.Indexing

(* This file is a minimal implementation of LRgrep, supporting only
   Reduce-filter patterns (chapter 2) and with over-approximated coverage
   (to avoid dealing with rechability analysis (chapter 7) *)

let usage () =
  Printf.eprintf
    "Minilrgrep\n\
     \n\
     Usage:\n\
     %s compile <grammar.cmly> <spec.mlyl>\n\
    " Sys.argv.(0)

let grammar_file, spec_file =
  match Sys.argv with
  | [|_; "compile"; grammar_file; spec_file|] ->
    grammar_file, spec_file
  | _ ->
    usage ();
    exit 1

module Grammar = MenhirSdk.Cmly_read.Read(struct let filename = grammar_file end)

module Info = Kernel.Info.Make(Grammar)

let list_from_set ss f =
  IndexSet.fold (fun s acc ->
      match f s with
      | None -> acc
      | Some x -> x :: acc
    ) ss []

(* Implementation of 2.3.2 : The viable reduction NFA *)

module Reduction_NFA = struct
  open Info

  (* Define states as described by in the thesis.
     Two simplifications:
     - The stack suffix is represented by a single list with the current state
       as the first element.
       Thus, what is described in the document as [s . s'] is represented by a
       list [List.rev (s :: s')].
     - During reduction, we do not care about the actual symbols to remove
       (written α) but only by the number of symbols, thus we only store |α| as
       an [int] *)
  type state =
    | Initial
    | Suffix of Lr1.t list * Terminal.set
    | Reduce of Lr1.t list * Terminal.set * Nonterminal.t * int


  (* The label of a transition is an optional LR(1) state, where [None] denotes ϵ. *)
  type label = Lr1.t option

  (* An (outgoing) transition is represented as a pair of a label and a target state. *)
  type transition = label * state

  (* Since we are compiling error-matching patterns, we are only interested in
     the subset of the reduction automaton starting from wait states.
     (See 4.2.1). *)
  let initial_lr1_states = Lr1.wait

  let transitions : state -> transition list = function
    | Initial ->
      (* Visit all initial states *)
      list_from_set initial_lr1_states
        (fun lr1 -> Some (Some lr1, Suffix ([lr1], Terminal.all)))
    | Suffix (stack, lookaheads) ->
      (* The current state of the LR automaton is at the top of the stack *)
      let current = List.hd stack in
      (* Visit all reductions applicable to the top state *)
      list_from_set (Reduction.from_lr1 current)
        (fun red ->
           (* Find lookaheads compatible with this reduction *)
           let lookaheads =
             Terminal.intersect lookaheads (Reduction.lookaheads red)
           in
           if IndexSet.is_empty lookaheads then
             (* Give up if there are none *)
             None
           else
             (* Otherwise, enter a reduction state *)
             let production = Reduction.production red in
             let lhs = Production.lhs production in
             let rhs = Production.length production in
             Some (None, Reduce (stack, lookaheads, lhs, rhs))
        )
    (* End of a reduction sequence *)
    | Reduce (stack, lookaheads, lhs, 0) ->
      (* The current state of the LR automaton is at the top of the stack *)
      let current = List.hd stack in
      (* Find the target of the goto transition labelled [lhs] *)
      let target = Transition.find_goto_target current lhs in
      [None, Suffix (target :: stack, lookaheads)]
    (* Reducing an empty suffix: consume one more symbol of the stack *)
    | Reduce ([current], lookaheads, lhs, n) ->
      (* Look at possible predecessors *)
      list_from_set (Lr1.predecessors current)
        (fun lr1 ->
           Some (Some lr1, Reduce ([lr1], lookaheads, lhs, n - 1)))
    (* Reducing from the current suffix *)
    | Reduce (stack, lookaheads, lhs, n) ->
      [None, Reduce (List.tl stack, lookaheads, lhs, n - 1)]

  (* (* Explicit representation of the NFA *)
  include IndexBuffer.Gen.Make()

  let states = get_generator ()

  (* A hashtable to memoize visited states *)
  let table = Hashtbl.create 7

  let rec visit state =
    match Hashtbl.find_opt table state with
    | Some index -> index
    | None ->
      let slot = IndexBuffer.Gen.reserve states in
      Hashtbl.add table state (IndexBuffer.Gen.index slot);
      let transitions =
        List.map (fun (label, state) -> (label, visit state)) (transitions state)
      in
      IndexBuffer.Gen.commit states slot (state, transitions);
      IndexBuffer.Gen.index slot

  let initial = visit Initial

  let states = IndexBuffer.Gen.freeze states

  let () =
    Printf.eprintf "Constructed the Reduction NFA with %d states\n" (cardinal n) *)
end

(* Determinize the automaton and use an explicit representation *)

module Reduction_DFA = struct
  open Info

  (* Determinize and close over ϵ-transitions *)
  let rec fold_transitions acc (state : Reduction_NFA.state) : Reduction_NFA.state list Lr1.map =
    List.fold_left (fun acc (label, state') ->
        match label with
        | None -> fold_transitions acc state'
        | Some lr1 ->
          IndexMap.update lr1
            (function None -> Some [state'] | Some states -> Some (state' :: states))
            acc
      ) acc (Reduction_NFA.transitions state)

  (* Explicit representation focusing on suffixes *)

  type suffix = {
    stack: Lr1.t list;
    lookaheads: Terminal.set;
  }

  include IndexBuffer.Gen.Make()

  type config = {
    suffixes: suffix list;
    transitions: n index Lr1.map;
  }

  let states = get_generator ()

  (* A hashtable to memoize visited states *)
  let table = Hashtbl.create 7

  let rec visit nstates =
    let nstates = List.sort_uniq compare nstates in
    match Hashtbl.find_opt table nstates with
    | Some index -> index
    | None ->
      let slot = IndexBuffer.Gen.reserve states in
      Hashtbl.add table nstates (IndexBuffer.Gen.index slot);
      let transitions =
        nstates
        |> List.fold_left fold_transitions IndexMap.empty
        |> IndexMap.map visit
      in
      let suffixes = List.filter_map (function
          | Reduction_NFA.Initial | Reduce _ -> None
          | Reduction_NFA.Suffix (stack, lookaheads) -> Some {stack; lookaheads}
        ) nstates
      in
      IndexBuffer.Gen.commit states slot {suffixes; transitions};
      IndexBuffer.Gen.index slot

  let initial = visit [Reduction_NFA.Initial]

  let states = IndexBuffer.Gen.freeze states

  let () =
    Printf.eprintf "Constructed the Reduction DFA with %d states\n" (cardinal n)
end
