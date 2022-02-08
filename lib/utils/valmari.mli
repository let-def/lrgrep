open Fix.Indexing

(** Valmari is an automata minimization algorithm, described in
    "Fast brief practical DFA minimization"
    https://dl.acm.org/doi/10.1016/j.ipl.2011.12.004 *)

module type DFA = sig
  type states
  val states : states cardinal
  (** The set of DFA nodes *)

  type transitions
  val transitions : transitions cardinal
  (** The set of DFA transitions *)

  type label
  (** The type of labels that annotate transitions *)

  val label  : transitions index -> label
  (** Get the label associated with a transition *)

  val source : transitions index -> states index
  (** Get the source state of the transition *)

  val target : transitions index -> states index
  (** Get the target state of the transition *)
end

module type INPUT = sig
  include DFA

  val initials : (states index -> unit) -> unit
  (** Iterate on initial states *)

  val finals : (states index -> unit) -> unit
  (** Iterate final states *)

  val refinements :
    refine:(iter:((states index -> unit) -> unit) -> unit) -> unit
    (** The minimization algorithms operate on a DFA plus an optional initial
        refinement (states that must be distinguished, because of some external
        properties not observable from the labelled transitions alone).

        If no refinements are needed, the minimum implementation is just:
          [let refinements ~refine:_ = ()]

        Otherwise, the [refinements] function should invoke the [refine]
        function for each set of equivalent states and call the [iter] for each
        equivalent state.

        E.g if our automata has 5 states, and states 2 and 3 have tag A while
        states 4 and 5 have tag B, we will do:

        let refinements ~refine =
          refine (fun ~iter -> iter [2; 3]);
          refine (fun ~iter -> iter [4; 5])
    *)
end

module Minimize
    (Label : Map.OrderedType)
    (In: INPUT with type label := Label.t) :
sig
  include DFA with type label = Label.t

  val initials : states index array
  val finals : states index array

  val transport_state : In.states index -> states index option
  val transport_transition : In.transitions index -> transitions index option

  val represent_state : states index -> In.states index
  val represent_transition : transitions index -> In.transitions index
end
