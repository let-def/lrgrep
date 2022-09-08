open Utils

module type DFA = sig
  module Regexp : Mid.Sigs.REGEXP
  open Regexp
  open Info

  (** Our final notion of expression.
      Internally its a tuple of:
      - a set of regular expressions (implemented by [KRE])
      - a set of ongoing reductions (implemented by [Reduction]).
  *)
  module Expr : sig
    type t
    val make : KRESet.t -> t
    val cmon : t -> Cmon.t
    val compare : t -> t -> int
    val empty : t
    val union : t -> t -> t

    (** FIXME: Cleanup *)
    val interpret : t -> stack:Lr1.t list -> unit
  end

  (** States of the DFA are keyed by expression, therefore they are represented
      by an [ExprMap.t] *)
  module ExprMap : Map.S with type key = Expr.t

  module State : sig
    type t
    val id : t -> int

    (** The indices of clauses matching in this state *)
    val accepted : t -> IntSet.t

    (** Iterate through all outgoing transitions of a state.
        Transitions are given by three arguments:
        - a set of lr1 states that label the transition (it should be followed
          only when the state of an LR stack frame belongs to this set)
        - a list of variables to update, if the transition is taken,
          with the matching LR stack frame
        - a target DFA state *)
    val iter_transitions : t -> (Lr1.set -> RE.var list -> t -> unit) -> unit
  end

  (** A DFA is a map from expression to state *)
  type t = State.t ExprMap.t

  (** Number of states in the DFA / cardinal of the map *)
  val number_of_states : t -> int

  (** Produce a DFA from an initial expression.
      Returns a pair [(dfa, state)] of the dfa and the initial state matching
      expression *)
  val derive_dfa : Expr.t -> t * State.t

  (** Compile a DFA to a compact table, suitable for use with Lrgrep_support
      and runtime libraries. *)
  val gen_table : t -> Lrgrep_support.compact_dfa

  (** FIXME: Cleanup *)
  val eval : t -> State.t -> stack:Lr1.t list -> unit
end
