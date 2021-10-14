open Utils
open Strong

(** This module defines the signatures of intermediate representations used by
    Lrgrep. The actual implementations are parametrised by a grammar and split
    in separate files. *)

(** The definition of compiled grammars exposed by Menhir *)
module type GRAMMAR = MenhirSdk.Cmly_api.GRAMMAR

(** Useful information extracted from LR1 automaton *)
module type LR1 = sig
  module Grammar : GRAMMAR

  type t = Grammar.lr1
  (** Type of an lr1 state *)

  module Set : BitSet.S with type element = t
  (** Type of a set of lr1 states *)

  val all_states : Set.t
  (** The set of all lr1 states *)

  val incoming_symbol : t -> Grammar.symbol option
  (** Get the incoming symbol of an lr1 state (or None for initial states) *)

  val by_incoming_symbol : Grammar.symbol -> Set.t
  (** Get the set of all lr1 states that have a specific incoming symbol *)

  val predecessors_of_state : t -> Set.t
  (** Get the set of all predecessors of a state.
      If [ts = predecessors_of_state t] then for all t' in ts and incoming
      symbol of t is sym, there is a transition t' -sym-> t. *)

  val predecessors_of_states : Set.t -> Set.t
  (** predecessors_of_states ts = U { predecessors_of_state t | t in ts } *)

  val goto : t -> Grammar.nonterminal -> t
  (** Follow a goto transition *)

  val productions_for_states :
    ?incoming:Grammar.nonterminal -> Set.t ->
    (Grammar.production * Set.t) list
  (** Lets introduce an intermediate definition:
        val productions_for_state : t -> Grammar.production list

      [productions_for_state t] is the list of productions that can be
      immediately reduced when in state [t], without shifting any symbol.

      Then
        [productions_for_states ts =
           { (p, t) | p in production_for_state t, t in ts }]

      For each state in ts, it finds the productions that can be reduced from
      it. Each production is paired with the subset of ts that could reduce it.

      If ?incoming is provided, then it must be the incoming symbol of all of
      states in ts. It is a hint used to speed-up the analysis.

      TODO: interface is unnecessarily complex
  *)

  val partition : Set.t list -> Set.t list
  (** For [Refine.DECOMPOSABLE]: Refine a list of overlapping sets
      into the smallest list of disjoint sets that has the same union.
  *)

  val partition_and_total : Set.t list -> Set.t list * Set.t
  (** For [Refine.DECOMPOSABLE]: Like [partition], but return the union as
      well.
  *)

  val annotated_partition : (Set.t * 'a) list -> (Set.t * 'a list) list

  val states_by_items :
    lhs:Grammar.nonterminal option ->
    prefix:Grammar.symbol option list ->
    suffix:Grammar.symbol option list ->
    Set.t
    (** Return all lr1 sets that have an item that matches the conditions:
        - prefix must matches the producers before the dot
        - suffix must matches the producers after the dot

        The item might be longer: only the first [length prefix] producers
        before the dot and the first [length suffix] producers after the dot
        are matched.

        Producer matching:
        - [Some symbol] requires the producer to consume exactly this symbol
        - [None] is a wildcard

        If [lhs] is provided, then the production of this item must reduce to
        this nonterminal.
    *)
end

(** The "Alphabet" on which Lrgrep regular expressions are built: all Lr1
    states. *)
module type SIGMA = sig
  module Lr1 : LR1

  (** The set of states is represented either as positive occurrences (all
      states that are contained) or negative occurrences (all states that are
      not contained).

      This makes complement a cheap operation.  *)
  type t =
    | Pos of Lr1.Set.t
    | Neg of Lr1.Set.t

  val singleton : Lr1.t -> t
  val to_lr1set : t -> Lr1.Set.t

  include Mulet.SIGMA with type t := t

  val union : t -> t -> t
  (** Compute union of two sets *)

  val intersect : t -> t -> bool
  (** Check if two sets intersect *)

  val mem : Lr1.t -> t -> bool
  (** [mem lr1 t] checks if the state [lr1] is an element of a sigma set [t] *)
end

(** Construction of the "reduction graph".

    It represents all the sequence of reductions that can be applied on an LR1
    automaton, without ever shifting (without consuming input).

    A node of the reduction graph is the pair of an lr1 state (the current
    state of the parser) and an abstract stack.
    The abstract stack is a list of non-terminals that we know would sit at the
    top of a concrete stack if we were to execute these reductions on a
    concrete parser.

    For a non-cyclic grammar, there is only a finite number of such stacks, so
    the reduction graph is finite.
    In practice, grammars are cyclic, and so is the reduction graph.

    For instance, if there is a right recursion in the grammar:
      S:
      | X c {}

      X:
      | (* empty *) { }
      | a X         { }

    When reading many "a", they are all pushed on the stack.
    Reading a "c" will trigger the reduction of all the a's to X's.
    Therefore in the reduction graph, there will be a loop in some state
    (s_a, [X]) which represents abstract parsers in a certain lr1 state s_a for
    which we know that we have an X at the top of the stack.
*)
module type REDUCTION = sig
  module Lr1 : LR1

  (** The graph itself. *)
  module Concrete : sig
    module States : Finite.Set.T
    type state = States.n Finite.elt
    (** States of the graph *)

    val from_lr1: Lr1.t -> state
    (** Entry points are lr1 state (represented as the pair (state, []),
        nothing is known about the stack). *)

    val transitions : state -> (Lr1.Set.t * state) list
    (** Get the transitions of a state.
        The result is list of pairs (lr1_states, new_state) which says that
        when the state at the top of the parser stack is one of [lr1_states],
        we can transition to the state [new_state].

        This transition is non-deterministic: the same lr1 state can appear in
        multiple transitions.
    *)

    module Set : BitSet.S with type element = state
    (** For convenience, a datastructure to manipulate set of states *)
  end

  (** With [Concrete.from_lr1] and [Concrete.transitions] we can navigate the
      graph but we don't know which parser transformation corresponds to each
      transition.

      The [Derivation] module answers this question:
      - first by listing all possible transformations, identified by the finite
        set [Derivation.t]
      - for a given [Concrete.state], by listing all transformations that
        applies ("this state is reached when applying any of these
        transformations")
      - for a given [Concrete.state], by listing all transformations that are
        still reachable, by following more transitions (that is by looking
        deeper in the stack)
  *)
  module Derivation : sig
    type n
    type t = n Finite.elt
    (** The set of all transformations *)

    type 'a derivations
    (** Transformations are not directly represented, instead users build their
        own representation.
        [user_type derivations] is one of those custom representations where
        [user_type] is the type of an individual transformation.
    *)

    val derive :
      step:(Lr1.t -> 'a -> 'a) -> finish:(Lr1.t -> 'a -> 'b) ->
      'a -> 'b derivations
    (** A custom representation is built by providing an initial state and two
        functions to simulate the primary actions of transformation:
        - [step] consumes a non-terminal that is at the top of the stack
          (reversing a goto transition)
        - [finish] is called when the chain of reduction is finished, and the
          parser reached a specific state
    *)

    val get : 'a derivations -> t -> 'a
    (** Access a specific transformation from a custom representation *)

    module Set : BitSet.S with type element = t
    (** A set of transformations *)

    val filter : ('a -> bool) -> 'a derivations -> Set.t
    (** Computes a subset of transformations that satisfies a certain property.
    *)

    val reached : Concrete.state -> Set.t
    (** The set of transformations that applies to this state.
        It means this state can be reached from the initial one by applying any
        of this transformation.  *)

    val reachable : Concrete.state -> Set.t
    (** The set of transformations that are still reachable by looking deeper
        in the stack *)
  end
end

module type MINIMIZED_DFA = sig
  open Strong.Finite

  type regex
  type sigma

  include Valmari.DFA with type label := sigma

  val initial : states elt array
  val finals : states elt array

  val transport_state : regex -> states elt option
  val represent_state : states elt -> regex
  val transitions_from : states elt -> transitions elt list
end

(** Our extended notion of regular expression.

    The base definition of regular expressions is an instance [Mulet] toolkit,
    using Lr1 states as alphabet and "actions" as labels (see [Action]).

    The extension is the [simulate_reductions] operator, that matches an
    expression against all stacks that could be reached by following reductions
    rather than against the concrete stack.
*)
module type REGEX =
sig
  module Sigma : SIGMA
  module Reduction : REDUCTION with module Lr1 = Sigma.Lr1

  (** Regular expressions and automaton states are tagged with actions.
      The action is a metadata that tells the driver what to do when the
      automaton enter a given state.
      Right now the only metadata is accept: it is a set of integers that
      identifies semantic actions. When this set is non-empty, it contains user
      actions that can be executed in this state.
  *)
  module Action :
  sig
    type t = { accept : BitSet.IntSet.t (*; store : BitSet.IntSet.t;*) }
    val empty : t
    val compare : t -> t -> int
    val append : t -> t -> t
    val accept : int -> t
  end

  type reduction_operator

  include Mulet.S with type sigma = Sigma.t
                   and type label = Action.t
                   and type abstract = reduction_operator

  val simulate_reductions : Expr.t -> Expr.t
  (** Matches an expression against all stacks that could reached by following
      reductions starting from the current stack rather than matching directly
      againt the current stack.  *)

  val cmon : Expr.t -> Cmon.t

  val minimize : dfa:dfa -> initials:Expr.t list ->
    (module MINIMIZED_DFA with type regex = Expr.t
                           and type sigma = Sigma.Lr1.Set.t)
end

(* WIP DSLs for going from the surface syntax to a DFA

  module type SYNTAX = sig
    module Grammar : GRAMMAR
    type binder_kind = Always | Maybe
    type binder = (binder_kind * string) option

    type atom_kind =
      | Symbol of Grammar.symbol
      | Wildcard

    type atom = atom_kind * binder

    type pattern =
      | Atom of atom
      | Item of Grammar.nonterminal option * atom list * atom list
      | Cat  of pattern * pattern
      | Or   of pattern * pattern
      | Not  of pattern
      | And  of pattern * pattern
      | Star of pattern

    type top_pattern = {
      direct: pattern option;
      reduced: pattern option;
    }

    type action = string

    type program = (top_pattern * action) list
  end

  module Syntax(G : GRAMMAR): SYNTAX with module Grammar = G =
  struct
    module Grammar = G
    module rec Fix : SYNTAX with module Grammar := G = Fix
    include Fix
  end

  module type TRANSL = sig
    module Regex : REGEX
    module Syntax : SYNTAX with module Grammar = Regex.Sigma.Lr1.Grammar

    val compile_pattern : Syntax.pattern -> Regex.Expr.t
    val compile_program : Syntax.program -> Regex.Expr.t
    val make_dfa : Regex.Expr.t -> Regex.dfa
    val dfa_to_file : Regex.Expr.t * Regex.dfa -> string -> unit
  end
   *)
