open Front
open Utils
open Misc
open Fix.Indexing

(** The representation of (compiled) grammars exported by Menhir *)
module type GRAMMAR = MenhirSdk.Cmly_api.GRAMMAR

(** The grammar defines many fixed sets (for terminals, non-terminals,
    productions, LR states, ...).
    For convenience, we will represent each of those sets using [Fix.Indexing].
    The [INDEXED] module type is the common interface shared by all these
    encodings: set definition and bijection with Menhir's representation. *)
module type INDEXED = sig

  (** This module defines a finite set *)
  include CARDINAL

  (** The type of Menhir's representation for elements of this finite set *)
  type raw

  (** An element of the set *)
  type t = n index

  (** A subset of elements *)
  type set = n indexset

  (** A partial map from elements to values of type ['a] *)
  type 'a map = (n, 'a) indexmap

  (** Import an element from the grammar representation *)
  val of_g : raw -> t

  (** Export an element to the grammar representation *)
  val to_g : t -> raw
end

(** The [INFO] module type first exposes a convenient interface for accessing
    all components of the [GRAMMAR] (using [INDEXED] interface).

    It also computes a few information that proves useful for later passes.
    For each [Lr1] state, the set of predecessors, the reductions grouped by
    lookahead tokens, the ε-closure of reductions, etc.

    It defines a reification of automaton's [Transition]s.
    Each transition is classified (shift or goto), given a name, and various
    useful operations are defined on it.

    Finally, it defines a useful function for working with partial derivatives
    with respect to Lr1 states.
*)
module type INFO = sig
  module Grammar : GRAMMAR

  module Terminal : sig
    include INDEXED with type raw = Grammar.terminal
    val to_string : t -> string
    val all : set
  end

  module Nonterminal : sig
    include INDEXED with type raw = Grammar.nonterminal
    val to_string : t -> string
    val all : set
    val kind : t -> [`REGULAR | `START]
  end

  module Symbol : sig
    type t = T of Terminal.t | N of Nonterminal.t
    val of_g : Grammar.symbol -> t
    val to_g : t -> Grammar.symbol
    val name : ?mangled:bool -> t -> string
  end

  module Production : sig
    include INDEXED with type raw = Grammar.production
    val lhs : t -> Nonterminal.t
    val rhs : t -> Symbol.t array
    val kind : t -> [ `REGULAR | `START ]
  end

  module Lr1 : sig
    include INDEXED with type raw = Grammar.lr1
    val all : set
    val incoming : t -> Symbol.t option
    val items : t -> (Production.t * int) list
    val reductions : t -> (Production.t * Terminal.set) list

    (* Printing functions, for debug purposes *)

    val to_string : t -> string
    val list_to_string : t list -> string
    val set_to_string : set -> string

    (** [shift_on t] is the set of terminals that the state [t] can shift *)
    val shift_on : t -> Terminal.set

    (** [reduce_on t] is the set of terminals that triggers a reduction in
        state [t] *)
    val reduce_on : t -> Terminal.set

    (** [reject t] is set of terminals that will immediately cause the
        automaton to fail when in state [t] *)
    val reject : t -> Terminal.set

    (** The list of reductions after ε-closure.
        The reductions are represented as triples [(pop, nt, lookahead)] where:
        - the reduction triggers when lookahead token is in [lookahead]
        - the action of the reduction is to pop [pop] states from the stack
        - then following the goto transition labelled by [nt]

        This list contains all the non-ε reduction and a bunch of "virtual"
        reductions.
        Such a "virtual" reduction is a sequence of one or more reduction
        starting from an ε-one and finishing by a normal reduction that will
        pop enough states such that the current state is popped.

        Therefore, [pop >= 1].
    *)
    val closed_reductions : t -> (int * Nonterminal.t * Terminal.set) list

    (** Like [reject], but also including all [reject] sets from the Lr1 state
        that where reached when following the sequences of reductions during
        ε-closure.

        TODO:
        - we can also define [closed_shift_on]
        - [closed_reduce t] is actually the union of lookaheads in
          [closed_reductions t].
    *)
    val closed_reject : t -> Terminal.set

    (** [predecessors t] is the set of LR(1) states that have transition going
        to [t]. *)
    val predecessors : t -> set

    (** [predecessors] but lifted to operate on a set of LR(1) states. *)
    val set_predecessors : set -> set
  end

  module Transition : sig
    (* Abstract types used as index to represent the different sets of
       transitions.
       For instance, [goto] represents the finite set of goto transition:
       - the value [goto : goto cardinal] is the cardinal of this set
       - any value of type [goto index] is a member of this set
         (representing a goto transition)
    *)
    type goto and shift and any

    (* The set of goto transitions *)
    val goto : goto cardinal
    (* The set of all transitions = goto U shift *)
    val any : any cardinal
    (* The set of shift transitions *)
    val shift : shift cardinal

    (* Building the isomorphism between any and goto U shift *)

    (* Inject goto into any *)
    val of_goto : goto index -> any index

    (* Inject shift into any *)
    val of_shift : shift index -> any index

    (* Project a transition into a goto or a shift transition *)
    val split : any index -> (goto index, shift index) either

    (* [find_goto s nt] finds the goto transition originating from [s] and
       labelled by [nt], or raise [Not_found].  *)
    val find_goto : Lr1.t -> Nonterminal.t -> goto index
    val find_goto_target : Lr1.t -> Nonterminal.t -> Lr1.t

    (* Get the source state of a transition *)
    val source : any index -> Lr1.t

    (* Get the target state of a transition *)
    val target : any index -> Lr1.t

    (* Symbol that labels a transition *)
    val symbol : any index -> Symbol.t

    (* Symbol that labels a goto transition *)
    val goto_symbol : goto index -> Nonterminal.t

    (* Symbol that labels a shift transition *)
    val shift_symbol : shift index -> Terminal.t

    (* [successors s] returns all the transitions [tr] such that
       [source tr = s] *)
    val successors : Lr1.t -> any index list

    (* [predecessors s] returns all the transitions [tr] such that
       [target tr = s] *)
    val predecessors : Lr1.t -> any index list
  end

  (** A simple type to represent a derivative of type ['a] with respect to a
      set of states. *)
  type 'a partial_derivative = Lr1.set * 'a

  (** Given a list of non-deterministic derivatives (the same state can appear
      in different derivation), return a list of deterministic ones
      where derivations that sharing the same states are grouped together. *)
  val determinize_derivatives :
    compare:('a -> 'a -> int) -> merge:('a list -> 'b) ->
    'a partial_derivative list -> 'b partial_derivative list
end

module type REGEXP = sig
  module Info : INFO
  open Info

  module RE : sig
    type uid = private int

    type var = int * int

    type t = { uid : uid; desc : desc; position : Syntax.position; }

    and desc =
      | Set of Lr1.set * var option
      | Alt of t list
      | Seq of t list
      | Star of t
      | Reduce

    val make : Syntax.position -> desc -> t
    val compare : t -> t -> int
    val cmon : ?var:(var -> Cmon.t) -> t -> Cmon.t
  end

  module KRE : sig
    type t =
      | Done of { clause : int }
      | More of RE.t * t

    val cmon : t -> Cmon.t
    val more : RE.t -> t -> t
    val compare : t -> t -> int
  end

  module KRESet : sig
    include Set.S with type elt = KRE.t

    val prederive :
      visited:t ref ->
      reached:int list ref ->
      direct:(Lr1.set * RE.var list * KRE.t) list ref ->
      reduce:KRE.t list ref -> elt -> unit

    val derive_reduce : t -> t partial_derivative list
    val cmon : t -> Cmon.t
  end

  val transl :
    alloc:(string -> RE.var) -> clause:int ->
    Syntax.regular_expr -> KRE.t
end

module type REDGRAPH = sig
  module Info : INFO
  open Info

  val derive :
    root:'a ->
    step:('a -> Lr1.t -> 'a option) ->
    join:('a list -> 'b) ->
    'b Lr1.map

  module State : sig
    include CARDINAL
    val of_lr1 : Lr1.t -> n index
  end

  val fail_on_closure : Lr1.t -> Terminal.set
  val reduce_on : Lr1.t -> Terminal.set

  type goto_closure = {
    sources: Lr1.set;
    targets: Lr1.set;
    lookahead: Terminal.set;
  }

  val state_lr1s : State.n index -> Lr1.set
  val state_parent : State.n index -> State.n index option
  val state_goto_closure : State.n index -> goto_closure list
  val state_reachable : State.n index -> Lr1.set
end

module type REDUCTION = sig
  module Info : INFO
  open Info

  module type DERIVABLE = sig
    type t
    val derive : t -> t partial_derivative list
    val merge : t list -> t
    val compare : t -> t -> int
    val cmon : t -> Cmon.t
  end

  module Cache (D : DERIVABLE) : sig
    include DERIVABLE
    val lift : D.t -> t
    val unlift : t -> D.t
  end

  module Make (D : DERIVABLE) :
  sig
    type t
    type transitions = D.t partial_derivative list * t partial_derivative list

    type compilation_cache
    val make_compilation_cache : unit -> compilation_cache

    type compilation
    val compile : compilation_cache -> D.t -> compilation
    val cmon : compilation -> Cmon.t

    val initial : compilation -> transitions
    val derive : t -> transitions
    val compare : t -> t -> int
  end
end
