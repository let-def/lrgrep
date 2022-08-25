open Fix.Indexing
open Utils.Misc

module type GRAMMAR = MenhirSdk.Cmly_api.GRAMMAR

module type INDEXED = sig
  type raw
  include CARDINAL
  type t = n index
  type set = n indexset
  type 'a map = (n, 'a) indexmap
  val of_g : raw -> t
  val to_g : t -> raw
end

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
    val to_string : t -> string
    val list_to_string : t list -> string
    val set_to_string : set -> string

    val shift_on : t -> Terminal.set
    val reduce_on : t -> Terminal.set
    val reject : t -> Terminal.set

    val closed_reductions : t -> (int * Nonterminal.t * Terminal.set) list
    val closed_reject : t -> Terminal.set

    val predecessors : t -> set
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

  type 'a dfa_transition = Lr1.set * 'a

  val dfa_normalize_transitions :
    ('a -> 'a -> int) -> 'a dfa_transition list -> 'a list dfa_transition list

  val dfa_normalize_and_merge :
    compare:('a -> 'a -> int) -> merge:('a list -> 'b) ->
    'a dfa_transition list -> (Lr1.set * 'b) list
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

    val derive_reduce : t -> t dfa_transition list
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
    val derive : t -> t dfa_transition list
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
    type transitions = D.t dfa_transition list * t dfa_transition list

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
