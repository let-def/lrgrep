open Fix.Indexing
open Utils
open Misc

module type INDEXED = sig
  type raw
  include Fix.Indexing.CARDINAL
  type t = n index
  type set = n indexset
  type 'a map = (n, 'a) IndexMap.t
  val of_g : raw -> t
  val to_g : t -> raw
end

module Terminal : sig
  include INDEXED with type raw = Grammar_raw.terminal
  val all : n indexset
end

module Nonterminal : sig
  include INDEXED with type raw = Grammar_raw.nonterminal
  val all : n indexset
end

module Symbol : sig
  type t = T of Terminal.t | N of Nonterminal.t
  val of_g : Grammar_raw.symbol -> t
  val to_g : t -> Grammar_raw.symbol
  val name : ?mangled:bool -> t -> string
end

module Production : sig
  include INDEXED with type raw = Grammar_raw.production
  val lhs : t -> Nonterminal.t
  val rhs : t -> Symbol.t array
  val kind : t -> [ `REGULAR | `START ]
end

module Lr1 : sig
  include INDEXED with type raw = Grammar_raw.lr1
  val all : set
  val incoming : t -> Symbol.t option
  val items : t -> (Production.t * int) list
  val reductions : t -> (Production.t * Terminal.set) list
  val to_string : t -> string
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

val lr1_predecessors : (Lr1.n, Lr1.set) vector
val lr1set_predecessors : Lr1.set -> Lr1.set

type 'a dfa_transition = Lr1.set * 'a

val dfa_normalize_transitions :
  ('a -> 'a -> int) -> 'a dfa_transition list -> 'a list dfa_transition list

val dfa_normalize_and_merge :
  compare:('a -> 'a -> int) -> merge:('a list -> 'b) ->
  'a dfa_transition list -> (Lr1.set * 'b) list
