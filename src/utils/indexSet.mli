open Fix.Indexing

(** Module for sets of indices. This module provides a type-safe abstraction
    over the underlying IntSet implementation. *)

include SetSig.S1 with type 'a t = private IntSet.t
                   and type 'a element = 'a index

(** [lift_sum s] lifts the index set [s] to a sum type. This is used when
    working with sum types in the type system.
    @param s The set to lift. *)
val lift_sum : 'a t -> ('a, _) Sum.n t

(** [unsafe_of_intset s] converts an IntSet.t to an index set of type ['a].
    This is unsafe as it changes the index type.
    @param s The IntSet to convert. *)
val unsafe_of_intset : IntSet.t -> 'a t

(** [all n] returns the set of all indices in the finite set of cardinal [n]. *)
val all : 'a cardinal -> 'a t

(** [init_from_set c f] creates a set of indices from a cardinal [c] and a
    predicate [f]. The set contains all indices [i] in [0, c-1] such that
    [f i] returns [true]. *)
val init_from_set : 'a cardinal -> ('a index -> bool) -> 'a t

(** Module for sets of index sets. *)
module Set : SetSig.StdSetS1 with type 'a t = private IntSetSet.t
                              and type 'a elt = 'a t

(** Module for maps from index sets to values. *)
module Map : SetSig.StdMapS1 with type ('n, 'a) t = private 'a IntSetMap.t
                              and type 'n key = 'n t
