(******************************************************************************)
(*                                                                            *)
(* Code is taken from Menhir's CompressedBitSet.                              *)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU General Public License version 2, as described in the    *)
(*  file LICENSE.                                                             *)
(*                                                                            *)
(******************************************************************************)

module type S1 = sig
  (* Elements are assumed to have a natural total order. *)

  type 'a element

  (* Sets. *)

  type 'a t

  (* The empty set. *)

  val empty: 'a t

  (* [is_empty s] tells whether [s] is the empty set. *)

  val is_empty: 'a t -> bool

  (* The full set. *)

  val full: 'a t

  (* [is_full s] tells whether [s] is the full set. *)

  val is_full: 'a t -> bool

  (* [singleton x] returns a singleton set containing [x] as its only
     element. *)

  val singleton: 'a element -> 'a t

  (* [is_singleton s] tests whether [s] is a singleton set. *)

  val is_singleton: 'a t -> bool

  (* [cardinal s] returns the cardinal of [s]. *)

  val cardinal: 'a t -> [`Pos | `Neg] * int

  (* [mem x s] returns [true] if and only if [x] appears in the set
     [s]. *)

  val mem: 'a element -> 'a t -> bool

  (* [add x s] returns a set whose elements are all elements of [s],
     plus [x]. *)

  val add: 'a element -> 'a t -> 'a t

  (* [remove x s] returns a set whose elements are all elements of
     [s], except [x]. *)

  val remove: 'a element -> 'a t -> 'a t

  (* [union s1 s2] returns the union of the sets [s1] and [s2]. *)

  val union: 'a t -> 'a t -> 'a t

  (* [inter s t] returns the set intersection of [s] and [t], that is,
     $s\cap t$. *)

  val inter: 'a t -> 'a t -> 'a t

  (* [disjoint s1 s2] returns [true] if and only if the sets [s1] and
     [s2] are disjoint, i.e. iff their intersection is empty. *)

  val disjoint: 'a t -> 'a t -> bool

  (* [iter f s] invokes [f x], in turn, for each element [x] of the
     set [s]. Elements are presented to [f] in increasing order. *)

  (* [compare] is an ordering over sets. *)

  val compare: 'a t -> 'a t -> int

  (* [equal] implements equality over sets. *)

  val equal: 'a t -> 'a t -> bool

  (* [subset] implements the subset predicate over sets. *)

  val subset: 'a t -> 'a t -> bool

  (* [quick_subset s1 s2] is a fast test for the set inclusion [s1 ⊆ s2].

     The sets [s1] and [s2] must be nonempty.

     It must be known ahead of time that either [s1] is a subset of [s2] or
     these sets are disjoint: that is, [s1 ⊆ s2 ⋁ s1 ∩ s2 = ∅] must hold.

     Under this hypothesis, [quick_subset s1 s2] can be implemented simply
     by picking an arbitrary element of [s1] (if there is one) and testing
     whether it is a member of [s2]. *)
  val quick_subset: 'a t -> 'a t -> bool

  val diff : 'a t -> 'a t -> 'a t

  val of_list : 'a element list -> 'a t

  val complement : 'a t -> 'a t

  val partition : 'a t list -> 'a t list

  val annotated_partition : ('a t * 'b) list -> ('a t * 'b list) list
end

module CoIndexSet : S1 with type 'a element = 'a Fix.Indexing.index
