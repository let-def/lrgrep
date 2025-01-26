(******************************************************************************)
(*                                                                            *)
(*                                   Mulnir                                   *)
(*                                                                            *)
(*                          Frédéric Bour, Tarides                            *)
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

  (* [singleton x] returns a singleton set containing [x] as its only
     element. *)

  val singleton: 'a element -> 'a t

  (* [is_singleton s] tests whether [s] is a singleton set. *)

  val is_singleton: 'a t -> bool

  (* [cardinal s] returns the cardinal of [s]. *)

  val cardinal: 'a t -> int

  (* [choose s] returns an arbitrarily chosen element of [s], if [s]
     is nonempty, and raises [Not_found] otherwise. *)

  val choose: 'a t -> 'a element

  val minimum: 'a t -> 'a element option

  val maximum: 'a t -> 'a element option

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

  val iter: ('a element -> unit) -> 'a t -> unit

  val rev_iter: ('a element -> unit) -> 'a t -> unit

  (* [fold f s seed] invokes [f x accu], in turn, for each element [x]
     of the set [s]. Elements are presented to [f] in increasing
     order. The initial value of [accu] is [seed]; then, at each new
     call, its value is the value returned by the previous invocation
     of [f]. The value returned by [fold] is the final value of
     [accu]. In other words, if $s = \{ x_1, x_2, \ldots, x_n \}$,
     where $x_1 < x_2 < \ldots < x_n$, then [fold f s seed] computes
     $([f]\,x_n\,\ldots\,([f]\,x_2\,([f]\,x_1\,[seed]))\ldots)$. *)

  val fold: ('a element -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val fold_right: ('a -> 'b element -> 'a) -> 'a -> 'b t -> 'a

  val map: ('a element -> 'b element) -> 'a t -> 'b t

  val exists: ('a element -> bool) -> 'a t -> bool

  (* [elements s] is a list of all elements in the set [s]. *)

  val elements: 'a t -> 'a element list

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

  (** {1 Decomposing sets}

      These functions implements the [Refine.DECOMPOSABLE] interface.
      We cannot reference it here as [Refine] is implemented using bitsets,
      that would create a reference cycle.
  *)

  (* [compare_minimum l r] order two sets by comparing their least element *)
  val compare_minimum : 'a t -> 'a t -> int

  (* [extract_unique_prefix l r] split l in two sets (l_min, l_rest) such that:
     - l_min contains elements strictly smaller than the all elements of [r]
     - l_rest contains other elements
  *)
   val extract_unique_prefix : 'a t -> 'a t -> 'a t * 'a t

  (* [extract_shared_prefix l r] decomposes l and r in (min, l', r') such that:
     - [min] is the set of minimal elements that are part of both [l] and [r]
     - [l = min U l'] and [r = min U r']
  *)
  val extract_shared_prefix : 'a t -> 'a t -> 'a t * ('a t * 'a t)

  (* [sorted_union l] computes the union of an ordered list of intervals.
     This is an optimized special case of union *)
  val sorted_union : 'a t list -> 'a t

  val of_list : 'a element list -> 'a t

  val init_interval : 'a element -> 'a element -> 'a t
  val init_subset : 'a element -> 'a element -> ('a element -> bool) -> 'a t
  val filter : ('a element -> bool) -> 'a t -> 'a t
  val filter_map : ('a element -> 'b element option) -> 'a t -> 'b t
  val split: 'a element -> 'a t -> 'a t * bool * 'a t
end

module type S0 = sig
  type element
  type t
  include S1 with type 'a element := element
              and type 'a t := t
end

module type StdSetS1 = sig
  type 'a elt
  type 'a t
  val empty: 'a t
  val is_empty: 'a t -> bool
  val mem: 'a elt -> 'a t -> bool
  val add: 'a elt -> 'a t -> 'a t
  val singleton: 'a elt -> 'a t
  val remove: 'a elt -> 'a t -> 'a t
  val union: 'a t -> 'a t -> 'a t
  val inter: 'a t -> 'a t -> 'a t
  val disjoint: 'a t -> 'a t -> bool
  val diff: 'a t -> 'a t -> 'a t
  val compare: 'a t -> 'a t -> int
  val equal: 'a t -> 'a t -> bool
  val subset: 'a t -> 'a t -> bool
  val iter: ('a elt -> unit) -> 'a t -> unit
  val map: ('a elt -> 'b elt) -> 'a t -> 'b t
  val fold: ('a elt -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val for_all: ('a elt -> bool) -> 'a t -> bool
  val exists: ('a elt -> bool) -> 'a t -> bool
  val filter: ('a elt -> bool) -> 'a t -> 'a t
  val filter_map: ('a elt -> 'b elt option) -> 'a t -> 'b t
  val partition: ('a elt -> bool) -> 'a t -> 'a t * 'a t
  val cardinal: 'a t -> int
  val elements: 'a t -> 'a elt list
  val min_elt: 'a t -> 'a elt
  val min_elt_opt: 'a t -> 'a elt option
  val max_elt: 'a t -> 'a elt
  val max_elt_opt: 'a t -> 'a elt option
  val choose: 'a t -> 'a elt
  val choose_opt: 'a t -> 'a elt option
  val split: 'a elt -> 'a t -> 'a t * bool * 'a t
  val find: 'a elt -> 'a t -> 'a elt
  val find_opt: 'a elt -> 'a t -> 'a elt option
  val find_first: ('a elt -> bool) -> 'a t -> 'a elt
  val find_first_opt: ('a elt -> bool) -> 'a t -> 'a elt option
  val find_last: ('a elt -> bool) -> 'a t -> 'a elt
  val find_last_opt: ('a elt -> bool) -> 'a t -> 'a elt option
  val of_list: 'a elt list -> 'a t
  val to_seq_from : 'a elt -> 'a t -> 'a elt Seq.t
  val to_seq : 'a t -> 'a elt Seq.t
  val to_rev_seq : 'a t -> 'a elt Seq.t
  val add_seq : 'a elt Seq.t -> 'a t -> 'a t
  val of_seq : 'a elt Seq.t -> 'a t
end

module type StdMapS1 = sig
  type 'n key
  type ('n, 'a) t
  val empty: ('n, 'a) t
  val is_empty: ('n, 'a) t -> bool
  val mem:  'n key -> ('n, 'a) t -> bool
  val add: 'n key -> 'a -> ('n, 'a) t -> ('n, 'a) t
  val update: 'n key -> ('a option -> 'a option) -> ('n, 'a) t -> ('n, 'a) t
  val singleton: 'n key -> 'a -> ('n, 'a) t
  val remove: 'n key -> ('n, 'a) t -> ('n, 'a) t
  val merge:
    ('n key -> 'a option -> 'b option -> 'c option) -> ('n, 'a) t -> ('n, 'b) t -> ('n, 'c) t
  val union: ('n key -> 'a -> 'a -> 'a option) -> ('n, 'a) t -> ('n, 'a) t -> ('n, 'a) t
  val compare: ('a -> 'a -> int) -> ('n, 'a) t -> ('n, 'a) t -> int
  val equal: ('a -> 'a -> bool) -> ('n, 'a) t -> ('n, 'a) t -> bool
  val iter: ('n key -> 'a -> unit) -> ('n, 'a) t -> unit
  val fold: ('n key -> 'a -> 'b -> 'b) -> ('n, 'a) t -> 'b -> 'b
  val for_all: ('n key -> 'a -> bool) -> ('n, 'a) t -> bool
  val exists: ('n key -> 'a -> bool) -> ('n, 'a) t -> bool
  val filter: ('n key -> 'a -> bool) -> ('n, 'a) t -> ('n, 'a) t
  val filter_map: ('n key -> 'a -> 'b option) -> ('n, 'a) t -> ('n, 'b) t
  val partition: ('n key -> 'a -> bool) -> ('n, 'a) t -> ('n, 'a) t * ('n, 'a) t
  val cardinal: ('n, 'a) t -> int
  val bindings: ('n, 'a) t -> ('n key * 'a) list
  val min_binding: ('n, 'a) t -> ('n key * 'a)
  val min_binding_opt: ('n, 'a) t -> ('n key * 'a) option
  val max_binding: ('n, 'a) t -> ('n key * 'a)
  val max_binding_opt: ('n, 'a) t -> ('n key * 'a) option
  val choose: ('n, 'a) t -> ('n key * 'a)
  val choose_opt: ('n, 'a) t -> ('n key * 'a) option
  val split: 'n key -> ('n, 'a) t -> ('n, 'a) t * 'a option * ('n, 'a) t
  val find: 'n key -> ('n, 'a) t -> 'a
  val find_opt: 'n key -> ('n, 'a) t -> 'a option
  val find_first: ('n key -> bool) -> ('n, 'a) t -> 'n key * 'a
  val find_first_opt: ('n key -> bool) -> ('n, 'a) t -> ('n key * 'a) option
  val find_last: ('n key -> bool) -> ('n, 'a) t -> 'n key * 'a
  val find_last_opt: ('n key -> bool) -> ('n, 'a) t -> ('n key * 'a) option
  val map: ('a -> 'b) -> ('n, 'a) t -> ('n, 'b) t
  val mapi: ('n key -> 'a -> 'b) -> ('n, 'a) t -> ('n, 'b) t
  val to_seq : ('n, 'a) t -> ('n key * 'a) Seq.t
  val to_rev_seq : ('n, 'a) t -> ('n key * 'a) Seq.t
  val to_seq_from : 'n key -> ('n, 'a) t -> ('n key * 'a) Seq.t
  val add_seq : ('n key * 'a) Seq.t -> ('n, 'a) t -> ('n, 'a) t
  val of_seq : ('n key * 'a) Seq.t -> ('n, 'a) t
end
