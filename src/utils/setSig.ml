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

  val is_not_empty: 'a t -> bool

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

  (** [minimum s] returns [Some x] where [x] is the smallest element of [s],
      or [None] if [s] is empty. *)
  val minimum: 'a t -> 'a element option

  (** [maximum s] returns [Some x] where [x] is the largest element of [s],
      or [None] if [s] is empty. *)
  val maximum: 'a t -> 'a element option

  (* [mem x s] returns [true] if and only if [x] appears in the set
     [s]. *)

  val mem: 'a element -> 'a t -> bool

  (* [add x s] returns a set whose elements are all elements of [s],
     plus [x]. If [x] was already in [s], [s] is returned unchanged. *)

  val add: 'a element -> 'a t -> 'a t

  (* [remove x s] returns a set whose elements are all elements of
     [s], except [x]. If [x] was not in [s], [s] is returned unchanged. *)

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
  val iteri: (int -> 'a element -> unit) -> 'a t -> unit

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

  (** [fold_right f acc x] computes [f acc x] for each element [x] of [s],
      in descending order. The initial value of [acc] is [seed]; the final
      value is the returned value. *)
  val fold_right: ('a -> 'b element -> 'a) -> 'a -> 'b t -> 'a

  (** [map f s] returns the set whose elements are [f x] for each [x] in [s]. *)
  val map: ('a element -> 'b element) -> 'a t -> 'b t

  (** [exists f s] returns [true] if [f x] is true for at least one element
      [x] in [s]. *)
  val exists: ('a element -> bool) -> 'a t -> bool

  (** [for_all f s] returns [true] if [f x] is true for all elements [x]
      in [s]. *)
  val for_all: ('a element -> bool) -> 'a t -> bool

  (* [elements s] is a list of all elements in the set [s]. *)

  val elements: 'a t -> 'a element list

  (* [compare] is an ordering over sets. *)

  val compare: 'a t -> 'a t -> int

  (* [equal] implements equality over sets. *)

  val equal: 'a t -> 'a t -> bool

  (* [subset s1 s2] returns [true] iff [s1 ⊆ s2]. *)

  val subset: 'a t -> 'a t -> bool

  (* [quick_subset s1 s2] is a fast test for the set inclusion [s1 ⊆ s2].

     The sets [s1] and [s2] must be nonempty.

     It must be known ahead of time that either [s1] is a subset of [s2] or
     these sets are disjoint: that is, [s1 ⊆ s2 ⋁ s1 ∩ s2 = ∅] must hold.

     Under this hypothesis, [quick_subset s1 s2] can be implemented simply
     by picking an arbitrary element of [s1] (if there is one) and testing
     whether it is a member of [s2]. *)
  val quick_subset: 'a t -> 'a t -> bool

  (** [diff s1 s2] returns the set of elements that are in [s1] but not in [s2]. *)
  val diff : 'a t -> 'a t -> 'a t

  (** {1 Decomposing sets}

      These functions implement the [Refine.DECOMPOSABLE] interface.
      We cannot reference it here as [Refine] is implemented using bitsets,
      that would create a reference cycle.
  *)

  (** [compare_minimum s1 s2] compares two sets by their least elements.
      Returns a negative number if the least element of [s1] is smaller,
      a positive number if the least element of [s2] is smaller, or zero
      if both sets are empty or have equal least elements. *)
  val compare_minimum : 'a t -> 'a t -> int

  (** [extract_unique_prefix s1 s2] splits [s1] into two sets:
      - The first set contains elements strictly smaller than all elements of [s2]
      - The second set contains the remaining elements of [s1]

      {b Raises} if [s2] is empty. *)
  val extract_unique_prefix : 'a t -> 'a t -> 'a t * 'a t

  (** [extract_shared_prefix s1 s2] decomposes [s1] and [s2] into:
      - A common prefix (elements present in both [s1] and [s2])
      - The remaining parts of [s1] and [s2]

      {b Returns} [(common, (rest1, rest2))] where:
      - [common] contains elements that are part of both [s1] and [s2]
      - [s1 = common U rest1] and [s2 = common U rest2] *)
  val extract_shared_prefix : 'a t -> 'a t -> 'a t * ('a t * 'a t)

  (** [sorted_union s] computes the union of an ordered list of sets.
      This is an optimized special case of [union] that assumes the list
      is already sorted by the set order. *)
  val sorted_union : 'a t list -> 'a t

  (** [of_list xs] creates a set from a list of elements. *)
  val of_list : 'a element list -> 'a t

  (** [init_interval i j] creates a set containing all integers from [i] to [j].
      The behavior is undefined if [i > j]. *)
  val init_interval : 'a element -> 'a element -> 'a t

  (** [init_subset i j f] creates a set containing all elements [x] such that
      [i ≤ x ≤ j] and [f x] returns [true]. *)
  val init_subset : 'a element -> 'a element -> ('a element -> bool) -> 'a t

  (** [filter f s] returns the set of elements [x] in [s] such that [f x]
      returns [true]. *)
  val filter : ('a element -> bool) -> 'a t -> 'a t

  (** [filter_map f s] returns the set of elements [y] such that [f x = Some y]
      for some element [x] in [s]. *)
  val filter_map : ('a element -> 'b element option) -> 'a t -> 'b t

  (** [split x s] returns [(s1, found, s2)] where:
      - [s1] contains all elements strictly smaller than [x]
      - [s2] contains all elements strictly larger than [x]
      - [found] is [true] if [x] is in [s]
      - [s = s1 U {x} U s2] if [found] is [true] *)
  val split: 'a element -> 'a t -> 'a t * bool * 'a t

  (** [find f s] returns the first element [x] in [s] such that [f x] is true.
      Raises [Not_found] if no such element exists. *)
  val find : ('a element -> bool) -> 'a t -> 'a element

  (** [find_map f s] returns [Some y] where [y] is the first element such that
      [f x = Some y] for some [x] in [s], or [None] if no such element exists. *)
  val find_map : ('a element -> 'b option) -> 'a t -> 'b option

  (** [to_seq s] returns a sequence of all elements in [s] in ascending order. *)
  val to_seq : 'a t -> 'a element Seq.t

  (** [bind m f] returns the union of [f x] for all elements [x] in [m]. *)
  val bind : 'a t -> ('a element -> 'b t) -> 'b t

  (** Split a set into consecutive "runs" of elements that share the same class.

      {b Parameters}
      - [cls : 'a element → 'b element] that assigns a class to each element.
      - [xs  : 'a t] – the input set to be split.

      {b Returns}
      A list of pairs.  Each pair is made of a class (the result of [cls] for
      the run) and the subset of the original elements that belong to that run
      (preserving the original order). *)
  val split_by_run : ('a element -> 'b element) -> 'a t -> ('b element * 'a t) list

  (** [fused_inter_union s1 s2 ~acc] returns [union (inter s1 s2) acc].
      This is an optimized version that computes the intersection and union
      in a single pass. *)
  val fused_inter_union : 'a t -> 'a t -> acc:'a t -> 'a t

  (** [rev_map_elements s f] returns a list of [f x] for all elements [x]
      in [s], built from the highest to the lowest element. *)
  val rev_map_elements: 'a t -> ('a element -> 'b) -> 'b list

  (** [map_to_array s f] returns an array of [f x] for all elements [x] in
      [s]. *)
  val map_to_array : 'a t -> ('a element -> 'b) -> 'b array

  (** [rank x s] returns the number of elements strictly smaller than [x] in the
      set [s]. *)
  val rank : 'a element -> 'a t -> int
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
  (** {1 StdMapS1}

      This signature provides a parameterized version of the standard library's
      {!StdMap.S} signature. It is lifted to work with parameterized keys
      ['n key] instead of ground ones [key].

      {b Note.} The type [('n, 'a) t] represents a map indexed by keys
      parameterized by  ['n] and carrying values of type ['a]. This is a direct
      adaptation of the standard library's {!StdMap.S} signature, generalized to
      support parameterized keys.

      @see [StdMap.S] (standard library) for the original signature upon which
      this is based.
  *)
  type 'n key
  type ('n, 'a) t
  (** {2 Basic operations} *)

  (** [empty] is the empty map. *)
  val empty: ('n, 'a) t

  (** [is_empty m] returns [true] if [m] is the empty map. *)
  val is_empty: ('n, 'a) t -> bool

  (** [mem k m] returns [true] if key [k] is bound in map [m]. *)
  val mem:  'n key -> ('n, 'a) t -> bool

  (** [add k d m] returns a map that is the extension of [m] with binding
      [k ↦ d]. If [k] was already bound in [m], the old binding is replaced
      by the new one. *)
  val add: 'n key -> 'a -> ('n, 'a) t -> ('n, 'a) t

  (** [update k f m] returns a map same as [m], except that the binding for
      [k] is modified by [f]. If the binding for [k] is present in [m],
      [f] is called with the current value, otherwise [f] is called with
      [None]. The result of [f] determines the binding state for [k] in the
      resulting map. *)
  val update: 'n key -> ('a option -> 'a option) -> ('n, 'a) t -> ('n, 'a) t

  (** [singleton k d] returns a map containing the single binding [k ↦ d]. *)
  val singleton: 'n key -> 'a -> ('n, 'a) t

  (** [remove m k] returns a map containing the same bindings as [m], except
      that [k] is unbound in the result. *)
  val remove: 'n key -> ('n, 'a) t -> ('n, 'a) t

  (** [merge f m1 m2] returns a map with the same bindings as [m1] and [m2],
      except that bindings that appear in both maps are combined using [f].
      Specifically, for each key [k], if [k] is bound in [m1] to [a1] and in
      [m2] to [a2], then [k] is bound in the result to [f k a1 a2]. *)
  val merge:
    ('n key -> 'a option -> 'b option -> 'c option) -> ('n, 'a) t -> ('n, 'b) t -> ('n, 'c) t

  (** [union f m1 m2] is an alias for [merge] with the union combinator. *)
  val union: ('n key -> 'a -> 'a -> 'a option) -> ('n, 'a) t -> ('n, 'a) t -> ('n, 'a) t
  (** {2 Comparisons} *)

  (** [compare cmp m1 m2] compares the two maps [m1] and [m2] using [cmp]
      to compare values. Returns a negative number if [m1] is strictly
      smaller than [m2], a positive number if [m1] is strictly greater
      than [m2], or zero if they are equal. *)
  val compare: ('a -> 'a -> int) -> ('n, 'a) t -> ('n, 'a) t -> int

  (** [equal eq m1 m2] returns [true] if the two maps are equal up to
      [eq], which compares values. *)
  val equal: ('a -> 'a -> bool) -> ('n, 'a) t -> ('n, 'a) t -> bool
  (** {2 Iteration} *)

  (** [iter f m] invokes [f k d] for each binding [k ↦ d] in [m].
      Bindings are presented in ascending key order. *)
  val iter: ('n key -> 'a -> unit) -> ('n, 'a) t -> unit

  (** [fold f m seed] computes [f k d acc] for each binding in [m],
      in ascending key order. The initial value of [acc] is [seed];
      the final value is the returned value. *)
  val fold: ('n key -> 'a -> 'b -> 'b) -> ('n, 'a) t -> 'b -> 'b

  (** [for_all f m] returns [true] if [f k d] is true for all bindings
      [k ↦ d] in [m]. *)
  val for_all: ('n key -> 'a -> bool) -> ('n, 'a) t -> bool

  (** [exists f m] returns [true] if [f k d] is true for at least one
      binding [k ↦ d] in [m]. *)
  val exists: ('n key -> 'a -> bool) -> ('n, 'a) t -> bool
  (** {2 Filtering} *)

  (** [filter f m] returns the map with the same bindings as [m], but
      only those bindings [k ↦ d] for which [f k d] returns [true]. *)
  val filter: ('n key -> 'a -> bool) -> ('n, 'a) t -> ('n, 'a) t

  (** [filter_map f m] returns the map with bindings [k ↦ d'] for each
      binding [k ↦ d] in [m] such that [f k d] returns [Some d']. *)
  val filter_map: ('n key -> 'a -> 'b option) -> ('n, 'a) t -> ('n, 'b) t

  (** [partition f m] returns the pair [(m1, m2)] where [m1] contains
      all bindings [k ↦ d] in [m] such that [f k d] is true, and [m2]
      contains the others. *)
  val partition: ('n key -> 'a -> bool) -> ('n, 'a) t -> ('n, 'a) t * ('n, 'a) t
  (** {2 Cardinality} *)

  (** [cardinal m] returns the number of bindings in [m]. *)
  val cardinal: ('n, 'a) t -> int

  (** {2 Bindings extraction} *)

  (** [bindings m] is the list of all bindings in [m], in ascending order. *)
  val bindings: ('n, 'a) t -> ('n key * 'a) list

  (** [min_binding m] returns the pair [(k, d)] where [k] is the smallest
      key bound in [m] and [d] is its binding. Raises [Not_found] if [m] is
      empty. *)
  val min_binding: ('n, 'a) t -> ('n key * 'a)

  (** [min_binding_opt m] returns [Some (k, d)] where [(k, d)] is the
      smallest key bound in [m], or [None] if [m] is empty. *)
  val min_binding_opt: ('n, 'a) t -> ('n key * 'a) option

  (** [max_binding m] returns the pair [(k, d)] where [k] is the largest
      key bound in [m] and [d] is its binding. Raises [Not_found] if [m] is
      empty. *)
  val max_binding: ('n, 'a) t -> ('n key * 'a)

  (** [max_binding_opt m] returns [Some (k, d)] where [(k, d)] is the
      largest key bound in [m], or [None] if [m] is empty. *)
  val max_binding_opt: ('n, 'a) t -> ('n key * 'a) option

  (** [choose m] returns an arbitrarily chosen binding [(k, d)] from [m].
      Raises [Not_found] if [m] is empty. *)
  val choose: ('n, 'a) t -> ('n key * 'a)

  (** [choose_opt m] returns [Some (k, d)] if [k] is bound in [m],
      or [None] if [m] is empty. *)
  val choose_opt: ('n, 'a) t -> ('n key * 'a) option
  (** {2 Splitting} *)

  (** [split k m] returns [(m1, opt, m2)] where:
      - [m1] contains all bindings with keys strictly smaller than [k]
      - [m2] contains all bindings with keys strictly larger than [k]
      - [opt] is [Some d] if [k] is bound to [d] in [m], or [None] otherwise *)
  val split: 'n key -> ('n, 'a) t -> ('n, 'a) t * 'a option * ('n, 'a) t

  (** {2 Find operations} *)

  (** [find k m] returns the value [d] associated with key [k] in [m].
      Raises [Not_found] if [k] is not bound in [m]. *)
  val find: 'n key -> ('n, 'a) t -> 'a

  (** [find_opt k m] returns [Some d] if [k] is bound to [d] in [m],
      or [None] otherwise. *)
  val find_opt: 'n key -> ('n, 'a) t -> 'a option

  (** [find_first f m] returns the first binding [(k, d)] such that
      [f k d] is true, or raises [Not_found] if no such binding exists. *)
  val find_first: ('n key -> bool) -> ('n, 'a) t -> 'n key * 'a

  (** [find_first_opt f m] returns [Some (k, d)] for the first binding
      such that [f k d] is true, or [None] if no such binding exists. *)
  val find_first_opt: ('n key -> bool) -> ('n, 'a) t -> ('n key * 'a) option

  (** [find_last f m] returns the last binding [(k, d)] such that
      [f k d] is true, or raises [Not_found] if no such binding exists. *)
  val find_last: ('n key -> bool) -> ('n, 'a) t -> 'n key * 'a

  (** [find_last_opt f m] returns [Some (k, d)] for the last binding
      such that [f k d] is true, or [None] if no such binding exists. *)
  val find_last_opt: ('n key -> bool) -> ('n, 'a) t -> ('n key * 'a) option
  (** {2 Mapping} *)

  (** [map f m] returns the map with the same keys as [m], where each
      value [d] has been replaced by [f d]. *)
  val map: ('a -> 'b) -> ('n, 'a) t -> ('n, 'b) t

  (** [mapi f m] returns the map with the same keys as [m], where each
      value has been replaced by [f k d] for the corresponding key [k]. *)
  val mapi: ('n key -> 'a -> 'b) -> ('n, 'a) t -> ('n, 'b) t

  (** {2 Sequences} *)

  (** [to_seq m] returns a sequence of all bindings in [m], in ascending
      key order. *)
  val to_seq : ('n, 'a) t -> ('n key * 'a) Seq.t

  (** [to_rev_seq m] returns a sequence of all bindings in [m], in descending
      key order. *)
  val to_rev_seq : ('n, 'a) t -> ('n key * 'a) Seq.t

  (** [to_seq_from k m] returns a sequence of all bindings in [m] with keys
      greater than or equal to [k]. *)
  val to_seq_from : 'n key -> ('n, 'a) t -> ('n key * 'a) Seq.t

  (** [add_seq s m] returns the map that is the union of [m] and all bindings
      in sequence [s]. *)
  val add_seq : ('n key * 'a) Seq.t -> ('n, 'a) t -> ('n, 'a) t

  (** [of_seq s] returns the map containing all bindings from sequence [s]. *)
  val of_seq : ('n key * 'a) Seq.t -> ('n, 'a) t
end
