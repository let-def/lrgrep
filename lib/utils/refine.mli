
(** [Refine] computes the refined partition of a list of overlapping set: the
    smallest list of non-overlapping sets such that each set is a subset of one
    of the input set.

    This is useful when computing the set of transitions of a DFA when the
    alphabet is large: rather than representing a transition for each letter
    separately, we group them by subset having the same destination.
*)

module type DECOMPOSABLE = sig

  type t
  (** The abstract type representing sets that we want to refine. *)

  val is_empty : t -> bool
  (** A set can be tested for emptiness. *)

  val compare_minimum : t -> t -> int
  (** Order two sets by their minimal element.

      If we had an function to extract the minimal element, then
      [compare_minimum x y = Element.compare (minimum x) (minimum y)].

      This function is never called on an empty set.
  *)

  val interval_union : t list -> t
  (** Computes the union of a list of set.

      However, [Refine.Make] makes stronger guarantees when calling
      [interval_union] that can be used to speed-up the computation.
      Sets in the list guaranteed to be sorted by [compare_minimum] and, if it
      existed, by [compare_maximum].

      That is:
      - each set [s_i] is a subset of the interval
          [minimum s_i .. maximum s_i],
      - intervals are totally ordered and non-overlapping
          [maximum s_i < minimum s_(i+1)]
     *)

  val extract_prefix : t -> t -> t * t
  (** When [compare_minimum s1 s2 < 0],
      [extract_prefix s1 s2] splits [s1] in [s1_head, s1_tail] such that
      [s1_head] is made of elements of [s1] that are strictly smaller than any
      element in [s2] and [s1_tail] is made of other elements.

      That is, assuming s1 < s2 by [compare_minimum]:
      - forall h in s1_head, t1 in s1_tail and h < t1
      - forall h in s1_head, t2 in s2 and h < t2
      - [s1 = s1_head U s1_tail]

      FIXME: Rename to extract_unique_prefix?
  *)

  val extract_common : t -> t -> t * (t * t)
  (** When [compare_minimum s1 s2 = 0],
      [extract_common s1 s2 = (common, s1', s2')] such that

      - [common] are elements that are both in [s1] and [s2]
      - [common] elements are smaller than any element in [s1'] and [s2']
      - [s1 = common U s1'] and [s2 = common U s2']

      FIXME: Rename to extract_shared_prefix?
  *)
end

(** The type of refined sets *)
module type S = sig

  type t
  (** Type of a set, like [DECOMPOSABLE.t] *)

  val partition : t list -> t list
  (* Returns the refined partition of a list of sets. *)

  val partition_and_total : t list -> t list * t
  (* Returns the refined partition of a list of sets as well as the union of
     all of them. *)
end

module Make (Set : DECOMPOSABLE) : S with type t := Set.t
