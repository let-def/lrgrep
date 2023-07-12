open Fix.Indexing

type (+'n, !+'a) t
(** The type of maps from type ['n index] to type ['a]. *)

val empty: ('n, 'a) t
(** The empty map. *)

val is_empty: ('n, 'a) t -> bool
(** Test whether a map is empty or not. *)

val mem: 'n index -> ('n, 'a) t -> bool
(** [mem x m] returns [true] if [m] contains a binding for [x],
    and [false] otherwise. *)

val add: 'n index -> 'a -> ('n, 'a) t -> ('n, 'a) t
(** [add key data m] returns a map containing the same bindings as
    [m], plus a binding of [key] to [data]. If [key] was already bound
    in [m] to a value that is physically equal to [data],
    [m] is returned unchanged (the result of the function is
    then physically equal to [m]). Otherwise, the previous binding
    of [key] in [m] disappears.
    @before 4.03 Physical equality was not ensured. *)

val update: 'n index -> ('a option -> 'a option) -> ('n, 'a) t -> ('n, 'a) t
(** [update key f m] returns a map containing the same bindings as
    [m], except for the binding of [key]. Depending on the value of
    [y] where [y] is [f (find_opt key m)], the binding of [key] is
    added, removed or updated. If [y] is [None], the binding is
    removed if it exists; otherwise, if [y] is [Some z] then [key]
    is associated to [z] in the resulting map.  If [key] was already
    bound in [m] to a value that is physically equal to [z], [m]
    is returned unchanged (the result of the function is then
    physically equal to [m]).
    @since 4.06.0
*)

val singleton: 'n index -> 'a -> ('n, 'a) t
(** [singleton x y] returns the one-element map that contains a binding
    [y] for [x].
    @since 3.12.0
*)

val remove: 'n index -> ('n, 'a) t -> ('n, 'a) t
(** [remove x m] returns a map containing the same bindings as
    [m], except for [x] which is unbound in the returned map.
    If [x] was not in [m], [m] is returned unchanged
    (the result of the function is then physically equal to [m]).
    @before 4.03 Physical equality was not ensured. *)

val merge:
  ('n index -> 'a option -> 'b option -> 'c option) ->
  ('n, 'a) t -> ('n, 'b) t -> ('n, 'c) t
(** [merge f m1 m2] computes a map whose keys are a subset of the keys of
    [m1] and of [m2]. The presence of each such binding, and the
    corresponding value, is determined with the function [f].
    In terms of the [find_opt] operation, we have
    [find_opt x (merge f m1 m2) = f x (find_opt x m1) (find_opt x m2)]
    for any key [x], provided that [f x None None = None].
    @since 3.12.0
*)

val union: ('n index -> 'a -> 'a -> 'a option) -> ('n, 'a) t -> ('n, 'a) t -> ('n, 'a) t
(** [union f m1 m2] computes a map whose keys are a subset of the keys
    of [m1] and of [m2].  When the same binding is defined in both
    arguments, the function [f] is used to combine them.
    This is a special case of [merge]: [union f m1 m2] is equivalent
    to [merge f' m1 m2], where
    - [f' _key None None = None]
    - [f' _key (Some v) None = Some v]
    - [f' _key None (Some v) = Some v]
    - [f' key (Some v1) (Some v2) = f key v1 v2]

    @since 4.03.0
*)

val compare: ('a -> 'a -> int) -> ('n, 'a) t -> ('n, 'a) t -> int
(** Total ordering between maps.  The first argument is a total ordering
    used to compare data associated with equal keys in the two maps. *)

val equal: ('a -> 'a -> bool) -> ('n, 'a) t -> ('n, 'a) t -> bool
(** [equal cmp m1 m2] tests whether the maps [m1] and [m2] are
    equal, that is, contain equal keys and associate them with
    equal data.  [cmp] is the equality predicate used to compare
    the data associated with the keys. *)

val iter: ('n index -> 'a -> unit) -> ('n, 'a) t -> unit
(** [iter f m] applies [f] to all bindings in map [m].
    [f] receives the key as first argument, and the associated value
    as second argument.  The bindings are passed to [f] in increasing
    order with respect to the ordering over the type of the keys. *)

val fold: ('n index -> 'a -> 'b -> 'b) -> ('n, 'a) t -> 'b -> 'b
(** [fold f m init] computes [(f kN dN ... (f k1 d1 init)...)],
    where [k1 ... kN] are the keys of all bindings in [m]
    (in increasing order), and [d1 ... dN] are the associated data. *)

val for_all: ('n index -> 'a -> bool) -> ('n, 'a) t -> bool
(** [for_all f m] checks if all the bindings of the map
    satisfy the predicate [f].
    @since 3.12.0
*)

val exists: ('n index -> 'a -> bool) -> ('n, 'a) t -> bool
(** [exists f m] checks if at least one binding of the map
    satisfies the predicate [f].
    @since 3.12.0
*)

val filter: ('n index -> 'a -> bool) -> ('n, 'a) t -> ('n, 'a) t
(** [filter f m] returns the map with all the bindings in [m]
    that satisfy predicate [p]. If every binding in [m] satisfies [f],
    [m] is returned unchanged (the result of the function is then
    physically equal to [m])
    @since 3.12.0
    @before 4.03 Physical equality was not ensured.
*)

val filter_map: ('n index -> 'a -> 'b option) -> ('n, 'a) t -> ('n, 'b) t
(** [filter_map f m] applies the function [f] to every binding of
    [m], and builds a map from the results. For each binding
    [(k, v)] in the input map:
    - if [f k v] is [None] then [k] is not in the result,
    - if [f k v] is [Some v'] then the binding [(k, v')]
      is in the output map.

    For example, the following function on maps whose values are lists
    {[
      filter_map
        (fun _k li -> match li with [] -> None | _::tl -> Some tl)
        m
    ]}
    drops all bindings of [m] whose value is an empty list, and pops
    the first element of each value that is non-empty.

    @since 4.11.0
*)

val partition: ('n index -> 'a -> bool) -> ('n, 'a) t -> ('n, 'a) t * ('n, 'a) t
(** [partition f m] returns a pair of maps [(m1, m2)], where
    [m1] contains all the bindings of [m] that satisfy the
    predicate [f], and [m2] is the map with all the bindings of
    [m] that do not satisfy [f].
    @since 3.12.0
*)

val cardinal: ('n, 'a) t -> int
(** Return the number of bindings of a map.
    @since 3.12.0
*)

val bindings: ('n, 'a) t -> ('n index * 'a) list
(** Return the list of all bindings of the given map.
    The returned list is sorted in increasing order of keys with respect
    to the ordering [Ord.compare], where [Ord] is the argument
    given to {!Make}.
    @since 3.12.0
*)

val min_binding: ('n, 'a) t -> ('n index * 'a)
(** Return the binding with the smallest key in a given map
    (with respect to the [Ord.compare] ordering), or raise
    [Not_found] if the map is empty.
    @since 3.12.0
*)

val min_binding_opt: ('n, 'a) t -> ('n index * 'a) option
(** Return the binding with the smallest key in the given map
    (with respect to the [Ord.compare] ordering), or [None]
    if the map is empty.
    @since 4.05
*)

val max_binding: ('n, 'a) t -> ('n index * 'a)
(** Same as {!S.min_binding}, but returns the binding with
    the largest key in the given map.
    @since 3.12.0
*)

val max_binding_opt: ('n, 'a) t -> ('n index * 'a) option
(** Same as {!S.min_binding_opt}, but returns the binding with
    the largest key in the given map.
    @since 4.05
*)

val choose: ('n, 'a) t -> ('n index * 'a)
(** Return one binding of the given map, or raise [Not_found] if
    the map is empty. Which binding is chosen is unspecified,
    but equal bindings will be chosen for equal maps.
    @since 3.12.0
*)

val choose_opt: ('n, 'a) t -> ('n index * 'a) option
(** Return one binding of the given map, or [None] if
    the map is empty. Which binding is chosen is unspecified,
    but equal bindings will be chosen for equal maps.
    @since 4.05
*)

val split: 'n index -> ('n, 'a) t -> ('n, 'a) t * 'a option * ('n, 'a) t
(** [split x m] returns a triple [(l, data, r)], where
      [l] is the map with all the bindings of [m] whose key
    is strictly less than [x];
      [r] is the map with all the bindings of [m] whose key
    is strictly greater than [x];
      [data] is [None] if [m] contains no binding for [x],
      or [Some v] if [m] binds [v] to [x].
    @since 3.12.0
*)

val find: 'n index -> ('n, 'a) t -> 'a
(** [find x m] returns the current value of [x] in [m],
    or raises [Not_found] if no binding for [x] exists. *)

val find_opt: 'n index -> ('n, 'a) t -> 'a option
(** [find_opt x m] returns [Some v] if the current value of [x]
    in [m] is [v], or [None] if no binding for [x] exists.
    @since 4.05
*)

val find_first: ('n index -> bool) -> ('n, 'a) t -> 'n index * 'a
(** [find_first f m], where [f] is a monotonically increasing function,
    returns the binding of [m] with the lowest key [k] such that [f k],
    or raises [Not_found] if no such key exists.

    For example, [find_first (fun k -> Ord.compare k x >= 0) m] will return
    the first binding [k, v] of [m] where [Ord.compare k x >= 0]
    (intuitively: [k >= x]), or raise [Not_found] if [x] is greater than
    any element of [m].

    @since 4.05
*)

val find_first_opt: ('n index -> bool) -> ('n, 'a) t -> ('n index * 'a) option
(** [find_first_opt f m], where [f] is a monotonically increasing
    function, returns an option containing the binding of [m] with the
    lowest key [k] such that [f k], or [None] if no such key exists.
    @since 4.05
*)

val find_last: ('n index -> bool) -> ('n, 'a) t -> 'n index * 'a
(** [find_last f m], where [f] is a monotonically decreasing function,
    returns the binding of [m] with the highest key [k] such that [f k],
    or raises [Not_found] if no such key exists.
    @since 4.05
*)

val find_last_opt: ('n index -> bool) -> ('n, 'a) t -> ('n index * 'a) option
(** [find_last_opt f m], where [f] is a monotonically decreasing
    function, returns an option containing the binding of [m] with
    the highest key [k] such that [f k], or [None] if no such key
    exists.
    @since 4.05
*)

val map: ('a -> 'b) -> ('n, 'a) t -> ('n, 'b) t
(** [map f m] returns a map with same domain as [m], where the
    associated value [a] of all bindings of [m] has been
    replaced by the result of the application of [f] to [a].
    The bindings are passed to [f] in increasing order
    with respect to the ordering over the type of the keys. *)

val mapi: ('n index -> 'a -> 'b) -> ('n, 'a) t -> ('n, 'b) t
(** Same as {!S.map}, but the function receives as arguments both the
    key and the associated value for each binding of the map. *)

(** {1 Maps and Sequences} *)

val to_seq : ('n, 'a) t -> ('n index * 'a) Seq.t
(** Iterate on the whole map, in ascending order of keys
    @since 4.07 *)

val to_rev_seq : ('n, 'a) t -> ('n index * 'a) Seq.t
(** Iterate on the whole map, in descending order of keys
    @since 4.12 *)

val to_seq_from : 'n index -> ('n, 'a) t -> ('n index * 'a) Seq.t
(** [to_seq_from k m] iterates on a subset of the bindings of [m],
    in ascending order of keys, from key [k] or above.
    @since 4.07 *)

val add_seq : ('n index * 'a) Seq.t -> ('n, 'a) t -> ('n, 'a) t
(** Add the given bindings to the map, in order.
    @since 4.07 *)

val of_seq : ('n index * 'a) Seq.t -> ('n, 'a) t
(** Build a map from the given bindings
    @since 4.07 *)

val domain : ('n, _) t -> 'n IndexSet.t

val inflate : ('n index -> 'a) -> 'n IndexSet.t -> ('n, 'a) t

val filter_inflate : ('n index -> 'a option) -> 'n IndexSet.t -> ('n, 'a) t
