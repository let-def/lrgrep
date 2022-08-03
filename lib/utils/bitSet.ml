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

  (* [fold f s seed] invokes [f x accu], in turn, for each element [x]
     of the set [s]. Elements are presented to [f] in increasing
     order. The initial value of [accu] is [seed]; then, at each new
     call, its value is the value returned by the previous invocation
     of [f]. The value returned by [fold] is the final value of
     [accu]. In other words, if $s = \{ x_1, x_2, \ldots, x_n \}$,
     where $x_1 < x_2 < \ldots < x_n$, then [fold f s seed] computes
     $([f]\,x_n\,\ldots\,([f]\,x_2\,([f]\,x_1\,[seed]))\ldots)$. *)

  val fold: ('a element -> 'b -> 'b) -> 'a t -> 'b -> 'b

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
end

module type S0 = sig
  type element
  type t
  include S1 with type 'a element := element
              and type 'a t := t
end

module IntSet = struct
  (* A compressed (or should we say sparse?) bit set is a list of pairs
     of integers. The first component of every pair is an index, while
     the second component is a bit field. The list is sorted by order
     of increasing indices. *)

  type t =
    | N
    | C of int * int * t

  type element =
    int

  let word_size =
    Sys.word_size - 1

  let empty =
    N

  let is_empty = function
    | N ->
      true
    | C _ ->
      false

  let add i s =
    let ioffset = i mod word_size in
    let iaddr = i - ioffset
    and imask = 1 lsl ioffset in
    let rec add = function
      | N ->
        (* Insert at end. *)
        C (iaddr, imask, N)
      | C (addr, ss, qs) as s ->
        if iaddr < addr then
          (* Insert in front. *)
          C (iaddr, imask, s)
        else if iaddr = addr then
          (* Found appropriate cell, update bit field. *)
          let ss' = ss lor imask in
          if ss' = ss then
            s
          else
            C (addr, ss', qs)
        else
          (* Not there yet, continue. *)
          let qs' = add qs in
          if qs == qs' then
            s
          else
            C (addr, ss, qs')
    in
    add s

  let singleton i =
    add i N

  let remove i s =
    let ioffset = i mod word_size in
    let iaddr = i - ioffset
    and imask = 1 lsl ioffset in
    let rec remove = function
      | N ->
        N
      | C (addr, ss, qs) as s ->
        if iaddr < addr then
          s
        else if iaddr = addr then
          (* Found appropriate cell, update bit field. *)
          let ss' = ss land (lnot imask) in
          if ss' = 0 then
            qs
          else if ss' = ss then
            s
          else
            C (addr, ss', qs)
        else
          (* Not there yet, continue. *)
          let qs' = remove qs in
          if qs == qs' then
            s
          else
            C (addr, ss, qs')
    in
    remove s

  let rec fold f s accu =
    match s with
    | N ->
      accu
    | C (base, ss, qs) ->
      loop f qs base ss accu

  and loop f qs i ss accu =
    if ss = 0 then
      fold f qs accu
    else
      (* One could in principle check whether [ss land 0x3] is zero and if
         so move to [i + 2] and [ss lsr 2], and similarly for various sizes.
         In practice, this does not seem to make a measurable difference. *)
      loop f qs (i + 1) (ss lsr 1) (if ss land 1 = 1 then f i accu else accu)

  let map f t =
    fold (fun x xs -> add (f x) xs) t empty

  let iter f s =
    fold (fun x () -> f x) s ()

  let exists f t =
    let exception Found in
    match fold (fun elt () -> if f elt then raise Found) t () with
    | () -> false
    | exception Found -> true

  let is_singleton s =
    match s with
    | C (_, ss, N) ->
      (* Test whether only one bit is set in [ss]. We do this by turning
         off the rightmost bit, then comparing to zero. *)
      ss land (ss - 1) = 0
    | C (_, _, C _)
    | N ->
      false

  let cardinal s =
    fold (fun _ m -> m + 1) s 0

  let elements s =
    fold (fun tl hd -> tl :: hd) s []

  let rec subset s1 s2 =
    match s1, s2 with
    | N, _ ->
      true
    | _, N ->
      false
    | C (addr1, ss1, qs1), C (addr2, ss2, qs2) ->
      if addr1 < addr2 then
        false
      else if addr1 = addr2 then
        if (ss1 land ss2) <> ss1 then
          false
        else
          subset qs1 qs2
      else
        subset s1 qs2

  let rec quick_subset a1 ss1 = function
    | N -> false
    | C (a2, ss2, qs2) ->
      if a1 = a2 then
        ss1 land ss2 <> 0
      else
        (a1 > a2 && quick_subset a1 ss1 qs2)

  let quick_subset s1 s2 =
    match s1 with
    | N -> true
    | C (a1, ss1, _) ->
      (* We know that, by construction, ss1 is not empty.
         It suffices to test s2 also has elements in common with ss1 at address
         a1 to determine the quick_subset relation. *)
      quick_subset a1 ss1 s2

  let mem i s =
    subset (singleton i) s

  let rec union s1 s2 =
    match s1, s2 with
    | N, s
    | s, N ->
      s
    | C (addr1, ss1, qs1), C (addr2, ss2, qs2) ->
      if addr1 < addr2 then
        C (addr1, ss1, union qs1 s2)
      else if addr1 > addr2 then
        let s = union s1 qs2 in
        if s == qs2 then
          s2
        else
          C (addr2, ss2, s)
      else
        let ss = ss1 lor ss2 in
        let s = union qs1 qs2 in
        if ss == ss2 && s == qs2 then
          s2
        else
          C (addr1, ss, s)

  let rec inter s1 s2 =
    match s1, s2 with
    | N, _
    | _, N ->
      N
    | C (addr1, ss1, qs1), C (addr2, ss2, qs2) ->
      if addr1 < addr2 then
        inter qs1 s2
      else if addr1 > addr2 then
        inter s1 qs2
      else
        let ss = ss1 land ss2 in
        let s = inter qs1 qs2 in
        if ss = 0 then
          s
        else
        if (ss = ss1) && (s == qs1) then
          s1
        else
          C (addr1, ss, s)

  exception Found of int

  let choose s =
    try
      iter (fun x ->
          raise (Found x)
        ) s;
      raise Not_found
    with Found x ->
      x

  let minimum s =
    try
      iter (fun x ->
          raise (Found x)
        ) s;
      None
    with Found x ->
      Some x

  let rec compare s1 s2 =
    match s1, s2 with
      N, N ->  0
    | _, N ->  1
    | N, _ -> -1
    | C (addr1, ss1, qs1), C (addr2, ss2, qs2) ->
      if addr1 < addr2 then -1
      else if addr1 > addr2 then 1
      else if ss1 < ss2 then -1
      else if ss1 > ss2 then 1
      else compare qs1 qs2

  let equal s1 s2 =
    compare s1 s2 = 0

  let rec disjoint s1 s2 =
    match s1, s2 with
    | N, _
    | _, N ->
      true
    | C (addr1, ss1, qs1), C (addr2, ss2, qs2) ->
      if addr1 = addr2 then
        if (ss1 land ss2) = 0 then
          disjoint qs1 qs2
        else
          false
      else if addr1 < addr2 then
        disjoint qs1 s2
      else
        disjoint s1 qs2

  let rec diff s1 s2 =
    match s1, s2 with
    | N, _ | _, N -> s1
    | C (addr1, ss1, qs1), C (addr2, ss2, qs2) ->
      if addr1 < addr2
      then C (addr1, ss1, diff qs1 s2)
      else if addr1 > addr2
      then diff s1 qs2
      else
        let ss = ss1 land lnot ss2 in
        let d = diff qs1 qs2 in
        if ss = 0
        then d
        else C (addr1, ss, d)

  let lsb x = (x land -x)
  let compare_lsb x y = lsb x - lsb y

  let compare_minimum s1 s2 =
    match s1, s2 with
    | N, N -> 0
    | N, _ -> -1
    | _, N -> 1
    | C (addr1, ss1, _), C (addr2, ss2, _) ->
      match Int.compare addr1 addr2 with
      | 0 ->
        begin match compare_lsb ss1 ss2 with
          | 0 ->
            let ss1' = ss1 land lnot ss2 in
            let ss2' = ss2 land lnot ss1 in
            compare_lsb ss1' ss2'
          | n -> n
        end
      | n -> n

  let sorted_union xs = List.fold_right union xs empty

  let rec extract_unique_prefix addr2 ss2 = function
    | N -> N, N
    | C (addr1, ss1, qs1) as self ->
      if addr1 < addr2 then
        let prefix, suffix = extract_unique_prefix addr2 ss2 qs1 in
        C (addr1, ss1, prefix), suffix
      else if addr1 > addr2 || ss1 = ss2 || compare_lsb ss1 ss2 >= 0 then
        N, self
      else
        (* l and r have the same address, and
           l has some prefix that is not part of r (lsb l < lsb r)*)
        let prefix_mask = (lsb ss2) - 1 in
        let ss0 = ss1 land prefix_mask in
        assert (ss0 <> 0);
        let ss1 = ss1 land lnot prefix_mask in
        if ss1 = 0 then
          (C (addr1, ss0, N), qs1)
        else
          (C (addr1, ss0, N), C (addr1, ss1, qs1))

  let extract_unique_prefix l r =
    match l, r with
    | N, _ -> N, N
    | _, N -> invalid_arg "extract_unique_prefix: r < l"
    | l, C (addr2, ss2, _) -> extract_unique_prefix addr2 ss2 l

  let rec extract_shared_prefix = function
    | C (addr1, ss1, qs1), C (addr2, ss2, qs2)
      when addr1 = addr2 ->
      if ss1 = ss2 then
        let common, rest = extract_shared_prefix (qs1, qs2) in
        (C (addr1, ss1, common), rest)
      else
        let ss1' = ss1 land lnot ss2 in
        let ss2' = ss2 land lnot ss1 in
        let common_mask = (lsb ss1' - 1) land (lsb ss2' - 1) in
        let rest_mask = lnot common_mask in
        let common = match ss1 land common_mask with
          | 0 -> N
          | n -> C (addr1, n, N)
        in
        let qs1' = match ss1 land rest_mask with
          | 0 -> qs1
          | ss1' -> C (addr1, ss1', qs1)
        in
        let qs2' = match ss2 land rest_mask with
          | 0 -> qs2
          | ss2' -> C (addr2, ss2', qs2)
        in
        common, (qs1', qs2')
    | (l, r) -> N, (l, r)

  let extract_shared_prefix l r = extract_shared_prefix (l, r)

  let of_list xs = List.fold_left (fun xs x -> add x xs) empty xs
end

module type S1_int = S1 with type 'a element = int
module type S1_index = S1 with type 'a t = private IntSet.t
                           and type 'a element = 'a Fix.Indexing.index

module IndexSeq_int : S1_int = struct
  type 'a element = int
  type 'a t = IntSet.t
  include (IntSet : S0 with type element := int and type t := IntSet.t)
end

module IndexSet =
  (val (Obj.magic
          ((module IndexSeq_int)
           : (module S1_int))
        : (module S1_index)))

module Make (Element : sig
    type t = private int
    val of_int : int -> t
  end) : S0 with type element = Element.t =
struct
  type element = Element.t
  type t = IntSet.t
  let empty        = IntSet.empty
  let is_empty     = IntSet.is_empty
  let singleton    = (IntSet.singleton :> element -> t)
  let is_singleton = IntSet.is_singleton
  let cardinal     = IntSet.cardinal
  let choose t     = Element.of_int (IntSet.choose t)
  let minimum t = match IntSet.minimum t with
    | None -> None
    | Some x -> Some (Element.of_int x)
  let mem          = (IntSet.mem :> element -> t -> bool)
  let add          = (IntSet.add :> element -> t -> t)
  let remove       = (IntSet.remove :> element -> t -> t)
  let union        = IntSet.union
  let inter        = IntSet.inter
  let disjoint     = IntSet.disjoint
  let iter f t     = IntSet.iter (fun i -> f (Element.of_int i)) t
  let fold f t acc = IntSet.fold (fun i acc -> f (Element.of_int i) acc) t acc
  let map f t      = IntSet.map (fun i -> (f (Element.of_int i) : element :> int)) t
  let exists f t   = IntSet.exists (fun i -> f (Element.of_int i)) t
  let elements t   = List.map Element.of_int (IntSet.elements t)
  let compare      = IntSet.compare
  let equal        = IntSet.equal
  let subset       = IntSet.subset
  let quick_subset = IntSet.quick_subset
  let diff         = IntSet.diff

  let sorted_union  = IntSet.sorted_union
  let compare_minimum = IntSet.compare_minimum
  let extract_unique_prefix  = IntSet.extract_unique_prefix
  let extract_shared_prefix  = IntSet.extract_shared_prefix

  let of_list = (IntSet.of_list :> element list -> t)
end

