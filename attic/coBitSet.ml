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

module CoIndexSet : S1 with type 'a element = 'a Fix.Indexing.index = struct
  module IS = BitSet.IndexSet

  (* Elements are assumed to have a natural total order. *)

  type 'a element = 'a Fix.Indexing.index

  (* Sets. *)

  type 'a t =
    | Pos of 'a IS.t
    | Neg of 'a IS.t

  (* The empty set. *)

  let empty = Pos IS.empty

  (* [is_empty s] tells whether [s] is the empty set. *)

  let is_empty = function
    | Pos s -> IS.is_empty s
    | Neg _ -> false

  (* The full set. *)

  let full = Neg IS.empty

  (* [is_empty s] tells whether [s] is the empty set. *)

  let is_full = function
    | Neg s -> IS.is_empty s
    | Pos _ -> false

  (* [singleton x] returns a singleton set containing [x] as its only
     element. *)

  let singleton x = Pos (IS.singleton x)

  (* [is_singleton s] tests whether [s] is a singleton set. *)

  let is_singleton = function
    | Pos s -> IS.is_singleton s
    | Neg _ -> false

  (* [cardinal s] returns the cardinal of [s]. *)

  let cardinal = function
    | Pos s -> (`Pos, IS.cardinal s)
    | Neg s -> (`Neg, IS.cardinal s)

  (* [mem x s] returns [true] if and only if [x] appears in the set
     [s]. *)

  let mem x = function
    | Pos s -> IS.mem x s
    | Neg s -> not (IS.mem x s)

  (* [add x s] returns a set whose elements are all elements of [s],
     plus [x]. *)

  let add x = function
    | Pos s -> Pos (IS.add x s)
    | Neg s -> Neg (IS.remove x s)

  (* [remove x s] returns a set whose elements are all elements of
     [s], except [x]. *)

  let remove x = function
    | Pos s -> Pos (IS.remove x s)
    | Neg s -> Neg (IS.add x s)

  (* [union s1 s2] returns the union of the sets [s1] and [s2]. *)

  let union s1 s2 =
    match s1, s2 with
    | Neg s1, Neg s2 -> Neg (IS.inter s1 s2)
    | Pos s1, Pos s2 -> Pos (IS.union s1 s2)
    | Pos s1, Neg s2 | Neg s2, Pos s1 ->
      Neg (IS.diff s2 s1)

  (* [inter s t] returns the set intersection of [s] and [t], that is,
     $s\cap t$. *)

  let inter s1 s2 =
    match s1, s2 with
    | Pos s1, Pos s2 -> Pos (IS.inter s1 s2)
    | Neg s1, Neg s2 -> Neg (IS.union s1 s2)
    | Pos s1, Neg s2 | Neg s2, Pos s1 ->
      Pos (IS.diff s1 s2)

  (* [disjoint s1 s2] returns [true] if and only if the sets [s1] and
     [s2] are disjoint, i.e. iff their intersection is empty. *)

  let disjoint s1 s2 =
    match s1, s2 with
    | Pos s1, Pos s2 -> IS.disjoint s1 s2
    | Neg _, Neg _ -> false
    | Pos s1, Neg s2 | Neg s2, Pos s1 ->
      IS.subset s1 s2

  (* [compare] is an ordering over sets. *)

  let compare s1 s2 =
    match s1, s2 with
    | Pos s1, Pos s2 | Neg s2, Neg s1 -> IS.compare s1 s2
    | Pos _, Neg _ -> -1
    | Neg _, Pos _ -> 1

  (* [equal] implements equality over sets. *)

  let equal s1 s2 =
    match s1, s2 with
    | Pos s1, Pos s2 | Neg s1, Neg s2 -> IS.equal s1 s2
    | Pos _, Neg _ | Neg _, Pos _ -> false

  (* [subset] implements the subset predicate over sets. *)

  let subset s1 s2 =
    match s1, s2 with
    | Pos s1, Pos s2 -> IS.subset s1 s2
    | Neg s1, Neg s2 -> IS.subset s2 s1
    | Pos s1, Neg s2 -> IS.disjoint s1 s2
    | Neg _ , Pos _ -> false

  (* [quick_subset s1 s2] is a fast test for the set inclusion [s1 ⊆ s2].

     The sets [s1] and [s2] must be nonempty.

     It must be known ahead of time that either [s1] is a subset of [s2] or
     these sets are disjoint: that is, [s1 ⊆ s2 ⋁ s1 ∩ s2 = ∅] must hold.

     Under this hypothesis, [quick_subset s1 s2] can be implemented simply
     by picking an arbitrary element of [s1] (if there is one) and testing
     whether it is a member of [s2]. *)
  let quick_subset s1 s2 =
    match s1, s2 with
    | Pos s1, Pos s2 -> IS.quick_subset s1 s2
    | Neg s1, Pos s2 -> not (IS.quick_subset s1 s2)
    | Pos s1, Neg s2 -> not (IS.quick_subset s2 s1)
    | Neg s1, Neg s2 -> IS.quick_subset s2 s1

  let diff s1 s2 =
    match s1, s2 with
    | Pos s1, Pos s2 -> Pos (IS.diff s1 s2)
    | Neg s1, Pos s2 -> Neg (IS.union s1 s2)
    | Pos s1, Neg s2 -> Pos (IS.inter s1 s2)
    | Neg s1, Neg s2 -> Pos (IS.diff s2 s1)

  let of_list xs = Pos (IS.of_list xs)

  let complement = function
    | Pos s -> Neg s
    | Neg s -> Pos s

  let partition l =
    let only_pos = ref true in
    let project = function Pos x -> x | Neg x -> only_pos := false; x in
    let l = List.map project l in
    let pos x = Pos x in
    try
      if !only_pos then
        List.map pos (IndexRefine.partition l)
      else
        let parts, total = IndexRefine.partition_and_total l in
        Neg total :: List.map pos parts
    with exn ->
      Printf.eprintf
        "Partition failed with %d inputs (strictly positive: %b):\n"
        (List.length l) !only_pos;
      List.iter (fun set ->
          Printf.eprintf "- cardinal=%d, set={" (IS.cardinal set);
          IS.iter (fun elt -> Printf.eprintf "%d,"
                      (elt : _ Fix.Indexing.index :> int)) set;
        ) l;
      raise exn

  let annotated_partition l =
    let negative = ref [] in
    let project (sg, tag) =
      match sg with
      | Pos x -> (x, `P tag)
      | Neg x ->
        let tag = (ref (-1), tag) in
        negative := tag :: !negative; (x, `N tag)
    in
    let l = List.map project l in
    let negative = !negative in
    try
      let parts, total = IndexRefine.annotated_partition_and_total l in
      let parts =
        List.mapi (fun i (part, tags) ->
            let tags =
              List.filter_map (function
                  | `P tag -> Some tag
                  | `N (r, _) -> r := i; None
                ) tags
            in
            let tags = List.fold_left
                (fun tags (r, tag) ->
                   if !r <> i then tag :: tags else tags)
                tags negative
            in
            Pos part, tags
          ) parts
      in
      match negative with
      | [] -> parts
      | neg -> (Neg total, List.map snd neg) :: parts
    with _ -> assert false
end


