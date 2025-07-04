(******************************************************************************)
(*                                                                            *)
(*                                    Fix                                     *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Library General Public License version 2, with a         *)
(*  special exception on linking, as described in the file LICENSE.           *)
(*                                                                            *)
(******************************************************************************)

(**This module offers {b a safe API for manipulating indices into fixed-size
   arrays}.

   It provides support for constructing finite sets at the type level and
   for encoding the inhabitants of these sets as runtime integers. These
   runtime integers are statically branded with the name of the set that
   they inhabit, so two inhabitants of two distinct sets cannot be
   inadvertently confused. *)

(**If [n] is a type-level name for a finite set, then a value of type
   [n cardinal] is a runtime integer that represents the cardinal of
   the set [n].

   In the following, the functor {!Gensym} allows creating open-ended
   sets, which can grow over time. If [n] is such a set, then a value
   of type [n cardinal] can be thought of as the as-yet-undetermined
   cardinal of the set. *)
type 'n cardinal

(**If [n] is the cardinal of the set [n], then [cardinal n] returns the
   cardinal of this set, as a concrete integer.

   In the following, the functor {!Gensym} allows creating open-ended sets,
   which can grow over time. If [n] is such a set, then calling [cardinal n]
   has the side-effect of freezing this set, thereby fixing its cardinal:
   thereafter, calling [fresh] becomes forbidden, so no new elements can be
   added. *)
val cardinal : 'n cardinal -> int

type (_, _) eq = Refl : ('a, 'a) eq

val assert_equal_cardinal : 'n cardinal -> 'm cardinal -> ('n, 'm) eq
val check_equal_cardinal : 'n cardinal -> 'm cardinal -> ('n, 'm) eq option

(**If [n] is a type-level name for a finite set, then a value [i] of type
   [n index] is an integer value that is guaranteed to inhabit the set [n].

   If [n] has type [n cardinal], then [0 <= i < cardinal n] must hold.

   The main reason why elements of a finite set are called "indices" is that
   their main purpose is to serve as indices in fixed-size vectors. See the
   submodule [Vector] below. *)
type 'n index =
  private int

(**A new type-level set is created by an application of the functors {!Const},
   {!Gensym}, and {!Sum} below. Each functor application creates a fresh type
   name [n]. More precisely, each functor application returns a module whose
   signature is {!CARDINAL}: it contains both a fresh abstract type [n] and a
   value [n] of type [n cardinal] that represents the cardinal of the
   newly-created set. *)
module type CARDINAL = sig
  type n
  val n : n cardinal
end

(**[Const(struct let cardinal = c end)] creates a fresh type-level name for a
   set whose cardinal is [c]. [c] must be nonnegative. *)
module Const (X : sig val cardinal : int end) : CARDINAL

module type UNSAFE_CARDINAL = sig
  type 'a t
  module Const(M : sig type t val cardinal : int end) : CARDINAL with type n = M.t t
  module Eq(M : sig type t include CARDINAL end) : sig val eq : (M.t t, M.n) eq end
end

module Unsafe_cardinal() : UNSAFE_CARDINAL

(**The function {!const} is a value-level analogue of the functor {!Const}. *)
val const : int -> (module CARDINAL)

(**{!Empty} contains a type-level name for the empty set. *)
module Empty: CARDINAL

(**{!Unit} contains a type-level name for the singleto set. *)
module Unit: sig
  include CARDINAL
  val element : n index
end

(**[Gensym()] creates an open-ended type-level set, whose cardinality is not
   known a priori. As long as the cardinal of the set has not been observed by
   invoking {!val-cardinal}, new elements can be added to the set by invoking
   [fresh]. *)
module Gensym () : sig

  (** The type-level name [n] of the set and its cardinal [n]. *)
  include CARDINAL

  (**If [cardinal n] has not been invoked yet, then [fresh()] adds a new
     element to the set. Otherwise, calling [fresh] is forbidden and causes a
     runtime failure. *)
  val fresh : unit -> n index

end

(**The type [('l, 'r) either] represents the disjoint sum of the types ['l]
   and ['r]. It is isomorphic to the type [either] found in [Stdlib.Either]
   in OCaml 4.12.0. *)
type ('l, 'r) either =
  | L of 'l
  | R of 'r

(**[Sum(L)(R)] creates a new type-level set, which is the disjoint sums of the
   sets [L] and [R]. The functor application [Sum(L)(R)] involves a call to
   [cardinal L.n], thereby fixing the cardinal of the set [L], if it was an
   open-ended set. The cardinal of the set [R] is not fixed: if [R] is an
   open-ended set, then the new set is open-ended as well, and it is still
   permitted to add new elements to [R] by calling [R.fresh()]. Fixing the
   cardinal of the new set fixes the cardinal of [R]. *)
module Sum : sig
  type (_, _) n

  val cardinal : 'l cardinal -> 'r cardinal -> ('l, 'r) n cardinal
  val inj_l : 'l index -> ('l, 'r) n index
  val inj_r : 'l cardinal -> 'r index -> ('l, 'r) n index
  val prj : 'l cardinal -> ('l, 'r) n index -> ('l index, 'r index) either

  (**The signature {!SUM} extends {!CARDINAL} with an explicit isomorphism between
     the set [n] and the disjoint sum [l + r]. The functions [inj_l] and [inj_r]
     convert an index into [l] or an index into [r] into an index into [n].
     Conversely, the function [prj] converts an index into [r] into either an
     index into [l] or an index into [r]. *)
  module type S = sig
    type l and r
    type nonrec n = (l, r) n
    include CARDINAL with type n := n
    val inj_l : l index -> n index
    val inj_r : r index -> n index
    val prj : n index -> (l index, r index) either
  end

  module Make (L : CARDINAL)(R : CARDINAL) : S with type l := L.n
                                                and type r := R.n

  (**The function {!sum} is a value-level analogue of the functor {!Sum}. *)
  val make : 'l cardinal -> 'r cardinal ->
             (module S with type l = 'l and type r = 'r)
end

module Prod : sig
  type (_, _) n

  val cardinal : 'l cardinal -> 'r cardinal -> ('l, 'r) n cardinal
  val inj : 'l cardinal -> 'l index -> 'r index -> ('l, 'r) n index
  val prj : 'l cardinal -> ('l, 'r) n index -> 'l index * 'r index

  module type S = sig
    type l and r
    type nonrec n = (l, r) n
    include CARDINAL with type n := n
    val inj : l index -> r index -> n index
    val prj : n index -> l index * r index
  end

  module Make (L : CARDINAL)(R : CARDINAL) : S with type l := L.n
                                                and type r := R.n

  val make : 'l cardinal -> 'r cardinal ->
             (module S with type l = 'l and type r = 'r)
end

(**The submodule {!Index} allows safely manipulating indices
   into a finite set. *)
module Index : sig

  type 'n t = 'n index

  (**If [n] is the cardinal of the type-level set [n], then [of_int n] casts
     an integer [i] of type [int] into an index: that is, [of_int n i] returns
     [i] at type [n index]. The integer [i] must lie in the semi-open interval
     [\[0, n)]. This is enforced by a runtime check. Calling [of_int n i]
     fixes the cardinal [n]. *)
  val of_int : 'n cardinal -> int -> 'n index

  (**{!to_int} casts an index [i] back to an ordinary integer value. *)
  val to_int : 'n index -> int

  (**[iter n yield] calls [yield i] successively for every index in the range
     [\[0, n)], in increasing order. *)
  val iter : 'n cardinal -> ('n index -> unit) -> unit

  (**[rev_iter n yield] calls [yield i] successively for every index in the range
     [\[0, n)], in decreasing order. *)
  val rev_iter : 'n cardinal -> ('n index -> unit) -> unit

  (** [pred i] is the index immediately before [i], if [i] is non-zero *)
  val pred : 'n index -> 'n index option

  (**This exception is raised by an iterator (created by {!enumerate}) that is
     queried after it has been exhausted. *)
  exception End_of_set

  (**[enumerate n] returns an imperative iterator, which produces all indices
     in the range [\[0, n)] in increasing order. Querying the iterator after
     all elements have been produced causes the exception {!End_of_set} to be
     raised. *)
  val enumerate : 'n cardinal -> (unit -> 'n index)
  val rev_enumerate : 'n cardinal -> (unit -> 'n index)

  (** To implement clever datastructures (for instance using bit-packing),
      it is useful to manipulate indices as integers. See [IndexSet] or
      [IndexMap] for usage examples. Refrain from using it if you are not sure
      of what you are doing since this can break some modular abstraction.

      To be safe, the coerced module should never introduce new indices; that
      is, it should not return an index that it did not receive as argument. *)
  module Unsafe : sig
    module type T = sig type 'a t end
    module type F = functor (X : T) -> sig module type S end

    module Int : T with type 'a t = int
    module Index : T with type 'a t = 'a index
    module Coerce (F: F) (X : F(Int).S) : F(Index).S
  end

  val equal : 'n index -> 'n index -> bool
  val compare : 'n index -> 'n index -> int
  val minimum : 'n index -> 'n index -> 'n index
  val maximum : 'n index -> 'n index -> 'n index
end

(**A vector of type [(n, a) vector] is a (fixed-size) array whose indices lie
   in the type-level set [n] and whose elements have type [a]. *)
type ('n, 'a) vector

(**The submodule {!Vector} allows safely manipulating indices into a vector. *)
module Vector : sig

  type ('n, 'a) t = ('n, 'a) vector

  (**{!length} is analogous to [Array.length], but returns a cardinal instead
     of an ordinary integer. *)
  val length : ('n, 'a) t -> 'n cardinal
  val length_as_int : ('n, 'a) t -> int

  (**{!get} is [Array.get], but expects an index instead of an ordinary
     integer. This guarantees that the index is within bounds. *)
  val get : ('n, 'a) t -> 'n index -> 'a

  (**{!set} is [Array.set], but expects an index instead of an ordinary
     integer. This guarantees that the index is within bounds. *)
  val set : ('n, 'a) t -> 'n index -> 'a -> unit

  (**[set_cons t i x] is short for [set t i (x :: get t i)]. *)
  val set_cons : ('n, 'a list) t -> 'n index -> 'a -> unit

  (**{!empty} is the empty vector. *)
  val empty : (Empty.n, _) t

  (**{!make} is analogous to [Array.make]. Invoking [make n x] fixes the
     cardinal [n]. *)
  val make : 'n cardinal -> 'a -> ('n, 'a) t

  (**[make' n f] is roughly analogous to [make n (f())], but removes the need
     to exhibit a value of type ['a] when [n] is zero. The function call [f()]
     takes place only if [n] is greater than zero. It takes place at most
     once. Invoking [make' n f] fixes the cardinal [n]. *)
  val make' : 'n cardinal -> (unit -> 'a) -> ('n, 'a) t

  (**{!init} is analogous to [Array.init]. Invoking [init n f] fixes the
     cardinal [n]. *)
  val init : 'n cardinal -> ('n index -> 'a) -> ('n, 'a) t

  (**{!map} is analogous to [Array.map]. *)
  val map : ('a -> 'b) -> ('n, 'a) t -> ('n, 'b) t

  val copy : ('n, 'a) t -> ('n, 'a) t
  val mapi : ('n index -> 'a -> 'b) -> ('n, 'a) t -> ('n, 'b) t

  val for_all : ('a -> bool) -> ('n, 'a) t -> bool
  val exists : ('a -> bool) -> ('n, 'a) t -> bool

  val iter : ('a -> unit) -> ('n, 'a) t -> unit
  val iteri : ('n index -> 'a -> unit) -> ('n, 'a) t -> unit
  val iter2 : ('a -> 'b -> unit) -> ('n, 'a) t -> ('n, 'b) t -> unit

  val rev_iteri : ('n index -> 'a -> unit) -> ('n, 'a) t -> unit

  val fold_left : ('a -> 'b -> 'a) -> 'a -> (_, 'b) t -> 'a
  val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> ('n, 'b) t -> ('n, 'c) t -> 'a

  val fold_lefti : ('a -> 'n index -> 'b -> 'a) -> 'a -> ('n, 'b) t -> 'a
  val fold_lefti2 : ('a -> 'n index -> 'b -> 'c -> 'a) -> 'a -> ('n, 'b) t -> ('n, 'c) t -> 'a

  val fold_right : ('b -> 'a -> 'a) -> (_, 'b) t -> 'a -> 'a
  val fold_right2 : ('b -> 'c -> 'a -> 'a) -> ('n, 'b) t -> ('n, 'c) t -> 'a -> 'a

  val fold_righti : ('n index -> 'b -> 'a -> 'a) -> ('n, 'b) t -> 'a -> 'a
  val fold_righti2 : ('n index -> 'b -> 'c -> 'a -> 'a) -> ('n, 'b) t -> ('n, 'c) t -> 'a -> 'a

  val cast_array : 'n cardinal -> 'a array -> ('n, 'a) t
  val as_array : (_, 'a) t -> 'a array
  val to_list : (_, 'a) t -> 'a list

  type 'a packed = Packed : (_, 'a) vector -> 'a packed
  val of_array : 'a array -> 'a packed
  val of_list : 'a list -> 'a packed

  module type V = sig type n type a val vector : (n, a) t end
  module Of_array (A : sig type a val array : a array end) : V with type a = A.a

  val equal : ('a -> 'a -> bool) -> ('n, 'a) t -> ('n, 'a) t -> bool
  val compare : ('a -> 'a -> int) -> ('n, 'a) t -> ('n, 'a) t -> int
end
