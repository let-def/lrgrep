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

(* A suspension is used to represent a cardinal-that-may-still-be-unknown. *)

type 'n cardinal =
  int Lazy.t

(* The function [cardinal] forces the cardinal to become fixed. *)

let cardinal =
  Lazy.force

type 'n index =
  int

module type CARDINAL = sig type n val n : n cardinal end

(* [Empty] and [Const] produce sets whose cardinal is known. *)

module Empty : CARDINAL = struct
  type n
  let n = lazy 0
end

module Const (X : sig val cardinal : int end) : CARDINAL = struct
  type n
  let () = assert (X.cardinal >= 0)
  let n = lazy X.cardinal
end

let const c : (module CARDINAL) =
  assert (c >= 0);
  (module struct type n let n = lazy c end)

(* [Gensym] produces a set whose cardinal is a priori unknown. A new reference
   stores the current cardinal, which grows when [fresh()] is invoked. [fresh]
   fails if the suspension [n] has been forced. *)

module Gensym () = struct

  type n
  let counter = ref 0
  let n = lazy !counter

  let fresh () =
    assert (not (Lazy.is_val n));
    let result = !counter in
    incr counter;
    result

end

type ('l, 'r) either =
  | L of 'l
  | R of 'r

module type SUM = sig
  type l and r
  include CARDINAL
  val inj_l : l index -> n index
  val inj_r : r index -> n index
  val prj : n index -> (l index, r index) either
end

module Sum (L : CARDINAL)(R : CARDINAL) = struct

  type n

  type l = L.n
  type r = R.n

  (* The cardinal [l] of the left-hand set becomes fixed now (if it
     wasn't already). We need it to be fixed for our injections and
     projections to make sense. *)
  let l : int = cardinal L.n
  (* The right-hand set can remain open-ended. *)
  let r : int cardinal = R.n

  let n =
    (* We optimize the case where [r] is fixed already, but the code
       in the [else] branch would work always. *)
    if Lazy.is_val r then
      let n = l + cardinal r in
      lazy n
    else
      lazy (l + cardinal r)

  (* Injections. The two sets are numbered side by side. *)
  let inj_l x = x
  let inj_r y = l + y

  (* Projection. *)
  let prj x = if x < l then L x else R (x - l)

end

let sum (type l r) (l : l cardinal) (r : r cardinal) =
  let module L = struct type n = l let n = l end in
  let module R = struct type n = r let n = r end in
  (module Sum(L)(R) : SUM with type l = l and type r = r)

module Index = struct

  type 'n t = 'n index

  let of_int (n : 'n cardinal) i : 'n index =
    let n = cardinal n in
    if i < 0 || i >= n then
      invalid_arg "Index.of_int";
    i

  let to_int i = i

  let iter (n : 'n cardinal) (yield : 'n index -> unit) =
    let n = cardinal n in
    for i = 0 to n - 1 do
      yield i
    done

  let rev_iter (n : 'n cardinal) (yield : 'n index -> unit) =
    let n = cardinal n in
    for i = n - 1 downto 0 do
      yield i
    done

  exception End_of_set

  let enumerate (n : 'n cardinal) : unit -> 'n index =
    let n = cardinal n in
    let next = ref 0 in
    fun () ->
      let i = !next in
      if n <= i then raise End_of_set;
      incr next;
      i

  module Unsafe = struct
    module type T = sig type 'a t end
    module type F = functor (X : T) -> sig module type S end

    module Int = struct type 'a t = int end
    module Index = Int
    module Coerce (F: F) (X : F(Int).S) : F(Index).S = X
  end

end

type ('n, 'a) vector =
  'a array

module Vector = struct

  type ('n, 'a) t = ('n, 'a) vector

  let get = Array.unsafe_get
  let set = Array.unsafe_set

  let set_cons t i x =
    set t i (x :: get t i)

  let length a =
    let n = Array.length a in
    lazy n

  let empty = [||]

  let make (n : _ cardinal) x =
    let n = cardinal n in
    Array.make n x

  let make' (n : _ cardinal) f =
    let n = cardinal n in
    if n = 0 then
      empty
    else
      Array.make n (f())

  let init (n : _ cardinal) f =
    let n = cardinal n in
    Array.init n f

  let map = Array.map
  let mapi = Array.mapi
  let copy = Array.copy
  let iter = Array.iter
  let iteri = Array.iteri

  let fold_left = Array.fold_left
  let fold_right = Array.fold_right

  module type V = sig type n type a val vector : (n, a) vector end

  let of_array (type a) (arr : a array) : (module V with type a = a) =
    (module struct type n type nonrec a = a let vector = arr end)

  let of_list (type a) (arr : a list) : (module V with type a = a) =
    (module struct type n type nonrec a = a let vector = Array.of_list arr end)

  let to_list = Array.to_list
  let to_array x = x

  module Of_array(A : sig type a val array : a array end) :
    V with type a = A.a =
  struct
    type n
    type a = A.a
    let vector = A.array
  end

  module Of_list(A : sig type a val list : a list end) :
    V with type a = A.a =
  struct
    type n
    type a = A.a
    let vector = Array.of_list A.list
  end
end
