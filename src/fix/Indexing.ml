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
  Cardinal : int Lazy.t -> unit cardinal [@@ocaml.unboxed]

(* The function [cardinal] forces the cardinal to become fixed. *)

let cardinal (type n) (Cardinal c : n cardinal) = Lazy.force_val c

let is_val (type n) (Cardinal l : n cardinal) = Lazy.is_val l

type (_, _) eq = Refl : ('a, 'a) eq

let assert_equal_cardinal (type n m) (n : n cardinal) (m : m cardinal) : (n, m) eq =
  let Cardinal n = n in
  let Cardinal m = m in
  if not (Int.equal (Lazy.force_val n) (Lazy.force_val m)) then
    invalid_arg "Indexing.equal_cardinal: not equal";
  Refl

let check_equal_cardinal (type n m) (n : n cardinal) (m : m cardinal) : (n, m) eq option =
  let Cardinal n = n in
  let Cardinal m = m in
  if Int.equal (Lazy.force_val n) (Lazy.force_val m)
  then Some Refl
  else None

type 'n index =
  int

module type CARDINAL = sig type n val n : n cardinal end

(* [Empty] and [Const] produce sets whose cardinal is known. *)

module Empty = struct
  type n = unit
  let n = Cardinal (lazy 0)
end

module Unit = struct
  type n = unit
  let n = Cardinal (lazy 1)
  let element = 0
end

module Const (X : sig val cardinal : int end) : CARDINAL = struct
  type n = unit
  let () = assert (X.cardinal >= 0)
  let n = Cardinal (lazy X.cardinal)
end

let const c : (module CARDINAL) =
  assert (c >= 0);
  (module struct type n = unit let n = Cardinal (lazy c) end)

module type UNSAFE_CARDINAL = sig
  type 'a t
  module Const(M : sig type t val cardinal : int end) : CARDINAL with type n = M.t t
  module Eq(M : sig type t include CARDINAL end) : sig val eq : (M.t t, M.n) eq end
end

module Unsafe_cardinal() : UNSAFE_CARDINAL = struct
  type 'a t = unit
  module Const(M : sig type t val cardinal : int end) = struct
    type n = M.t t
    let n : n cardinal = Cardinal (lazy (M.cardinal))
  end
  module Eq(M : sig type t include CARDINAL end) = struct
    let eq : (M.t t, M.n) eq =
      let Cardinal _ = M.n in
      Refl
  end
end

(* [Gensym] produces a set whose cardinal is a priori unknown. A new reference
   stores the current cardinal, which grows when [fresh()] is invoked. [fresh]
   fails if the suspension [n] has been forced. *)

module Gensym () = struct

  type n = unit
  let counter = ref 0
  let n = Cardinal (lazy !counter)

  let fresh () =
    assert (not (is_val n));
    let result = !counter in
    incr counter;
    result

end

type ('l, 'r) either =
  | L of 'l
  | R of 'r

module Sum = struct
  type (_, _) n = unit

  module type S = sig
    type l and r
    type nonrec n = (l, r) n
    include CARDINAL with type n := n
    val inj_l : l index -> n index
    val inj_r : r index -> n index
    val prj : n index -> (l index, r index) either
  end

  module Make (L : CARDINAL)(R : CARDINAL) = struct
    type l = L.n
    type r = R.n
    type nonrec n = (l, r) n

    (* The cardinal [l] of the left-hand set becomes fixed now (if it
       wasn't already). We need it to be fixed for our injections and
       projections to make sense. *)
    let l : int = cardinal L.n

    (* The right-hand set can remain open-ended. *)
    let r : r cardinal = R.n

    let n : n cardinal =
      (* We optimize the case where [r] is fixed already, but the code
         in the [else] branch would work always. *)
      if is_val r then
        let n = l + cardinal r in
        Cardinal (lazy n)
      else
        Cardinal (lazy (l + cardinal r))

    (* Injections. The two sets are numbered side by side. *)
    let inj_l x = x
    let inj_r y = l + y

    (* Projection. *)
    let prj x = if x < l then L x else R (x - l)
  end

  let cardinal (type l r)
        (l : l cardinal)
        (r : r cardinal)
      : (l, r) n cardinal =
    let c = cardinal l + cardinal r in
    Cardinal (lazy c)

  let inj_l x = x

  let inj_r (type l r) (Cardinal l : l cardinal) (x : r index) =
    Lazy.force_val l + x

  let prj (type l r) (Cardinal l : l cardinal) (x : (l, r) n index) : (l index, r index) either =
    let l = Lazy.force_val l in
    if x < l then L x else R (x - l)

  let make (type l r) (l : l cardinal) (r : r cardinal) =
    let module L = struct type n = l let n = l end in
    let module R = struct type n = r let n = r end in
    (module Make(L)(R) : S with type l = l and type r = r)
end

module Prod = struct
  type (_, _) n = unit

  module type S = sig
    type l and r
    type nonrec n = (l, r) n
    include CARDINAL with type n := n
    val inj : l index -> r index -> n index
    val prj : n index -> l index * r index
  end

  module Make (L : CARDINAL)(R : CARDINAL) = struct
    type n = unit

    type l = L.n
    type r = R.n

    (* The cardinal [l] of the left-hand set becomes fixed now (if it
       wasn't already). We need it to be fixed for our injections and
       projections to make sense. *)
    let l : int = cardinal L.n
    (* The right-hand set can remain open-ended. *)
    let r : r cardinal = R.n

    let n : n cardinal =
      (* We optimize the case where [r] is fixed already, but the code
         in the [else] branch would work always. *)
      if is_val r then
        let n = l * cardinal r in
        Cardinal (lazy n)
      else
        Cardinal (lazy (l * cardinal r))

    (* Injections. The two sets are numbered side by side. *)
    let inj x y = y * l + x

    (* Projection. *)
    let prj x = (x mod l, x / l)
  end

  let cardinal (type l r)
        (Cardinal l : l cardinal)
        (Cardinal r : r cardinal)
      : (l, r) n cardinal =
    let l = Lazy.force_val l in
    if Lazy.is_val r then
      let c = l * Lazy.force_val r in
      Cardinal (lazy c)
    else
      Cardinal (lazy (l * Lazy.force_val r))

  let inj (type l) (Cardinal l : l cardinal) lx rx =
    lx + rx * (Lazy.force_val l)

  let prj (type l) (Cardinal l : l cardinal) x =
    let l = Lazy.force_val l in
    (x mod l, x / l)

  let make (type l r) (l : l cardinal) (r : r cardinal) =
    let module L = struct type n = l let n = l end in
    let module R = struct type n = r let n = r end in
    (module Make(L)(R) : S with type l = l and type r = r)
end

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

  let rev_enumerate (n : 'n cardinal) : unit -> 'n index =
    let n = cardinal n in
    let next = ref (n - 1) in
    fun () ->
      let i = !next in
      if i < 0 then raise End_of_set;
      decr next;
      i

  let pred = function
    | 0 -> None
    | i -> Some (pred i)

  let equal = Int.equal
  let compare = Int.compare
  let minimum = Int.min
  let maximum = Int.max

  module Unsafe = struct
    module type T = sig type 'a t end
    module type F = functor (X : T) -> sig module type S end

    module Int = struct type 'a t = int end
    module Index = Int
    module Coerce (F: F) (X : F(Int).S) : F(Index).S = X
  end

end

type ('n, 'a) vector = Vector : 'a array -> (unit, 'a) vector [@@ocaml.unboxed]

module Vector = struct

  type ('n, 'a) t = ('n, 'a) vector

  let get (type n) (Vector a : (n, _) t) i = Array.unsafe_get a i
  let set (type n) (Vector a : (n, _) t) i x = Array.unsafe_set a i x

  let set_cons t i x =
    set t i (x :: get t i)

  let length_as_int (type n) (Vector a : (n, _) vector) = Array.length a

  let length (type n) (Vector a : (n, _) t) : n cardinal =
    let n = Array.length a in
    Cardinal (lazy n)

  let empty : (Empty.n, _) t = Vector [||]

  let make (type n) (Cardinal n : n cardinal) x : (n, _) t =
    Vector (Array.make (Lazy.force_val n) x)

  let make' (type n) (Cardinal n : n cardinal) f : (n, _) t=
    match Lazy.force_val n with
    | 0 -> empty
    | n -> Vector (Array.make n (f()))

  let init (type n) (Cardinal n : n cardinal) f : (n, _) t=
    Vector (Array.init (Lazy.force_val n) f)

  let map (type n) f (Vector a : (n, _) t) : (n, _) t =
    Vector (Array.map f a)

  let mapi (type n) f (Vector a : (n, _) t) : (n, _) t =
    Vector (Array.mapi f a)

  let copy (type n) (Vector a : (n, _) t) : (n, _) t =
    Vector (Array.copy a)

  let equal (type n) eq (Vector a : (n, _) t) (Vector b : (n, _) t) =
    let rec loop i j =
      if i = j then
        true
      else if eq (Array.unsafe_get a i) (Array.unsafe_get b i) then
        loop (i + 1) j
      else
        false
    in
    loop 0 (Array.length a)

  let compare (type n) cmp (Vector a : (n, _) t) (Vector b : (n, _) t) =
    let rec loop i j =
      if i = j then
        0
      else
        let c = cmp (Array.unsafe_get a i) (Array.unsafe_get b i) in
        if c <> 0 then
          c
        else
          loop (i + 1) j
    in
    loop 0 (Array.length a)

  let for_all (type n) f (Vector a : (n, _) t) =
    Array.for_all f a

  let exists (type n) f (Vector a : (n, _) t) =
    Array.exists f a

  let iter (type n) f (Vector a : (n, _) t) =
    Array.iter f a

  let iteri (type n) f (Vector a : (n, _) t) =
    Array.iteri f a

  let iter2 (type n) f (Vector a : (n, _) t) (Vector b : (n, _) t) =
    Array.iter2 f a b

  let fold_left (type n) f acc (Vector a : (n, _) t) =
    Array.fold_left f acc a

  let fold_left2 (type n) f acc (Vector a : (n, _) t) (Vector b : (n, _) t) =
    let acc = ref acc in
    for i = 0 to Array.length a - 1 do
      acc := f !acc a.(i) b.(i)
    done;
    !acc

  let fold_lefti (type n) f acc (Vector a : (n, _) t) =
    let acc = ref acc in
    for i = 0 to Array.length a - 1 do
      acc := f !acc i a.(i)
    done;
    !acc

  let fold_lefti2 (type n) f acc (Vector a : (n, _) t) (Vector b : (n, _) t) =
    let acc = ref acc in
    for i = 0 to Array.length a - 1 do
      acc := f !acc i a.(i) b.(i)
    done;
    !acc

  let fold_right (type n) f (Vector a : (n, _) t) acc =
    Array.fold_right f a acc

  let fold_right2 (type n) f (Vector a : (n, _) t) (Vector b : (n, _) t) acc =
    let acc = ref acc in
    for i = Array.length a - 1 downto 0 do
      acc := f a.(i) b.(i) !acc
    done;
    !acc

  let fold_righti (type n) f (Vector a : (n, _) t) acc =
    let acc = ref acc in
    for i = Array.length a - 1 downto 0 do
      acc := f i a.(i) !acc
    done;
    !acc

  let fold_righti2 (type n) f (Vector a : (n, _) t) (Vector b : (n, _) t) acc =
    let acc = ref acc in
    for i = Array.length a - 1 downto 0 do
      acc := f i a.(i) b.(i) !acc
    done;
    !acc

  let rev_iteri (type n) f (Vector a : (n, _) t) =
    for i = Array.length a - 1 downto 0 do
      f i a.(i)
    done

  let as_array (type n) (Vector a : (n, _) t) = a

  let to_list (type n) (Vector a : (n, _) t) = Array.to_list a

  let cast_array (type n) (Cardinal n : n cardinal) arr : (n, _) t =
    if Lazy.force_val n <> Array.length arr then
      invalid_arg "Vector.cast_array: incorrect length";
    Vector arr

  type 'a packed = Packed : (_, 'a) vector -> 'a packed

  let of_array a = Packed (Vector a)

  let of_list l = Packed (Vector (Array.of_list l))

  module type V = sig type n type a val vector : (n, a) t end
  module Of_array (A : sig type a val array : a array end) = struct
    type n = unit
    type a = A.a
    let vector = Vector A.array
  end
end
