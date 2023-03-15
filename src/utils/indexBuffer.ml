open Fix.Indexing

let rec next_pow2 i n =
  if i < n then
    n
  else next_pow2 i (n * 2 + 1)

module Dyn = struct
  type ('n, 'a) t = {
    default: 'a;
    mutable values: 'a array;
  }

  let make default = { default; values = [||] }

  let get t i =
    let i = (i : _ index :> int) in
    if Array.length t.values <= i
    then t.default
    else t.values.(i)

  let set t i x =
    let i = (i : _ index :> int) in
    let n = Array.length t.values in
    if i >= n then (
      let values' = Array.make (next_pow2 i n) t.default in
      Array.blit t.values 0 values' 0 n;
      t.values <- values';
    );
    t.values.(i) <- x

  let contents t c =
    let vector = Vector.make c t.default in
    let len = min (cardinal c) (Array.length t.values) in
    Array.blit t.values 0 (vector : _ vector :> _ array) 0 len;
    vector
end

module Gen = struct
  type ('n ,'a) t = {
    cardinal : 'n cardinal;
    fresh : unit -> 'n index;
    mutable last : int;
    mutable last_locked : int;
    mutable lock : int;
    mutable values : 'a array;
  }

  let raw_set t i x =
    let n = Array.length t.values in
    if i >= n then (
      let values' = Array.make (next_pow2 i n) x in
      Array.blit t.values 0 values' 0 n;
      t.values <- values';
    );
    t.values.(i) <- x

  let add t x =
    let i = t.fresh () in
    let i' = (i : _ index :> int) in
    if t.lock = 0 then
      t.last <- i'
    else
      t.last_locked <- i';
    raw_set t i' x;
    i

  let add' t f =
    t.lock <- t.lock + 1;
    let i = t.fresh () in
    let i' = (i : _ index :> int) in
    t.last_locked <- i';
    let x = f i in
    raw_set t i' x;
    t.lock <- t.lock - 1;
    if t.lock = 0 then
      t.last <- t.last_locked;
    (i, x)

  let get t i =
    let i' = (i : _ index :> int) in
    if i' > t.last then
      invalid_arg "Gen.get: accessing locked index";
    t.values.(i')

  let set t i x =
    let i' = (i : _ index :> int) in
    if i' > t.last then
      invalid_arg "Gen.set: accessing locked index";
    t.values.(i') <- x

  let freeze t =
    let vector = Vector.make' t.cardinal (fun () -> t.values.(0)) in
    Array.blit t.values 0 (vector : _ vector :> _ array) 0 (cardinal t.cardinal);
    vector

  module Make () = struct
    include Gensym()

    let used = ref false
    let get_generator () : (n, 'a) t =
      if !used then invalid_arg "Gen.Make.get_generator: already used";
      used := true;
      {cardinal = n; fresh; last = -1; last_locked = -1; lock = 0; values = [||]}
  end
end
