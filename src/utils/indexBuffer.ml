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
    Array.blit t.values 0 (Vector.as_array vector) 0 len;
    vector
end

module Gen = struct
  type ('n ,'a) t = {
    cardinal : 'n cardinal;
    fresh : unit -> 'n index;
    mutable last : int;
    mutable last_reserved : int;
    mutable lock : int;
    mutable values : 'a array;
  }

  type 'n reservation = {index : 'n index; mutable committed: bool}

  let raw_set t i x =
    let n = Array.length t.values in
    if i >= n then (
      let values' = Array.make (next_pow2 i n) x in
      Array.blit t.values 0 values' 0 n;
      t.values <- values';
    );
    t.values.(i) <- x

  let add t x =
    let index = t.fresh () in
    let i = (index : _ index :> int) in
    if t.lock = 0 then
      t.last <- i
    else
      t.last_reserved <- i;
    raw_set t i x;
    index

  let reserve t =
    t.lock <- t.lock + 1;
    let index = t.fresh () in
    t.last_reserved <- (index : _ index :> int);
    {index; committed = false}

  let index r = r.index

  let commit t r x =
    if r.committed then
      invalid_arg "Gen.commit: already committed";
    r.committed <- true;
    let i = (r.index : _ index :> int) in
    raw_set t i x;
    t.lock <- t.lock - 1;
    if t.lock = 0 then
      t.last <- t.last_reserved

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
    Array.blit t.values 0 (Vector.as_array vector) 0 (cardinal t.cardinal);
    vector

  let freeze_map t f =
    Vector.init t.cardinal (fun i -> f i t.values.((i : _ index :> int)))

  module Make () = struct
    include Gensym()

    let used = ref false
    let get_generator () : (n, 'a) t =
      if !used then invalid_arg "Gen.Make.get_generator: already used";
      used := true;
      {cardinal = n; fresh; last = -1; last_reserved = -1; lock = 0; values = [||]}
  end
end
