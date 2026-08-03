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

module PDyn = struct
  type ('n, 'a) t = {
    mutable count: int;
    mutable domain: bytes;
    mutable values: 'a array;
  }

  let make () = { count = 0; domain = Bytes.empty; values = [||] }

  exception Uninitialized

  let check_bit b i =
    let c = i lsr 3 in
    (c < Bytes.length b) &&
    let m = 1 lsl (i land 0x7) in
    Char.code (Bytes.get b c) land m <> 0

  let set_bit b i =
    let c = i lsr 3 in
    let m = 1 lsl (i land 0x7) in
    let x = (Char.code (Bytes.get b c)) in
    let x' = m lor x in
    if x <> x'
    then (Bytes.set b c (Char.unsafe_chr x'); true)
    else false

  let get t i =
    let i = (i : _ index :> int) in
    if check_bit t.domain i
    then t.values.(i)
    else raise Uninitialized

  let set t i x =
    let i = (i : _ index :> int) in
    let n = Array.length t.values in
    if i >= n then (
      let m = next_pow2 i n in
      let values = Array.make m x in
      Array.blit t.values 0 values 0 n;
      let m' = (m + 7) lsr 3 in
      let domain = Bytes.make m' '\000' in
      Bytes.blit t.domain 0 domain 0 ((n + 7) lsr 3);
      t.values <- values;
      t.domain <- domain;
    );
    t.values.(i) <- x;
    if set_bit t.domain i then
      t.count <- t.count + 1

  let contents t c =
    let len = cardinal c in
    if t.count <> len then
      raise Uninitialized;
    let vector = Vector.make' c (fun () -> t.values.(0)) in
    Array.blit t.values 0 (Vector.as_array vector) 0 len;
    vector
end
