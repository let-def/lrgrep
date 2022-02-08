open Fix.Indexing

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

let rec next_pow2 i n =
if i < n then
n
else next_pow2 i (n * 2 + 1)

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

module type GEN = sig
  type elt
  type n
  val n : n cardinal

  val push : elt -> n index

  val get : n index -> elt
  val set : n index -> elt -> unit

  val freeze : unit -> (n, elt) vector
end

module Gen(T : sig type t end)() : GEN with type elt = T.t = struct
  type elt = T.t

  include Gensym()

  let buffer : (n, elt) t option ref = ref None

  let get_buffer () =
    match !buffer with
    | Some buffer -> buffer
    | None -> assert false

  let push x =
    let result = fresh () in
    let buffer = match !buffer with
      | Some buffer -> buffer
      | None -> let buf = make x in buffer := Some buf; buf
    in
    set buffer result x;
    result

  let get i = (get_buffer ()).values.((i : _ index :> int))

  let set i x = set (get_buffer ()) i x

  let freeze () =
    match !buffer with
    | None -> Vector.make' n (fun _ -> assert false)
    | Some buf -> contents buf n
end

let gen (type a) () =
  (module Gen(struct type t = a end)() : GEN with type elt = a)
