open Fix.Indexing

type 'n t = {
  bytes: bytes;
  mutable marked: IntSet.t;
}

let index_of i = i lsr 6
let offset_of i = i land 63

let make n =
  let n = cardinal n in
  let bytes = Bytes.make (index_of (n + 63) lsl 3) '\000' in
  { bytes; marked = IntSet.empty }

let mark t n =
  let n = (n : _ index :> int) in
  let index = index_of n in
  let cell = Bytes.get_int64_ne t.bytes (index lsl 3) in
  let bit = Int64.shift_left 1L (offset_of n) in
  if Int64.logand cell bit = 0L then (
    if cell = 0L then t.marked <- IntSet.add index t.marked;
    Bytes.set_int64_ne t.bytes (index lsl 3) (Int64.logor cell bit)
  )

let marked t =
  IndexSet.unsafe_of_intset (
    IntSet.fold_right
      (fun set index ->
         let set = ref set in
         let cell = Bytes.get_int64_ne t.bytes (index lsl 3) in
         for i = 63 downto 0 do
           if Int64.(logand (shift_left 1L i) cell) <> 0L then
             set := IntSet.add ((index lsl 6) lor i) !set
         done;
         !set
      ) IntSet.empty t.marked
  )

let clear t =
  IntSet.iter (fun index ->
      Bytes.set_int64_ne t.bytes (index lsl 3) 0L
    ) t.marked;
  t.marked <- IntSet.empty

let is_empty t = IntSet.is_empty t.marked
