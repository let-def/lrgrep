let debug = false

module Bit_packer : sig
  type row
  val import : (int * _) list -> row

  type t
  val make : unit -> t
  val add_row : t -> row -> int

  val length : t -> int
end = struct
  let bits = Sys.word_size - 1

  type cell_list =
    | N
    | C of int * int * cell_list

  let prepend xs x =
    let iaddr = x / bits in
    let imask = 1 lsl (x mod bits) in
    match xs with
    | N -> C (iaddr, imask, N)
    | C (iaddr', imask', xs') ->
      if iaddr = iaddr' then
        C (iaddr, imask lor imask', xs')
      else (
        assert (iaddr < iaddr');
        C (iaddr, imask, xs)
      )

  type row = {
    first: int;
    set: cell_list;
    width: int;
  }

  let import = function
    | [] -> {first = 0; set = N; width = 0}
    | (last, _) :: rest as cells ->
      let first = List.fold_left (fun _ (i, _) -> i) last rest in
      let add set (index, _) = prepend set (index - first) in
      let set = List.fold_left add N cells in
      {first; set; width = (last - first + 1)}

  let shift_mask mask shift =
    let mask0 = mask lsl shift in
    let mask1 = mask lsr (bits - shift) in
    (mask0, mask1)

  let compatible cells0 cells1 mask shift =
    let mask0, mask1 = shift_mask mask shift in
    (cells0 land mask0) lor (cells1 land mask1) = 0

  type t = {
    mutable cells: int array;
    used: (int, unit) Hashtbl.t;
    mutable length: int;
  }

  let rec fit_cells table oaddr oshift = function
    | N -> oshift
    | C (base, mask, cells') ->
      let addr = oaddr + base in
      let len = Array.length table in
      if addr >= len then
        oshift
      else
        let cells0 = table.(addr) in
        let cells1 =
          if addr + 1 = len
          then 0
          else table.(addr + 1)
        in
        if compatible cells0 cells1 mask oshift then
          fit_cells table oaddr oshift cells'
        else (
          let oshift = ref (oshift + 1) in
          while !oshift < bits &&
                not (compatible cells0 cells1 mask !oshift)
          do incr oshift; done;
          !oshift
        )

  let rec fit_row t oaddr oshift row =
    let oshift' = fit_cells t.cells oaddr oshift row.set in
    if oshift' = oshift &&
       let offset = oaddr * bits + oshift - row.first in
       not (Hashtbl.mem t.used offset) then
      (oaddr, oshift)
    else
      let oshift' = if oshift = oshift' then oshift + 1 else oshift' in
      if oshift' < bits then
        fit_row t oaddr oshift' row
      else
        fit_row t (oaddr + 1) 0 row

  let grow_table table target =
    let len = Array.length table in
    if len > target then
      table
    else
      let rlen = ref len in
      while !rlen <= target do
        rlen := !rlen * 2
      done;
      let table' = Array.make !rlen 0 in
      Array.blit table 0 table' 0 len;
      table'

  let rec write_cells table oaddr oshift = function
    | N -> table
    | C (base, mask, cells') ->
      let addr = oaddr + base in
      let table = grow_table table (addr + 1) in
      let mask0, mask1 = shift_mask mask oshift in
      let cells0 = table.(addr) in
      let cells1 = table.(addr + 1) in
      assert (cells0 land mask0 = 0);
      assert (cells1 land mask1 = 0);
      table.(addr + 0) <- cells0 lor mask0;
      table.(addr + 1) <- cells1 lor mask1;
      write_cells table oaddr oshift cells'

  let add_row table row =
    let addr, shift = fit_row table 0 0 row in
    let cells = write_cells table.cells addr shift row.set in
    table.cells <- cells;
    let offset = addr * bits + shift in
    let length = offset + row.width in
    if length > table.length then
      table.length <- length;
    let final = (offset - row.first) in
    Hashtbl.add table.used final ();
    final

  let make () = {
    cells = [|0|];
    used = Hashtbl.create 0;
    length = 0;
  }

  let length table =
    table.length
end

type 'a cells = (int * 'a) list

type promise = int

type 'a t = {
  mutable vectors: (promise * 'a cells) list;
  mutable cols: int;
  mutable rows: int;
}

let make () = { rows = 0; vectors = []; cols = 0 }

type row_mapping = Lrgrep_runtime.Sparse_table.row array

let add_row t cells =
  let index = t.rows in
  t.rows <- t.rows + 1;
  let cells = List.sort (fun (i, _) (j, _) -> Int.compare i j) cells in
  let rec columns i = function
    | [] -> 1 + i
    | (j, _) :: rest ->
      if j <= i then
        invalid_arg "sparse_table_packer.add_row: \
                     invalid sparse row (indices not strictly increasing)";
      columns j rest
  in
  t.cols <- Int.max t.cols (columns (-1) cells);
  t.vectors <- (index, cells) :: t.vectors;
  index

let resolve = Array.get

let equal_cell (i1,v1) (i2,v2) =
  Int.equal i1 i2 && Int.equal v1 v2

let compare_cell (i1,v1) (i2,v2) =
  match Int.compare i1 i2 with
  | 0 -> Int.compare v1 v2
  | n -> n

let rec merge_vectors acc r c ps = function
  | [] -> (c, ps) :: acc
  | (r', c', p') :: rest ->
    if Int.equal r r' && List.equal equal_cell c c'
    then merge_vectors acc r c (p' :: ps) rest
    else merge_vectors ((c, ps) :: acc) r' c' [p'] rest

let merge_vectors = function
  | [] -> []
  | (r, c, p) :: rest -> merge_vectors [] r c [p] rest

type table = {
  displacement: int array;
  keys: int array;
  values: int array;
}

let table_lookup t row col =
  assert (row >= 0 && col >= 0);
  if col > Array.length t.displacement then
    None
  else
    let offset = t.displacement.(col) + row in
    if offset < 0 || offset > Array.length t.keys ||
       t.keys.(offset) <> row
    then
      None
    else Some t.values.(offset)

let pack t f =
  (* Sort rows by decreasing rank, merge when equal *)
  let prepare_cell (i, v) = (i, f v) in
  let by_decreasing_rank (r1, c1, _) (r2, c2, _) =
    match Int.compare r2 r1 with
    | 0 -> List.compare compare_cell c1 c2
    | n -> n
  in
  let rows =
    t.vectors
    |> List.rev_map (fun (p, cells) ->
        let cells = List.rev_map prepare_cell cells in
        let rank = List.length cells in
        (rank, cells, p)
      )
    |> List.sort by_decreasing_rank
    |> merge_vectors
  in
  (* Transpose *)
  let cols = Array.make t.cols [] in
  let row_mapping = Array.make t.rows 0 in
  List.iteri begin fun row (cells, promises) ->
    List.iter (fun p -> row_mapping.(p) <- row)
      promises;
    List.iter (fun (col, value) -> cols.(col) <- (row, value) :: cols.(col))
      cells;
  end rows;
  (* Sort columns by decreasing rank, merge when equal *)
  let cols =
    Array.mapi (fun col cells -> (List.length cells, cells, col)) cols
  in
  Array.sort by_decreasing_rank cols;
  let cols = merge_vectors (Array.to_list cols) in
  (* Pack vectors *)
  let packer = Bit_packer.make () in
  let offsets =
    List.mapi (fun row (cells, _indices) ->
        if debug then (
          Printf.eprintf "row %03d, cells:" row;
          List.iter (fun (k,v) ->
              Printf.eprintf " %d -> 0x%04X;" k v)
            cells;
          Printf.eprintf "\n";
        );
        Bit_packer.add_row packer (Bit_packer.import cells)
      ) cols
  in
  (* Construct displacement and data *)
  let displacement = Array.make t.cols 0 in
  let keys = Array.make (Bit_packer.length packer) 0 in
  let values = Array.make (Bit_packer.length packer) 0 in
  List.iter2 begin fun (cells, indices) offset ->
    List.iter (fun i ->
        if debug then
          Printf.eprintf "displacement: % 4d -> % 4d\n" i offset;
        displacement.(i) <- offset
      ) indices;
    List.iter (fun (k, v) ->
        keys.(offset + k) <- k + 1;
        values.(offset + k) <- v;
      ) cells;
  end cols offsets;
  (row_mapping, {displacement; keys; values})

let int_size i =
  assert (i >= 0);
  if i <= 0xFF then
    1
  else if i <= 0xFFFF then
    2
  else if i <= 0xFF_FFFF then
    3
  else if (i <= 0x3FFF_FFFF) then
    4
  else
    invalid_arg "Sparse_table_packer: int overflow"


let encode1 arr =
  let len = Array.length arr in
  let result = Bytes.create len in
  Array.iteri (fun i v -> Bytes.set_uint8 result i v) arr;
  Bytes.unsafe_to_string result

let encode2 arr =
  let len = Array.length arr in
  let result = Bytes.create (len * 2) in
  Array.iteri (fun i v -> Bytes.set_uint16_be result (i * 2) v) arr;
  Bytes.unsafe_to_string result

let encode3 arr =
  let len = Array.length arr in
  let result = Bytes.create (len * 3) in
  Array.iteri (fun i v ->
      let i = i * 3 in
      Bytes.set_uint16_be result i (v land 0xFFFF);
      Bytes.set_uint8 result (i + 2) (v lsr 16)
    ) arr;
  Bytes.unsafe_to_string result

let encode4 arr =
  let len = Array.length arr in
  let result = Bytes.create (len * 4) in
  Array.iteri (fun i v ->
      let i = i * 4 in
      Bytes.set_uint16_be result i (v land 0xFFFF);
      Bytes.set_uint16_be result (i + 2) (v lsr 16)
    ) arr;
  Bytes.unsafe_to_string result

let encode_array name = function
  | [||] ->
    ("(fun _ -> min_int)", (fun _ -> min_int))
  | arr ->
    let max_v = Array.fold_left Int.max arr.(0) arr in
    let log code =
      if !Utils.Misc.verbosity_level > 0 then
        Printf.eprintf "table %s has %d entries and takes %d bytes\n"
          name (Array.length arr) (String.length code);
      code
    in
    match int_size max_v with
    | 1 ->
      let code = log @@ encode1 arr in
      (Printf.sprintf "(Lrgrep_runtime.Sparse_table.get1 %S)" code,
       Lrgrep_runtime.Sparse_table.get1 code)
    | 2 ->
      let code = log @@ encode2 arr in
      (Printf.sprintf "(Lrgrep_runtime.Sparse_table.get2 %S)" code,
       Lrgrep_runtime.Sparse_table.get2 code)
    | 3 ->
      let code = log @@ encode3 arr in
      (Printf.sprintf "(Lrgrep_runtime.Sparse_table.get3 %S)" code,
       Lrgrep_runtime.Sparse_table.get3 code)
    | 4 ->
      let code = log @@ encode4 arr in
      (Printf.sprintf "(Lrgrep_runtime.Sparse_table.get4 %S)" code,
       Lrgrep_runtime.Sparse_table.get4 code)
    | _ -> assert false

let encode {displacement; keys; values} =
  let offset = - Array.fold_left Int.min 0 displacement in
  let disp_source, displacement =
    encode_array "displacement" (Array.map ((+) offset) displacement)
  in
  let keys_source, keys = encode_array "keys" keys in
  let vals_source, values = encode_array "values" values in
  let coded = {Lrgrep_runtime.Sparse_table.displacement; offset; keys; values} in
  let source =
    Printf.sprintf
      "{Lrgrep_runtime.Sparse_table.\
       displacement = %s; offset = %d; keys = %s; values = %s}"
      disp_source offset keys_source vals_source
  in
  (source, coded)
