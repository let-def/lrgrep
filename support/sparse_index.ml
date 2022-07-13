type index = int
type key = int
type value = int

type repr = string

let get_int table ~offset = function
  | 1 -> String.get_uint8 table offset
  | 2 -> String.get_uint16_be table offset
  | 3 -> (String.get_uint16_be table offset) lor
         (String.get_uint8 table (offset + 2) lsl 16)
  | 4 -> Int32.to_int (String.get_int32_be table offset)
  | _ -> assert false

let set_int table ~offset ~value = function
  | 1 -> Bytes.set_uint8 table offset value
  | 2 -> Bytes.set_uint16_be table offset value
  | 3 -> Bytes.set_uint16_be table offset (value land 0xFFFF);
         Bytes.set_uint8 table (offset + 2) (value lsr 16)
  | 4 -> Bytes.set_int32_be table offset (Int32.of_int value)
  | _ -> assert false

let lookup (table : repr) (index : index) (key : key) : value option =
  let ksize = String.get_uint8 table 0 in
  let vsize = String.get_uint8 table 1 in
  assert (index >= 0 && key >= 0);
  let offset = 2 + (index + key) * (ksize + vsize) in
  if offset + 4 > String.length table then
    None
  else if get_int table ~offset ksize = key then
    Some (get_int table ~offset:(offset + ksize) vsize)
  else
    None

type 'a cell =
  | Unused
  | Used of key * 'a

type 'a packer = {
  mutable table: 'a cell array;
}

let make () = {
  table = Array.make 16 Unused;
}

let cell_unused packer index =
  assert (index >= 0);
  index >= Array.length packer.table || (
    match packer.table.(index) with
    | Unused -> true
    | _ -> false
  )

(*
let get_cell packer index =
  assert (index >= 0);
  if index < Array.length packer.table
  then packer.table.(index)
  else Unused
*)

let set_cell packer index value =
  assert (index >= 0);
  let length = Array.length packer.table in
  if index < length then
    packer.table.(index) <- value
  else (
    let length' = max (index + 1) (length * 2) in
    let table = Array.make length' Unused in
    Array.blit packer.table 0 table 0 length;
    packer.table <- table;
    packer.table.(index) <- value
  )

let add_vector packer cells =
  let rec loop index = function
    | [] -> index
    | (k, _) :: cells' ->
      if cell_unused packer (index + k) then
        loop index cells'
      else
        loop (index + 1) cells
  in
  let index = loop 0 cells in
  List.iter (fun (k, v) -> set_cell packer (index + k) (Used (k, v))) cells;
  index

let int_size i =
  assert (i >= 0);
  if i <= 0xFF then
    1
  else if i <= 0xFFFF then
    2
  else if i <= 0xFF_FFFF then
    3
  else
    (assert (i <= 0xFFFF_FFFF); 4)

let pack packer value_repr =
  let max_k = ref 0 in
  let max_v = ref 0 in
  let table =
    Array.map (function
        | Unused -> Unused
        | Used (k, v) ->
          let v = value_repr v in
          assert (v >= 0);
          if k > !max_k then max_k := k;
          if v > !max_v then max_v := v;
          Used (k, v)
      )
      packer.table
  in
  let k_size = int_size !max_k in
  let v_size = int_size !max_v in
  let repr = Bytes.make (2 + Array.length table * (k_size + v_size)) '\x00' in
  set_int repr ~offset:0 ~value:k_size 1;
  set_int repr ~offset:0 ~value:v_size 1;
  Array.iteri (fun i cell ->
      let offset = 2 + i * (k_size + v_size) in
      match cell with
      | Unused -> set_int repr ~offset ~value:0 k_size
      | Used (k, v) ->
        set_int repr ~offset ~value:k k_size;
        set_int repr ~offset:(offset + k_size) ~value:v v_size;
    ) table;
  Bytes.unsafe_to_string repr
