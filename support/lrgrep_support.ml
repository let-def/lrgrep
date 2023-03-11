open Utils

module RT = Lrgrep_runtime

module Sparse_packer : sig
  type 'a t
  val make : unit -> 'a t
  type 'a vector = (RT.lr1 * 'a) list
  val add_vector : 'a t -> (RT.lr1 * 'a) list -> RT.sparse_index
  val pack : 'a t -> ('a -> RT.program_counter) -> RT.sparse_table
end = struct
  let set_int table ~offset ~value = function
    | 1 -> Bytes.set_uint8 table offset value
    | 2 -> Bytes.set_uint16_be table offset value
    | 3 -> Bytes.set_uint16_be table offset (value land 0xFFFF);
           Bytes.set_uint8 table (offset + 2) (value lsr 16)
    | 4 -> Bytes.set_int32_be table offset (Int32.of_int value)
    | _ -> assert false

  type 'a cell =
    | Unused
    | Used of RT.lr1 * 'a

  type 'a t = {
    mutable table: 'a cell array;
  }

  type 'a vector = (RT.lr1 * 'a) list

  let make () = { table = Array.make 16 Unused }

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
      let length = ref (Array.length packer.table) in
      while !length > 0 && (
          match packer.table.(!length - 1) with
          | Unused -> true
          | _ -> false
        )
      do
        decr length;
      done;
      Array.init !length (fun i ->
          match packer.table.(i) with
          | Unused -> Unused
          | Used (k, v) ->
            let v = value_repr v in
            assert (v >= 0);
            if k > !max_k then max_k := k;
            if v > !max_v then max_v := v;
            Used (k, v)
        )
    in
    let k_size = int_size !max_k in
    let v_size = int_size !max_v in
    let repr = Bytes.make (2 + Array.length table * (k_size + v_size)) '\x00' in
    set_int repr ~offset:0 ~value:k_size 1;
    set_int repr ~offset:1 ~value:v_size 1;
    Array.iteri (fun i cell ->
        let offset = 2 + i * (k_size + v_size) in
        match cell with
        | Unused -> set_int repr ~offset ~value:0 k_size
        | Used (k, v) ->
          set_int repr ~offset ~value:k k_size;
          set_int repr ~offset:(offset + k_size) ~value:v v_size;
      ) table;
    Printf.eprintf "key size: %d\nvalue size: %d\ntable size:%d\n" k_size v_size (Array.length table);
    Bytes.unsafe_to_string repr
end

module Code_emitter : sig
  type t
  val make : unit -> t
  val position : t -> int
  val emit : t -> RT.program_instruction -> unit
  val emit_yield_reloc : t -> RT.program_counter ref -> unit
  val link : t -> RT.program
end = struct
  type t = {
    mutable reloc: (int * int ref) list;
    buffer: Buffer.t;
  }

  let make () = {
    reloc = [];
    buffer = Buffer.create 15;
  }

  let position t = Buffer.length t.buffer

  let emit t : RT.program_instruction -> _ = function
    | Store i ->
      assert (i < 0xFF);
      Buffer.add_char t.buffer '\x01';
      Buffer.add_uint8 t.buffer i
    | Move (i, j) ->
      assert (i < 0xFF && j < 0xFF);
      if i <> j then (
        Buffer.add_char t.buffer '\x02';
        Buffer.add_uint8 t.buffer i;
        Buffer.add_uint8 t.buffer j
      )
    | Clear i ->
      assert (i < 0xFF);
      Buffer.add_char t.buffer '\x03';
      Buffer.add_uint8 t.buffer i
    | Yield pos ->
      assert (pos <= 0xFFFFFF);
      Buffer.add_char t.buffer '\x04';
      Buffer.add_uint16_be t.buffer (pos land 0xFFFF);
      Buffer.add_uint8 t.buffer (pos lsr 16)
    | Accept (clause, registers) ->
      Buffer.add_char t.buffer '\x05';
      Buffer.add_uint8 t.buffer (Array.length registers);
      Buffer.add_uint8 t.buffer clause;
      Array.iter (function
          | None -> Buffer.add_uint8 t.buffer 0xFF;
          | Some i ->
            assert (i < 0xFF);
            Buffer.add_uint8 t.buffer i
        ) registers
    | Match index ->
      Buffer.add_char t.buffer '\x06';
      assert (index <= 0xFFFF);
      Buffer.add_uint16_be t.buffer index
    | Halt ->
      Buffer.add_char t.buffer '\x07'

  let emit_yield_reloc t reloc =
    Buffer.add_char t.buffer '\x04';
    let pos = Buffer.length t.buffer in
    Buffer.add_string t.buffer "   ";
    t.reloc <- (pos, reloc) :: t.reloc

  let link t =
    let buf = Buffer.to_bytes t.buffer in
    List.iter (fun (pos, reloc) ->
        assert (0 <= !reloc && !reloc < 0xFFFFFF);
        Bytes.set_uint16_be buf pos (!reloc land 0xFFFF);
        Bytes.set_uint8 buf (pos + 2) (!reloc lsr 16);
      ) t.reloc;
    Bytes.unsafe_to_string buf
end

type transition_action = {
  move: (RT.register * RT.register) list;
  store: RT.register list;
  target: int;
}

type state = {
  accept: (RT.clause * RT.register option array) list;
  halting: IntSet.t;
  transitions: (IntSet.t * transition_action) list;
}
type dfa = state array

type compact_dfa = RT.program * RT.sparse_table * RT.program_counter array

let compare_ints (i1, j1) (i2, j2) =
  let c = Int.compare i1 i2 in
  if c <> 0 then c else
    Int.compare j1 j2

let compare_transition_action t1 t2 =
  let c = Int.compare t1.target t2.target in
  if c <> 0 then c else
    let c = List.compare compare_ints t1.move t2.move in
    if c <> 0 then c else
      List.compare Int.compare t1.store t2.store

let same_action a1 a2 = compare_transition_action a1 a2 = 0

let compact (dfa : dfa) =
  let code = Code_emitter.make () in
  let index = Sparse_packer.make () in
  let halt_pc = ref (Code_emitter.position code) in
  Code_emitter.emit code Halt;
  let pcs = Array.init (Array.length dfa) (fun _ -> ref (-1)) in
  let rec emit_moves = function
    | (i, j) :: rest ->
      if i < j then (
        emit_moves rest;
        Code_emitter.emit code (Move (i, j));
      ) else if i > j then (
        Code_emitter.emit code (Move (i, j));
        emit_moves rest;
      ) else
        emit_moves rest
    | [] -> ()
  in
  let emit_action act =
    emit_moves act.move;
    List.iter (fun i -> Code_emitter.emit code (Store i)) act.store;
    Code_emitter.emit_yield_reloc code pcs.(act.target);
  in
  let goto_action action = (*function
    | ([], target) -> pcs.(target)
    | action ->*)
      let position = Code_emitter.position code in
      emit_action action;
      ref position
  in
  let transition_count = ref 0 in
  let transition_dom = ref 0 in
  let cell_count = ref 0 in
  Array.iter2 (fun {accept; halting; transitions} pc ->
      let default, other_transitions =
        let _, most_frequent_action =
          List.fold_left (fun (count, _ as default) (dom, action) ->
              let count' = IntSet.cardinal dom in
              incr transition_count;
              transition_dom := !transition_dom + count';
              if count' > count
              then (count', Some action)
              else default
            ) (IntSet.cardinal halting, None) transitions
        in
        let prepare_transition (dom, action) =
          match most_frequent_action with
          | Some action' when same_action action action' -> None
          | _ -> Some (dom, goto_action action)
        in
        let other = List.filter_map prepare_transition transitions in
        let other =
          if Option.is_some most_frequent_action && IntSet.cardinal halting > 0
          then (halting, halt_pc) :: other
          else other
        in
        (most_frequent_action, other)
      in
      assert (!pc = -1);
      pc := Code_emitter.position code;
      List.iter
        (fun (clause, registers) ->
           Code_emitter.emit code (Accept (clause, registers)))
        accept;
      begin match
        List.concat_map
          (fun (dom, target) ->
             List.map (fun x -> (x, target)) (IntSet.elements dom))
          other_transitions
        with
        | [] -> ()
        | cells ->
          cell_count := !cell_count + List.length cells;
          let i = Sparse_packer.add_vector index cells in
          Code_emitter.emit code (Match i);
      end;
      begin match default with
        | None -> Code_emitter.emit code Halt
        | Some default -> emit_action default
      end
    ) dfa pcs;
  Printf.eprintf "total transitions: %d (domain: %d), non-default: %d\n%!"
    !transition_count !transition_dom !cell_count;
  (Code_emitter.link code, Sparse_packer.pack index (!), Array.map (!) pcs)
