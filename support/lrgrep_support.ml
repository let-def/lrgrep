[@@@ocaml.warning "-32-37"]

open Utils
open Misc
open Fix.Indexing

module RT = Lrgrep_runtime

module Register = struct
  include Positive
  type t = n index
  type set = n indexset
  type 'a map = (n, 'a) indexmap
  let of_int = Index.of_int n
end

let add_uint24_be b i =
  assert (0 <= i && i <= 0xFFFFFF);
  Buffer.add_uint16_be b (i land 0xFFFF);
  Buffer.add_uint8 b (i lsr 16)

module Bit_packer : sig
  type row
  val import : (int * _) list -> row

  type table
  val new_table : unit -> table
  val add_row : table -> row -> int

  val length : table -> int
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

  type table = {
    mutable cells: int array;
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

  let rec fit_row table oaddr oshift cells =
    let oshift' = fit_cells table oaddr oshift cells in
    if oshift' = oshift then
      (oaddr, oshift)
    else if oshift' < bits then
      fit_row table oaddr oshift' cells
    else
      fit_row table (oaddr + 1) 0 cells

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
    let addr, shift = fit_row table.cells 0 0 row.set in
    let cells = write_cells table.cells addr shift row.set in
    table.cells <- cells;
    let offset = addr * bits + shift in
    let length = offset + row.width in
    if length > table.length then
      table.length <- length;
    (offset - row.first)

  let new_table () = {
    cells = [|0|];
    length = 0;
  }

  let length table =
    table.length
end

let debug = false

module Code_emitter : sig
  type t
  val make : unit -> t
  val position : t -> int
  val emit : t -> RT.program_instruction -> unit
  val emit_yield_reloc : t -> RT.program_counter ref -> unit
  val emit_match_reloc : t -> Lrgrep_support_packer.promise -> unit
  val link : t -> Lrgrep_support_packer.row_mapping -> RT.program_code
end = struct
  type t = {
    mutable reloc: (int * int ref) list;
    mutable promises: (int * Lrgrep_support_packer.promise) list;
    buffer: Buffer.t;
  }

  let make () = {
    reloc = [];
    promises = [];
    buffer = Buffer.create 15;
  }

  let position t = Buffer.length t.buffer

  let emit t : RT.program_instruction -> _ = fun inst ->
    if debug then
      Printf.eprintf "0x%04X: %s\n" (position t) (match inst with
          | RT.Store _             -> "Store"
          | RT.Move (_, _)         -> "Move"
          | RT.Swap (_, _)         -> "Swap"
          | RT.Clear _             -> "Clear"
          | RT.Yield _             -> "Yield"
          | RT.Accept (_, _, _)    -> "Accept"
          | RT.Match _             -> "Match"
          | RT.Priority (_, _, _)  -> "Priority"
          | RT.Halt                -> "Halt"
        );
    match inst with
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
    | Swap (i, j) ->
      assert (i < 0xFF && j < 0xFF);
      if i <> j then (
        Buffer.add_char t.buffer '\x09';
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
    | Accept (clause, priority, registers) ->
      assert (Array.length registers <= 0xFF);
      assert (clause <= 0xFFFF);
      Buffer.add_char t.buffer '\x05';
      Buffer.add_uint16_be t.buffer clause;
      Buffer.add_uint8 t.buffer priority;
      Buffer.add_uint8 t.buffer (Array.length registers);
      Array.iter (function
          | None -> Buffer.add_uint8 t.buffer 0xFF;
          | Some i ->
            assert (i < 0xFF);
            Buffer.add_uint8 t.buffer i
        ) registers
    | Match index ->
      Buffer.add_char t.buffer '\x06';
      assert (index <= 0xFFFFFF);
      add_uint24_be t.buffer index
    | Priority (clause, p1, p2) ->
      Buffer.add_char t.buffer '\x08';
      assert (clause <= 0xFFFF);
      Buffer.add_uint16_be t.buffer clause;
      Buffer.add_uint8 t.buffer p1;
      Buffer.add_uint8 t.buffer p2
    | Halt ->
      Buffer.add_char t.buffer '\x07'

  let emit_yield_reloc t reloc =
    if debug then
      Printf.eprintf "0x%04X: Yield\n" (position t);
    Buffer.add_char t.buffer '\x04';
    let pos = Buffer.length t.buffer in
    Buffer.add_string t.buffer "   ";
    t.reloc <- (pos, reloc) :: t.reloc

  let emit_match_reloc t promise =
    if debug then
      Printf.eprintf "0x%04X: Match\n" (position t);
    Buffer.add_char t.buffer '\x06';
    let pos = Buffer.length t.buffer in
    add_uint24_be t.buffer 0;
    t.promises <- (pos, promise) :: t.promises

  let link t remap =
    let buf = Buffer.to_bytes t.buffer in
    List.iter (fun (pos, reloc) ->
        assert (0 <= !reloc && !reloc < 0xFFFFFF);
        Bytes.set_uint16_be buf pos (!reloc land 0xFFFF);
        Bytes.set_uint8 buf (pos + 2) (!reloc lsr 16);
      ) t.reloc;
    List.iter (fun (pos, promise) ->
        let p = Lrgrep_support_packer.resolve remap promise in
        Bytes.set_uint16_be buf pos (p land 0xFFFF);
        Bytes.set_uint8 buf (pos + 2) (p lsr 16);
      ) t.promises;
    Printf.eprintf "bytecode size: %d\n" (Bytes.length buf);
    Bytes.unsafe_to_string buf
end

(** The action of a transition is pair of:
    - a possibly empty list of registers to save the current state to
    - a possibly empty list of priority mappings for matching clauses
    - a target state (index of the state in the dfa array) *)
type ('clause, 'state) transition_action = {
  move: (Register.t * Register.t) list;
  store: Register.t list;
  clear: Register.t list;
  priority: ('clause index * RT.priority * RT.priority) list;
  target: 'state index;
}

type ('state, 'clause, 'lr1) state = {
  accept: ('clause index * RT.priority * RT.register option array) list;
  (** a clause to accept in this state. *)

  halting: 'lr1 IndexSet.t;
  (** The set of labels that should cause matching to halt (this can be seen as
      a transition to a "virtual" sink state). *)

  transitions: ('lr1 IndexSet.t * ('clause, 'state) transition_action) list;
  (** Transitions for this state, as a list of labels and actions. *)
}

type compact_dfa =
  RT.program_code * Lrgrep_support_packer.table * RT.program_counter array

let compare_priority (i1, s1, t1) (i2, s2, t2) =
  let c = compare_index i1 i2 in
  if c <> 0 then c else
    let c = Int.compare s1 s2 in
    if c <> 0 then c else
      let c = Int.compare t1 t2 in
      c

let compare_transition_action t1 t2 =
  let c = Int.compare (Index.to_int t1.target) (Index.to_int t2.target) in
  if c <> 0 then c else
    let c = List.compare (compare_pair compare_index compare_index)  t1.move t2.move in
    if c <> 0 then c else
      let c = List.compare compare_index t1.store t2.store in
      if c <> 0 then c else
        let c = List.compare compare_index t1.clear t2.clear in
        if c <> 0 then c else
          let c = List.compare compare_priority t1.priority t2.priority in
          c

let same_action a1 a2 = compare_transition_action a1 a2 = 0

(* Appendix A *)
let emit_moves code moves =
  let n = 1 + List.fold_left (fun n (src, _dst) -> Int.max n src) (-1) moves in
  (* A.1 Initialization *)
  let aux = Array.init n Fun.id in
  let mark = Array.make n false in
  let dup = Array.make n false in
  List.iter (fun (j, _) ->
      if mark.(j)
      then dup.(j) <- true
      else mark.(j) <- true
    ) moves;
  (* A.2 First pass *)
  List.iter (fun (j, i) ->
      if mark.(j) then (
        mark.(j) <- false;
        let j = aux.(j) in
        if i != j then (
          if i < n && (mark.(i) || dup.(i)) then (
            Code_emitter.emit code (Swap (j, i));
            aux.(i) <- j
          ) else
            Code_emitter.emit code (Move (j, i))
        );
        aux.(j) <- i
      )
    ) moves;
  (* A.3 Second pass *)
  List.iter (fun (j, i) ->
      if mark.(j) then (
        Code_emitter.emit code (Move (aux.(j), i))
      ) else
        mark.(j) <- true
    ) moves

let compact (type dfa clause lr1)
    (dfa : dfa cardinal)
    (get_state : dfa index -> (dfa, clause, lr1) state)
  =
  let code = Code_emitter.make () in
  let packer = Lrgrep_support_packer.make () in
  let halt_pc = ref (Code_emitter.position code) in
  Code_emitter.emit code Halt;
  let pcs = Vector.init dfa (fun _ -> ref (-1)) in
  let emit_action {move; store; clear; priority; target} =
    emit_moves code (move : (_ index * _ index) list :> (int * int) list);
    List.iter (fun i -> Code_emitter.emit code (Store (i : _ index :> int))) store;
    List.iter (fun i -> Code_emitter.emit code (Clear (i : _ index :> int))) clear;
    List.iter (fun (c, i, j) ->
        Code_emitter.emit code (Priority ((c : _ index :> int), i, j))
      ) priority;
    Code_emitter.emit_yield_reloc code (Vector.get pcs target);
  in
  let goto_action =
    let table = Hashtbl.create 7 in
    fun action ->
      match Hashtbl.find_opt table action with
      | Some r -> r
      | None ->
        let position = Code_emitter.position code in
        if debug then
          Printf.eprintf "Action at 0x%04X\n" position;
        emit_action action;
        let r = ref position in
        Hashtbl.add table action r;
        r
  in
  let transition_count = ref 0 in
  let transition_dom = ref 0 in
  let cell_count = ref 0 in
  let prepare_state index pc =
    let {halting; transitions; accept} = get_state index in
    let _, most_frequent_action =
      List.fold_left (fun (count, _ as default) (dom, action) ->
          let count' = IndexSet.cardinal dom in
          incr transition_count;
          transition_dom := !transition_dom + count';
          if count' > count
          then (count', Some action)
          else default
        ) (IndexSet.cardinal halting, None) transitions
    in
    let prepare_transition (dom, action) =
      match most_frequent_action with
      | Some action' when same_action action action' -> None
      | _ -> Some (dom, goto_action action)
    in
    let other = List.filter_map prepare_transition transitions in
    let other =
      if Option.is_some most_frequent_action &&
         IndexSet.cardinal halting > 0
      then (halting, halt_pc) :: other
      else other
    in
    let cardinal =
      List.fold_left
        (fun acc (set, _) -> IndexSet.cardinal set + acc)
        0 other
    in
    (cardinal, pc, accept, most_frequent_action, other)
  in
  let process_state (_cardinal, pc, accept, default, other_transitions) =
    assert (!pc = -1);
    pc := Code_emitter.position code;
    List.iter
      (fun (clause, priority, registers) ->
         Code_emitter.emit code (Accept (Index.to_int clause, priority, registers)))
      accept;
    begin match
        List.concat_map
          (fun (dom, target) ->
             List.map (fun x -> (x, target)) (IndexSet.elements dom))
          other_transitions
      with
      | [] -> ()
      | cells ->
        cell_count := !cell_count + List.length cells;
        let cells = (cells : (lr1 index * _) list :> (RT.lr1 * _) list) in
        let i = Lrgrep_support_packer.add_row packer cells in
        Code_emitter.emit_match_reloc code i;
    end;
    begin match default with
      | None -> Code_emitter.emit code Halt
      | Some default -> emit_action default
    end
  in
  let preparation = Vector.as_array (Vector.mapi prepare_state pcs) in
  Array.sort (fun (c1, _, _, _, _) (c2, _, _, _, _) -> Int.compare c2 c1)
    preparation;
  Array.iter process_state preparation;
  if debug then
    Vector.iteri (fun state pc ->
        Printf.eprintf "state % 4d at 0x%04X\n" (Index.to_int state) !pc)
      pcs;
  Printf.eprintf "total transitions: %d (domain: %d), non-default: %d\n%!"
    !transition_count !transition_dom !cell_count;
  let remap, table = Lrgrep_support_packer.pack packer (!) in
  let code = Code_emitter.link code remap in
  let pcs = Vector.as_array (Vector.map (!) pcs) in
  (code, table, pcs)
