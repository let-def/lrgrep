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

module Sparse_packer : sig
  type 'a t
  type 'a vector = (RT.lr1 * 'a) list
  type displacement

  val make : unit -> 'a t

  val add_vector : 'a t -> (RT.lr1 * 'a) list -> displacement
  val pack : 'a t -> ('a -> RT.program_counter) -> RT.sparse_table

  val get_displacement : displacement -> int
end = struct

  type displacement = int ref
  type 'a vector = (RT.lr1 * 'a) list
  type 'a t = (displacement * 'a vector) list ref

  (* Packer implementation *)

  let make () = ref []

  let get_displacement disp =
    if !disp = min_int then
      invalid_arg "Sparse_packer.get_displacement: table has not been packed yet";
    !disp

  let add_vector t v =
    let disp = ref min_int in
    push t (disp, v);
    disp

  (* Vector fitting *)

  type key = RT.lr1
  type value = RT.program_counter

  type cell =
    | Unused
    | Used0 of (key * value)
    | Used1 of (key * value)

  let cell_unused = function
    | Unused -> true
    | _ -> false

  let set_cell arr index k v =
    assert (index >= 0);
    let length = Array.length !arr in
    if index >= length then (
      let length' = max (index + 1) (length * 2) in
      let arr' = Array.make length' Unused in
      Array.blit !arr 0 arr' 0 length;
      arr := arr'
    );
    match arr.contents.(index) with
    | Unused ->
      arr.contents.(index) <- Used0 (k, v)
    | _ ->
      match arr.contents.(index+1) with
      | Unused -> arr.contents.(index+1) <- Used1 (k, v)
      | _ -> assert false

  let fit_vector arr = function
    | [] -> 0
    | (ofs, _) :: _ as cells ->
      let fit index =
        let rec loop k' = function
          | [] -> true
          | (k, _) :: _ when index + k >= Array.length !arr - 1 -> true
          | (k, _) :: xs ->
            if k' < k && cell_unused arr.contents.(index + k) then
              loop k xs
            else
              cell_unused arr.contents.(index + k + 1) &&
              loop (k + 1) xs
        in
        loop (-1) cells
      in
      let i = ref (-ofs) in
      while not (fit !i) do incr i done;
      let index = !i in
      List.iter (fun (k, v) -> set_cell arr (index + k) k v) cells;
      index

  let fit_vectors (rows : (int ref * RT.program_counter vector) list) : int * cell array =
    let arr = ref (Array.make 16 Unused) in
    let rows =
      match
        (* Sort by decreasing length *)
        List.sort (fun (_, r1) (_, r2) ->
            let c = List.compare_lengths r2 r1 in
            if c <> 0 then c else
              List.compare (compare_pair Int.compare Int.compare) r1 r2
          ) rows
      with
      | [] -> []
      | (d, r) :: rest ->
        (* Merge equal vectors *)
        let rec merge ds r = function
          | [] -> [(ds, r)]
          | (d', r') :: rest ->
            if List.equal (fun (a1,a2) (b1,b2) -> a1 = a2 && b1 = b2) r r' then
              merge (d' :: ds) r rest
            else
              (ds, r) :: merge [d'] r' rest
        in
        merge [d] r rest
    in
    let disps = List.map (fun (_, r) -> fit_vector arr r) rows in
    let min_disp = - List.fold_left Int.min 0 disps in
    List.iter2 (fun (ds, _) disp ->
        let disp = disp + min_disp in
        List.iter (fun d -> d := disp) ds)
      rows disps;
    (min_disp, !arr)

  (* Sparse table encoding *)

  let set_int table ~offset ~value = function
    | 1 -> Bytes.set_uint8 table offset value
    | 2 ->
      Bytes.set_uint16_be table offset value
    | 3 ->
      Bytes.set_uint16_be table offset (value land 0xFFFF);
      Bytes.set_uint8 table (offset + 2) (value lsr 16)
    | 4 -> Bytes.set_int32_be table offset (Int32.of_int value)
    | _ -> assert false

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

  let pack t link =
    let max_k = ref 0 in
    let max_v = ref 0 in
    let min_disp, table =
      (* Ensure keys are increasing and link values *)
      let rec prepare_row last_k = function
        | [] -> []
        | (k, v) :: rest ->
          if last_k >= k then
            invalid_arg "pack: row indices are not strictly increasing";
          let v = link v in
          if v < 0 then
            invalid_arg "pack: row value is negative after linking";
          if k > !max_k then max_k := k;
          if v > !max_v then max_v := v;
          (k, v) :: prepare_row k rest
      in
      fit_vectors (List.rev_map (fun (d,r) -> (d, prepare_row (-1) r)) !t)
    in
    let length = ref (Array.length table) in
    while !length > 0 && cell_unused table.(!length - 1) do
      decr length
    done;
    let length = !length in
    let k_size = int_size (!max_k * 2 + 1) in
    let v_size = int_size !max_v in
    if min_disp < 0 || min_disp > 0xFFFF then
      Printf.ksprintf invalid_arg
        "pack: internal limit reached (minimal displacement %d does not fit on 16 bits)"
        min_disp;
    let repr = Bytes.make (4 + (length + 2) * (k_size + v_size)) '\x00' in
    set_int repr ~offset:0 ~value:k_size 1;
    set_int repr ~offset:1 ~value:v_size 1;
    set_int repr ~offset:2 ~value:min_disp 2;
    let unused = ref 0 in
    for i = 0 to length - 1 do
      let offset = 4 + i * (k_size + v_size) in
      match table.(i) with
      | Unused ->
        incr unused
      | Used0 (k, v) ->
        set_int repr ~offset ~value:((k + 1) lsl 1) k_size;
        set_int repr ~offset:(offset + k_size) ~value:v v_size
      | Used1 (k, v) ->
        set_int repr ~offset ~value:(((k + 1) lsl 1) lor 1) k_size;
        set_int repr ~offset:(offset + k_size) ~value:v v_size
    done;
    Printf.eprintf "max key: %d\nmax value: %d\n\n" !max_k !max_v;
    Printf.eprintf "key size: %d\nvalue size: %d\n" k_size v_size;
    Printf.eprintf "table size: %d (%d unused)\nrepr size: %d\ndisplacement offset:%d\n"
      length !unused
      (Bytes.length repr)
      min_disp;
    if false then (
      let hist_unused = Array.make 1000 0 in
      let consec_unused = ref 0 in
      let flush_unused () =
        hist_unused.(!consec_unused) <- hist_unused.(!consec_unused) + 1;
        unused := !unused + !consec_unused;
        consec_unused := 0
      in
      flush_unused ();
      Printf.eprintf "unused ratio: %.02f%%\n" (100.0 *. float !unused /. float length);
      Array.iter begin function
        | Unused -> incr consec_unused;
        | Used0 _ | Used1 _ -> flush_unused ();
      end table;
      flush_unused ();
      for i = 0 to 999 do
        if hist_unused.(i) > 0 then
          Printf.eprintf "%d consecutive unused appeared %d times\n" i hist_unused.(i)
      done;
    );
    Bytes.unsafe_to_string repr
end

module Code_emitter : sig
  type t
  val make : unit -> t
  val position : t -> int
  val emit : t -> RT.program_instruction -> unit
  val emit_yield_reloc : t -> RT.program_counter ref -> unit
  val emit_match : t -> Sparse_packer.displacement -> unit
  val link : t -> RT.program_code
end = struct
  type t = {
    mutable reloc: (int * int ref) list;
    mutable disps: (int * Sparse_packer.displacement) list;
    buffer: Buffer.t;
  }

  let make () = {
    reloc = [];
    disps = [];
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
      assert (index > 0 && index <= 0xFFFFFF);
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
    Buffer.add_char t.buffer '\x04';
    let pos = Buffer.length t.buffer in
    Buffer.add_string t.buffer "   ";
    t.reloc <- (pos, reloc) :: t.reloc

  let emit_match t disp =
    Buffer.add_char t.buffer '\x06';
    let pos = Buffer.length t.buffer in
    Buffer.add_string t.buffer "   ";
    t.disps <- (pos, disp) :: t.disps

  let link t =
    let buf = Buffer.to_bytes t.buffer in
    List.iter (fun (pos, reloc) ->
        assert (0 <= !reloc && !reloc < 0xFFFFFF);
        Bytes.set_uint16_be buf pos (!reloc land 0xFFFF);
        Bytes.set_uint8 buf (pos + 2) (!reloc lsr 16);
      ) t.reloc;
    List.iter (fun (pos, disp) ->
        let reloc = Sparse_packer.get_displacement disp in
        assert (0 <= reloc && reloc < 0xFFFFFF);
        Bytes.set_uint16_be buf pos (reloc land 0xFFFF);
        Bytes.set_uint8 buf (pos + 2) (reloc lsr 16);
      ) t.disps;
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

type compact_dfa = RT.program_code * RT.sparse_table * RT.program_counter array

let compare_transition_action t1 t2 =
  let c = Int.compare (Index.to_int t1.target) (Index.to_int t2.target) in
  if c <> 0 then c else
    let c = List.compare (compare_pair compare_index compare_index)  t1.move t2.move in
    if c <> 0 then c else
      let c = List.compare compare_index t1.store t2.store in
      if c <> 0 then c else
        let c = List.compare compare_index t1.clear t2.clear in
        c

let same_action a1 a2 = compare_transition_action a1 a2 = 0

let compact (type dfa clause lr1)
    (dfa : dfa cardinal)
    (get_state : dfa index -> (dfa, clause, lr1) state)
  =
  let code = Code_emitter.make () in
  let packer = Sparse_packer.make () in
  let halt_pc = ref (Code_emitter.position code) in
  Code_emitter.emit code Halt;
  let pcs = Vector.init dfa (fun _ -> ref (-1)) in
  let rec emit_moves = function
    | (j, i) :: rest ->
      if i < j then (
        emit_moves rest;
        Code_emitter.emit code (Move (j, i));
      ) else if i > j then (
        Code_emitter.emit code (Move (j, i));
        emit_moves rest;
      ) else
        emit_moves rest
    | [] -> ()
  in
  let emit_action {move; store; clear; priority; target} =
    emit_moves (move : (_ index * _ index) list :> (int * int) list);
    List.iter (fun i -> Code_emitter.emit code (Store (i : _ index :> int))) store;
    List.iter (fun i -> Code_emitter.emit code (Clear (i : _ index :> int))) clear;
    List.iter (fun (c, i, j) ->
        Code_emitter.emit code (Priority ((c : _ index :> int), i, j))
      ) priority;
    Code_emitter.emit_yield_reloc code (Vector.get pcs target);
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
        Code_emitter.emit_match code
          (Sparse_packer.add_vector packer cells)
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
  Printf.eprintf "total transitions: %d (domain: %d), non-default: %d\n%!"
    !transition_count !transition_dom !cell_count;
  let index = Sparse_packer.pack packer (!) in
  let code = Code_emitter.link code in
  let pcs = Vector.as_array (Vector.map (!) pcs) in
  (code, index, pcs)
