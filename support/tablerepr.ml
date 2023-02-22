type lr1 = int
type clause = int
type var = int
type register = clause * var
type arities = int array

module Code : sig

  type bytecode =
    | Var of int * int
    | Goto of int
    | Accept of int
    | Match of int
    | Halt

  type emitter

  val make : unit -> emitter
  val position : emitter -> int
  val emit : emitter -> bytecode -> unit
  val emit_goto_reloc : emitter -> int ref -> unit

  type repr = string
  val repr : emitter -> repr

  val step : repr -> int ref -> bytecode
end = struct

  type bytecode =
    | Var of int * int
    | Goto of int
    | Accept of int
    | Match of int
    | Halt

  type emitter = {
    mutable reloc: (int * int ref) list;
    buffer: Buffer.t;
  }

  let make () = {
    reloc = [];
    buffer = Buffer.create 15;
  }

  let position t = Buffer.length t.buffer

  let emit t = function
    | Var (i, j) ->
      assert (i <= 0xFF && j <= 0xFF);
      Buffer.add_char t.buffer '\x01';
      Buffer.add_uint8 t.buffer i;
      Buffer.add_uint8 t.buffer j
    | Goto pos ->
      assert (pos <= 0xFFFF);
      Buffer.add_char t.buffer '\x02';
      Buffer.add_uint16_be t.buffer pos
    | Accept clause ->
      Buffer.add_char t.buffer '\x03';
      Buffer.add_uint8 t.buffer clause
    | Match index ->
      Buffer.add_char t.buffer '\x04';
      assert (index <= 0xFFFF);
      Buffer.add_uint16_be t.buffer index
    | Halt ->
      Buffer.add_char t.buffer '\x05'

  let emit_goto_reloc t reloc =
    Buffer.add_char t.buffer '\x02';
    let pos = Buffer.length t.buffer in
    Buffer.add_uint16_be t.buffer 0;
    t.reloc <- (pos, reloc) :: t.reloc

  type repr = string

  let repr t =
    let buf = Buffer.to_bytes t.buffer in
    List.iter (fun (pos, reloc) ->
        assert (0 <= !reloc && !reloc < 0xFFFF);
        Bytes.set_int16_be buf pos !reloc
      ) t.reloc;
    Bytes.unsafe_to_string buf

  let step t r =
    let pc = !r in
    match t.[pc] with
    | '\x01' ->
      r := !r + 3;
      Var (String.get_uint8 t (pc + 1), String.get_uint8 t (pc + 2))
    | '\x02' ->
      r := !r + 3;
      Goto (String.get_uint16_be t (pc + 1))
    | '\x03' ->
      r := !r + 2;
      Accept (String.get_uint8 t (pc + 1))
    | '\x04' ->
      r := !r + 5;
      Match (String.get_uint16_be t (pc + 3))
    | '\x05' ->
      r := !r + 1;
      Halt
    | _ -> assert false
end

type transition_action = (int * int) list * int

let compare_ints (i1, j1) (i2, j2) =
  let c = Int.compare i1 i2 in
  if c <> 0 then c else
    Int.compare j1 j2

let compare_transition_action (v1, t1) (v2, t2) =
  let c = Int.compare t1 t2 in
  if c <> 0 then c else
    List.compare compare_ints v1 v2

module IS = Utils.BitSet.IndexSet

let compact (dfa : (int option * (_ IS.t * transition_action) list) array) =
  let code = Code.make () in
  let index = Sparse_index.make () in
  let pcs = Array.init (Array.length dfa) (fun _ -> ref (-1)) in
  let emit_action (vars, target) =
    List.iter (fun (i, j) -> Code.emit code (Var (i, j))) vars;
    Code.emit_goto_reloc code pcs.(target);
  in
  let goto_action = function
    | ([], target) -> pcs.(target)
    | action ->
      let position = Code.position code in
      emit_action action;
      ref position
  in
  Array.iter2 (fun (accept, transitions) pc ->
      let transitions = match transitions with
        | [] -> None
        | [(_, t)] -> Some (t, [])
        | ((_, t) :: _) ->
          let _, most_frequent_action =
            List.fold_left (fun (count, _ as default) (dom, action) ->
                let count' = IS.cardinal dom in
                if count' > count
                then (count', action)
                else default
              ) (0, t) transitions
          in
          let other_transitions =
            List.filter_map (fun (dom, action) ->
                if compare_transition_action action most_frequent_action = 0
                then None
                else (
                  Some (dom, goto_action action)
                )
              ) transitions
          in
          Some (most_frequent_action, other_transitions)
      in
      assert (!pc = -1);
      pc := Code.position code;
      begin match accept with
        | None -> ()
        | Some clause -> Code.emit code (Accept clause)
      end;
      begin match transitions with
        | None -> Code.emit code Halt
        | Some (default, []) ->
          emit_action default
        | Some (default, cells) ->
          let cells =
            List.concat_map
              (fun (dom, target) ->
                 List.map
                   (fun x -> (x, target))
                   (IS.elements dom : _ Fix.Indexing.index list :> int list))
              cells
          in
          let i = Sparse_index.add_vector index cells in
          Code.emit code (Match i);
          emit_action default
      end
    ) dfa pcs;
  (Code.repr code, Sparse_index.pack index (!), Array.map (!) pcs)
