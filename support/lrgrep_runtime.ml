type lr1 = int
type clause = int
type register = int

(* Representation of automaton as sparse tables and bytecoded programs *)

type sparse_table = string
type sparse_index = int

type program = string
type program_counter = int

type program_instruction =
  | Store of register
  | Move of register * register
  | Yield of program_counter
  | Accept of clause * int * int
  | Match of sparse_index
  | Halt

let get_int table ~offset = function
  | 1 -> Bytes.get_uint8 table offset
  | 2 -> Bytes.get_uint16_be table offset
  | 3 -> (Bytes.get_uint16_be table offset) lor
         (Bytes.get_uint8 table (offset + 2) lsl 16)
  | 4 -> Int32.to_int (Bytes.get_int32_be table offset)
  | _ -> assert false

let sparse_lookup (table : sparse_table) (index : sparse_index) (lr1 : lr1)
  : program_counter option =
  let table = Bytes.unsafe_of_string table in
  let ksize = Bytes.get_uint8 table 0 in
  let vsize = Bytes.get_uint8 table 1 in
  assert (index >= 0 && lr1 >= 0);
  let offset = 2 + (index + lr1) * (ksize + vsize) in
  if offset + 4 > Bytes.length table then
    None
  else if get_int table ~offset ksize = lr1 then
    Some (get_int table ~offset:(offset + ksize) vsize)
  else
    None

let program_step (t : program) (r : program_counter ref)
  : program_instruction =
  let pc = !r in
  let c = t.[pc] in
  let t = Bytes.unsafe_of_string t in
  match c with
  | '\x01' ->
    r := !r + 2;
    Store (Bytes.get_uint8 t (pc + 1))
  | '\x02' ->
    r := !r + 3;
    Move (Bytes.get_uint8 t (pc + 1), Bytes.get_uint8 t (pc + 2))
  | '\x03' ->
    r := !r + 4;
    Yield (get_int t ~offset:(pc + 1) 3)
  | '\x04' ->
    r := !r + 4;
    Accept (Bytes.get_uint8 t (pc + 1),
            Bytes.get_uint8 t (pc + 2),
            Bytes.get_uint8 t (pc + 3))
  | '\x05' ->
    r := !r + 3;
    Match (Bytes.get_uint16_be t (pc + 1))
  | '\x06' ->
    r := !r + 1;
    Halt
  | _ -> assert false

module type Parse_errors = sig
  val registers : int
  val initial : program_counter
  val table : sparse_table
  val program : program
end

module type Parser = sig
  type 'a env
  type element
  val current_state_number : 'a env -> int
  val top : 'a env -> element option
  val pop : 'a env -> 'a env option
end

module Interpreter (PE : Parse_errors) (P : Parser) =
struct

  let interpret bank env candidate (pc : program_counter) =
    let pc = ref pc in
    let rec loop () =
      match program_step PE.program pc with
      | Store reg ->
        bank.(reg) <- P.top env;
        loop ()
      | Move (r1, r2) ->
        (*prerr_endline "Store";*)
        bank.(r1) <- bank.(r2);
        loop ()
      | Yield pc' ->
        (*prerr_endline "Yield";*)
        Some pc'
      | Accept (clause, start, count) ->
        (*prerr_endline "Accept";*)
        candidate := (clause, Array.sub bank start count) :: !candidate;
        loop ()
      | Match index ->
        (*prerr_endline "Match";*)
        begin
          match sparse_lookup PE.table index (P.current_state_number env) with
          | Some pc' ->
            (*prerr_endline "Match success";*)
            pc := pc'
          | None ->
            (*prerr_endline "Match failure";*)
            ()
        end;
        loop ()
      | Halt ->
        (*prerr_endline "Halt";*)
        None
    in
    loop ()

  let run env =
    let bank = Array.make PE.registers None in
    let candidate = ref [] in
    let rec loop env pc =
      match interpret bank env candidate pc with
      | None -> ()
      | Some pc' ->
        let env = match P.pop env with
          | None -> env
          | Some env -> env
        in
        loop env pc'
    in
    loop env PE.initial;
    !candidate
    |> List.sort_uniq (fun (a,_) (b,_) -> Int.compare a b)
end
