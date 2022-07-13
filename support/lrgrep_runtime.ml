type lr1 = int
type clause = int
type var = int
type register = clause * var
type arities = int array

(* Representation of automaton as sparse tables and bytecoded programs *)

type sparse_table = string
type sparse_index = int

type program = string
type program_counter = int

type program_instruction =
  | Store of register
  | Goto of program_counter
  | Accept of clause
  | Match of sparse_index
  | Halt

let get_int table ~offset = function
  | 1 -> String.get_uint8 table offset
  | 2 -> String.get_uint16_be table offset
  | 3 -> (String.get_uint16_be table offset) lor
         (String.get_uint8 table (offset + 2) lsl 16)
  | 4 -> Int32.to_int (String.get_int32_be table offset)
  | _ -> assert false

let sparse_lookup (table : sparse_table) (index : sparse_index) (lr1 : lr1)
  : program_counter option =
  let ksize = String.get_uint8 table 0 in
  let vsize = String.get_uint8 table 1 in
  assert (index >= 0 && lr1 >= 0);
  let offset = 2 + (index + lr1) * (ksize + vsize) in
  if offset + 4 > String.length table then
    None
  else if get_int table ~offset ksize = lr1 then
    Some (get_int table ~offset:(offset + ksize) vsize)
  else
    None

let program_step (t : program) (r : program_counter ref)
  : program_instruction =
  let pc = !r in
  match t.[pc] with
  | '\x01' ->
    r := !r + 3;
    Store (String.get_uint8 t (pc + 1), String.get_uint8 t (pc + 2))
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

module type Parse_errors = sig
  val arities : int array
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

  let step bank env candidate (pc : program_counter) =
    let rec loop () =
      let pc = ref pc in
      match program_step PE.program pc with
      | Store (clause, var) ->
        bank.(clause).(var) <- P.top env;
        loop ()
      | Goto pc' -> Some pc'
      | Accept clause ->
        begin match !candidate with
          | Some clause' when clause >= clause' -> ()
          | _ -> candidate := Some clause
        end;
        loop ()
      | Match index ->
        begin
          match sparse_lookup PE.table index (P.current_state_number env) with
          | Some pc' -> pc := pc'
          | None -> ()
        end;
        loop ()
      | Halt ->
        None
    in
    loop ()

  let run env =
    let bank = Array.map (fun a -> Array.make a None) PE.arities in
    let candidate = ref None in
    let rec loop env pc =
      match step bank env candidate pc with
      | None -> ()
      | Some pc' -> loop env pc'
    in
    loop env PE.initial;
    match !candidate with
    | None -> None
    | Some clause -> Some (clause, bank.(clause))
end
