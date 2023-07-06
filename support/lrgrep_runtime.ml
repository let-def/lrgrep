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
  | Clear of register
  | Yield of program_counter
  | Accept of clause * register option array
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
  else if get_int table ~offset ksize = lr1 + 1 then
    Some (get_int table ~offset:(offset + ksize) vsize)
  else
    None

let get_uint24_be str i =
  (String.get_uint16_be str i) lor (String.get_uint8 str (i + 2) lsl 16)

let program_step (t : program) (r : program_counter ref)
  : program_instruction =
  let pc = !r in
  match t.[pc] with
  | '\x01' ->
    r := !r + 2;
    Store (String.get_uint8 t (pc + 1))
  | '\x02' ->
    r := !r + 3;
    Move (String.get_uint8 t (pc + 1), String.get_uint8 t (pc + 2))
  | '\x03' ->
    r := !r + 2;
    Clear (String.get_uint8 t (pc + 1))
  | '\x04' ->
    r := !r + 4;
    Yield (get_int t ~offset:(pc + 1) 3)
  | '\x05' ->
    let arity = String.get_uint8 t (pc + 1) in
    r := !r + 4 + arity;
    let registers = Array.init arity (fun i ->
        let x = String.get_uint8 t (pc + 4 + i) in
        if x = 255 then None else Some x
      ) in
    Accept (String.get_uint16_be t (pc + 2), registers)
  | '\x06' ->
    r := !r + 4;
    Match (get_uint24_be t (pc + 1))
  | '\x07' ->
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
  val really_top : 'a env -> element
  val pop : 'a env -> 'a env option
end

module Interpreter (PE : Parse_errors) (P : Parser) =
struct

  let debug = false

  let eprintf = Printf.eprintf

  let print_regs bank regs =
    Printf.sprintf "[%s]"
      (String.concat ", "
         (List.map (function
              | None -> "None"
              | Some i -> "%" ^ string_of_int i ^ " = " ^ match bank.(i) with
                | None -> "None"
                | Some _ -> "Some _"
            ) (Array.to_list regs)))

  let interpret_last bank candidate pc =
    match program_step PE.program (ref pc) with
    | Accept (clause, registers) ->
      if debug then eprintf "Accept (%d,%s) (bottom)\n" clause (print_regs bank registers);
      let may_get = function
        | None -> None
        | Some i -> bank.(i)
      in
      candidate := (clause, Array.map may_get registers) :: !candidate
    | _ -> ()

  let interpret bank env candidate (pc : program_counter) =
    let pc = ref pc in
    let rec loop () =
      match program_step PE.program pc with
      | Store reg ->
        bank.(reg) <- Some (P.really_top env);
        loop ()
      | Move (r1, r2) ->
        (*prerr_endline "Store";*)
        bank.(r2) <- bank.(r1);
        loop ()
      | Clear r1 ->
        bank.(r1) <- None;
        loop ()
      | Yield pc' ->
        (*prerr_endline "Yield";*)
        Some pc'
      | Accept (clause, registers) ->
        (*prerr_endline "Accept";*)
        let may_get = function
          | None -> None
          | Some i -> bank.(i)
        in
        candidate := (clause, Array.map may_get registers) :: !candidate;
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
        match P.pop env with
        | None -> interpret_last bank candidate pc'
        | Some env -> loop env pc'
    in
    loop env PE.initial;
    let rec uniq k v = function
      | [] -> [k, v]
      | (k', v') :: rest ->
        if Int.equal k k' then
          uniq k v rest
        else
          (k, v) :: uniq k' v' rest
    in
    match
      List.stable_sort (fun (a,_) (b,_) -> Int.compare a b)
        !candidate
    with
    | (k, v) :: rest -> uniq k v rest
    | [] -> []
end
