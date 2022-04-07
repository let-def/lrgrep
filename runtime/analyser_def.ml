type clause = int
type var = int
type register = clause * var
type arities = int array

module Registers : sig
  type 'a bank
  val make_bank : arities -> 'a bank
  val store : 'a bank -> register -> 'a -> unit

  type 'a t = 'a option array
  val get : 'a bank -> clause -> 'a t
end = struct
  type 'a bank = 'a option array array

  let make_bank clauses =
    Array.map (fun arity -> Array.make arity None) clauses

  let store bank (clause, var) value =
    bank.(clause).(var) <- Some value

  type 'a t = 'a option array
  let get bank clause =
    Array.copy bank.(clause)
end

module type Parse_errors = sig
  val arities : int array
  module Table : sig
    type state
    val initial : state
    val step : state -> clause -> register list * clause option * state option
  end
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
  let run env =
    let bank = Registers.make_bank PE.arities in
    let candidate = ref None in
    let update_candidate clause =
      let should_update =
        match !candidate with
        | None -> true
        | Some (clause', _) -> clause < clause'
      in
      if should_update then
        candidate := Some (clause, Registers.get bank clause)
    in
    let rec loop env state =
      let registers, clause, next =
        PE.Table.step state (P.current_state_number env)
      in
      Option.iter update_candidate clause;
      begin match registers with
        | [] -> ()
        | regs ->
          let element = Option.get (P.top env) in
          List.iter (fun reg -> Registers.store bank reg element)
            regs
      end;
      begin match next with
        | None -> ()
        | Some state' ->
          match P.pop env with
          | None -> ()
          | Some env' -> loop env' state'
      end
    in
    loop env PE.Table.initial;
    !candidate
end
