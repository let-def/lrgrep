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
  | Yield of program_counter
  | Accept of clause
  | Match of sparse_index
  | Halt

val sparse_lookup : sparse_table -> sparse_index -> lr1 -> program_counter option

val program_step : program -> program_counter ref -> program_instruction

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

module Interpreter (PE : Parse_errors) (P : Parser) : sig
  val run : 'a P.env -> (program_counter * P.element option array) option
end
