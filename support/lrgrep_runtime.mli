(** The Runtime library is used by the generated programs.
    It defines a compact representation for the automaton in the form of simple
    bytecoded instructions and a sparse index. *)

(** At runtime, an lr1 state is just an integer *)
type lr1 = int

(** For the interpreter, a clause is also represented by an integer.
    When a clause matches, its id is returned and it is the duty of the
    (generated) client program to map it to a semantic action. *)
type clause = int

(** Variables are also named by small integers, attributed monotonically
    starting from 0 by traversing the clause pattern from right to left. *)
type var = int

(** A register is the pair of a clause and a variable.
    A variable name is unique only within a clause. *)
type register = clause * var

(** The arities array describe the number of variables of each clause.
    For instance, an array [|1; 2|] means the matching program has two clauses
    the first one with a single variable and the second one with two variables.
    The corresponding registers are therefor (0, 0), (1, 0) and (1, 1). *)
type arities = int array

(* Representation of the automaton as sparse tables and bytecoded programs *)

(** A sparse table stores many partial mapping from [0..k-1] to [0..v-1]
    (the set of keys and values) and is directly serialized to a string.

    The first two bytes are used to encode the size in bytes of keys and
    values. Possible values are 1, 2, 3, or 4.
    For instance, a table starting with "\x01\x02..." represents a table
    with one byte keys and two bytes values.

    The rest is just a sequence of cells composed of a key and a value.
    The size of a table is therefore always of the form:
      2 + (size_k + size_v) * n

    In the current implementation, keys are lr1 states and values are program
    counters.
*)
type sparse_table = string

(** A sparse index identifies a cell in a sparse table.
    Indexing starts after the first two bytes that determine the size of a
    cell. The cell with index [i] starts at 2 + (size_k + size_v) * i
*)
type sparse_index = int

(** A program is a sequence of serialized [program_instruction] *)
type program = string

(** A [program_counter] is an offset of the [program] string, and by
    construction always point at the beginning of an instruction
    (it is invalid otherwise). *)
type program_counter = int

(** The instructions of the bytecode language of the matcher *)
type program_instruction =
  | Store of register
    (** [Store r] stores the state at the top of the parser stack in
        register [r]. *)
  | Yield of program_counter
    (** Jump and consume input:
        [Yield pc] stops the current interpretation to consume one state of the
        input stack. After consuming, execution should resume at [pc]. *)
  | Accept of clause
    (** When reaching [Accept id], the matcher found that clause number [id] is
        matching. Add it to the set of matching candidates and resume
        execution. *)
  | Match of sparse_index
    (** [Match sidx] lookup the sparse table for a cell matching the state
        at the top of the parser stack at index [sidx].
        If the lookup is successful, execution should jump to
    *)
  | Halt
    (** Program is finished, there will be no more matches. *)

(** [sparse_lookup table sidx state] searches for in [table] from index [sidx]
    for a cell mapping state [lr1].
    This realizes the lookup part of a [Match] instruction. *)
val sparse_lookup : sparse_table -> sparse_index -> lr1 -> program_counter option

(** [program_step program pc] decodes the instruction at address [!pc] and
    increases [pc]. *)
val program_step : program -> program_counter ref -> program_instruction

(** All the elements composing a parse error matching program. *)
module type Parse_errors = sig
  val arities : int array
  val initial : program_counter
  val table : sparse_table
  val program : program
end

(** A minimal module type mimicking [Menhir] incremental interface,
    suitable to run a matching program. *)
module type Parser = sig
  (** Represents the state of the parser (mostly a parsing stack) *)
  type 'a env

  (** The type of semantic values stored in stack frame *)
  type element

  (** Returns the current lr1 state (the state at the top of the stack) *)
  val current_state_number : 'a env -> int

  (** Returns the semantic value at the top of the stack, if any. *)
  val top : 'a env -> element option

  (** Returns the stack with the top frame removed,
      or [None] for an empty stack *)
  val pop : 'a env -> 'a env option
end

(** Instantiate an interpreter for a parse error program and a parser
    representation *)
module Interpreter (PE : Parse_errors) (P : Parser) : sig

  (** Runs the program on a concrete parser.
      Returns the list of matching clauses with the captured variables. *)
  val run : 'a P.env -> (clause * P.element option array) list

end
