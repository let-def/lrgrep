open Utils
open Misc
open Fix.Indexing

module Register : sig
  type n
  val n : n cardinal
  type t = n index
  type set = n indexset
  type 'a map = (n, 'a) indexmap
  val of_int : int -> t
end

module RT = Lrgrep_runtime

(** Emit and link bytecode program *)
module Code_emitter : sig

  (** A code emitter. *)
  type t

  (** Create a new emitter *)
  val make : unit -> t

  (** The [pc] of the next emitted instruction. Starts at [0]. *)
  val position : t -> RT.program_counter

  (** Emit a new instruction *)
  val emit : t -> RT.program_instruction -> unit

  (** Emit a yield instruction to a target that (may) not yet be known.
      The reference will be read during linking. *)
  val emit_yield_reloc : t -> RT.program_counter ref -> unit

  (** Emit a match instruction to a vector that (may) not yet be known.
      The promise will be resolved during linking. *)
  val emit_match_reloc : t -> Lrgrep_support_packer.promise -> unit

  (** Produce a final program, with all emitted instructions at the
      correct positions and reading all pending relocations references. *)
  val link : t -> Lrgrep_support_packer.row_mapping -> RT.program_code
end

(** The action of a transition is pair of:
    - a possibly empty list of registers to save the current state to
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

(* It is not necessarily the case that
    [state.halting  ⋃  {fst tr | tr ∈  state.transitions} = Lr1.all].

   Since only valid LR stacks are expected to be matched, some transitions are
   impossible. This translates to a partial definition of transitions.
   The runtime behavior is undefined if an impossible transition is taken:
   the optimizer is free to chose the interpretation that suits it the most.
*)

(** The result of compaction is a program, a sparse table, and an array
    mapping each DFA state to the PC of the instructions that implement this
    state. *)
type compact_dfa = RT.program_code * Lrgrep_support_packer.table * RT.program_counter array

(** Run the compaction algorithm on a dfa *)
val compact : 'state cardinal -> ('state index -> ('state, _, _) state) -> compact_dfa
