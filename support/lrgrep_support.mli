module RT = Lrgrep_runtime

module Sparse_packer : sig
  type 'a t
  val make : unit -> 'a t
  val add_vector : 'a t -> (RT.lr1 * 'a) list -> RT.sparse_index
  val pack : 'a t -> ('a -> RT.program_counter) -> RT.sparse_table
end

module Code_emitter : sig
  type t
  val make : unit -> t
  val position : t -> int
  val emit : t -> RT.program_instruction -> unit
  val emit_yield_reloc : t -> RT.program_counter ref -> unit
  val link : t -> RT.program
end

type transition_action = RT.register list * int

val compact :
  (int option * (Utils.BitSet.IntSet.t * transition_action) list) array ->
  RT.program * RT.sparse_table * RT.program_counter array
