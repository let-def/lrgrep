(** Compressed sparse bit set represented as a linked list of bitmasks, such
    that elements from the same group of [Sys.word_size - 1] are stored in a
    single cell. *)

module type S = SetSig.S0

(** [intSet] is a set of integers implemented as a sparse bit set. *)
include S with type element = int

(** [allocate s] finds the first integer not in [s] and returns it.
    The set is modified to mark the returned integer as allocated.
    @param s A reference to the set.
    @return The allocated integer. *)
val allocate : t ref -> int
