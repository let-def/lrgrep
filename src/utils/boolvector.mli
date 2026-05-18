open Fix.Indexing

(** A vector of bools implemented as compact byte array (8 bools per byte) for
    efficient storage *)

type 'n t

(** [make c default] creates a new vector of cardinal [c], with all bools
    initialized to [default].
    @param c The domain of the vector.
    @param default The initial value for all bools. *)
val make : 'n cardinal -> bool -> 'n t

(** [init c f] creates a new vector of cardinal [c], where each bool is
    determined by the function [f]. *)
val init : 'n cardinal -> ('n index -> bool) -> 'n t

(** [test b n] returns the value of the bool at index [n] in vector [b]. *)
val test : 'n t -> 'n index -> bool

(** [set b n] sets the bool at index [n] in vector [b] to [true]. *)
val set : 'n t -> 'n index -> unit

(** [clear b n] clears the bool at index [n] in vector [b] (sets it to [false]). *)
val clear : 'n t -> 'n index -> unit

(** [from_vector vec f] creates a new vector where each bool is set to [true]
    if the corresponding element in [vec] satisfies predicate [f]. *)
val from_vector : ('n, 'a) vector -> ('a -> bool) -> 'n t
