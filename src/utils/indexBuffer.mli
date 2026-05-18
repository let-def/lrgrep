open Fix.Indexing

(** Module for dynamic vectors, useful when the cardinal of a finite domain is
    not yet known. *)

module Dyn : sig
  (** Type representing a dynamic array indexed by indices. *)
  type ('n, 'a) t

  (** [make default] creates a new dynamic array with the given default value.
      @param default The default value for indices not yet set. *)
  val make : 'a -> ('n, 'a) t

  (** [get t i] returns the value at index [i] in array [t]. *)
  val get : ('n, 'a) t -> 'n index -> 'a

  (** [set t i x] sets the value at index [i] in array [t] to [x]. *)
  val set : ('n, 'a) t -> 'n index -> 'a -> unit

  (** Extract a static vector from a dynamic one once the cardinal is known. *)
  val contents : ('n, 'a) t -> 'n cardinal -> ('n, 'a) vector
end


(** Module for generating a new finite domain, allocating elements and
    associating a value to each element.

    FIXME: Deprecated, this should be re-designed.
*)

module Gen : sig
  type ('n, 'a) t

  type 'n reservation

  val add : ('n, 'a) t -> 'a -> 'n index

  val reserve : ('n, 'a) t -> 'n reservation

  val index  : 'n reservation -> 'n index

  val commit : ('n, 'a) t -> 'n reservation -> 'a -> unit

  val get : ('n, 'a) t -> 'n index -> 'a

  val set : ('n, 'a) t -> 'n index -> 'a -> unit

  (** [freeze t] returns a vector containing all committed values of the generator.
      The generator must not be used after freezing. *)
  val freeze : ('n, 'a) t -> ('n, 'a) vector

  (** [freeze_map t f] returns a vector by applying [f] to each index and value.
      The function receives the index and the associated value. *)
  val freeze_map : ('n, 'a) t -> ('n index -> 'a -> 'b) -> ('n, 'b) vector

  module Make () : sig
    type n
    val n : n cardinal

    (** [get_generator ()] returns a new generator instance.
      @raise Invalid_argument if called more than once. *)
    val get_generator : unit -> (n, 'a) t
  end
end
