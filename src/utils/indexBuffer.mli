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

(** Support uninitialized elements, but fails when getting uninitialized
    contents ([get] or [contents]). *)

module PDyn : sig
  (** Type representing a dynamic array indexed by indices. *)
  type ('n, 'a) t

  (** [make ()] creates a new uninitialied dynamic array. *)
  val make : unit -> ('n, 'a) t

  exception Uninitialized

  (** [get t i] returns the value at index [i] in array [t]. *)
  val get : ('n, 'a) t -> 'n index -> 'a

  (** [set t i x] sets the value at index [i] in array [t] to [x]. *)
  val set : ('n, 'a) t -> 'n index -> 'a -> unit

  (** Extract a static vector from a dynamic one once the cardinal is known. *)
  val contents : ('n, 'a) t -> 'n cardinal -> ('n, 'a) vector
end
