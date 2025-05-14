open Fix.Indexing

module Dyn : sig
  type ('n, 'a) t
  val make : 'a -> ('n, 'a) t
  val get : ('n, 'a) t -> 'n index -> 'a
  val set : ('n, 'a) t -> 'n index -> 'a -> unit
  val contents : ('n, 'a) t -> 'n cardinal -> ('n, 'a) vector
end


module Gen : sig
  type ('n, 'a) t

  type 'n reservation

  val add : ('n, 'a) t -> 'a -> 'n index
  val reserve : ('n, 'a) t -> 'n reservation
  val index  : 'n reservation -> 'n index
  val commit : ('n, 'a) t -> 'n reservation -> 'a -> unit

  val get : ('n, 'a) t -> 'n index -> 'a
  val set : ('n, 'a) t -> 'n index -> 'a -> unit
  val freeze : ('n, 'a) t -> ('n, 'a) vector
  val freeze_map : ('n, 'a) t -> ('n index -> 'a -> 'b) -> ('n, 'b) vector

  module Make () : sig
    type n
    val n : n cardinal
    val get_generator : unit -> (n, 'a) t
  end
end
