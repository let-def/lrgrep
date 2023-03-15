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

  val add : ('n, 'a) t -> 'a -> 'n index
  val add' : ('n, 'a) t -> ('n index -> 'a) -> 'n index * 'a

  val get : ('n, 'a) t -> 'n index -> 'a
  val set : ('n, 'a) t -> 'n index -> 'a -> unit
  val freeze : ('n, 'a) t -> ('n, 'a) vector

  module Make () : sig
    type n
    val n : n cardinal
    val get_generator : unit -> (n, 'a) t
  end
end
