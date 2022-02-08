open Fix.Indexing

type ('n, 'a) t

val make : 'a -> ('n, 'a) t
val get : ('n, 'a) t -> 'n index -> 'a
val set : ('n, 'a) t -> 'n index -> 'a -> unit
val contents : ('n, 'a) t -> 'n cardinal -> ('n, 'a) vector

module type GEN = sig
  type elt
  type n
  val n : n cardinal

  val push : elt -> n index

  val get : n index -> elt
  val set : n index -> elt -> unit

  val freeze : unit -> (n, elt) vector
end

module Gen(T : sig type t end)() : GEN with type elt := T.t

val gen : unit -> (module GEN with type elt = 'a)
