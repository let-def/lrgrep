open Fix.Indexing

type ('n, 'a) t

val make : 'a -> ('n, 'a) t
val get : ('n, 'a) t -> 'n index -> 'a
val set : ('n, 'a) t -> 'n index -> 'a -> unit
val contents : ('n, 'a) t -> 'n cardinal -> ('n, 'a) vector

module type GEN = sig
  type 'n elt
  type n
  val n : n cardinal

  val add : n elt -> n index

  val get : n index -> n elt
  val set : n index -> n elt -> unit

  val freeze : unit -> (n, n elt) vector
end

module Gen(T : sig type 'n t end)() : GEN with type 'n elt := 'n T.t

(*val gen : unit -> (module GEN with type elt = 'a)*)
