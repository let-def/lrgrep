(* Reported by Gospel users *)
module type A = sig
  val f : int -> [@@gospel "..."] int
end
