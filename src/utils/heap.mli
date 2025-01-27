module Make(T : sig
    type 'a t
    val compare : 'a t -> 'a t -> int
  end) :
sig
  type ('a, 'b) t = private
    | Leaf
    | Node of ('a, 'b) t * 'a T.t * 'b * ('a, 'b) t * int

  val empty: ('a, 'b) t
  val merge: ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
  val singleton : 'a T.t -> 'b -> ('a, 'b) t
  val insert : 'a T.t -> 'b -> ('a, 'b) t -> ('a, 'b) t

  val pop : ('a, 'b) t -> ('a T.t * 'b * ('a, 'b) t) option

  type ('a, 'b) pop2 =
    | Head of 'a T.t * 'b * 'a T.t * 'b * ('a, 'b) t
    | Tail of 'a T.t * 'b
    | Done
  val pop2 : ('a, 'b) t -> ('a, 'b) pop2
end
