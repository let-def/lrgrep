open Fix.Indexing

module Increasing : sig
  type ('a, 'b) piece = ('a IndexSet.t * 'b IndexSet.t)
  type ('a, 'b) t = private ('a, 'b) piece list

  val minimum : ('a, 'b) t
  val is_minimum : ('a, 'b) t -> bool
  val piece : 'a IndexSet.t -> 'b IndexSet.t -> ('a, 'b) t
  val piecewise : ('a IndexSet.t * 'b IndexSet.t) list -> ('a, 'b) t
  val increase :
    ?ignore:'b IndexSet.t ->
    ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t * ('a, 'b) t
  val image : ('a, 'b) t -> 'a Index.t -> 'b IndexSet.t
  val to_list : ('a, 'b) t -> ('a, 'b) piece list
  val map : (('a1, 'b1) piece -> ('a2, 'b2) piece) -> ('a1, 'b1) t -> ('a2, 'b2) t
  val from : 'a IndexSet.t -> ('a index -> 'b IndexSet.t) -> ('a, 'b) t
end

module Increasing_ref : sig
  type ('a, 'b) piece = ('a IndexSet.t * 'b IndexSet.t)
  type ('a, 'b) t

  val equal : ('a, 'b) t -> ('a, 'b) t -> bool
  val compare : ('a, 'b) t -> ('a, 'b) t -> int

  val minimum : ('a, 'b) t
  val is_minimum : ('a, 'b) t -> bool
  val piece : 'a IndexSet.t -> 'b IndexSet.t -> ('a, 'b) t
  val piecewise : ('a IndexSet.t * 'b IndexSet.t) list -> ('a, 'b) t
  val increase :
    ?ignore:'b IndexSet.t ->
    ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t * ('a, 'b) t
  val image : ('a, 'b) t -> 'a Index.t -> 'b IndexSet.t
  val to_list : ('a, 'b) t -> ('a, 'b) piece list
  val from : 'a IndexSet.t -> ('a index -> 'b IndexSet.t) -> ('a, 'b) t
  val filter : ('a, 'b) t -> ('a index -> 'b IndexSet.t -> bool) -> ('a, 'b) t
  val add : ('a, 'b) t -> 'a index -> 'b IndexSet.t -> ('a, 'b) t

  val union : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
  val intersect : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
  val less_than : ('a, 'b) t -> ('a, 'b) t -> bool

  val fold : ('a, 'b) t -> ('a index -> 'b IndexSet.t -> 'c -> 'c) -> 'c -> 'c
  val iter : ('a, 'b) t -> ('a index -> 'b IndexSet.t -> unit) -> unit
  val subtract : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
end

module Decreasing : sig
  type ('a, 'b) piece = ('a IndexSet.t * 'b IndexSet.t)
  type ('a, 'b) t = private ('a, 'b) piece list

  val maximum : ('a, 'b) t
  val is_maximum : ('a, 'b) t -> bool
  val piece : 'a IndexSet.t -> 'b IndexSet.t -> ('a, 'b) t
  val piecewise : ('a IndexSet.t * 'b IndexSet.t) list -> ('a, 'b) t
  val decrease : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t * ('a, 'b) t
  val image : ('a, 'b) t -> 'a Index.t -> 'b IndexSet.t option
  val to_list : ('a, 'b) t -> ('a, 'b) piece list
  val map : (('a1, 'b1) piece -> ('a2, 'b2) piece) -> ('a1, 'b1) t -> ('a2, 'b2) t
end
