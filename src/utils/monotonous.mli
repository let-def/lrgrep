open Fix.Indexing

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
val map : (('a1, 'b1) piece -> ('a2, 'b2) piece list) -> ('a1, 'b1) t -> ('a2, 'b2) t
val from : 'a IndexSet.t -> ('a index -> 'b IndexSet.t) -> ('a, 'b) t
