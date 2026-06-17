open Fix.Indexing

type ('n, 'b) _t
type 'n t = T : ('n, 'b) _t -> 'n t

val make : 'n cardinal -> 'n t
val link_left : ('n, 'b) _t -> 'n index -> 'n index -> unit
val link_right : ('n, 'b) _t -> 'n index -> 'n index -> unit
val bridge : ('n, 'b) _t -> 'n index -> 'n index -> 'b index

val create_problem : ('n, 'b) _t -> 'b cardinal * 'b Bridge_burner.problem
