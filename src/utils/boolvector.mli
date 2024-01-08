open Fix.Indexing

type 'n t

val make : 'n cardinal -> bool -> 'n t
val test : 'n t -> 'n index -> bool
val set : 'n t -> 'n index -> unit
val clear : 'n t -> 'n index -> unit
