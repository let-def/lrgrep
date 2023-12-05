open Fix.Indexing

type 'n t

val make : 'n cardinal -> 'n t
val mark : 'n t -> 'n index -> unit
val marked : 'n t -> 'n IndexSet.t
val clear : 'n t -> unit
val is_empty : 'n t -> bool
