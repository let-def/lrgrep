open Fix.Indexing
open Utils

type 'b problem
val make : 'b cardinal -> 'b problem
val link_l : 'b problem -> 'b index -> 'b index -> unit
val link_r : 'b problem -> 'b index -> 'b index -> unit
val link_b : 'b problem -> 'b index -> 'b index -> unit
val solve : 'b problem -> 'b IndexSet.t
