
type mark = { mutable used : bool; }
val new_mark : unit -> mark
val is_unused : mark -> bool

type set
val empty : set
val is_empty : set -> bool
val singleton : mark -> set
val join : set -> set -> set
val mark_used : set -> unit
