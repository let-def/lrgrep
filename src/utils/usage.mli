
type mark = { mutable used : bool; }
val new_mark : unit -> mark
val is_unused : mark -> bool

type set
val empty : set
val singleton : mark -> set
val join : set -> set -> set
val mark_used : set -> unit
