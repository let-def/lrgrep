type t

val empty : t
val is_empty : t -> bool
val inter : t -> t -> t
val mem : int -> t -> bool
val init : int -> (int -> bool) -> t
val import : string -> t
val export : t -> string
val source_code : t -> string
