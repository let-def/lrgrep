type t

val empty : t
val is_empty : t -> bool
val inter : t -> t -> t

val import : string -> t
val export : t -> string
val source_code : string -> string
