type t

val output : (string -> unit) ref

val create : unit -> t

val reset : t -> unit

val step : t -> ('a, unit, string, unit) format4 -> 'a

val enter : t -> ('a, unit, string, t) format4 -> 'a

val leave : t -> unit

val main : t
