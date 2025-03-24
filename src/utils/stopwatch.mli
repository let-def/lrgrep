type t

val create : unit -> t

val reset : t -> unit

val step : t -> ('a, unit, string, unit) format4 -> 'a

val enter : t -> ('a, unit, string, t) format4 -> 'a

val leave : t -> unit

val main : t

(* Configure output *)

(* The function called to print things; defaults to outputting on stderr *)
val output : (string -> unit) ref

(* Verbosity level; 0 for silent, 1 to report only top level steps,
   2 for sub-steps, etc. *)
val verbosity : int ref
