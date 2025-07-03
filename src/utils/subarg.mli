type spec = (Arg.key * Arg.spec * Arg.doc)

type command = {
  name : string;
  help : Arg.usage_msg;
  specs : spec list;
  enter : unit -> unit;
  anon : (string -> unit) option;
  commit : unit -> unit;
  abort : unit -> unit;
}

val command :
  string ->
  Arg.usage_msg ->
  ?enter:(unit -> unit) ->
  ?anon:Arg.anon_fun ->
  ?commit:(unit -> unit) ->
  ?abort:(unit -> unit) ->
  spec list -> command

val usage_string : spec list -> command list -> Arg.usage_msg -> string

val usage : spec list -> command list -> Arg.usage_msg -> unit

val parse_argv : ?current:int ref -> string array -> spec list -> command list ->
  Arg.anon_fun -> Arg.usage_msg -> unit

val parse : spec list -> command list -> Arg.anon_fun -> Arg.usage_msg -> unit
