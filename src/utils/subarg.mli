(** {1 Subarg}

    This module provides an extension of the standard {!Arg} library with
    support for sub-commands. It allows defining multiple commands, each with
    their own set of options and arguments, and parsing command-line arguments
    to select and execute the appropriate command.

    {b Note.} This module is designed to work with the {!Arg} module from the
    standard library. It extends its functionality to support complex command
    line interfaces with multiple sub-commands.

    @see [Arg] (standard library) for the base argument parsing functionality.
*)

type spec = (Arg.key * Arg.spec * Arg.doc)

type command = {
  name : string;
  (** The name of the command. *)

  help : Arg.usage_msg;
  (** A short help string describing what the command does. *)

  specs : spec list;
  (** The list of options and arguments accepted by this command. *)

  enter : unit -> unit;
  (** A function called when this command is selected. *)

  anon : (string -> unit) option;
  (** A function to handle anonymous (positional) arguments. *)

  commit : unit -> unit;
  (** A function called after successful parsing of the command. *)

  abort : unit -> unit;
  (** A function called when parsing fails. *)
}

val command :
  string ->
  Arg.usage_msg ->
  ?enter:(unit -> unit) ->
  ?anon:Arg.anon_fun ->
  ?commit:(unit -> unit) ->
  ?abort:(unit -> unit) ->
  spec list -> command
(** [command name help ?enter ?anon ?commit ?abort specs] creates a new command
    with the given name and help string. The command accepts the options defined
    in [specs].

    @param name The name of the command.
    @param help A short help string describing what the command does.
    @param enter A function called when this command is selected (default: [ignore]).
    @param anon A function to handle anonymous arguments (default: None).
    @param commit A function called after successful parsing (default: [ignore]).
    @param abort A function called when parsing fails (default: [ignore]).
    @param specs The list of options and arguments accepted by this command.
    @raise Invalid_argument if the default command is not in the command list. *)

val usage_string : spec list -> command list -> Arg.usage_msg -> string
(** [usage_string specs commands errmsg] returns a complete usage string for
    the command set. It includes the global options and the sub-command list.

    @param specs The global options (applies to all commands).
    @param commands The list of available commands.
    @param errmsg The error message to display if parsing fails. *)

val usage : spec list -> command list -> Arg.usage_msg -> unit
(** [usage speclist commands errmsg] prints a complete usage message to
    standard error and exits with code 2.

    @param speclist The global options.
    @param commands The list of available commands.
    @param errmsg The error message to display if parsing fails. *)

val parse_argv :
  ?current:int ref -> string array ->
  spec list -> command list ->
  ?default:string -> ?warn_default:(unit -> unit) ->
  ?no_subcommand:(unit -> unit) ->
  Arg.anon_fun -> Arg.usage_msg -> unit
(** [parse_argv ?current argv specs commands ?default ?warn_default ?no_subcommand
        anon msg] parses command-line arguments and executes the specified
    command.

    The function selects the appropriate command based on the first argument
    (which must be one of the command names), then parses the remaining
    arguments according to that command's specifications.

    @param ?current A reference to the current argument index (default: {!Arg.current}).
    @param argv The command-line arguments array.
    @param specs The global options (applies to all commands).
    @param commands The list of available commands.
    @param ?default The default command name if no command is specified.
    @param ?warn_default A function called when the default command is used.
    @param ?no_subcommand A function called when no sub-command is specified.
    @param anon A function to handle anonymous arguments.
    @param msg The usage message to display on error.
    @raise Bad if parsing fails.
    @raise Help if the [-help] or [--help] option is encountered. *)

val parse :
  spec list -> command list ->
  ?default:string -> ?warn_default:(unit -> unit) ->
  ?no_subcommand:(unit -> unit) ->
  Arg.anon_fun -> Arg.usage_msg -> unit
(** [parse specs commands ?default ?warn_default ?no_subcommand anon usage]
    parses command-line arguments and executes the specified command,
    exiting cleanly on error.

    This is a wrapper around {!parse_argv} that handles exit codes and
    exceptions.

    @param specs The global options.
    @param commands The list of available commands.
    @param ?default The default command name if no command is specified.
    @param ?warn_default A function called when the default command is used.
    @param ?no_subcommand A function called when no sub-command is specified.
    @param anon A function to handle anonymous arguments.
    @param usage The usage message to display on error.
    @raise Exit 2 if parsing fails.
    @raise Exit 0 if [-help] or [--help] is encountered. *)
