open Utils
open Misc

let name_string = "The Menhir parser lexer generator :-]"
let version_string = "0.1"

let usage = "\
Usage: lrgrep [global options] <parser.cmly> [error-spec.mlyl] <commands>...

Global options:
  -v, --verbose      Increase verbosity
  -h, --help         Show this help messages
  -V, --version      Display version information and exit

Commands:
  compile    Generate an error classifier from an error specification.
    -o <file>            Output file for the classifier source code.
                         Defaults to <error-spec>.ml

  interpret  Annotate invalid sentences.
    -i <sentence>        Invalid sentence to annotate.
    -f <-|file>          Read invalid sentences from a file.

    A sentence is specified as a list of terminals.
    By default the sentence is parsed using the first entry point of the grammar
    To specify another entrypoint, write its name followed by ':'.

  enumerate  Produce invalid sentences for coverage.
    -o <-|file>          Output file for the invalid sentences.

  recover    Generate an error-resilient parser.
    -o <file.ml>            Output file for the total parser source code.

  complete   Generate code for completing an input prefix.
    -o <file.ml>            Output file for the completion source code.

Examples:
  lrgrep -v grammar.cmly error.mlyl compile -o error.ml
  lrgrep grammar.cmly error.mlyl interpret -i \"invalid sentence\"
"

let version = "LRgrep 1.0\n"

let usage_error fmt =
  Printf.eprintf "lrgrep: ";
  Printf.kfprintf (fun _ ->
      prerr_string usage;
      exit 2
    ) stderr fmt


(* Quick'n'dirty argument parsing *)

type compile_options = {
  compile_output: string option;
  compile_dummy: unit;
}

type command =
  | Compile of compile_options
  | Interpret of unit
  | Enumerate of unit
  | Recover of unit
  | Complete of unit

type arg =
  | Short of char
  | Long   of string
  | Text   of string

let default_compile_options = {
  compile_output = None;
  compile_dummy = ();
}

let is_command = function
  | "compile"   -> Some (Compile default_compile_options)
  | "interpret" -> Some (Interpret ())
  | "enumerate" -> Some (Enumerate ())
  | "recover"   -> Some (Recover ())
  | "complete"  -> Some (Complete ())
  | _ -> None

let arg_is_command = function
  | Text cmd -> is_command cmd
  | _ -> None

let parse_arg x =
  let l = String.length x in
  if l >= 2 && x.[0] = '-' then (
    if x.[1] <> '-' then (
      let acc = ref [] in
      for j = l - 1 downto 1 do
        acc := Short x.[j] :: !acc
      done;
      !acc
    ) else
      [Long (String.sub x 2 (l - 2))]
  ) else
    [Text x]

let arg_to_text = function
  | Short s -> "-" ^ String.make 1 s
  | Long l -> "--" ^ l
  | Text t -> t

let rec parse_compile acc = function
  | (Short 'o' | Long "output" as arg) :: rest ->
    begin match acc.compile_output with
      | None -> ()
      | Some path ->
        usage_error
          "compile: redundant -o, output has already been set to %S" path
    end;
    begin match rest with
      | Text path :: rest ->
        parse_compile {acc with compile_output = Some path} rest
      | _ ->
        usage_error "compile: expecting a filename after %s"
          (arg_to_text arg)
    end;
  | rest -> Compile acc, rest

let parse_interpret () _ = failwith "interpret: TODO"
let parse_enumerate () _ = failwith "enumerate: TODO"
let parse_recover   () _ = failwith "recover: TODO"
let parse_complete  () _ = failwith "complete: TODO"

let rec parse_commands acc command args =
  let command, args = match command with
    | Compile   opts -> parse_compile opts args
    | Interpret opts -> parse_interpret opts args
    | Enumerate opts -> parse_enumerate opts args
    | Recover   opts -> parse_recover opts args
    | Complete  opts -> parse_complete opts args
  in
  let acc = command :: acc in
  match args with
  | [] -> List.rev acc
  | arg :: rest ->
    match arg_is_command arg with
    | None ->
      usage_error "unexpected argument %S\n\
                   Expecting a command (compile, interpret, enumerate, recover, complete).\n"
        (arg_to_text arg)
    | Some command ->
      parse_commands acc command rest

let conf_verbosity = ref 0
let conf_grammar_file = ref None
let conf_spec_file = ref None

let rec parse_global_options = function
  | Short 'v' :: rest ->
    incr conf_verbosity;
    parse_global_options rest

  | (Short 'h' | Long "help") :: _ ->
    print_string usage;
    exit 2

  | (Short 'V' | Long "version") :: _ ->
    print_string version;
    exit 0

  | Short o :: _ ->
    usage_error "invalid option -- %C.\n" o

  | Long f :: _ ->
    usage_error "unrecognized option -- %S.\n" f

  | Text text :: rest ->
    begin match is_command text with
    | None ->
      begin match !conf_grammar_file, !conf_spec_file with
        | None, _ ->
          conf_grammar_file := Some text
        | Some _, None ->
          conf_spec_file := Some text
        | Some grammar, Some spec ->
          usage_error
            "unexpected argument %S\n\
             Grammar: %S.\n\
             Specification: %S.\n\
             Expecting a command \
             (compile, interpret, enumerate, recover, complete).\n"
            text grammar spec
      end;
      parse_global_options rest

    | Some command ->
      parse_commands [] command rest
    end

  | [] ->
    usage_error
      "Expecting a command \
       (compile, interpret, enumerate, recover, complete).\n"

let commands = parse_global_options
    (List.concat_map parse_arg (List.tl (Array.to_list Sys.argv)))

(* Load and pre-process grammar *)

let grammar_filename = match !conf_grammar_file with
  | Some file -> file
  | None ->
    usage_error "No parser has been specified.\
                 Expecting an argument of the form <parser.cmly>.\n"

module Grammar =
  MenhirSdk.Cmly_read.Read(struct let filename = grammar_filename end)

module Info = Kernel.Info.Make(Grammar)

let () = stopwatch 1 "Loaded grammar %s" grammar_filename

(* Load and parse specification, if any *)

let spec =
  let print_parse_error_and_exit lexbuf exn =
    let bt = Printexc.get_raw_backtrace () in
    begin match exn with
      | Front.Parser.Error ->
        let p = Lexing.lexeme_start_p lexbuf in
        Printf.fprintf stderr
          "File \"%s\", line %d, character %d: syntax error.\n"
          p.Lexing.pos_fname p.Lexing.pos_lnum
          (p.Lexing.pos_cnum - p.Lexing.pos_bol)
      | Front.Lexer.Lexical_error {msg; file; line; col} ->
        Printf.fprintf stderr
          "File \"%s\", line %d, character %d: %s.\n"
          file line col msg
      | _ -> Printexc.raise_with_backtrace exn bt
    end;
    exit 3
  in
  let parse_spec source_file =
    let ic = open_in_bin source_file in
    Front.Lexer.ic := Some ic;
    let lexbuf = Lexing.from_channel ~with_positions:true ic in
    Lexing.set_filename lexbuf source_file;
    let result =
      try Front.Parser.lexer_definition Front.Lexer.main lexbuf
      with exn -> print_parse_error_and_exit lexbuf exn
    in
    Front.Lexer.ic := None;
    result
  in
  match !conf_spec_file with
  | None -> None
  | Some file ->
    let result = parse_spec file in
    stopwatch 1 "Loaded specification %s" file;
    Some result

module type Viable = module type of Kernel.Viable_reductions.Make(Info)()

module Viable =
  LazyFunctor
    (struct
      module type T = Viable
      let log = Some "viable reductions"
    end)
    (Kernel.Viable_reductions.Make(Info))

module type Regexp = module type of Kernel.Regexp.Make(Info)(Viable())

module Regexp =
  LazyFunctor
    (struct
      module type T = Regexp
      let log = None
    end)
    (functor () -> Kernel.Regexp.Make(Info)(Viable()))

module X = Viable()

module type Transl = module type of Kernel.Transl.Make(Regexp())

module Transl =
  LazyFunctor
    (struct
      module type T = Transl
      let log = None
    end)
    (functor () -> Kernel.Transl.Make(Regexp()))

module type Reachability =
  module type of Kernel.Reachability.Make(Info)()

module Reachability =
  LazyFunctor
    (struct
      module type T = Reachability
      let log = Some "reachability information"
    end)
    (Kernel.Reachability.Make(Info))

module type Lrc = module type of Kernel.Lrc.Make(Info)(Reachability())

module Lrc =
  LazyFunctor
    (struct
      module type T = Lrc
      let log = Some "LRC" (*refinement of LR(1) states with reachable classes*)
    end)
    (functor () -> Kernel.Lrc.Make(Info)(Reachability()))
    ()

let process_command = function
  | Compile options ->
    let output = match options.compile_output with
      | Some o -> o
      | None ->
        usage_error "compile: no output file has been set"
    in
    let oc = open_out_bin output in
    close_out oc
  | _ -> failwith "TODO"
