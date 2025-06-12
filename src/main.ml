open Fix.Indexing
open Utils
open Misc

let name_string = "The Menhir parser lexer generator :-]"
let version_string = "0.1"

let usage = "\
Usage: lrgrep [options] [ [-g] parser.cmly] [ [-s] spec.mlyl] <commands>...

Global options:
  -g, --grammar <parser.cmly> Grammar/automaton to work on
  -s, --spec    <spec.mlyl>   Specification to process
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

  cover      Analyses to cover all errors.
    -o <-|file>          Output file for the invalid sentences.

Examples:
  lrgrep -v grammar.cmly error.mlyl compile -o error.ml
  lrgrep grammar.cmly error.mlyl interpret -i \"invalid sentence\"
"

(*

  recover    Generate an error-resilient parser.
    -o <file.ml>            Output file for the total parser source code.

  complete   Generate code for completing an input prefix.
    -o <file.ml>            Output file for the completion source code.

*)

let version = "LRgrep 1.0\n"

let usage_error fmt =
  prerr_string "lrgrep: ";
  Printf.kfprintf (fun _ -> prerr_newline (); exit 2) stderr fmt


(* Quick'n'dirty argument parsing *)

type compile_options = {
  compile_output: string option;
  compile_dummy: unit;
}

type command =
  | Compile of compile_options
  | Interpret of unit
  | Cover of unit
  | Recover of unit
  | Complete of unit

type arg =
  | Short of char
  | Long  of string
  | Text  of string

let default_compile_options = {
  compile_output = None;
  compile_dummy = ();
}

let is_command = function
  | "compile"   -> Some (Compile default_compile_options)
  | "interpret" -> Some (Interpret ())
  | "cover"     -> Some (Cover ())
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

let conf_grammar_file = ref None

let set_grammar_file text =
  match !conf_grammar_file with
  | None -> conf_grammar_file := Some text
  | Some text' ->
    usage_error "-g %S: grammar has already been set to %S" text text'

let conf_spec_file = ref None

let set_spec_file text =
  match !conf_spec_file with
  | None -> conf_spec_file := Some text
  | Some text' ->
    usage_error "-s %S: spec has already been set to %S" text text'

let parse_common_option = function
  | (Short 'g' | Long "grammar") :: Text file :: rest ->
    set_grammar_file file;
    Some rest

  | (Short 's' | Long "spec") :: Text file :: rest ->
    set_spec_file file;
    Some rest

  | (Short ('s'|'g') | Long ("spec"|"grammar") as arg) :: _ ->
    usage_error "expecting filename after %s" (arg_to_text arg)

  | _ -> None

let rec parse_common_options args =
  match parse_common_option args with
  | Some args' -> parse_common_options args'
  | None -> args

let rec parse_compile acc args =
  match parse_common_options args with
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
let parse_cover () _ = failwith "cover: TODO"
let parse_recover () _ = failwith "recover: TODO"
let parse_complete () _ = failwith "complete: TODO"

let rec parse_commands acc command args =
  let command, args = match command with
    | Compile   opts -> parse_compile opts args
    | Interpret opts -> parse_interpret opts args
    | Cover     opts -> parse_cover opts args
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

let anon_arguments = ref []
let rec parse_global_options args =
  match parse_common_option args with
  | Some rest -> parse_global_options rest
  | None ->
    match args with
    | Short 'v' :: rest ->
      incr Misc.verbosity_level;
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
          push anon_arguments text;
          (*begin ;*)
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

let () = List.iter (fun text ->
    match !conf_grammar_file, !conf_spec_file with
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
  ) (List.rev !anon_arguments)

(* Load and pre-process grammar *)

let grammar_filename = match !conf_grammar_file with
  | Some file -> file
  | None ->
    usage_error "No parser has been specified.\
                 Expecting an argument of the form <parser.cmly>.\n"

module Grammar =
  MenhirSdk.Cmly_read.Read(struct let filename = grammar_filename end)

open Kernel.Info
include Lift(Grammar)

let () = stopwatch 1 "Loaded grammar %s" grammar_filename

(* Load and parse specification, if any *)

let spec =
  let print_parse_error_and_exit lexbuf exn =
    let bt = Printexc.get_raw_backtrace () in
    match exn with
    | Front.Parser.Error ->
      let pos = Lexing.lexeme_start_p lexbuf in
      Kernel.Syntax.error pos "syntax error."
    | Front.Lexer.Lexical_error {msg; pos} ->
      Kernel.Syntax.error pos "%s." msg
    | _ -> Printexc.raise_with_backtrace exn bt
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
    Some (file, result)

let parser_name =
  String.capitalize_ascii
    (Filename.remove_extension
       (Filename.basename grammar_filename))

module T = struct
  let viable = Kernel.Viable_reductions.make grammar
  let indices = Kernel.Transl.Indices.make grammar
  let trie = Kernel.Transl.Reductum_trie.make viable
  let reachability = Kernel.Reachability.make grammar
  let lrc = Kernel.Lrc.make grammar reachability
end

let lrc_from_entrypoints =
  let cache = Hashtbl.create 7 in
  fun from_entrypoints ->
    match Hashtbl.find_opt cache from_entrypoints with
    | Some ep -> ep
    | None ->
      let lr1s =
        if IndexSet.is_empty from_entrypoints
        then Lr1.entrypoints grammar
        else from_entrypoints
      in
      let lrcs = indexset_bind lr1s (Vector.get T.lrc.lrcs_of) in
      let ep = Kernel.Lrc.from_entrypoints grammar T.lrc lrcs in
      Hashtbl.add cache from_entrypoints ep;
      stopwatch 2 "Computed LRC subset reachable from entrypoints";
      ep

let with_output_file fmt =
  Printf.ksprintf (fun str k ->
      let oc = open_out_bin str in
      k oc;
      close_out oc;
    ) fmt

let do_compile spec (cp : Code_printer.t option) =
  Kernel.Codegen.output_header grammar spec cp;
  List.iter begin fun (rule : Kernel.Syntax.rule) ->
    let unknown = ref [] in
    let initial_states =
      List.filter_map (fun (sym, pos) ->
          let result = Hashtbl.find_opt (Lr1.entrypoint_table grammar) sym in
          if Option.is_none result then
            push unknown (sym, pos);
          result
        ) rule.startsymbols
      |> IndexSet.of_list
    in
    begin match List.rev !unknown with
      | [] -> ()
      | (_, pos) :: _ as unknowns ->
        let names = String.concat ", " (List.map fst unknowns) in
        let candidates =
          String.concat ", " (List.of_seq (Hashtbl.to_seq_keys (Lr1.entrypoint_table grammar)))
        in
        Kernel.Syntax.error pos
          "unknown start symbols %s.\n\
           Start symbols of this grammar are:\n\
           %s\n"
          names candidates
    end;
    let subset = lrc_from_entrypoints initial_states in
    let stacks = {
      Kernel.Automata.
      tops =
        if fst rule.error
        then subset.wait
        else subset.reachable;
      prev = Vector.get subset.predecessors;
      label = (fun n -> IndexSet.singleton T.lrc.lr1_of.:(n));
    } in
    let Kernel.Spec.Rule (clauses, branches) =
      Kernel.Spec.import_rule grammar T.viable T.indices T.trie rule
    in
    let nfa = Kernel.Automata.NFA.from_branches grammar T.viable branches in
    Vector.iteri (fun br nfa ->
        with_output_file
          "/tmp/%s_br_%d_line_%d.dot"
          parser_name (br : _ index :> int)
          branches.expr.:(br).position.pos_lnum
          (Kernel.Automata.NFA.dump grammar nfa);
      ) nfa;
    let Kernel.Automata.DFA.T dfa =
      Kernel.Automata.DFA.determinize branches stacks nfa in
    with_output_file "/tmp/%s_dfa.dot" parser_name
      (Kernel.Automata.DFA.dump grammar dfa);
    let dataflow = Kernel.Automata.Dataflow.make branches dfa in
    with_output_file "/tmp/%s_dataflow.dot" parser_name
      (Kernel.Automata.Dataflow.dump grammar dfa dataflow);
    let Kernel.Automata.Machine.T machine =
      Kernel.Automata.Machine.minimize branches dfa dataflow in
    with_output_file "/tmp/%s_machine.dot" parser_name
      (Kernel.Automata.Machine.dump grammar machine);
    Kernel.Codegen.output_rule grammar spec rule clauses branches machine cp
  end spec.lexer_definition.rules;
  Kernel.Codegen.output_trailer grammar spec cp

let process_command = function
  | Compile options ->
    let (input_file, lexer_definition) = match spec with
      | Some i -> i
      | None ->
        usage_error "compile: expecting a specification (-s <spec.mlyl>)"
    in
    let output_file = match options.compile_output with
      | Some o -> o
      | None -> Filename.remove_extension input_file ^ ".ml"
    in
    let oc = open_out_bin output_file in
    let cp = Code_printer.create ~filename:output_file (output_string oc) in
    do_compile {parser_name; lexer_definition} (Some cp);
    close_out oc
  | _ -> failwith "TODO"

let () = List.iter process_command commands
