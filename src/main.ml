open Utils
(*open Misc*)

(*let usage = "\
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
  lrgrep -v -g grammar.cmly -s error.mlyl compile -o error.ml
  lrgrep -g grammar.cmly -s error.mlyl interpret -i \"invalid sentence\"
  "*)

(*

  recover    Generate an error-resilient parser.
    -o <file.ml>            Output file for the total parser source code.

  complete   Generate code for completing an input prefix.
    -o <file.ml>            Output file for the completion source code.

*)

let opt_spec_name = ref None
let opt_output_name = ref None
let opt_grammar_file = ref None
let opt_dump_dot = ref false

let print_version_num () =
  print_endline "0.2";
  exit 0

let print_version_string () =
  print_string "LRGrep: toolkit for Menhir parsers, version ";
  print_version_num ()

let global_specs = [
  "-g", Arg.String (fun x -> opt_grammar_file := Some x),
  " <file.cmly>  Path to the compiled Menhir grammar to analyse (*.cmly)";
  "-s", Arg.String (fun x -> opt_spec_name := Some x),
  " <file.mlyl>  Path to the error specification to process (*.mlyl)";
  "-v", Arg.Unit (fun () -> incr Misc.verbosity_level),
  " Increase output verbosity (for debugging/profiling)";
  "-version", Arg.Unit print_version_string,
  " Print version and exit";
  "-vnum", Arg.Unit print_version_num,
  " Print version number and exit";
  "-dump-dot", Arg.Set opt_dump_dot,
  " debug: Dump internal automata to Graphviz .dot files";
]

let commands =
  let not_implemented command () =
    prerr_endline (command ^ " not implemented.");
    exit 1
  in
  Subarg.[
    command "compile" "Translate a specification to an OCaml module" [
      "-o", Arg.String (fun x -> opt_output_name := Some x),
      " <file.ml>  Set output file name to <file> (defaults to <source>.ml)";
    ];
    command "interpret" "Parse a sentence and suggest patterns that can match it" []
      ~commit:(not_implemented "interpret");
    command "cover" "Generate sentences to cover possible failures" []
      ~commit:(not_implemented "cover");
    command "recover" "Generate an error-resilient parser for the grammar" []
      ~commit:(not_implemented "recover");
    command "complete" "Generate an OCaml module that produces syntactic completion for the grammar" []
      ~commit:(not_implemented "complete");
]

let () = Subarg.parse global_specs commands
    ~default:(fun () ->
        prerr_endline "running LRgrep without a sub-command is deprecated, add `compile' to the command-line";
        "compile"
      )
    (fun _ -> failwith "FIXME")
    "lrgrep <global options> [ command <command options ]..."

(*let rec parse_commands acc command args =
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
    | (Short 'v' | Long "verbose") :: rest ->
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
*)

(* Load and pre-process grammar *)

(*let grammar_filename = match !opt_grammar_file with
  | Some file -> file
  | None ->
    usage_error "No parser has been specified.\
                 Expecting an argument of the form <parser.cmly>.\n"

(*module Grammar =
  MenhirSdk.Cmly_read.Read(struct let filename = grammar_filename end)*)

let () = stopwatch 1 "Loaded file %s" grammar_filename

open Kernel.Info
include Lift(Grammar)

let () = stopwatch 1 "Imported grammar (%d terminals, %d non-terminals, %d lr0 states, %d lr1 states)"
    Grammar.Terminal.count
    Grammar.Nonterminal.count
    Grammar.Lr0.count
    Grammar.Lr1.count

(* Load and parse specification, if any *)

(*let spec =
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
  let compute_reachability () =
    let reachability = Kernel.Reachability.make grammar in
    stopwatch 1 "Done with reachability";
    (*Kernel.Lrc.check_deterministic grammar reachability;*)
    (*let lrc = Kernel.Lrc.make grammar reachability in*)
    (*let lrc' = Kernel.Lrc.make_deterministic grammar reachability in*)
    let lrc = Kernel.Lrc.make_minimal grammar reachability in
    (*Kernel.Lrc.check_equivalence grammar lrc lrc' lrc.all_leaf lrc'.all_leaf;*)
    (reachability, lrc)

  let d = match Sys.getenv_opt "SINGLE" with
    | None ->
      let d = Domain.spawn compute_reachability in
      lazy (Domain.join d)
    | Some _ -> Lazy.from_fun compute_reachability

  let viable = Kernel.Viable_reductions.make grammar
  let () = stopwatch 1 "Done with viable reductions"
  let indices = Kernel.Transl.Indices.make grammar
  let () = stopwatch 1 "Indexed items and symbols for translation"
  let trie = Kernel.Transl.Reductum_trie.make viable
  let () = stopwatch 1 "Indexed reductions for translation"
  let reachability, lrc = Lazy.force d
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
      let lrcs = IndexSet.bind lr1s (Vector.get T.lrc.lrcs_of) in
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
      label = Vector.get T.lrc.lr1_of;
    } in
    let Kernel.Spec.Rule (clauses, branches) =
      Kernel.Spec.import_rule grammar T.viable T.indices T.trie rule
    in
    let nfa = Kernel.Automata.NFA.from_branches grammar T.viable branches in
    Vector.iteri (fun br nfa ->
        if !dump_dot then
          with_output_file "%s_%s_br_%d_line_%d.dot" parser_name rule.name
            (br : _ index :> int) branches.expr.:(br).position.pos_lnum
            (Kernel.Automata.NFA.dump grammar nfa);
      ) nfa;
    stopwatch 1 "constructed NFA\n";
    let Kernel.Automata.DFA.T dfa =
      Kernel.Automata.DFA.determinize branches stacks nfa in
    if !dump_dot then
      with_output_file "%s_%s_dfa.dot" parser_name rule.name
        (Kernel.Automata.DFA.dump grammar dfa);
    begin
      let states = Vector.length_as_int dfa.states in
      let branches = ref 0 in
      Vector.iter (fun (Kernel.Automata.DFA.Packed st) ->
          branches := !branches + Vector.length_as_int st.branches
        ) dfa.states;
      stopwatch 1 "determinization (%d states, %d branches, average %.02f branch/state)\n"
        states !branches (float !branches /. float states);
    end;
    let dataflow = Kernel.Automata.Dataflow.make branches dfa in
    stopwatch 1 "dataflow analysis\n";
    if !dump_dot then
      with_output_file "%s_%s_dataflow.dot" parser_name rule.name
        (Kernel.Automata.Dataflow.dump grammar dfa dataflow);
    let Kernel.Automata.Machine.T machine =
      Kernel.Automata.Machine.minimize branches dfa dataflow in
    stopwatch 1 "machine minimization\n";
    if !dump_dot then
      with_output_file "%s_%s_machine.dot" parser_name rule.name
        (Kernel.Automata.Machine.dump grammar machine);
    Kernel.Codegen.output_rule grammar spec rule clauses branches machine cp;
    stopwatch 1 "table & code generation\n"
  end spec.lexer_definition.rules;
  Kernel.Codegen.output_trailer grammar spec cp

let process_command = function
  | Compile options ->
    let (input_file, lexer_definition) = match spec with
      | Some i -> i
      | None ->
        usage_error "compile: expecting a specification (-s <spec.mlyl>)"
    in
    Printf.eprintf "# Processing %s\n" (try Unix.realpath input_file with _ -> input_file);
    let output_file = match options.compile_output with
      | Some o -> o
      | None -> Filename.remove_extension input_file ^ ".ml"
    in
    let oc = open_out_bin output_file in
    let cp = Code_printer.create ~filename:output_file (output_string oc) in
    do_compile {parser_name; lexer_definition} (Some cp);
    close_out oc
  | _ -> failwith "TODO"*)
*)
