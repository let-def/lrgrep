(* MIT License:

   Copyright (c) 2025 Frédéric Bour

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell

   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
 *)

open Utils
open Misc
open Fix.Indexing
open Kernel

(*
let usage = "
Usage: lrgrep [options] [ [-g] parser.cmly] [ [-s] spec.lrgrep] <commands>...

Global options:
  -g, --grammar <parser.cmly> Grammar/automaton to work on
  -s, --spec    <spec.lrgrep>   Specification to process
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

  recover    Generate an error-resilient parser.
    -o <file.ml>            Output file for the total parser source code.

  complete   Generate code for completing an input prefix.
    -o <file.ml>            Output file for the completion source code.
"
*)

let usage_prompt =
  "Usage: lrgrep <global-options> [ command <command-options> ]..."

let usage_examples = "\
Examples:
  lrgrep -v -g grammar.cmly -s error.lrgrep compile -o error.ml
  lrgrep -g grammar.cmly -s error.lrgrep interpret -i \"invalid sentence\"
  "


let badf fmt =
  Printf.ksprintf (fun msg -> raise (Arg.Bad msg)) fmt

let usage_error_fun = ref failwith

let usage_error fmt =
  Printf.ksprintf !usage_error_fun fmt

let fatal_error ?exn fmt =
  Printf.ksprintf (fun msg ->
      prerr_string "Fatal error: ";
      prerr_string msg;
      begin match exn with
        | None -> ()
        | Some exn ->
          prerr_string " (";
          prerr_string (Printexc.to_string exn);
          prerr_string ")";
      end;
      prerr_newline ();
      exit 1
    ) fmt

let with_output_file fmt =
  Printf.ksprintf (fun str k ->
      let oc = open_out_bin str in
      k oc;
      close_out oc;
    ) fmt

let opt_spec_file = ref None

let set_spec_file name =
  match !opt_spec_file with
  | Some name' ->
    badf "unexpected argument %S: the specification is already set to %S"
      name name'
  | None ->
    opt_spec_file := Some name

let get_spec_file () =
  match !opt_spec_file with
  | None ->
    usage_error "no specification file has been set (pass -s <spec.lrgrep>)";
    exit 1
  | Some fname -> fname

let opt_grammar_file = ref None

let set_grammar_file name =
  match !opt_grammar_file with
  | Some name' ->
    let message = Printf.sprintf
        "unexpected argument %S: the grammar is already set to %S"
        name name'
    in
    raise (Arg.Bad message)
  | None ->
    opt_grammar_file := Some name

let get_grammar_file () =
  match !opt_grammar_file with
  | None ->
    usage_error "grammar file has not been set (pass -g <parser.cmly>)";
    exit 1
  | Some fname -> fname

let opt_dump_dot = ref false

let print_version_num () =
  print_endline "0.2";
  exit 0

let print_version_string () =
  print_string "LRGrep: toolkit for Menhir parsers, version ";
  print_version_num ()

let global_specs = [
  "-g", Arg.String set_grammar_file,
  " <file.cmly>  Path to the compiled Menhir grammar to analyse";
  "-s", Arg.String set_spec_file,
  " <file.lrgrep>  Path to the error specification to process";
  "-v", Arg.Unit (fun () -> incr Misc.verbosity_level),
  " Increase output verbosity (for debugging/profiling)";
  "-version", Arg.Unit print_version_string,
  " Print version and exit";
  "-vnum", Arg.Unit print_version_num,
  " Print version number and exit";
  "-dump-dot", Arg.Set opt_dump_dot,
  " debug: Dump internal automata to Graphviz .dot files";
]

(* Lazily load grammar and specification *)

include Info.Lift()

let grammar : g Info.grammar lazy_t = lazy (
  let path = get_grammar_file () in
  try
    let module G = MenhirSdk.Cmly_read.Read(struct let filename = path end) in
    stopwatch 1 "Loaded grammar from disk (%d terminals, %d non-terminals, %d lr0 states, %d lr1 states)"
      G.Terminal.count G.Nonterminal.count
      G.Lr0.count G.Lr1.count;
    let module I = Load_grammar(G) in
    stopwatch 1 "Pre-processed grammar definition";
    I.grammar
  with exn ->
    fatal_error ~exn "cannot load grammar file %S" path
)

let parser_name = lazy (
  String.capitalize_ascii
    (Filename.remove_extension
       (Filename.basename (get_grammar_file ())))
)

let spec = lazy (
  let print_parse_error_and_exit lexbuf exn =
    let bt = Printexc.get_raw_backtrace () in
    match exn with
    | Front.Parser.Error ->
      let pos = Lexing.lexeme_start_p lexbuf in
      Syntax.error pos "syntax error."
    | Front.Lexer.Error {msg; pos} ->
      Syntax.error pos "%s." msg
    | _ -> Printexc.raise_with_backtrace exn bt
  in
  let parse_spec path =
    match open_in_bin path with
    | exception exn ->
      fatal_error ~exn "cannot load specification file %s" path
    | ic ->
      let state = Front.Lexer.fresh_state () in
      let lexbuf = Lexing.from_channel ~with_positions:true ic in
      let lexbuf = Front.Lexer.prepare_lexbuf state lexbuf in
      Lexing.set_filename lexbuf path;
      let result =
        try Front.Parser.parse_lexer_definition (Front.Lexer.main state) lexbuf
        with exn -> print_parse_error_and_exit lexbuf exn
      in
      result
  in
  let file = get_spec_file () in
  let result = parse_spec file in
  stopwatch 1 "Loaded specification %s" file;
  result
)

(* Various analyses on the automaton *)

let (!!) = Lazy.force

let reachability = lazy (
  let result = Reachability.make !!grammar in
  stopwatch 1 "Solved reachability on the LR(1) automaton";
  result
)

let lrc = lazy (
  let result = Lrc.make_minimal !!grammar !!reachability in
  stopwatch 1 "Computed minimal LRC on the LR(1) automaton";
  result
)

let red_closure = lazy (
  Redgraph.close_lr1_reductions !!grammar
)

let red_trie, red_targets =
  let red_index = lazy (Redgraph.index_targets !!grammar !!red_closure) in
  lazy (fst (Lazy.force red_index)),
  lazy (snd (Lazy.force red_index))

let red_graph = lazy (
  let result = Redgraph.make !!grammar !!red_closure !!red_targets in
  stopwatch 1 "Computed viable reductions";
  result
)

let indices = lazy (
  let result = Transl.Indices.make !!grammar in
  stopwatch 1 "Indexed items and symbols for translation";
  result
)

open Info

let lrc_from_entrypoints =
  let cache = Hashtbl.create 7 in
  fun from_entrypoints ->
    match Hashtbl.find_opt cache from_entrypoints with
    | Some ep -> ep
    | None ->
      let lr1s =
        if IndexSet.is_empty from_entrypoints
        then Lr1.entrypoints !!grammar
        else from_entrypoints
      in
      let lrcs = IndexSet.bind lr1s (Fix.Indexing.Vector.get !!lrc.lrcs_of) in
      let ep = Lrc.from_entrypoints !!grammar !!lrc lrcs in
      Hashtbl.add cache from_entrypoints ep;
      stopwatch 2 "Computed LRC subset reachable from entrypoints";
      ep

let translate_entrypoints symbols =
  let unknown = ref [] in
  let initial_states =
    List.filter_map (fun (sym, pos) ->
        let result = Hashtbl.find_opt (Lr1.entrypoint_table !!grammar) sym in
        if Option.is_none result then
          push unknown (sym, pos);
        result
      ) symbols
    |> IndexSet.of_list
  in
  begin match List.rev !unknown with
    | [] -> ()
    | (_, pos) :: _ as unknowns ->
      let names = String.concat ", " (List.map fst unknowns) in
      let candidates = Hashtbl.to_seq_keys (Lr1.entrypoint_table !!grammar) in
      Syntax.error pos
        "unknown start symbols %s.\n\
         Start symbols of this grammar are:\n\
         %s\n"
        names (String.concat ", " (List.of_seq candidates))
  end;
  initial_states

let make_stacks (subset : _ Lrc.entrypoints) ~error_only =
  let domain = Vector.length !!lrc.lr1_of in
  let tops = if error_only then subset.wait else subset.reachable in
  let prev = Vector.get subset.predecessors in
  let label = Vector.get !!lrc.lr1_of in
  {Automata. domain; tops; prev; label}

(* Compile command *)

let opt_output_name = ref None

let do_compile spec (cp : Code_printer.t option) =
  let grammar = !!grammar in
  Codegen.output_header grammar spec cp;
  List.iter begin fun (rule : Syntax.rule) ->
    let entrypoints = lrc_from_entrypoints (translate_entrypoints rule.startsymbols) in
    let stacks = make_stacks entrypoints ~error_only:(fst rule.error) in
    let Spec.Rule (clauses, branches) =
      Spec.import_rule grammar !!red_graph !!indices !!red_trie rule
    in
    let nfa = Automata.NFA.from_branches grammar !!red_graph branches in
    Vector.iteri (fun br nfa ->
        if !opt_dump_dot then
          with_output_file "%s_%s_br_%d_line_%d.dot" !!parser_name rule.name
            (br : _ index :> int) branches.expr.:(br).position.pos_lnum
            (Automata.NFA.dump grammar nfa);
      ) nfa;
    stopwatch 1 "constructed NFA";
    let Automata.DFA.T dfa =
      Automata.DFA.determinize grammar branches stacks nfa in
    if !opt_dump_dot then
      with_output_file "%s_%s_dfa.dot" !!parser_name rule.name
        (Automata.DFA.dump grammar dfa !!red_graph);
    begin
      let states = Vector.length_as_int dfa.states in
      let branches = ref 0 in
      Vector.iter (fun (Automata.DFA.Packed st) ->
          branches := !branches + Vector.length_as_int st.branches
        ) dfa.states;
      stopwatch 1 "determinization (%d states, %d branches, average %.02f branch/state)"
        states !branches (float !branches /. float states);
    end;
    let dataflow = Automata.Dataflow.make branches dfa in
    stopwatch 1 "dataflow analysis";
    if !opt_dump_dot then
      with_output_file "%s_%s_dataflow.dot" !!parser_name rule.name
        (Automata.Dataflow.dump grammar dfa dataflow);
    let Automata.Machine.T machine =
      Automata.Machine.minimize branches dfa dataflow in
    stopwatch 1 "machine minimization";
    if !opt_dump_dot then
      with_output_file "%s_%s_machine.dot" !!parser_name rule.name
        (Automata.Machine.dump grammar machine);
    Codegen.output_rule grammar spec rule clauses branches machine cp;
    stopwatch 1 "table & code generation";
    Option.iter begin fun initial ->
      let cposition = Coverage.make_positions grammar in
      let coverage =
        Coverage.coverage
          grammar branches machine stacks
          !!red_closure cposition initial
      in
      Coverage.report_coverage grammar !!red_closure stacks cposition
        !!reachability coverage
        ~get_prefix:entrypoints.some_prefix
    end machine.initial;
    stopwatch 1 "coverage check";
  end spec.lexer_definition.rules;
  Codegen.output_trailer grammar spec cp

let compile_command () =
  let output_file = match !opt_output_name with
    | Some o -> o
    | None -> Filename.remove_extension (get_spec_file ()) ^ ".ml"
  in
  match open_out_bin output_file with
  | exception exn ->
    fatal_error ~exn "cannot open output file %S" output_file
  | oc ->
    let cp = Code_printer.create ~filename:output_file (output_string oc) in
    do_compile {parser_name = !!parser_name; lexer_definition= !!spec} (Some cp);
    close_out oc

(* Enumeration command *)

let opt_enum_all = ref false
let opt_enum_entrypoints = ref []

let enumerate_command () =
  let grammar = !!grammar in
  let initial_states =
    !opt_enum_entrypoints
    |> List.rev_map (fun name -> name, Lexing.dummy_pos)
    |> translate_entrypoints
  in
  let subset = lrc_from_entrypoints initial_states in
  let stacks = make_stacks subset ~error_only:true in
  let regular_terminals = Terminal.regular grammar in
  let initial_enum =
    IndexSet.rev_map_elements stacks.tops
      (fun lrc -> Enumeration.kernel lrc regular_terminals, lrc)
  in
  let Enumeration.Graph graph =
    Enumeration.make_graph
      grammar
      !!red_closure
      stacks
      initial_enum
  in
  let cases = ref 0 in
  let report_sentences sentences =
    let by_lr0 = Vector.make (Lr0.cardinal grammar) [] in
    Seq.iter begin fun sentence ->
      let lr0 =
        Enumeration.get_lr0_state grammar stacks
          graph.ker.:(sentence.Enumeration.first)
      in
      by_lr0.@(lr0) <- List.cons sentence
    end sentences;
    let output_pattern lr0 =
      let lhs =
        match Lr0.incoming grammar lr0 with
        | Some sym when Symbol.is_nonterminal grammar sym ->
          Symbol.name grammar sym ^ " "
        | _ -> ""
      in
      let pad = String.make (String.length lhs + 1) ' ' in
      let first = ref true in
      let items = Lr0.items grammar lr0 in
      if IndexSet.is_empty items then
        Printf.printf "[]\n"
      else (
        IndexSet.iter begin fun item ->
          Printf.printf "%c%s/%s"
            (if !first then '[' else '\n')
            (if !first then lhs else pad)
            (Item.to_string grammar item);
          first := false
        end items;
        Printf.printf "]\n";
      )
    in
    Vector.iteri begin fun lr0 -> function
      | [] -> ()
      | sentences ->
        incr cases;
        Printf.printf
          "## Pattern %d\n\
           \n\
           ```\n" !cases;
        output_pattern lr0;
        Printf.printf "```\n\n";
        List.iteri begin fun i (sentence : _ Enumeration.failing_sentence) ->
          Printf.printf
            "### Sample %d\n\
             \n"
            (i + 1);
          let lrc = graph.ker.:(sentence.first).lrc in
          let suffix =
            List.fold_right
              (fun edge acc -> edge.Enumeration.path @ acc)
              sentence.edges
              [sentence.entry]
          in
          let lrcs = List.rev_append (subset.some_prefix lrc) suffix in
          let lr1s = List.map stacks.label lrcs in
          let terms = Sentence_generation.sentence_of_stack grammar !!reachability lr1s in
          Printf.printf
            "Sentence:\n\
             ```\n\
             %s\n\
             ```\n"
            (string_concat_map " " (Terminal.to_string grammar) terms);
          Printf.printf
            "Stack:\n\
             ```\n\
             %s\n\
             ```\n"
            (string_concat_map " " (Lr1.symbol_to_string grammar) lr1s);
          Printf.printf
            "Rejected when looking ahead at:\n\
             ```\n\
             %s\n\
             ```\n\
             \n"
            (String.concat " "
               (List.rev_map (Terminal.to_string grammar)
                  (IndexSet.elements
                     (IndexSet.inter regular_terminals sentence.failing))));
          if not (list_is_empty sentence.edges) then (
            Printf.printf
              "Also covered by these intermediate patterns:\n\
               ```\n";
            List.iter (fun edge ->
                let node = edge.Enumeration.source in
                let lr0 = Enumeration.get_lr0_state grammar stacks graph.ker.:(node) in
                output_pattern lr0
              ) sentence.edges;
            Printf.printf "```\n\n"
          )
        end sentences
    end by_lr0;
  in
  let sentences = Enumeration.cover_with_maximal_patterns
      grammar !!red_closure stacks graph
  in
  Printf.printf "# Maximal patterns\n\n";
  report_sentences (List.to_seq sentences);
  if !opt_enum_all then
    let cover = Enumeration.cover_all grammar !!red_closure stacks graph in
    List.iter (Enumeration.mark_sentence_covered grammar stacks graph cover) sentences;
    Printf.printf "# Exhaustive coverage\n\n";
    report_sentences (Enumeration.to_seq cover)

(* Command import *)

let opt_import_file = ref ""
let opt_import_output = ref ""
let opt_import_shortest = ref false

let set_import_message_file path =
  if !opt_import_file <> "" then
    Printf.eprintf "unexpected argument %S: message file already set to %S\n"
      path !opt_import_file;
  opt_import_file := path

let import_command () =
  if !opt_import_file = "" then (
    prerr_endline "No .message file specified";
    exit 1
  );
  if !opt_import_output = "" then (
    prerr_endline "Use -o <file.lrgrep> to specify the output";
    exit 1
  );
  let ic = open_in_bin !opt_import_file in
  let rec lines () =
    match input_line ic with
    | exception End_of_file -> Seq.Nil
    | line -> Seq.Cons (line, lines)
  in
  let parser = Message_file.parse_sentence !!grammar in
  let blocks =
    lines
    |> Message_file.extract_pre_block
    |> Message_file.extract_block
    |> Seq.map (Message_file.map_block (fun block ->
        let sentences = List.map (Message_file.map_line (fun sentence ->
            parser (Lrgrep_interpreter.lift_sentence !!grammar sentence)
          )) block.Message_file.sentences
        in
        {block with sentences}
      ))
    (* Force any error *)
    |> List.of_seq |> List.to_seq
  in
  close_in ic;
  let oc = open_out_bin !opt_import_output in
  Seq.iter
    (fun line -> output_string oc line; output_char oc '\n')
    (Message_file.blocks_to_file
       !!grammar ~shortest:!opt_import_shortest blocks);
  close_out oc

(* Interpret *)

let opt_interpret_patterns = ref true
let opt_interpret_items = ref false

let interpret_command () =
  let parser = Lrgrep_interpreter.make_parser !!grammar in
  let config =
    Lrgrep_interpreter.config
      ~print_reduce_filter:!opt_interpret_patterns
      ~print_stack_items:!opt_interpret_items
      ()
  in
  let rec loop () =
    output_string stdout "$ ";
    flush stdout;
    match input_line stdin with
    | exception End_of_file -> ()
    | line ->
      match Lrgrep_interpreter.lift_sentence !!grammar line with
      | exception Exit ->
        flush_all ();
        loop ()
      | sentence ->
        let entrypoint =
          match sentence.entrypoint with
          | None -> Option.get (IndexSet.minimum (Lr1.entrypoints !!grammar))
          | Some (ep, _, _) -> ep
        in
        let stack, _final_stack, remainder =
          Lrgrep_interpreter.parse_sentence parser
            (entrypoint, Lexing.dummy_pos, Lexing.dummy_pos)
            (List.to_seq sentence.symbols)
        in
        let remainder = List.map (fun (x, _, _) -> x) (List.of_seq remainder) in
        Lrgrep_interpreter.analyze_stack ~stack ~remainder
          !!grammar !!red_closure config;
        loop ()
  in
  loop ()

(* Argument parser *)
let commands =
  Subarg.[
    command "compile" "Translate a specification to an OCaml module" [
      "-o", Arg.String (fun x -> opt_output_name := Some x),
      "<file.ml> Set output file name to <file> (defaults to <spec>.ml)";
    ] ~commit:compile_command;
    command "interpret" "Parse a sentence and suggest patterns that can match it" [
      "-no-patterns", Arg.Clear opt_interpret_patterns, " Do not suggest patterns";
      "-items", Arg.Set opt_interpret_items," Annotate each state with its items";
    ]
      ~commit:interpret_command;
    (* command "recover" "Generate an error-resilient parser for the grammar" []
         ~commit:(not_implemented "recover");
       command "complete" "Generate an OCaml module that produces syntactic completion for the grammar" []
         ~commit:(not_implemented "complete");
    *)
    command "enumerate" "Generate a negative testsuite (sentences that cover possible failures)" [
      "-a", Arg.Set opt_enum_all, " Cover all filter-reduce patterns";
      "-e", Arg.String (push opt_enum_entrypoints),
      "<entrypoint> Enumerate sentences from this entrypoint (multiple allowed)";
    ] ~commit:enumerate_command;
    command "import-messages" "<file.messages> Generate a .lrgrep file from a .messages file" [
      "-o", Arg.Set_string opt_import_output, "<file.lrgrep> Output destination";
      "--shortest", Arg.Set opt_import_shortest, " Shortest matching strategy";
    ] ~anon:set_import_message_file ~commit:import_command;
]

let () =
  usage_error_fun := (fun msg ->
      Subarg.usage global_specs commands
        ("lrgrep: " ^ msg ^ "\n\n" ^ usage_prompt)
    );
  Subarg.parse global_specs commands
    ~default:"compile"
    ~warn_default:(fun () ->
        prerr_endline "running LRgrep without a sub-command is deprecated, \
                       add `compile' to the command-line")
    (fun arg ->
       if Sys.file_exists arg then (
         set_spec_file arg;
         Printf.eprintf "warning: directly passing %S is deprecated, use -s %S\n"
           arg arg
       )
    )
    usage_prompt
    ~no_subcommand:(fun () -> usage_error "expecting at least one command")
