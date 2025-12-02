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
  lrgrep -v -g grammar.cmly -s error.mlyl compile -o error.ml
  lrgrep -g grammar.cmly -s error.mlyl interpret -i \"invalid sentence\"
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
    usage_error "no specification file has been set (pass -s <spec.mlyl>)";
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
  " <file.mlyl>  Path to the error specification to process";
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

(* Compile command *)

let opt_output_name = ref None

let do_compile spec (cp : Code_printer.t option) =
  let grammar = !!grammar in
  Codegen.output_header grammar spec cp;
  List.iter begin fun (rule : Syntax.rule) ->
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
        Syntax.error pos
          "unknown start symbols %s.\n\
           Start symbols of this grammar are:\n\
           %s\n"
          names candidates
    end;
    let subset = lrc_from_entrypoints initial_states in
    let stacks = {
      Automata.
      tops =
        if fst rule.error
        then subset.wait
        else subset.reachable;
      prev = Vector.get subset.predecessors;
      label = Vector.get !!lrc.lr1_of;
    } in
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
    stopwatch 1 "table & code generation"
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

(* Argument parser *)
let commands =
  let not_implemented command () =
    prerr_endline (command ^ " not implemented.");
    exit 1
  in
  Subarg.[
    command "compile" "Translate a specification to an OCaml module" [
      "-o", Arg.String (fun x -> opt_output_name := Some x),
      " <file.ml>  Set output file name to <file> (defaults to <spec>.ml)";
    ] ~commit:compile_command;
    command "interpret" "Parse a sentence and suggest patterns that can match it" []
      ~commit:(not_implemented "interpret");
    command "cover" "Generate sentences to cover possible failures" []
      ~commit:(not_implemented "cover");
    command "recover" "Generate an error-resilient parser for the grammar" []
      ~commit:(not_implemented "recover");
    command "complete" "Generate an OCaml module that produces syntactic completion for the grammar" []
      ~commit:(not_implemented "complete");
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
(* Load and pre-process grammar *)
