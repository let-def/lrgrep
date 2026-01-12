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
    let stacks =
      make_stacks
        (lrc_from_entrypoints (translate_entrypoints rule.startsymbols))
        ~error_only:(fst rule.error)
    in
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
  let initial_enum =
    let lookahead = Terminal.regular grammar in
    let add lrc = List.cons (Enumeration.kernel lrc lookahead) in
    IndexSet.fold add stacks.tops []
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
        Printf.printf
          "## Pattern %d\n\
           \n\
           ```\n" !cases;
        incr cases;
        output_pattern lr0;
        Printf.printf "```\n\n";
        List.iteri begin fun i sentence ->
          Printf.printf
            "### Sample sentence %d\n\
             \n\
             Here is a sample sentence prefix covered this pattern:\n\
             ```\n" i;
          let lrc = graph.ker.:(sentence.Enumeration.first).lrc in
          let suffix =
            lrc ::
            List.concat_map
              (fun edge -> edge.Enumeration.path)
              sentence.edges
          in
          let prefix = subset.some_prefix lrc in
          let lrcs = List.rev_append prefix suffix in
          List.iter (fun lrc ->
              Printf.printf " %s"
                (Lr1.symbol_to_string grammar (stacks.label lrc))
            ) lrcs;
          Printf.printf
            "\n```\n\
             \n\
             It is rejected when looking ahead at:\n\
             ```\n";
          IndexSet.rev_iter (fun t ->
              Printf.printf " %s" (Terminal.to_string grammar t)
            ) sentence.failing;
          Printf.printf
            "\n```\n\
             \n";
          if not (list_is_empty sentence.edges) then (
            Printf.printf
              "It is also covered by these intermediate patterns:\n\
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
    let sentences =
      Enumeration.cover_all grammar !!red_closure stacks graph
        ~already_covered:sentences
    in
    Printf.printf "# Exhaustive coverage\n\n";
    report_sentences sentences

(* Command import *)

let opt_import_file = ref ""
let opt_import_output = ref ""

let classify_line txt =
  let is_whitespace = function ' ' | '\t' -> true | _ -> false in
  let l = String.length txt in
  let i = ref 0 in
  while !i < l && is_whitespace txt.[!i] do
    incr i
  done;
  if !i = l then
    `Whitespace
  else if txt.[!i] = '#' then (
    if !i + 1 < l && txt.[!i+1] = '#' then
      `Autocomment
    else
      `Comment
  ) else
    `Text

(*let get_comments lines =
  let rec aux comments = function
    | Seq.Nil -> (comments, Seq.Nil)
    | Seq.Cons (line, lines) as seq ->
      match classify_line line with
      | `Autocomment -> aux comments (lines ())
      | `Comment -> aux (line :: comments) (lines ())
      | _ -> comments, seq
  in
  let rev_comments, lines = aux [] (lines ()) in
  (List.rev rev_comments, lines)

let get_text lines =
  let pre_comments, lines = get_comments lines in
  match lines with
  | Seq.Nil -> (pre_comments, None, Seq.Nil)
  | Seq.Cons (line, lines) ->
    match classify_line line with
    | `Autocomment | `Comment -> assert false
    | `Text text ->
    | `Whitespace ws *)

let group_lines lines =
  let texts = ref [] in
  let comments = ref [] in
  let rec aux lines =
    match lines () with
    | Seq.Nil -> Seq.empty
    | Seq.Cons (line, lines) ->
      match classify_line line with
      | `Whitespace ->
        if list_is_empty !texts
        then aux lines
        else lines
      | `Autocomment ->
        aux lines
      | `Comment ->
        push comments (String.sub line 1 (String.length line - 1));
        aux lines
      | `Text ->
        push texts (line, List.rev !comments);
        comments := [];
        aux lines
  in
  let lines = aux lines in
  let rec post_process acc comments = function
    | [] -> (comments, acc, lines)
    | (text, comments') :: rest ->
      post_process ((text, comments) :: acc) comments' rest
  in
  post_process [] (List.rev !comments) !texts

type comment = string

type 'sentence message_block = {
  sentence_pre_comments: comment list;
  sentences: ('sentence * comment list) list;
  message_pre_comments: comment list;
  message: (string * comment list) list;
}

let decompose_sentence sentence =
  let lhs, rhs =
    match String.index_opt sentence ':' with
    | None -> None, sentence
    | Some colon ->
      let lhs = String.trim (String.sub sentence 0 colon) in
      let rhs =
        String.sub sentence
          (colon + 1)
          (String.length sentence - colon - 1)
      in
      (Some lhs, rhs)
  in
  let rhs = List.filter ((<>) "") (String.split_on_char ' ' rhs) in
  (lhs, rhs)

let parse_terminal t =
  match Terminal.find !!grammar t with
  | Result.Ok t -> t
  | Result.Error dym ->
    Printf.eprintf "Unknown terminal %S%a\n" t
      (print_dym (fun (_,s,_) -> s)) dym;
    exit 1

let parse_message message =
  let parse_sentence (text, comments) =
    let lhs, rhs = decompose_sentence text in
    ((lhs, List.map parse_terminal rhs), comments)
  in
  {message with sentences = List.map parse_sentence message.sentences}

let rec group_messages messages lines =
  match group_lines lines with
  | last_comments, [], _lines ->
    (List.rev messages, last_comments)
  | sentence_pre_comments, sentences, lines ->
    let message_pre_comments, message, lines = group_lines lines in
    if list_is_empty message then
      Printf.eprintf "warning: last sentences without message in .messages file\n";
    let message = {
      sentence_pre_comments;
      sentences;
      message_pre_comments;
      message;
    } in
    group_messages (message :: messages) lines

let set_import_message_file path =
  if !opt_import_file <> "" then
    Printf.eprintf "unexpected argument %S: message file already set to %S\n"
      path !opt_import_file;
  opt_import_file := path

let get_action =
  let table : (g lr1 index * g terminal index, _) Hashtbl.t = Hashtbl.create 7 in
  fun state terminal ->
    let key = (state, terminal) in
    match Hashtbl.find_opt table key with
    | Some action -> action
    | None ->
      let grammar = !!grammar in
      let action =
        match
          IndexSet.find
            (fun red -> IndexSet.mem terminal (Reduction.lookaheads grammar red))
            (Reduction.from_lr1 grammar state)
        with
        | red -> `Reduce (Reduction.production grammar red)
        | exception Not_found ->
          let sym = Symbol.inj_t grammar terminal in
          match
            IndexSet.find
              (fun tr -> Index.equal sym (Transition.symbol grammar tr))
              (Transition.successors grammar state)
          with
          | tr -> `Shift (Transition.target grammar tr)
          | exception Not_found ->
            `Reject
      in
      Hashtbl.add table key action;
      action

let parse_sentence (lhs, rhs) =
  let grammar = !!grammar in
  let lhs = match lhs with
    | None -> IndexSet.choose (Lr1.entrypoints grammar)
    | Some lhs -> lhs
  in
  let rec consume_terminal stack (t, startp, endp as token) =
    let (state, _, currp) = List.hd stack in
    match get_action state t with
    | `Reject -> Result.Error stack
    | `Shift state -> Result.Ok ((state, startp, endp) :: stack)
    | `Reduce prod ->
      let (stack, startp', endp') =
        match Production.length grammar prod with
        | 0 -> (stack, currp, currp)
        | n ->
          let (_, _, endp) = List.hd stack in
          let stack = list_drop (n - 1) stack in
          let (_, startp, _) = List.hd stack in
          let stack = List.tl stack in
          (stack, startp, endp)
      in
      let (state, _, _) = List.hd stack in
      let state' = Transition.find_goto_target grammar state (Production.lhs grammar prod) in
      let stack = (state', startp', endp') :: stack in
      consume_terminal stack token
  in
  let rec loop stack ts =
    match ts () with
    | Seq.Nil -> (stack, stack, Seq.empty)
    | Seq.Cons (t, ts') as ts0 ->
      match consume_terminal stack t with
      | Result.Ok stack' -> loop stack' ts'
      | Result.Error stack' -> (stack, stack', fun () -> ts0)
  in
  let _canonical_stack, intermediate_stack, _remainder =
    loop [lhs, Lexing.dummy_pos, Lexing.dummy_pos] (List.to_seq rhs)
  in
  let state, _, _ = List.hd intermediate_stack in
  state

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
  let blocks, _comments = group_messages [] lines in
  close_in ic;
  Printf.eprintf ".messages: parsed %d messages"
    (List.length blocks);
  List.iter begin fun { sentence_pre_comments; sentences; message_pre_comments; message } ->
    Printf.eprintf "{ sentence_pre_comments=[";
    List.iter (Printf.eprintf "\n    %S") sentence_pre_comments;
    Printf.eprintf "];\n\
                   \  sentences=[";
    List.iter (fun (a,_) -> Printf.eprintf "\n    %S" a) sentences;
    Printf.eprintf "];\n\
                   \  message_pre_comments=[";
    List.iter (Printf.eprintf "\n    %S") message_pre_comments;
    Printf.eprintf "];\n\
                   \  message=[";
    List.iter (fun (a,_) -> Printf.eprintf "\n    %S" a) message;
    Printf.eprintf "] }\n";
  end blocks;
  let blocks = List.map parse_message blocks in
  ()

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
    command "cover" "Check that the error specification cover all possible failures" []
      ~commit:(not_implemented "cover");
    command "recover" "Generate an error-resilient parser for the grammar" []
      ~commit:(not_implemented "recover");
    command "complete" "Generate an OCaml module that produces syntactic completion for the grammar" []
      ~commit:(not_implemented "complete");
    command "enumerate" "Generate sentences to cover possible failures" [
      "-a", Arg.Set opt_enum_all, " Cover all filter-reduce patterns";
      "-e", Arg.String (push opt_enum_entrypoints),
      " <entrypoint> Enumerate sentences from this entrypoint (multiple allowed)";
    ] ~commit:enumerate_command;
    command "import-messages" "" [
      "-o", Arg.Set_string opt_import_output, "";
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
