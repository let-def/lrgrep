(* MIT License:
 *
 * Copyright (c) 2025 Frédéric Bour
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 *
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *)

open Utils
open Misc
open Fix.Indexing
open Kernel
open Info

let (!!) = Lazy.force

module Interpreter = Interpreter
module Message_file = Message_file

type 'a with_position = 'a * Lexing.position * Lexing.position

module type CUSTOM_GRAMMAR = sig
  type g
  val grammar : g grammar
  val string_of_terminal : (g terminal index -> string) option
  val string_of_sentence : (g terminal index list -> string) option
  val sentence_lexer : (in_channel -> string with_position option *
                                      g terminal index with_position list) option
end

module Lazy_grammar (Load : functor() -> CUSTOM_GRAMMAR) : sig
  type g
  module Force() : CUSTOM_GRAMMAR with type g = g
end = struct
  type g

  let loaded = lazy (
    let m : (module CUSTOM_GRAMMAR) = (module Load()) in
    let m' : (module CUSTOM_GRAMMAR with type g = g) = Obj.magic m in
    m'
  )

  module Force() = (val !!loaded)
end

module Make(Language : sig
    val name : string
    val grammar : (module CUSTOM_GRAMMAR) option
    val parser_name : string
  end)() =
struct
  let command_name =
    match Language.name with
    | "" -> "lrgrep"
    | name -> "lrgrep for " ^ name

  let binary_name = Sys.argv.(0)

  let usage_prompt =
    "Usage: " ^ binary_name ^ " <global-options> [ command <command-options> ]..."

  (*let usage_examples = Printf.sprintf "\
Examples:
  %s -v -g grammar.cmly -s error.lrgrep compile -o error.ml
  %s -g grammar.cmly -s error.lrgrep interpret -i \"invalid sentence\"
    " binary_name binary_name*)

  let opt_terminal_printer = ref `Builtin

  let opt_report_format = ref `Text

  let badf fmt =
    Printf.ksprintf (fun msg -> raise (Arg.Bad msg)) fmt

  let usage_error_fun : (string -> unit) ref = ref failwith

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
    (* dune subst my substitute this string before release *)
    let version = "%%VERSION%%" in
    (* check if it has been substituted *)
    let version = if version.[0] = '%' then "(dev)" else version in
    print_endline version;
    exit 0

  let print_version_string () =
    print_string "LRGrep toolkit for Menhir parsers";
    if Language.name <> "" then (
      print_string ", specialized for ";
      print_string Language.name;
    );
    print_string ", version ";
    print_version_num ()

  let global_specs =
    cons_if (Option.is_none Language.grammar)
      ("-g", Arg.String set_grammar_file,
       " <file.cmly>  Path to the compiled Menhir grammar to analyse")
    @@ [
        "-s", Arg.String set_spec_file,
        " <file.lrgrep>  Path to the error specification to process";
        "-v", Arg.Unit (fun () -> incr Misc.verbosity_level),
        " Increase output verbosity (for debugging/profiling)";
        "--version", Arg.Unit print_version_string,
        " Print version and exit";
        "--vnum", Arg.Unit print_version_num,
        " Print version number and exit";
        "--dump-dot", Arg.Set opt_dump_dot,
        " debug: Dump internal automata to Graphviz .dot files";
        "--terminal-display", Arg.String (function
            | "name" -> opt_terminal_printer := `Name
            | "alias" -> opt_terminal_printer := `Alias
            | "builtin" -> opt_terminal_printer := `Builtin
            | other ->
              usage_error
                "--terminal-display: unexpected argument %s\n\
                 Accepted values are:\n\
                 - name: print terminal names\n\
                 - alias: prefer string aliases when available\n\
                 - builtin: use builtin printer if available for the language\n"
                other
          ),
        "<name|alias|builtin> When reporting a terminal, display it by its name or its alias";
        "--report-format", Arg.String (function
            | "text" -> opt_report_format := `Text
            | "json" -> opt_report_format := `Json
            | other ->
              usage_error
                "--report-format: unexpected argument %s\n\
                 Accepted values are:\n\
                 - text: human-readable report\n\
                 - json: json-object easy to post-process\n"
                other
          ),
        "<text|json> Report serialization format";
      ]

  (* Lazily load grammar and specification *)

  include Lazy_grammar(functor () ->
        (val
          match Language.grammar with
          | None ->
            let path = get_grammar_file () in
            begin try
                let module G = MenhirSdk.Cmly_read.Read(struct let filename = path end) in
                stopwatch 1 "Loaded grammar from disk (%d terminals, %d non-terminals, %d lr0 states, %d lr1 states)"
                  G.Terminal.count G.Nonterminal.count
                  G.Lr0.count G.Lr1.count;
                let module I = Load_grammar(G) in
                stopwatch 1 "Pre-processed grammar definition";
                (module struct
                  include I
                  let string_of_terminal = None
                  let string_of_sentence = None
                  let sentence_lexer = None
                end : CUSTOM_GRAMMAR)
              with exn ->
                fatal_error ~exn "cannot load grammar file %S" path
            end
          | Some g -> g
        )
      )

  let grammar =
    lazy (let module G = Force() in G.grammar)

  let builtin_print_terminal =
    lazy (let module G = Force() in
          match G.string_of_terminal with
          | Some f -> f
          | None -> Terminal.to_string G.grammar)

  let builtin_print_sentence =
    lazy (let module G = Force() in G.string_of_sentence)

  let builtin_sentence_lexer =
    lazy (let module G = Force() in G.sentence_lexer)

  let parser_name =
    match Language.grammar with
    | None ->
      lazy (
        String.capitalize_ascii
          (Filename.remove_extension
             (Filename.basename (get_grammar_file ())))
      )
    | Some _ -> lazy Language.parser_name

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

  let red_closure = lazy (Redgraph.close_lr1_reductions !!grammar)

  let red_table = lazy (Redpos.make !!grammar)

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

  let opt_compile_output = ref None
  let opt_compile_cover_report = ref ""
  let opt_compile_cover_error = ref false

  let compiler_cover_output = lazy (
    if !opt_compile_cover_report = "" then
      None
    else
      try Some (open_out_bin !opt_compile_cover_report)
      with exn ->
        Syntax.error Lexing.dummy_pos
          "cannot open %S for reporting coverage: %S"
          !opt_compile_cover_report
          (Printexc.to_string exn)
  )

  let print_terminal g t =
    match !opt_terminal_printer with
    | `Name -> Terminal.to_string g t
    | `Alias -> begin match Terminal.alias g t with
        | None ->  Terminal.to_string g t
        | Some alias -> alias
      end
    | `Builtin -> !!builtin_print_terminal t

  let print_sentence = lazy (
    match !!builtin_print_sentence with
    | Some f -> f
    | None -> string_concat_map " " (print_terminal !!grammar)
  )

  type 'g report_sample = {
    stack: 'g lr1 index list;
    sentence: g terminal index list;
    failing: 'g terminal indexset;
    patterns: 'g lr0 indexset;
  }

  let prepare_sample grammar (stacks : _ Automata.stacks)
      ~some_prefix ~get_lrc ~reached ~all (tactic, failing)
    =
    let suffix = List.rev_map get_lrc tactic in
    let _, prefix = some_prefix (List.hd suffix) in
    let stack =
      list_rev_mappend stacks.label prefix @@
      List.map stacks.label suffix
    in
    let sentence = Sentence_generation.sentence_of_stack grammar !!reachability stack in
    let patterns =
      if all then
        List.fold_left (fun acc n -> IndexSet.union (reached n) acc) IndexSet.empty tactic
      else
        IndexSet.empty
    in
    {stack; sentence; failing; patterns}

  let report_text_sample oc grammar main i {stack; sentence; failing; patterns} =
    Printf.fprintf oc "### Sample %d\n\n" i;
    Printf.fprintf oc "Sentence:\n```\n%s\n```\n" (!!print_sentence sentence);
    Printf.fprintf oc "Stack:\n```\n%s\n```\n"
      (string_concat_map " " (Lr1.symbol_to_string grammar) stack);
    Printf.fprintf oc "Rejected when looking ahead at any of the terminals in:\n\
                       ```\n%s\n```\n"
      (String.concat " " (List.rev_map (print_terminal grammar) (IndexSet.elements failing)));
    let patterns = IndexSet.remove main patterns in
    if IndexSet.is_not_empty patterns then begin
      Printf.fprintf oc "Also covered by the following patterns:\n```\n";
      IndexSet.iter begin fun lr0 ->
        List.iter (Printf.fprintf oc "%s\n") (Coverage.print_pattern grammar lr0)
      end patterns;
      Printf.fprintf oc "```\n";
    end;
    Printf.fprintf oc "\n"

  let report_text oc grammar header cases =
    begin match header with
      | `Rule name -> Printf.fprintf oc "# Rule %s\n" name
      | `Enum_maximal -> Printf.fprintf oc "# Maximal patterns\n\n"
      | `Enum_other -> Printf.fprintf oc "# Other reduce-filter patterns\n\n";
    end;
    Seq.iteri begin fun i (lr0, samples) ->
      Printf.fprintf oc "## Pattern %d\n\n```\n" i;
      List.iter (Printf.fprintf oc "%s\n") (Coverage.print_pattern grammar lr0);
      Printf.fprintf oc "```\n\n";
      Seq.iteri (report_text_sample oc grammar lr0) samples
    end cases

  let escape_json_string oc string =
    escape_json_string (output_substring oc) string

  let print_json_string oc string =
    output_char oc '"';
    escape_json_string oc string;
    output_char oc '"'

  let print_comma_sep () =
    let first = ref true in
    fun oc ->
      if !first then
        first := false
      else
        output_char oc ','

  let report_json_sample oc grammar acc i {stack; sentence; failing; patterns} =
    if i > 0 then
      output_char oc ',';
    output_string oc "{\"sentence\":";
    print_json_string oc (!!print_sentence sentence);
    output_string oc ",\"stack\":\"";
    List.iteri begin fun i sym ->
      if i > 0 then output_char oc ' ';
      escape_json_string oc (Lr1.symbol_to_string grammar sym);
    end stack;
    output_string oc "\",\"failing\":[";
    let print_comma = print_comma_sep () in
    IndexSet.rev_iter begin fun term ->
      print_comma oc;
      print_json_string oc (print_terminal grammar term)
    end failing;
    if IndexSet.is_not_empty patterns then begin
      acc := IndexSet.union patterns !acc;
      output_string oc "],\"patterns\":[";
      let print_comma = print_comma_sep () in
      IndexSet.iter begin fun lr0 ->
        print_comma oc;
        Printf.fprintf oc "\"%d\"" (Index.to_int lr0);
      end patterns;
    end;
    output_string oc "]}"

  let report_json_patterns oc grammar patterns =
    output_string oc "{";
    let print_comma = print_comma_sep () in
    IndexSet.iter begin fun lr0 ->
      print_comma oc;
      Printf.fprintf oc "\"%d\":\"" (Index.to_int lr0);
      List.iteri begin fun i text ->
        if i > 0 then output_string oc "\\n";
        escape_json_string oc text;
      end (Coverage.print_pattern grammar lr0);
      output_string oc "\""
    end patterns;
    output_string oc "}"

  let report_json oc grammar header cases acc =
    output_string oc "{\"kind\":";
    begin match header with
      | `Rule name ->
        output_string oc "\"coverage\",\"name\":";
        print_json_string oc name;
      | `Enum_maximal ->
        output_string oc "\"enumeration\",\"name\":\"maximal\""
      | `Enum_other ->
        output_string oc "\"enumeration\",\"name\":\"other\""
    end;
    output_string oc ",\"cases\":[";
    Seq.iteri begin fun i (lr0, samples) ->
      if i > 0 then output_char oc ',';
      acc := IndexSet.add lr0 !acc;
      Printf.fprintf oc "{\"pattern\":\"%d\",\"sentences\":[" (Index.to_int lr0);
      Seq.iteri (report_json_sample oc grammar acc) samples;
      output_string oc "]}";
    end cases;
    output_string oc "]}"

  let begin_report oc grammar =
    match !opt_report_format with
    | `Json ->
      output_string oc "{\"reports\":[";
      let print_comma = print_comma_sep () in
      let patterns = ref IndexSet.empty in
      ((fun header cases ->
          print_comma oc;
          report_json oc grammar header cases patterns),
       (fun () ->
          output_string oc "]";
          output_string oc ",\"patterns\":";
          report_json_patterns oc grammar !patterns;
          output_string oc "}"))
    | `Text -> (report_text oc grammar, ignore)

  let do_compile spec (cp : Code_printer.t option) =
    let grammar = !!grammar in
    Codegen.output_header grammar spec cp;
    let cover_report = match compiler_cover_output with
      | lazy None -> None
      | lazy (Some oc) -> Some (begin_report oc grammar)
    in
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
      if fst rule.error then (
        let rcs = !!red_closure in
        let rtable = Redpos.make grammar in
        let open Coverage in
        let Andor.Graph agr = Andor.make grammar rcs stacks rtable in
        let Deter.Graph dgr = Deter.make grammar rcs stacks rtable agr in
        let Cover.Graph cgr = Cover.coverage grammar branches machine stacks agr dgr in
        if not (list_is_empty cgr.sinks) then (
          begin match cover_report with
            | None -> ()
            | (Some (report, _)) ->
              let get_lrc st =
                match Sum.prj stacks.domain (cgr.position st) with
                | L lrc -> lrc
                | R enu -> Deter.get_lrc agr dgr.nodes.:(enu)
              in
              let reached st =
                match Sum.prj stacks.domain (cgr.position st) with
                | L _ -> IndexSet.empty
                | R enu -> Deter.get_lr0 grammar rcs stacks rtable agr dgr enu
              in
              let iter_sinks f =
                List.iter (fun sink -> f sink (fst (entrypoints.some_prefix (get_lrc sink))))
                  cgr.sinks
              in
              let maximals = Extract.compute_maximal_prefixes
                  ~graph:cgr.enum
                  ~iter_sinks
                  ~reached
              in
              let locals, _globals =
                Report.emit_all
                  ~goals:(Lr0.cardinal grammar)
                  ~graph:cgr.enum
                  ~maximals
                  ~globals:[||]
                  ~reached
              in
              let prepare = prepare_sample grammar stacks
                  ~some_prefix:entrypoints.some_prefix
                  ~get_lrc ~reached ~all:false
              in
              report (`Rule rule.name)
                (Seq.map (fun (lr0, sentences) -> (lr0, Seq.map prepare sentences)) locals);
          end;
          stopwatch 1 "coverage report";
          let report =
            if !opt_compile_cover_error
            then Syntax.error
            else Syntax.warn
          in
          report Lexing.dummy_pos
            "rule %s has only partial coverage%s" rule.name
            (if !opt_compile_cover_report <> "" then ""
             else " (use --cover-report <file> to get more information)");
        )
      )
    end spec.lexer_definition.rules;
    begin match cover_report with
      | None -> ()
      | Some (_, end_report) -> end_report ()
    end;
    Codegen.output_trailer grammar spec cp

  let compile_command () =
    let output_file = match !opt_compile_output with
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
    let rcs = !!red_closure in
    let rtbl = !!red_table  in
    let stacks = make_stacks subset ~error_only:true in
    let open Coverage in
    let Andor.Graph agr = Andor.make grammar rcs stacks rtbl in
    let Deter.Graph dgr = Deter.make grammar rcs stacks rtbl agr in
    let graph, maximals = Enum.prepare grammar agr dgr subset.some_prefix in
    let reached = Deter.get_lr0 grammar rcs stacks rtbl agr dgr in
    let globals =
      if !opt_enum_all then
        Extract.compute_global_prefixes
          ~graph ~maximals ~reached
      else [||]
    in
    let locals, globals =
      Report.emit_all
        ~goals:(Lr0.cardinal grammar)
        ~graph ~maximals ~globals ~reached
    in
    let prepare_sample =
      prepare_sample grammar stacks
        ~some_prefix:subset.some_prefix
        ~get_lrc:(fun node -> Deter.get_lrc agr dgr.Deter.nodes.:(node))
        ~reached:(Deter.get_lr0 grammar rcs stacks rtbl agr dgr)
        ~all:!opt_enum_all
    in
    let prepare_samples seq =
      Seq.map (fun (lr0, sentences) -> (lr0, Seq.map prepare_sample sentences))
        seq
    in
    let report, end_report = begin_report stdout grammar in
    report `Enum_maximal (prepare_samples locals);
    if !opt_enum_all then
      report `Enum_other (prepare_samples globals);
    end_report ()

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
              parser (Interpreter.lift_sentence !!grammar sentence)
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
    let grammar = !!grammar in
    let parser = Interpreter.make_parser grammar in
    let config =
      Interpreter.config
        ~print_reduce_filter:!opt_interpret_patterns
        ~print_stack_items:!opt_interpret_items
        ()
    in
    let rec loop () =
      match
        (* Lex one sentence *)
        match !!builtin_sentence_lexer with
        | None ->
          output_string stdout "$ ";
          flush stdout;
          Interpreter.lift_sentence grammar (input_line stdin)
        | Some f ->
          let entrypoint, symbols = f stdin in
          let entrypoint = Option.map (Interpreter.lift_entrypoint grammar) entrypoint in
          {entrypoint; symbols}
      with
      | exception Exit ->
        flush_all ();
        loop ()
      | exception End_of_file -> ()
      | sentence ->
        let entrypoint =
          match sentence.entrypoint with
          | None -> Option.get (IndexSet.minimum (Lr1.entrypoints grammar))
          | Some (ep, _, _) -> ep
        in
        let stack, _final_stack, remainder =
          Interpreter.parse_sentence parser
            (entrypoint, Lexing.dummy_pos, Lexing.dummy_pos)
            (List.to_seq sentence.symbols)
        in
        let remainder = List.map (fun (x, _, _) -> x) (List.of_seq remainder) in
        Interpreter.analyze_stack ~stack ~remainder
          grammar !!red_closure config;
        loop ()
    in
    loop ()

  (* Argument parser *)
  let commands =
    Subarg.[
      command "compile" "Translate a specification to an OCaml module" [
        "-o", Arg.String (fun x -> opt_compile_output := Some x),
        "<file.ml> Set output file name to <file> (defaults to <spec>.ml)";
        "--cover-all", Arg.Set opt_compile_cover_error,
        " Exit with a failure if coverage of errors is not exhaustive";
        "--cover-report", Arg.Set_string opt_compile_cover_report,
        "<report.md> Write a detailed report of uncovered cases (if any)";
      ] ~commit:compile_command;
      command "interpret" "Parse a sentence and suggest patterns that can match it" [
        "--no-patterns", Arg.Clear opt_interpret_patterns, " Do not suggest patterns";
        "--items", Arg.Set opt_interpret_items," Annotate each state with its items";
      ]
        ~commit:interpret_command;
      command "enumerate" "Generate a negative testsuite (sentences that cover possible failures)" [
        "-a", Arg.Set opt_enum_all, " Cover all filter-reduce patterns";
        "-e", Arg.String (push opt_enum_entrypoints),
        "<entrypoint> Enumerate sentences from this entrypoint (multiple allowed)";
      ] ~commit:enumerate_command;
      command "import-messages" "<file.messages> Generate a .lrgrep file from a .messages file" [
        "-o", Arg.Set_string opt_import_output, "<file.lrgrep> Output destination";
        "--shortest", Arg.Set opt_import_shortest, " Shortest matching strategy";
      ] ~anon:set_import_message_file ~commit:import_command;
      command "menhir-to-tree-sitter" "Generate a tree-sitter template implementing a menhir grammar" [
      ] ~commit:(fun () -> Treesitter_gen.import !!grammar !!parser_name);
      (* command "recover" "Generate an error-resilient parser for the grammar" []
           ~commit:(not_implemented "recover");
         command "complete" "Generate an OCaml module that produces syntactic completion for the grammar" []
           ~commit:(not_implemented "complete");
      *)
    ]

  let () =
    usage_error_fun := (fun msg ->
        Subarg.usage global_specs commands
          (command_name ^ ": " ^ msg ^ "\n\n" ^ usage_prompt)
      );
    Subarg.parse global_specs commands
      (fun arg ->
         if Sys.file_exists arg then (
           set_spec_file arg;
           Printf.eprintf "warning: directly passing %S is deprecated, use -s %S\n"
             arg arg
         )
      )
      usage_prompt
      ~no_subcommand:(fun () -> usage_error "expecting at least one command")
end

let run_lrgrep () =
  let open Make(struct
      let name = ""
      let grammar = None
      let parser_name = ""
    end)()
  in
  ()

let run_custom_lrgrep
    (type g)
    ~language_name ~parser_module_name ~(grammar:g grammar)
    ?string_of_terminal
    ?string_of_sentence
    ?sentence_lexer
    ()
  =
  let open Make(struct
      let name = language_name
      let grammar = Some (module struct
        type nonrec g = g
        let grammar = grammar
        let string_of_terminal = string_of_terminal
        let string_of_sentence = string_of_sentence
        let sentence_lexer = sentence_lexer
      end : CUSTOM_GRAMMAR)
      let parser_name = parser_module_name
    end)()
  in
  ()
