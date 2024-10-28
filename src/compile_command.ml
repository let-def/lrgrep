open Utils
open Misc

let error {Front.Syntax. line; col} fmt =
  Printf.eprintf "Error line %d, column %d: " line col;
  Printf.kfprintf (fun oc -> output_char oc '\n'; flush oc; exit 1) stderr fmt

(* Command-line parsing. *)

let opt_output_name = ref None
let opt_grammar_file = ref None
let opt_coverage = ref false
let opt_coverage_silent = ref false
let opt_coverage_fatal = ref false
let opt_verbose = ref false
let opt_debug_stack = ref []

let specs = [
  "-o", Arg.String (fun x -> opt_output_name := Some x),
  " <file.ml>  Set output file name to <file> (defaults to <source>.ml)";
  "-g", Arg.String (fun x -> opt_grammar_file := Some x),
  " <file.cmly>  Path to the Menhir compiled grammar to analyse (*.cmly)";
  "-v", Arg.Set opt_verbose,
  " Increase output verbosity";
  "-coverage", Arg.Set opt_coverage,
  " Check error coverage";
  "-coverage-silent", Arg.Set opt_coverage_silent,
  " Check coverage but do not enumerate uncovered cases (suitable for continuous integration)";
  "-coverage-fatal", Arg.Set opt_coverage_fatal,
  " Abort LRGrep if uncovered cases were found";
  "-debug-stack", Arg.String (fun stack ->
      opt_debug_stack :=
        List.map
          (fun state -> int_of_string (String.trim state))
          (String.split_on_char ',' stack)
    ),
  " For debugging purposes. Simulate action of the automaton on a stack,\n\
  \ specified as a comma separated list of state numbers (top comes first)"
]

module Run(P : sig
  val grammar_file : string
  val source_file : string
end)() = struct
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

  let () = Stopwatch.step Stopwatch.main "Beginning"

  module Grammar = MenhirSdk.Cmly_read.Read(struct let filename = P.grammar_file end)

  let () = Stopwatch.step Stopwatch.main "Loaded grammar"

  module Info = Mid.Info.Make(Grammar)
  module Viable = Mid.Viable_reductions.Make(Info)()
  module Regexp = Mid.Regexp.Make(Info)(Viable)
  module Transl = Mid.Transl.Make(Regexp)
  module Reachability = Mid.Reachability.Make(Info)()
  module Lrc = Mid.Lrc.Make(Info)(Reachability)

  module type STACKS = Mid.Automata.STACKS with type lr1 := Info.Lr1.n

  module Lr1_stacks : STACKS with type n = Info.Lr1.n =
  struct
    type n = Info.Lr1.n
    let n = Info.Lr1.n
    let initials = Info.Lr1.idle
    let next = Info.Lr1.predecessors
    let label = IndexSet.singleton
  end

  let translate_entrypoints prj loc err symbols =
    let unhandled = ref [] in
    let result =
      List.filter_map (fun sym ->
          let result = Hashtbl.find_opt Info.Lr1.entrypoints (prj sym) in
          if Option.is_none result then push unhandled sym;
          result
        ) symbols
    in
    match List.rev !unhandled with
    | [] -> IndexSet.of_list result
    | first :: rest as all ->
      Printf.ksprintf (fun msg -> err (loc first) msg)
        "Unknown start symbol%s %s.\nValid start symbols are: %s.\n"
        (if rest = [] then "" else "s")
        (string_concat_map ", " prj all)
        (string_concat_map ", " Fun.id
           (List.sort String.compare
              (Hashtbl.fold (fun key _ acc -> key :: acc) Info.Lr1.entrypoints [])))

  let parser_name =
    String.capitalize_ascii (Filename.basename Grammar.Grammar.basename)

  let process_entry oc (entry : Front.Syntax.entry) = (
    let open Fix.Indexing in
    let entrypoints =
      match entry.startsymbols with
      | [] -> Info.Lr1.all_entrypoints
      | syms ->
        translate_entrypoints fst snd
          (fun loc msg -> error loc "%s" msg)
          syms
    in
    let module Lrc = Mid.Lrc.Close(Info)(Lrc)
        (struct let entrypoints = indexset_bind entrypoints Lrc.lrcs_of_lr1 end)
    in
    let open Mid.Automata.Entry
        (Transl)
        (struct
          include Lrc
          let initials =
            if entry.error then
              Lrc.idle
            else
              IndexSet.init_from_set Lrc.n (fun _ -> true)
          let next = Lrc.predecessors
          let label lrc = IndexSet.singleton (Lrc.lr1_of_lrc lrc)
        end)
        (struct
          let parser_name = parser_name
          let entry = entry
        end)
        ()
    in
    let time = Stopwatch.enter Stopwatch.main "Generating code for entry %s" entry.name in
    Printf.eprintf "Raw DFA states: %d\n" (cardinal BigDFA.n);
    Printf.eprintf "Min DFA states: %d\n" (cardinal MinDFA.states);
    Printf.eprintf "Output DFA states: %d\n" (cardinal OutDFA.states);
    Printf.eprintf "Time spent: %.02fms\n" (Sys.time () *. 1000.);
    if !opt_coverage || !opt_coverage_silent || !opt_coverage_fatal then (
      (*let open Info in*)
      let module Reach = Mid.Reachable_reductions2.Make(Info)(Viable)(Lrc)() in
      let module Failure = Mid.Reachable_reductions2.FailureNFA(Info)(Viable)(Lrc)(Reach)() in
      let module Check = Mid.Reachable_reductions2.Coverage_check(Info)(Lrc)(Failure)(struct
          type n = OutDFA.states
          let n = OutDFA.states
          let initial = OutDFA.initial
          let successors = tabulate_finset n (fun st ->
              IndexSet.fold (fun tr acc ->
                  ((OutDFA.label tr).filter, OutDFA.target tr) :: acc
                ) (OutDFA.outgoing st) []
            )
          let accept = tabulate_finset n (fun st ->
              let m = OutDFA.matching st in
              List.fold_left (fun acc (clause, _, _) ->
                  if Clause.total clause then
                    match Clause.lookaheads clause with
                    | None -> Info.Terminal.all
                    | Some set -> IndexSet.union set acc
                  else
                    acc
                ) IndexSet.empty m
            )
        end)() in
      (*if !Check.Forward.found_uncovered then (
        Printf.eprintf "rule %s: coverage is not exhaustive\n"
          entry.Front.Syntax.name;
        if not !opt_coverage_silent then (
          let module Backward = Check.Backward() in
          Backward.enum_sentence (fun suffix lrc unhandled ->
              let suffix = List.filter_map (fun sym ->
                  match Backward.Sym_or_lr1.prj sym with
                  | L sym -> Some sym
                  | R _ -> None
                ) suffix
              in
              let entry, prefix =
                match List.rev_map Lrc.lr1_of_lrc (lrc :: Lrc.some_prefix lrc) with
                | [] -> assert false
                | entry :: prefix ->
                  entry, prefix
              in
              let prefix = List.filter_map Lr1.incoming prefix in
              Printf.printf "%s %s %s\n"
                (Lr1.to_string entry)
                (string_concat_map " " Symbol.name (prefix @ suffix))
                (string_of_indexset ~index:Terminal.to_string unhandled)
            )
        );
        if !opt_coverage_fatal then
          exit 2
      );*)
      ()
    );
    let get_state_for_compaction index =
      let add_match (clause, priority, regs) =
        let cap = captures clause in
        let registers =
          let add_reg cap acc =
            let reg = IndexMap.find_opt cap regs in
            (reg : _ index option :> int option) :: acc
          in
          Array.of_list (List.rev (IndexSet.fold add_reg cap []))
        in
        (clause, priority, registers)
      in
      let add_transition tr acc =
        let {Label. filter; captures; clear; moves; priority} = OutDFA.label tr in
        let actions = {
          Lrgrep_support.
          move = IndexMap.bindings moves;
          store = List.map snd captures;
          clear = IndexSet.elements clear;
          target = OutDFA.target tr;
          priority;
        } in
        (filter, actions) :: acc
      in
      let transitions = OutDFA.outgoing index in
      {
        Lrgrep_support.
        accept = List.map add_match (OutDFA.matching index);
        halting = OutDFA.unhandled index;
        transitions = IndexSet.fold add_transition transitions [];
      }
    in
    let program = Lrgrep_support.compact OutDFA.states get_state_for_compaction in
    Stopwatch.step time "Compacted program";
    Option.iter output_code oc;
    if !opt_debug_stack <> [] then (
      let rec process st stack =
        Printf.eprintf "state %d\n" (Index.to_int st);
        List.iteri (fun i (accept, clause, captures) ->
            Printf.eprintf "- thread %d recognizes%s clause %d"
              i (if accept then " and ACCEPTS" else "") (Index.to_int clause);
            List.iter (fun (cap, reg) ->
                Printf.eprintf ", var%d in %%%d" (Index.to_int cap) (Index.to_int reg))
              (IndexMap.bindings captures);
            Printf.eprintf "\n";
          ) (OutDFA.threads st);
        match stack with
        | [] ->
          Printf.eprintf "done\n";
        | lr1 :: lr1s ->
          Printf.eprintf "consume lr1 state %d\n" lr1;
          let lr1 = Index.of_int Info.Lr1.n lr1 in
          let check_transition tr acc =
            let label = OutDFA.label tr in
            if not (IndexSet.mem lr1 label.filter)
            then acc
            else match acc with
              | None -> Some tr
              | Some _ -> failwith "non-deterministic transition"
          in
          match IndexSet.fold check_transition (OutDFA.outgoing st) None with
          | None ->
            if IndexSet.mem lr1 (OutDFA.unhandled st) then
              Printf.eprintf "halt (*no transition*)\n"
            else
              Printf.eprintf "error: no transition, stopping there. Stack is invalid or an internal error happened.\n"
          | Some tr ->
            let {Label. captures; clear; moves; _} = OutDFA.label tr in
            IndexMap.iter (fun src tgt ->
                Printf.eprintf
                  "move %%%d -> %%%d\n"
                  (Index.to_int src) (Index.to_int tgt)
              ) moves;
            List.iter (fun (cap, reg) ->
                Printf.eprintf "store %%%d (*variable %d*)\n"
                  (Index.to_int reg) (Index.to_int cap)
              ) captures;
            IndexSet.iter (fun reg ->
                Printf.eprintf "clear %%%d\n" (Index.to_int reg)
              ) clear;
            Printf.eprintf "\n";
            process (OutDFA.target tr) lr1s;
      in
      process OutDFA.initial !opt_debug_stack
    );
    (Label.register_count, Index.to_int OutDFA.initial, program)
  )

  let output_table out entry (registers, initial, (program, table, remap)) =
    let print fmt = Mid.Automata.Printer.fmt out fmt in
    print
      "let lrgrep_program_%s : Lrgrep_runtime.program = {\n"
      entry.Front.Syntax.name;
    print "  registers = %d;\n" registers;
    print "  initial = %d;\n" remap.(initial);
    print "  table = %S;\n" table;
    print "  code = %S;\n" program;
    print "}\n"

  let output_wrapper out entry =
    let {Front.Syntax.name; args; _} = entry in
    let args = "lrgrep_initial" :: "lrgrep_lookahead" :: args in
    let args = String.concat " " args in
    Mid.Automata.Printer.fmt out
      "let %s _lrgrep_env %s = (\n\
      \  List.find_map \n\
      \    (fun m -> lrgrep_execute_%s m %s)\n\
      \    (lrgrep_run lrgrep_program_%s _lrgrep_env)\n\
       )\n"
      name args
      name args
      name

  let process_source source_file =
    let lexer_definition =
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
    let oc, out = match !opt_output_name with
      | None -> (None, None)
      | Some filename ->
        let oc = open_out_bin filename in
        let out =
          Mid.Automata.Printer.create
            ~filename:(Filename.basename filename)
            (output_string oc)
        in
        (Some oc, Some out)
    in
    let print_ocaml_code out (loc, code) =
      Mid.Automata.Printer.print out ~loc code
    in
    out |> Option.iter (fun out ->
        begin match Grammar.Grammar.parameters with
          | [] -> ()
          | parameters ->
            Mid.Automata.Printer.print out "module Make";
            List.iter (Mid.Automata.Printer.fmt out "(%s)") parameters;
            Mid.Automata.Printer.fmt out
              "(%s : module type of %s.Make" parser_name parser_name;
            let extract_name name =
              match String.index_opt name ':' with
              | None -> name
              | Some index -> String.sub name 0 index
            in
            List.iter
              (fun param ->
                 Mid.Automata.Printer.fmt out "(%s)"
                   (extract_name param))
              parameters;
            Mid.Automata.Printer.print out ") = struct\n";
        end;
        print_ocaml_code out lexer_definition.header
      );
    Option.iter (fun out ->
        Mid.Automata.Printer.fmt out
          "include Lrgrep_runtime.Interpreter(%s.MenhirInterpreter)\n" parser_name;
      ) out;
    List.iter (fun entry ->
        let program = process_entry out entry in
        Option.iter (fun out ->
            output_table out entry program;
            output_wrapper out entry
          ) out
      ) lexer_definition.entrypoints;
    out |> Option.iter (fun out ->
        Mid.Automata.Printer.print out "\n";
        print_ocaml_code out lexer_definition.trailer;
        begin match Grammar.Grammar.parameters with
          | [] -> ()
          | _ -> Mid.Automata.Printer.print out "\nend\n"
        end;
      );
    Option.iter close_out oc

  let () = process_source P.source_file
end

let run = function
  | [source_file] -> (
    match !opt_grammar_file with
    | None ->
      Error "No grammar provided (-g)"
    | Some grammar_file ->
      let module _ : sig end = Run(struct
        let source_file = source_file
        let grammar_file = grammar_file
      end)() in
      Ok()
  )
  | [] ->
    Error "No source provided"
  | x1 :: x2 :: _ ->
    Error (
      Printf.sprintf
        "Unexpected arguments: %s %s...\n\
         Expecting a single source file, stopping now" x1 x2
    )
