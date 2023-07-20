open Utils
module StringSet = Set.Make(String)

(* Command-line parsing. *)

let opt_source_name = ref None
let opt_output_name = ref None
let opt_grammar_file = ref None
let opt_check_coverage = ref false
let opt_verbose = ref false
let opt_debug_stack = ref []

let escape_and_align_left fmt =
  Printf.ksprintf (fun str ->
      let str = Bytes.unsafe_of_string (String.escaped str) in
      for i = 0 to Bytes.length str - 2 do
        if Bytes.get str (i+0) = '\\' &&
           Bytes.get str (i+1) = 'n' then
          Bytes.set str (i+1) 'l'
      done;
      ("\"" ^ Bytes.unsafe_to_string str ^ "\\l\"")
    ) fmt

let usage =
  Printf.sprintf
    "lrgrep, a menhir lexer\n\
     usage: %s [options] <source>"
    Sys.argv.(0)

let print_version_num () =
  print_endline "0.1";
  exit 0

let print_version_string () =
  print_string "The Menhir parser lexer generator :-], version ";
  print_version_num ()

let error {Front.Syntax. line; col} fmt =
  Printf.eprintf "Error line %d, column %d: " line col;
  Printf.kfprintf (fun oc -> output_char oc '\n'; flush oc; exit 1) stderr fmt

let warn {Front.Syntax. line; col} fmt =
  Printf.eprintf "Warning line %d, column %d: " line col;
  Printf.kfprintf (fun oc -> output_char oc '\n'; flush oc) stderr fmt

let eprintf = Printf.eprintf

let specs = [
  "-o", Arg.String (fun x -> opt_output_name := Some x),
  " <file.ml>  Set output file name to <file> (defaults to <source>.ml)";
  "-g", Arg.String (fun x -> opt_grammar_file := Some x),
  " <file.cmly>  Path of the Menhir compiled grammar to analyse (*.cmly)";
  "-v", Arg.Set opt_verbose,
  " Increase output verbosity";
  "-version", Arg.Unit print_version_string,
  " Print version and exit";
  "-vnum", Arg.Unit print_version_num,
  " Print version number and exit";
  "-coverage", Arg.Set opt_check_coverage,
  " Check error coverage";
  "-debug-stack", Arg.String (fun stack ->
      opt_debug_stack :=
        List.map
          (fun state -> int_of_string (String.trim state))
          (String.split_on_char ',' stack)
    ),
  " For debugging purposes. Simulate action of the automaton on a stack,\n\
  \ specified as a comma separated list of state numbers (top comes first)"
]

let () = Arg.parse specs (fun name -> opt_source_name := Some name) usage

let source_file = match !opt_source_name with
  | None ->
    Format.eprintf "No source provided, stopping now.\n";
    Arg.usage specs usage;
    exit 1
  | Some name -> name

let grammar_file = match !opt_grammar_file with
  | Some filename -> filename
  | None ->
    Format.eprintf "No grammar provided (-g), stopping now.\n";
    Arg.usage specs usage;
    exit 1

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

let () = Stopwatch.step Stopwatch.main "Beginning"

module Grammar = MenhirSdk.Cmly_read.Read(struct let filename = grammar_file end)

let () = Stopwatch.step Stopwatch.main "Loaded grammar"

module Info = Mid.Info.Make(Grammar)
module Regexp = Mid.Regexp.Make(Info)()
module Transl = Mid.Transl.Make(Regexp)
module Lrc = Mid.Lrc.Make(Info)()
module Tmp = Lrc.Redgraph2(Regexp.Redgraph)()

module type STACKS = Mid.Automata.STACKS with type lr1 := Info.Lr1.n

module Lr1_stacks : STACKS with type n = Info.Lr1.n =
struct
  type n = Info.Lr1.n
  let n = Info.Lr1.n
  let initials = Info.Lr1.idle
  let next = Info.Lr1.predecessors
  let label = IndexSet.singleton
end

let parser_name =
  String.capitalize_ascii (Filename.basename Grammar.Grammar.basename)

let process_entry oc (entry : Front.Syntax.entry) = (
  let open Fix.Indexing in
  let open Mid.Automata.Entry
      (Transl)
      (*Lrc.Lrce Lrc.Lrc_NFA Lr1_stacks*)
      (Lrc.Lrce)
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
  let unhandled = ref 0 in
  Index.iter OutDFA.states (fun state ->
      if not (IndexSet.is_empty (OutDFA.unhandled state)) then
        incr unhandled;
    );
  Printf.eprintf "states with unhandled transitions: %d\n%!" !unhandled;
  (*let module Coverage =
    Mid.Coverage.Make(Info)(OutDFA)(Lrc.Lrce)
  in*)
  let get_state_for_compaction index =
    let add_match (clause, priority, regs) =
      let cap = Clause.captures clause in
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


let output_table oc entry (registers, initial, (program, table, remap)) =
  let print fmt = Printf.fprintf oc fmt in
  print "module Table_%s : Lrgrep_runtime.Parse_errors = struct\n"
    entry.Front.Syntax.name;
  print "  let registers = %d\n" registers;
  print "  let initial = %d\n" remap.(initial);
  print "  let table = %S\n" table;
  print "  let program = %S\n" program;
  print "end\n"

let () = (
  (*if !verbose then (
    let doc = Cmon.list_map Regexp.K.cmon kst.direct in
    Format.eprintf "%a\n%!" Cmon.format (Syntax.print_entrypoints entry);
    Format.eprintf "%a\n%!" Cmon.format doc;
    );*)
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

  List.iter (fun entry ->
      let program = process_entry out entry in
      Option.iter (fun oc -> output_table oc entry program) oc
    ) lexer_definition.entrypoints;

  out |> Option.iter (fun out ->
      Mid.Automata.Printer.print out "\n";
      print_ocaml_code out lexer_definition.trailer;
      begin match Grammar.Grammar.parameters with
        | [] -> ()
        | _ -> Mid.Automata.Printer.print out "\nend\n"
      end;
    );

  Option.iter close_out oc;
  (* Print matching functions *)
)
