open Utils
open Misc

module StringSet = Set.Make(String)

(* Command-line parsing. *)

let opt_source_name = ref None
let opt_output_name = ref None
let opt_grammar_file = ref None
let opt_check_coverage = ref false
let opt_verbose = ref false
let opt_debug_stack = ref []

type enumerate =
  | Enum_lr0
  | Enum_lr1
  | Enum_goto

let opt_enumerate = ref None

type enumerate_format =
  | Efmt_raw
  | Efmt_json

let opt_enumerate_format = ref Efmt_raw

let opt_enumerate_entrypoint = ref []

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
  "-enumerate", Arg.String (function
      | "lr0" -> opt_enumerate := Some Enum_lr0
      | "lr1" -> opt_enumerate := Some Enum_lr1
      | "goto" -> opt_enumerate := Some Enum_goto
      | arg ->
        Printf.eprintf
          "-enumerate: invalid value %S\n\
           Valid values, from least to most verbose:\n\
           - lr0: enumerate sentences to reach lr0 states\n\
           - lr1: enumerate sentences to reach lr1 states\n\
           - goto: enumerate sentences to follow all goto transitions\n"
          arg;
      exit 1
  ),
  " <lr0|lr1|goto> Enumerate sentences to cover failing configurations";
  "-enumerate-format", Arg.String (function
      | "raw" -> opt_enumerate_format := Efmt_raw
      | "json" -> opt_enumerate_format := Efmt_json
      | arg ->
        Printf.eprintf
          "-enumerate-format: invalid value %S\n\
           From least verbose to most verbose:\n\
           - raw (default): custom format, with one sentence group per line\n\
           - json: a line-delimited sequence of json objects\n"
          arg;
      exit 1
  ),
  " <raw|json> Format used to output enumeration";
  "-enumerate-entrypoint", Arg.String (push opt_enumerate_entrypoint),
  " <start-symbol> Enumerate starting from this entrypoint\n\
  \ Default is to enumerate all start symbols.";
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

let () = Stopwatch.step Stopwatch.main "Beginning"

module Grammar = MenhirSdk.Cmly_read.Read(struct let filename = grammar_file end)

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

let entrypoints =
  let table = Hashtbl.create 7 in
  Fix.Indexing.Index.iter Info.Lr1.n (fun lr1 ->
      match Info.Lr0.entrypoint (Info.Lr1.to_lr0 lr1) with
      | None -> ()
      | Some nt ->
        let name = Info.Nonterminal.to_string nt in
        (* When "n" is a non-terminal marked %start, Menhir generates a special
           symbol "n'" that represents the use of "n" as a start symbol. "n" is
           kept for uses as a normal non-terminal elsewhere in the grammar.
           To refer to "n" as a start-symbol we have to look for "n'".
           Here we do the converse, chopping of a "'" to find the source
           nonterminal . *)
        let name = String.sub name 0 (String.length name - 1) in
        Hashtbl.add table name lr1
    );
  table

let all_entrypoints =
  Hashtbl.fold
    (fun _ lr1 acc -> IndexSet.add lr1 acc)
    entrypoints IndexSet.empty

let translate_entrypoints prj loc err symbols =
  let unhandled = ref [] in
  let result =
    List.filter_map (fun sym ->
        let result = Hashtbl.find_opt entrypoints (prj sym) in
        if Option.is_none result then push unhandled sym;
        result
      ) symbols
  in
  match List.rev !unhandled with
  | [] -> IndexSet.of_list result
  | first :: rest as all ->
    Printf.ksprintf (fun msg -> err (loc first) msg)
      "Unknown start symbol%s %s.\nValid start symbols are %s."
      (if rest = [] then "" else "s")
      (string_concat_map ", " prj all)
      (string_concat_map ", " Fun.id
         (List.sort String.compare
            (Hashtbl.fold (fun key _ acc -> key :: acc) entrypoints [])))

let parser_name =
  String.capitalize_ascii (Filename.basename Grammar.Grammar.basename)

let process_entry oc (entry : Front.Syntax.entry) = (
  let open Fix.Indexing in
  let initials =
    match entry.startsymbols with
    | [] -> all_entrypoints
    | syms ->
      translate_entrypoints fst snd
        (fun loc msg -> error loc "%s" msg)
        syms
  in
  let module Lrc = Mid.Lrc.Close(Info)(Lrc)
    (struct let initials = indexset_bind initials Lrc.lrcs_of_lr1 end)
  in
  let open Mid.Automata.Entry
      (Transl)
      (struct
        include Lrc
        let initials = Lrc.idle
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
  if !opt_check_coverage then (
    let open Info in
    let module Reach = Mid.Reachable_reductions.Make(Info)(Viable)(Lrc)() in
    let module Failure = Mid.Reachable_reductions.FailureNFA(Info)(Viable)(Lrc)(Reach)() in
    let module Check = Mid.Reachable_reductions.Coverage_check(Info)(Lrc)(Failure)(struct
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
    Check.enum_sentence (fun suffix lrc unhandled ->
      let suffix = List.filter_map (fun sym ->
        match Check.Sym_or_lr1.prj sym with
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


let output_table oc entry (registers, initial, (program, table, remap)) =
  let print fmt = Printf.fprintf oc fmt in
  print "module Table_%s : Lrgrep_runtime.Parse_errors = struct\n"
    entry.Front.Syntax.name;
  print "  let registers = %d\n" registers;
  print "  let initial = %d\n" remap.(initial);
  print "  let table = %S\n" table;
  print "  let program = %S\n" program;
  print "end\n"

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
  Option.iter close_out oc

let () =
  begin match !opt_enumerate with
    | None -> ()
    | Some precision ->
      let initials =
        match List.rev !opt_enumerate_entrypoint with
        | [] -> all_entrypoints
        | syms ->
          translate_entrypoints Fun.id (Fun.const ())
            (fun () msg ->
               Printf.eprintf "Invalid -enumerat-entrypoint: %s" msg;
               exit 1)
            syms
      in
      let module Lrc = Mid.Lrc.Close(Info)(Lrc)(struct
        let initials = indexset_bind initials Lrc.lrcs_of_lr1
      end) in
      let module Reachable = Mid.Reachable_reductions.Make(Info)(Viable)(Lrc)() in
      let module Enum = Enum.Make(Info)(Reachability)(Viable)(Lrc)(Reachable) in
      let output = match !opt_enumerate_format with
        | Efmt_raw -> Enum.Output_raw.output_sentence
        | Efmt_json -> Enum.Output_json.output_sentence
      in
      match precision with
      | Enum_lr0 ->
        Enum.enumerate
          ~cover:Info.Lr0.n
          ~index:(fun x -> Info.Lr1.to_lr0 (Enum.Coverage.lr1_of x))
          (output stdout)
      | Enum_lr1 ->
        Enum.enumerate
          ~cover:Info.Lr1.n
          ~index:(fun x -> Enum.Coverage.lr1_of x)
          (output stdout)
      | Enum_goto ->
        Enum.enumerate
          ~cover:Reachable.n
          ~index:(fun x -> x)
          (output stdout)
  end;
  begin match !opt_source_name with
    | None ->
      if Option.is_none !opt_enumerate then (
        Format.eprintf "No source provided, stopping now.\n";
        Arg.usage specs usage;
        exit 1
      )
    | Some path -> process_source path
  end;
