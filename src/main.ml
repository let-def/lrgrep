open Utils
module StringSet = Set.Make(String)

(* Command-line parsing. *)

let source_name = ref None
let output_name = ref None
let grammar_file = ref None
let check_coverage = ref false
let verbose = ref false

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
  "-o", Arg.String (fun x -> output_name := Some x),
  " <file.ml>  Set output file name to <file> (defaults to <source>.ml)";
  "-g", Arg.String (fun x -> grammar_file := Some x),
  " <file.cmly>  Path of the Menhir compiled grammar to analyse (*.cmly)";
  "-v", Arg.Set verbose,
  " Increase output verbosity";
  "-version", Arg.Unit print_version_string,
  " Print version and exit";
  "-vnum", Arg.Unit print_version_num,
  " Print version number and exit";
  "-coverage", Arg.Set check_coverage,
  " Check error coverage";
]

let () = Arg.parse specs (fun name -> source_name := Some name) usage

let source_file = match !source_name with
  | None ->
    Format.eprintf "No source provided, stopping now.\n";
    Arg.usage specs usage;
    exit 1
  | Some name -> name

let grammar_file = match !grammar_file with
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

module Grammar = MenhirSdk.Cmly_read.Read(struct let filename = grammar_file end)
module Info = Mid.Info.Make(Grammar)
module Regexp = Mid.Regexp.Make(Info)()
module Transl = Mid.Transl.Make(Regexp)
module Lrc = Mid.Lrc.Make(Info)()

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
  Printf.eprintf "Raw DFA states: %d\n" (cardinal BigDFA.n);
  Printf.eprintf "Minimized DFA states: %d\n" (cardinal MinDFA.states);
  Printf.eprintf "Time spent: %.02fms\n" (Sys.time () *. 1000.);
  let unhandled = ref 0 in
  Index.iter MinDFA.states (fun state ->
      if not (IndexSet.is_empty (MinDFA.unhandled state)) then
        incr unhandled;
    );
  Printf.eprintf "states with unhandled transitions: %d\n%!" !unhandled;
  (*let module Coverage =
    Mid.Coverage.Make(Info)(MinDFA)(Lrc.Lrce)
  in*)
  let get_state_for_compaction index =
    let add_match (clause, regs) =
      let cap = Clause.captures clause in
      let registers =
        let add_reg cap acc =
          let reg = IndexMap.find_opt cap regs in
          (reg : _ index option :> int option) :: acc
        in
        Array.of_list (List.rev (IndexSet.fold add_reg cap []))
      in
      (clause, registers)
    in
    let add_transition tr acc =
      let {Label. filter; captures; clear; moves} = MinDFA.label tr in
      let actions = {
        Lrgrep_support.
        move = IndexMap.bindings moves;
        store = List.map snd (IndexMap.bindings captures);
        clear = IndexSet.elements clear;
        target = MinDFA.target tr;
      } in
      (filter, actions) :: acc
    in
    let transitions = MinDFA.outgoing index in
    {
      Lrgrep_support.
      accept = List.map add_match (MinDFA.matches index);
      halting = MinDFA.unhandled index;
      transitions = IndexSet.fold add_transition transitions [];
    }
  in
  Label.register_count,
  Index.to_int MinDFA.initial,
  let program = Lrgrep_support.compact MinDFA.states get_state_for_compaction in
  Option.iter output_code oc;
  program
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
  let oc = Option.map open_out_bin !output_name in

  oc |> Option.iter (fun oc -> output_string oc (snd lexer_definition.header));

  List.iter (fun entry ->
      let program = process_entry oc entry in
      Option.iter (fun oc -> output_table oc entry program) oc
    ) lexer_definition.entrypoints;

  oc |> Option.iter (fun oc ->
      output_char oc '\n';
      output_string oc (snd lexer_definition.trailer);
    );

  Option.iter close_out oc;
  (* Print matching functions *)
)
