open Utils
open Misc

(* Command-line parsing. *)

let source_name = ref None
let output_name = ref None
let grammar_file = ref None
let check_coverage = ref false

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
  "-v", Arg.Unit print_version_string,
  " Print version and exit";
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

(* Index LR(1) states by incoming symbol, goto transitions, items, ... *)

module Lr1_index = struct
  open Front
  open Info
  open Fix.Indexing

  (* Group by incoming symbol *)

  let states_of_symbol = Vector.make Symbol.n IndexSet.empty

  let () =
    Index.iter Lr1.n (fun lr1 ->
        match Lr1.incoming lr1 with
        | None -> ()
        | Some sym -> vector_set_add states_of_symbol sym lr1
      )

  let states_of_symbol = Vector.get states_of_symbol

  (* Group by goto transition *)

  let states_by_goto_transition = Vector.make Nonterminal.n IndexSet.empty

  let () =
    Index.iter Transition.goto (fun tr ->
        let state = Transition.(source (of_goto tr)) in
        let symbol = Transition.goto_symbol tr in
        vector_set_add states_by_goto_transition symbol state
      )

  let states_by_goto_transition = Vector.get states_by_goto_transition

  let states_by_item_rhs = Vector.make Symbol.n IndexSet.empty

  let () =
    Index.iter Lr1.n (fun state ->
        List.iter (fun (prod, dot) ->
            if dot < Production.length prod then (
              vector_set_add states_by_item_rhs
                (Production.rhs prod).(dot) state
            )
          ) (Lr1.items state)
      )

  let states_by_item_rhs pattern =
    let pattern = Array.of_list pattern in
    let match_sym sym = function
      | None -> true
      | Some sym' -> equal_index sym sym'
    in
    let match_item (prod, pos) =
      let rhs = Production.rhs prod in
      match
        if Array.length rhs <> pos + Array.length pattern then
          raise Not_found;
        Array.iteri (fun i sym' ->
           if not (match_sym rhs.(pos + i) sym') then
             raise Not_found
          ) pattern
      with
      | () -> true
      | exception Not_found -> false
    in
    let match_state state = List.exists match_item (Lr1.items state) in
    let candidates =
      match pattern.(0) with
      | None -> Lr1.all
      | Some sym -> Vector.get states_by_item_rhs sym
    in
    IndexSet.filter match_state candidates

  (* Map symbol names to actual symbols *)

  let linearize_symbol =
    let buffer = Buffer.create 32 in
    function
    | Syntax.Name s -> s
    | sym ->
      Buffer.reset buffer;
      let rec aux = function
        | Syntax.Name s -> Buffer.add_string buffer s
        | Syntax.Apply (s, args) ->
          Buffer.add_string buffer s;
          Buffer.add_char buffer '(';
          List.iteri (fun i sym ->
              if i > 0 then Buffer.add_char buffer ',';
              aux sym
            ) args;
          Buffer.add_char buffer ')'
      in
      aux sym;
      Buffer.contents buffer

  let find_symbol =
    let table = Hashtbl.create 7 in
    let add_symbol s = Hashtbl.add table (Symbol.name ~mangled:false s) s in
    Index.iter Symbol.n add_symbol;
    fun name -> Hashtbl.find_opt table (linearize_symbol name)

  let get_symbol pos sym =
    match find_symbol sym with
    | None -> error pos "Unknown symbol %s" (linearize_symbol sym)
    | Some sym -> sym
end

module Reduce = struct
  open Info
  open Front.Syntax
  open Regexp

  let transl_filter position = function
    | Filter_item (sym, prefix, suffix) ->
      RE.Filter (
        match Symbol.desc (Lr1_index.get_symbol position symbol) with
        | T _ -> error position "Expecting a non-terminal before ':'"
        | N n -> Lr1_index.states_by_goto_transition n
      )
    | Filter_dot symbols ->
      RE.Filter (
        symbols
        |> List.map (Option.map (Lr1_index.get_symbol position))
        |> Lr1_index.states_by_item_rhs
      )

  let rec transl re =
    let desc = match re.desc with
      | Atom (capture, symbol) ->
        if Option.is_some capture then
          error re.position "Captures are not allowed inside reductions";
        let set = match symbol with
          | None -> Lr1.all
          | Some sym ->
            match Lr1_index.find_symbol sym with
            | None ->
              error re.position "Unknown symbol"
            | Some sym ->
              if Symbol.is_terminal sym then
                warn re.position "A reduction can only match non-terminals";
              Lr1_index.states_of_symbol sym
        in
        RE.Set (set, None)
      | Alternative res ->
        RE.Alt (List.map transl res)
      | Repetition re ->
        RE.Star (transl re)
      | Reduce _ ->
        error re.position "Reductions cannot be nested"
      | Concat res ->
        RE.Seq (List.rev_map transl res)
      | Filter filter ->
        transl_filter re.position filter
    in
    RE.make re.position desc
end

open Front

let parser_module =
  String.capitalize_ascii (Filename.basename Grammar.Grammar.basename)

let process_entry oc entry =
  let cases, vars =
    let var_count = ref 0 in
    let transl_case i case =
      let vars = ref [] in
      let varset = ref IndexSet.empty in
      let alloc name =
        let id = !var_count in
        Utils.Misc.push vars name;
        incr var_count;
        let v = Regexp.RE.var id in
        varset := IndexSet.add v !varset;
        v
      in
      let clause = Regexp.KRE.clause i in
      let kre = Regexp.transl ~alloc ~clause case.Syntax.pattern in
      let vars = List.rev !vars in
      (kre, (vars, !varset))
    in
    List.split (List.mapi transl_case entry.Syntax.clauses)
  in
  let cases = Regexp.KRESet.of_list cases in
  let dfa = Dfa.derive_dfa cases in
  if !check_coverage then (
    let module Coverage = Back.Coverage.Make(Dfa)() in
    let module Check = Coverage.Check_dfa(Coverage.Lrce.NFA) in
    let check = Check.analyse dfa in
    let count = ref 6 in
    try Seq.iter (fun (_st, _nfa, nfa_path) ->
        let initial = ref false in
        let print nfa =
          let lr1 = Coverage.Lrce.NFA.label nfa in
          match Info.Lr1.incoming lr1 with
          | None -> initial := true; Info.Lr1.to_string lr1
          | Some sym -> Info.Symbol.name sym
        in
        let path = List.rev_map print nfa_path in
        let path = if !initial then path else "..." :: path in
        let path = String.concat " " path in
        prerr_endline ("Found uncovered case:\n  " ^ path);
        let gotos, la = Coverage.Lrce.compute_lookahead nfa_path in
        prerr_endline ("Looking ahead at:\n  {" ^
                       string_concat_map ", " Info.Terminal.to_string
                         (IndexSet.elements la) ^ "}\n");
        prerr_endline "Goto targets on the path:\n";
        List.iter (fun step ->
            prerr_endline "Step:\n";
            List.iter (List.iter (fun lr1 ->
                let items = Info.Lr1.items lr1 in
                prerr_endline
                  (string_concat_map " | "
                     (fun (p,i) -> Printf.sprintf "(#%d,%d)" (p : _ Fix.Indexing.index :> int) i)
                     items);
                let items = List.map (fun (p,i) -> Info.Production.to_g p, i) items in
                Format.eprintf "%a\n%!" Grammar.Print.itemset items
              )) step;
          ) gotos;
        decr count;
        (*if !count = 0 then (
          prerr_endline "Press enter to get more cases, \
                         type anything else to stop";
          match read_line () with
          | "" -> count := 6
          | _ -> raise Exit
        )*)
      ) (Check.paths check)
    with Exit -> ()
  );
  Format.eprintf "(* %d states *)\n%!" (Array.length dfa);
  begin match oc with
  | None -> ()
  | Some oc ->
    let registers, liveness =
      let t0 = Sys.time () in
      let module RA = Back.Regalloc.Make(Dfa) in
      let liveness = RA.liveness (Array.of_list (List.map snd vars)) dfa in
      let dt = Sys.time () -. t0 in
      Printf.eprintf "liveness: %.02fms\n" (dt *. 1000.0);
      liveness
    in
    let optionals =
      IndexMap.fold (fun _ -> IndexSet.union) liveness.(0) IndexSet.empty
    in
    output_char oc '\n';
    let _symbols, typeable = recover_types dfa in
    gen_code entry oc optionals vars typeable entry.Syntax.clauses;
    output_char oc '\n';
    output_table oc entry registers (Dfa.gen_table dfa liveness)
  end

let () = (
  (*let doc = Cmon.list_map (KRE.cmon ()) kst.direct in
  if verbose then (
    Format.eprintf "%a\n%!" Cmon.format (Syntax.print_entrypoints entry);
    Format.eprintf "%a\n%!" Cmon.format doc;
  );*)
  let oc = match !output_name with
    | None -> None
    | Some path ->
      let oc = open_out_bin path in
      output_string oc (snd lexer_definition.header);
      Some oc
  in
  List.iter (process_entry oc) lexer_definition.entrypoints;
  begin match oc with
    | None -> ()
    | Some oc ->
      output_char oc '\n';
      output_string oc (snd lexer_definition.trailer);
      close_out oc
  end;
  (* Print matching functions *)
)
