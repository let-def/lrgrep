open Utils
open Misc

module StringSet = Set.Make(String)

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

module Transl = struct
  open Info
  open Front.Syntax
  open Fix.Indexing
  open Regexp

  let transl_filter position = function
    | Filter_item (sym, prefix, suffix) ->
      let dot = List.length prefix in
      let symbols =
        Array.of_list (prefix @ suffix)
        |> Array.map (Option.map (Lr1_index.get_symbol position))
      in
      let wild_match sym = function
        | None -> true
        | Some sym' -> sym = sym'
      in
      let approx_candidates =
        if dot = 0 then Lr1.all
        else match symbols.(dot - 1) with
          | None -> Lr1.all
          | Some sym -> Lr1_index.states_of_symbol sym
      in
      let filter_state state =
        Lr1.items state
        |> List.exists (fun (prod, dot') ->
            dot = dot' &&
            Production.length prod = Array.length symbols &&
            (match sym with
             | None -> true
             | Some sym ->
               Lr1_index.get_symbol position sym =
               Symbol.inj_r (Production.lhs prod)
            ) &&
            Array.for_all2 wild_match (Production.rhs prod) symbols
          )
      in
      IndexSet.filter filter_state approx_candidates
    | Filter_dot symbols ->
      symbols
      |> List.map (Option.map (Lr1_index.get_symbol position))
      |> Lr1_index.states_by_item_rhs

  let rec transl_reduce re =
    let desc = match re.desc with
      | Atom (capture, symbol) ->
        if Option.is_some capture then
          error re.position "Captures are not allowed inside reductions";
        let set = match symbol with
          | None -> Lr1.all
          | Some sym ->
            let sym = Lr1_index.get_symbol re.position sym in
            if Symbol.is_terminal sym then
              warn re.position "A reduction can only match non-terminals";
            Lr1_index.states_of_symbol sym
        in
        RE.Set (set, None)
      | Alternative res ->
        RE.Alt (List.map transl_reduce res)
      | Repetition re ->
        RE.Star (transl_reduce re)
      | Reduce _ ->
        error re.position "Reductions cannot be nested"
      | Concat res ->
        RE.Seq (List.map transl_reduce res)
      | Filter filter ->
        let states = transl_filter re.position filter in
        if IndexSet.is_empty states then
          warn re.position "No items match this filter";
        RE.Filter states
    in
    RE.make re.position desc

  type lr1_trie = {
    mutable sub: lr1_trie Lr1.map;
    mutable reached: Redgraph.state indexset;
  }

  let lr1_trie_root =
    let root = {sub = IndexMap.empty; reached = IndexSet.empty} in
    let rec visit_trie node = function
      | [] -> node
      | x :: xs ->
        let node' = match IndexMap.find_opt x node.sub with
          | Some node' -> node'
          | None ->
            let node' = {sub = IndexMap.empty; reached = IndexSet.empty} in
            node.sub <- IndexMap.add x node' node.sub;
            node'
        in
        visit_trie node' xs
    in
    Index.iter Redgraph.state (fun state ->
        let def = Redgraph.states state in
        let node = visit_trie root (def.stack : Redgraph.stack :> _ list) in
        node.reached <- IndexSet.add state node.reached
      );
    root

  let compile_reduce_expr re =
    let reached = ref IndexSet.empty in
    let rec step node k =
      K.derive
        ~accept:(fun () -> reached := IndexSet.union !reached node.reached)
        ~direct:(fun labels _ k' ->
            IndexMap.iter (fun label node' ->
                if IndexSet.mem label labels then
                  step node' k'
              ) node.sub
          )
        k
    in
    step lr1_trie_root (K.More (re, K.Done ()));
    !reached

  let compile_reduce expr =
    (*print_cmon stderr (Front.Syntax.cmon_regular_expression expr);
      prerr_newline ();*)
    let re = transl_reduce expr in
    (*print_cmon stderr (RE.cmon re);
      prerr_newline ();*)
    compile_reduce_expr re

  let rec transl re =
    let desc = match re.desc with
      | Atom (capture, symbol) ->
        ignore capture;
        let set = match symbol with
          | None -> Lr1.all
          | Some sym ->
            Lr1_index.states_of_symbol (Lr1_index.get_symbol re.position sym)
        in
        RE.Set (set, None)
      | Alternative res ->
        RE.Alt (List.map transl res)
      | Repetition re ->
        RE.Star (transl re)
      | Reduce {capture; kind; expr} ->
        ignore capture;
        ignore kind;
        (* print_cmon stderr (Front.Syntax.cmon_regular_expression expr);*)
        let pattern = compile_reduce expr in
        warn re.position
          "Reduce pattern is matching %d/%d cases\n"
          (IndexSet.cardinal pattern) (cardinal Redgraph.state);
        let strings = ref StringSet.empty in
        IndexSet.iter
          (fun state -> strings := StringSet.add (Redgraph.to_string state) !strings)
          pattern;
        (*StringSet.iter prerr_endline !strings;*)
        RE.Reduce {capture = None; pattern}
      | Concat res ->
        RE.Seq (List.map transl res)
      | Filter filter ->
        let states = transl_filter re.position filter in
        if IndexSet.is_empty states then
          warn re.position "No item matches this filter";
        RE.Filter states
    in
    RE.make re.position desc

end

(*module Clause = struct
  open Info
  open Front.Syntax
  open Regexp
  end*)

open Front

let parser_module =
  String.capitalize_ascii (Filename.basename Grammar.Grammar.basename)

let process_entry _oc (entry : Syntax.entry) =
  List.iter (fun (clause : Syntax.clause) ->
      (*print_cmon stderr (Front.Syntax.cmon_regular_expression clause.pattern);*)
      let _ = Transl.transl clause.pattern in
      ()
    ) entry.clauses

let () = (
  (*let doc = Cmon.list_map (KRE.cmon ()) kst.direct in
  if verbose then (
    Format.eprintf "%a\n%!" Cmon.format (Syntax.print_entrypoints entry);
    Format.eprintf "%a\n%!" Cmon.format doc;
  );*)
  (*let oc = match !output_name with
    | None -> None
    | Some path ->
      let oc = open_out_bin path in
      output_string oc (snd lexer_definition.header);
      Some oc
    in*)
  let oc = None in
  List.iter (process_entry oc) lexer_definition.entrypoints;
  (*begin match oc with
    | None -> ()
    | Some oc ->
      output_char oc '\n';
      output_string oc (snd lexer_definition.trailer);
      close_out oc
    end;*)
  (* Print matching functions *)
)
