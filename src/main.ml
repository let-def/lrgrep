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

let () =
  let open Grammar in
  let (+:=) x y = x := !x + y in
  let shift = ref 0 in
  let reduce_without_default_reductions = ref 0 in
  let reduce_with_default_reductions = ref 0 in
  let default_reductions = ref 0 in
  let red_aggressive = ref 0 in
  let count_aggressive = ref 0 in
  Lr1.iter (fun lr1 ->
      let shifts =
        Lr1.transitions lr1
        |> List.filter (fun (sym, _) -> match sym with T _ -> true | _ -> false)
        |> List.length
      in
      shift +:= shifts;
      let total = ref 0 in
      let largest = ref 0 in
      let reds =
        Lr1.reductions lr1
        |> List.map (fun (_, p) -> p)
        |> Misc.group_by
          ~compare:(fun p1 p2 -> Int.compare (Production.to_int p1) (Production.to_int p2))
          ~group:(fun _p ps -> (1 + List.length ps))
      in
      List.iter (fun count ->
          largest := max !largest count;
          total +:= count
        ) reds;
      reduce_without_default_reductions +:= !total;
      begin match reds, shifts with
        | [_], 0 ->
          default_reductions +:= 1
        | _, _ -> reduce_with_default_reductions +:= !total
      end;
      red_aggressive +:= (!total - !largest);
      if !largest > 0 then
        incr count_aggressive
    );
  Printf.eprintf
    "Transition statistics\n\
     shift (incompressible)         : %d\n\
     total (w/o default reductions) : %d\n\
     total (w/ default reductions)  : %d + %d default reductions\n\
     aggressive default reductions  : %d + %d default reductions\n"
    !shift
    !reduce_without_default_reductions
    !reduce_with_default_reductions !default_reductions
    !red_aggressive !count_aggressive

let () =
  let open Grammar in
  let by_default = ref IntMap.empty in
  Lr1.iter (fun lr1 ->
      let reds =
        Lr1.reductions lr1
        |> Misc.group_by
          ~compare:(fun (_,p1) (_,p2) -> Int.compare (Production.to_int p1) (Production.to_int p2))
          ~group:(fun (t,p) tps -> (p, 1 + List.length tps, (t,p) :: tps))
      in
      let prod = ref (-1) in
      let largest = ref 0 in
      let shifts =
        List.filter_map
          (fun (sym, lr1) -> match sym with T t -> Some (t, `Shift lr1) | _ -> None)
          (Lr1.transitions lr1)
      in
      let num_transitions = ref (List.length shifts) in
      List.iter (fun (p, count, _) ->
          if count > !largest then (
            largest := count;
            prod := (Production.to_int p);
          );
          num_transitions := !num_transitions + count
        ) reds;
      let transitions =
        shifts @ List.concat_map (fun (prod', _, tps) ->
          if Production.to_int prod' = !prod then
            []
          else
            List.map (fun (t, p) -> (t, `Reduce p)) tps
        ) reds
      in
      by_default := IntMap.update !prod (fun states ->
          let states = Option.value states ~default:[] in
          Some ((!num_transitions - !largest, List.sort compare transitions, lr1) :: states)
        ) !by_default
    );
  let count = ref 0 in
  let group = ref 0 in
  let sum_of_diff = ref 0 in
  let largest_class = ref 0 in
  IntMap.iter (fun _ states ->
      let states =
        List.sort
          (fun (n1, _, _) (n2, _, _) -> Int.compare n1 n2)
          states
      in
      let rec distance = function
        | [], x | x, [] -> List.length x
        | (k1, v1) :: l1, (k2, v2) :: l2 ->
          if k1 = k2 then (
            if v1 = v2
            then distance (l1, l2)
            else 1 + distance (l1, l2)
          ) else if k1 < k2 then
            1 + distance (l1, (k2, v2) :: l2)
          else
            1 + distance ((k1, v1) :: l1, l2)
      in
      let class_size =
        List.fold_left (fun best (_, t1, _) ->
            min best
              (List.fold_left (fun sum (_, t2, _) -> sum + min (distance(t1,t2)) (List.length t2)) 0 states)
          ) max_int states
      in
      sum_of_diff := !sum_of_diff + class_size;
      largest_class := max !largest_class class_size;
      let _ =
        List.fold_left (fun candidates (n, transitions, _) ->
            let count' = List.fold_left (fun count' candidate ->
                min count' (distance (candidate, transitions))
              ) n candidates
            in
            count := !count + count';
            transitions :: candidates
          ) [] states
      in
      group := max !group (List.length states);
    ) !by_default;
  Printf.eprintf
    "compress everything   : %d edges, largest class: %d\n\
     sum of diff (1-parent): %d, largest class: %d\n"
    !count !group !sum_of_diff !largest_class

module Regexp = Mid.Regexp.Make(Info)()

open Front

module Symbols = Fix.Indexing.Sum(Info.Terminal)(Info.Nonterminal)

let parser_module =
  String.capitalize_ascii (Filename.basename Grammar.Grammar.basename)

let gen_code entry oc optionals vars var_typeable clauses =
  let print fmt = Printf.fprintf oc fmt in
  print
    "let execute_%s %s : (int * %s.MenhirInterpreter.element option array) * %s.token -> _ option = function\n"
    entry.Syntax.name (String.concat " " entry.Syntax.args) parser_module parser_module;
  List.iteri (fun index ((varnames, varindices), clause) ->
      let recover_types =
        let symbol_matcher s = match Symbols.prj s with
          | L t -> "T T_" ^ Info.Terminal.to_string t
          | R n -> "N N_" ^ Grammar.Nonterminal.mangled_name (Info.Nonterminal.to_g n)
        in
        List.fold_left2 (fun acc name index ->
            let is_optional = IndexSet.mem index optionals in
            let types = match IndexMap.find_opt index var_typeable with
              | None -> None
              | Some (typ, cases) ->
                let matchers =
                  List.map symbol_matcher (IndexSet.elements cases)
                in
                Some (
                  Printf.sprintf "\
                  match %s.MenhirInterpreter.incoming_symbol st with \
                  | %s -> ((x : %s), startp, endp)
                  | _ -> assert false
                  " parser_module
                    (String.concat " | " matchers) typ
                )
            in
            match is_optional, types with
            | true, None -> acc
            | true, Some types ->
              Printf.sprintf "\
              let %s = match %s with \
                | None -> None \
                | Some (%s.MenhirInterpreter.Element (st, x, startp, endp)) -> \
                  Some (%s) in" name name parser_module types
              :: acc
            | false, None ->
              Printf.sprintf "let %s = match %s with None -> assert false | Some x -> x in"
                name name :: acc
            | false, Some types ->
              Printf.sprintf "\
              let %s = match %s with None -> assert false \
                | Some (%s.MenhirInterpreter.Element (st, x, startp, endp)) -> \
                  %s in" name name parser_module types :: acc
          ) [] varnames (IndexSet.elements varindices)
        |> String.concat "\n"
      in
      let print_loc (loc : Syntax.location) =
        Printf.sprintf "# %d %S\n%s"
          loc.start_line loc.loc_file
          (String.make loc.start_col ' ')
      in
      let lookahead_constraint = match clause.Syntax.lookaheads with
         | [] -> None
         | terminals ->
           let sym_pattern sym =
             match Regexp.transl_symbol (Syntax.Name sym) with
             | Info.Symbol.N n ->
               failwith ("Lookahead should be a terminal, " ^
                         Info.Nonterminal.to_string n ^ " is a nonterminal")
             | Info.Symbol.T t ->
               let name = Info.Terminal.to_string t in
               match Info.Terminal.semantic_value t with
               | None -> name
               | Some _ -> name ^ " _"
           in
           Some (string_concat_map ~wrap:("(",")") "|" sym_pattern terminals)
      in
      print "  | (%d, [|%s|]), %s -> %s begin\n%s\n    end\n"
        index
        (String.concat ";" varnames)
        (Option.value lookahead_constraint ~default:"_")
        recover_types
        (match clause.Syntax.action with
         | Unreachable -> "failwith \"Should be unreachable\""
         | Partial (loc, str) ->
           print_loc loc ^ str
         | Total (loc, str) ->
           "Some (\n" ^ print_loc loc ^ str ^ ")");
      if Option.is_some lookahead_constraint then
        print "  | (%d, [|%s|]), _ -> None\n"
          index (string_concat_map ";" (fun _ -> "_") varnames)
    ) (List.combine vars clauses);
  print "  | _ -> failwith \"Invalid action (internal error or API misuse)\"\n\n"

let output_table oc entry registers (program, table, remap) =
  let print fmt = Printf.fprintf oc fmt in
  print "module Table_%s : Lrgrep_runtime.Parse_errors = struct\n"
    entry.Syntax.name;
  print "  let registers = %d\n" registers;
  print "  let initial = %d\n" remap.(0);
  print "  let table = %S\n" table;
  print "  let program = %S\n" program;
  print "end\n"

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
