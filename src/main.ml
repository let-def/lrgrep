(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* The lexer generator. Command-line parsing. *)

let source_name = ref None
let output_name = ref None
let grammar_file = ref None
let interpret = ref false

let usage = "usage: menhirlex [options] sourcefile"

let print_version_string () =
  print_string "The Menhir parser lexer generator :-], version ";
  print_string Sys.ocaml_version;
  print_newline ();
  exit 0

let print_version_num () =
  print_endline Sys.ocaml_version;
  exit 0

let specs = [
  "-o", Arg.String (fun x -> output_name := Some x),
  " <file.ml>  Set output file name to <file> (defaults to <source>.ml)";
  "-g", Arg.String (fun x -> grammar_file := Some x),
  " <file.cmly>  Path of the Menhir compiled grammar to analyse (*.cmly)";
  "-q", Arg.Set Common.quiet_mode,
  " Do not display informational messages";
  "-i", Arg.Set interpret,
  " Start an interpreter to test sentences (do not produce other output)";
  "-n", Arg.Set Common.dry_run,
  " Process input but do not generate any file";
  "-d", Arg.Set Common.dump_parsetree,
  " Dump parsetree";
  "-v",  Arg.Unit print_version_string,
  " Print version and exit";
  "-version",  Arg.Unit print_version_string,
  " Print version and exit";
  "-vnum",  Arg.Unit print_version_num,
  " Print version number and exit";
]

let () = Arg.parse specs (fun name -> source_name := Some name) usage

let maybe_close lout =
  let result = Lazy.is_val lout in
  if result then (
    let lazy (oc, tr) = lout in
    close_out oc;
    Common.close_tracker tr;
  );
  result

(* Specification of a "parser lexer", an .mlyl file *)
type lexer = {
  (* The channel that reads in the file *)
  channel: in_channel;
  (* AST corresponding to the source *)
  def: Syntax.lexer_definition;
}

module Grammar = MenhirSdk.Cmly_read.Read (struct
    let filename = match !grammar_file with
      | Some filename -> filename
      | None ->
        Format.eprintf "No grammar provided (-g), stopping now.\n";
        exit 1
  end)

module Lr1 = Middle.Lr1.Make(Grammar)
module Sigma = Middle.Sigma.Make(Lr1)
module Reduction_graph = Middle.Reduction_graph.Make(Sigma)()
module Regex = Middle.Regex.Make(Sigma)(Reduction_graph)
module TranslR = Transl.Make(Regex)

let initial_states : (Grammar.nonterminal * Grammar.lr1) list =
  Grammar.Lr1.fold begin fun lr1 acc ->
    let lr0 = Grammar.Lr1.lr0 lr1 in
    match Grammar.Lr0.incoming lr0 with
    | Some _ -> acc
    | None ->
      (* Initial state *)
      Format.eprintf "Initial state %d\n%a\n"
        (Grammar.Lr1.to_int lr1)
        Grammar.Print.itemset (Grammar.Lr0.items lr0);
      let (prod, _) = List.hd (Grammar.Lr0.items lr0) in
      let nt = match (Grammar.Production.rhs prod).(0) with
        | Grammar.N nt, _, _ -> nt
        | _ -> assert false
      in
      (nt, lr1) :: acc
  end []

let compiler _out lexer =
  let _, (module DFA) = TranslR.translate lexer.def in
  Printf.printf "let %s =\n"
    (String.concat ", " (List.map (fun e -> e.Syntax.name) lexer.def.entrypoints));
  let st_fun st =
    "st_" ^ string_of_int (Utils.Strong.Finite.Elt.to_int st)
  in
  Utils.Strong.Finite.Set.iter DFA.states (fun st ->
      Printf.printf "  %s %s stack = match current_state stack with\n"
        (if Utils.Strong.Finite.Elt.to_int st = 0 then "let rec" else "and")
        (st_fun st);
      let expr = DFA.represent_state st in
      let action = Regex.Expr.get_label expr in
        let transitions =
          Utils.Misc.group_by ~compare:(fun tr1 tr2 ->
              let tgt1 = DFA.target tr1 and tgt2 = DFA.target tr2 in
              Int.compare
                (Utils.Strong.Finite.Elt.to_int tgt1)
                (Utils.Strong.Finite.Elt.to_int tgt2)
            )
            ~group:(fun tr trs ->
                DFA.target tr,
                List.fold_left
                  (fun sg tr -> Lr1.Set.union sg (DFA.label tr))
                  (DFA.label tr) trs
              )
            (DFA.transitions_from st)
        in
        List.iter (fun (target, sigma) ->
            Printf.printf "    | %s -> %s (next stack)\n"
              (String.concat "|"
                 (List.map
                    (fun lr1 -> string_of_int (lr1 : Grammar.lr1 :> int))
                    (Lr1.Set.elements sigma)))
              (st_fun target)
          ) transitions;
        Printf.printf "    | _ -> failwith \"Match failure\"\n";
      if Utils.BitSet.IntSet.is_empty action.Regex.Action.accept then (
      ) else (
        let index = Utils.BitSet.IntSet.choose action.Regex.Action.accept in
        let entry = List.hd lexer.def.entrypoints in
        let clause = List.nth entry.clauses index in
        match clause.action with
        | None -> failwith "Reached unreachable case!"
        | Some loc ->
          let body = Common.read_location lexer.channel loc in
          print_endline body
      )
    );
  Printf.printf "  %s\n"
    (String.concat ", " (List.map st_fun (Array.to_list DFA.initial)));
  let table = ref [|-1|] in
  let grow_table () =
    let table0 = !table in
    let length = Array.length table0 in
    table := Array.make (length * 2) (-1);
    Array.blit table0 0 !table 0 length
  in
  let add_state st =
    let sigma =
      List.fold_left
        (fun acc tr -> Lr1.Set.union acc (DFA.label tr))
        Lr1.Set.empty (DFA.transitions_from st)
    in
    if not (Lr1.Set.is_empty sigma) then
      let exception Found in
      let base = Lr1.Set.choose sigma in
      let check_symbol offset symbol =
        let index = offset + (symbol : Grammar.lr1 :> int) - (base :> int) in
        if Array.length !table <= index
        then (grow_table (); raise Found)
        else (!table).(index) > -1
      in
      let find_offset () =
        let offset = ref 0 in
        begin try
            while Lr1.Set.exists (check_symbol !offset) sigma
            do incr offset; done
          with Found -> ()
        end;
        !offset
      in
      let commit_offset st offset =
        Lr1.Set.iter (fun symbol ->
            let index = offset + (symbol : Grammar.lr1 :> int) - (base :> int) in
            while Array.length !table <= index do grow_table () done;
            (!table).(index) <- (st : _ Utils.Strong.Finite.elt :> int);
          ) sigma
      in
      commit_offset st (find_offset ())
  in
  Utils.Strong.Finite.Set.iter DFA.states add_state;
  let maxi = ref 0 in
  Array.iteri (fun index n -> if n <> -1 then maxi := index) !table;
  let unoccupied = ref 0 in
  for i = 0 to !maxi - 1 do
    if (!table).(i) = -1 then incr unoccupied;
  done;
  Printf.eprintf "compact table has %d cells, %d unoccupied ones\n" !maxi !unoccupied

let main () =
  let source_name = match !source_name with
    | None ->
      Arg.usage specs usage;
      exit 2
    | Some name -> name
  in
  let dest_name = match !output_name with
    | Some name -> name
    | None ->
      if Filename.check_suffix source_name ".mlyl"
      then Filename.chop_suffix source_name ".mlyl" ^ ".ml"
      else source_name ^ ".ml"
  in
  let ic = open_in_bin source_name in
  let out = lazy (
    let oc = open_out dest_name in
    let tr = Common.open_tracker dest_name oc in
    (oc, tr)
  ) in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.Lexing.lex_curr_p <- {
    Lexing.pos_fname = source_name;
    Lexing.pos_lnum = 1;
    Lexing.pos_bol = 0;
    Lexing.pos_cnum = 0;
  };
  try
    let def = Parser.lexer_definition Lexer.main lexbuf in
    if !Common.dump_parsetree then
      Format.eprintf "Parsetree:\n%!%a"
        Cmon.format (Syntax.print_definition def);
    List.iter (fun entry ->
        List.iter (fun clause ->
            Syntax.check_wellformed clause.Syntax.pattern
          ) entry.Syntax.clauses
      ) def.Syntax.entrypoints;
    let lexer = {channel = ic; def} in
    if !interpret then
      () (*interpreter lexer*)
    else
      compiler out lexer;
    close_in ic;
    ignore (maybe_close out : bool);
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    close_in ic;
    if maybe_close out then
      Sys.remove dest_name;
    begin match exn with
      | Parser.Error ->
        let p = Lexing.lexeme_start_p lexbuf in
        Printf.fprintf stderr
          "File \"%s\", line %d, character %d: syntax error.\n"
          p.Lexing.pos_fname p.Lexing.pos_lnum
          (p.Lexing.pos_cnum - p.Lexing.pos_bol)
      | Lexer.Lexical_error {msg; file; line; col} ->
        Printf.fprintf stderr
          "File \"%s\", line %d, character %d: %s.\n"
          file line col msg
      | Syntax.Illformed {msg; file; line; col} ->
        Printf.fprintf stderr
          "File \"%s\", line %d, character %d: %s.\n"
          file line col msg
      | Transl.Error str ->
        Printf.fprintf stderr "Error during translation: %s.\n" str
      | _ -> Printexc.raise_with_backtrace exn bt
    end;
    exit 3

let () =
  main ();
  exit 0
