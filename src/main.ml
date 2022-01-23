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

open Utils
open BitSet
open Strong

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
module Transl = Translate.Make(Regex)

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

let compiler_raw lexer (entries, dfa)
  =
  Printf.printf "let %s =\n"
    (String.concat ", " (List.map (fun e -> e.Syntax.name) lexer.def.entrypoints));
  let numbers =
    let k = ref 0 in
    Regex.Map.map (fun _ -> let result = !k in incr k; result) dfa
  in
  let st_fun st = "st_" ^ string_of_int (Regex.Map.find st numbers) in
  let first = ref true in
  Regex.Map.iter begin fun st transitions ->
    Printf.printf "  %s %s stack =\n"
      (if !first then (first := false; "let rec") else "and")
      (st_fun st);
    let action = Regex.Expr.get_label st in
    begin match IntSet.minimum action.accept with
      | None -> ()
      | Some elt -> Printf.printf "    %d ::\n" elt
    end;
    Printf.printf "    match current_state stack with\n";
    let transitions =
      Misc.group_by
        ~compare:(fun (_,_,e1) (_,_,e2) -> Regex.Expr.compare e1 e2)
        ~group:(fun (sg,_,e) trs ->
            e,
            List.fold_left
              (fun sg (sg',_,_) -> Lr1.Set.union sg (Sigma.to_lr1set sg'))
              (Sigma.to_lr1set sg) trs
          )
        transitions
    in
    List.iter (fun (target, sigma) ->
        match st_fun target with
        | exception Not_found -> ()
        | st_fun ->
        Printf.printf "    | %s -> %s (next stack)\n"
          (String.concat "|"
             (List.map
                (fun lr1 -> string_of_int (lr1 : Grammar.lr1 :> int))
                (Lr1.Set.elements sigma)))
          st_fun
      ) transitions;
    Printf.printf "    | _ -> []\n";
  end dfa;
  Printf.printf "  in %s\n"
    (String.concat ", " (List.map st_fun entries))

let compiler_min
    _out lexer
    (module DFA: Middle.Intf.MINIMIZED_DFA with type regex = Regex.Expr.t and type sigma = Lr1.Set.t)
  =
  Printf.printf "let %s =\n"
    (String.concat ", " (List.map (fun e -> e.Syntax.name) lexer.def.entrypoints));
  let st_fun st =
    "st_" ^ string_of_int (Finite.Elt.to_int st)
  in
  Finite.Set.iter DFA.states (fun st ->
      Printf.printf "  %s %s stack =\n"
        (if Finite.Elt.to_int st = 0 then "let rec" else "and")
        (st_fun st);
      let expr = DFA.represent_state st in
      let action = Regex.Expr.get_label expr in
      begin match IntSet.minimum action.accept with
        | None -> ()
        | Some elt -> Printf.printf "    %d ::\n" elt
      end;
      Printf.printf "    match current_state stack with\n";
      let transitions =
        Misc.group_by ~compare:(fun tr1 tr2 ->
            let tgt1 = DFA.target tr1 and tgt2 = DFA.target tr2 in
            Int.compare
              (Finite.Elt.to_int tgt1)
              (Finite.Elt.to_int tgt2)
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
      Printf.printf "    | _ -> []\n";
      (*if IntSet.is_empty action.Regex.Action.accept then (
      ) else (
        let index = IntSet.choose action.Regex.Action.accept in
        let entry = List.hd lexer.def.entrypoints in
        let clause = List.nth entry.clauses index in
        match clause.action with
        | None -> failwith "Reached unreachable case!"
        | Some loc ->
          let body = Common.read_location lexer.channel loc in
          print_endline body
      )*)
    );
  Printf.printf "  in %s\n"
    (String.concat ", " (List.map st_fun (Array.to_list DFA.initial)))

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
      Format.eprintf "Parsetree:\n%a\n%!"
        Cmon.format (Syntax.print_definition def);
    List.iter (fun entry ->
        List.iter (fun clause ->
            Syntax.check_wellformed clause.Syntax.pattern
          ) entry.Syntax.clauses
      ) def.Syntax.entrypoints;
    let lexer = {channel = ic; def} in
    Format.printf "module Make(Stack : sig\n\
                  \  type t\n\
                  \  val current_state : t -> int\n\
                  \  val next : t -> t\n\
                   end) : sig\n\
                  \  val error_message : Stack.t -> int list\n\
                  \  val messages : int -> string option\n\
                   end = struct\n\
                  \  open Stack\n\
                  ";
    let dfa_raw, dfa_min = Transl.translate lexer.def in
    compiler_raw lexer dfa_raw;
    compiler_min out lexer dfa_min;
    let entry = List.hd lexer.def.entrypoints in
    Format.printf "let messages = function\n";
    List.iteri (fun index clause ->
        match clause.Syntax.action with
        | None -> Format.printf "  | %d -> None\n" index
        | Some action ->
          Format.printf "  | %d -> Some %S\n"
            index (Common.read_location ic action)
      ) entry.clauses;
    Format.printf "  | _ -> failwith \"Unknown action\"\n\
                   end\n%!";
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
      | Translate.Error str ->
        Printf.fprintf stderr "Error during translation: %s.\n" str
      | _ -> Printexc.raise_with_backtrace exn bt
    end;
    exit 3

let () =
  main ();
  exit 0
