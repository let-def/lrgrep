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
  " <file>  Set output file name to <file>";
  "-q", Arg.Set Common.quiet_mode,
  " Do not display informational messages";
  "-v",  Arg.Unit print_version_string,
  " Print version and exit";
  "-version",  Arg.Unit print_version_string,
  " Print version and exit";
  "-vnum",  Arg.Unit print_version_num,
  " Print version number and exit";
]

let () = Arg.parse specs (fun name -> source_name := Some name) usage

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
      if Filename.check_suffix source_name ".mll"
      then Filename.chop_suffix source_name ".mll" ^ ".ml"
      else source_name ^ ".ml"
  in
  let ic = open_in_bin source_name in
  let oc = open_out dest_name in
  let tr = Common.open_tracker dest_name oc in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.Lexing.lex_curr_p <-
    {Lexing.pos_fname = source_name; Lexing.pos_lnum = 1;
     Lexing.pos_bol = 0; Lexing.pos_cnum = 0};
  try
    let def = Parser.lexer_definition Lexer.main lexbuf in
    Format.printf "%a" Utils.Cmon.format (Syntax.print_definition def);
    (*let (entries, transitions) = Lexgen.make_dfa def.entrypoints in
    if !ml_automata then begin
      Outputbis.output_lexdef
        ic oc tr
        def.header def.refill_handler entries transitions def.trailer
    end else begin
       let tables = Compact.compact_tables transitions in
       Output.output_lexdef ic oc tr
         def.header def.refill_handler tables entries def.trailer
    end;*)
    close_in ic;
    close_out oc;
    Common.close_tracker tr;
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    close_in ic;
    close_out oc;
    Common.close_tracker tr;
    Sys.remove dest_name;
    begin match exn with
      | Parser.Error ->
        let p = Lexing.lexeme_start_p lexbuf in
        Printf.fprintf stderr
          "File \"%s\", line %d, character %d: syntax error.\n"
          p.Lexing.pos_fname p.Lexing.pos_lnum
          (p.Lexing.pos_cnum - p.Lexing.pos_bol)
      | Lexer.Lexical_error (msg, file, line, col) ->
        Printf.fprintf stderr
          "File \"%s\", line %d, character %d: %s.\n"
          file line col msg
      | _ -> Printexc.raise_with_backtrace exn bt
    end;
    exit 3

let () =
  main ();
  exit 0
