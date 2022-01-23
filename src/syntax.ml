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

(* The shallow abstract syntax *)

type location = {
  loc_file : string;
  start_pos : int;
  end_pos : int;
  start_line : int;
  start_col : int;
}

type position = {line: int; col: int}

type symbol =
  | Name of string
  | Apply of string * symbol list

type regular_term =
  | Symbol of {
      symbol: symbol;
      reduce: bool;
    }
  | Item of {
      lhs: symbol option;
      prefix: symbol option list;
      suffix: symbol option list;
    }
  | Wildcard
  | Alternative of regular_expression * regular_expression
  | Repetition of regular_expression * position

and regular_expression =
  (regular_term * position) list

type clause = {
  pattern: regular_expression;
  action: location option;
}

type entry = {
  startsymbols: string list;
  error   : bool;
  name    : string;
  args    : string list;
  clauses : clause list;
}

type lexer_definition = {
  header      : location;
  entrypoints : entry list;
  trailer     : location;
}

let make_position {Lexing. pos_lnum; pos_cnum; pos_bol; _} =
  {line = pos_lnum; col = pos_cnum - pos_bol + 1}

let make_location startpos endpos = {
  loc_file = startpos.Lexing.pos_fname;
  start_line = startpos.Lexing.pos_lnum;
  start_pos = startpos.Lexing.pos_cnum;
  start_col =  startpos.Lexing.pos_cnum - startpos.Lexing.pos_bol + 1;
  end_pos = endpos.Lexing.pos_cnum;
}

let print_location {
    loc_file;
    start_pos;
    end_pos;
    start_line;
    start_col;
  } = Cmon.(record [
    "loc_file"  , string loc_file;
    "start_pos" , int start_pos;
    "end_pos"   , int end_pos;
    "start_line", int start_line;
    "start_col" , int start_col;
  ])

let print_position {line; col} =
  Cmon.(record ["line", int line; "col", int col])

let print_option f = function
  | None -> Cmon.constant "None"
  | Some x -> Cmon.constructor "Some" (f x)

let rec print_symbol = function
  | Name sym -> Cmon.constructor "Name" (Cmon.string sym)
  | Apply (sym, args) -> Cmon.construct "Apply" [
      Cmon.string sym;
      Cmon.list (List.map print_symbol args);
    ]

let rec print_regular_term = function
  | Symbol {symbol; reduce} ->
    Cmon.crecord "Symbol" [
      "symbol", print_symbol symbol;
      "reduce", Cmon.bool reduce;
    ]
  | Item {lhs; prefix; suffix} ->
    Cmon.crecord "Item" [
      "lhs", print_option print_symbol lhs;
      "prefix", Cmon.list (List.map (print_option print_symbol) prefix);
      "suffix", Cmon.list (List.map (print_option print_symbol) suffix);
    ]
  | Wildcard -> Cmon.constant "Wildcard"
  | Alternative (re1, re2) ->
    Cmon.construct "Alternative" [
      print_regular_expression re1;
      print_regular_expression re2;
    ]
  | Repetition (re, pos) ->
    Cmon.construct "Repetition" [
      print_regular_expression re;
      print_position pos;
    ]

and print_regular_expression re =
  let print_regular_term (term, pos) =
    Cmon.tuple [
      print_regular_term term;
      print_position pos;
    ]
  in
  Cmon.list (List.map print_regular_term re)

let print_clause {pattern; action} =
  Cmon.record [
    "pattern", print_regular_expression pattern;
    "action", print_option print_location action;
  ]

let print_entrypoints {error; startsymbols; name; args; clauses} =
  Cmon.record [
    "startsymbols", Cmon.list_map Cmon.string startsymbols;
    "error", Cmon.bool error;
    "name", Cmon.string name;
    "args", Cmon.list_map Cmon.string args;
    "clauses", Cmon.list_map print_clause clauses;
  ]

let print_definition {header; entrypoints; trailer} : Cmon.t =
  Cmon.record [
    "header", print_location header;
    "entrypoints", Cmon.list (List.map print_entrypoints entrypoints);
    "trailer", print_location trailer;
  ]

type prompt_sentence =
  | Prompt_interpret of symbol list
  | Prompt_entrypoint of symbol
