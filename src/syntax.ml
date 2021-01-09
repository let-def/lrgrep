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

type symbol =
  | Name of string
  | Apply of string * symbol list

type regular_term =
  | Symbol of symbol
  | Item of {
      lhs: symbol option;
      prefix: symbol list;
      suffix: symbol list;
    }
  | Wildcard
  | Alternative of regular_expression * regular_expression
  | Repetition of regular_expression
  | Reduce of regular_expression

and regular_expression =
  regular_term list

type clause = {
  pattern: regular_expression;
  action: location option;
}

type entry = {
  name    : string;
  args    : string list;
  clauses : clause list;
}

type lexer_definition = {
  header      : location;
  entrypoints : entry list;
  trailer     : location;
}

open Utils

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

let print_option f = function
  | None -> Cmon.constant "None"
  | Some x -> Cmon.constructor "Some" (f x)

let rec print_symbol = function
  | Name sym -> Cmon.constructor "Name" (Cmon.string sym)
  | Apply (sym, args) -> Cmon.ctuple "Apply" [
      Cmon.string sym;
      Cmon.list (List.map print_symbol args);
    ]

let rec print_regular_term = function
  | Symbol sym -> Cmon.constructor "Symbol" (print_symbol sym)
  | Item {lhs; prefix; suffix} ->
    Cmon.crecord "Item" [
      "lhs", print_option print_symbol lhs;
      "prefix", Cmon.list (List.map print_symbol prefix);
      "suffix", Cmon.list (List.map print_symbol suffix);
    ]
  | Wildcard -> Cmon.constant "Wildcard"
  | Alternative (re1, re2) ->
    Cmon.ctuple "Alternative" [
      print_regular_expression re1;
      print_regular_expression re2;
    ]
  | Repetition re ->
    Cmon.constructor "Repetition"
      (print_regular_expression re)
  | Reduce re ->
    Cmon.constructor "Reduce"
      (print_regular_expression re)

and print_regular_expression re =
  Cmon.list (List.map print_regular_term re)

let print_clause {pattern; action} =
  Cmon.record [
    "pattern", print_regular_expression pattern;
    "action", print_option print_location action;
  ]

let print_entrypoints {name; args; clauses} =
  Cmon.record [
    "name", Cmon.string name;
    "args", Cmon.list (List.map Cmon.string args);
    "clauses", Cmon.list (List.map print_clause clauses);
  ]

let print_definition {header; entrypoints; trailer} : Cmon.t =
  Cmon.record [
    "header", print_location header;
    "entrypoints", Cmon.list (List.map print_entrypoints entrypoints);
    "trailer", print_location trailer;
  ]
