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

type regular_expression =
  | Epsilon
  | Symbol of string
  | Item of {
      lhs: string option;
      prefix: string list;
      suffix: string list;
    }
  | Wildcard
  | Sequence of regular_expression * regular_expression
  | Alternative of regular_expression * regular_expression
  | Repetition of regular_expression
  | Reduce of regular_expression

type ('arg,'action) entry = {
  name    : string;
  args    : 'arg;
  clauses : (regular_expression * 'action) list;
}

type lexer_definition = {
  header      : location;
  entrypoints : ((string list, location option) entry) list;
  trailer     : location;
}

let print_definition {header; entrypoints; trailer} =
