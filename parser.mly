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

(* The grammar for lexer definitions *)

%{
open Syntax
%}

%token <string> IDENT
%token <Syntax.location> ACTION
%token RULE       "rule"
       PARSE      "parse"
       AND        "and"
       EQUAL      "="
       END        "end"
       BAR        "|"
       UNDERSCORE "_"
       EOF        "eof"
       LBRACKET   "["
       RBRACKET   "]"
       STAR       "*"
       MAYBE      "?"
       (*PLUS       "+"*)
       LPAREN     "("
       RPAREN     ")"
       CARET      "^"
       DASH       "-"
       LET        "let"
       DOT        "."
       (*AS         "as"*)
       (*HASH       "#"*)

(*%right "as"*)
%left "|"
%nonassoc prec_CONCAT
%nonassoc "?" "*" (*"+"*)
(*%left "#"*)
%nonassoc IDENT "_" "eof" "[" "("

%start lexer_definition
%type <Syntax.lexer_definition> lexer_definition

%%

lexer_definition:
| header=header "rule" definition other_definitions trailer=header "end"
  { {header; entrypoints = $3 :: List.rev $4; trailer } }
;

header:
| ACTION
  { $1 }
| (*epsilon*)
  { { loc_file = "";
      start_pos = 0;
      end_pos = 0;
      start_line = 1;
      start_col = 0;
    }
  }
;

other_definitions:
| other_definitions "and" definition
  { $3::$1 }
| (*epsilon*)
  { [] }
;

definition:
| name=IDENT args=IDENT* "=" "parse" "|"? clauses=separated_list("|", case)
  { {name; args; clauses} }
;

case:
| regexp ACTION
  { ($1,$2) }
;

regexp:
| "_"
  { Wildcard }
| "eof"
  { Eof }
| IDENT
  { Symbol $1 }
| "[" prefix=IDENT* "." suffix=IDENT* "]"
  { Item (prefix, suffix) }
| regexp "*"
  { Repetition $1 }
| regexp "?"
  { Alternative (Epsilon, $1) }
(*| regexp "+"
  { Sequence (Repetition (remove_as $1), $1) }*)
(*| regexp "#" regexp
  {
    let s1 = as_cset $1
    and s2 = as_cset $3 in
    Characters (Cset.diff s1 s2)
  }*)
| regexp "|" regexp
  { Alternative ($1,$3) }
| regexp regexp %prec prec_CONCAT
  { Sequence ($1,$2) }
| "(" regexp ")"
  { $2 }
(*| regexp "as" ident
  { let p1 = Parsing.rhs_start_pos 3
    and p2 = Parsing.rhs_end_pos 3 in
    let p = {
      loc_file = p1.Lexing.pos_fname;
      start_pos = p1.Lexing.pos_cnum;
      end_pos = p2.Lexing.pos_cnum;
      start_line = p1.Lexing.pos_lnum;
      start_col = p1.Lexing.pos_cnum - p1.Lexing.pos_bol;
    } in
    Bind ($1, ($3, p))
  }*)
;

%%
