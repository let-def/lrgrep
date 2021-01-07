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
%token RULE        "rule"
       PARSE       "parse"
       AND         "and"
       EQUAL       "="
       EOF
       BAR         "|"
       UNDERSCORE  "_"
       LBRACKET    "["
       RBRACKET    "]"
       STAR        "*"
       QUESTION    "?"
       (*PLUS       "+"*)
       LPAREN      "("
       RPAREN      ")"
       DOT         "."
       LEFT_ARROW "<-"
       RIGHT_ARROW  "->"
       (*AS         "as"*)
       (*HASH       "#"*)

(*%right "as"*)
%left "|"
%nonassoc prec_CONCAT
%nonassoc "<-" "?" "*" (*"+"*)
(*%left "#"*)
%nonassoc IDENT "_" "[" "("

%start lexer_definition
%type <Syntax.lexer_definition> lexer_definition

%%

lexer_definition:
| header "rule" separated_list("and", definition) header EOF
  { { header = $1; entrypoints = $3; trailer = $4 } }
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

definition:
| name=IDENT args=IDENT* "=" "parse"
    "|"? clauses=separated_list("|", case)
  { {name; args; clauses} }
;

case:
| regexp RIGHT_ARROW ACTION
  { ($1, Some $3) }
| regexp RIGHT_ARROW DOT
  { ($1, None) }
;

regexp:
| "_"
  { Wildcard }
| IDENT
  { Symbol $1 }
| "[" prefix=IDENT* "." suffix=IDENT* "]"
  { Item {lhs=None; prefix; suffix} }
| regexp "*"
  { Repetition $1 }
| regexp "?"
  { Alternative (Epsilon, $1) }
| regexp "<-"
  { Reduce $1 }
| regexp "|" regexp
  { Alternative ($1,$3) }
| regexp regexp %prec prec_CONCAT
  { Sequence ($1,$2) }
| "(" regexp ")"
  { $2 }
;

%%
