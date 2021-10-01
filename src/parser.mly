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
%token UNREACHABLE
%token RULE        "rule"
       PARSE       "parse"
       ERROR       "error"
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
       LEFT_ARROW  "<-"
       COMMA       ","
       SEMI        ";"
       COLON       ":"
       (*AS         "as"*)
       (*HASH       "#"*)

(*%right "as"*)
%left "|"
%nonassoc "?" "*" (*"+"*)
(*%left "#"*)
(*%nonassoc IDENT "_" "[" "("*)
%left "<-" ";"

%start <Syntax.lexer_definition> lexer_definition
%start <Syntax.prompt_sentence> prompt_sentence

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
| name=IDENT args=IDENT* "=" "parse" startsymbols=IDENT* error=boption("error")
    "|" clauses=separated_list("|", case)
  { {startsymbols; error; name; args; clauses} }
;

case:
| regexp ACTION
  { {pattern = $1; action = Some $2} }
| regexp UNREACHABLE
  { {pattern = $1; action = None} }
;

symbol:
| IDENT
  { Name $1 }
| IDENT "(" separated_list(",", symbol) ")"
  { Apply ($1, $3) }
;

wild_symbol:
| "_" { None }
| symbol { Some $1 }
;

regterm:
| "_"
  { Wildcard }
| symbol
  { Symbol $1 }
| "[" prefix=wild_symbol* "." suffix=wild_symbol* "]"
  { Item {lhs=None; prefix; suffix} }
| "[" lhs=symbol ":" prefix=wild_symbol* "." suffix=wild_symbol* "]"
  { Item {lhs=Some lhs; prefix; suffix} }
| regexp "*"
  { Repetition ($1, make_position $startpos($2)) }
| regexp "?"
  { Alternative ([], $1) }
| regexp "|" regexp
  { Alternative ($1, $3) }

regexp:
| regterm
  { [$1, make_position $endpos] }
| "(" regexp ")"
  { $2 }
| regexp ioption(";") "<-"
  { [Reduce ($1, make_location $startpos($1) $endpos($1)),
     make_position $startpos($3)] }
| regexp ioption(";") "<-" regexp
  { (Reduce ($1, make_location $startpos($1) $endpos($1)),
     make_position $startpos($3)) :: $4 }
| regexp ";" regexp
  { $1 @ $3 }
;

prompt_sentence:
| symbol* DOT
| symbol* EOF
  { Syntax.Prompt_interpret $1 }
| symbol ":"
  { Syntax.Prompt_entrypoint $1 }
;

%%
