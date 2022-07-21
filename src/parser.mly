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
let mk_re desc pos = {desc; position = make_position pos}
%}

%token <string> IDENT
%token <Syntax.ocaml_code> ACTION
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
       BANG        "!"
       COMMA       ","
       SEMI        ";"
       COLON       ":"
       AS          "as"
       PARTIAL     "partial"
       (*HASH       "#"*)

%start <Syntax.lexer_definition> lexer_definition
%start <Syntax.prompt_sentence> prompt_sentence

%%

lexer_definition:
| header "rule" separated_list("and", definition) header EOF
  { {header=$1; entrypoints=$3; trailer=$4} }
;

header:
| ACTION { $1 }
| (*empty*)
  { ({loc_file=""; start_pos=0; end_pos=0; start_line=1; start_col=0}, "") }
;

definition:
| name=IDENT args=IDENT* "=" "parse" startsymbols=IDENT* error=boption("error")
    "|" clauses=cases
  { {startsymbols; error; name; args; clauses} }
;

cases:
| case { [$1] }
| case "|" cases { $1 :: $3}
;

case:
| regexp ACTION           { {pattern = $1; action = Total $2} }
| regexp "partial" ACTION { {pattern = $1; action = Partial $3} }
| regexp UNREACHABLE      { {pattern = $1; action = Unreachable} }
;

symbol:
| IDENT                                     { Name $1 }
| IDENT "(" separated_list(",", symbol) ")" { Apply ($1, $3) }
;

wild_symbol:
| "_"    { None }
| symbol { Some $1 }
;

atom:
| "_"    { Wildcard }
| symbol { Symbol $1 }
| "[" lhs=item_lhs prefix=wild_symbol* "." suffix=wild_symbol* "]"
  { Item {lhs; prefix; suffix} }
;

%inline item_lhs:
| (*empty*)  { None }
| symbol ":" { Some $1 }
;

regleaf:
| "(" regexp ")"             { $2 }
| atom preceded("as",IDENT)? { mk_re (Atom ($1, $2)) $endpos }
;

regterm:
| regleaf     { $1 }
| regleaf "*" { mk_re (Repetition $1) $endpos }
| regleaf "?" { mk_re (Alternative [$1; mk_re (Concat []) $endpos]) $endpos }
| "!"         { mk_re Reduce $endpos }
;

regseq_loop:
| regterm                 { [$1] }
| regseq_loop ";" regterm { $3 :: $1 }
;

regseq:
| (*empty*) { mk_re (Concat []) $endpos }
| regseq_loop { match $1 with [x] -> x | xs  -> mk_re (Concat xs) $endpos }
;

regsum_loop:
| regseq                 { [$1] }
| regseq "|" regsum_loop { $1 :: $3 }
;

regexp:
| regsum_loop { match $1 with [x] -> x | xs -> mk_re (Alternative xs) $endpos }
;

prompt_sentence:
| symbol* DOT
| symbol* EOF
  { Syntax.Prompt_interpret $1 }
| symbol ":"
  { Syntax.Prompt_entrypoint $1 }
;

%%
