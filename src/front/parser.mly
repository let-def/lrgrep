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
open Kernel.Syntax
let mk_re desc position = {desc; position}
%}

%token <string> IDENT
%token <Kernel.Syntax.ocaml_code> ACTION
%token UNREACHABLE
%token RULE        "rule"
       PARSE       "parse"
       ERROR       "error"
       EQUAL       "="
       BAR         "|"
       UNDERSCORE  "_"
       LBRACKET    "["
       RBRACKET    "]"
       STAR        "*"
       QUESTION    "?"
       LPAREN      "("
       RPAREN      ")"
       DOT         "."
       COMMA       ","
       SEMI        ";"
       COLON       ":"
       PARTIAL     "%partial"
       SLASH       "/"
       AT          "@"
       EOF

%start <Kernel.Syntax.lexer_definition> lexer_definition

%%

lexer_definition:
| header rule+ header EOF
  { {header=$1; rules=$2; trailer=$3} }
;

ident:
| IDENT { $1      }
| RULE  { "rule"  }
| PARSE { "parse" }
| ERROR { "error" }
;

header:
| ACTION { $1 }
| (*empty*)
  { (Lexing.dummy_pos, "") }
;

startsymbols:
| (*empty*) { [] }
| "(" separated_nonempty_list(",", positioned(ident)) ")" { $2 }
;

rule:
| "rule" name=ident args=ident* "=" "parse" error=positioned(boption("error")) startsymbols=startsymbols
  clauses=clause*
  { {startsymbols; error; name; args; clauses} }
;

case_patterns:
| regexp { [{expr=$1; lookaheads=[]}] }
| regexp lookaheads { [{ expr=$1; lookaheads=$2 }] }
| regexp lookaheads "|" case_patterns { { expr=$1; lookaheads=$2 } :: $4 }
;

clause:
| "|" case_patterns case_action { {patterns=$2; action=$3} }
;

case_action:
| ACTION            { Total $1    }
| "%partial" ACTION { Partial $2  }
| UNREACHABLE       { Unreachable }


positioned(X):
| X { ($1, $startpos) }
;

lookaheads:
| preceded("@", separated_list(",", positioned(symbol))) { $1 }
;

symbol:
| ident                                     { Name $1 }
| ident "(" separated_list(",", symbol) ")" { Apply ($1, $3) }
;

wild_symbol:
| "_"    { None }
| symbol { Some $1 }
;

%inline capture:
| ioption(terminated(ident,"=")) { $1 }
;

regleaf:
| "(" regexp ")" { $2 }
| capture wild_symbol { mk_re (Atom ($1, $2, Utils.Usage.new_mark ())) $startpos }
| capture "[" regexp "]"
  { let mark, policy, expr = match $3.desc with
        | Reduce {capture=None; mark; policy=Shortest; expr} ->
           (mark, Longest, expr)
        | _ -> (Utils.Usage.new_mark (), Shortest, $3)
    in
    mk_re (Reduce {capture=$1; mark; expr; policy}) $startpos
  }
;

regterm:
| regleaf         { $1 }
| regleaf "*"     { mk_re (Repetition {expr=$1; policy=Shortest}) $startpos }
| regleaf "*" "*" { mk_re (Repetition {expr=$1; policy=Longest}) $startpos }
| regleaf "?"     { mk_re (Alternative [$1; mk_re (Concat []) $startpos]) $startpos }
;

filter_symbol:
| "."         { Dot }
| "_" "*"     { Skip }
| wild_symbol { Find $1 }
;

filter:
| "/" ioption(terminated(symbol, ":")) positioned(filter_symbol)+
  { mk_re (Filter {lhs = $2; rhs = $3}) $startpos }
;

regseq_loop:
| regseq_loop filter      { $2 :: $1 }
| regterm                 { [$1] }
| filter                  { [$1] }
| regseq_loop ";" regterm { $3 :: $1 }
;

regseq:
| (*empty*)   { mk_re (Concat []) $startpos }
| regseq_loop { match $1 with [x] -> x | xs  -> mk_re (Concat (List.rev xs)) $startpos }
;

regsum_loop:
| regseq                 { [$1] }
| regseq "|" regsum_loop { $1 :: $3 }
;

regexp:
| regsum_loop { match $1 with [x] -> x | xs -> mk_re (Alternative xs) $startpos }
;

%%
