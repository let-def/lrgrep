(* MIT License

   Copyright (c) 2025 Frédéric Bour

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
*)

(** This module defines the shallow abstract syntax for an error specification
    file (.mlyl). It includes types for representing locations, symbols, regular
    expressions, semantic actions, and the overall structure of a lexer
    definition. *)
open Utils

(** {1 The shallow abstract syntax} *)

type location = Code_printer.location = {
  loc_file : string;
  start_pos : int;
  end_pos : int;
  start_line : int;
  start_col : int;
}

(** Kind of quantifier used in regular expressions. *)
type quantifier_kind =
  | Longest  (* aka "greedy" *)
  | Shortest (* aka "lazy" *)

(** This represents a piece of OCaml code, as appearing in semantic
    actions, as well as in the header and trailer. *)
type ocaml_code = location * string

(** A position in the input file. *)
type position = {line: int; col: int}

let invalid_position = {line = -1; col = -1}

(** A grammar symbol (a terminal or a non-terminal) *)
type symbol =
  | Name of string
  (** Symbols are usually simple names, like 'a' or 'X'. *)
  | Apply of string * symbol list
  (** Menhir supports higher-order non-terminals. In this case, a symbol is
      the application of the higher-order non-terminal to some arguments.
      E.g separated_list(sep, X) is represented as:
        [Apply ("separated_list", [Name "sep"; Name "X"])] *)

(** A wildcard symbol is either a symbol or '_'. *)
type wild_symbol = symbol option

(** Symbols used in filter globbing expressions to specify matching criteria. *)
type filter_symbol =
  | Skip
  | Find of wild_symbol
  | Dot

(** [regular_desc] describes the different cases of the regular expression
    syntax. *)

type regular_desc =
  | Atom of string option * wild_symbol * Usage.mark
  | Alternative of regular_expr list
  (** A disjunction of multiple expressions.
      [e1 | e2 | e3] is represented as [Alternative [e1; e2; e3]] *)
  | Repetition of {
      expr: regular_expr;
      policy: quantifier_kind;
    }
  (** [Repetition e] represents [e*] *)
  | Reduce of {
      capture: string option;
      mark: Usage.mark;
      expr: regular_expr;
      policy: quantifier_kind;
    }
  (** [Reduce] represents the [!] operator *)
  | Concat of regular_expr list
  (** [Concat [e1; e2; ..]] is [e1; e2; ...] *)
  | Filter of {lhs: wild_symbol; rhs: (filter_symbol * position) list}

(** [regular_expr] adds position information to [regular_desc] for error
    reporting purposes. *)
and regular_expr = {
  desc: regular_desc;
  position: position; (** the position where this term ends *)
}

(** The semantic action associated to a pattern *)
type clause_action =
  | Total of ocaml_code   (** ... { code }, normal semantic action **)
  | Partial of ocaml_code (** ... partial { ... }, a semantic action that can
                              return [None] to continue matching *)
  | Unreachable           (** [... { . }] the pattern should never match *)

(** A pattern is a combination of a regular expression and an optional list
    of lookahead constraints. *)
type pattern = {
  expr: regular_expr; (** the pattern *)
  lookaheads: (symbol * position) list; (** restrict matching to these lookahead terminals, or [] for all terminals *)
}

(** A clause is a pair of a pattern and an action, representing one rule. *)
type clause = {
  patterns: pattern list;
  action: clause_action; (** the semantic action *)
}

(** An .mlyl file can contain multiple entrypoints, each represented as an
    instance of [entry]. *)
type entry = {
  name    : string;
  (** Name of this entry *)
  error   : bool * position;
  (** [error] is true if this entry only matches failing stacks.
      Syntactically, an error entry has the form:
        rule x ... = parse error
        | ...
  *)
  startsymbols: (string * position) list;
  (** The list of entrypoints to support, or [] for all entrypoints. *)
  args    : string list;
  (** The list of OCaml arguments to abstract over,
      e.g the [x y] in [rule foo x y = ...] *)
  clauses : clause list;
  (** The list of clauses to match *)
}

(** An .mlyl file is an header containing some OCaml code, one or more entries,
    and a trailer with some other OCaml code. *)
type lexer_definition = {
  header      : ocaml_code;
  entrypoints : entry list;
  trailer     : ocaml_code;
}

(** {1 Helper and cmoning functions} *)

(** Create a position from a Lexing position. *)
let make_position {Lexing. pos_lnum; pos_cnum; pos_bol; _} =
  {line = pos_lnum; col = pos_cnum - pos_bol + 1}

(** Create a location from start and end positions. *)
let make_location startpos endpos = {
  loc_file = startpos.Lexing.pos_fname;
  start_line = startpos.Lexing.pos_lnum;
  start_pos = startpos.Lexing.pos_cnum;
  start_col =  startpos.Lexing.pos_cnum - startpos.Lexing.pos_bol + 1;
  end_pos = endpos.Lexing.pos_cnum;
}

(** Convert a location to a Cmon record. *)
let cmon_location {
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

(** Convert an OCaml code to a Cmon tuple. *)
let cmon_ocamlcode (location, code) =
  Cmon.tuple [
    cmon_location location;
    Cmon.string code;
  ]

(** Convert a position to a Cmon record. *)
let cmon_position {line; col} =
  Cmon.(record ["line", int line; "col", int col])

(** Convert a function and a position to a Cmon pair. *)
let cmon_positioned f =
  Utils.Misc.cmon_pair f cmon_position

(** Convert an option to a Cmon value. *)
let cmon_option f = function
  | None -> Cmon.constant "None"
  | Some x -> Cmon.constructor "Some" (f x)

(** Convert a symbol to a Cmon value. *)
let rec cmon_symbol = function
  | Name sym -> Cmon.constructor "Name" (Cmon.string sym)
  | Apply (sym, args) -> Cmon.construct "Apply" [
      Cmon.string sym;
      Cmon.list (List.map cmon_symbol args);
    ]

(** Convert a capture to a Cmon value. *)
let cmon_capture cap =
  cmon_option Cmon.string cap

(** Convert a wildcard symbol to a Cmon value. *)
let cmon_wild_symbol sym =
  cmon_option cmon_symbol sym

(** Convert a filter symbol to a Cmon value. *)
let cmon_filter_symbol = function
  | Skip -> Cmon.constant "Skip"
  | Dot -> Cmon.constant "Dot"
  | Find sym -> cmon_wild_symbol sym

let cmon_quantifier_kind = function
  | Longest -> Cmon.constant "Longest"
  | Shortest -> Cmon.constant "Shortest"

let cmon_usage_mark _ = Cmon.constant "<Usage.mark>"

let rec cmon_regular_term = function
  | Atom (cap, sym, mark) ->
    Cmon.construct "Atom" [cmon_capture cap; cmon_wild_symbol sym; cmon_usage_mark mark]
  | Alternative res ->
    Cmon.constructor "Alternative" (Cmon.list_map cmon_regular_expression res)
  | Concat res ->
    Cmon.constructor "Concat" (Cmon.list_map cmon_regular_expression res)
  | Repetition {policy; expr} ->
    Cmon.crecord "Repetition" [
      "expr", cmon_regular_expression expr;
      "policy", cmon_quantifier_kind policy;
    ]
  | Reduce {capture; mark; policy; expr} ->
    Cmon.crecord "Reduce" [
      "capture", cmon_capture capture;
      "mark", cmon_usage_mark mark;
      "expr", cmon_regular_expression expr;
      "policy", cmon_quantifier_kind policy;
    ]
  | Filter {lhs; rhs} ->
    Cmon.crecord "Filter" [
      "lhs", cmon_wild_symbol lhs;
      "rhs", Cmon.list_map (cmon_positioned cmon_filter_symbol) rhs;
    ]

(** Convert a regular expression to a Cmon value. *)
and cmon_regular_expression re =
  Cmon.record [
    "desc", cmon_regular_term re.desc;
    "position", cmon_position re.position;
  ]

(** Convert a clause action to a Cmon value. *)
let cmon_clause_action = function
  | Unreachable -> Cmon.constant "Unreachable"
  | Total code -> Cmon.constructor "Total" (cmon_ocamlcode code)
  | Partial code -> Cmon.constructor "Partial" (cmon_ocamlcode code)

(** Convert a pattern to a Cmon value. *)
let cmon_pattern {expr; lookaheads} =
  Cmon.record [
    "expr", cmon_regular_expression expr;
    "lookaheads", Cmon.list_map (cmon_positioned cmon_symbol) lookaheads;
  ]

(** Convert a clause to a Cmon value. *)
let cmon_clause {patterns; action} =
  Cmon.record [
    "patterns", Cmon.list_map cmon_pattern patterns;
    "action", cmon_clause_action action;
  ]

(** Convert an entry to a Cmon value. *)
let cmon_entrypoints {error; startsymbols; name; args; clauses} =
  Cmon.record [
    "startsymbols", Cmon.list_map (cmon_positioned Cmon.string) startsymbols;
    "error", cmon_positioned Cmon.bool error;
    "name", Cmon.string name;
    "args", Cmon.list_map Cmon.string args;
    "clauses", Cmon.list_map cmon_clause clauses;
  ]

(** Convert a lexer definition to a Cmon value. *)
let cmon_definition {header; entrypoints; trailer} : Cmon.t =
  Cmon.record [
    "header", cmon_ocamlcode header;
    "entrypoints", Cmon.list (List.map cmon_entrypoints entrypoints);
    "trailer", cmon_ocamlcode trailer;
  ]
