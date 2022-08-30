(* The shallow abstract syntax *)

type location = {
  loc_file : string;
  start_pos : int;
  end_pos : int;
  start_line : int;
  start_col : int;
}

type ocaml_code = location * string

type position = {line: int; col: int}

type symbol =
  | Name of string
  | Apply of string * symbol list

type atom_desc =
  | Symbol of symbol
  | Item of {
      lhs: symbol option;
      prefix: symbol option list;
      suffix: symbol option list;
    }
  | Wildcard

type regular_desc =
  | Atom of atom_desc * string option
  | Alternative of regular_expr list
  | Repetition of regular_expr
  | Reduce
  | Concat of regular_expr list

and regular_expr = {
  desc: regular_desc;
  position: position;
}

type clause_action =
  | Unreachable
  | Partial of ocaml_code
  | Total of ocaml_code

type clause = {
  pattern: regular_expr;
  action: clause_action;
}

type entry = {
  startsymbols: string list;
  error   : bool;
  name    : string;
  args    : string list;
  clauses : clause list;
}

type lexer_definition = {
  header      : ocaml_code;
  entrypoints : entry list;
  trailer     : ocaml_code;
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

let print_ocamlcode (location, code) =
  Cmon.tuple [
    print_location location;
    Cmon.string code;
  ]

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

let print_atom_desc = function
  | Symbol sym ->
    Cmon.construct "Symbol" [print_symbol sym]
  | Wildcard ->
    Cmon.constant "Wildcard"
  | Item {lhs; prefix; suffix} ->
    Cmon.crecord "Item" [
      "lhs"    , print_option print_symbol lhs;
      "prefix" , Cmon.list (List.map (print_option print_symbol) prefix);
      "suffix" , Cmon.list (List.map (print_option print_symbol) suffix);
    ]

let rec print_regular_term = function
  | Atom (ad, cap) ->
    Cmon.construct "Atom" [print_atom_desc ad; print_option Cmon.string cap]
  | Alternative res ->
    Cmon.constructor "Alternative" (Cmon.list_map print_regular_expression res)
  | Concat res ->
    Cmon.constructor "Concat" (Cmon.list_map print_regular_expression res)
  | Repetition re ->
    Cmon.constructor "Repetition" (print_regular_expression re)
  | Reduce -> Cmon.constant "Reduce"

and print_regular_expression re =
  Cmon.record [
    "desc", print_regular_term re.desc;
    "position", print_position re.position;
  ]

let print_clause_action = function
  | Unreachable -> Cmon.constant "Unreachable"
  | Total code -> Cmon.constructor "Total" (print_ocamlcode code)
  | Partial code -> Cmon.constructor "Partial" (print_ocamlcode code)

let print_clause {pattern; action} =
  Cmon.record [
    "pattern", print_regular_expression pattern;
    "action", print_clause_action action;
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
    "header", print_ocamlcode header;
    "entrypoints", Cmon.list (List.map print_entrypoints entrypoints);
    "trailer", print_ocamlcode trailer;
  ]

type prompt_sentence =
  | Prompt_interpret of symbol list
  | Prompt_entrypoint of symbol
