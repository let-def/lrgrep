(** {1 The shallow abstract syntax} *)

(** A location represents a range of characters from the input file.
    TODO: this is inherited from ocamllex and not really used anymore, cleanup.
*)
type location = {
  loc_file : string;
  start_pos : int;
  end_pos : int;
  start_line : int;
  start_col : int;
}

(** This represents a piece of OCaml code, as appearing in semantic
    actions, as well as in the header and trailer. *)
type ocaml_code = location * string

(** A position in the input file. *)
type position = {line: int; col: int}

(** A grammar symbol (a terminal or a non-terminal) *)
type symbol =
  | Name of string
  (** Symbols are usually simples names, like a or X *)
  | Apply of string * symbol list
  (** Menhir supports higher-order non-terminals. In this case, a symbol is
      the application of the higher-order non-terminal to a some arguments.
      E.g separated_list(sep, X) is represented as:
        [Apply ("separated_list", [Name "sep"; Name "X"])] *)

(** An optional symbol, used to represents wildcards "_" *)
type wild_symbol = symbol option

(** Atomic pattern element (the "characters" of our regular expresssions) *)
type atom_desc =
  | Symbol of symbol
  (** [Symbol s] matches all states whose incoming transitions are labelled
      with symbol [s] *)
  | Item of
      {
        lhs: symbol option;
        (** if specified, the item must have this non-terminal as lhs *)
        anchored : bool;
          (** Whether the prefix must exactly match the item, rather
              than allowing longer productions. *)
        prefix: symbol option list;
        (** the list of producers before the dot *)
        suffix: symbol option list;
        (** the list of producers after the dot *)
      }
  (** [Item {lhs; prefix; suffix}] matches all states that have a
      corresponding an item of the form [lhs ::= ... prefix . suffix ... ]
      in their item set. They are all optional, and for prefix and suffix,
      longer productions are allowed.
      For instance, [. INT] will match an actual item of the form
      [foo: BAR . INT baz]. *)
  | Wildcard
    (* Matches all states *)

(** [regular_desc] describes the different cases of the regular expression
    syntax. *)
type regular_desc =
  | Atom of atom_desc * string option
  (** Leaves of regular expressions, and an optional name to bind the state
      too. [_ as x] is represented as [Atom (Wildcard, Some "x")]. *)
  | Alternative of regular_expr list
  (** A disjunction of multiple expressions.
      [e1 | e2 | e3] is represented as [Alternative [e1; e2; e3]] *)
  | Repetition of regular_expr
  (** [Repetition e] represents [e*] *)
  | Reduce
  (** [Reduce] represents the [!] operator *)
  | Concat of regular_expr list
  (** [Concat [e1; e2; ..]] is [e1; e2; ...] *)

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

(** A clause is a pair of a pattern and an action, representing one rule *)
type clause = {
  pattern: regular_expr; (** the pattern *)
  action: clause_action; (** the semantic action *)
}

(** An .mlyl file can contain multiple entrypoints, each is represented as an
    instance of [entry] *)
type entry = {
  startsymbols: string list; (** TODO: unused for now, the list of startsymbols
                                 of the grammar that this rule applies on. *)
  error   : bool; (** true if the entry is of the form
                        rule x ... = parse error
                        | ...
                      This entrypoint only matches stack that ended up in an
                      error. *)
  name    : string; (** the name of this entry point *)
  args    : string list; (** the list of OCaml arguments to abstract over,
                             e.g the [x y] in [rule foo x y = ...] *)
  clauses : clause list; (** the list of clause that are matched *)
}

(** An .mlyl file is an header containing some OCaml code, one or more entries,
    and a trailer with some other OCaml code. *)
type lexer_definition = {
  header      : ocaml_code;
  entrypoints : entry list;
  trailer     : ocaml_code;
}

(** This definition is used by the interpreter, not the analyser.
    The interpreter reads a list of [prompt_sentence] commands from
    standard input. *)
type prompt_sentence =
  | Prompt_entrypoint of symbol
  (** Starts interpretation from this grammar entrypoint *)
  | Prompt_interpret of symbol list
  (** Interpret this list of symbol *)

(** {1 Helper and printing functions} *)

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
  | Item {lhs; anchored; prefix; suffix} ->
    Cmon.crecord "Item" [
      "lhs"     , print_option print_symbol lhs;
      "anchored", Cmon.bool anchored;
      "prefix"  , Cmon.list (List.map (print_option print_symbol) prefix);
      "suffix"  , Cmon.list (List.map (print_option print_symbol) suffix);
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

