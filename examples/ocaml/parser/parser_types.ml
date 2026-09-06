(* Types shared between two or more internal (non-%start) nonterminals in
   parser.mly, pulled out into their own compilation unit.

   Menhir's generated .mli only ever exposes types for %start symbols
   (via their %type declarations) plus, when --inspection is requested,
   a GADT entry for every nonterminal in the grammar, parameterized or
   not. That GADT entry names the nonterminal's own semantic-value type
   directly - but the parser.mly header (the code between %{ and %}) is
   only ever copied into the generated .ml, never the .mli, so a type
   declared there (as let_binding/let_bindings used to be) is invisible
   from the .mli's point of view and --inspection fails to compile with
   "Unbound type constructor". Moving the type here, to a real, separately
   compiled module, and referencing it from parser.mly's own %type
   declarations, gives both the .ml and the --inspection-generated .mli a
   type they can actually resolve. *)

type let_binding =
  { lb_pattern: Parsetree.pattern;
    lb_expression: Parsetree.expression;
    lb_constraint: Parsetree.value_constraint option;
    lb_is_pun: bool;
    lb_attributes: Parsetree.attributes;
    lb_docs: Docstrings.docs Lazy.t;
    lb_text: Docstrings.text Lazy.t;
    lb_loc: Location.t; }

type let_bindings =
  { lbs_bindings: let_binding list;
    lbs_rec: Asttypes.rec_flag;
    lbs_extension: string Asttypes.loc option }
