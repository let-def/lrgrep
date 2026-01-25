/labeled_tuple_pat_element_list_pattern_: labeled_tuple_pat_element_list_pattern_ COMMA TILDE LPAREN LIDENT . COLON core_type RPAREN
/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT . COLON _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA DOTDOT

Expected a type after `:` in a labeled tuple element. The colon must be followed by a core type before the closing parenthesis.

---

/labeled_tuple_pat_element_list_pattern_: labeled_tuple_pat_element_list_pattern_ COMMA TILDE LPAREN LIDENT COLON . core_type RPAREN
/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON . core_type _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA DOTDOT

Expected a core type after `:` in a labeled tuple element. The colon must be followed by a core type.

---

/labeled_tuple_pat_element_list_pattern_: labeled_tuple_pat_element_list_pattern_ COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN
/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA DOTDOT

Expected a closing `)` after the core type in a labeled tuple element.

---

/labeled_tuple_pat_element_list_pattern_: labeled_tuple_pat_element_list_pattern_ COMMA LABEL . simple_pattern
/labeled_tuple_pat_element_list_pattern_: LABEL . simple_pattern _*
/reversed_labeled_tuple_pattern_pattern_: LABEL . simple_pattern COMMA DOTDOT

Expected a simple pattern after a label name in a labeled tuple element. The label must be followed by a pattern.

---

/constr_extra_nonprefix_ident: LPAREN . RPAREN
/constr_longident: LPAREN . COLONCOLON RPAREN
/pattern_gen: constr_longident LPAREN . TYPE nonempty_list_mkrhs_LIDENT__ RPAREN simple_pattern
/simple_pattern_not_ident: LPAREN . pattern RPAREN
/simple_pattern_not_ident: LPAREN . MODULE _*
/simple_pattern_not_ident: LPAREN . pattern COLON core_type RPAREN
/val_extra_ident: LPAREN . operator RPAREN

Expected a constructor long identifier, a pattern, the keyword `module`, a type specifier, or an operator after `(`. The opening parenthesis must be followed by one of these constructs.

---

/pattern_gen: constr_longident LPAREN TYPE . nonempty_list_mkrhs_LIDENT__ RPAREN simple_pattern

Expected a list of type variable identifiers after `type` inside a constructor pattern.

---

/pattern_gen: constr_longident LPAREN TYPE nonempty_list_mkrhs_LIDENT__ . RPAREN simple_pattern

Expected a closing `)` after the list of type variable identifiers in a constructor pattern.

---

/pattern_gen: constr_longident LPAREN TYPE nonempty_list_mkrhs_LIDENT__ RPAREN . simple_pattern

Expected a simple pattern after the closing `)` of a constructor type application.

---

/constr_longident: LPAREN COLONCOLON . RPAREN

Expected a closing `)` after the `::` constructor token.

---

/labeled_tuple_pat_element_list_pattern_: pattern . COMMA _*
/reversed_labeled_tuple_pattern_pattern_: pattern . COMMA DOTDOT
/simple_pattern_not_ident: LPAREN pattern . RPAREN
/simple_pattern_not_ident: LPAREN pattern . COLON core_type RPAREN

Expected a closing `)` after a pattern in parentheses, or a `:` followed by a core type, or a comma to continue the tuple.

---

/simple_pattern_not_ident: LPAREN pattern COLON . core_type RPAREN

Expected a core type after `:` in a typed pattern.

---

/simple_pattern_not_ident: LPAREN pattern COLON core_type . RPAREN

Expected a closing `)` after a typed pattern.

---

/constr_longident: mod_longident DOT LPAREN COLONCOLON . RPAREN

Expected a closing `)` after the `::` constructor token following a module qualifier.

---

/labeled_tuple_pat_element_list_pattern_: pattern . COMMA _*
/reversed_labeled_tuple_pattern_pattern_: pattern . COMMA DOTDOT
/simple_pattern_not_ident: mod_longident DOT LPAREN pattern . RPAREN

Expected a closing `)` after a pattern following a module-qualified constructor.

---

/simple_delimited_pattern: LBRACKET . separated_or_terminated_nonempty_list_SEMI_pattern_ RBRACKET
/simple_pattern_not_ident: mod_longident DOT LBRACKET . RBRACKET

Expected a list of patterns or a closing `]` after an opening `[` in a list pattern.

---

/simple_delimited_pattern: LBRACKET separated_or_terminated_nonempty_list_SEMI_pattern_ . RBRACKET

Expected a closing `]` after a list of patterns.

---

/simple_delimited_pattern: LBRACKETBAR separated_or_terminated_nonempty_list_SEMI_pattern_ . BARRBRACKET

Expected a closing `|]` after a list of patterns in a bar‑bracket list pattern.

---

/labeled_tuple_pat_element_list_pattern_no_exn_: LABEL . simple_pattern _*
/labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LIDENT COMMA LABEL . simple_pattern
/reversed_labeled_tuple_pattern_pattern_no_exn_: LABEL . simple_pattern COMMA DOTDOT

Expected a simple pattern after a label in a non‑exhaustive tuple element, or after a comma following a labeled element.

---

/labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA . pattern_no_exn
/labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA . LABEL simple_pattern
/labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA . TILDE _*
/reversed_labeled_tuple_pattern_pattern_no_exn_: LABEL simple_pattern COMMA . DOTDOT

Expected another pattern element after a comma in a non‑exhaustive labeled tuple.

---

/labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA TILDE . LIDENT
/labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA TILDE . LPAREN LIDENT COLON core_type RPAREN
/labeled_tuple_pat_element_list_pattern_no_exn_: TILDE . LIDENT _*
/labeled_tuple_pat_element_list_pattern_no_exn_: TILDE . LPAREN _*
/reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE . LIDENT COMMA DOTDOT
/reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE . LPAREN LIDENT COLON core_type RPAREN COMMA DOTDOT

Expected an identifier after `~` in a non‑exhaustive tuple element, or a type annotation after `(`, or a closing `)`.

---

/labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA TILDE . LIDENT
/labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA TILDE . LPAREN LIDENT COLON core_type RPAREN
/labeled_tuple_pat_element_list_pattern_no_exn_: TILDE . LIDENT _*
/labeled_tuple_pat_element_list_pattern_no_exn_: TILDE . LPAREN _*
/reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE . LIDENT COMMA DOTDOT
/reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE . LPAREN LIDENT COLON core_type RPAREN COMMA DOTDOT

Same as above; expected an identifier or a typed pattern after `~`.

---

/labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA TILDE LPAREN . LIDENT COLON core_type RPAREN
/labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN . LIDENT _*
/reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN . LIDENT COLON core_type RPAREN COMMA DOTDOT

Expected an identifier after `(` in a `~(` construct, or a colon‑type before the closing parenthesis.

---

/labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT . COLON core_type RPAREN
/labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT . COLON _*
/reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT

Expected a colon after the identifier inside `~(`, followed by a core type.

---

/labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON . core_type RPAREN
/labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON . core_type _*
/reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON . core_type RPAREN COMMA DOTDOT

Expected the core type after the colon inside `~(`.

---

/labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN
/labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . RPAREN _*
/reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT

Expected a closing `)` after the typed `~(` element.

---

/labeled_tuple_pat_element_list_pattern_no_exn_: LABEL . simple_pattern _*
/labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA LABEL . simple_pattern
/labeled_tuple_pat_element_list_pattern_no_exn_: LABEL . simple_pattern _*
/reversed_labeled_tuple_pattern_pattern_no_exn_: LABEL . simple_pattern COMMA DOTDOT

Expected a simple pattern after a label, or another label after a comma.

---

/labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA . pattern_no_exn
/labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA . LABEL simple_pattern
/labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA . TILDE _*
/reversed_labeled_tuple_pattern_pattern_no_exn_: LABEL simple_pattern COMMA . DOTDOT

Expected another pattern element (pattern, label, or `~`) after a comma.

---

/labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA TILDE . LIDENT
/labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA TILDE . LPAREN LIDENT COLON core_type RPAREN
/labeled_tuple_pat_element_list_pattern_no_exn_: TILDE . LIDENT _*
/labeled_tuple_pat_element_list_pattern_no_exn_: TILDE . LPAREN _*
/reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE . LIDENT COMMA DOTDOT
/reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE . LPAREN LIDENT COLON core_type RPAREN COMMA DOTDOT

Expected an identifier after `~` following a comma, or a typed `~(` construct.

---

/labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA TILDE LPAREN . LIDENT COLON core_type RPAREN
/labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN . LIDENT _*
/reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN . LIDENT COLON core_type RPAREN COMMA DOTDOT

Expected an identifier after `(` inside `~(`, followed by a colon and type.

---

/labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT . COLON core_type RPAREN
/labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT . COLON _*
/reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT

Expected a colon after the identifier inside `~(`.

---

/labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON . core_type RPAREN
/labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON . core_type _*
/reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON . core_type RPAREN COMMA DOTDOT

Expected a core type after the colon inside `~(`.

---

/labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN
/labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . RPAREN _*
/reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT

Expected a closing `)` after the typed `~(` element.

---

/labeled_tuple_pat_element_list_pattern_no_exn_: LABEL . simple_pattern _*
/labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA LABEL . simple_pattern
/reversed_labeled_tuple_pattern_pattern_no_exn_: LABEL . simple_pattern COMMA DOTDOT

Expected a simple pattern after a label, or another label after a comma.

---

/labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LIDENT . COMMA _*
/reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LIDENT . COMMA DOTDOT

Expected a comma after the identifier in a `~x` element, or a dot‑dot sequence.

---

/labeled_tuple_pat_element_list_pattern_no_exn_: LABEL . simple_pattern _*
/reversed_labeled_tuple_pattern_pattern_no_exn_: LABEL . simple_pattern COMMA DOTDOT

Same as above for label.

---

/labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern . COMMA _*
/reversed_labeled_tuple_pattern_pattern_no_exn_: LABEL simple_pattern . COMMA DOTDOT

Expected a comma after a labeled pattern element, or a dot‑dot sequence.

---

/simple_param_pattern: TILDE . LPAREN label_let_pattern RPAREN
/simple_param_pattern: TILDE . LIDENT

Expected a label pattern or identifier after `~` in a function parameter.

---

/simple_param_pattern: TILDE LPAREN . label_let_pattern RPAREN

Expected a label pattern after `~(` in a function parameter.

---

/label_let_pattern: LIDENT COLON . possibly_poly_core_type_

Expected a type annotation after the colon in a label pattern.

---

/possibly_poly_core_type_: reversed_nonempty_llist_typevar_ . DOT core_type

Expected a dot `.` after a sequence of type variables in a polymorphic type.

---

/possibly_poly_core_type_: reversed_nonempty_llist_typevar_ DOT . core_type

Expected a core type after the dot in a polymorphic type.

---

/simple_param_pattern: TILDE LPAREN label_let_pattern . RPAREN

Expected a closing `)` after a label pattern in a `~(` parameter.

---

/simple_param_pattern: QUESTION . LPAREN label_let_pattern option_preceded_EQUAL_seq_expr__ RPAREN
/simple_param_pattern: QUESTION . LIDENT

Expected a label pattern or identifier after `?` in an optional parameter.

---

/simple_param_pattern: QUESTION LPAREN . label_let_pattern option_preceded_EQUAL_seq_expr__ RPAREN

Expected a label pattern after `?(` in an optional parameter.

---

/option_preceded_EQUAL_seq_expr__: EQUAL . seq_expr

Expected an expression after `=` in a let‑binding or similar construct.

---

/module_type_declaration: MODULE TYPE ext list_attribute_ . ident option_preceded_EQUAL_module_type__ list_post_item_attribute_

Expected a module type name identifier after `module type`.

---

/module_type_declaration: MODULE TYPE ext list_attribute_ ident . EQUAL module_type list_post_item_attribute_

Expected `=` after the module type name.

---

/functor_arg: LPAREN . RPAREN
/functor_arg: LPAREN . module_name COLON module_type RPAREN
/module_type: LPAREN . module_type RPAREN
/module_type: FUNCTOR list_attribute_ . reversed_nonempty_llist_functor_arg_ MINUSGREATER module_type

Expected `)` or a module name after an opening `(` in a functor argument, or a functor argument after `functor`.

---

/functor_arg: LPAREN . RPAREN
/functor_arg: LPAREN . module_name COLON module_type RPAREN
/module_type: FUNCTOR list_attribute_ . reversed_nonempty_llist_functor_arg_ MINUSGREATER module_type
/module_type: FUNCTOR list_attribute_ . reversed_nonempty_llist_functor_arg_ MINUSGREATER module_type

Expected a closing `)` or module name after `(` when parsing a module type.

---

/functor_arg: LPAREN . RPAREN
/functor_arg: LPAREN . module_name COLON module_type RPAREN

Expected `)` after an empty functor argument or a module name after `(`.

---

/functor_arg: LPAREN module_name . COLON module_type RPAREN

Expected `:` after a module name in a functor argument.

---

/functor_arg: LPAREN module_name COLON . module_type RPAREN

Expected a module type after the colon in a functor argument.

---

/module_type: reversed_nonempty_llist_functor_arg_ . MINUSGREATER module_type

Expected `->` after the functor argument list in a module type.

---

/module_type: reversed_nonempty_llist_functor_arg_ MINUSGREATER . module_type

Expected the result module type after `->`.

---

/module_type: module_type WITH . reversed_separated_nonempty_llist_AND_with_constraint_

Expected a `with` constraint after `with`.

---

/with_constraint: TYPE type_parameters . label_longident _*

Expected a label identifier after the type parameters in a `with type` constraint.

---

/with_constraint: TYPE type_parameters label_longident . with_type_binder alias_type reversed_llist_preceded_CONSTRAINT_constrain__
/with_constraint: TYPE type_parameters label_longident . COLONEQUAL alias_type

Expected a type binder (`=` or `= private`) after the label identifier.

---

/with_constraint: TYPE type_parameters label_longident COLONEQUAL . alias_type

Expected the alias type after `=` in a `with type` constraint.

---

/with_constraint: TYPE type_parameters label_longident with_type_binder . alias_type reversed_llist_preceded_CONSTRAINT_constrain__

Expected the alias type after the binder in a `with type` constraint.

---

/reversed_llist_preceded_CONSTRAINT_constrain__: reversed_llist_preceded_CONSTRAINT_constrain__ CONSTRAINT . core_type EQUAL core_type

Expected a core type after `constraint` in a type constraint list.

---

/reversed_llist_preceded_CONSTRAINT_constrain__: reversed_llist_preceded_CONSTRAINT_constrain__ CONSTRAINT core_type . EQUAL core_type

Expected `=` after the core type in a constraint.

---

/reversed_llist_preceded_CONSTRAINT_constrain__: reversed_llist_preceded_CONSTRAINT_constrain__ CONSTRAINT core_type EQUAL . core_type

Expected the second core type after `=` in a constraint.

---

/with_constraint: MODULE . mod_longident _*
/with_constraint: MODULE . TYPE _*

Expected a module name or `type` after `with module`.

---

/with_constraint: MODULE TYPE . mty_longident _*

Expected a module type identifier after `with module type`.

---

/with_constraint: MODULE TYPE mty_longident . EQUAL module_type
/with_constraint: MODULE TYPE mty_longident . COLONEQUAL module_type

Expected `=` or `:=` after a module type identifier in a `with module type` constraint.

---

/with_constraint: MODULE TYPE mty_longident EQUAL . module_type

Expected a module type after `=` in a `with module type` constraint.

---

/module_type: module_type MINUSGREATER . module_type

Expected a result module type after `->` in a module type expression.

---

/mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident . DOT UIDENT
/mk_longident_mod_ext_longident_ident_: mod_ext_longident . DOT ident

Expected a dot followed by an identifier after a module extension long identifier.

---

/mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident DOT . UIDENT
/mk_longident_mod_ext_longident_ident_: mod_ext_longident DOT . ident

Expected an identifier after the dot in a module extension long identifier.

---

/with_constraint: MODULE TYPE mty_longident COLONEQUAL . module_type

Expected a module type after `:=` in a `with module type` constraint.

---

/mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT
/with_constraint: MODULE mod_longident . EQUAL mod_ext_longident
/with_constraint: MODULE mod_longident . COLONEQUAL mod_ext_longident

Expected `=` or `:=` after a module name in a `with module` constraint.

---

/with_constraint: MODULE mod_longident EQUAL . mod_ext_longident

Expected a module extension long identifier after `=` in a `with module` constraint.

---

/mk_longident_mod_longident_UIDENT_: mod_longident DOT . UIDENT

Expected an identifier after the dot in a module long identifier.

---

/with_constraint: MODULE mod_longident COLONEQUAL . mod_ext_longident

Expected a module extension long identifier after `:=` in a `with module` constraint.

---

/reversed_separated_nonempty_llist_AND_with_constraint_: reversed_separated_nonempty_llist_AND_with_constraint_ AND . with_constraint

Expected another `with` constraint after `and`.

---

/functor_arg: LPAREN . RPAREN
/functor_arg: LPAREN . module_name COLON module_type RPAREN
/module_type: LPAREN . module_type RPAREN

Expected `)` or module name after `(` in a functor argument or module type.

---

/module_type: FUNCTOR list_attribute_ . reversed_nonempty_llist_functor_arg_ MINUSGREATER module_type

Expected a functor argument list after `functor`.

---

/module_type: FUNCTOR list_attribute_ . reversed_nonempty_llist_functor_arg_ MINUSGREATER module_type
/module_type: FUNCTOR list_attribute_ . reversed_nonempty_llist_functor_arg_ MINUSGREATER module_type

Expected a functor argument after `functor`.

---

/functor_arg: LPAREN . RPAREN
/functor_arg: LPAREN . module_name COLON module_type RPAREN
/module_type: LPAREN . module_type RPAREN

Expected a closing parenthesis or module name after `(`.

---

/functor_arg: LPAREN module_name . COLON module_type RPAREN

Expected `:` after the module name in a functor argument.

---

/functor_arg: LPAREN module_name COLON . module_type RPAREN

Expected a module type after the colon in a functor argument.

---

/module_type: reversed_nonempty_llist_functor_arg_ . MINUSGREATER module_type

Expected `->` after the functor argument list.

---

/module_type: reversed_nonempty_llist_functor_arg_ MINUSGREATER . module_type

Expected the resulting module type after `->`.

---

/module_type: module_type WITH . reversed_separated_nonempty_llist_AND_with_constraint_

Expected a `with` constraint after `with`.

---

/with_constraint: TYPE type_parameters . label_longident _*

Expected a label identifier after type parameters in a `with type` constraint.

---

/with_constraint: TYPE type_parameters label_longident . with_type_binder alias_type reversed_llist_preceded_CONSTRAINT_constrain__
/with_constraint: TYPE type_parameters label_longident . COLONEQUAL alias_type

Expected a type binder (`=` or `:=`) after the label in a `with type` constraint.

---

/with_constraint: TYPE type_parameters label_longident COLONEQUAL . alias_type

Expected the alias type after `=` in a `with type` constraint.

---

/with_constraint: TYPE type_parameters label_longident with_type_binder . alias_type reversed_llist_preceded_CONSTRAINT_constrain__

Expected the alias type after the binder in a `with type` constraint.

---

/reversed_llist_preceded_CONSTRAINT_constrain__: reversed_llist_preceded_CONSTRAINT_constrain__ CONSTRAINT . core_type EQUAL core_type

Expected a core type after `constraint` in a type constraint list.

---

/reversed_llist_preceded_CONSTRAINT_constrain__: reversed_llist_preceded_CONSTRAINT_constrain__ CONSTRAINT core_type . EQUAL core_type

Expected `=` after the core type in a constraint.

---

/reversed_llist_preceded_CONSTRAINT_constrain__: reversed_llist_preceded_CONSTRAINT_constrain__ CONSTRAINT core_type EQUAL . core_type

Expected the second core type after `=`.

---

/with_constraint: MODULE . mod_longident _*
/with_constraint: MODULE . TYPE _*

Expected a module identifier or the keyword `type` after `with module`.

---

/with_constraint: MODULE TYPE . mty_longident _*

Expected a module type identifier after `module type`.

---

/with_constraint: MODULE TYPE mty_longident . EQUAL module_type
/with_constraint: MODULE TYPE mty_longident . COLONEQUAL module_type

Expected `=` or `:=` after the module type identifier.

---

/with_constraint: MODULE TYPE mty_longident EQUAL . module_type

Expected a module type after `=`.

---

/module_type: module_type MINUSGREATER . module_type

Expected a module type after `->` in a module type expression.

---

/mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident . DOT UIDENT
/mk_longident_mod_ext_longident_ident_: mod_ext_longident . DOT ident

Expected an identifier after a dot in a module extension long identifier.

---

/mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident DOT . UIDENT
/mk_longident_mod_ext_longident_ident_: mod_ext_longident DOT . ident

Expected an identifier after a dot in a module extension long identifier.

---

/with_constraint: MODULE TYPE mty_longident COLONEQUAL . module_type

Expected a module type after `:=` in a `with module type` constraint.

---

/mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT
/with_constraint: MODULE mod_longident . EQUAL mod_ext_longident
/with_constraint: MODULE mod_longident . COLONEQUAL mod_ext_longident

Expected `=` or `:=` after a module name in a `with module` constraint.

---

/with_constraint: MODULE mod_longident EQUAL . mod_ext_longident

Expected a module extension identifier after `=`.

---

/mk_longident_mod_longident_UIDENT_: mod_longident DOT . UIDENT

Expected an identifier after the dot in a module long identifier.

---

/with_constraint: MODULE mod_longident COLONEQUAL . mod_ext_longident

Expected a module extension identifier after `:=`.

---

/reversed_separated_nonempty_llist_AND_with_constraint_: reversed_separated_nonempty_llist_AND_with_constraint_ AND . with_constraint

Expected another `with` constraint after `and`.

---

/functor_arg: LPAREN . RPAREN
/functor_arg: LPAREN . module_name COLON module_type RPAREN
/module_type: LPAREN . module_type RPAREN

Expected `)` or a module name after `(`.

---

/module_type: FUNCTOR list_attribute_ . reversed_nonempty_llist_functor_arg_ MINUSGREATER module_type

Expected a functor argument after `functor`.

---

/module_type: FUNCTOR list_attribute_ . reversed_nonempty_llist_functor_arg_ MINUSGREATER module_type
/module_type: FUNCTOR list_attribute_ . reversed_nonempty_llist_functor_arg_ MINUSGREATER module_type

Expected a functor argument after `functor`.

---

/functor_arg: LPAREN . RPAREN
/functor_arg: LPAREN . module_name COLON module_type RPAREN

Expected `)` or a module name after `(`.

---

/functor_arg: LPAREN module_name . COLON module_type RPAREN

Expected `:` after a module name inside a functor argument.

---

/functor_arg: LPAREN module_name COLON . module_type RPAREN

Expected a module type after `:`.

---

/module_type: reversed_nonempty_llist_functor_arg_ . MINUSGREATER module_type

Expected `->` after a functor argument list.

---

/module_type: reversed_nonempty_llist_functor_arg_ MINUSGREATER . module_type

Expected the result module type after `->`.

---

/module_type: module_type WITH . reversed_separated_nonempty_llist_AND_with_constraint_

Expected a `with` constraint after `with`.

---

/with_constraint: TYPE type_parameters . label_longident _*

Expected a label identifier after type parameters in a `with type` constraint.

---

/with_constraint: TYPE type_parameters label_longident . with_type_binder alias_type reversed_llist_preceded_CONSTRAINT_constrain__
/with_constraint: TYPE type_parameters label_longident . COLONEQUAL alias_type

Expected a type binder (`=` or `:=`) after the label.

---

/with_constraint: TYPE type_parameters label_longident COLONEQUAL . alias_type

Expected the alias type after `=`.

---

/with_constraint: TYPE type_parameters label_longident with_type_binder . alias_type reversed_llist_preceded_CONSTRAINT_constrain__

Expected the alias type after the binder.

---

/reversed_llist_preceded_CONSTRAINT_constrain__: reversed_llist_preceded_CONSTRAINT_constrain__ CONSTRAINT . core_type EQUAL core_type

Expected a core type after `constraint`.

---

/reversed_llist_preceded_CONSTRAINT_constrain__: reversed_llist_preceded_CONSTRAINT_constrain__ CONSTRAINT core_type . EQUAL core_type

Expected `=` after the core type.

---

/reversed_llist_preceded_CONSTRAINT_constrain__: reversed_llist_preceded_CONSTRAINT_constrain__ CONSTRAINT core_type EQUAL . core_type

Expected the second core type after `=`.

---

/with_constraint: MODULE . mod_longident _*
/with_constraint: MODULE . TYPE _*

Expected a module identifier or `type` after `with module`.

---

/with_constraint: MODULE TYPE . mty_longident _*

Expected a module type identifier after `module type`.

---

/with_constraint: MODULE TYPE mty_longident . EQUAL module_type
/with_constraint: MODULE TYPE mty_longident . COLONEQUAL module_type

Expected `=` or `:=` after the module type identifier.

---

/with_constraint: MODULE TYPE mty_longident EQUAL . module_type

Expected a module type after `=`.

---

/module_type: module_type MINUSGREATER . module_type

Expected a module type after `->`.

---

/mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident . DOT UIDENT
/mk_longident_mod_ext_longident_ident_: mod_ext_longident . DOT ident

Expected an identifier after the dot in a module extension long identifier.

---

/mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident DOT . UIDENT
/mk_longident_mod_ext_longident_ident_: mod_ext_longident DOT . ident

Expected an identifier after the dot.

---

/with_constraint: MODULE TYPE mty_longident COLONEQUAL . module_type

Expected a module type after `:=`.

---

/mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT
/with_constraint: MODULE mod_longident . EQUAL mod_ext_longident
/with_constraint: MODULE mod_longident . COLONEQUAL mod_ext_longident

Expected `=` or `:=` after a module name.

---

/with_constraint: MODULE mod_longident EQUAL . mod_ext_longident

Expected a module extension identifier after `=`.

---

/mk_longident_mod_longident_UIDENT_: mod_longident DOT . UIDENT

Expected an identifier after the dot.

---

/with_constraint: MODULE mod_longident COLONEQUAL . mod_ext_longident

Expected a module extension identifier after `:=`.

---

/reversed_separated_nonempty_llist_AND_with_constraint_: reversed_separated_nonempty_llist_AND_with_constraint_ AND . with_constraint

Expected another `with` constraint after `and`.

---

/functor_arg: LPAREN . RPAREN
/functor_arg: LPAREN . module_name COLON module_type RPAREN
/module_type: LPAREN . module_type RPAREN

Expected a closing parenthesis or module name after `(`.

---

/module_type: FUNCTOR list_attribute_ . reversed_nonempty_llist_functor_arg_ MINUSGREATER module_type

Expected a functor argument after `functor`.

---

/module_type: FUNCTOR list_attribute_ . reversed_nonempty_llist_functor_arg_ MINUSGREATER module_type
/module_type: FUNCTOR list_attribute_ . reversed_nonempty_llist_functor_arg_ MINUSGREATER module_type

Expected a functor argument after `functor`.

---

/functor_arg: LPAREN . RPAREN
/functor_arg: LPAREN . module_name COLON module_type RPAREN

Expected a closing `)` or a module name after `(`.

---

/functor_arg: LPAREN module_name . COLON module_type RPAREN

Expected `:` after a module name in a functor argument.

---

/functor_arg: LPAREN module_name COLON . module_type RPAREN

Expected a module type after the colon.

---

/module_type: reversed_nonempty_llist_functor_arg_ . MINUSGREATER module_type

Expected `->` after the functor argument list.

---

/module_type: reversed_nonempty_llist_functor_arg_ MINUSGREATER . module_type

Expected the result module type after `->`.

---

/module_type: module_type WITH . reversed_separated_nonempty_llist_AND_with_constraint_

Expected a `with` constraint after `with`.

---

/with_constraint: TYPE type_parameters . label_longident _*

Expected a label identifier after type parameters in a `with type` constraint.

---

/with_constraint: TYPE type_parameters label_longident . with_type_binder alias_type reversed_llist_preceded_CONSTRAINT_constrain__
/with_constraint: TYPE type_parameters label_longint . COLONEQUAL alias_type

Expected a type binder (`=` or `:=`) after the label.

---

/with_constraint: TYPE type_parameters label_longident COLONEQUAL . alias_type

Expected the alias type after `=`.

---

/with_constraint: TYPE type_parameters label_longident with_type_binder . alias_type reversed_llist_preceded_CONSTRAINT_constrain__

Expected the alias type after the binder.

---

/reversed_llist_preceded_CONSTRAINT_constrain__: reversed_llist_preceded_CONSTRAINT_constrain__ CONSTRAINT . core_type EQUAL core_type

Expected a core type after `constraint`.

---

/reversed_llist_preceded_CONSTRAINT_constrain__: reversed_llist_preceded_CONSTRAINT_constrain__ CONSTRAINT core_type . EQUAL core_type

Expected `=` after the core type.

---

/reversed_llist_preceded_CONSTRAINT_constrain__: reversed_llist_preceded_CONSTRAINT_constrain__ CONSTRAINT core_type EQUAL . core_type

Expected the second core type after `=`.

---

/with_constraint: MODULE . mod_longident _*
/with_constraint: MODULE . TYPE _*

Expected a module identifier or `type` after `with module`.

---

/with_constraint: MODULE TYPE . mty_longident _*

Expected a module type identifier after `module type`.

---

/with_constraint: MODULE TYPE mty_longident . EQUAL module_type
/with_constraint: MODULE TYPE mty_longident . COLONEQUAL module_type

Expected `=` or `:=` after the module type identifier.

---

/with_constraint: MODULE TYPE mty_longident EQUAL . module_type

Expected a module type after `=`.

---

/module_type: module_type MINUSGREATER . module_type

Expected a module type after `->`.

---

/mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident . DOT UIDENT
/mk_longident_mod_ext_longident_ident_: mod_ext_longident . DOT ident

Expected an identifier after the dot.

---

/mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident DOT . UIDENT
/mk_longident_mod_ext_longident_ident_: mod_ext_longident DOT . ident

Expected an identifier after the dot.

---

/with_constraint: MODULE TYPE mty_longident COLONEQUAL . module_type

Expected a module type after `:=`.

---

/mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT
/with_constraint: MODULE mod_longident . EQUAL mod_ext_longident
/with_constraint: MODULE mod_longident . COLONEQUAL mod_ext_longident

Expected `=` or `:=` after a module name.

---

/with_constraint: MODULE mod_longident EQUAL . mod_ext_longident

Expected a module extension identifier after `=`.

---

/mk_longident_mod_longident_UIDENT_: mod_longident DOT . UIDENT

Expected an identifier after the dot.

---

/with_constraint: MODULE mod_longident COLONEQUAL . mod_ext_longident

Expected a module extension identifier after `:=`.

---

/reversed_separated_nonempty_llist_AND_with_constraint_: reversed_separated_nonempty_llist_AND_with_constraint_ AND . with_constraint

Expected another `with` constraint after `and`.

---
    /fun_expr: fun_expr MINUS . expr
/fun_expr: fun_expr LESS . expr
/fun_expr: fun_expr INFIXOP4 . expr
/fun_expr: fun_expr INFIXOP3 . expr
/fun_expr: fun_expr INFIXOP2 . expr
/fun_expr: fun_expr INFIXOP1 . expr
/fun_expr: fun_expr INFIXOP0 . expr
/fun_expr: fun_expr GREATER . expr
/fun_expr: fun_expr EQUAL . expr
/fun_expr: fun_expr COLONEQUAL . expr
/fun_expr: fun_expr COLONCOLON . expr
/fun_expr: fun_expr BARBAR . expr
/fun_expr: fun_expr AMPERSAND . expr
/fun_expr: fun_expr AMPERAMPER . expr

A binary operator must be followed by an expression. After “-”, “<”, “>”, “=”, “:=”, “::”, “||”, “&&”, “&”, or any infix operator (`INFIXOP0`‑`INFIXOP4`) the parser expects the right‑hand side expression; none of the listed tokens can appear here.

/fun_seq_expr: fun_expr SEMI PERCENT . attr_id seq_expr

After a semicolon and a ‘%’, an attribute identifier is required. The parser was expecting an `attr_id` (e.g. `foo`) before the next expression.

/fun_seq_expr: fun_expr SEMI PERCENT attr_id . seq_expr

An attribute identifier must be followed by an expression. The parser was expecting a sequence expression after the attribute name.

/reversed_labeled_tuple_body: expr . COMMA _*

A comma inside a tuple must be followed by another expression (or another tuple element). The parser was expecting the next tuple component after ‘,’.

/and_let_binding: AND list_attribute_ . let_binding_body list_post_item_attribute_

After the keyword `and` (optionally followed by attributes) a new let‑binding must start. The parser was expecting a binding after `and`.

/strict_binding: EQUAL . seq_expr

The `=` in a binding must be followed by an expression. The parser was expecting the right‑hand side after ‘=’.

/strict_binding: fun_params option_type_constraint_ . EQUAL fun_body

When a function parameter list is followed by `=`, a function body must appear. The parser was expecting a function body after ‘=’.

/strict_binding: fun_params option_type_constraint_ EQUAL . fun_body

Same as above – the parser expects a function body after ‘=’ in a `let*` binding.

/let_binding_body_no_punning: val_ident COLON . reversed_nonempty_llist_typevar_ DOT core_type EQUAL seq_expr
/let_binding_body_no_punning: val_ident COLON . TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL seq_expr
/type_constraint: COLON . core_type _*
/let_binding_body_no_punning: val_ident COLON TYPE . nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL seq_expr
/let_binding_body_no_punning: val_ident COLON TYPE nonempty_list_mkrhs_LIDENT__ . DOT core_type EQUAL seq_expr
/let_binding_body_no_punning: val_ident COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT . core_type EQUAL seq_expr
/let_binding_body_no_punning: val_ident COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL . seq_expr
/let_binding_body_no_punning: val_ident COLON reversed_nonempty_llist_typevar_ DOT core_type EQUAL seq_expr
/let_binding_body_no_punning: val_ident COLON reversed_nonempty_llist_typevar_ DOT . core_type EQUAL seq_expr
/let_binding_body_no_punning: val_ident COLON reversed_nonempty_llist_typevar_ DOT core_type . EQUAL seq_expr
/let_binding_body_no_punning: val_ident COLON reversed_nonempty_llist_typevar_ DOT core_type EQUAL . seq_expr
/let_binding_body_no_punning: val_ident type_constraint . EQUAL seq_expr
/let_binding_body_no_punning: val_ident type_constraint EQUAL . seq_expr

After a value name and a colon a type annotation must be supplied. The parser expected a type (or `type` keyword) after ‘:’, and after the type it expects either ‘=’ followed by an expression or the end of the binding. Tokens such as `=`, `;`, `and`, etc., are not valid directly after ‘:’.

/strict_binding: fun_params option_type_constraint_ . EQUAL fun_body

A function definition with parameters must have an `=` before its body. The parser was expecting ‘=’ after the parameter list.

/let_binding_body_no_punning: simple_pattern_not_ident COLON . core_type EQUAL seq_expr
/let_binding_body_no_punning: simple_pattern_not_ident COLON core_type . EQUAL seq_expr
/let_binding_body_no_punning: simple_pattern_not_ident COLON core_type EQUAL . seq_expr

When a non‑identifier pattern is followed by a colon, a type annotation is required. The parser expects a type after ‘:’, then ‘=’ and an expression. Other tokens are invalid here.

/labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn . COMMA _*
/letop_binding_body: pattern_no_exn . EQUAL seq_expr
/reversed_labeled_tuple_pattern_pattern_no_exn_: pattern_no_exn . COMMA DOTDOT
/let_binding_body_no_punning: pattern_no_exn . EQUAL seq_expr
/letop_binding_body: pattern_no_exn EQUAL . seq_expr

After a pattern without exception (`pattern_no_exn`) a comma (or `, ..` for open tuples) may continue the tuple, or an `=` may start a binding. The parser expects either a comma (or `, ..`) or ‘=’ after the pattern, not the listed tokens.

/fun_expr: simple_expr DOTOP LBRACKET . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS expr
/simple_expr: simple_expr DOTOP LBRACKET . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET

After a “.+ [” the parser expects a list of expressions (separated by semicolons) before the closing ‘]’. The ‘[’ must be followed by at least one expression element or a closing ‘]’.

/fun_expr: simple_expr DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACKET LESSMINUS expr
/simple_expr: simple_expr DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACKET

Inside a “.+ [ … ]” the parser expects a closing ‘]’ after the sequence of expressions. A missing ‘]’ causes the error.

/fun_expr: simple_expr DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS . expr

After the closing ‘]’ of a “.+ [ … ]” the parser expects the assignment operator “<-”. The ‘<-’ token is missing.

/fun_expr: simple_expr DOTOP LBRACE . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS expr
/simple_expr: simple_expr DOTOP LBRACE . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE

After a “.+ {” the parser expects a list of expressions before the closing ‘}’. The ‘{’ must be followed by an expression or a closing ‘}’.

/fun_expr: simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACE LESSMINUS expr
/simple_expr: simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACE

Inside a “.+ { … }” the parser expects a closing ‘}’ after the sequence of expressions.

/fun_expr: simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS . expr

After the closing ‘}’ of a “.+ { … }” the parser expects the assignment operator “<-”. The ‘<-’ token is missing.

/fun_expr: simple_expr DOT . label_longident LESSMINUS expr
/fun_expr: simple_expr DOT . LPAREN seq_expr RPAREN LESSMINUS expr
/fun_expr: simple_expr DOT . LBRACE seq_expr RBRACE LESSMINUS expr
/fun_expr: simple_expr DOT . LBRACKET seq_expr RBRACKET LESSMINUS expr
/fun_expr: simple_expr DOT . mod_longident _*
/simple_expr: simple_expr DOT . LPAREN seq_expr RPAREN
/simple_expr: simple_expr DOT . LBRACE seq_expr RBRACE
/simple_expr: simple_expr DOT . LBRACKET seq_expr RBRACKET
/simple_expr: simple_expr DOT . mod_longident _*
/simple_expr: simple_expr DOT . label_longident

After a dot (`.`) the parser expects a field name, a module name, or a parenthesized expression. A bare dot without a following identifier, ‘(’, ‘{’, or ‘[’ is invalid.

/fun_expr: simple_expr DOT LPAREN . seq_expr RPAREN LESSMINUS expr
/simple_expr: simple_expr DOT LPAREN . seq_expr RPAREN

After “.<(” the parser expects an expression inside the parentheses before the closing ‘)’. The opening ‘(’ must be followed by an expression.

/fun_expr: simple_expr DOT LPAREN seq_expr . RPAREN LESSMINUS expr
/simple_expr: simple_expr DOT LPAREN seq_expr . RPAREN

After an expression inside “.<( … )” the parser expects a closing ‘)’. The ‘)’ is missing.

/fun_expr: simple_expr DOT LPAREN seq_expr RPAREN LESSMINUS . expr

After the closing ‘)’ of “.<( … )” the parser expects the assignment operator “<-”. The ‘<-’ token is missing.

/fun_expr: simple_expr DOT LBRACKET . seq_expr RBRACKET LESSMINUS expr
/simple_expr: simple_expr DOT LBRACKET . seq_expr RBRACKET

After “.<[” the parser expects an expression before the closing ‘]’. A missing expression leads to the error.

/fun_expr: simple_expr DOT LBRACKET seq_expr . RBRACKET LESSMINUS expr
/simple_expr: simple_expr DOT LBRACKET seq_expr . RBRACKET

After an expression inside “.<[ … ]” the parser expects the closing ‘]’. The ‘]’ is missing.

/fun_expr: simple_expr DOT LBRACKET seq_expr RBRACKET LESSMINUS . expr

After the closing ‘]’ of “.<[ … ]” the parser expects the assignment operator “<-”. The ‘<-’ token is missing.

/fun_expr: simple_expr DOT LBRACE . seq_expr RBRACE LESSMINUS expr
/simple_expr: simple_expr DOT LBRACE . seq_expr RBRACE

After “.<{” the parser expects an expression before the closing ‘}’. A missing expression causes the error.

/fun_expr: simple_expr DOT LBRACE seq_expr . RBRACE LESSMINUS expr
/simple_expr: simple_expr DOT LBRACE seq_expr . RBRACE

After an expression inside “.<{ … }” the parser expects the closing ‘}’. The ‘}’ is missing.

/fun_expr: simple_expr DOT LBRACE seq_expr RBRACE LESSMINUS . expr

After the closing ‘}’ of “.<{ … }” the parser expects the assignment operator “<-”. The ‘<-’ token is missing.

/fun_expr: simple_expr DOT mod_longident . DOTOP _*
/mk_longident_mod_longident_LIDENT_: mod_longident . DOT LIDENT
/mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT
/simple_expr: simple_expr DOT mod_longident . DOTOP _*

After a module path followed by a dot, the parser expects either another identifier (`LIDENT`/`UIDENT`) or a dot‑operator (`DOTOP`). A lone dot without the following component is invalid.

/fun_expr: simple_expr DOT mod_longident DOTOP . LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN LESSMINUS expr
/fun_expr: simple_expr DOT mod_longident DOTOP . LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS expr
/fun_expr: simple_expr DOT mod_longident DOTOP . LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS expr
/simple_expr: simple_expr DOT mod_longident DOTOP . LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN
/simple_expr: simple_expr DOT mod_longident DOTOP . LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE
/simple_expr: simple_expr DOT mod_longident DOTOP . LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET

After `mod_longident .+` (dot‑operator) the parser expects a parenthesized, brace‑ or bracket‑enclosed list of expressions. The opening token (`(`, `{`, `[`) must be followed by at least one expression before the closing token.

/fun_expr: simple_expr DOT mod_longident DOTOP LPAREN . separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN LESSMINUS expr
/simple_expr: simple_expr DOT mod_longident DOTOP LPAREN . separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN

After `.(mod_longident .+ (` the parser expects an expression inside the parentheses before the closing ‘)’.

/fun_expr: simple_expr DOT mod_longident DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ . RPAREN LESSMINUS expr
/simple_expr: simple_expr DOT mod_longident DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ . RPAREN

After the list of expressions inside `.(mod_longident .+ (` the parser expects the closing ‘)’. The ‘)’ is missing.

/fun_expr: simple_expr DOT mod_longident DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN LESSMINUS . expr

After the closing ‘)’ of `.(mod_longident .+ (` the parser expects the assignment operator “<-”. The ‘<-’ token is missing.

/fun_expr: simple_expr DOT mod_longident DOTOP LBRACKET . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS expr
/simple_expr: simple_expr DOT mod_longident DOTOP LBRACKET . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET

After `.(mod_longident .+ [` the parser expects an expression before the closing ‘]’.

/fun_expr: simple_expr DOT mod_longident DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACKET LESSMINUS expr
/simple_expr: simple_expr DOT mod_longident DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACKET

After the expression list inside `.(mod_longident .+ [` the parser expects the closing ‘]’. The ‘]’ is missing.

/fun_expr: simple_expr DOT mod_longident DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS . expr

After the closing ‘]’ of `.(mod_longident .+ [` the parser expects the assignment operator “<-”. The ‘<-’ token is missing.

/fun_expr: simple_expr DOT mod_longident DOTOP LBRACE . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS expr
/simple_expr: simple_expr DOT mod_longident DOTOP LBRACE . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE

After `.(mod_longident .+ {` the parser expects an expression before the closing ‘}’.

/fun_expr: simple_expr DOT mod_longident DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACE LESSMINUS expr
/simple_expr: simple_expr DOT mod_longident DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACE

After the expression list inside `.(mod_longident .+ {` the parser expects the closing ‘}’. The ‘}’ is missing.

/fun_expr: simple_expr DOT mod_longident DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS . expr

After the closing ‘}’ of `.(mod_longident .+ {` the parser expects the assignment operator “<-”. The ‘<-’ token is missing.

/fun_expr: simple_expr DOT label_longident LESSMINUS . expr

After a label (`x`) and a dot, the parser expects the assignment operator “<-”. The ‘<-’ token is missing.

/labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn . COMMA _*
/letop_binding_body: pattern_no_exn . EQUAL seq_expr
/reversed_labeled_tuple_pattern_pattern_no_exn_: pattern_no_exn . COMMA DOTDOT
/letop_binding_body: pattern_no_exn EQUAL . seq_expr

A pattern without exception followed by a comma (or `, ..`) may continue a tuple, or be followed by `=` to start a binding. The parser expects either ‘,’, ‘, ..’, or ‘=’ after the pattern, not the listed tokens.

/fun_expr: LETOP letop_bindings . IN seq_expr

After a `let*` binding list the keyword `in` must appear. The parser expects `in` before the subsequent expression.

/fun_expr: LETOP letop_bindings IN . seq_expr

After `let* … in` the parser expects a sequence expression. A valid expression must follow `in`.

/letop_bindings: letop_bindings ANDOP . letop_binding_body

After `and` in a `let*` chain the parser expects another binding. The `and` must be followed by a `letop_binding_body`.

/simple_expr: mod_longident DOT LPAREN seq_expr . RPAREN

After a module field access with parentheses (`M.x(`) the parser expects a closing ‘)’. The ‘)’ is missing.

/simple_expr: mod_longident DOT LBRACKETBAR . separated_or_terminated_nonempty_list_SEMI_expr_ BARRBRACKET
/simple_expr: mod_longident DOT LBRACKETBAR . BARRBRACKET

After a module field access with `[| … |]` the parser expects the opening “[`|” to be followed either by a list of expressions or directly by the closing “|]”. A missing expression list causes the error.

/simple_expr: mod_longident DOT LBRACKETBAR separated_or_terminated_nonempty_list_SEMI_expr_ . BARRBRACKET

After the expression list inside `M.x[| … |]` the parser expects the closing “|]”. The ‘|]’ token is missing.

/simple_expr: mod_longident DOT LBRACKET . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET
/simple_expr: mod_longident DOT LBRACKET . RBRACKET

After a module field access with brackets (`M.x[`), the parser expects either a list of expressions followed by ‘]’ or an empty list (`]`). A missing expression list triggers the error.

/simple_expr: mod_longident DOT LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACKET

After the expression list inside `M.x[ … ]` the parser expects the closing ‘]’. The ‘]’ is missing.

/simple_expr: mod_longident DOT LBRACKETLESS . separated_or_terminated_nonempty_list_SEMI_object_expr_field_ GREATERRBRACE

After a module field access with `{<` the parser expects a list of object fields before the closing ‘>}’. A missing field list causes the error.

/simple_expr: mod_longident DOT LBRACKETLESS separated_or_terminated_nonempty_list_SEMI_object_expr_field_ . GREATERRBRACE

After the object field list inside `M.x{< … >}` the parser expects the closing ‘>}’. The ‘>}’ token is missing.

/simple_expr: mod_longident DOT LBRACE . record_expr_content RBRACE

After a module field access with `{` the parser expects a record expression before the closing ‘}’. A missing record content leads to the error.

/record_expr_content: simple_expr . WITH separated_or_terminated_nonempty_list_SEMI_record_expr_field_

After a record expression, the `with` keyword may start a record update. The parser expects `with` after the record expression; other tokens are invalid.

/record_expr_content: simple_expr WITH . separated_or_terminated_nonempty_list_SEMI_record_expr_field_

After `with` in a record update the parser expects a list of record fields. A field list must follow `with`.

/floating_attribute: LBRACKETATATAT . attr_id attr_payload RBRACKET

A floating attribute begins with `[@@@`. The parser expects an attribute identifier after `[@@@`.

/structure_item: INCLUDE ext list_attribute_ . module_expr list_post_item_attribute_

After the keyword `include` the parser expects a module expression to include. A module expression must follow `include` (optionally preceded by attributes).

/primitive_declaration: EXTERNAL ext list_attribute_ . val_ident COLON possibly_poly_core_type_ EQUAL nonempty_list_raw_string_ list_post_item_attribute_

After the keyword `external` the parser expects a value name. A `val_ident` must follow `external` (optionally preceded by attributes).

/primitive_declaration: EXTERNAL ext list_attribute_ val_ident . COLON possibly_poly_core_type_ EQUAL nonempty_list_raw_string_ list_post_item_attribute_

After the external name the parser expects a colon introducing the type. A `:` must follow the identifier.

/primitive_declaration: EXTERNAL ext list_attribute_ val_ident COLON . possibly_poly_core_type_ EQUAL nonempty_list_raw_string_ list_post_item_attribute_

After the colon the parser expects a type specification. A type must follow `:`.

/primitive_declaration: EXTERNAL ext list_attribute_ val_ident COLON possibly_poly_core_type_ . EQUAL nonempty_list_raw_string_ list_post_item_attribute_

After the type the parser expects an equals sign before the external implementation strings. ‘=’ must follow the type.

/primitive_declaration: EXTERNAL ext list_attribute_ val_ident COLON possibly_poly_core_type_ EQUAL . nonempty_list_raw_string_ list_post_item_attribute_

After the equals sign the parser expects at least one string literal representing the external name(s). A string literal must follow `=`.

/sig_exception_declaration: EXCEPTION ext list_attribute_ . constr_ident generalized_constructor_arguments list_attribute_ list_post_item_attribute_
/str_exception_declaration: EXCEPTION ext list_attribute_ . constr_ident EQUAL constr_longident list_attribute_ list_post_item_attribute_

After the keyword `exception` the parser expects a constructor identifier. A constructor name must follow `exception` (optionally preceded by attributes).

/constr_extra_nonprefix_ident: LPAREN . RPAREN
/constr_ident: LPAREN . COLONCOLON RPAREN

After an opening parenthesis in an exception declaration, the parser expects either a closing parenthesis (for a bare `(`) or the token `::` followed by `)`. `(` must be closed appropriately.

/constr_ident: LPAREN COLONCOLON . RPAREN

After `(::` the parser expects a closing parenthesis. The pattern `(::)` must be completed.

/constr_extra_nonprefix_ident: LBRACKET . RBRACKET

After `[` in a constructor identifier, the parser expects a closing `]`. `[` must be closed.

/generalized_constructor_arguments: OF . constructor_arguments

After `of` in a constructor definition, the parser expects the argument type description. A `constructor_arguments` must follow `of`.

/label_declaration: mutable_flag . LIDENT COLON possibly_poly_core_type_no_attr_ list_attribute_
/label_declaration_semi: mutable_flag . LIDENT COLON possibly_poly_core_type_no_attr_ list_attribute_ SEMI list_attribute_

A record field declaration begins with an optional mutability keyword. After `mutable`/`virtual` the parser expects the field name (`LIDENT`).

/label_declaration: mutable_flag LIDENT . COLON possibly_poly_core_type_no_attr_ list_attribute_
/label_declaration_semi: mutable_flag LIDENT . COLON possibly_poly_core_type_no_attr_ list_attribute_ SEMI list_attribute_

After a mutable flag and a field name, the parser expects a colon introducing the field’s type. ‘:’ must follow the identifier.

/label_declaration: mutable_flag LIDENT COLON . possibly_poly_core_type_no_attr_ list_attribute_
/label_declaration_semi: mutable_flag LIDENT COLON . possibly_poly_core_type_no_attr_ list_attribute_ SEMI list_attribute_

After the colon in a record field the parser expects the field’s type. A type must follow ‘:’.

/possibly_poly_core_type_no_attr_: reversed_nonempty_llist_typevar_ . DOT alias_type

When a polymorphic type variable list is followed by a dot, an alias type must follow. The parser expects the actual type after the dot.

/possibly_poly_core_type_no_attr_: reversed_nonempty_llist_typevar_ DOT . alias_type

After the dot separating type variables from the aliased type, the parser expects the alias type. A concrete type must follow the dot.

/constructor_arguments: LBRACE label_declarations . RBRACE

A record argument in a constructor must be enclosed in braces. The parser expects the closing ‘}’ after the label declarations.

/constructor_arguments: reversed_separated_nonempty_llist_STAR_atomic_type_ . STAR atomic_type
/reversed_separated_nonempty_llist_STAR_atomic_type_: reversed_separated_nonempty_llist_STAR_atomic_type_ STAR . atomic_type

When using the `*` operator in constructor arguments, the parser expects another atomic type after the `*`. An atomic type must follow the star.

/str_exception_declaration: EXCEPTION ext list_attribute_ constr_ident EQUAL . constr_longident list_attribute_ list_post_item_attribute_

After `=` in an exception definition, the parser expects the exception’s long identifier. A `constr_longident` must follow `=`.

/constr_extra_nonprefix_ident: LPAREN . RPAREN
/constr_longident: LPAREN . COLONCOLON RPAREN

A parenthesized constructor identifier must be closed properly. Either `()` or `( :: )` is required.

/constr_longident: mod_longident DOT . LPAREN COLONCOLON RPAREN
/mk_longident_mod_longident_LIDENT_: mod_longident . DOT LIDENT
/mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT

After a module qualifier and a dot, the parser expects either an identifier (`LIDENT`/`UIDENT`) or the special constructor token `(::)`. A component must follow the dot.

/constr_longident: mod_longident DOT LPAREN . COLONCOLON RPAREN

After `mod_longident.` and an opening parenthesis, the parser expects `::` before the closing parenthesis. `(::)` must be completed.

/generalized_constructor_arguments: COLON . constructor_arguments MINUSGREATER atomic_type
/generalized_constructor_arguments: COLON . reversed_nonempty_llist_typevar_ DOT constructor_arguments MINUSGREATER atomic_type
/generalized_constructor_arguments: COLON . atomic_type
/generalized_constructor_arguments: COLON . reversed_nonempty_llist_typevar_ DOT atomic_type

After a colon in a generalized constructor argument, the parser expects either a type (possibly with type variables) or a full argument description followed by `->`. A type or argument must follow `:`.

/generalized_constructor_arguments: COLON reversed_nonempty_llist_typevar_ . DOT _*

After a colon and type variables, a dot must be followed by a type. The parser expects the concrete type after the dot.

/generalized_constructor_arguments: COLON reversed_nonempty_llist_typevar_ DOT . constructor_arguments MINUSGREATER atomic_type
/generalized_constructor_arguments: COLON reversed_nonempty_llist_typevar_ DOT . atomic_type

After the dot separating type variables from the rest of the constructor argument, the parser expects either a constructor argument list or a plain atomic type. A type must follow the dot.

/generalized_constructor_arguments: COLON reversed_nonempty_llist_typevar_ DOT constructor_arguments . MINUSGREATER atomic_type

After the constructor argument list, the parser expects the arrow `->`. ‘->’ must follow the argument list.

/generalized_constructor_arguments: COLON reversed_nonempty_llist_typevar_ DOT constructor_arguments MINUSGREATER . atomic_type

After the arrow in a generalized constructor argument, the parser expects the result type. A type must follow `->`.

/generalized_constructor_arguments: COLON constructor_arguments . MINUSGREATER atomic_type

After a plain argument list, the parser expects the arrow `->`. ‘->’ must follow the argument description.

/generalized_constructor_arguments: COLON constructor_arguments MINUSGREATER . atomic_type

After the arrow, the parser expects the result type. A type must follow `->`.

/open_description: OPEN BANG ext list_attribute_ . mod_ext_longident list_post_item_attribute_
/open_description: OPEN ext list_attribute_ . mod_ext_longident list_post_item_attribute_

After `open` (optionally with `!`) the parser expects a module extension long identifier. A module name must follow `open`.

/module_type_declaration: MODULE TYPE ext list_attribute_ . ident option_preceded_EQUAL_module_type__ list_post_item_attribute_
/module_type_subst: MODULE TYPE ext list_attribute_ . ident COLONEQUAL module_type list_post_item_attribute_

After `module type` the parser expects an identifier naming the module type. An identifier must follow the keyword pair.

/module_type_subst: MODULE TYPE ext list_attribute_ ident COLONEQUAL . module_type list_post_item_attribute_

After the `=` in a module type substitution, the parser expects a module type expression. A `module_type` must follow `=`.

/module_subst: MODULE ext list_attribute_ . UIDENT COLONEQUAL mod_ext_longident list_post_item_attribute_
/signature_item: MODULE ext list_attribute_ . module_name _*
/signature_item: MODULE ext list_attribute_ . REC module_name COLON module_type list_post_item_attribute_ list_and_module_declaration_

After `module` (optionally `rec`) the parser expects a module name. A `module_name` must follow the keyword.

/module_subst: MODULE ext list_attribute_ UIDENT COLONEQUAL . mod_ext_longident list_post_item_attribute_

After `=` in a module substitution, the parser expects a module extension long identifier. A `mod_ext_longident` must follow `=`.

/signature_item: MODULE ext list_attribute_ REC . module_name COLON module_type list_post_item_attribute_ list_and_module_declaration_

After the `rec` keyword, the parser expects a module name. A `module_name` must follow `rec` before the colon.

/signature_item: MODULE ext list_attribute_ REC module_name . COLON module_type list_post_item_attribute_ list_and_module_declaration_

After a module name in a `module rec` declaration, the parser expects a colon introducing the module type. ‘:’ must follow the name.

/list_and_module_declaration_: AND list_attribute_ . module_name COLON module_type list_post_item_attribute_ list_and_module_declaration_

After `and` in a module declaration list, the parser expects another module declaration (`module_name : module_type`). A new module name must follow `and`.

/list_and_module_declaration_: AND list_attribute_ module_name . COLON module_type list_post_item_attribute_ list_and_module_declaration_

After a module name following `and`, the parser expects a colon before the module type. ‘:’ must follow the module name.

/signature_item: MODULE ext list_attribute_ module_name . module_declaration_body list_post_item_attribute_
/signature_item: MODULE ext list_attribute_ module_name EQUAL . mod_longident list_post_item_attribute_

After a module name in a signature, the parser expects either a module body (starting with `:` or `=`) or an equals sign followed by a module path. `:`/`=` must follow the name, and after `=` a module identifier is required.

/module_declaration_body: COLON . module_type

After a module name, a colon must introduce the module type. The parser expects a `module_type` after ‘:’.

/module_declaration_body: functor_arg . module_declaration_body

In a functor declaration, after a functor argument the parser expects another functor argument or the module type body. A subsequent `functor_arg` or a colon must follow the first argument.

/signature_item: INCLUDE ext list_attribute_ . module_type list_post_item_attribute_

After `include` the parser expects a module type expression to include. A `module_type` must follow `include`.

/sig_exception_declaration: EXCEPTION ext list_attribute_ . constr_ident generalized_constructor_arguments list_attribute_ list_post_item_attribute_
/str_exception_declaration: EXCEPTION ext list_attribute_ . constr_ident EQUAL constr_longident list_attribute_ list_post_item_attribute_

After `exception` the parser expects a constructor identifier (and possibly arguments). A constructor name must follow `exception`.

/formal_class_parameters: LBRACKET reversed_separated_nonempty_llist_COMMA_type_parameter_ . RBRACKET

In a class declaration, after `[` a list of type parameters must be closed with `]`. The parser expects the closing bracket after the parameters.

/class_type_declarations: CLASS TYPE ext list_attribute_ virtual_flag formal_class_parameters . LIDENT EQUAL class_signature list_post_item_attribute_ list_and_class_type_declaration_

After the optional class type parameter list, the parser expects a class name (`LIDENT`). A class identifier must follow the parameter list.

/class_type_declarations: CLASS TYPE ext list_attribute_ virtual_flag formal_class_parameters LIDENT . COLON class_type list_post_item_attribute_ list_and_class_type_declaration_

After the class name, a colon must introduce the class type. ‘:’ must follow the class identifier.

/class_self_type: LPAREN . core_type RPAREN
/class_self_type: LPAREN core_type . RPAREN

Inside an object type, after an opening parenthesis the parser expects a core type before the closing parenthesis. A type must be provided inside the parentheses.

/class_sig_field: VAL list_attribute_ mutable_virtual_flags . LIDENT COLON core_type list_post_item_attribute_

After `val` and optional mutability/virtual flags, the parser expects a field name. An identifier must follow `val`.

/class_sig_field: VAL list_attribute_ mutable_virtual_flags LIDENT . COLON core_type list_post_item_attribute_

After a field name in a class signature, the parser expects a colon before the field’s type. ‘:’ must follow the identifier.

/class_sig_field: METHOD list_attribute_ private_virtual_flags . LIDENT COLON possibly_poly_core_type_ list_post_item_attribute_

After `method` and optional privacy/virtual flags, the parser expects a method name. An identifier must follow `method`.

/class_sig_field: METHOD list_attribute_ private_virtual_flags LIDENT . COLON possibly_poly_core_type_ list_post_item_attribute_

After a method name, the parser expects a colon before the method’s type. ‘:’ must follow the identifier.

/class_sig_field: INHERIT list_attribute_ . class_signature list_post_item_attribute_

After the `inherit` keyword, a class signature must follow. A `class_signature` is required after `inherit`.

/class_signature: LET . OPEN _*

After `let` inside a class signature, the parser expects `open`. `open` must follow `let` when starting a let‑open declaration.

/class_signature: LET OPEN BANG list_attribute_ . mod_longident IN class_signature

After `let open !` the parser expects a module name to open. A `mod_longident` must follow the `!`.

/class_signature: LET OPEN BANG list_attribute_ mod_longident . IN class_signature
/mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT

After a module name in a `let open !` declaration, the parser expects `in` to end the open. `in` must follow the module identifier.

/class_signature: LET OPEN BANG list_attribute_ mod_longident IN . class_signature

After `in` in a `let open !` block, the parser expects the rest of the class signature. A class signature must follow `in`.

/class_signature: LBRACKET . reversed_separated_nonempty_llist_COMMA_core_type_ RBRACKET clty_longident
/delimited_type_supporting_local_open: LBRACKET . tag_field RBRACKET
/delimited_type_supporting_local_open: LBRACKET . BAR reversed_separated_nonempty_llist_BAR_row_field_ RBRACKET
/delimited_type_supporting_local_open: LBRACKET . row_field BAR reversed_separated_nonempty_llist_BAR_row_field_ RBRACKET
/delimited_type_supporting_local_open: LBRACKETGREATER . option_BAR_ reversed_separated_nonempty_llist_BAR_row_field_ RBRACKET
/delimited_type_supporting_local_open: LBRACKETGREATER RBRACKET
/delimited_type_supporting_local_open: LBRACKETLESS . option_BAR_ reversed_separated_nonempty_llist_BAR_row_field_ RBRACKET
/delimited_type_supporting_local_open: LBRACKETLESS option_BAR_ reversed_separated_nonempty_llist_BAR_row_field_ GREATER reversed_nonempty_llist_name_tag_ RBRACKET

After an opening bracket in a type expression, the parser expects a type description (e.g., a list of core types, a tag field, row fields, etc.) before the closing bracket. A valid type component must follow ‘[’ (or `[>`, `[<`).

/class_type: LIDENT COLON . tuple_type MINUSGREATER class_type
/class_type: LIDENT COLON tuple_type . MINUSGREATER class_type
/class_type: LIDENT COLON tuple_type MINUSGREATER . class_type

After a class type parameter (`LIDENT :`) the parser expects a tuple type, then `->` and the resulting class type. Both the tuple type and the arrow must be present.

/class_type: optlabel tuple_type MINUSGREATER . class_type
/class_type: LIDENT COLON tuple_type MINUSGREATER . class_type

After the `->` in a class type, the parser expects another class type (for higher‑arity arrows). A subsequent `class_type` must follow the arrow.

/class_type: optlabel LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER class_type

After a type variable list in a class type, the parser expects a core type inside the parentheses and then `->`. Both the core type and the arrow are required.

/class_type: optlabel LIDENT COLON LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER class_type

Same as above, but with an explicit label name before the type variables. The components after the colon must form a valid function‑type arrow.

/class_type: optlabel tuple_type . MINUSGREATER class_type

Expected `->` after the optional label and tuple type in a class type declaration.

/class_type: optlabel tuple_type MINUSGREATER . class_type

Expected a class type after the `->` in a class type declaration.

/list_and_class_description_: AND list_attribute_ virtual_flag formal_class_parameters . LIDENT COLON class_type list_post_item_attribute_ list_and_class_description_

Expected a class name (identifier) after `and` in a class description list.

/list_and_class_description_: AND list_attribute_ virtual_flag formal_class_parameters LIDENT . COLON class_type list_post_item_attribute_ list_and_class_description_

Expected `:` after the class name in a class description list.

/list_and_class_description_: AND list_attribute_ virtual_flag formal_class_parameters LIDENT COLON . class_type list_post_item_attribute_ list_and_class_description_

Expected a class type after `:` in a class description list.

/list_generic_and_type_declaration_type_kind__: AND list_attribute_ type_parameters . LIDENT type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_ list_generic_and_type_declaration_type_kind__

Expected a type name (identifier) after `and` in a type declaration list.

/list_generic_and_type_declaration_type_subst_kind__: AND list_attribute_ type_parameters . LIDENT COLONEQUAL nonempty_type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_ list_generic_and_type_declaration_type_subst_kind__

Expected a type name (identifier) after `and` in a type substitution declaration list.

/list_generic_and_type_declaration_type_subst_kind__: AND list_attribute_ type_parameters LIDENT . COLONEQUAL nonempty_type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_ list_generic_and_type_declaration_type_subst_kind__

Expected `:=` after the type name in a type substitution declaration.

/list_generic_and_type_declaration_type_subst_kind__: AND list_attribute_ type_parameters LIDENT COLONEQUAL . nonempty_type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_ list_generic_and_type_declaration_type_subst_kind__

Expected a type kind after `:=` in a type substitution declaration.

/constr_extra_nonprefix_ident: LBRACKET . RBRACKET
delimited_type_supporting_local_open: LBRACKET . tag_field RBRACKET
delimited_type_supporting_local_open: LBRACKET . BAR reversed_separated_nonempty_llist_BAR_row_field_ RBRACKET
delimited_type_supporting_local_open: LBRACKET . row_field BAR reversed_separated_nonempty_llist_BAR_row_field_ RBRACKET

Expected `]` (or a tag/row field) after `[` in a type expression.

/nonempty_type_kind: LBRACE label_declarations . RBRACE

Expected `}` after the record field declarations in a type definition.

/nonempty_type_kind: EXTERNAL . STRING

Expected a string literal after `external` in a type definition.

/generic_constructor_declaration_BAR_: BAR . constr_ident generalized_constructor_arguments list_attribute_

Expected a constructor identifier after `|` in an extensible type declaration.

/nonempty_type_kind: core_type EQUAL . constructor_declarations
nonempty_type_kind: core_type EQUAL . PRIVATE constructor_declarations
nonempty_type_kind: core_type EQUAL . DOTDOT
nonempty_type_kind: core_type EQUAL . PRIVATE DOTDOT
nonempty_type_kind: core_type EQUAL . LBRACE label_declarations RBRACE
nonempty_type_kind: core_type EQUAL . PRIVATE LBRACE label_declarations RBRACE

Expected one of `{ … }`, `private`, `..`, or a list of constructors after `=` in a type definition.

/nonempty_type_kind: core_type EQUAL PRIVATE . constructor_declarations
nonempty_type_kind: core_type EQUAL PRIVATE . DOTDOT
nonempty_type_kind: core_type EQUAL PRIVATE . LBRACE label_declarations RBRACE

Expected constructors, `..`, or `{ … }` after the keyword `private` in a type definition.

/nonempty_type_kind: core_type EQUAL PRIVATE LBRACE label_declarations . RBRACE

Expected `}` after the record field declarations in a private type definition.

/nonempty_type_kind: core_type EQUAL LBRACE label_declarations . RBRACE

Expected `}` after the record field declarations in a type definition.

/local_structure_item: CLASS ext list_attribute_ virtual_flag formal_class_parameters . LIDENT class_fun_binding list_post_item_attribute_ list_and_class_declaration_

Expected a class name (identifier) after the class header in a class declaration.

/local_structure_item: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT . class_fun_binding list_post_item_attribute_ list_and_class_declaration_

Expected `=` or `:` after the class name in a class declaration.

/constr_extra_nonprefix_ident: LPAREN . RPAREN
constr_longident: LPAREN . COLONCOLON RPAREN
simple_param_pattern: LPAREN . pattern COLON reversed_nonempty_llist_typevar_ DOT core_type RPAREN
simple_pattern_not_ident: LPAREN . pattern RPAREN
simple_pattern_not_ident: LPAREN . MODULE _*
simple_pattern_not_ident: LPAREN . pattern COLON core_type RPAREN
val_extra_ident: LPAREN . operator RPAREN

Expected `)` after `(` (empty constructor argument).
Expected `::` after `(` for a polymorphic variant constructor.
Expected a pattern after `(` in a parameter pattern.
Expected a pattern after `(` in a simple pattern.
Expected the keyword `module` after `(` in a module pattern.
Expected a pattern after `(` when annotating a pattern with a type.
Expected an operator after `(` in a value identifier.

/class_fun_binding: EQUAL . class_expr

Expected a class expression after `=` in a class binding.

/value: BANG list_attribute_ mutable_flag . LIDENT _*

Expected a value name after `!` (mutable value) in a value definition.

/value: BANG list_attribute_ mutable_flag LIDENT . EQUAL seq_expr
value: BANG list_attribute_ mutable_flag LIDENT . type_constraint EQUAL seq_expr

Expected `=` after the mutable value name (or after its type constraint) in a value definition.

/value: BANG list_attribute_ mutable_flag LIDENT EQUAL . seq_expr

Expected an expression after `=` in a mutable value definition.

/value: BANG list_attribute_ mutable_flag LIDENT type_constraint . EQUAL seq_expr

Expected `=` after the type constraint in a mutable value definition.

/value: BANG list_attribute_ mutable_flag LIDENT type_constraint EQUAL . seq_expr

Expected an expression after `=` in a mutable value definition with a type constraint.

/value: list_attribute_ virtual_with_mutable_flag . LIDENT COLON core_type

Expected a value name after `virtual`/`mutable` in a virtual/mutable value definition.

/value: list_attribute_ virtual_with_mutable_flag LIDENT . COLON core_type

Expected `:` after the value name in a virtual/mutable value definition.

/value: list_attribute_ virtual_with_mutable_flag LIDENT COLON . core_type

Expected a core type after `:` in a virtual/mutable value definition.

/value: list_attribute_ mutable_flag . LIDENT _*

Expected a value name after the `mutable` flag in a value definition.

/value: list_attribute_ mutable_flag LIDENT . EQUAL seq_expr

Expected `=` after the mutable value name in a value definition.

/value: list_attribute_ mutable_flag LIDENT EQUAL . seq_expr

Expected an expression after `=` in a mutable value definition.

/value: list_attribute_ mutable_flag LIDENT type_constraint . EQUAL seq_expr

Expected `=` after the type constraint in a mutable value definition.

/value: list_attribute_ mutable_flag LIDENT type_constraint EQUAL . seq_expr

Expected an expression after `=` in a mutable value definition with a type constraint.

/method_: BANG list_attribute_ private_flag . LIDENT _*

Expected a method name after `!` (private method) in a method definition.

/method_: BANG list_attribute_ private_flag LIDENT . strict_binding
method_: BANG list_attribute_ private_flag LIDENT . COLON _*

Expected a method body (binding) or `:` after the method name in a private method definition.

/method_: BANG list_attribute_ private_flag LIDENT COLON . possibly_poly_core_type_ EQUAL seq_expr
method_: BANG list_attribute_ private_flag LIDENT COLON . TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL seq_expr

Expected a type after `:` (or after `type … .`) in a private method definition.

/method_: BANG list_attribute_ private_flag LIDENT COLON TYPE . nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL seq_expr

Expected a type constructor name after `type` in a private method definition.

/method_: BANG list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ . DOT core_type EQUAL seq_expr

Expected `.` after the type name in a private method definition.

/method_: BANG list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT . core_type EQUAL seq_expr

Expected a core type after `.` in a private method definition.

/method_: BANG list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type . EQUAL seq_expr

Expected `=` after the core type in a private method definition.

/method_: list_attribute_ virtual_with_private_flag . LIDENT COLON possibly_poly_core_type_

Expected a method name after `virtual`/`private` in a virtual method declaration.

/method_: list_attribute_ virtual_with_private_flag LIDENT . COLON possibly_poly_core_type_

Expected `:` after the virtual method name.

/method_: list_attribute_ virtual_with_private_flag LIDENT COLON . possibly_poly_core_type_

Expected a type after `:` in a virtual method declaration.

/method_: list_attribute_ private_flag . LIDENT _*

Expected a method name after the `private` flag in a method declaration.

/method_: list_attribute_ private_flag LIDENT . strict_binding
method_: list_attribute_ private_flag LIDENT . COLON _*

Expected a method body (binding) or `:` after the private method name.

/method_: list_attribute_ private_flag LIDENT COLON . possibly_poly_core_type_ EQUAL seq_expr
method_: list_attribute_ private_flag LIDENT COLON . TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL seq_expr

Expected a type after `:` (or after `type … .`) in a private method declaration.

/method_: list_attribute_ private_flag LIDENT COLON TYPE . nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL seq_expr

Expected a type constructor name after `type` in a private method declaration.

/method_: list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ . DOT core_type EQUAL seq_expr

Expected `.` after the type name in a private method declaration.

/method_: list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT . core_type EQUAL seq_expr

Expected a core type after `.` in a private method declaration.

/method_: list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type . EQUAL seq_expr

Expected `=` after the core type in a private method declaration.

/method_: list_attribute_ private_flag LIDENT COLON possibly_poly_core_type_ . EQUAL seq_expr

Expected `=` after the type in a private method declaration.

/method_: list_attribute_ private_flag LIDENT COLON possibly_poly_core_type_ EQUAL . seq_expr

Expected an expression after `=` in a private method declaration.

/class_field: INITIALIZER list_attribute_ . seq_expr list_post_item_attribute_

Expected an expression after `initializer` in a class field.

/class_field: INHERIT BANG list_attribute_ . class_expr option_preceded_AS_mkrhs_LIDENT___ list_post_item_attribute_

Expected a class expression after `inherit !` in a class field.

/class_simple_expr: LPAREN . class_expr _*

Expected a class expression after `(` in a class simple expression.

/class_expr: LET OPEN BANG list_attribute_ . mod_longident IN class_expr

Expected a module identifier after `let open !` in a class expression.

/class_expr: LET OPEN BANG list_attribute_ mod_longident . IN class_expr
mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT

Expected `in` after the module identifier in a `let open !` class expression.

/class_expr: LET OPEN BANG list_attribute_ mod_longident IN . class_expr

Expected a class expression after `in` in a `let open ! … in` construct.

/class_simple_expr: LBRACKET . reversed_separated_nonempty_llist_COMMA_core_type_ RBRACKET class_longident

Expected a type (or a list of types) after `[` in a class simple expression.

/class_simple_expr: LBRACKET reversed_separated_nonempty_llist_COMMA_core_type_ . RBRACKET class_longident

Expected `]` after the list of types in a class simple expression.

/class_simple_expr: LBRACKET reversed_separated_nonempty_llist_COMMA_core_type_ RBRACKET . class_longident

Expected a class long identifier after the closing `]` in a class simple expression.

/class_expr: FUN list_attribute_ . class_fun_def

Expected a function definition after `fun` in a class expression.

/class_fun_def: simple_param_pattern . MINUSGREATER class_expr
class_fun_def: simple_param_pattern . class_fun_def

Expected `->` after the parameter pattern in a class function definition (or another parameter pattern if curried).

/class_fun_def: simple_param_pattern MINUSGREATER . class_expr

Expected a class expression after `->` in a class function definition.

/class_expr: let_bindings_no_ext_ . IN class_expr

Expected `in` after the let‑bindings in a class expression.

/class_expr: let_bindings_no_ext_ IN . class_expr

Expected a class expression after `in` in a let‑binding within a class.

/class_expr: LET OPEN list_attribute_ . mod_longident IN class_expr

Expected a module identifier after `let open` in a class expression.

/class_expr: LET OPEN list_attribute_ mod_longident . IN class_expr
mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT

Expected `in` after the module identifier in a `let open` class expression.

/class_expr: LET OPEN list_attribute_ mod_longident IN . class_expr

Expected a class expression after `in` in a `let open … in` construct.

/let_bindings_ext_: LET ext list_attribute_ rec_flag . let_binding_body list_post_item_attribute_

Expected a let‑binding body after `let` (or `let rec`) in a top‑level let binding.

/floating_attribute: LBRACKETATATAT attr_id attr_payload . RBRACKET

Expected `]` after the floating attribute payload.

/item_extension: LBRACKETPERCENTPERCENT attr_id payload . RBRACKET

Expected `]` after the extension payload.

/fun_expr: LET ext list_attribute_ local_structure_item . IN seq_expr

Expected `in` after a local structure item in a let‑expression.

/fun_expr: LET ext list_attribute_ local_structure_item IN . seq_expr

Expected an expression after `in` in a let‑expression.

/simple_param_pattern: QUESTION LPAREN label_let_pattern option_preceded_EQUAL_seq_expr__ . RPAREN

Expected `)` after the optional default expression in an optional argument pattern.

/simple_expr: LBRACKETBAR separated_or_terminated_nonempty_list_SEMI_expr_ . BARRBRACKET

Expected `]|` (closing bracket) after the list of expressions inside `[| …`.

/simple_expr: LPAREN MODULE ext list_attribute_ . module_expr _*

Expected a module expression after `(` `module` in a packed module expression.

/simple_expr: LPAREN MODULE ext list_attribute_ module_expr . RPAREN
simple_expr: LPAREN MODULE ext list_attribute_ module_expr . COLON module_type RPAREN

Expected `)` (or `:`) after the module expression in a packed module expression.

/simple_expr: LPAREN MODULE ext list_attribute_ module_expr COLON . module_type RPAREN

Expected a module type after `:` in a typed packed module expression.

/simple_expr: LPAREN MODULE ext list_attribute_ module_expr COLON module_type . RPAREN

Expected `)` after the module type in a typed packed module expression.

/simple_expr: LPAREN seq_expr . RPAREN
simple_expr: LPAREN seq_expr . type_constraint RPAREN

Expected `)` (or a type constraint) after the sequence expression in a parenthesized expression.

/simple_expr: LPAREN seq_expr type_constraint . RPAREN

Expected `)` after the type constraint in a parenthesized expression with a type annotation.

/fun_expr: MATCH ext list_attribute_ seq_expr . WITH reversed_preceded_or_separated_nonempty_llist_BAR_match_case_

Expected `with` after the expression being matched in a match expression.

/fun_expr: MATCH ext list_attribute_ seq_expr WITH . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_

Expected a match case after `with` in a match expression.

/simple_expr: METAOCAML_BRACKET_OPEN seq_expr . METAOCAML_BRACKET_CLOSE

Expected `]` (or appropriate closing token) after the meta‑OCaml bracket expression.

/paren_module_expr: LPAREN VAL list_attribute_ expr_colon_package_type . RPAREN

Expected `)` after a value package type expression in a parenthesized module expression.

/expr_colon_package_type: expr COLONGREATER . module_type

Expected a module type after `:>` in an expression‑colon‑package type.

/expr_colon_package_type: expr COLON . module_type _*

Expected a module type after `:` in an expression‑colon‑package type.

/expr_colon_package_type: expr COLON module_type COLONGREATER . module_type

Expected `:`> after the first module type in a double‑colon package type.

/attribute: LBRACKETAT attr_id attr_payload . RBRACKET

Expected `]` after an attribute payload.

/fun_expr: WHILE ext list_attribute_ . seq_expr DO seq_expr DONE

Expected a condition expression after `while` in a while loop.

/fun_expr: WHILE ext list_attribute_ seq_expr . DO seq_expr DONE

Expected `do` after the while condition.

/fun_expr: WHILE ext list_attribute_ seq_expr DO . seq_expr DONE

Expected the loop body after `do` in a while loop.

/fun_expr: WHILE ext list_attribute_ seq_expr DO seq_expr . DONE

Expected `done` after the loop body in a while loop.

/implementation: structure . EOF

Expected end‑of‑file after the implementation structure (or more structure items).

/interface: signature . EOF

Expected end‑of‑file after the interface signature (or more signature items).

/parse_any_longident': . parse_any_longident

Expected a long identifier after the start of a long identifier parser.

/constr_extra_nonprefix_ident: LPAREN . RPAREN
mk_longident_mod_ext_longident___anonymous_42_: LPAREN . COLONCOLON RPAREN
val_extra_ident: LPAREN . operator RPAREN

Expected `)` after `(` for an empty constructor argument, `::` after `(` for a polymorphic variant constructor, or an operator after `(` in a value identifier.

/mk_longident_mod_ext_longident___anonymous_42_: LPAREN COLONCOLON . RPAREN

Expected `)` after `(::` in a polymorphic variant long identifier.

/mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident . DOT UIDENT
mk_longident_mod_ext_longident___anonymous_42_: mod_ext_longident . DOT _*

Expected a component after `.` in a module‑extended long identifier (either an identifier or further continuation).

/mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident DOT . UIDENT
mk_longident_mod_ext_longident___anonymous_42_: mod_ext_longident DOT . ident
mk_longident_mod_ext_longident___anonymous_42_: mod_ext_longident DOT . LPAREN COLONCOLON RPAREN
mk_longident_mod_ext_longident___anonymous_42_: mod_ext_longident DOT . val_extra_ident

Expected an identifier, another long identifier, `(::)` or a parenthesized operator after `.` in a module‑extended long identifier.

/mk_longident_mod_ext_longident___anonymous_42_: mod_ext_longident DOT LPAREN . COLONCOLON RPAREN
val_extra_ident: LPAREN . operator RPAREN

Expected `::` after `(` in a module‑extended long identifier, or an operator after `(`.

/mk_longident_mod_ext_longident___anonymous_42_: mod_ext_longident DOT LPAREN COLONCOLON . RPAREN

Expected `)` after `(::` in a module‑extended long identifier.

/parse_any_longident: any_longident . EOF

Expected end‑of‑file after a top‑level long identifier (or more tokens).

/parse_constr_longident': . parse_constr_longident

Expected a constructor long identifier after the start of the parser.

/parse_constr_longident: constr_longident . EOF

Expected end‑of‑file after a constructor long identifier.

/parse_core_type': . parse_core_type

Expected a core type after the start of the parser.

/parse_core_type: core_type . EOF

Expected end‑of‑file after a core type.

/parse_expression': . parse_expression

Expected an expression after the start of the parser.

/parse_expression: seq_expr . EOF

Expected end‑of‑file after a top‑level expression (or more expressions).

/parse_mod_ext_longident': . parse_mod_ext_longident

Expected a module extended long identifier after the start of the parser.

/parse_mod_ext_longident: mod_ext_longident . EOF

Expected end‑of‑file after a module extended long identifier.

/parse_mod_longident': . parse_mod_longident

Expected a module long identifier after the start of the parser.

/parse_mod_longident: mod_longident . EOF

Expected end‑of‑file after a module long identifier.

/parse_module_expr': . parse_module_expr

Expected a module expression after the start of the parser.

/parse_module_expr: module_expr . EOF

Expected end‑of‑file after a module expression.

/parse_module_type': . parse_module_type

Expected a module type after the start of the parser.

/parse_module_type: module_type . EOF

Expected end‑of‑file after a module type.

/parse_mty_longident': . parse_mty_longident

Expected a module type long identifier after the start of the parser.

/parse_mty_longident: mty_longident . EOF

Expected end‑of‑file after a module type long identifier.

/parse_val_longident': . parse_val_longident

Expected a value long identifier after the start of the parser.

/parse_val_longident: val_longident . EOF

Expected end‑of‑file after a value long identifier.

/toplevel_directive: HASH . ident _*

Expected an identifier after `#` in a toplevel directive.

/toplevel_phrase: toplevel_directive . SEMISEMI

Expected `;;` after a toplevel directive.

/toplevel_phrase: seq_expr list_post_item_attribute_ . SEMISEMI

Expected `;;` after a top‑level expression (or after its attributes).

/toplevel_phrase: list_text_str_structure_item__ . SEMISEMI

Expected `;;` after a top‑level structure item (or after its attributes).
