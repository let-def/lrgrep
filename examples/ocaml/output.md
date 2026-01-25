/implementation': . implementation
Expected a structure (e.g. `struct … end`) or end‑of‑file after the start of an implementation.

---
/fun_expr: WHILE . ext list_attribute_ seq_expr DO seq_expr DONE
Expected a condition expression after `while`.

---
/ext: PERCENT . attr_id
Expected an attribute identifier after `%`.

---
/attr_id: single_attr_id DOT . attr_id
Expected an identifier after `.` in an attribute path.

---
/attribute: LBRACKETAT . attr_id attr_payload RBRACKET
Expected an attribute identifier after `[@`.

---
/attribute: LBRACKETAT attr_id . attr_payload RBRACKET
Expected an identifier after `.` in the attribute `[@…`.

---
/value_description: VAL . ext list_attribute_ val_ident COLON possibly_poly_core_type_ list_post_item_attribute_
Expected a value name after `val`.

---
/value_description: VAL ext list_attribute_ . val_ident COLON possibly_poly_core_type_ list_post_item_attribute_
Expected a value name after the attribute list in a `val` declaration.

---
/val_extra_ident: LPAREN . operator RPAREN
Expected an operator after `(` in an extra identifier.

---
/operator: DOTOP . LPAREN _*
/operator: DOTOP . LBRACKET _*
/operator: DOTOP . LBRACE _*
Expected `(`, `[` or `{` after the dot‑operator (`.@`/`.(`/.`[`/.`{`).

---
/operator: DOTOP LPAREN . index_mod _*
Expected `;` or `..` (or a closing `)`) after `(` in a dot‑indexed expression.

---
/index_mod: SEMI . DOTDOT
Expected `..` after `;` in an index modifier.

---
/operator: DOTOP LPAREN index_mod . RPAREN _*
Expected `)` to close the indexed expression.

---
/operator: DOTOP LBRACKET . index_mod _*
Expected `;` or `..` (or a closing `]`) after `[` in a dot‑indexed expression.

---
/operator: DOTOP LBRACKET index_mod . RBRACKET _*
Expected `]` to close the indexed expression.

---
/operator: DOTOP LBRACE . index_mod _*
Expected `;` or `..` (or a closing `}`) after `{` in a dot‑indexed expression.

---
/operator: DOTOP LBRACE index_mod . RBRACE _*
Expected `}` to close the indexed expression.

---
/val_extra_ident: LPAREN operator . RPAREN
Expected `)` after the operator in an extra identifier.

---
/value_description: VAL ext list_attribute_ val_ident . COLON possibly_poly_core_type_ list_post_item_attribute_
Expected `:` after the value name in a `val` declaration.

---
/value_description: VAL ext list_attribute_ val_ident COLON . possibly_poly_core_type_ list_post_item_attribute_
Expected a type after `:` in a `val` declaration.

---
/atomic_type: QUOTE . ident
/reversed_nonempty_llist_typevar_: QUOTE . ident
Expected an identifier after the quote (`'`).

---
/optlabel: QUESTION . LIDENT COLON
Expected a label identifier after `?`.

---
/optlabel: QUESTION LIDENT . COLON
Expected `:` after the label name in an optional label.

---
/atomic_type: LPAREN . reversed_separated_nontrivial_llist_COMMA_core_type_ _*
/delimited_type_supporting_local_open: LPAREN . core_type RPAREN
/delimited_type_supporting_local_open: LPAREN . MODULE ext list_attribute_ module_type RPAREN
/function_type: LPAREN . reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER function_type
Expected a type expression after `(`.

---
/delimited_type_supporting_local_open: LPAREN MODULE . ext list_attribute_ module_type RPAREN
Expected an optional `%` attribute after `module` in a parenthesised module type.

---
/delimited_type_supporting_local_open: LPAREN MODULE ext list_attribute_ . module_type RPAREN
Expected a module type after the attribute list in a parenthesised module type.

---
/module_type: SIG . list_attribute_ signature END
Expected a signature (or attributes) after `sig`.

---
/generic_type_declaration_no_nonrec_flag_type_subst_kind_: TYPE . ext _*
/generic_type_declaration_nonrec_flag_type_kind_: TYPE . ext _*
/signature_item: TYPE . ext _*
Expected an optional attribute (`%…`) after `type`.

---
/generic_type_declaration_no_nonrec_flag_type_subst_kind_: TYPE ext list_attribute_ NONREC . type_parameters LIDENT COLONEQUAL …
/generic_type_declaration_nonrec_flag_type_kind_: TYPE ext list_attribute_ NONREC . type_parameters LIDENT type_kind …
/signature_item: TYPE ext list_attribute_ NONREC . type_parameters type_longident PLUSEQ …
Expected type parameters after `type nonrec`.

---
/type_parameters: LPAREN . reversed_separated_nonempty_llist_COMMA_type_parameter_ RPAREN
Expected a type parameter after `(`.

---
/type_parameter: type_variance . type_variable
Expected a type variable after the variance modifier.

---
/type_variable: QUOTE . ident
Expected an identifier after the quote (`'`).

---
/type_parameters: LPAREN reversed_separated_nonempty_llist_COMMA_type_parameter_ . RPAREN
Expected `)` to close the type‑parameter list.

---
/generic_type_declaration_no_nonrec_flag_type_subst_kind_: TYPE ext list_attribute_ NONREC type_parameters . LIDENT COLONEQUAL …
/generic_type_declaration_nonrec_flag_type_kind_: TYPE ext list_attribute_ NONREC type_parameters . LIDENT type_kind …
/signature_item: TYPE ext list_attribute_ NONREC type_parameters . type_longident PLUSEQ …
Expected the type name after the list of type parameters.

---
/type_kind: EQUAL . nonempty_type_kind
Expected a type definition after `=`.

---
/atomic_type: QUOTE . ident
Expected an identifier after the quote (`'`).

---
/nonempty_type_kind: PRIVATE . core_type
/nonempty_type_kind: PRIVATE . constructor_declarations
/nonempty_type_kind: PRIVATE . DOTDOT
/nonempty_type_kind: PRIVATE . LBRACE label_declarations RBRACE
Expected a type definition after `private`.

---
/atomic_type: LPAREN . reversed_separated_nontrivial_llist_COMMA_core_type_ _*
/constr_extra_nonprefix_ident: LPAREN . RPAREN
/constr_ident: LPAREN . COLONCOLON RPAREN
/delimited_type_supporting_local_open: LPAREN . core_type RPAREN
/delimited_type_supporting_local_open: LPAREN . MODULE ext list_attribute_ module_type RPAREN
/function_type: LPAREN . reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER function_type
Expected a type, module type, or constructor after `(`.

---
/function_type: LIDENT COLON . LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER function_type
/function_type: LIDENT COLON . tuple_type MINUSGREATER function_type
/function_type: LIDENT COLON . atomic_type _*
Expected a type after the colon in a labeled function type.

---
/atomic_type: LPAREN . reversed_separated_nontrivial_llist_COMMA_core_type_ _*
/delimited_type_supporting_local_open: LPAREN . core_type RPAREN
/delimited_type_supporting_local_open: LPAREN . MODULE ext list_attribute_ module_type RPAREN
/function_type: LIDENT COLON LPAREN . reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER function_type
Expected a type after `(` in a type annotation.

---
/object_type: LESS . meth_list GREATER
/object_type: LESS . GREATER
Expected a method list or `>` after `<` in an object type.

---
/object_type: LESS LPAREN . meth_list GREATER
Expected a method list or `>` after `<(` in an object type.

---
/extension: LBRACKETPERCENT . attr_id payload RBRACKET
Expected an attribute identifier after `[%`.

---
/generic_type_declaration_nonrec_flag_type_kind_: TYPE . ext _*
/local_structure_item: TYPE . ext _*
Expected an optional attribute after `type` (non‑rec version).

---
/generic_type_declaration_nonrec_flag_type_kind_: TYPE ext list_attribute_ NONREC . type_parameters LIDENT type_kind …
/local_structure_item: TYPE ext list_attribute_ NONREC . type_parameters type_longident PLUSEQ …
Expected the type‑parameter list after `type nonrec`.

---
/generic_type_declaration_nonrec_flag_type_kind_: TYPE ext list_attribute_ NONREC type_parameters . LIDENT type_kind …
/local_structure_item: TYPE ext list_attribute_ NONREC type_parameters . type_longident PLUSEQ …
Expected the type name after the type‑parameter list in a `type nonrec` declaration.

---
/post_item_attribute: LBRACKETATAT . attr_id attr_payload RBRACKET
Expected an attribute identifier after `[@@`.

---
/post_item_attribute: LBRACKETATAT attr_id . attr_payload RBRACKET
Expected an identifier after `.` in a `[@@…` attribute.

---

/fun_expr: TRY . ext list_attribute_ seq_expr WITH reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expected an expression after `try`.
/fun_expr: TRY ext list_attribute_ . seq_expr WITH reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expected a `try … with` expression after the optional attribute list – i.e. a sequence expression before the `with` keyword.

---

/reversed_labeled_tuple_body: TILDE . LIDENT _*
/reversed_labeled_tuple_body: TILDE . LPAREN _*
Expected a labelled tuple element after `~`; either a label identifier or a parenthesised pattern.

---

/reversed_labeled_tuple_body: TILDE LPAREN . LIDENT _*
Expected a label identifier after the opening parenthesis of a labelled tuple pattern (`~(`).

---

/reversed_labeled_tuple_body: TILDE LPAREN LIDENT . type_constraint _*
Expected a type constraint (`:` …) after the identifier inside a labelled tuple pattern.

---

/type_constraint: COLONGREATER . core_type
Expected a core type after the `:>` operator in a type constraint.

---

/delimited_type_supporting_local_open: LBRACKETLESS . option_BAR_ _*
Expected either `]` or a row field after `[<`.

---

/delimited_type_supporting_local_open: LBRACKETLESS option_BAR_ . reversed_separated_nonempty_llist_BAR_row_field_ _*
Expected a row field after `[< |`.

---

/delimited_type_supporting_local_open: LBRACKETGREATER . option_BAR_ reversed_separated_nonempty_llist_BAR_row_field_ RBRACKET
/delimited_type_supporting_local_open: LBRACKETGREATER . RBRACKET
Expected a row field or a closing `]` after `[>`.

---

/delimited_type_supporting_local_open: LBRACKETGREATER option_BAR_ . reversed_separated_nonempty_llist_BAR_row_field_ RBRACKET
Expected a row field after `[> |`.

---

/delimited_type_supporting_local_open: LBRACKET . tag_field RBRACKET
/delimited_type_supporting_local_open: LBRACKET . BAR reversed_separated_nonempty_llist_BAR_row_field_ RBRACKET
/delimited_type_supporting_local_open: LBRACKET . row_field BAR reversed_separated_nonempty_llist_BAR_row_field_ RBRACKET
Expected a tag field, a row field, or a closing `]` after `[`.

---

/atomic_type: HASH . clty_longident
Expected a class type identifier after `#`.

---

/mk_longident_mod_ext_longident_LIDENT_: mod_ext_longident . DOT LIDENT
/mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident . DOT UIDENT
Expected a module identifier (`LIDENT`/`UIDENT`) after a dot in a module‑extended long identifier.

---

/mod_ext_longident: mod_ext_longident LPAREN . mod_ext_longident RPAREN
Expected a module long identifier after the opening parenthesis in a module‑extension application.

---

/mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident . DOT UIDENT
/mod_ext_longident: mod_ext_longident LPAREN mod_ext_longident . RPAREN
Expected a closing parenthesis after the inner module‑extension in a parenthesised application.

---

/mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident DOT . UIDENT
Expected an identifier after the dot of a module‑extension long identifier.

---

/mk_longident_mod_ext_longident_LIDENT_: mod_ext_longident DOT . LIDENT
/mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident DOT . UIDENT
Expected an identifier after the dot of a module‑extension long identifier.

---

/delimited_type_supporting_local_open: LBRACKET BAR . reversed_separated_nonempty_llist_BAR_row_field_ RBRACKET
Expected a row field after `[|`.

---

/name_tag: BACKQUOTE . ident
Expected an identifier after a backquote in a polymorphic variant tag.

---

/function_type: tuple_type MINUSGREATER . function_type
Expected another function type after the arrow of a tuple‑type function.

---

/function_type: optlabel . LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER function_type
/function_type: optlabel . tuple_type MINUSGREATER function_type
/function_type: optlabel . atomic_type _*
Expected a parameter list or a return type after the optional label in a function type.

---

/function_type: optlabel LPAREN reversed_nonempty_llist_typevar_ . DOT core_type RPAREN MINUSGREATER function_type
/function_type: optlabel LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN . MINUSGREATER function_type
Expected a dot (`.`) after the type variable list in a labelled function type.

---

/function_type: optlabel LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER . function_type
Expected the arrow (`->`) after the closing parenthesis of the parameter list in a labelled function type.

---

/delimited_type_supporting_local_open: LPAREN . core_type RPAREN
/reversed_separated_nontrivial_llist_COMMA_core_type_: core_type . COMMA core_type
Expected a closing parenthesis after a core type in a parenthesised type, or a comma after a core type in a tuple.

---

/reversed_separated_nontrivial_llist_COMMA_core_type_: core_type COMMA . core_type
Expected the second core type after a comma in a tuple type.

---

/function_type: optlabel tuple_type . MINUSGREATER function_type
Expected an arrow after the tuple type in a labelled function type.

---

/function_type: optlabel tuple_type MINUSGREATER . function_type
Expected a function type after the arrow in a labelled function type.

---

/delimited_type_supporting_local_open: LBRACKET BAR reversed_separated_nonempty_llist_BAR_row_field_ . RBRACKET
Expected a closing `]` after a row field in a `[| … |]` type.

---

/tag_field: name_tag OF . opt_ampersand reversed_separated_nonempty_llist_AMPERSAND_core_type_no_attr_ list_attribute_
Expected `opt_ampersand` (optional `&`) after `OF` in a variant tag definition.

---

/tag_field: name_tag OF opt_ampersand . reversed_separated_nonempty_llist_AMPERSAND_core_type_no_attr_ list_attribute_
Expected a core type after the optional ampersand in a variant tag definition.

---

/type_constraint: COLON . core_type _*
Expected a core type after a colon in a type annotation.

---

/type_constraint: COLON core_type COLONGREATER . core_type
Expected a core type after the `:>` in a coercion type.

---

/atomic_type: LPAREN . reversed_separated_nontrivial_llist_COMMA_core_type_ _*
/atomic_type: LPAREN . core_type RPAREN
/atomic_type: LPAREN . MODULE ext list_attribute_ module_type RPAREN
/function_type: LPAREN . reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER function_type
Expected a type (or module type) after an opening parenthesis in a type expression.

---

/delimited_type_supporting_local_open: LPAREN MODULE . ext list_attribute_ module_type RPAREN
Expected an attribute (`%…`) after `MODULE` in a parenthesised module type.

---

/delimited_type_supporting_local_open: LPAREN MODULE ext list_attribute_ . module_type RPAREN
Expected a module type after the optional attribute list in a parenthesised module type.

---

/atomic_type: LPAREN . reversed_separated_nontrivial_llist_COMMA_core_type_ _*
Expected a core type (or a tuple of core types) after `(`.

---

/atomic_type: LPAREN reversed_separated_nontrivial_llist_COMMA_core_type_ . RPAREN _*
Expected a closing `)` after the comma‑separated core types.

---

/atomic_type: LPAREN reversed_separated_nontrivial_llist_COMMA_core_type_ RPAREN . type_longident
/atomic_type: LPAREN reversed_separated_nontrivial_llist_COMMA_core_type_ RPAREN . HASH clty_longident
Expected a type identifier (or class type identifier) after a parenthesised tuple type.

---

/delimited_type_supporting_local_open: LPAREN reversed_separated_nontrivial_llist_COMMA_core_type_ RPAREN HASH . clty_longident
Expected a class type identifier after `#` following a parenthesised tuple type.

---

/reversed_separated_nontrivial_llist_COMMA_core_type_: reversed_separated_nontrivial_llist_COMMA_core_type_ COMMA . core_type
Expected another core type after a comma in a tuple type.

---

/atomic_type: mod_ext_longident . DOT delimited_type_supporting_local_open
/mk_longident_mod_ext_longident_LIDENT_: mod_ext_longident . DOT LIDENT
/mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident . DOT UIDENT
Expected a module‑extended long identifier followed by a dot and a type expression (or identifier) after the dot.

---

/atomic_type: mod_ext_longident DOT . delimited_type_supporting_local_open
/mk_longident_mod_ext_longident_LIDENT_: mod_ext_longident DOT . LIDENT
/mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident DOT . UIDENT
Expected an identifier after the dot of a module‑extended long identifier.

---

/delimited_type_supporting_local_open: LPAREN . core_type RPAREN
/delimited_type_supporting_local_open: LPAREN . MODULE ext list_attribute_ module_type RPAREN
Expected a core type or a module type after `(` in a parenthesised type.

---

/delimited_type_supporting_local_open: LPAREN core_type . RPAREN
Expected a closing `)` after a core type in a parenthesised type.

---

/tuple_type: atomic_type STAR . reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_
Expected a labeled tuple element after `*` in a tuple type.

---

/reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_: LIDENT COLON . atomic_type
Expected an atomic type after the colon of a labeled tuple element.

---

/atomic_type: atomic_type HASH . clty_longident
Expected a class type identifier after `#` when hashing an atomic type.

---

/reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_: reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ STAR . atomic_type
/reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_: reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ STAR . LIDENT COLON atomic_type
Expected another atomic type (or a labeled element) after `*` in a tuple type.

---

/reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_: reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ STAR LIDENT COLON . atomic_type
Expected an atomic type after the colon in a labeled tuple element that follows a `*`.

---

/alias_type: alias_type AS . QUOTE ident
Expected a quoted identifier after `as` in a type alias.

---

/alias_type: alias_type AS QUOTE . ident
Expected an identifier after the quote in a type alias.

---

/function_type: optlabel LPAREN reversed_nonempty_llist_typevar_ . DOT core_type RPAREN MINUSGREATER function_type
Expected a dot (`.`) after the type‑variable list in a labelled function type.

---

/reversed_nonempty_llist_typevar_: reversed_nonempty_llist_typevar_ QUOTE . ident
Expected an identifier after the quote in a sequence of type variables.

---

/function_type: optlabel LPAREN reversed_nonempty_llist_typevar_ DOT . core_type RPAREN MINUSGREATER function_type
Expected a core type after the dot in a labelled function type.

---

/function_type: optlabel LPAREN reversed_nonempty_llist_typevar_ DOT core_type . RPAREN MINUSGREATER function_type
Expected a closing parenthesis after the return type in a labelled function type.

---

/function_type: optlabel LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER . function_type
Expected the arrow (`->`) after the closing parenthesis of the parameter list.

---

/type_constraint: COLON . core_type _*
Expected a core type after a colon in a type annotation.

---

/type_constraint: COLON core_type COLONGREATER . core_type
Expected a core type after the `:>` in a coercion.

---

/delimited_type_supporting_local_open: LBRACKETLESS . option_BAR_ _*
Expected either `]` or a row field after `[<`.

---

/delimited_type_supporting_local_open: LBRACKETLESS option_BAR_ . reversed_separated_nonempty_llist_BAR_row_field_ _*
Expected a row field after `[< |`.

---

/delimited_type_supporting_local_open: LBRACKETGREATER . option_BAR_ reversed_separated_nonempty_llist_BAR_row_field_ RBRACKET
/delimited_type_supporting_local_open: LBRACKETGREATER . RBRACKET
Expected a row field or a closing `]` after `[>`.

---

/delimited_type_supporting_local_open: LBRACKETGREATER option_BAR_ . reversed_separated_nonempty_llist_BAR_row_field_ RBRACKET
Expected a row field after `[> |`.

---

/delimited_type_supporting_local_open: LBRACKET . tag_field RBRACKET
/delimited_type_supporting_local_open: LBRACKET . BAR reversed_separated_nonempty_llist_BAR_row_field_ RBRACKET
/delimited_type_supporting_local_open: LBRACKET . row_field BAR reversed_separated_nonempty_llist_BAR_row_field_ RBRACKET
Expected a tag field, a row field, or a closing `]` after `[`.

---

/atomic_type: HASH . clty_longident
Expected a class type identifier after `#`.

---

/mk_longident_mod_ext_longident_LIDENT_: mod_ext_longident . DOT LIDENT
/mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident . DOT UIDENT
Expected an identifier after the dot in a module‑extension long identifier.

---

/mod_ext_longident: mod_ext_longident LPAREN . mod_ext_longident RPAREN
Expected an inner module‑extension long identifier after the opening parenthesis.

---

/mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident DOT . UIDENT
Expected an identifier after the dot of a module‑extension long identifier.

---

/delimited_type_supporting_local_open: LBRACKET BAR . reversed_separated_nonempty_llist_BAR_row_field_ RBRACKET
Expected a row field after `[|`.

---

/name_tag: BACKQUOTE . ident
Expected an identifier after a backquote in a polymorphic variant tag.

---

/function_type: tuple_type MINUSGREATER . function_type
Expected a function type after the arrow of a tuple‑type function.

---

/function_type: optlabel . LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER function_type
Expected a dot after the type‑variable list in a labelled function type.

---

/function_type: optlabel . tuple_type MINUSGREATER function_type
Expected a tuple type after the optional label in a function type.

---

/function_type: optlabel . atomic_type _*
Expected an atomic type (or type expression) after the optional label in a function type.

---

/atomic_type: LPAREN . reversed_separated_nontrivial_llist_COMMA_core_type_ _*
Expected a core type (or a tuple of core types) after `(`.

---

/delimited_type_supporting_local_open: LPAREN . core_type RPAREN
Expected a closing `)` after a core type in a parenthesised type.

---

/delimited_type_supporting_local_open: LPAREN . MODULE ext list_attribute_ module_type RPAREN
Expected a module type after `MODULE` in a parenthesised type.

---

/function_type: optlabel LPAREN . reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER function_type
Expected a dot after the type‑variable list in a labelled function type.

---

/type_parameters: LPAREN . reversed_separated_nonempty_llist_COMMA_type_parameter_ RPAREN
Expect a type parameter after the opening parenthesis of type parameters.

---

/type_parameter: type_variance . type_variable
Expect a type variable after the variance modifier.

---

/type_variable: QUOTE . ident
Expect an identifier after a quote in a type variable.

---

/type_parameters: LPAREN reversed_separated_nonempty_llist_COMMA_type_parameter_ . RPAREN _*
Expect a closing `)` after the comma‑separated type parameters.

---

/generic_type_declaration_no_nonrec_flag_type_subst_kind_: TYPE . ext _*
/generic_type_declaration_nonrec_flag_type_kind_: TYPE . ext _*
/signature_item: TYPE . ext _*
Expected an optional attribute (`%…`) after the `type` keyword.

---

/generic_type_declaration_no_nonrec_flag_type_subst_kind_: TYPE ext list_attribute_ NONREC . type_parameters LIDENT COLONEQUAL …
/generic_type_declaration_nonrec_flag_type_kind_: TYPE ext list_attribute_ NONREC . type_parameters LIDENT type_kind …
/signature_item: TYPE ext list_attribute_ NONREC . type_parameters type_longident PLUSEQ …
Expected the type parameters after `type nonrec`.

---

/type_parameters: LPAREN . reversed_separated_nonempty_llist_COMMA_type_parameter_ RPAREN
Expect a type parameter after the opening parenthesis.

---

/type_parameter: type_variance . type_variable
Expect a type variable after the variance.

---

/type_variable: QUOTE . ident
Expect an identifier after the quote.

---

/type_parameters: LPAREN reversed_separated_nonempty_llist_COMMA_type_parameter_ . RPAREN
Expect a closing `)` after the type parameters.

---

/generic_type_declaration_no_nonrec_flag_type_subst_kind_: TYPE ext list_attribute_ NONREC type_parameters . LIDENT COLONEQUAL …
/generic_type_declaration_nonrec_flag_type_kind_: TYPE ext list_attribute_ NONREC type_parameters . LIDENT type_kind …
/signature_item: TYPE ext list_attribute_ NONREC type_parameters . type_longident PLUSEQ …
Expected the type name after the type‑parameter list.

---

/type_kind: EQUAL . nonempty_type_kind
Expected a type definition after `=`.

---

/atomic_type: QUOTE . ident
Expected an identifier after the quote.

---

/nonempty_type_kind: PRIVATE . core_type
/nonempty_type_kind: PRIVATE . constructor_declarations
/nonempty_type_kind: PRIVATE . DOTDOT
/nonempty_type_kind: PRIVATE . LBRACE label_declarations RBRACE
Expected a core type, constructor list, `..`, or a record after `private`.

---

/atomic_type: LPAREN . reversed_separated_nontrivial_llist_COMMA_core_type_ _*
/constr_extra_nonprefix_ident: LPAREN . RPAREN
/constr_ident: LPAREN . COLONCOLON RPAREN
/delimited_type_supporting_local_open: LPAREN . core_type RPAREN
/delimited_type_supporting_local_open: LPAREN . MODULE ext list_attribute_ module_type RPAREN
/function_type: LPAREN . reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER function_type
Expect a core type, a constructor, or a module type after `(` in various contexts.

---

/function_type: optlabel LPAREN reversed_nonempty_llist_typevar_ . DOT core_type RPAREN MINUSGREATER function_type
Expect a dot after the type‑variable list in a labelled function type.

---

/reversed_nonempty_llist_typevar_: reversed_nonempty_llist_typevar_ QUOTE . ident
Expect an identifier after the quote in a list of type variables.

---

/function_type: optlabel LPAREN reversed_nonempty_llist_typevar_ DOT . core_type RPAREN MINUSGREATER function_type
Expect a core type after the dot in a labelled function type.

---

/function_type: optlabel LPAREN reversed_nonempty_llist_typevar_ DOT core_type . RPAREN MINUSGREATER function_type
Expect a closing parenthesis after the core type in a labelled function type.

---

/function_type: optlabel LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN . MINUSGREATER function_type
Expect the arrow (`->`) after the parameter list of a labelled function type.

---

/function_type: optlabel tuple_type . MINUSGREATER function_type
Expect the arrow after a tuple type in a labelled function type.

---

/function_type: optlabel tuple_type MINUSGREATER . function_type
Expect a function type after the arrow in a labelled function type.

---

/delimited_type_supporting_local_open: LBRACKET BAR reversed_separated_nonempty_llist_BAR_row_field_ . RBRACKET
Expect a closing `]` after a row field in a `[| … |]` type.

---

/reversed_separated_nonempty_llist_BAR_row_field_: reversed_separated_nonempty_llist_BAR_row_field_ BAR . row_field
Expect a row field after the `|` separator.

---

/name_tag: BACKQUOTE . ident
Expect an identifier after the backquote in a variant tag.

---

/function_type: tuple_type MINUSGREATER . function_type
Expect a function type after the arrow of a tuple‑type function.

---

/function_type: optlabel . LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER function_type
Expect a dot after the type‑variable list in a labelled function type.

---

/atomic_type: LPAREN . reversed_separated_nontrivial_llist_COMMA_core_type_ _*
Expect a core type (or tuple) after `(`.

---

/atomic_type: LPAREN reversed_separated_nontrivial_llist_COMMA_core_type_ . RPAREN _*
Expect a closing `)` after the tuple of core types.

---

/atomic_type: LPAREN reversed_separated_nontrivial_llist_COMMA_core_type_ RPAREN . type_longident
Expect a type identifier after a parenthesised tuple type.

---

/atomic_type: LPAREN reversed_separated_nontrivial_llist_COMMA_core_type_ RPAREN . HASH clty_longident
Expect a class type identifier after `#` following a parenthesised tuple type.

---

/reversed_separated_nontrivial_llist_COMMA_core_type_: reversed_separated_nontrivial_llist_COMMA_core_type_ COMMA . core_type
Expect another core type after a comma in a tuple type.

---

/atomic_type: mod_ext_longident . DOT delimited_type_supporting_local_open
/mk_longident_mod_ext_longident_LIDENT_: mod_ext_longident . DOT LIDENT
/mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident . DOT UIDENT
Expect an identifier or a type after the dot of a module‑extended long identifier.

---

/atomic_type: mod_ext_longident DOT . delimited_type_supporting_local_open
/mk_longident_mod_ext_longident_LIDENT_: mod_ext_longident DOT . LIDENT
/mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident DOT . UIDENT
Expect an identifier after the dot of a module‑extended long identifier.

---

/delimited_type_supporting_local_open: LPAREN . core_type RPAREN
/delimited_type_supporting_local_open: LPAREN . MODULE ext list_attribute_ module_type RPAREN
Expect a core type or a module type after `(` in a parenthesised type.

---

/delimited_type_supporting_local_open: LPAREN core_type . RPAREN
Expect a closing `)` after a core type in a parenthesised type.

---

/tuple_type: atomic_type STAR . reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_
Expect a labeled tuple element after `*` in a tuple type.

---

/reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_: LIDENT COLON . atomic_type
Expect an atomic type after the colon of a labeled tuple element.

---

/atomic_type: atomic_type HASH . clty_longident
Expect a class type identifier after `#` on an atomic type.

---

/reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_: reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ STAR . atomic_type
/reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_: reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ STAR . LIDENT COLON atomic_type
Expect another atomic type (or a labeled element) after `*` in a tuple type.

---

/reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_: reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ STAR LIDENT COLON . atomic_type
Expect an atomic type after the colon in a labeled tuple element following a `*`.

---

/alias_type: alias_type AS . QUOTE ident
Expect a quoted identifier after `as` in a type alias.

---

/alias_type: alias_type AS QUOTE . ident
Expect an identifier after the quote in a type alias.

---

/function_type: optlabel LPAREN reversed_nonempty_llist_typevar_ . DOT core_type RPAREN MINUSGREATER function_type
Expect a dot after the list of type variables in a labelled function type.

---

/reversed_nonempty_llist_typevar_: reversed_nonempty_llist_typevar_ QUOTE . ident
Expect an identifier after the quote in a list of type variables.

---

/function_type: optlabel LPAREN reversed_nonempty_llist_typevar_ DOT . core_type RPAREN MINUSGREATER function_type
Expect a core type after the dot in a labelled function type.

---

/function_type: optlabel LPAREN reversed_nonempty_llist_typevar_ DOT core_type . RPAREN MINUSGREATER function_type
Expect a closing parenthesis after the return type in a labelled function type.

---

/function_type: optlabel LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN . MINUSGREATER function_type
Expect the arrow (`->`) after the parameter list in a labelled function type.

---

/simple_expr: PREFIXOP . simple_expr
Expect an operand after a prefix operator.

---

/simple_expr: OBJECT . ext list_attribute_ class_self_pattern list_text_cstr_class_field__ END
Expect the opening `object` keyword to be followed by optional attributes, a self pattern, class fields, and `end`.

---

/class_self_pattern: LPAREN . pattern _*
Expect a pattern after the opening parenthesis of a class self pattern.

---

/object_type: LESS . meth_list GREATER
/object_type: LESS . GREATER
Expect a method list or a closing `>` after `<` in an object type.

---

/object_type: LESS LPAREN . meth_list GREATER
Expect a method list after `(<`.

---

/labeled_tuple_pat_element_list_pattern_: TILDE . LIDENT _*
/labeled_tuple_pat_element_list_pattern_: TILDE . LPAREN _*
/reversed_labeled_tuple_pattern_pattern_: TILDE . LIDENT COMMA DOTDOT
/reversed_labeled_tuple_pattern_pattern_: TILDE . LPAREN LIDENT COLON core_type RPAREN COMMA DOTDOT
Expected a label name or a parenthesised pattern after `~` in a tuple pattern.

---

/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN . LIDENT _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN . LIDENT COLON core_type RPAREN COMMA DOTDOT
Expect a label identifier after `~(` in a tuple pattern.

---

/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT . COLON _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT
Expect a colon after the identifier in a labelled tuple pattern.

---

/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON . core_type _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON . core_type RPAREN COMMA DOTDOT
Expect a core type after the colon in a labelled tuple pattern.

---

/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT
Expect a closing parenthesis after the core type in a labelled tuple pattern.

---

/labeled_tuple_pat_element_list_pattern_: TILDE LIDENT COMMA . pattern
/labeled_tuple_pat_element_list_pattern_: TILDE LIDENT COMMA . LABEL simple_pattern
/labeled_tuple_pat_element_list_pattern_: TILDE LIDENT COMMA . TILDE _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LIDENT COMMA . DOTDOT
Expect a pattern, a labelled pattern, or another `~` after `~,` in a tuple pattern.

---

/labeled_tuple_pat_element_list_pattern_: TILDE . LIDENT _*
/labeled_tuple_pat_element_list_pattern_: TILDE . LPAREN _*
/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT type_constraint RPAREN COMMA TILDE . LIDENT
/labeled_tuple_pat_element_list_pattern_: TILDE . LPAREN LIDENT type_constraint RPAREN COMMA TILDE LPAREN LIDENT type_constraint RPAREN
/reversed_labeled_tuple_pattern_pattern_: TILDE . LIDENT COMMA DOTDOT
/reversed_labeled_tuple_pattern_pattern_: TILDE . LPAREN LIDENT type_constraint RPAREN COMMA DOTDOT
Expect continuation of a labelled tuple pattern after `~` or `~(`.

---

/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN . LIDENT _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT
Expect a label identifier after `~(` in a tuple pattern.

---

/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT . type_constraint _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT
Expect a type constraint after the identifier in a labelled tuple pattern.

---

/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT type_constraint . RPAREN _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT type_constraint . RPAREN COMMA DOTDOT
Expect a closing parenthesis after the type constraint in a labelled tuple pattern.

---

/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT type_constraint RPAREN COMMA . pattern
/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT type_constraint RPAREN COMMA . LABEL simple_pattern
/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT type_constraint RPAREN COMMA . TILDE _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT type_constraint RPAREN COMMA . DOTDOT
Expect the next element after a comma in a labelled tuple pattern.

---

/labeled_tuple_pat_element_list_pattern_: TILDE . LIDENT _*
/labeled_tuple_pat_element_list_pattern_: TILDE LIDENT COMMA TILDE . LIDENT
/labeled_tuple_pat_element_list_pattern_: TILDE . LIDENT COMMA TILDE LPAREN LIDENT type_constraint RPAREN
/labeled_tuple_pat_element_list_pattern_: TILDE LIDENT COMMA TILDE . LPAREN LIDENT type_constraint RPAREN
/labeled_tuple_pat_element_list_pattern_: TILDE . LPAREN _*
/reversed_labeled_tuple_pattern_pattern_: TILDE . LIDENT COMMA DOTDOT
/reversed_labeled_tuple_pattern_pattern_: TILDE . LPAREN LIDENT type_constraint RPAREN COMMA DOTDOT
Continue parsing a tuple pattern after `~` or `~(`.

---

/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN . LIDENT _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT
Expect a label identifier after `~(`.

---

/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT . COLON _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT
Expect a colon after the identifier in a labelled tuple pattern.

---

/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON . core_type _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON . core_type RPAREN COMMA DOTDOT
Expect a core type after the colon in a labelled tuple pattern.

---

/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT
Expect a closing parenthesis after the core type.

---

/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN . COMMA _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN . COMMA DOTDOT
Expect a comma after a complete labelled tuple element.

---

/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA . pattern
/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA . LABEL simple_pattern
/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA . TILDE _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA . DOTDOT
Expect the next element after a comma in a labelled tuple pattern.

---

/labeled_tuple_pat_element_list_pattern_: TILDE . LIDENT _*
/labeled_tuple_pat_element_list_pattern_: TILDE . LPAREN _*
/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE . LIDENT
/labeled_tuple_pat_element_list_pattern_: TILDE . LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN LIDENT COLON core_type RPAREN
/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE . LPAREN LIDENT COLON core_type RPAREN
/reversed_labeled_tuple_pattern_pattern_: TILDE . LIDENT COMMA DOTDOT
/reversed_labeled_tuple_pattern_pattern_: TILDE . LPAREN LIDENT COLON core_type RPAREN COMMA DOTDOT
Continue parsing after a `~` or `~(` element.

---

/labeled_tuple_pat_element_list_pattern_: TILDE . LIDENT _*
/labeled_tuple_pat_element_list_pattern_: TILDE . LPAREN _*
/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE . LIDENT
/labeled_tuple_pat_element_list_pattern_: TILDE . LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN LIDENT COLON core_type RPAREN
/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE . LPAREN LIDENT COLON core_type RPAREN
/reversed_labeled_tuple_pattern_pattern_: TILDE . LIDENT COMMA DOTDOT
/reversed_labeled_tuple_pattern_pattern_: TILDE . LPAREN LIDENT COLON core_type RPAREN COMMA DOTDOT
Continue parsing after a `~` element.

---

/labeled_tuple_pat_element_list_pattern_: LABEL . simple_pattern _*
/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA . pattern
/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA . LABEL simple_pattern
/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA . TILDE _*
/reversed_labeled_tuple_pattern_pattern_: LABEL simple_pattern COMMA . DOTDOT
Expected a pattern, a label, or another `~` after a labelled tuple element.

---

/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE . LIDENT
/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE . LPAREN LIDENT COLON core_type RPAREN
/labeled_tuple_pat_element_list_pattern_: TILDE . LIDENT _*
/labeled_tuple_pat_element_list_pattern_: TILDE . LPAREN _*
/reversed_labeled_tuple_pattern_pattern_: TILDE . LIDENT COMMA DOTDOT
/reversed_labeled_tuple_pattern_pattern_: TILDE . LPAREN LIDENT COLON core_type RPAREN COMMA DOTDOT
Expected continuation after a comma in a labelled tuple pattern.

---

/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN . LIDENT COLON core_type RPAREN
/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN . LIDENT _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN . LIDENT COLON core_type RPAREN COMMA DOTDOT
Expect a label identifier after `~(` in a tuple pattern.

---

/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT . COLON _*
/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT . COLON _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT
Expect a colon after the identifier in a `~(` pattern.

---

/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON . core_type RPAREN
/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON . core_type _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON . core_type RPAREN COMMA DOTDOT
Expect a core type after the colon.

---

/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT
Expect a closing parenthesis after the core type.

---

/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON core_type RPAREN . COMMA _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN . COMMA DOTDOT
Expect a comma after a complete labelled tuple element.

---

/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON core_type RPAREN COMMA . pattern
/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON core_type RPAREN COMMA . LABEL simple_pattern
/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON core_type RPAREN COMMA . TILDE _*
/reversed_labeled_tuple_pattern_pattern_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON core_type RPAREN COMMA . DOTDOT
Expect the next pattern element after a comma.

---

/labeled_tuple_pat_element_list_pattern_: LABEL . simple_pattern _*
/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA LABEL . simple_pattern
/labeled_tuple_pat_element_list_pattern_: LABEL . simple_pattern _*
/reversed_labeled_tuple_pattern_pattern_: LABEL . simple_pattern COMMA DOTDOT
Expected a simple pattern after a label or after a comma in a labelled tuple pattern.

---

/pattern: EXCEPTION . ext list_attribute_ pattern
Expected a pattern after `exception` (optionally followed by an attribute).

---

/pattern: EXCEPTION ext list_attribute_ . pattern
Expected a pattern after `exception` and its attribute list.

---

/pattern: EFFECT . pattern_gen COMMA simple_pattern
Expected a pattern after `effect`; it must be a tuple of a generated pattern and a simple pattern.

---

/pattern: EFFECT pattern_gen . COMMA simple_pattern
Expected a comma after the generated pattern in an `effect` pattern.

---

/pattern: EFFECT pattern_gen COMMA . simple_pattern
Expected a simple pattern after the comma in an `effect` pattern.

---

/labeled_tuple_pat_element_list_pattern_: pattern COMMA . pattern
/labeled_tuple_pat_element_list_pattern_: pattern COMMA . LABEL simple_pattern
/labeled_tuple_pat_element_list_pattern_: pattern COMMA . TILDE _*
/reversed_labeled_tuple_pattern_pattern_: pattern COMMA . DOTDOT
Expected another pattern, a label, or a `~` after a comma in a tuple pattern.

---

/labeled_tuple_pat_element_list_pattern_: pattern COMMA TILDE . LIDENT
/labeled_tuple_pat_element_list_pattern_: pattern COMMA TILDE . LPAREN LIDENT COLON core_type RPAREN
/labeled_tuple_pat_element_list_pattern_: TILDE . LIDENT _*
/labeled_tuple_pat_element_list_pattern_: TILDE . LPAREN _*
/reversed_labeled_tuple_pattern_pattern_: TILDE . LIDENT COMMA DOTDOT
/reversed_labeled_tuple_pattern_pattern_: TILDE . LPAREN LIDENT COLON core_type RPAREN COMMA DOTDOT
Expected continuation after a `~,` in a tuple pattern.

---

/labeled_tuple_pat_element_list_pattern_: pattern COMMA TILDE LPAREN . LIDENT COLON core_type RPAREN
/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN . LIDENT _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN . LIDENT COLON core_type RPAREN COMMA DOTDOT
Expect a label identifier after `~(` in a tuple pattern.

---

/labeled_tuple_pat_element_list_pattern_: pattern COMMA TILDE LPAREN LIDENT . COLON _*
/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT . COLON _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT
Expect a colon after the identifier in a `~(` pattern.

---

/labeled_tuple_pat_element_list_pattern_: pattern COMMA TILDE LPAREN LIDENT COLON . core_type RPAREN
/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON . core_type _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON . core_type RPAREN COMMA DOTDOT
Expect a core type after the colon.

---

/labeled_tuple_pat_element_list_pattern_: pattern COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT
Expect a closing parenthesis after the core type.

---

/labeled_tuple_pat_element_list_pattern_: pattern COMMA TILDE LPAREN LIDENT COLON core_type RPAREN . COMMA _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN . COMMA DOTDOT
Expect a comma after a complete labelled tuple element.

---

/labeled_tuple_pat_element_list_pattern_: pattern COMMA TILDE LPAREN LIDENT COLON core_type RPAREN COMMA . pattern
/labeled_tuple_pat_element_list_pattern_: pattern COMMA TILDE LPAREN LIDENT COLON core_type RPAREN COMMA . LABEL simple_pattern
/labeled_tuple_pat_element_list_pattern_: pattern COMMA TILDE LPAREN LIDENT COLON core_type RPAREN COMMA . TILDE _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA . DOTDOT
Expect the next element after a comma.

---

/labeled_tuple_pat_element_list_pattern_: LABEL . simple_pattern _*
/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA . pattern
/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA . LABEL simple_pattern
/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA . TILDE _*
/reversed_labeled_tuple_pattern_pattern_: LABEL simple_pattern COMMA . DOTDOT
Expected a pattern, label, or `~` after a comma following a labelled element.

---

/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE . LIDENT
/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE . LPAREN LIDENT COLON core_type RPAREN
/labeled_tuple_pat_element_list_pattern_: TILDE . LIDENT _*
/labeled_tuple_pat_element_list_pattern_: TILDE . LPAREN _*
/reversed_labeled_tuple_pattern_pattern_: TILDE . LIDENT COMMA DOTDOT
/reversed_labeled_tuple_pattern_pattern_: TILDE . LPAREN LIDENT COLON core_type RPAREN COMMA DOTDOT
Expected continuation after a comma in a labelled tuple pattern.

---

/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN . LIDENT COLON core_type RPAREN
/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN . LIDENT _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN . LIDENT COLON core_type RPAREN COMMA DOTDOT
Expect a label identifier after `~(` in a tuple pattern.

---

/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT . COLON _*
/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT . COLON _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT
Expect a colon after the identifier.

---

/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON . core_type RPAREN
/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON . core_type _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON . core_type RPAREN COMMA DOTDOT
Expect a core type after the colon.

---

/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT
Expect a closing parenthesis after the core type.

---

/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON core_type RPAREN . COMMA _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN . COMMA DOTDOT
Expect a comma after a complete element.

---

/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON core_type RPAREN COMMA . pattern
/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON core_type RPAREN COMMA . LABEL simple_pattern
/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON core_type RPAREN COMMA . TILDE _*
/reversed_labeled_tuple_pattern_pattern_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON core_type RPAREN COMMA . DOTDOT
Expect the next element after a comma.

---

/labeled_tuple_pat_element_list_pattern_: LABEL . simple_pattern _*
/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA LABEL . simple_pattern
/labeled_tuple_pat_element_list_pattern_: LABEL . simple_pattern _*
/reversed_labeled_tuple_pattern_pattern_: LABEL . simple_pattern COMMA DOTDOT
Expect a simple pattern after a label (or after a comma).

---

/signed_constant: PLUS . INT
/signed_constant: PLUS . FLOAT
Expected an integer or float literal after the unary `+`.

---

/signed_constant: MINUS . INT
/signed_constant: MINUS . FLOAT
Expected an integer or float literal after the unary `-`.

---

/constr_extra_nonprefix_ident: LPAREN . RPAREN
/constr_longident: LPAREN . COLONCOLON RPAREN
/simple_pattern_not_ident: LPAREN . pattern RPAREN
/simple_pattern_not_ident: LPAREN . MODULE _*
/simple_pattern_not_ident: LPAREN . pattern COLON core_type RPAREN
/val_extra_ident: LPAREN . operator RPAREN
Expected a pattern, a module expression, a type annotation, or an operator after `(` in a pattern or extra identifier.

---

/simple_pattern_not_ident: LPAREN MODULE . ext _*
Expected an attribute (`%…`) after `module` in a parenthesised pattern.

---

/simple_pattern_not_ident: LPAREN MODULE ext list_attribute_ . module_name _*
Expected a module name after the optional attribute list in a parenthesised pattern.

---

/simple_pattern_not_ident: LPAREN MODULE ext list_attribute_ module_name . RPAREN
/simple_pattern_not_ident: LPAREN MODULE ext list_attribute_ module_name . COLON module_type RPAREN
Expected `)` or `:` `module_type` after a module name in a parenthesised pattern.

---

/simple_pattern_not_ident: LPAREN MODULE ext list_attribute_ module_name COLON . module_type RPAREN
Expected a module type after the colon in a parenthesised pattern.

---

/module_type: MODULE . TYPE OF list_attribute_ module_expr
Expected `type of` after `module` in a module type expression.

---

/module_type: MODULE TYPE . OF list_attribute_ module_expr
Expected `of` after `module type` in a module type expression.

---

/module_type: MODULE TYPE OF . list_attribute_ module_expr
Expected an optional attribute list after `module type of`.

---

/module_type: MODULE TYPE OF list_attribute_ . module_expr
Expected a module expression after the optional attribute list in a `module type of` construct.

---

/module_expr: STRUCT . list_attribute_ structure END
Expected a list of attributes (or the closing `end`) after `struct`.

---

/open_declaration: OPEN . ext list_attribute_ module_expr list_post_item_attribute_
/open_declaration: OPEN . BANG ext list_attribute_ module_expr list_post_item_attribute_
Expected a module expression (or a `!` followed by it) after the `open` keyword.

---

/open_declaration: OPEN BANG . ext list_attribute_ module_expr list_post_item_attribute_
Expected an attribute list after `open !`.

---

/open_declaration: OPEN BANG ext list_attribute_ . module_expr list_post_item_attribute_
Expected a module expression after the optional attribute list in `open !`.

---

/paren_module_expr: LPAREN . module_expr _*
/paren_module_expr: LPAREN . VAL list_attribute_ expr_colon_package_type RPAREN
Expected a module expression or a `val …` package type after an opening parenthesis.

---

/paren_module_expr: LPAREN VAL . list_attribute_ expr_colon_package_type RPAREN
Expected an attribute list after `val` in a parenthesised module expression.

---

/paren_module_expr: LPAREN VAL list_attribute_ . expr_colon_package_type RPAREN
Expected the package type after the optional attribute list in a `val` expression.

---

/simple_expr: NEW . ext list_attribute_ class_longident
Expected a class long identifier after `new` (optionally preceded by attributes).

---

/simple_expr: NEW ext list_attribute_ . class_longident
Expected a class long identifier after optional attributes following `new`.

---

/mk_longident_mod_longident_LIDENT_: mod_longident . DOT LIDENT
/mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT
Expected an identifier after the dot of a module long identifier.

---

/mk_longident_mod_longident_LIDENT_: mod_longident DOT . LIDENT
/mk_longident_mod_longident_UIDENT_: mod_longident DOT . UIDENT
Expected an identifier after the dot of a module long identifier.

---

/simple_expr: METAOCAML_ESCAPE . simple_expr
Expected a sub‑expression after a meta‑ocaml escape (`.~`).

---

/simple_expr: METAOCAML_BRACKET_OPEN . seq_expr METAOCAML_BRACKET_CLOSE
Expected a sequence expression after an opening meta‑ocaml bracket (`.<`).

---

/fun_expr: MATCH . ext list_attribute_ seq_expr WITH reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expected a sequence expression after `match` (possibly followed by attributes).

---

/fun_expr: MATCH ext list_attribute_ . seq_expr WITH reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expected a sequence expression after `match` and its attribute list.

---

/constr_extra_nonprefix_ident: LPAREN . RPAREN
/constr_longident: LPAREN . COLONCOLON RPAREN
/simple_expr: LPAREN . seq_expr _*
/simple_expr: LPAREN . MODULE _*
/val_extra_ident: LPAREN . operator RPAREN
Expected a closing parenthesis, a module expression, a sequence expression, or an operator after `(`.

---

/simple_expr: LBRACKETBAR . separated_or_terminated_nonempty_list_SEMI_expr_ BARRBRACKET
/simple_expr: LBRACKETBAR . BARRBRACKET
Expected a list of expressions or a closing `|]` after `[|`.

---

/fun_expr: LIDENT LESSMINUS . fun_expr
/fun_expr: LIDENT LESSMINUS . FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expected an expression (or a `function …` construct) after the assignment operator `<-`.

---

/fun_expr: LETOP . letop_bindings IN seq_expr
Expected a binding after `let*` (or other `letop` operators).

---

/labeled_tuple_pat_element_list_pattern_: TILDE . LIDENT _*
/labeled_tuple_pat_element_list_pattern_: TILDE . LPAREN _*
/reversed_labeled_tuple_pattern_pattern_: TILDE . LIDENT COMMA DOTDOT
/reversed_labeled_tuple_pattern_pattern_: TILDE . LPAREN LIDENT COLON core_type RPAREN COMMA DOTDOT
Expected a label identifier or a parenthesised pattern after `~` in a tuple pattern.

---

/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN . LIDENT _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN . LIDENT COLON core_type RPAREN COMMA DOTDOT
Expected a label identifier after `~(` in a tuple pattern.

---

/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT . COLON _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT
Expected a colon after the identifier in a labelled tuple pattern.

---

/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON . core_type _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON . core_type RPAREN COMMA DOTDOT
Expected a core type after the colon in a labelled tuple pattern.

---

/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT
Expected a closing parenthesis after the core type.

---

/labeled_tuple_pat_element_list_pattern_: TILDE LIDENT COMMA . pattern
/labeled_tuple_pat_element_list_pattern_: TILDE LIDENT COMMA . LABEL simple_pattern
/labeled_tuple_pat_element_list_pattern_: TILDE LIDENT COMMA . TILDE _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LIDENT COMMA . DOTDOT
Expected a pattern, a label, or another `~` after `~,` in a tuple pattern.

---

/labeled_tuple_pat_element_list_pattern_: TILDE . LIDENT _*
/labeled_tuple_pat_element_list_pattern_: TILDE . LPAREN _*
/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE . LIDENT
/labeled_tuple_pat_element_list_pattern_: TILDE . LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN LIDENT COLON core_type RPAREN
/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE . LPAREN LIDENT COLON core_type RPAREN
/reversed_labeled_tuple_pattern_pattern_: TILDE . LIDENT COMMA DOTDOT
/reversed_labeled_tuple_pattern_pattern_: TILDE . LPAREN LIDENT COLON core_type RPAREN COMMA DOTDOT
Continuation of a tuple pattern after a `~` or `~(` element.

---

/labeled_tuple_pat_element_list_pattern_: LABEL . simple_pattern _*
/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA . pattern
/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA . LABEL simple_pattern
/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA . TILDE _*
/reversed_labeled_tuple_pattern_pattern_: LABEL simple_pattern COMMA . DOTDOT
Expected a pattern, label, or `~` after a comma following a labelled element.

---

/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE . LIDENT
/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE . LPAREN LIDENT COLON core_type RPAREN
/labeled_tuple_pat_element_list_pattern_: TILDE . LIDENT _*
/labeled_tuple_pat_element_list_pattern_: TILDE . LPAREN _*
/reversed_labeled_tuple_pattern_pattern_: TILDE . LIDENT COMMA DOTDOT
/reversed_labeled_tuple_pattern_pattern_: TILDE . LPAREN LIDENT COLON core_type RPAREN COMMA DOTDOT
Continuation after a comma in a labelled tuple pattern.

---

/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN . LIDENT COLON core_type RPAREN
/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN . LIDENT _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN . LIDENT COLON core_type RPAREN COMMA DOTDOT
Expect a label identifier after `~(` in a tuple pattern.

---

/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT . COLON _*
/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT . COLON _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT
Expect a colon after the identifier in a `~(` pattern.

---

/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON . core_type RPAREN
/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON . core_type _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON . core_type RPAREN COMMA DOTDOT
Expect a core type after the colon.

---

/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT
Expect a closing parenthesis after the core type.

---

/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON core_type RPAREN . COMMA _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN . COMMA DOTDOT
Expect a comma after a complete labelled tuple element.

---

/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON core_type RPAREN COMMA . pattern
/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON core_type RPAREN COMMA . LABEL simple_pattern
/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON core_type RPAREN COMMA . TILDE _*
/reversed_labeled_tuple_pattern_pattern_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON core_type RPAREN COMMA . DOTDOT
Expect the next pattern after a comma.

---

/labeled_tuple_pat_element_list_pattern_: LABEL . simple_pattern _*
/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA LABEL . simple_pattern
/labeled_tuple_pat_element_list_pattern_: LABEL . simple_pattern _*
/reversed_labeled_tuple_pattern_pattern_: LABEL . simple_pattern COMMA DOTDOT
Expected a simple pattern after a label (or after a comma).

---

/pattern: EXCEPTION . ext list_attribute_ pattern
Expected a pattern after `exception` (optionally with attributes).

/pattern: EXCEPTION ext list_attribute_ . pattern
Expected a pattern after `exception` and its attribute list.

---

/pattern: EFFECT . pattern_gen COMMA simple_pattern
Expected a tuple of a generated pattern and a simple pattern after `effect`.

---

/pattern: EFFECT pattern_gen . COMMA simple_pattern
Expected a comma after the generated pattern in an `effect` pattern.

---

/pattern: EFFECT pattern_gen COMMA . simple_pattern
Expected a simple pattern after the comma in an `effect` pattern.

---

/labeled_tuple_pat_element_list_pattern_: pattern COMMA . pattern
/labeled_tuple_pat_element_list_pattern_: pattern COMMA . LABEL simple_pattern
/labeled_tuple_pat_element_list_pattern_: pattern COMMA . TILDE _*
/reversed_labeled_tuple_pattern_pattern_: pattern COMMA . DOTDOT
Expected another pattern, a label, or another `~` after a comma in a tuple pattern.

---

/labeled_tuple_pat_element_list_pattern_: pattern COMMA TILDE . LIDENT
/labeled_tuple_pat_element_list_pattern_: pattern COMMA TILDE . LPAREN LIDENT COLON core_type RPAREN
/labeled_tuple_pat_element_list_pattern_: TILDE . LIDENT _*
/labeled_tuple_pat_element_list_pattern_: TILDE . LPAREN _*
/reversed_labeled_tuple_pattern_pattern_: TILDE . LIDENT COMMA DOTDOT
/reversed_labeled_tuple_pattern_pattern_: TILDE . LPAREN LIDENT COLON core_type RPAREN COMMA DOTDOT
Continuation after a `~,` in a tuple pattern.

---

/labeled_tuple_pat_element_list_pattern_: pattern COMMA TILDE LPAREN . LIDENT COLON core_type RPAREN
/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN . LIDENT _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN . LIDENT COLON core_type RPAREN COMMA DOTDOT
Expect a label identifier after `~(` in a tuple pattern.

---

/labeled_tuple_pat_element_list_pattern_: pattern COMMA TILDE LPAREN LIDENT . COLON _*
/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT . COLON _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT
Expect a colon after the identifier.

---

/labeled_tuple_pat_element_list_pattern_: pattern COMMA TILDE LPAREN LIDENT COLON . core_type RPAREN
/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON . core_type _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON . core_type RPAREN COMMA DOTDOT
Expect a core type after the colon.

---

/labeled_tuple_pat_element_list_pattern_: pattern COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT
Expect a closing parenthesis after the core type.

---

/labeled_tuple_pat_element_list_pattern_: pattern COMMA TILDE LPAREN LIDENT COLON core_type RPAREN . COMMA _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN . COMMA DOTDOT
Expect a comma after a complete element.

---

/labeled_tuple_pat_element_list_pattern_: pattern COMMA TILDE LPAREN LIDENT COLON core_type RPAREN COMMA . pattern
/labeled_tuple_pat_element_list_pattern_: pattern COMMA TILDE LPAREN LIDENT COLON core_type RPAREN COMMA . LABEL simple_pattern
/labeled_tuple_pat_element_list_pattern_: pattern COMMA TILDE LPAREN LIDENT COLON core_type RPAREN COMMA . TILDE _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA . DOTDOT
Expect the next element after a comma.

---

/labeled_tuple_pat_element_list_pattern_: LABEL . simple_pattern _*
/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA . pattern
/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA . LABEL simple_pattern
/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA . TILDE _*
/reversed_labeled_tuple_pattern_pattern_: LABEL simple_pattern COMMA . DOTDOT
Expected a pattern, label, or `~` after a comma following a labelled element.

---

/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE . LIDENT
/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE . LPAREN LIDENT COLON core_type RPAREN
/labeled_tuple_pat_element_list_pattern_: TILDE . LIDENT _*
/labeled_tuple_pat_element_list_pattern_: TILDE . LPAREN _*
/reversed_labeled_tuple_pattern_pattern_: TILDE . LIDENT COMMA DOTDOT
/reversed_labeled_tuple_pattern_pattern_: TILDE . LPAREN LIDENT COLON core_type RPAREN COMMA DOTDOT
Continuation after a comma in a labelled tuple pattern.

---

/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN . LIDENT COLON core_type RPAREN
/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN . LIDENT _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN . LIDENT COLON core_type RPAREN COMMA DOTDOT
Expect a label identifier after `~(`.

---

/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT . COLON _*
/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT . COLON _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT
Expect a colon after the identifier.

---

/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON . core_type RPAREN
/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON . core_type _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON . core_type RPAREN COMMA DOTDOT
Expect a core type after the colon.

---

/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT
Expect a closing parenthesis after the core type.

---

/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON core_type RPAREN . COMMA _*
/reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN . COMMA DOTDOT
Expect a comma after a complete element.

---

/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON core_type RPAREN COMMA . pattern
/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON core_type RPAREN COMMA . LABEL simple_pattern
/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON core_type RPAREN COMMA . TILDE _*
/reversed_labeled_tuple_pattern_pattern_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON core_type RPAREN COMMA . DOTDOT
Expect the next pattern after a comma.

---

/labeled_tuple_pat_element_list_pattern_: LABEL . simple_pattern _*
/labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA LABEL . simple_pattern
/labeled_tuple_pat_element_list_pattern_: LABEL . simple_pattern _*
/reversed_labeled_tuple_pattern_pattern_: LABEL . simple_pattern COMMA DOTDOT
Expected a simple pattern after a label (or after a comma).

---

/signed_constant: PLUS . INT
/signed_constant: PLUS . FLOAT
Expected an integer or float literal after unary `+`.

---

/signed_constant: MINUS . INT
/signed_constant: MINUS . FLOAT
Expected an integer or float literal after unary `-`.

---

/constr_extra_nonprefix_ident: LPAREN . RPAREN
/constr_longident: LPAREN . COLONCOLON RPAREN
/simple_pattern_not_ident: LPAREN . pattern RPAREN
/simple_pattern_not_ident: LPAREN . MODULE _*
/simple_pattern_not_ident: LPAREN . pattern COLON core_type RPAREN
/val_extra_ident: LPAREN . operator RPAREN
Expected a closing parenthesis, a module expression, a pattern, a type annotation, or an operator after `(` in various constructs.

---

/​labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA TILDE LPAREN . LIDENT COLON core_type RPAREN
Expected a label name after the opening parenthesis of a labeled tuple pattern (`~(`).

/​labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN . LIDENT _*
Expected a label name after `~(` in a labeled tuple pattern.

/​reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN . LIDENT COLON core_type RPAREN COMMA DOTDOT
Expected a closing parenthesis `)` after the type annotation in a labeled tuple pattern that ends with `..`.

---

/​labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT . COLON core_type RPAREN
Expected `:` after the label identifier inside `~(`.

/​labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT . COLON _*
Expected `:` after the label identifier inside `~(`.

/​reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT
Expected a closing parenthesis `)` after the type annotation in a labeled tuple pattern that ends with `..`.

---

/​labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON . core_type RPAREN
Expected a type after `:` in a labeled tuple pattern.

/​labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON . core_type _*
Expected a type after `:` in a labeled tuple pattern.

/​reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON . core_type RPAREN COMMA DOTDOT
Expected a closing parenthesis `)` after the type annotation in a labeled tuple pattern that ends with `..`.

---

/​labeled_tuple_pat_element_list_pattern_no_exn_: LABEL . simple_pattern _*
Expected a simple pattern after a label name.

/​labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA LABEL . simple_pattern
Expected a simple pattern after the second label in a labeled tuple pattern.

/​labeled_tuple_pat_element_list_pattern_no_exn_: LABEL . simple_pattern _*
(duplicate of first) – same as above.

​/​reversed_labeled_tuple_pattern_pattern_no_exn_: LABEL . simple_pattern COMMA DOTDOT
Expected `..` after a labeled element in a tuple pattern.

---

/​labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA . pattern_no_exn
Expected a pattern after a comma in a non‑exception tuple pattern.

​/​labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA . LABEL simple_pattern
Expected a labeled element after a comma.

​/​labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA . TILDE _*
Expected a `~`‑label after a comma.

​/​reversed_labeled_tuple_pattern_pattern_no_exn_: pattern_no_exn COMMA . DOTDOT
Expected `..` after a comma in a non‑exception tuple pattern.

---

/​labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA TILDE . LIDENT
Expected a label identifier after `~` in a tuple element.

​/​labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA TILDE . LPAREN LIDENT COLON core_type RPAREN
Expected a full label with type after `~(`.

​/​labeled_tuple_pat_element_list_pattern_no_exn_: TILDE . LIDENT _*
Expected a label identifier after `~`.

​/​labeled_tuple_pat_element_list_pattern_no_exn_: TILDE . LPAREN _*
Expected a `(` after `~` to start a labeled tuple element.

​/​reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE . LIDENT COMMA DOTDOT
Expected `..` after a labeled element.

​/​reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE . LPAREN LIDENT COLON core_type RPAREN COMMA DOTDOT
Expected `..` after a full labeled element.

---

/​labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA TILDE LPAREN . LIDENT COLON core_type RPAREN
Expected a label identifier after `~(`.

​/​labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN . LIDENT _*
Same as above – expecting label identifier.

​/​reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN . LIDENT COLON core_type RPAREN COMMA DOTDOT
Expected `..` after the closing parenthesis of a labeled element.

---

/​labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA TILDE LPAREN LIDENT . COLON core_type RPAREN
Expected `:` after the label identifier inside `~(`.

​/​labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT . COLON _*
Expected `:` after the label identifier inside `~(`.

​/​reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT
Expected `..` after the closing parenthesis of a labeled element.

---

/​labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA TILDE LPAREN LIDENT COLON . core_type RPAREN
Expected a type after `:` in a labeled element.

​/​labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON . core_type _*
Expected a type after `:` in a labeled element.

​/​reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON . core_type RPAREN COMMA DOTDOT
Expected `..` after the closing parenthesis of a labeled element.

---

/​labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN
Expected a closing parenthesis after the type in a labeled element.

​/​labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . RPAREN _*
Same as above – expecting `)`.

​/​reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT
Expected `..` after the closing parenthesis.

---

/​labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA LABEL . simple_pattern
Expected a simple pattern after a label identifier.

​/​labeled_tuple_pat_element_list_pattern_no_exn_: LABEL . simple_pattern _*
Expected a simple pattern after a label identifier.

​/​reversed_labeled_tuple_pattern_pattern_no_exn_: LABEL . simple_pattern COMMA DOTDOT
Expected `..` after a labeled element.

---

/​pattern_no_exn: pattern_no_exn COLONCOLON . pattern
Expected a pattern after the double‑colon operator.

---

/​pattern_no_exn: pattern_no_exn BAR . pattern
Expected a pattern after the vertical‑bar in a pattern alternative.

---

/​pattern_no_exn: pattern_no_exn AS . val_ident
Expected an identifier after `as` in a pattern.

---

/​possibly_poly_core_type_: reversed_nonempty_llist_typevar_ . DOT core_type
Expected `.` after a sequence of type variables in a poly‑type.

---

/​possibly_poly_core_type_: reversed_nonempty_llist_typevar_ DOT . core_type
Expected a core type after the dot.

---

/​simple_param_pattern: TILDE . LPAREN label_let_pattern RPAREN
Expected a label pattern after `~(`.

---

/​simple_param_pattern: TILDE . LIDENT
Expected an identifier after `~`.

---

/​simple_param_pattern: QUESTION . LPAREN label_let_pattern option_preceded_EQUAL_seq_expr__ RPAREN
Expected a label pattern after `?(`.

---

/​simple_param_pattern: QUESTION . LIDENT
Expected an identifier after `?`.

---

/​simple_param_pattern: QUESTION LPAREN . label_let_pattern option_preceded_EQUAL_seq_expr__ RPAREN
Expected a label pattern after `?(`.

---

/​simple_param_pattern: QUESTION LPAREN label_let_pattern . option_preceded_EQUAL_seq_expr__ RPAREN
Expected `=` or nothing after the label pattern inside `?(`.

---

/​option_preceded_EQUAL_seq_expr__: EQUAL . seq_expr
Expected an expression after `=`.

---

/​fun_expr: LET . ext list_attribute_ local_structure_item IN seq_expr
Expected `let`‑binding keywords (`rec`/`and`/pattern…) after `let`.

---

/​let_bindings_ext_: LET . ext list_attribute_ rec_flag let_binding_body list_post_item_attribute_
Expected `rec`, a pattern, or other binding start after `let`.

---

/​local_structure_item: MODULE . ext _*
Expected `module` keyword possibly followed by `rec` or a module name after `module`.

---

/​module_type_declaration: MODULE . TYPE ext list_attribute_ ident option_preceded_EQUAL_module_type__ list_post_item_attribute_
Expected `type` after `module`.

---

/​module_type_declaration: MODULE TYPE . ext list_attribute_ ident option_preceded_EQUAL_module_type__ list_post_item_attribute_
Expected an optional attribute (`%…`) after `module type`.

---

/​module_type_declaration: MODULE TYPE ext list_attribute_ . ident option_preceded_EQUAL_module_type__ list_post_item_attribute_
Expected a module type name after `module type` and attributes.

---

/​module_type: SIG . list_attribute_ signature END
Expected a signature (or attributes) after `sig`.

---

/​generic_type_declaration_no_nonrec_flag_type_subst_kind_: TYPE . ext _*
Expected optional attribute after `type`.

---

/​generic_type_declaration_nonrec_flag_type_kind_: TYPE . ext _*
Same as above – attribute after `type`.

---

/​signature_item: TYPE . ext _*
Expected optional attribute after `type` in a signature.

---

/​generic_type_declaration_no_nonrec_flag_type_subst_kind_: TYPE ext list_attribute_ NONREC . type_parameters LIDENT COLONEQUAL …
Expected type parameters after `type nonrec`.

---

/​generic_type_declaration_nonrec_flag_type_kind_: TYPE ext list_attribute_ NONREC . type_parameters LIDENT type_kind …
Same – expect type parameters after `type nonrec`.

---

/​signature_item: TYPE ext list_attribute_ NONREC . type_parameters type_longident PLUSEQ …
Expected type parameters after `type nonrec` in a signature.

---

/​type_parameters: LPAREN . reversed_separated_nonempty_llist_COMMA_type_parameter_ RPAREN
Expected a type parameter after `(`.

---

/​type_parameter: type_variance . type_variable
Expected a type variable after a variance annotation.

---

/​type_variable: QUOTE . ident
Expected an identifier after `'`.

---

/​type_parameters: LPAREN reversed_separated_nonempty_llist_COMMA_type_parameter_ . RPAREN
Expected `)` to close the type‑parameter list.

---

/​generic_type_declaration_no_nonrec_flag_type_subst_kind_: TYPE ext list_attribute_ NONREC type_parameters . LIDENT COLONEQUAL …
Expected the type name after the type‑parameter list.

---

/​generic_type_declaration_nonrec_flag_type_kind_: TYPE ext list_attribute_ NONREC type_parameters . LIDENT type_kind …
Same – expect the type name.

---

/​signature_item: TYPE ext list_attribute_ NONREC type_parameters . type_longident PLUSEQ …
Expect a long identifier after the type‑parameter list.

---

/​type_kind: EQUAL . nonempty_type_kind
Expected a type definition after `=`.

---

/​atomic_type: LPAREN . reversed_separated_nontrivial_llist_COMMA_core_type_ _*
/​constr_extra_nonprefix_ident: LPAREN . RPAREN
/​constr_ident: LPAREN . COLONCOLON RPAREN
/​delimited_type_supporting_local_open: LPAREN . core_type RPAREN
/​delimited_type_supporting_local_open: LPAREN . MODULE ext list_attribute_ module_type RPAREN
/​function_type: LPAREN . reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER function_type
Expected a type (or module type) after `(`; specifically a closed parenthesised type, a module type, or the start of a function type.

---

/​function_type: LIDENT COLON . LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER function_type
/​function_type: LIDENT COLON . tuple_type MINUSGREATER function_type
/​function_type: LIDENT COLON . atomic_type _*
Expected a type after the colon in a labeled function type.

---

/​atomic_type: LPAREN . reversed_separated_nontrivial_llist_COMMA_core_type_ _*
/​delimited_type_supporting_local_open: LPAREN . core_type RPAREN
/​delimited_type_supporting_local_open: LPAREN . MODULE ext list_attribute_ module_type RPAREN
/​function_type: LIDENT COLON LPAREN . reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER function_type
Expected a type after `(` in a function type annotation.

---

/​object_type: LESS . meth_list GREATER
/​object_type: LESS . GREATER
Expected a method list or closing `>` after `<`.

---

/​object_type: LESS LPAREN . meth_list GREATER
Expected a method list or `>` after `<(`.

---

/​extension: LBRACKETPERCENT . attr_id payload RBRACKET
Expected an attribute identifier after `[%`.

---

/​generic_type_declaration_nonrec_flag_type_kind_: TYPE . ext _*
/​local_structure_item: TYPE . ext _*
Expected an optional attribute after `type`.

---

/​generic_type_declaration_nonrec_flag_type_kind_: TYPE ext list_attribute_ NONREC . type_parameters LIDENT type_kind …
/​local_structure_item: TYPE ext list_attribute_ NONREC . type_parameters type_longident PLUSEQ …
Expected the type‑parameter list after `type nonrec`.

---

/​generic_type_declaration_nonrec_flag_type_kind_: TYPE ext list_attribute_ NONREC type_parameters . LIDENT type_kind …
/​local_structure_item: TYPE ext list_attribute_ NONREC type_parameters . type_longident PLUSEQ …
Expect the type name after the type‑parameter list.

---

/​post_item_attribute: LBRACKETATAT . attr_id attr_payload RBRACKET
Expected an attribute identifier after `[@@`.

---

/​post_item_attribute: LBRACKETATAT attr_id . attr_payload RBRACKET
Expect the payload after the dot in a post‑item attribute.

---

/​fun_expr: TRY . ext list_attribute_ seq_expr WITH reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expected a `try` expression body after `try`.

---

/​labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA TILDE LPAREN . LIDENT COLON core_type RPAREN
Expected a label name after `~(` in a labeled tuple element.

---

/​labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT . COLON _*
Expected `:` after the label identifier inside `~(`.

---

/​reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT
Expected a closing parenthesis after the type annotation in a labeled tuple element that ends with `..`.

---

/​labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN
Expected a closing parenthesis after the type annotation.

---

/​labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . RPAREN _*
Expected a closing parenthesis after the type annotation.

---

/​reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT
Expected `..` after the closing parenthesis of the labeled element.

---

/​labeled_tuple_pat_element_list_pattern_no_exn_: LABEL . simple_pattern _*
/​labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA LABEL . simple_pattern
/​labeled_tuple_pat_element_list_pattern_no_exn_: LABEL . simple_pattern _*
/​reversed_labeled_tuple_pattern_pattern_no_exn_: LABEL . simple_pattern COMMA DOTDOT
Expected a simple pattern after a label name; optionally followed by `..`.

---

/​labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA . pattern_no_exn
/​labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA . LABEL simple_pattern
/​labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA . TILDE _*
/​reversed_labeled_tuple_pattern_pattern_no_exn_: pattern_no_exn COMMA . DOTDOT
Expected a pattern, a labeled element, or a `~` label after a comma; `..` terminates the tuple.

---

/​labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA TILDE . LIDENT
/​labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA TILDE . LPAREN LIDENT COLON core_type RPAREN
/​labeled_tuple_pat_element_list_pattern_no_exn_: TILDE . LIDENT _*
/​labeled_tuple_pat_element_list_pattern_no_exn_: TILDE . LPAREN _*
/​reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE . LIDENT COMMA DOTDOT
/​reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE . LPAREN LIDENT COLON core_type RPAREN COMMA DOTDOT
Expect a label identifier or a full `~(` label after a comma; `..` ends the tuple.

---

/​labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA TILDE LPAREN . LIDENT COLON core_type RPAREN
/​labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN . LIDENT _*
/​reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN . LIDENT COLON core_type RPAREN COMMA DOTDOT
Expect the label identifier after `~(`; `..` ends the tuple.

---

/​labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA TILDE LPAREN LIDENT . COLON core_type RPAREN
/​labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT . COLON _*
/​reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT
Expect `:` after the label identifier inside `~(`.

---

/​labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA TILDE LPAREN LIDENT COLON . core_type RPAREN
/​labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON . core_type _*
/​reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON . core_type RPAREN COMMA DOTDOT
Expect a type after `:` in a labeled element.

---

/​labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN
/​labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . RPAREN _*
/​reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT
Expect a closing parenthesis after the type; `..` may follow.

---

/​labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA LABEL . simple_pattern
/​labeled_tuple_pat_element_list_pattern_no_exn_: LABEL . simple_pattern _*
/​reversed_labeled_tuple_pattern_pattern_no_exn_: LABEL . simple_pattern COMMA DOTDOT
Expect a simple pattern after a label; `..` may terminate.

---

/​pattern_no_exn: pattern_no_exn COLONCOLON . pattern
Expected a pattern after `::` in a non‑exception pattern.

---

/​pattern_no_exn: pattern_no_exn BAR . pattern
Expected a pattern after `|` in a non‑exception pattern.

---

/​pattern_no_exn: pattern_no_exn AS . val_ident
Expected an identifier after `as` in a non‑exception pattern.

---

/​possibly_poly_core_type_: reversed_nonempty_llist_typevar_ . DOT core_type
Expect a `.` after type variables in a poly‑type.

---

/​possibly_poly_core_type_: reversed_nonempty_llist_typevar_ DOT . core_type
Expect a core type after the dot.

---

/​simple_param_pattern: TILDE . LPAREN label_let_pattern RPAREN
Expect a label pattern after `~(`.

---

/​simple_param_pattern: TILDE . LIDENT
Expect an identifier after `~`.

---

/​simple_param_pattern: QUESTION . LPAREN label_let_pattern option_preceded_EQUAL_seq_expr__ RPAREN
Expect a label pattern after `?(`.

---

/​simple_param_pattern: QUESTION . LIDENT
Expect an identifier after `?`.

---

/​option_preceded_EQUAL_seq_expr__: EQUAL . seq_expr
Expect an expression after `=`.

---

/​fun_expr: LET . ext list_attribute_ local_structure_item IN seq_expr
Expect a `let` binding (rec/and/pattern…) after `let`.

---

/​let_bindings_ext_: LET . ext list_attribute_ rec_flag let_binding_body list_post_item_attribute_
Expect `rec`/`and`/pattern after `let`.

---

/​local_structure_item: MODULE . ext _*
Expect a module name (or `rec`) after `module`.

---

/​module_type_declaration: MODULE . TYPE ext list_attribute_ ident option_preceded_EQUAL_module_type__ list_post_item_attribute_
Expect `type` after `module`.

---

/​module_type_declaration: MODULE TYPE . ext list_attribute_ ident option_preceded_EQUAL_module_type__ list_post_item_attribute_
Expect an attribute (`%…`) after `module type`.

---

/​module_type_declaration: MODULE TYPE ext list_attribute_ . ident option_preceded_EQUAL_module_type__ list_post_item_attribute_
Expect a module type identifier after `module type` and its attributes.

---

/​option_preceded_EQUAL_module_type__: EQUAL . module_type
Expect a module type after `=`.

---

/​functor_arg: LPAREN . RPAREN
/​functor_arg: LPAREN . module_name COLON module_type RPAREN
/​module_type: LPAREN . module_type RPAREN
Expect a `)` or a module name after `(` in a functor argument or a parenthesised module type.

---

/​module_type: FUNCTOR . list_attribute_ reversed_nonempty_llist_functor_arg_ MINUSGREATER module_type
Expect a functor argument after `functor`.

---

/​module_type: FUNCTOR list_attribute_ . reversed_nonempty_llist_functor_arg_ MINUSGREATER module_type
Expect a functor argument after `functor` and its attributes.

---

/​module_type: FUNCTOR list_attribute_ reversed_nonempty_llist_functor_arg_ . MINUSGREATER module_type
Expect `->` after the functor argument list.

---

/​module_expr: FUNCTOR . list_attribute_ reversed_nonempty_llist_functor_arg_ MINUSGREATER module_expr
Expect a functor argument after `functor` in a module expression.

---

/​module_expr: FUNCTOR list_attribute_ . reversed_nonempty_llist_functor_arg_ MINUSGREATER module_expr
Expect a functor argument after `functor` and its attributes.

---

/​module_expr: FUNCTOR list_attribute_ reversed_nonempty_llist_functor_arg_ . MINUSGREATER module_expr
Expect `->` after the functor argument list.

---

/​module_expr: module_expr LPAREN . RPAREN
/​paren_module_expr: LPAREN . module_expr _*
/​paren_module_expr: LPAREN . VAL list_attribute_ expr_colon_package_type RPAREN
Expect a `)` after `(` in a module application, or a module expression after `(`, or a packaged `val` after `(`.

---

/​paren_module_expr: LPAREN module_expr . COLON module_type RPAREN
/​paren_module_expr: LPAREN module_expr . RPAREN
Expect a `:` (followed by a module type) or a closing `)` after a parenthesised module expression.

---

/​paren_module_expr: LPAREN module_expr COLON . module_type RPAREN
Expect a module type after `:` in a parenthesised module expression.

---

/​paren_module_expr: LPAREN module_expr COLON module_type . RPAREN
Expect a closing `)` after the module type.

---

/​module_binding_body: COLON . module_type EQUAL module_expr
Expect a module type after `:` in a constrained module binding.

---

/​module_binding_body: COLON module_type . EQUAL module_expr
Expect `=` after the module type in a constrained module binding.

---

/​module_binding_body: COLON module_type EQUAL . module_expr
Expect a module expression after `=`.

---

/​list_and_module_binding_: AND . list_attribute_ module_name module_binding_body list_post_item_attribute_ list_and_module_binding_
Expect an attribute after `and` in a list of module bindings.

---

/​module_type: FUNCTOR list_attribute_ . reversed_nonempty_llist_functor_arg_ MINUSGREATER module_type
Expect a functor argument after `functor` and its attributes.

---

/​module_type: FUNCTOR list_attribute_ reversed_nonempty_llist_functor_arg_ MINUSGREATER . module_type
Expect `->` after the functor argument list.

---

/​module_type: LPAREN module_type . RPAREN
Expect a closing `)` after a parenthesised module type.

---

/​local_structure_item: MODULE ext list_attribute_ . REC module_name module_binding_body list_post_item_attribute_ list_and_module_binding_
/​local_structure_item: MODULE ext list_attribute_ . module_name module_binding_body list_post_item_attribute_
Expect `rec` or a module name after `module` with attributes.

---

/​local_structure_item: MODULE ext list_attribute_ REC . module_name module_binding_body list_post_item_attribute_ list_and_module_binding_
Expect a module name after `module rec`.

---

/​local_structure_item: MODULE ext list_attribute_ REC module_name . module_binding_body list_post_item_attribute_ list_and_module_binding_
Expect `=` or `:` after the module name in a `module rec` binding.

---

/​module_binding_body: EQUAL . module_expr
Expect a module expression after `=`.

---

/​module_expr: FUNCTOR . list_attribute_ reversed_nonempty_llist_functor_arg_ MINUSGREATER module_expr
Expect a functor argument after `functor` in a module expression.

---

/​module_expr: FUNCTOR list_attribute_ . reversed_nonempty_llist_functor_arg_ MINUSGREATER module_expr
Expect a functor argument after `functor` and its attributes.

---

/​module_expr: FUNCTOR list_attribute_ reversed_nonempty_llist_functor_arg_ . MINUSGREATER module_expr
Expect `->` after the functor argument list.

---

/​module_expr: module_expr LPAREN . RPAREN
Expect a closing `)` after `(` in a module application.

---

/​paren_module_expr: LPAREN module_expr . COLON module_type RPAREN
/​paren_module_expr: LPAREN module_expr . RPAREN
Expect a `:` (and a module type) or a closing `)` after a parenthesised module expression.

---

/​paren_module_expr: LPAREN module_expr COLON . module_type RPAREN
Expect a module type after `:`.

---

/​paren_module_expr: LPAREN module_expr COLON module_type . RPAREN
Expect a closing `)` after the module type.

---

/​letop_binding_body: simple_pattern COLON . core_type EQUAL seq_expr
Expect a type after `:` in a `letop` binding.

---

/​letop_binding_body: simple_pattern COLON core_type . EQUAL seq_expr
Expect `=` after the type annotation in a `letop` binding.

---

/​letop_binding_body: simple_pattern COLON core_type EQUAL . seq_expr
Expect an expression after `=` in a `letop` binding.

---

/​fun_expr: FOR . ext list_attribute_ pattern EQUAL seq_expr direction_flag seq_expr DO seq_expr DONE
Expect a `for` loop header (pattern, `=`, bounds, direction, `do`, …) after `for`.

---

/​fun_expr: FOR ext list_attribute_ . pattern EQUAL seq_expr direction_flag seq_expr DO seq_expr DONE
Expect a loop pattern after `for` with attributes.

---

/​fun_expr: FOR ext list_attribute_ pattern . EQUAL seq_expr direction_flag seq_expr DO seq_expr DONE
Expect `=` after the loop pattern.

---

/​fun_expr: FOR ext list_attribute_ pattern EQUAL . seq_expr direction_flag seq_expr DO seq_expr DONE
Expect the lower bound expression after `=`.

---

/​fun_expr: ASSERT . ext list_attribute_ simple_expr
Expect an assertion expression after `assert`.

---

/​fun_expr: ASSERT ext list_attribute_ . simple_expr
Expect an assertion expression after `assert` with attributes.

---

/​fun_expr: subtractive . fun_expr
/​fun_expr: subtractive . FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expect a sub‑expression after the unary minus operator (`-`).

---

/​fun_expr: subtractive FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expect an attribute after `- function`.

---

/​fun_expr: subtractive FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expect the match‑case list after `- function` and its attributes.

---

/​reversed_preceded_or_separated_nonempty_llist_BAR_match_case_: reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ BAR . match_case
Expect a pattern after `|` in a function’s match‑case list.

---

/​labeled_simple_expr: TILDE . LIDENT
/​labeled_simple_expr: TILDE . LPAREN LIDENT type_constraint RPAREN
Expect a label identifier after `~` in a labeled expression.

---

/​labeled_simple_expr: TILDE LPAREN . LIDENT type_constraint RPAREN
Expect a label identifier after `~(`.

---

/​labeled_simple_expr: TILDE LPAREN LIDENT . type_constraint RPAREN
Expect a type constraint after the label identifier inside `~(`.

---

/​labeled_simple_expr: TILDE LPAREN LIDENT type_constraint . RPAREN
Expect a closing `)` after the type constraint.

---

/​labeled_simple_expr: QUESTION . LIDENT
Expect a label identifier after `?` in a labeled expression.

---

/​labeled_simple_expr: OPTLABEL . simple_expr
Expect a simple expression after `?label:`.

---

/​labeled_simple_expr: LABEL . simple_expr
Expect a simple expression after `~label:`.

---

/​fun_expr: simple_expr DOTOP . LPAREN _*
/​fun_expr: simple_expr DOTOP . LBRACE _*
/​fun_expr: simple_expr DOTOP . LBRACKET _*
/​simple_expr: simple_expr DOTOP . LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN
/​simple_expr: simple_expr DOTOP . LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE
/​simple_expr: simple_expr DOTOP . LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET
Expect a parenthesised, braced, or bracketed argument after the `.+` operator.

---

/​fun_expr: simple_expr DOTOP LPAREN . separated_or_terminated_nonempty_list_SEMI_expr_ _*
/​simple_expr: simple_expr DOTOP LPAREN . separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN
Expect the argument list after `.+(`.

---

/​reversed_labeled_tuple_body: FUNCTION . ext _*
/​separated_or_terminated_nonempty_list_SEMI_expr_: FUNCTION . ext _*
Expect an attribute after `function`.

---

/​reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ _*
/​separated_or_terminated_nonempty_list_SEMI_expr_: FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ _*
Expect a match‑case list after `function` with attributes.

---

/​reversed_preceded_or_separated_nonempty_llist_BAR_match_case_: reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ BAR . match_case
Expect a pattern after `|` in a function’s match‑case list.

---

/​labeled_simple_expr: TILDE . LIDENT
/​labeled_simple_expr: TILDE . LPAREN LIDENT type_constraint RPAREN
Expect a label identifier after `~` (or after `~(`).

---

/​labeled_simple_expr: TILDE LPAREN . LIDENT type_constraint RPAREN
Expect a label identifier after `~(`.

---

/​labeled_simple_expr: TILDE LPAREN LIDENT . type_constraint RPAREN
Expect a type constraint after the label identifier inside `~(`.

---

/​labeled_simple_expr: TILDE LPAREN LIDENT type_constraint . RPAREN
Expect a closing `)` after the type constraint.

---

/​labeled_simple_expr: QUESTION . LIDENT
Expect a label identifier after `?`.

---

/​labeled_simple_expr: OPTLABEL . simple_expr
Expect a simple expression after `?label:`.

---

/​labeled_simple_expr: LABEL . simple_expr
Expect a simple expression after `~label:`.

---

/​fun_expr: simple_expr DOTOP . LPAREN _*
/​fun_expr: simple_expr DOTOP . LBRACE _*
/​fun_expr: simple_expr DOTOP . LBRACKET _*
/​simple_expr: simple_expr DOTOP . LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN
/​simple_expr: simple_expr DOTOP . LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE
/​simple_expr: simple_expr DOTOP . LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET
Expect a parenthesised, braced, or bracketed argument after the `.+` operator.

---

/​fun_expr: simple_expr DOTOP LPAREN . separated_or_terminated_nonempty_list_SEMI_expr_ _*
/​simple_expr: simple_expr DOTOP LPAREN . separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN
Expect the argument list after `.+(`.

---

/​extension: LBRACKETPERCENT . attr_id payload RBRACKET
Expect an attribute identifier after `[%`.

---

/​payload: QUESTION . pattern _*
Expect a pattern after `?` inside an extension payload.

---

/​payload: QUESTION pattern WHEN . seq_expr
Expect an expression after `when` in a conditional payload.

---

/​constr_extra_nonprefix_ident: LBRACKET . RBRACKET
/​simple_expr: LBRACKET . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET
Expect a closing `]` after `[` (or an expression list inside brackets).

---

/​simple_expr: LBRACELESS . separated_or_terminated_nonempty_list_SEMI_object_expr_field_ GREATERRBRACE
/​simple_expr: LBRACELESS . GREATERRBRACE
Expect a field list or `}>` after `{<`.

---

/​option_preceded_EQUAL_expr__: EQUAL . fun_expr
/​option_preceded_EQUAL_expr__: EQUAL . FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expect a function expression after `=`.

---

/​simple_expr: LBRACE . record_expr_content RBRACE
Expect a record field list after `{`.

---

/​simple_expr: BEGIN . ext _*
Expect an attribute after `begin` or a `begin … end` block.

---

/​simple_expr: BEGIN ext list_attribute_ . seq_expr END
/​simple_expr: BEGIN ext list_attribute_ . END
Expect a sequence expression (or `end`) after `begin` with attributes.

---

/​fun_expr: LAZY . ext list_attribute_ simple_expr
Expect a lazy expression after `lazy`.

---

/​fun_expr: LAZY ext list_attribute_ . simple_expr
Expect a simple expression after `lazy` and its attributes.

---

/​simple_expr: BANG . simple_expr
Expect an expression after the unary `!` operator.

---

/​simple_expr: simple_expr HASHOP . simple_expr
Expect an expression after a custom hash operator (`##`).

---

/​simple_expr: simple_expr HASH . LIDENT
Expect an identifier after `#`.

---

/​simple_expr: simple_expr DOTOP . LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN
/​simple_expr: simple_expr DOTOP . LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE
/​simple_expr: simple_expr DOTOP . LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET
Expect an argument list after the `.+` operator.

---

/​fun_expr: IF . ext _*
Expect an `if` expression body after `if`.

---

/​fun_expr: IF ext list_attribute_ . seq_expr _*
Expect a condition expression after `if` with attributes.

---

/​reversed_labeled_tuple_body: FUNCTION . ext _*
/​separated_or_terminated_nonempty_list_SEMI_expr_: FUNCTION . ext _*
Expect an attribute after `function`.

---

/​reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ _*
/​separated_or_terminated_nonempty_list_SEMI_expr_: FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ _*
Expect a match‑case list after `function` with attributes.

---

/​reversed_preceded_or_separated_nonempty_llist_BAR_match_case_: reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ BAR . match_case
Expect a pattern after `|` in a function’s match‑case list.

---

/​labeled_simple_expr: TILDE . LIDENT
/​labeled_simple_expr: TILDE . LPAREN LIDENT type_constraint RPAREN
Expect a label identifier after `~` (or after `~(`).

---

/​labeled_simple_expr: TILDE LPAREN . LIDENT type_constraint RPAREN
Expect a label identifier after `~(`.

---

/​labeled_simple_expr: TILDE LPAREN LIDENT . type_constraint RPAREN
Expect a type constraint after the label identifier inside `~(`.

---

/​labeled_simple_expr: TILDE LPAREN LIDENT type_constraint . RPAREN
Expect a closing `)` after the type constraint.

---

/​labeled_simple_expr: QUESTION . LIDENT
Expect a label identifier after `?`.

---

/​labeled_simple_expr: OPTLABEL . simple_expr
Expect an expression after `?label:`.

---

/​labeled_simple_expr: LABEL . simple_expr
Expect an expression after `~label:`.

---

/​fun_expr: simple_expr DOTOP . LPAREN _*
/​fun_expr: simple_expr DOTOP . LBRACE _*
/​fun_expr: simple_expr DOTOP . LBracket _*
/​simple_expr: simple_expr DOTOP . LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN
/​simple_expr: simple_expr DOTOP . LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE
/​simple_expr: simple_expr DOTOP . LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET
Expect an argument after the `.+` operator.

---

/​fun_expr: simple_expr DOTOP LPAREN . separated_or_terminated_nonempty_list_SEMI_expr_ _*
/​simple_expr: simple_expr DOTOP LPAREN . separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN
Expect the argument list after `.+(`.

---

/​reversed_labeled_tuple_body: FUNCTION . ext _*
/​separated_or_terminated_nonempty_list_SEMI_expr_: FUNCTION . ext _*
Expect an attribute after `function`.

---

/​reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ _*
/​separated_or_terminated_nonempty_list_SEMI_expr_: FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ _*
Expect a match‑case list after `function` with attributes.

---

/​reversed_labeled_tuple_body: reversed_labeled_tuple_body COMMA . fun_expr
/reversed_labeled_tuple_body: reversed_labeled_tuple_body COMMA . FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
/reversed_labeled_tuple_body: reversed_labeled_tuple_body COMMA . LABEL simple_expr
/reversed_labeled_tuple_body: reversed_labeled_tuple_body COMMA . TILDE _*
Expect another tuple element (expression, function case, labeled element, or `~`) after a comma.

---

/​reversed_labeled_tuple_body: reversed_labeled_tuple_body COMMA TILDE . LIDENT
/reversed_labeled_tuple_body: reversed_labeled_tuple_body COMMA TILDE . LPAREN LIDENT type_constraint RPAREN
/reversed_labeled_tuple_body: TILDE . LIDENT _*
/reversed_labeled_tuple_body: TILDE . LPAREN _*
Expect a label identifier (or a parenthesised label) after a comma and `~`.

---

/​reversed_labeled_tuple_body: reversed_labeled_tuple_body COMMA TILDE LPAREN . LIDENT type_constraint RPAREN
/reversed_labeled_tuple_body: TILDE LPAREN . LIDENT _*
Expect a label identifier after `~(`.

---

/​reversed_labeled_tuple_body: reversed_labeled_tuple_body COMMA TILDE LPAREN LIDENT . type_constraint RPAREN
/reversed_labeled_tuple_body: TILDE LPAREN LIDENT . type_constraint _*
Expect a type constraint after the label identifier inside `~(`.

---

/​reversed_labeled_tuple_body: reversed_labeled_tuple_body COMMA TILDE LPAREN LIDENT type_constraint . RPAREN
/reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint . RPAREN _*
Expect a closing `)` after the type constraint.

---

/​reversed_labeled_tuple_body: reversed_labeled_tuple_body COMMA LABEL . simple_expr
/reversed_labeled_tuple_body: LABEL . simple_expr _*
Expect a simple expression after a label following a comma.

---

/​reversed_labeled_tuple_body: reversed_labeled_tuple_body COMMA FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
/reversed_labeled_tuple_body: FUNCTION . ext _*
Expect a function case after a comma and `function`.

---

/​reversed_labeled_tuple_body: reversed_labeled_tuple_body COMMA FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
/reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ _*
Expect a match‑case list after `function` with attributes, following a comma.

---

/​fun_expr: let_bindings_ext_ . IN seq_expr
Expect `in` after a `let` binding.

/fun_expr: let_bindings_ext_ IN . seq_expr
Expected an expression after `in` in a let‑binding.

---
/fun_expr: fun_expr STAR . fun_expr
fun_expr: fun_expr STAR . FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expected a right‑hand expression after the `*` operator.

---
/fun_expr: fun_expr STAR FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . ext _*
After `* function` an attribute list (or the match cases) is expected.

---
/fun_expr: fun_expr STAR FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ _*
After `* function …` the function body (match cases) is expected.

---
/fun_expr: fun_expr PLUSEQ . fun_expr
fun_expr: fun_expr PLUSEQ . FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expected an expression after the `+=` operator.

---
/fun_expr: fun_expr PLUSEQ FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . ext _*
After `+= function` an attribute list (or the match cases) is expected.

---
/fun_expr: fun_expr PLUSEQ FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ _*
After `+= function …` the function body (match cases) is expected.

---
/fun_expr: fun_expr PLUSDOT . fun_expr
fun_expr: fun_expr PLUSDOT . FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expected an expression after the `+.` operator.

---
/fun_expr: fun_expr PLUSDOT FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . ext _*
After `+. function` an attribute list (or the match cases) is expected.

---
/fun_expr: fun_expr PLUSDOT FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ _*
After `+. function …` the function body (match cases) is expected.

---
/fun_expr: fun_expr PLUS . fun_expr
fun_expr: fun_expr PLUS . FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expected an expression after the `+` operator.

---
/fun_expr: fun_expr PLUS FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . ext _*
After `+ function` an attribute list (or the match cases) is expected.

---
/fun_expr: fun_expr PLUS FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ _*
After `+ function …` the function body (match cases) is expected.

---
/fun_expr: fun_expr PERCENT . fun_expr
fun_expr: fun_expr PERCENT . FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expected an expression after the `%` operator.

---
/fun_expr: fun_expr PERCENT FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . ext _*
After `% function` an attribute list (or the match cases) is expected.

---
/fun_expr: fun_expr PERCENT FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ _*
After `% function …` the function body (match cases) is expected.

---
/fun_expr: fun_expr OR . fun_expr
fun_expr: fun_expr OR . FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expected an expression after the `or` operator.

---
/fun_expr: fun_expr OR FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . ext _*
After `or function` an attribute list (or the match cases) is expected.

---
/fun_expr: fun_expr OR FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ _*
After `or function …` the function body (match cases) is expected.

---
/fun_expr: fun_expr MINUSDOT . fun_expr
fun_expr: fun_expr MINUSDOT . FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expected an expression after the `-.` operator.

---
/fun_expr: fun_expr MINUSDOT FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . ext _*
After `-. function` an attribute list (or the match cases) is expected.

---
/fun_expr: fun_expr MINUSDOT FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ _*
After `-. function …` the function body (match cases) is expected.

---
/fun_expr: fun_expr MINUS . fun_expr
fun_expr: fun_expr MINUS . FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expected an expression after the `-` operator.

---
/fun_expr: fun_expr MINUS FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . ext _*
After `- function` an attribute list (or the match cases) is expected.

---
/fun_expr: fun_expr MINUS FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ _*
After `- function …` the function body (match cases) is expected.

---
/fun_expr: fun_expr LESS . fun_expr
fun_expr: fun_expr LESS . FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expected an expression after the `<` operator.

---
/fun_expr: fun_expr LESS FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . ext _*
After `< function` an attribute list (or the match cases) is expected.

---
/fun_expr: fun_expr LESS FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ _*
After `< function …` the function body (match cases) is expected.

---
/fun_expr: fun_expr INFIXOP4 . fun_expr
fun_expr: fun_expr INFIXOP4 . FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expected an expression after the exponentiation operator `**`.

---
/fun_expr: fun_expr INFIXOP4 FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . ext _*
After `** function` an attribute list (or the match cases) is expected.

---
/fun_expr: fun_expr INFIXOP4 FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ _*
After `** function …` the function body (match cases) is expected.

---
/fun_expr: fun_expr INFIXOP3 . fun_expr
fun_expr: fun_expr INFIXOP3 . FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expected an expression after the `land` operator.

---
/fun_expr: fun_expr INFIXOP3 FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . ext _*
After `land function` an attribute list (or the match cases) is expected.

---
/fun_expr: fun_expr INFIXOP3 FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ _*
After `land function …` the function body (match cases) is expected.

---
/fun_expr: fun_expr INFIXOP2 . fun_expr
fun_expr: fun_expr INFIXOP2 . FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expected an expression after the `+!` operator.

---
/fun_expr: fun_expr INFIXOP2 FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . ext _*
After `+! function` an attribute list (or the match cases) is expected.

---
/fun_expr: fun_expr INFIXOP2 FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ _*
After `+! function …` the function body (match cases) is expected.

---
/fun_expr: fun_expr INFIXOP1 . fun_expr
fun_expr: fun_expr INFIXOP1 . FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expected an expression after the `^` operator.

---
/fun_expr: fun_expr INFIXOP1 FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . ext _*
After `^ function` an attribute list (or the match cases) is expected.

---
/fun_expr: fun_expr INFIXOP1 FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ _*
After `^ function …` the function body (match cases) is expected.

---
/fun_expr: fun_expr INFIXOP0 . fun_expr
fun_expr: fun_expr INFIXOP0 . FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expected an expression after the `!=` operator.

---
/fun_expr: fun_expr INFIXOP0 FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . ext _*
After `!= function` an attribute list (or the match cases) is expected.

---
/fun_expr: fun_expr INFIXOP0 FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ _*
After `!= function …` the function body (match cases) is expected.

---
/fun_expr: fun_expr GREATER . fun_expr
fun_expr: fun_expr GREATER . FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expected an expression after the `>` operator.

---
/fun_expr: fun_expr GREATER FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . ext _*
After `> function` an attribute list (or the match cases) is expected.

---
/fun_expr: fun_expr GREATER FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ _*
After `> function …` the function body (match cases) is expected.

---
/fun_expr: fun_expr EQUAL . fun_expr
fun_expr: fun_expr EQUAL . FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expected an expression after the `=` operator.

---
/fun_expr: fun_expr EQUAL FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . ext _*
After `= function` an attribute list (or the match cases) is expected.

---
/fun_expr: fun_expr EQUAL FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ _*
After `= function …` the function body (match cases) is expected.

---
/reversed_labeled_tuple_body: fun_expr COMMA . fun_expr
reversed_labeled_tuple_body: fun_expr COMMA . FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: fun_expr COMMA . LABEL simple_expr
reversed_labeled_tuple_body: fun_expr COMMA . TILDE _*
Expected an expression (or label / pattern) after the comma in a tuple.

---
/reversed_labeled_tuple_body: fun_expr COMMA TILDE . LIDENT
reversed_labeled_tuple_body: fun_expr COMMA TILDE . LPAREN LIDENT type_constraint RPAREN
reversed_labeled_tuple_body: TILDE . LIDENT _*
reversed_labeled_tuple_body: TILDE . LPAREN _*
Expected a label identifier after `~` (or a typed pattern) inside a tuple.

---
/reversed_labeled_tuple_body: fun_expr COMMA TILDE LPAREN . LIDENT type_constraint RPAREN
reversed_labeled_tuple_body: TILDE LPAREN . LIDENT _*
Expected a label name after `~(`.

---
/reversed_labeled_tuple_body: fun_expr COMMA TILDE LPAREN LIDENT . type_constraint RPAREN
reversed_labeled_tuple_body: TILDE LPAREN LIDENT . type_constraint _*
Expected a colon (`:`) after the type variable in a `~(x : …)` pattern.

---
/reversed_labeled_tuple_body: fun_expr COMMA TILDE LPAREN LIDENT type_constraint . RPAREN
reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint . RPAREN _*
Expected a closing parenthesis after the type constraint in a `~(x : t)` pattern.

---
/reversed_labeled_tuple_body: fun_expr COMMA LABEL . simple_expr
reversed_labeled_tuple_body: LABEL . simple_expr _*
Expected an expression after a labelled tuple element (`label:`).

---
/reversed_labeled_tuple_body: fun_expr COMMA FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . ext _*
After a comma, `function` must be followed by an optional attribute list and match cases.

---
/reversed_labeled_tuple_body: fun_expr COLONEQUAL . fun_expr
fun_expr: fun_expr COLONEQUAL . FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expected an expression after the assignment operator `:=`.

---
/reversed_labeled_tuple_body: fun_expr COLONEQUAL FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . ext _*
After `:= function` an attribute list (or match cases) is expected.

---
/reversed_labeled_tuple_body: fun_expr COLONCOLON . fun_expr
fun_expr: fun_expr COLONCOLON . FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expected an expression after the list‑concatenation operator `::`.

---
/reversed_labeled_tuple_body: fun_expr COLONCOLON FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . ext _*
After `:: function` an attribute list (or match cases) is expected.

---
/reversed_labeled_tuple_body: fun_expr BARBAR . fun_expr
fun_expr: fun_expr BARBAR . FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expected an expression after the logical-or operator `||`.

---
/reversed_labeled_tuple_body: fun_expr BARBAR FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . ext _*
After `|| function` an attribute list (or match cases) is expected.

---
/reversed_labeled_tuple_body: fun_expr AMPERSAND . fun_expr
fun_expr: fun_expr AMPERSAND . FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expected an expression after the bitwise‑and operator `&`.

---
/reversed_labeled_tuple_body: fun_expr AMPERSAND FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . ext _*
After `& function` an attribute list (or match cases) is expected.

---
/reversed_labeled_tuple_body: fun_expr AMPERAMPER . fun_expr
fun_expr: fun_expr AMPERAMPER . FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expected an expression after the logical‑and operator `&&`.

---
/reversed_labeled_tuple_body: fun_expr AMPERAMPER FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . ext _*
After `&& function` an attribute list (or match cases) is expected.

---
/fun_expr: additive . fun_expr
fun_expr: additive . FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expected an operand after a unary plus/minus operator.

---
/fun_expr: additive FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . ext _*
After `+ function` (or `- function`) an attribute list (or match cases) is expected.

---
/fun_seq_expr: fun_expr SEMI PERCENT . attr_id seq_expr
Expected an attribute identifier after the `%` that follows a semicolon.

---
/fun_seq_expr: fun_expr SEMI PERCENT attr_id . seq_expr
Expected an expression after the attribute identifier in a `%`‑prefixed statement.

---
/and_let_binding: AND . list_attribute_ let_binding_body list_post_item_attribute_
Expected attributes (or a let binding) after the keyword `and`.

---
/and_let_binding: AND list_attribute_ . let_binding_body list_post_item_attribute_
Expected a let binding after the attribute list that follows `and`.

---
/strict_binding: EQUAL . seq_expr
Expected an expression after `=` in a binding.

---
/let_binding_body_no_punning: val_ident COLON . reversed_nonempty_llist_typevar_ DOT core_type EQUAL seq_expr
let_binding_body_no_punning: val_ident COLON . TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL seq_expr
type_constraint: COLON . core_type _*
Expected a type (or type variables) after the colon in a type annotation.

---
/let_binding_body_no_punning: val_ident COLON TYPE . nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL seq_expr
Expected a type identifier after the keyword `type`.

---
/let_binding_body_no_punning: val_ident COLON TYPE nonempty_list_mkrhs_LIDENT__ . DOT core_type EQUAL seq_expr
Expected a dot (`.`) after the type name.

---
/let_binding_body_no_punning: val_ident COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type . EQUAL seq_expr
Expected `=` after the type annotation.

---
/let_binding_body_no_punning: val_ident COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL . seq_expr
Expected an expression after `=`.

---
/let_binding_body_no_punning: val_ident COLON reversed_nonempty_llist_typevar_ . DOT core_type EQUAL seq_expr
Expected a dot after the type‑variable list.

---
/let_binding_body_no_punning: val_ident COLON reversed_nonempty_llist_typevar_ DOT . core_type EQUAL seq_expr
Expected a core type after the dot.

---
/let_binding_body_no_punning: val_ident COLON reversed_nonempty_llist_typevar_ DOT core_type . EQUAL seq_expr
Expected `=` after the core type.

---
/let_binding_body_no_punning: val_ident COLON reversed_nonempty_llist_typevar_ DOT core_type EQUAL . seq_expr
Expected an expression after `=`.

---
/let_binding_body_no_punning: val_ident type_constraint . EQUAL seq_expr
Expected `=` after the type constraint.

---
/type_constraint: COLON core_type COLONGREATER core_type .
Expected an attribute (or expression) after a coercion type annotation.

---
/type_constraint: EQUAL . seq_expr
Expected an expression after `=` in a type constraint.

---
/strict_binding: fun_params . option_type_constraint_ EQUAL fun_body
Expected a type constraint (or `=`) after the function parameters.

---
/strict_binding: fun_params option_type_constraint_ . EQUAL fun_body
Expected `=` before the function body.

---
/strict_binding: fun_params option_type_constraint_ EQUAL . fun_body
Expected the function body after `=`.

---
/and_let_binding: AND . list_attribute_ let_binding_body list_post_item_attribute_
Expected attributes (or a let binding) after `and` in a `let*` sequence.

---
/and_let_binding: AND list_attribute_ . let_binding_body list_post_item_attribute_
Expected a let binding after the attribute list following `and`.

---
/strict_binding: EQUAL . seq_expr
(duplicate of earlier) Expected an expression after `=`.

---
/let_binding_body_no_punning: simple_pattern_not_ident COLON . core_type EQUAL seq_expr
Expected a core type after the colon in a pattern binding.

---
/let_binding_body_no_punning: simple_pattern_not_ident COLON core_type . EQUAL seq_expr
Expected `=` after the core type.

---
/let_binding_body_no_punning: simple_pattern_not_ident COLON core_type EQUAL . seq_expr
Expected an expression after `=`.

---
/fun_expr: simple_expr DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ . RPAREN _*
simple_expr: simple_expr DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ . RPAREN
Expected a closing parenthesis after the argument list of a dot‑operator call.

---
/fun_expr: simple_expr DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN LESSMINUS . fun_expr
fun_expr: simple_expr DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN LESSMINUS . FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expected an expression (or `function …`) after the assignment operator `<-` in a dot‑operator call.

---
/fun_expr: simple_expr DOTOP LBRACKET . separated_or_terminated_nonempty_list_SEMI_expr_ _*
simple_expr: simple_expr DOTOP LBRACKET . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET
Expected a list element after `[` in a dot‑operator call.

---
/fun_expr: simple_expr DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACKET _*
simple_expr: simple_expr DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACKET
Expected a closing `]` after the list elements of a dot‑operator call.

---
/fun_expr: simple_expr DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS . fun_expr
fun_expr: simple_expr DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS . FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expected an expression (or `function …`) after `<-` in a dot‑operator list update.

---
/fun_expr: simple_expr DOTOP LBRACE . separated_or_terminated_nonempty_list_SEMI_expr_ _*
simple_expr: simple_expr DOTOP LBRACE . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE
Expected a record field after `{` in a dot‑operator call.

---
/fun_expr: simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACE _*
simple_expr: simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACE
Expected a closing `}` after the record fields.

---
/fun_expr: simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS . fun_expr
fun_expr: simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS . FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expected an expression (or `function …`) after `<-` in a dot‑operator record update.

---
/fun_expr: simple_expr DOT label_longident LESSMINUS . fun_expr
fun_expr: simple_expr DOT label_longident LESSMINUS . FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expected an expression after `<-` when assigning to a field.

---
/fun_expr: simple_expr DOT label_longident LESSMINUS FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . ext _*
After `<- function` an attribute list (or match cases) is expected.

---
/fun_expr: FOR ext list_attribute_ pattern EQUAL seq_expr . direction_flag seq_expr DO seq_expr DONE
Expected `to` or `downto` after the upper bound of a `for` loop.

---
/fun_expr: FOR ext list_attribute_ pattern EQUAL seq_expr direction_flag . seq_expr DO seq_expr DONE
Expected the lower bound expression after `to`/`downto`.

---
/fun_expr: FOR ext list_attribute_ pattern EQUAL seq_expr direction_flag seq_expr . DO seq_expr DONE
Expected the keyword `do` after the range expressions.

---
/fun_expr: FOR ext list_attribute_ pattern EQUAL seq_expr direction_flag seq_expr DO . seq_expr DONE
Expected the loop body expression after `do`.

---
/letop_binding_body: pattern_no_exn EQUAL . seq_expr
Expected an expression after `=` in a `let*` pattern binding.

---
/fun_expr: LETOP letop_bindings . IN seq_expr
Expected `in` followed by an expression after a `let*` sequence.

---
/fun_expr: LETOP letop_bindings IN . seq_expr
Expected an expression after `in` in a `let*` construct.

---
/letop_bindings: letop_bindings ANDOP . letop_binding_body
Expected a binding body after the `and` in a `let*` multi‑binding.

---
/simple_expr: mod_longident DOT LPAREN seq_expr . RPAREN
Expected a closing parenthesis after a module‑qualified function call.

---
/simple_expr: mod_longident DOT LBRACKETBAR . separated_or_terminated_nonempty_list_SEMI_expr_ BARRBRACKET
simple_expr: mod_longident DOT LBRACKETBAR . BARRBRACKET
Expected either list elements or the closing `|]` after `|[`.

---
/simple_expr: mod_longident DOT LBRACKETBAR separated_or_terminated_nonempty_list_SEMI_expr_ . BARRBRACKET
Expected the closing `|]` after the array elements.

---
/simple_expr: mod_longident DOT LBRACKET . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET
simple_expr: mod_longident DOT LBRACKET . RBRACKET
Expected a list element after `[` in a module‑qualified array access.

---
/simple_expr: mod_longident DOT LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACKET
Expected the closing `]` after the array elements.

---
/simple_expr: mod_longident DOT LBRACELESS . separated_or_terminated_nonempty_list_SEMI_object_expr_field_ GREATERRBRACE
Expected a field after `{<` in a module‑qualified object literal.

---
/simple_expr: mod_longident DOT LBRACELESS separated_or_terminated_nonempty_list_SEMI_object_expr_field_ . GREATERRBRACE
Expected the closing `>}` after the object fields.

---
/simple_expr: mod_longident DOT LBRACE . record_expr_content . RBRACE
Expected a closing `}` after a record literal.

---
/constr_longident: mod_longident DOT . LPAREN COLONCOLON RPAREN
simple_expr: mod_longident DOT . LPAREN seq_expr RPAREN
simple_expr: mod_longident DOT . LBRACELESS separated_or_terminated_nonempty_list_SEMI_object_expr_field_ GREATERRBRACE
simple_expr: mod_longident DOT . LPAREN RPAREN
simple_expr: mod_longident DOT . LBRACE record_expr_content RBRACE
simple_expr: mod_longident DOT . LBRACKETBAR _*
simple_expr: mod_longident DOT . LBRACKET _*
simple_expr: mod_longident DOT . LPAREN MODULE ext list_attribute_ module_expr COLON module_type RPAREN
Expected a component (e.g., `(`, `{`, `[`, `|[`, etc.) after the dot of a module path.

---
/reversed_labeled_tuple_body: FUNCTION . ext _*
reversed_labeled_tuple_body: LABEL simple_expr COMMA FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
After a comma, `function` must be followed by an optional attribute list and match cases.

---
/reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ _*
reversed_labeled_tuple_body: LABEL simple_expr COMMA FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
After `function …` an attribute list and match cases are required.

---
/match_case: pattern WHEN seq_expr . MINUSGREATER seq_expr
Expected `->` after the guard expression in a `when` clause.

---
/match_case: pattern WHEN seq_expr MINUSGREATER . seq_expr
Expected the right‑hand side expression after `->` in a `when` clause.

---
/match_case: pattern MINUSGREATER . seq_expr
match_case: pattern MINUSGREATER . DOT
Expected an expression (or a dot for record syntax) after `->` in a match case.

---
/fun_expr: IF ext list_attribute_ seq_expr . THEN _*
Expected a `then` branch after the condition of an `if` expression.

---
/fun_expr: IF ext list_attribute_ seq_expr THEN . fun_expr _*
fun_expr: IF ext list_attribute_ seq_expr THEN . FUNCTION _*
fun_expr: IF ext list_attribute_ seq_expr THEN . fun_expr
fun_expr: IF ext list_attribute_ seq_expr THEN . FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expected an expression (or a `function …`) after `then`.

---
/fun_expr: IF ext list_attribute_ seq_expr THEN FUNCTION . ext _*
reversed_labeled_tuple_body: FUNCTION . ext _*
After `then function` an attribute list (or match cases) is expected.

---
/fun_expr: IF ext list_attribute_ seq_expr THEN FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ _*
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ _*
After `then function …` the function body (match cases) is expected.

---
/fun_expr: IF ext list_attribute_ seq_expr THEN fun_expr ELSE . fun_expr
fun_expr: IF ext list_attribute_ seq_expr THEN fun_expr ELSE . FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expected the `else` branch after an `if … then` expression.

---
/fun_expr: IF ext list_attribute_ seq_expr THEN fun_expr ELSE FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . ext _*
After `else function` an attribute list (or match cases) is expected.

---
/fun_expr: IF ext list_attribute_ seq_expr THEN fun_expr ELSE FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ _*
After `else function …` the function body (match cases) is expected.

---
/simple_expr: simple_expr DOT . label_longident _*
simple_expr: simple_expr DOT . LPAREN _*
simple_expr: simple_expr DOT . LBRACE _*
simple_expr: simple_expr DOT . LBRACKET _*
simple_expr: simple_expr DOT . mod_longident _*
fun_expr: simple_expr DOT . label_longident _*
fun_expr: simple_expr DOT . LPAREN _*
fun_expr: simple_expr DOT . LBRACE _*
fun_expr: simple_expr DOT . LBRACKET _*
fun_expr: simple_expr DOT . mod_longident _*
Expected a field name, tuple, array, module access, or a parenthesised expression after a dot.

---
/fun_expr: simple_expr DOT LPAREN . seq_expr _*
simple_expr: simple_expr DOT LPAREN . seq_expr RPAREN
Expected an expression after `(` in a field access.

---
/simple_expr: simple_expr DOT LPAREN seq_expr . RPAREN _*
simple_expr: simple_expr DOT LPAREN seq_expr . RPAREN
Expected a closing `)` after the field‑access argument list.

---
/fun_expr: simple_expr DOT LPAREN seq_expr RPAREN LESSMINUS . fun_expr
fun_expr: simple_expr DOT LPAREN seq_expr RPAREN LESSMINUS . FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expected an expression (or `function …`) after the assignment operator `<-` in a field update.

---
/fun_expr: simple_expr DOT LBRACKET . seq_expr _*
simple_expr: simple_expr DOT LBRACKET . seq_expr RBRACKET
Expected an expression after `[` in a field update.

---
/simple_expr: simple_expr DOT LBRACKET seq_expr . RBRACKET _*
simple_expr: simple_expr DOT LBRACKET seq_expr . RBRACKET
Expected a closing `]` after the index expression.

---
/fun_expr: simple_expr DOT LBRACKET seq_expr RBRACKET LESSMINUS . fun_expr
fun_expr: simple_expr DOT LBRACKET seq_expr RBRACKET LESSMINUS . FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expected an expression (or `function …`) after `<-` in an array element update.

---
/fun_expr: simple_expr DOT LBRACE . seq_expr _*
simple_expr: simple_expr DOT LBRACE . seq_expr RBRACE
Expected an expression after `{` in a record field update.

---
/simple_expr: simple_expr DOT LBRACE seq_expr . RBRACE _*
simple_expr: simple_expr DOT LBRACE seq_expr . RBRACE
Expected a closing `}` after the field value.

---
/fun_expr: simple_expr DOT LBRACE seq_expr RBRACE LESSMINUS . fun_expr
fun_expr: simple_expr DOT LBRACE seq_expr RBRACE LESSMINUS . FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expected an expression (or `function …`) after `<-` in a record update.

---
/fun_expr: simple_expr DOT label_longident LESSMINUS . fun_expr
fun_expr: simple_expr DOT label_longident LESSMINUS . FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expected an expression after `<-` when assigning to a record field.

---
/fun_expr: FOR ext list_attribute_ pattern EQUAL seq_expr . direction_flag seq_expr DO seq_expr DONE
Expected `to` or `downto` after the upper bound of a `for` loop.

---
/fun_expr: FOR ext list_attribute_ pattern EQUAL seq_expr direction_flag . seq_expr DO seq_expr DONE
Expected the lower‑bound expression after `to`/`downto`.

---
/fun_expr: FOR ext list_attribute_ pattern EQUAL seq_expr direction_flag seq_expr . DO seq_expr DONE
Expected the keyword `do` after the range expressions.

---
/fun_expr: FOR ext list_attribute_ pattern EQUAL seq_expr direction_flag seq_expr DO . seq_expr DONE
Expected the loop body after `do`.

---
/fun_expr: simple_expr DOT . label_longident _*
simple_expr: simple_expr DOT . LPAREN _*
simple_expr: simple_expr DOT . LBRACE _*
simple_expr: simple_expr DOT . LBRACKET _*
simple_expr: simple_expr DOT . mod_longident _*
Expected a field name, a parenthesised expression, a record literal, an array literal, or a module path after a dot.

---
/fun_expr: simple_expr DOT OP . seq_expr (generic) – (covers all dot‑followed constructs).
Expected a valid suffix (field, method, array, record, or module access) after `.`.

---
Below is a complete list of error‑message drafts – one for each of the patterns you supplied (650 – 849).
Each entry follows the format you used in the examples: the pattern is reproduced exactly, then a short, clear description of what the parser was expecting at the point of failure.

---

### 650
```
simple_expr: simple_expr DOT mod_longident DOTOP LBRACE . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE
```
**Message** – After `.{` the parser expected a sequence of semicolon‑separated expressions (or `}` to close the record), but found a token that cannot start an expression.

---

### 651
```
simple_expr: simple_expr DOT mod_longident DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACE
```
**Message** – Expected `}` to close the record expression after the list of expressions.

---

### 652
```
simple_expr: simple_expr DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ . RPAREN
```
**Message** – Expected `)` to close the parenthesised expression after the list of expressions.

---

### 653
```
simple_expr: simple_expr DOTOP LBRACKET . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET
```
**Message** – Expected a list of semicolon‑separated expressions after `.[` (the opening of an array literal).

---

### 654
```
simple_expr: simple_expr DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACKET
```
**Message** – Expected `]` to close the array literal.

---

### 655
```
simple_expr: simple_expr DOTOP LBRACE . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE
```
**Message** – Expected a list of semicolon‑separated expressions after `.{` (the opening of a record literal).

---

### 656
```
simple_expr: simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACE
```
**Message** – Expected `}` to close the record literal.

---

### 657
```
simple_expr: BEGIN ext list_attribute_ seq_expr . END
```
**Message** – Expected `end` to terminate the `begin … end` block.

---

### 658
```
simple_expr: LBRACE record_expr_content . RBRACE
```
**Message** – Expected `}` to close the record after its fields.

---

### 659
```
option_preceded_EQUAL_expr__: EQUAL FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . ext _*
```
**Message** – After the keyword `function` a possible attribute list (`%…`) must follow, but none was found.

---

### 660
```
option_preceded_EQUAL_expr__: EQUAL FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ _*
```
**Message** – After the attribute list that follows `function` a match case (or `|`) is required.

---

### 661
```
simple_expr: LBRACELESS separated_or_terminated_nonempty_list_SEMI_object_expr_field_ . GREATERRBRACE
```
**Message** – Expected `}>` to close a polymorphic record literal.

---

### 662
```
simple_expr: LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACKET
```
**Message** – Expected `]` to close the array literal.

---

### 663
```
floating_attribute: LBRACKETATATAT . attr_id attr_payload RBRACKET
```
**Message** – After `[@@@` an attribute identifier is required.

---

### 664
```
floating_attribute: LBRACKETATATAT attr_id . attr_payload RBRACKET
```
**Message** – Expected the payload of a floating attribute after the identifier.

---

### 665
```
structure_item: INCLUDE . ext list_attribute_ module_expr list_post_item_attribute_
```
**Message** – After `include` a possible attribute list (`%…`) may appear, then a module expression is required.

---

### 666
```
structure_item: INCLUDE ext list_attribute_ . module_expr list_post_item_attribute_
```
**Message** – Expected a module expression after the optional attribute list following `include`.

---

### 667
```
primitive_declaration: EXTERNAL . ext list_attribute_ val_ident COLON possibly_poly_core_type_ EQUAL nonempty_list_raw_string_ list_post_item_attribute_
```
**Message** – After `external` an optional attribute list may appear; the next token must be the name of the external value.

---

### 668
```
primitive_declaration: EXTERNAL ext list_attribute_ . val_ident COLON possibly_poly_core_type_ EQUAL nonempty_list_raw_string_ list_post_item_attribute_
```
**Message** – Expected the identifier of the external value after the attribute list.

---

### 669
```
primitive_declaration: EXTERNAL ext list_attribute_ val_ident . COLON possibly_poly_core_type_ EQUAL nonempty_list_raw_string_ list_post_item_attribute_
```
**Message** – Expected `:` after the external identifier to introduce its type.

---

### 670
```
primitive_declaration: EXTERNAL ext list_attribute_ val_ident COLON . possibly_poly_core_type_ EQUAL nonempty_list_raw_string_ list_post_item_attribute_
```
**Message** – Expected a type (possibly polymorphic) after `:`.

---

### 671
```
primitive_declaration: EXTERNAL ext list_attribute_ val_ident COLON possibly_poly_core_type_ . EQUAL nonempty_list_raw_string_ list_post_item_attribute_
```
**Message** – Expected `=` after the type to bind the external implementation.

---

### 672
```
primitive_declaration: EXTERNAL ext list_attribute_ val_ident COLON possibly_poly_core_type_ EQUAL . nonempty_list_raw_string_ list_post_item_attribute_
```
**Message** – Expected a string literal (or list of strings) after `=`.

---

### 673
```
sig_exception_declaration: EXCEPTION . ext list_attribute_ constr_ident generalized_constructor_arguments list_attribute_ list_post_item_attribute_
str_exception_declaration: EXCEPTION . ext list_attribute_ constr_ident EQUAL constr_longident list_attribute_ list_post_item_attribute_
```
**Message** – After `exception` an optional attribute list may follow; the next token must be the exception constructor name.

---

### 674
```
sig_exception_declaration: EXCEPTION ext list_attribute_ . constr_ident generalized_constructor_arguments list_attribute_ list_post_item_attribute_
str_exception_declaration: EXCEPTION ext list_attribute_ . constr_ident EQUAL constr_longident list_attribute_ list_post_item_attribute_
```
**Message** – Expected the constructor identifier after the attribute list.

---

### 675
```
constr_extra_nonprefix_ident: LPAREN . RPAREN
constr_ident: LPAREN . COLONCOLON RPAREN
```
**Message** – After `(` the parser expects either `)` (empty tuple constructor) or `::` (polymorphic variant constructor).

---

### 676
```
constr_ident: LPAREN COLONCOLON . RPAREN
```
**Message** – Expected `)` to close the `(::)` constructor.

---

### 677
```
constr_extra_nonprefix_ident: LBRACKET . RBRACKET
```
**Message** – Expected `]` to close the empty list constructor `[]`.

---

### 678
```
generalized_constructor_arguments: OF . constructor_arguments
```
**Message** – After `of` a constructor argument must follow.

---

### 679
```
constructor_arguments: LBRACE . label_declarations RBRACE
```
**Message** – Expected a list of label declarations after `{` in a record constructor.

---

### 680
```
label_declaration: mutable_flag . LIDENT COLON possibly_poly_core_type_no_attr_ list_attribute_
label_declaration_semi: mutable_flag . LIDENT COLON possibly_poly_core_type_no_attr_ list_attribute_ SEMI list_attribute_
```
**Message** – After the mutability flag (`mutable`/`virtual`) the parser expects the field name.

---

### 681
```
label_declaration: mutable_flag LIDENT . COLON possibly_poly_core_type_no_attr_ list_attribute_
label_declaration_semi: mutable_flag LIDENT . COLON possibly_poly_core_type_no_attr_ list_attribute_ SEMI list_attribute_
```
**Message** – Expected `:` after the field name.

---

### 682
```
label_declaration: mutable_flag LIDENT COLON . possibly_poly_core_type_no_attr_ list_attribute_
label_declaration_semi: mutable_flag LIDENT COLON . possibly_poly_core_type_no_attr_ list_attribute_ SEMI list_attribute_
```
**Message** – Expected a type (or `_`) after `:`.

---

### 683
```
possibly_poly_core_type_no_attr_: reversed_nonempty_llist_typevar_ . DOT alias_type
```
**Message** – After a type‑variable list the parser expects `.` to start a type alias.

---

### 684
```
possibly_poly_core_type_no_attr_: reversed_nonempty_llist_typevar_ DOT . alias_type
```
**Message** – Expected the aliased type after the dot.

---

### 685
```
constructor_arguments: LBRACE label_declarations . RBRACE
```
**Message** – Expected `}` to close the record of constructor arguments.

---

### 686
```
constructor_arguments: reversed_separated_nonempty_llist_STAR_atomic_type_ . STAR atomic_type
```
**Message** – After the first `*` in a tuple of constructor arguments, another `*` followed by a type is required.

---

### 687
```
constructor_arguments: reversed_separated_nonempty_llist_STAR_atomic_type_ STAR . atomic_type
reversed_separated_nonempty_llist_STAR_atomic_type_: reversed_separated_nonempty_llist_STAR_atomic_type_ STAR . atomic_type
```
**Message** – Expected an atomic type after the second `*`.

---

### 688
```
str_exception_declaration: EXCEPTION ext list_attribute_ constr_ident EQUAL . constr_longident list_attribute_ list_post_item_attribute_
```
**Message** – Expected the long identifier of the exception after `=`.

---

### 689
```
constr_extra_nonprefix_ident: LPAREN . RPAREN
constr_longident: LPAREN . COLONCOLON RPAREN
```
**Message** – Same as pattern 675: after `(` expect either `)` or `::`.

---

### 690
```
constr_longident: mod_longident DOT . LPAREN COLONCOLON RPAREN
mk_longident_mod_longident_UIDENT_: mod_longident DOT . UIDENT
```
**Message** – After a module path and a dot, the parser expects either a constructor `(::)` or an identifier.

---

### 691
```
constr_longident: mod_longident DOT LPAREN . COLONCOLON RPAREN
```
**Message** – Expected `::` after the opening parenthesis.

---

### 692
```
generalized_constructor_arguments: COLON . constructor_arguments MINUSGREATER atomic_type
generalized_constructor_arguments: COLON . reversed_nonempty_llist_typevar_ DOT constructor_arguments MINUSGREATER atomic_type
generalized_constructor_arguments: COLON . atomic_type
generalized_constructor_arguments: COLON . reversed_nonempty_llist_typevar_ DOT atomic_type
```
**Message** – After `:` the parser expects either a list of constructor arguments (possibly preceded by type variables) or a simple type.

---

### 693
```
generalized_constructor_arguments: COLON reversed_nonempty_llist_typevar_ . DOT _*
```
**Message** – Expected `.` after the type‑variable list.

---

### 694
```
generalized_constructor_arguments: COLON reversed_nonempty_llist_typevar_ DOT . constructor_arguments MINUSGREATER atomic_type
generalized_constructor_arguments: COLON reversed_nonempty_llist_typevar_ DOT . atomic_type
```
**Message** – After the dot a constructor argument list or a type must follow.

---

### 695
```
generalized_constructor_arguments: COLON reversed_nonempty_llist_typevar_ DOT constructor_arguments . MINUSGREATER atomic_type
```
**Message** – Expected `->` after the constructor arguments.

---

### 696
```
generalized_constructor_arguments: COLON reversed_nonempty_llist_typevar_ DOT constructor_arguments MINUSGREATER . atomic_type
```
**Message** – Expected the result type after `->`.

---

### 697
```
generalized_constructor_arguments: COLON constructor_arguments . MINUSGREATER atomic_type
```
**Message** – Expected `->` after the constructor arguments.

---

### 698
```
generalized_constructor_arguments: COLON constructor_arguments MINUSGREATER . atomic_type
```
**Message** – Expected the result type after `->`.

---

### 699
```
open_description: OPEN . ext list_attribute_ mod_ext_longident list_post_item_attribute_
open_description: OPEN . BANG ext list_attribute_ mod_ext_longident list_post_item_attribute_
```
**Message** – After `open` an optional attribute list may appear; the next token must be a module long identifier (or `!` for a destructive open).

---

### 700
```
open_description: OPEN BANG . ext list_attribute_ mod_ext_longident list_post_item_attribute_
```
**Message** – Expected an attribute list after `open !`.

---

### 701
```
open_description: OPEN BANG ext list_attribute_ . mod_ext_longident list_post_item_attribute_
```
**Message** – Expected the module long identifier after the attribute list that follows `open !`.

---

### 702
```
open_description: OPEN ext list_attribute_ . mod_ext_longident list_post_item_attribute_
```
**Message** – Expected the module long identifier after the optional attribute list following `open`.

---

### 703
```
module_subst: MODULE . ext list_attribute_ UIDENT COLONEQUAL mod_ext_longident list_post_item_attribute_
module_type_declaration: MODULE . TYPE ext list_attribute_ ident option_preceded_EQUAL_module_type__ list_post_item_attribute_
module_type_subst: MODULE . TYPE ext list_attribute_ ident COLONEQUAL module_type list_post_item_attribute_
signature_item: MODULE . ext _*
```
**Message** – After `module` (or `module type`) the parser expects an optional attribute list, then the module name (or type name).

---

### 704
```
module_type_declaration: MODULE TYPE . ext list_attribute_ ident option_preceded_EQUAL_module_type__ list_post_item_attribute_
module_type_subst: MODULE TYPE . ext list_attribute_ ident COLONEQUAL module_type list_post_item_attribute_
```
**Message** – After `module type` an optional attribute list may appear; the next token must be the identifier.

---

### 705
```
module_type_declaration: MODULE TYPE ext list_attribute_ . ident option_preceded_EQUAL_module_type__ list_post_item_attribute_
module_type_subst: MODULE TYPE ext list_attribute_ . ident COLONEQUAL module_type list_post_item_attribute_
```
**Message** – Expected the identifier after the attribute list.

---

### 706
```
module_type_subst: MODULE TYPE ext list_attribute_ ident COLONEQUAL . module_type list_post_item_attribute_
```
**Message** – Expected a module type after the `=`.

---

### 707
```
module_subst: MODULE ext list_attribute_ . UIDENT COLONEQUAL mod_ext_longident list_post_item_attribute_
signature_item: MODULE ext list_attribute_ . module_name _*
signature_item: MODULE ext list_attribute_ . REC module_name COLON module_type list_post_item_attribute_ list_and_module_declaration_
```
**Message** – After the attribute list, the parser expects a module name (or `rec` followed by a module name).

---

### 708
```
module_subst: MODULE ext list_attribute_ UIDENT COLONEQUAL . mod_ext_longident list_post_item_attribute_
```
**Message** – Expected a module expression after `=`.

---

### 709
```
signature_item: MODULE ext list_attribute_ REC . module_name COLON module_type list_post_item_attribute_ list_and_module_declaration_
```
**Message** – Expected the module name after `rec`.

---

### 710
```
signature_item: MODULE ext list_attribute_ REC module_name . COLON module_type list_post_item_attribute_ list_and_module_declaration_
```
**Message** – Expected `:` after the module name.

---

### 711
```
signature_item: MODULE ext list_attribute_ REC module_name COLON . module_type list_post_item_attribute_ list_and_module_declaration_
```
**Message** – Expected a module type after `:`.

---

### 712
```
list_and_module_declaration_: AND . list_attribute_ module_name COLON module_type list_post_item_attribute_ list_and_module_declaration_
```
**Message** – After `and` an optional attribute list may appear; the next token must be a module name.

---

### 713
```
list_and_module_declaration_: AND list_attribute_ . module_name COLON module_type list_post_item_attribute_ list_and_module_declaration_
```
**Message** – Expected the module name after the attribute list.

---

### 714
```
list_and_module_declaration_: AND list_attribute_ module_name . COLON module_type list_post_item_attribute_ list_and_module_declaration_
```
**Message** – Expected `:` after the module name.

---

### 715
```
list_and_module_declaration_: AND list_attribute_ module_name COLON . module_type list_post_item_attribute_ list_and_module_declaration_
```
**Message** – Expected a module type after `:`.

---

### 716
```
signature_item: MODULE ext list_attribute_ module_name . module_declaration_body list_post_item_attribute_
signature_item: MODULE ext list_attribute_ module_name . EQUAL mod_longident list_post_item_attribute_
```
**Message** – Expected either a module body (`:` …) or `=` followed by a module identifier after the module name.

---

### 717
```
signature_item: MODULE ext list_attribute_ module_name EQUAL . mod_longident list_post_item_attribute_
```
**Message** – Expected a module identifier after `=`.

---

### 718
```
module_declaration_body: COLON . module_type
```
**Message** – Expected a module type after `:`.

---

### 719
```
module_declaration_body: functor_arg . module_declaration_body
```
**Message** – Expected another functor argument or a module body after the first functor argument.

---

### 720
```
signature_item: INCLUDE . ext list_attribute_ module_type list_post_item_attribute_
```
**Message** – After `include` an optional attribute list may appear; the next token must be a module type.

---

### 721
```
signature_item: INCLUDE ext list_attribute_ . module_type list_post_item_attribute_
```
**Message** – Expected the module type after the attribute list.

---

### 722
```
sig_exception_declaration: EXCEPTION . ext list_attribute_ constr_ident generalized_constructor_arguments list_attribute_ list_post_item_attribute_
```
**Message** – After `exception` an optional attribute list may appear; the next token must be the constructor name.

---

### 723
```
sig_exception_declaration: EXCEPTION ext list_attribute_ . constr_ident generalized_constructor_arguments list_attribute_ list_post_item_attribute_
```
**Message** – Expected the constructor identifier after the attribute list.

---

### 724
```
class_type_declarations: CLASS . TYPE ext list_attribute_ virtual_flag formal_class_parameters LIDENT EQUAL class_signature list_post_item_attribute_ list_and_class_type_declaration_
local_structure_item: CLASS . ext list_attribute_ virtual_flag formal_class_parameters LIDENT class_fun_binding list_post_item_attribute_ list_and_class_declaration_
```
**Message** – After `class` the parser expects either `type` (for a class type) or an optional attribute list followed by the class name.

---

### 725
```
class_type_declarations: CLASS TYPE . ext list_attribute_ virtual_flag formal_class_parameters LIDENT EQUAL class_signature list_post_item_attribute_ list_and_class_type_declaration_
```
**Message** – After `class type` an optional attribute list may appear; the next token must be the class name.

---

### 726
```
formal_class_parameters: LBRACKET reversed_separated_nonempty_llist_COMMA_type_parameter_ . RBRACKET
```
**Message** – Expected `]` to close the list of type parameters.

---

### 727
```
class_type_declarations: CLASS TYPE ext list_attribute_ virtual_flag formal_class_parameters . LIDENT EQUAL class_signature list_post_item_attribute_ list_and_class_type_declaration_
```
**Message** – Expected the class name after the type‑parameter list.

---

### 728
```
class_type_declarations: CLASS TYPE ext list_attribute_ virtual_flag formal_class_parameters LIDENT . EQUAL class_signature list_post_item_attribute_ list_and_class_type_declaration_
```
**Message** – Expected `=` after the class name.

---

### 729
```
class_type_declarations: CLASS TYPE ext list_attribute_ virtual_flag formal_class_parameters LIDENT EQUAL . class_signature list_post_item_attribute_ list_and_class_type_declaration_
```
**Message** – Expected the class signature after `=`.

---

### 730
```
class_signature: OBJECT . list_attribute_ class_self_type list_text_csig_class_sig_field__ END
```
**Message** – After `object` an optional attribute list may appear; the next token must be the self‑type (or `end` if the object has no fields).

---

### 731
```
class_self_type: LPAREN . core_type RPAREN
```
**Message** – Expected a core type after the opening parenthesis of the self‑type.

---

### 732
```
class_self_type: LPAREN core_type . RPAREN
```
**Message** – Expected `)` to close the self‑type.

---

### 733
```
class_signature: OBJECT list_attribute_ class_self_type . list_text_csig_class_sig_field__ END
```
**Message** – Expected a class signature field after the self‑type.

---

### 734
```
class_sig_field: VAL . list_attribute_ mutable_virtual_flags LIDENT COLON core_type list_post_item_attribute_
```
**Message** – After `val` an optional attribute list may appear; the next token must be the mutability/virtual flags (or the field name).

---

### 735
```
class_sig_field: VAL list_attribute_ mutable_virtual_flags . LIDENT COLON core_type list_post_item_attribute_
```
**Message** – Expected the field name after the attribute list.

---

### 736
```
class_sig_field: VAL list_attribute_ mutable_virtual_flags LIDENT . COLON core_type list_post_item_attribute_
```
**Message** – Expected `:` after the field name.

---

### 737
```
class_sig_field: METHOD . list_attribute_ private_virtual_flags LIDENT COLON possibly_poly_core_type_ list_post_item_attribute_
```
**Message** – After `method` an optional attribute list may appear; the next token must be the visibility flags (or the method name).

---

### 738
```
class_sig_field: METHOD list_attribute_ private_virtual_flags . LIDENT COLON possibly_poly_core_type_ list_post_item_attribute_
```
**Message** – Expected the method name after the attribute list.

---

### 739
```
class_sig_field: METHOD list_attribute_ private_virtual_flags LIDENT . COLON possibly_poly_core_type_ list_post_item_attribute_
```
**Message** – Expected `:` after the method name.

---

### 740
```
class_sig_field: INHERIT . list_attribute_ class_signature list_post_item_attribute_
```
**Message** – After `inherit` an optional attribute list may appear; the next token must start a class expression.

---

### 741
```
class_sig_field: INHERIT list_attribute_ . class_signature list_post_item_attribute_
```
**Message** – Expected a class expression after the attribute list.

---

### 742
```
class_signature: LET . OPEN _*
```
**Message** – After `let` the parser expects the keyword `open` (or a `let` binding) to start a local opening.

---

### 743
```
class_signature: LET OPEN . list_attribute_ mod_longident IN class_signature
class_signature: LET OPEN . BANG list_attribute_ mod_longident IN class_signature
```
**Message** – After `let open` an optional attribute list may appear; the next token must be a module identifier (or `!` for destructive open).

---

### 744
```
class_signature: LET OPEN BANG . list_attribute_ mod_longident IN class_signature
```
**Message** – Expected an attribute list after `let open !`.

---

### 745
```
class_signature: LET OPEN BANG list_attribute_ . mod_longident IN class_signature
```
**Message** – Expected the module identifier after the attribute list.

---

### 746
```
class_signature: LET OPEN BANG list_attribute_ mod_longident . IN class_signature
```
**Message** – Expected `in` after the module identifier.

---

### 747
```
class_signature: LBRACKET . reversed_separated_nonempty_llist_COMMA_core_type_ RBRACKET clty_longident
```
**Message** – Expected `]` to close the type parameter list of a polymorphic class type.

---

### 748
```
class_signature: LBRACKET reversed_separated_nonempty_llist_COMMA_core_type_ . RBRACKET clty_longident
```
**Message** – Same as 747 – expecting `]`.

---

### 749
```
class_signature: LBRACKET reversed_separated_nonempty_llist_COMMA_core_type_ RBRACKET . clty_longident
```
**Message** – Expected the class type identifier after the closed bracket.

---

### 750
```
reversed_separated_nonempty_llist_COMMA_core_type_: reversed_separated_nonempty_llist_COMMA_core_type_ COMMA . core_type
```
**Message** – Expected another core type after the comma in the list.

---

### 751
```
class_signature: LET OPEN list_attribute_ . mod_longident IN class_signature
```
**Message** – Expected the module identifier after the optional attribute list.

---

### 752
```
class_signature: LET OPEN list_attribute_ mod_longident . IN class_signature
mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT
```
**Message** – Expected `in` after the module identifier.

---

### 753
```
class_signature: LET OPEN list_attribute_ mod_longident IN . class_signature
```
**Message** – Expected the continuation of the `let open … in` construct (a class signature).

---

### 754
```
class_sig_field: CONSTRAINT . list_attribute_ constrain_field list_post_item_attribute_
```
**Message** – After `constraint` an optional attribute list may appear; the next token must start a constraint (`type = type`).

---

### 755
```
class_sig_field: CONSTRAINT list_attribute_ . constrain_field list_post_item_attribute_
```
**Message** – Expected the left‑hand side of a type constraint.

---

### 756
```
constrain_field: core_type . EQUAL core_type
```
**Message** – Expected `=` after the left‑hand side type.

---

### 757
```
constrain_field: core_type EQUAL . core_type
```
**Message** – Expected the right‑hand side type after `=`.

---

### 758
```
class_signature: OBJECT list_attribute_ class_self_type list_text_csig_class_sig_field__ . END
```
**Message** – Expected `end` to close the object type.

---

### 759
```
list_and_class_type_declaration_: AND . list_attribute_ virtual_flag formal_class_parameters LIDENT EQUAL class_signature list_post_item_attribute_ list_and_class_type_declaration_
```
**Message** – After `and` an optional attribute list may appear; the next token must be a class name.

---

### 760
```
list_and_class_type_declaration_: AND list_attribute_ virtual_flag formal_class_parameters . LIDENT EQUAL class_signature list_post_item_attribute_ list_and_class_type_declaration_
```
**Message** – Expected the class name after the attribute list.

---

### 761
```
list_and_class_type_declaration_: AND list_attribute_ virtual_flag formal_class_parameters LIDENT . EQUAL class_signature list_post_item_attribute_ list_and_class_type_declaration_
```
**Message** – Expected `=` after the class name.

---

### 762
```
list_and_class_type_declaration_: AND list_attribute_ virtual_flag formal_class_parameters LIDENT EQUAL . class_signature list_post_item_attribute_ list_and_class_type_declaration_
```
**Message** – Expected the class signature after `=`.

---

### 763
```
signature_item: CLASS . ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON class_type list_post_item_attribute_ list_and_class_description_
```
**Message** – After `class` an optional attribute list may appear; the next token must be the class name.

---

### 764
```
signature_item: CLASS TYPE . ext list_attribute_ virtual_flag formal_class_parameters LIDENT EQUAL class_signature list_post_item_attribute_ list_and_class_type_declaration_
```
**Message** – Same as 763 but for a class type declaration.

---

### 765
```
signature_item: CLASS TYPE ext list_attribute_ . ident type_longident PLUSEQ private_flag reversed_bar_llist_extension_constructor_declaration_ list_post_item_attribute_
```
**Message** – Expected the identifier after the attribute list.

---

### 766
```
signature_item: CLASS TYPE ext list_attribute_ ident . PLUSEQ private_flag reversed_bar_llist_extension_constructor_declaration_ list_post_item_attribute_
```
**Message** – Expected `+=` after the type name.

---

### 767
```
signature_item: CLASS TYPE ext list_attribute_ ident PLUSEQ . private_flag reversed_bar_llist_extension_constructor_declaration_ list_post_item_attribute_
```
**Message** – Expected the visibility flag (`private` or nothing) after `+=`.

---

### 768
```
signature_item: CLASS TYPE ext list_attribute_ ident PLUSEQ private_flag . reversed_bar_llist_extension_constructor_declaration_ list_post_item_attribute_
```
**Message** – Expected a constructor declaration after the visibility flag.

---

### 769
```
signature_item: CLASS TYPE ext list_attribute_ ident PLUSEQ private_flag reversed_bar_llist_extension_constructor_declaration_ . list_post_item_attribute_
```
**Message** – Expected the end of the class type declaration (or another constructor) after the constructor list.

---

### 770
```
signature_item: CLASS ext list_attribute_ virtual_flag formal_class_parameters . LIDENT COLON class_type list_post_item_attribute_ list_and_class_description_
```
**Message** – Expected the class name after the attribute list.

---

### 771
```
signature_item: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT . COLON class_type list_post_item_attribute_ list_and_class_description_
```
**Message** – Expected `:` after the class name.

---

### 772
```
signature_item: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON . class_type list_post_item_attribute_ list_and_class_description_
```
**Message** – Expected the class type after `:`.

---

### 773
```
class_type: LIDENT COLON . tuple_type MINUSGREATER class_type
```
**Message** – Expected a tuple type after `:`.

---

### 774
```
class_type: LIDENT COLON tuple_type . MINUSGREATER class_type
```
**Message** – Expected `->` after the tuple type.

---

### 775
```
class_type: LIDENT COLON tuple_type MINUSGREATER . class_type
```
**Message** – Expected the result class type after `->`.

---

### 776
```
class_type: optlabel . tuple_type MINUSGREATER class_type
```
**Message** – Expected a tuple type after the optional label.

---

### 777
```
class_type: optlabel tuple_type . MINUSGREATER class_type
```
**Message** – Expected `->` after the tuple type.

---

### 778
```
class_type: optlabel tuple_type MINUSGREATER . class_type
```
**Message** – Expected the result class type after `->`.

---

### 779
```
list_and_class_description_: AND . list_attribute_ virtual_flag formal_class_parameters LIDENT COLON class_type list_post_item_attribute_ list_and_class_description_
```
**Message** – After `and` an optional attribute list may appear; the next token must be a class name.

---

### 780
```
list_and_class_description_: AND list_attribute_ . virtual_flag formal_class_parameters LIDENT COLON class_type list_post_item_attribute_ list_and_class_description_
```
**Message** – Expected the visibility flag after the attribute list.

---

### 781
```
list_and_class_description_: AND list_attribute_ virtual_flag . formal_class_parameters LIDENT COLON class_type list_post_item_attribute_ list_and_class_description_
```
**Message** – Expected the formal class parameters after the visibility flag.

---

### 782
```
list_and_class_description_: AND list_attribute_ virtual_flag formal_class_parameters . LIDENT COLON class_type list_post_item_attribute_ list_and_class_description_
```
**Message** – Expected the class name.

---

### 783
```
list_and_class_description_: AND list_attribute_ virtual_flag formal_class_parameters LIDENT . COLON class_type list_post_item_attribute_ list_and_class_description_
```
**Message** – Expected `:` after the class name.

---

### 784
```
list_and_class_description_: AND list_attribute_ virtual_flag formal_class_parameters LIDENT COLON . class_type list_post_item_attribute_ list_and_class_description_
```
**Message** – Expected the class type.

---

### 785
```
list_generic_and_type_declaration_type_kind__: AND . list_attribute_ type_parameters LIDENT type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_ list_generic_and_type_declaration_type_kind__
```
**Message** – After `and` an optional attribute list may appear; the next token must be a type name.

---

### 786
```
list_generic_and_type_declaration_type_kind__: AND list_attribute_ . type_parameters LIDENT type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_ list_generic_and_type_declaration_type_kind__
```
**Message** – Expected the list of type parameters after the attribute list.

---

### 787
```
list_generic_and_type_declaration_type_kind__: AND list_attribute_ type_parameters . LIDENT type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_ list_generic_and_type_declaration_type_kind__
```
**Message** – Expected the type name after the type‑parameter list.

---

### 788
```
list_generic_and_type_declaration_type_subst_kind__: AND . list_attribute_ type_parameters LIDENT COLONEQUAL nonempty_type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_ list_generic_and_type_declaration_type_subst_kind__
```
**Message** – After `and` an optional attribute list may appear; the next token must be a type name.

---

### 789
```
list_generic_and_type_declaration_type_subst_kind__: AND list_attribute_ . type_parameters LIDENT COLONEQUAL nonempty_type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_ list_generic_and_type_declaration_type_subst_kind__
```
**Message** – Expected the type‑parameter list after the attribute list.

---

### 790
```
list_generic_and_type_declaration_type_subst_kind__: AND list_attribute_ type_parameters . LIDENT COLONEQUAL nonempty_type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_ list_generic_and_type_declaration_type_subst_kind__
```
**Message** – Expected the type name after the type‑parameter list.

---

### 791
```
list_generic_and_type_declaration_type_subst_kind__: AND list_attribute_ type_parameters LIDENT . COLONEQUAL nonempty_type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_ list_generic_and_type_declaration_type_subst_kind__
```
**Message** – Expected `=` after the type name.

---

### 792
```
list_generic_and_type_declaration_type_subst_kind__: AND list_attribute_ type_parameters LIDENT COLONEQUAL . nonempty_type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_ list_generic_and_type_declaration_type_subst_kind__
```
**Message** – Expected the type definition after `=`.

---

### 793
```
constr_extra_nonprefix_ident: LBRACKET . RBRACKET
delimited_type_supporting_local_open: LBRACKET . tag_field RBRACKET
delimited_type_supporting_local_open: LBRACKET . BAR reversed_separated_nonempty_llist_BAR_row_field_ RBRACKET
delimited_type_supporting_local_open: LBRACKET . row_field BAR reversed_separated_nonempty_llist_BAR_row_field_ RBRACKET
```
**Message** – Expected `]` to close the list or variant type after the opening `[`.

---

### 794
```
nonempty_type_kind: LBRACE label_declarations . RBRACE
```
**Message** – Expected `}` to close the record type.

---

### 795
```
nonempty_type_kind: EXTERNAL . STRING
```
**Message** – Expected a string literal after the keyword `external`.

---

### 796
```
generic_constructor_declaration_BAR_: BAR . constr_ident generalized_constructor_arguments list_attribute_
```
**Message** – After `|` the parser expects a constructor identifier.

---

### 797
```
nonempty_type_kind: core_type EQUAL . constructor_declarations
nonempty_type_kind: core_type EQUAL . PRIVATE constructor_declarations
nonempty_type_kind: core_type EQUAL . DOTDOT
nonempty_type_kind: core_type EQUAL . PRIVATE DOTDOT
nonempty_type_kind: core_type EQUAL . LBRACE label_declarations RBRACE
nonempty_type_kind: core_type EQUAL . PRIVATE LBRACE label_declarations RBRACE
```
**Message** – After `=` the parser expects either a constructor list, `..`, or a record definition (optionally preceded by `private`).

---

### 798
```
nonempty_type_kind: core_type EQUAL PRIVATE . constructor_declarations
nonempty_type_kind: core_type EQUAL PRIVATE . DOTDOT
nonempty_type_kind: core_type EQUAL PRIVATE . LBRACE label_declarations RBRACE
```
**Message** – Same as 797, but after the keyword `private`.

---

### 799
```
nonempty_type_kind: core_type EQUAL PRIVATE LBRACE label_declarations . RBRACE
```
**Message** – Expected `}` to close the private record definition.

---

### 800
```
class_type_declarations: CLASS . TYPE ext list_attribute_ virtual_flag formal_class_parameters LIDENT EQUAL class_signature list_post_item_attribute_ list_and_class_type_declaration_
local_structure_item: CLASS . ext list_attribute_ virtual_flag formal_class_parameters LIDENT class_fun_binding list_post_item_attribute_ list_and_class_declaration_
```
**Message** – After `class` the next token must be either `type` (for a class type) or an optional attribute list before the class name.

---

### 801
```
local_structure_item: CLASS ext list_attribute_ virtual_flag formal_class_parameters . LIDENT class_fun_binding list_post_item_attribute_ list_and_class_declaration_
```
**Message** – Expected the class name after the attribute list.

---

### 802
```
local_structure_item: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT . class_fun_binding list_post_item_attribute_ list_and_class_declaration_
```
**Message** – Expected `=` (or `:`) to start the class definition after the class name.

---

### 803
```
constr_extra_nonprefix_ident: LPAREN . RPAREN
constr_longident: LPAREN . COLONCOLON RPAREN
simple_param_pattern: LPAREN . pattern COLON reversed_nonempty_llist_typevar_ DOT core_type RPAREN
simple_pattern_not_ident: LPAREN . pattern RPAREN
simple_pattern_not_ident: LPAREN . MODULE _*
simple_pattern_not_ident: LPAREN . pattern COLON core_type RPAREN
val_extra_ident: LPAREN . operator RPAREN
```
**Message** – After `(` the parser expects one of:
* `)` (empty tuple constructor)
* `::` (polymorphic variant constructor)
* a pattern (possibly with a type annotation)
* a module path (for a module pattern)
* an operator (for an extra identifier).

---

### 804
```
class_fun_binding: EQUAL . class_expr
```
**Message** – Expected a class expression after `=` in a class definition.

---

### 805
```
class_simple_expr: OBJECT . list_attribute_ class_self_pattern list_text_cstr_class_field__ END
```
**Message** – After `object` an optional attribute list may appear; the next token must be the self‑type (or `end` if there are no fields).

---

### 806
```
class_simple_expr: OBJECT list_attribute_ class_self_pattern . list_text_cstr_class_field__ END
```
**Message** – Expected a class field after the self‑type.

---

### 807
```
class_field: VAL . value list_post_item_attribute_
```
**Message** – After `val` the parser expects a value declaration (e.g., a field name and type).

---

### 808
```
value: BANG . list_attribute_ _*
```
**Message** – After `!` an optional attribute list may appear; the next token must start a value definition.

---

### 809
```
value: BANG list_attribute_ mutable_flag . LIDENT _*
```
**Message** – Expected the identifier of the mutable value after the attribute list.

---

### 810
```
value: BANG list_attribute_ mutable_flag LIDENT . EQUAL seq_expr
value: BANG list_attribute_ mutable_flag LIDENT . type_constraint EQUAL seq_expr
```
**Message** – Expected `=` (or a type constraint followed by `=`) after the mutable value name.

---

### 811
```
value: BANG list_attribute_ mutable_flag LIDENT EQUAL . seq_expr
```
**Message** – Expected the expression that defines the mutable value.

---

### 812
```
value: BANG list_attribute_ mutable_flag LIDENT COLON . possibly_poly_core_type_ EQUAL seq_expr
```
**Message** – Expected a type after `:`.

---

### 813
```
value: BANG list_attribute_ mutable_flag LIDENT COLON possibly_poly_core_type_ EQUAL . seq_expr
```
**Message** – Expected the defining expression after `=`.

---

### 814
```
value: list_attribute_ virtual_with_mutable_flag . LIDENT COLON core_type
```
**Message** – After the mutability/virtual flag list the parser expects a field name.

---

### 815
```
value: list_attribute_ virtual_with_mutable_flag LIDENT . COLON core_type
```
**Message** – Expected `:` after the field name.

---

### 816
```
value: list_attribute_ virtual_with_mutable_flag LIDENT COLON . core_type
```
**Message** – Expected a core type after `:`.

---

### 817
```
value: list_attribute_ mutable_flag . LIDENT _*
```
**Message** – Expected the identifier of a mutable value after the attribute list.

---

### 818
```
value: list_attribute_ mutable_flag LIDENT . EQUAL seq_expr
value: list_attribute_ mutable_flag LIDENT . type_constraint EQUAL seq_expr
```
**Message** – Expected `=` (or a type constraint then `=`) after the mutable identifier.

---

### 819
```
value: list_attribute_ mutable_flag LIDENT EQUAL . seq_expr
```
**Message** – Expected the expression defining the mutable value.

---

### 820
```
value: list_attribute_ mutable_flag LIDENT COLON . possibly_poly_core_type_ EQUAL seq_expr
```
**Message** – Expected a type after `:`.

---

### 821
```
value: list_attribute_ mutable_flag LIDENT COLON possibly_poly_core_type_ EQUAL . seq_expr
```
**Message** – Expected the defining expression after `=`.

---

### 822
```
class_field: METHOD . list_attribute_ private_virtual_flags LIDENT COLON possibly_poly_core_type_ list_post_item_attribute_
```
**Message** – After `method` an optional attribute list may appear; the next token must be the visibility flags (or the method name).

---

### 823
```
method_: BANG . list_attribute_ _*
```
**Message** – After `!` an optional attribute list may appear; the next token must start a method definition.

---

### 824
```
method_: BANG list_attribute_ private_flag . LIDENT _*
```
**Message** – Expected the method name after the attribute list.

---

### 825
```
method_: BANG list_attribute_ private_flag LIDENT . strict_binding
method_: BANG list_attribute_ private_flag LIDENT . COLON _*
```
**Message** – Expected either a binding (`=` …) or a type annotation (`:` …) after the method name.

---

### 826
```
method_: BANG list_attribute_ private_flag LIDENT COLON . possibly_poly_core_type_ EQUAL seq_expr
method_: BANG list_attribute_ private_flag LIDENT COLON . TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL seq_expr
```
**Message** – Expected a type (or a polymorphic type) after `:`.

---

### 827
```
method_: BANG list_attribute_ private_flag LIDENT COLON TYPE . nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL seq_expr
```
**Message** – Expected the identifier list after `type`.

---

### 828
```
method_: BANG list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ . DOT core_type EQUAL seq_expr
```
**Message** – Expected `.` before the core type.

---

### 829
```
method_: BANG list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT . core_type EQUAL seq_expr
```
**Message** – Expected the core type after the dot.

---

### 830
```
method_: BANG list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type . EQUAL seq_expr
```
**Message** – Expected `=` after the type.

---

### 831
```
method_: BANG list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL . seq_expr
```
**Message** – Expected the method body (expression) after `=`.

---

### 832
```
method_: BANG list_attribute_ private_flag LIDENT COLON possibly_poly_core_type_ . EQUAL seq_expr
```
**Message** – Expected `=` after the type annotation.

---

### 833
```
method_: BANG list_attribute_ private_flag LIDENT COLON possibly_poly_core_type_ EQUAL . seq_expr
```
**Message** – Expected the method body after `=`.

---

### 834
```
method_: list_attribute_ virtual_with_private_flag . LIDENT COLON possibly_poly_core_type_
```
**Message** – After the visibility flags an identifier (method name) is required.

---

### 835
```
method_: list_attribute_ virtual_with_private_flag LIDENT . COLON possibly_poly_core_type_
```
**Message** – Expected `:` after the method name.

---

### 836
```
method_: list_attribute_ virtual_with_private_flag LIDENT COLON . possibly_poly_core_type_
```
**Message** – Expected a type after `:`.

---

### 837
```
method_: list_attribute_ private_flag . LIDENT _*
```
**Message** – Expected the method name after the `private` flag.

---

### 838
```
method_: list_attribute_ private_flag LIDENT . strict_binding
method_: list_attribute_ private_flag LIDENT . COLON _*
```
**Message** – Expected either a binding (`=` …) or a type annotation (`:` …) after the method name.

---

### 839
```
method_: list_attribute_ private_flag LIDENT COLON . possibly_poly_core_type_ EQUAL seq_expr
method_: list_attribute_ private_flag LIDENT COLON . TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL seq_expr
```
**Message** – Expected a type (or a polymorphic type) after `:`.

---

### 840
```
method_: list_attribute_ private_flag LIDENT COLON TYPE . nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL seq_expr
```
**Message** – Expected the identifier list after `type`.

---

### 841
```
method_: list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ . DOT core_type EQUAL seq_expr
```
**Message** – Expected `.` before the core type.

---

### 842
```
method_: list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT . core_type EQUAL seq_expr
```
**Message** – Expected the core type after the dot.

---

### 843
```
method_: list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type . EQUAL seq_expr
```
**Message** – Expected `=` after the type.

---

### 844
```
method_: list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL . seq_expr
```
**Message** – Expected the method body (expression) after `=`.

---

### 845
```
method_: list_attribute_ private_flag LIDENT COLON possibly_poly_core_type_ . EQUAL seq_expr
```
**Message** – Expected `=` after the type annotation.

---

### 846
```
method_: list_attribute_ private_flag LIDENT COLON possibly_poly_core_type_ EQUAL . seq_expr
```
**Message** – Expected the method body after `=`.

---

### 847
```
class_field: CONSTRAINT . list_attribute_ constrain_field list_post_item_attribute_
```
**Message** – After `constraint` an optional attribute list may appear; the next token must start a type equality (`type = type`).

---

### 848
```
class_field: CONSTRAINT list_attribute_ . constrain_field list_post_item_attribute_
```
**Message** – Expected the left‑hand side of a type constraint.

---

### 849
```
constrain_field: core_type . EQUAL core_type
```
**Message** – Expected `=` after the left‑hand side type.
