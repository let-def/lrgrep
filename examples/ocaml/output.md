implementation': . implementation
Expected a structure (struct...end) or a module expression.

fun_expr: WHILE . ext list_attribute_ seq_expr DO seq_expr DONE
Expected an attribute or a boolean expression after `while`.

ext: PERCENT . attr_id
Expected an attribute name after `%`.

attr_id: single_attr_id DOT . attr_id
Expected another attribute name to complete this attribute identifier.

fun_expr: WHILE ext . list_attribute_ seq_expr DO seq_expr DONE
Expected an attribute or a boolean expression after the `while` condition.

attribute: LBRACKETAT . attr_id attr_payload RBRACKET
Expected an attribute name after `[@`.

attribute: LBRACKETAT attr_id . attr_payload RBRACKET
Expected the attribute payload or a closing bracket `]`.

value_description: VAL . ext list_attribute_ val_ident COLON possibly_poly_core_type_ list_post_item_attribute_
Expected an attribute, a value identifier, or `:=` after `val`.

value_description: VAL ext . list_attribute_ val_ident COLON possibly_poly_core_type_ list_post_item_attribute_
Expected an attribute or a value identifier after `val` and any attributes.

value_description: VAL ext list_attribute_ . val_ident COLON possibly_poly_core_type_ list_post_item_attribute_
Expected a value identifier after `val` and attributes.

val_extra_ident: LPAREN . operator RPAREN
Expected an operator (e.g., `::`, `::=`, `!`, `-`, `*`, `%`, `+`, `->`, etc.) after `(`.

operator: DOTOP . _*
Expected `..` to complete this operator after `.`.

operator: DOTOP LPAREN . _*
Expected `..` after `(` in an indexing expression.

index_mod: SEMI . DOTDOT
Expected `..` after `;` in an indexing expression.

operator: DOTOP LPAREN index_mod . _*
Expected `)` to close the indexing expression.

operator: DOTOP LBRACKET . _*
Expected an index expression after `.` `[`.

operator: DOTOP LBRACKET index_mod . _*
Expected `]` to close the indexing expression.

operator: DOTOP LBRACE . _*
Expected an index expression after `.` `{`.

operator: DOTOP LBRACE index_mod . _*
Expected `}` to close the indexing expression.

val_extra_ident: LPAREN operator . RPAREN
Expected `)` to close the operator expression.

value_description: VAL ext list_attribute_ val_ident . COLON possibly_poly_core_type_ list_post_item_attribute_
Expected `:` to specify the type of the value.

value_description: VAL ext list_attribute_ val_ident COLON . possibly_poly_core_type_ list_post_item_attribute_
Expected a type expression after `:` in the value declaration.

atomic_type: QUOTE . ident
Expected a type variable name (e.g., `'a`) after `'`.

reversed_nonempty_llist_typevar_: QUOTE . ident
Expected a type variable name (e.g., `'a`) after `'`.

optlabel: QUESTION . LIDENT COLON
Expected a label name after `?`.

optlabel: QUESTION LIDENT . COLON
Expected `:` to specify the type of the argument after `?label`.

atomic_type: LPAREN . _*
Expected a type expression (e.g., `int`, `{...}`, `module T : sig... end`, etc.) after `(`.

delimited_type_supporting_local_open: LPAREN . _*
Expected a type expression (e.g., `int`, `{...}`, `module T : sig... end`, etc.) after `(`.

function_type: LPAREN . reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER function_type
Expected a type expression (e.g., `int`, `{...}`, `module T : sig... end`, etc.) after `(`.

delimited_type_supporting_local_open: LPAREN MODULE . ext list_attribute_ module_type RPAREN
Expected an attribute or a module type name after `module` in a parenthesized type.

delimited_type_supporting_local_open: LPAREN MODULE ext . list_attribute_ module_type RPAREN
Expected an attribute or a module type name after `module` and any attributes.

delimited_type_supporting_local_open: LPAREN MODULE ext list_attribute_ . module_type RPAREN
Expected a module type name after `module` and its attributes.

module_type: SIG . list_attribute_ signature END
Expected a signature (items) or `end`.

module_type: SIG list_attribute_ . signature END
Expected a signature (items).

generic_type_declaration_no_nonrec_flag_type_subst_kind_: TYPE . _*
Expected `nonrec`, a type variable list, or a type name after `type`.

generic_type_declaration_nonrec_flag_type_kind_: TYPE . _*
Expected `nonrec`, a type variable list, or a type name after `type`.

signature_item: TYPE . _*
Expected `nonrec`, a type variable list, or a type name after `type`.

generic_type_declaration_no_nonrec_flag_type_subst_kind_: TYPE ext . _*
Expected an attribute or a type variable list after `type` and attributes.

generic_type_declaration_nonrec_flag_type_kind_: TYPE ext . _*
Expected an attribute or a type variable list after `type` and attributes.

signature_item: TYPE ext . _*
Expected an attribute or a type variable list after `type` and attributes.

generic_type_declaration_no_nonrec_flag_type_subst_kind_: TYPE ext list_attribute_ . _*
Expected a type variable list or `nonrec` after `type`, attributes, and the `NONREC` keyword.

generic_type_declaration_nonrec_flag_type_kind_: TYPE ext list_attribute_ . _*
Expected a type variable list or `nonrec` after `type`, attributes, and the `NONREC` keyword.

signature_item: TYPE ext list_attribute_ . _*
Expected a type variable list or `nonrec` after `type`, attributes, and the `NONREC` keyword.

generic_type_declaration_no_nonrec_flag_type_subst_kind_: TYPE ext list_attribute_ NONREC . type_parameters LIDENT COLONEQUAL nonempty_type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_
Expected a type variable list after `nonrec`.

generic_type_declaration_nonrec_flag_type_kind_: TYPE ext list_attribute_ NONREC . type_parameters LIDENT type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_
Expected a type variable list after `nonrec`.

signature_item: TYPE ext list_attribute_ NONREC . type_parameters type_longident PLUSEQ private_flag reversed_bar_llist_extension_constructor_declaration_ list_post_item_attribute_
Expected a type variable list after `nonrec`.

type_parameters: LPAREN . reversed_separated_nonempty_llist_COMMA_type_parameter_ RPAREN
Expected a type variable (e.g., `'a`, `+ 'b`) after `(`.

type_parameter: type_variance . type_variable
Expected a type variable name (e.g., `'a` or `_`) after the variance modifier.

type_variable: QUOTE . ident
Expected a type variable name (e.g., `'a`) after `'`.

type_parameters: LPAREN reversed_separated_nonempty_llist_COMMA_type_parameter_ . RPAREN
Expected `)` to close the type variable list.

reversed_separated_nonempty_llist_COMMA_type_parameter_: reversed_separated_nonempty_llist_COMMA_type_parameter_ COMMA . type_parameter
Expected another type variable (e.g., `+, 'a` or `-, 'b`) after `.`.

generic_type_declaration_no_nonrec_flag_type_subst_kind_: TYPE ext list_attribute_ NONREC type_parameters . LIDENT COLONEQUAL nonempty_type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_
Expected a type name after the type parameters.

generic_type_declaration_nonrec_flag_type_kind_: TYPE ext list_attribute_ NONREC type_parameters . LIDENT type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_
Expected a type name after the type parameters.

signature_item: TYPE ext list_attribute_ NONREC type_parameters . type_longident PLUSEQ private_flag reversed_bar_llist_extension_constructor_declaration_ list_post_item_attribute_
Expected a type name after the type parameters.

type_kind: EQUAL . nonempty_type_kind
Expected a type definition after `=` in a type declaration.

atomic_type: QUOTE . ident
Expected a type variable name (e.g., `'a`) after `'`.

nonempty_type_kind: PRIVATE . _*
Expected a type expression after `private`.

atomic_type: LPAREN . _*
Expected a type expression (e.g., `int`, `{...}`, `module T : sig... end`, etc.) after `(`.

constr_extra_nonprefix_ident: LPAREN . RPAREN
Expected `)` to close the constructor.

delimited_type_supporting_local_open: LPAREN . _*
Expected a type expression (e.g., `int`, `{...}`, `module T : sig... end`, etc.) after `(`.

function_type: LPAREN . reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER function_type
Expected a type expression (e.g., `int`, `{...}`, `module T : sig... end`, etc.) after `(`.

function_type: LIDENT COLON . _*
Expected a type expression after `variable :` in a function type.

atomic_type: LPAREN . _*
Expected a type expression (e.g., `int`, `{...}`, `module T : sig... end`, etc.) after `(`.

delimited_type_supporting_local_open: LPAREN . _*
Expected a type expression (e.g., `int`, `{...}`, `module T : sig... end`, etc.) after `(`.

function_type: LIDENT COLON LPAREN . reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER function_type
Expected a type variable (e.g., `+ 'a`) or a type expression after `:` in a function type.

object_type: LESS . _*
Expected a method list (e.g., `method x : int`) or `>` to close the object type.

atomic_type: LPAREN . _*
Expected a type expression (e.g., `int`, `{...}`, `module T : sig... end`, etc.) after `(`.

delimited_type_supporting_local_open: LPAREN . _*
Expected a type expression (e.g., `int`, `{...}`, `module T : sig... end`, etc.) after `(`.

/extension: LBRACKETPERCENT . attr_id payload RBRACKET
use_file: LBRACKETPERCENT
Expected an attribute identifier after `[%`.

/extension: LBRACKETPERCENT attr_id . payload RBRACKET
use_file: LBRACKETPERCENT AND
Expected an attribute payload after attribute identifier in `[%...]`.

/generic_type_declaration_nonrec_flag_type_kind_: TYPE . _*
local_structure_item: TYPE . _*
use_file: TYPE
Expected a type name after `TYPE`.

/generic_type_declaration_nonrec_flag_type_kind_: TYPE ext . _*
local_structure_item: TYPE ext . _*
use_file: TYPE PERCENT AND
Expected `NONREC` or a type name after `TYPE %...`.

/generic_type_declaration_nonrec_flag_type_kind_: TYPE ext list_attribute_ . _*
local_structure_item: TYPE ext list_attribute_ . _*
use_file: TYPE ext [@@] AND
Expected `NONREC` after attribute in `TYPE` declaration.

/generic_type_declaration_nonrec_flag_type_kind_: TYPE ext list_attribute_ NONREC . _*
local_structure_item: TYPE ext list_attribute_ NONREC . _*
use_file: TYPE ext [@@] NONREC
Expected a type name or type parameter after `NONREC` in `TYPE` declaration.

/generic_type_declaration_nonrec_flag_type_kind_: TYPE ext list_attribute_ NONREC type_parameters . _*
local_structure_item: TYPE ext list_attribute_ NONREC type_parameters . _*
use_file: TYPE ext [@@] NONREC LPAREN ... RPAREN
Expected a type name after `NONREC` and type parameters in `TYPE` declaration.

/post_item_attribute: LBRACKETATAT . attr_id attr_payload RBRACKET
implementation: seq_expr LBRACKETATAT
Expected an attribute identifier after `[@@`.

/post_item_attribute: LBRACKETATAT attr_id . attr_payload RBRACKET
implementation: seq_expr LBRACKETATAT AND
Expected an attribute payload after attribute identifier.

/fun_expr: TRY . ext list_attribute_ seq_expr WITH ...
use_file: TRY
Expected an attribute or keyword after `try`.

/fun_expr: TRY ext . _*
use_file: TRY PERCENT AND
Expected a sequence expression after `try`.

/fun_expr: TRY ext list_attribute_ . _*
use_file: TRY ext [@@] AND
Expected a sequence expression after `try`.

/reversed_labeled_tuple_body: TILDE . _*
use_file: TILDE
Expected a label identifier or pattern after `~`.

/reversed_labeled_tuple_body: TILDE LPAREN . _*
use_file: TILDE LPAREN
Expected a label identifier after `~ (`.

/reversed_labeled_tuple_body: TILDE LPAREN LIDENT . _*
use_file: TILDE LPAREN LIDENT
Expected `:` or `=` after `~ ( label`.

/type_constraint: COLONGREATER . core_type
use_file: LPAREN ... COLONGREATER
Expected a core type after `:>` in type constraint.

/delimited_type_supporting_local_open: LBRACKETLESS . _*
parse_core_type: LBRACKETLESS
Expected a row field or `>` after `<`.

/delimited_type_supporting_local_open: LBRACKETLESS BAR . _*
parse_core_type: LBRACKETLESS BAR
Expected a row field after `< |`.

/delimited_type_supporting_local_open: LBRACKETGREATER . _*
parse_core_type: LBRACKETGREATER
Expected a row field or `>` after `>`.

/delimited_type_supporting_local_open: LBRACKETGREATER BAR . _*
parse_core_type: LBRACKETGREATER BAR
Expected a row field after `> |`.

/delimited_type_supporting_local_open: LBRACKET . _*
parse_core_type: LBRACKET
Expected a core type, tag or row field after `[`.

/atomic_type: HASH . clty_longident
parse_core_type: HASH
Expected a class type after `#`.

/mk_longident_mod_ext_longident_LIDENT_: mod_ext_longident . DOT LIDENT
use_file: TYPE ... type_parameters mod_ext_longident LPAREN ... RPAREN
Expected `.` after module identifier in type declaration.

/mod_ext_longident: mod_ext_longident LPAREN . mod_ext_longident RPAREN
parse_any_longident: ... LPAREN
Expected a module identifier after `(` in module application.

/mod_ext_longident: mod_ext_longident LPAREN mod_ext_longident . RPAREN
parse_any_longident: ... LPAREN ... RPAREN
Expected `)` after module name in application.

/mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident DOT . UIDENT
mod_ext_longident: mod_ext_longident LPAREN mod_ext_longident DOT . RPAREN
Expected an identifier or `)` after `.` in module path.

/mk_longident_mod_ext_longident_LIDENT_: mod_ext_longident DOT . LIDENT
mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident DOT . UIDENT
Expected an identifier after `.` in module path.

/delimited_type_supporting_local_open: LBRACKET BAR . _*
parse_core_type: LBRACKET BAR
Expected a row field after `|` in row type.

/name_tag: BACKQUOTE . ident
use_file: BACKQUOTE
Expected an identifier after `` ` ``.

/function_type: tuple_type MINUSGREATER . function_type
parse_core_type: tuple_type MINUSGREATER
Expected a function type after `->`.

/function_type: optlabel . _*
use_file: ... OPTLABEL
Expected `?` or `?:` after `->`.

/atomic_type: LPAREN . _*
delimited_type_supporting_local_open: LPAREN . _*
function_type: optlabel LPAREN . _*
parse_core_type: optlabel LPAREN
Expected a type variable or `.` after `?(`.

/atomic_type: LPAREN reversed_separated_nontrivial_llist_COMMA_core_type_ . _*
delimited_type_supporting_local_open: LPAREN reversed_separated_nontrivial_llist_COMMA_core_type_ . _*
parse_core_type: LPAREN ... RPAREN (with attributes)
Expected a core type after `,` in tuple type.

/atomic_type: LPAREN reversed_separated_nontrivial_llist_COMMA_core_type_ RPAREN . _*
parse_core_type: LPAREN ... RPAREN
Expected a core type after `(...)`.

/atomic_type: LPAREN reversed_separated_nontrivial_llist_COMMA_core_type_ RPAREN HASH . _*
parse_core_type: LPAREN ... RPAREN HASH
Expected a class type after `#` in type.

/reversed_separated_nontrivial_llist_COMMA_core_type_: ... COMMA . _*
parse_core_type: ... COMMA
Expected a core type after `,`.

/atomic_type: mod_ext_longident . _*
use_file: TYPE ... type_parameters mod_ext_longident ...
Expected a type name or `)` after `(` in type.

/atomic_type: mod_ext_longident DOT . _*
parse_core_type: mod_ext_longident DOT
Expected an identifier or `)` after module path.

/delimited_type_supporting_local_open: LPAREN . _*
parse_core_type: DOT LPAREN
Expected a core type after `.` in object type.

/delimited_type_supporting_local_open: LPAREN core_type . _*
parse_core_type: DOT LPAREN core_type
Expected a core type after `(` in object type.

/tuple_type: atomic_type STAR . _*
parse_core_type: atomic_type STAR
Expected a core type after `*`.

/reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_: LIDENT COLON . _*
parse_core_type: ... STAR LIDENT COLON
Expected a core type after `:` in labeled tuple type.

/atomic_type: atomic_type HASH . _*
parse_core_type: atomic_type HASH
Expected a class type after `#`.

/reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_: ... STAR . _*
parse_core_type: ... STAR
Expected a core type after `*`.

/reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_: ... STAR LIDENT COLON . _*
parse_core_type: ... STAR LIDENT COLON
Expected a core type after `:` in labeled tuple type.

/alias_type: alias_type AS . _*
parse_core_type: ... AS
Expected a type variable after `as`.

/alias_type: alias_type AS QUOTE . _*
parse_core_type: ... AS QUOTE
Expected an identifier after `` ' ``.

/function_type: optlabel LPAREN reversed_nonempty_llist_typevar_ . DOT core_type RPAREN ...
parse_core_type: optlabel LPAREN QUOTE LIDENT
Expected `.` or a core type after type variables.

/reversed_nonempty_llist_typevar_: ... QUOTE . _*
parse_core_type: ... QUOTE
Expected an identifier after `` ' ``.

/function_type: optlabel LPAREN reversed_nonempty_llist_typevar_ DOT . core_type RPAREN ...
parse_core_type: optlabel LPAREN QUOTE LIDENT DOT
Expected a core type after `.` in type parameters.

/function_type: optlabel LPAREN reversed_nonempty_llist_typevar_ DOT core_type LBRACKETAT attr_id attr_payload RBRACKET . RPAREN MINUSGREATER function_type
Expected a closing parenthesis ')' in the type constraint for label 'label'.

/function_type: optlabel LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN . MINUSGREATER function_type
Expected `->` (arrow) to continue the function type.

/function_type: optlabel LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER . function_type
Expected a type after `->` (arrow).

/delimited_type_supporting_local_open: LPAREN core_type LBRACKETAT attr_id attr_payload RBRACKET . RPAREN
reversed_separated_nontrivial_llist_COMMA_core_type_: core_type LBRACKETAT attr_id attr_payload RBRACKET . COMMA core_type
Expected a closing parenthesis ')' or a comma ',' to complete the type.

/reversed_separated_nontrivial_llist_COMMA_core_type_: LPAREN core_type COMMA LBRACKETAT attr_id attr_payload RBRACKET . COMMA core_type
Expected a type after comma ',' in the tuple.

/function_type: optlabel UNDERSCORE . MINUSGREATER function_type
Expected `->` (arrow) to complete the function type.

/function_type: optlabel UNDERSCORE MINUSGREATER . function_type
Expected a type after `->` (arrow).

/delimited_type_supporting_local_open: LBRACKET BAR core_type LBRACKETAT attr_id attr_payload RBRACKET . RBRACKET
Expected a closing bracket `]` to finish the variant or record declaration.

/reversed_separated_nonempty_llist_BAR_row_field_: LBRACKET BAR core_type LBRACKETAT attr_id attr_payload RBRACKET BAR . row_field
Expected a variant tag or a closing bracket `]` after `|`.

/tag_field: LBRACKET name_tag OF core_type LBRACKETAT attr_id attr_payload RBRACKET . opt_ampersand reversed_separated_nonempty_llist_AMPERSAND_core_type_no_attr_ list_attribute_
Expected a type to specify the field `x`.

/tag_field: LBRACKET name_tag OF opt_ampersand core_type LBRACKETAT attr_id attr_payload RBRACKET . reversed_separated_nonempty_llist_AMPERSAND_core_type_no_attr_ list_attribute_
Expected a type to specify the field `x`.

/reversed_separated_nonempty_llist_AMPERSAND_core_type_no_attr_: LBRACKET name_tag OF opt_ampersand core_type LBRACKETAT attr_id attr_payload RBRACKET AMPERSAND . alias_type
Expected a type after `&` to specify the next field.

/delimited_type_supporting_local_open: LBRACKET core_type LBRACKETAT attr_id attr_payload RBRACKET . BAR reversed_separated_nonempty_llist_BAR_row_field_ RBRACKET
Expected `|` to separate variant tags or `]` to finish the declaration.

/delimited_type_supporting_local_open: LBRACKET core_type LBRACKETAT attr_id attr_payload RBRACKET BAR . reversed_separated_nonempty_llist_BAR_row_field_ RBRACKET
Expected `|` to separate more variant tags or `]` to finish the declaration.

/delimited_type_supporting_local_open: LBRACKETGREATER option_BAR_ reversed_separated_nonempty_llist_BAR_row_field_ BAR core_type LBRACKETAT attr_id attr_payload RBRACKET . RBRACKET
Expected a closing bracket `]` to finish the variant/record declaration.

/delimited_type_supporting_local_open: LBRACKETLESS option_BAR_ reversed_separated_nonempty_llist_BAR_row_field_ BAR core_type LBRACKETAT attr_id attr_payload RBRACKET . _*
Expected `>` to close the bounds or `]` to finish the type declaration.

/delimited_type_supporting_local_open: LBRACKETLESS option_BAR_ reversed_separated_nonempty_llist_BAR_row_field_ GREATER . reversed_nonempty_llist_name_tag_ RBRACKET
Expected `>` or `]` to finish the bounds or `]` to finish the type declaration.

/delimited_type_supporting_local_open: LBRACKETLESS option_BAR_ reversed_separated_nonempty_llist_BAR_row_field_ GREATER reversed_nonempty_llist_name_tag_ BACKQUOTE LIDENT . RBRACKET
Expected `]` to finish the bounds or `]` to finish the type declaration.

/type_constraint: LPAREN seq_expr COLON core_type LBRACKETAT attr_id attr_payload RBRACKET . _*
Expected a type to complete the type constraint.

/type_constraint: LPAREN seq_expr COLON core_type COLONGREATER core_type LBRACKETAT attr_id attr_payload RBRACKET . _*
Expected a type after `:>`.

/reversed_labeled_tuple_body: TILDE LPAREN LIDENT COLON core_type COLONGREATER core_type LBRACKETAT attr_id attr_payload RBRACKET . _*
Expected a closing parenthesis ')' or a comma ',' to finish the label argument.

/reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint RPAREN . _*
Expected a comma ',' or another labeled argument to finish the argument list.

/reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint RPAREN COMMA . _*
Expected a comma ',' or another labeled argument to continue the argument list.

/reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint RPAREN COMMA TILDE . LIDENT
Expected a label name or a pattern in parenthesis after `~`.

/reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint RPAREN COMMA TILDE LPAREN . LIDENT type_constraint RPAREN
Expected a label name or a pattern in parenthesis after `~`.

/reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint RPAREN COMMA TILDE LPAREN LIDENT . type_constraint RPAREN
Expected a label name or `:` and a type in parenthesis after `~`.

/reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint RPAREN COMMA TILDE LPAREN LIDENT type_constraint . RPAREN
Expected a closing parenthesis ')' or a comma ','.

/reversed_labeled_tuple_body: TILDE LIDENT COMMA . _*
Expected a comma ',' or another labeled argument.

/reversed_labeled_tuple_body: TILDE LIDENT COMMA TILDE . LIDENT
Expected a label name or a pattern in parenthesis after `~`.

/reversed_labeled_tuple_body: TILDE LIDENT COMMA TILDE LPAREN . LIDENT type_constraint RPAREN
Expected a label name or a pattern in parenthesis after `~`.

/reversed_labeled_tuple_body: TILDE LIDENT COMMA TILDE LPAREN LIDENT . type_constraint RPAREN
Expected a label name or `:` and a type in parenthesis after `~`.

/reversed_labeled_tuple_body: TILDE LIDENT COMMA TILDE LPAREN LIDENT type_constraint . RPAREN
Expected a closing parenthesis ')' or a comma ','.

/simple_expr: PREFIXOP . simple_expr
Expected an operand after prefix operator.

/simple_expr: OBJECT . ext list_attribute_ class_self_pattern list_text_cstr_class_field__ END
Expected an attribute or the self pattern after `object`.

/simple_expr: OBJECT PERCENT . ext list_attribute_ class_self_pattern list_text_cstr_class_field__ END
Expected an attribute or the self pattern after `object`.

/simple_expr: OBJECT ext LBRACKETAT attr_id attr_payload RBRACKET . class_self_pattern list_text_cstr_class_field__ END
Expected the self pattern or content after the object attributes.

/simple_expr: OBJECT ext list_attribute_ LPAREN . _*
Expected a self pattern in parenthesis or the object content after `object`.

/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type LBRACKETAT attr_id attr_payload RBRACKET . _*
Expected a closing parenthesis ')' or a comma ',' in the labeled argument.

/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT . _*
Expected `:` and a type or `)` in parenthesis after `~`.

/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON . _*
Expected `:` and a type or `)` in parenthesis after `~`.

/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type . _*
Expected `)` or `]` to finish the labeled argument.

/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN . _*
Expected a comma ',' or another labeled argument.

/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA . _*
Expected a comma ',' or another labeled argument.

/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE . LIDENT
Expected a label name or a pattern in parenthesis after `~`.

/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN . LIDENT COLON core_type RPAREN
Expected a label name or a pattern in parenthesis after `~`.

/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN LIDENT . COLON core_type RPAREN
Expected `:` and a type or `)` in parenthesis after `~`.

/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN LIDENT COLON . core_type RPAREN
Expected `:` and a type or `)` in parenthesis after `~`.

/labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN
Expected a closing parenthesis ')' or a comma ','.

labeled_tuple_pat_element_list_pattern_: TILDE LIDENT COMMA . _*
Expected a pattern or label after `,`.

labeled_tuple_pat_element_list_pattern_: TILDE . _*
Expected a label identifier after `~`.

labeled_tuple_pat_element_list_pattern_: TILDE LPAREN . LIDENT COLON core_type RPAREN
Expected `label: type` after `~ (`.

labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT . COLON core_type RPAREN
Expected `:` after label name.

labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON . core_type RPAREN
Expected a type after `:`.

labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN
Expected `)` after type.

signed_constant: PLUS . _*
Expected a constant (int, float, char, string) after `+`.

signed_constant: MINUS . _*
Expected a constant (int, float, char, string) after `-`.

constr_extra_nonprefix_ident: LPAREN . RPAREN
Expected `)` after `(`.

simple_pattern_not_ident: LPAREN MODULE . _*
Expected a module name or type after `module`.

simple_pattern_not_ident: LPAREN MODULE ext . _*
Expected a module name or type after `module %attr`.

simple_pattern_not_ident: LPAREN MODULE ext list_attribute_ . _*
Expected a module name or type after `module %attr`.

simple_pattern_not_ident: LPAREN MODULE ext list_attribute_ module_name . _*
Expected a module type or `)` after module name.

simple_pattern_not_ident: LPAREN MODULE ext list_attribute_ module_name COLON . module_type RPAREN
Expected a module type after `:`.

module_type: MODULE . TYPE OF list_attribute_ module_expr
Expected `TYPE` after `module`.

module_type: MODULE TYPE . OF list_attribute_ module_expr
Expected `of` after `module type`.

module_type: MODULE TYPE OF . list_attribute_ module_expr
Expected a module expression after `of`.

module_type: MODULE TYPE OF list_attribute_ . module_expr
Expected a module expression.

module_expr: STRUCT . list_attribute_ structure END
Expected `struct` content or `end`.

module_expr: STRUCT list_attribute_ . structure END
Expected `struct` content or `end`.

open_declaration: OPEN . _*
Expected `!` and a module name after `open`.

open_declaration: OPEN BANG . ext list_attribute_ module_expr list_post_item_attribute_
Expected a module name after `open !`.

open_declaration: OPEN BANG ext . list_attribute_ module_expr list_post_item_attribute_
Expected a module name after `open ! %attr`.

open_declaration: OPEN BANG ext list_attribute_ . module_expr list_post_item_attribute_
Expected a module name.

paren_module_expr: LPAREN . _*
Expected a module expression after `(`.

paren_module_expr: LPAREN VAL . list_attribute_ expr_colon_package_type RPAREN
Expected a value name or type after `val`.

paren_module_expr: LPAREN VAL list_attribute_ . expr_colon_package_type RPAREN
Expected a value name or type after `val`.

simple_expr: NEW . ext list_attribute_ class_longident
Expected a class name after `new`.

simple_expr: NEW ext . list_attribute_ class_longident
Expected a class name after `new %attr`.

simple_expr: NEW ext list_attribute_ . class_longident
Expected a class name.

mk_longident_mod_longident_LIDENT_: mod_longident . DOT LIDENT
Expected a module name after `.`.

mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT
Expected a module name after `.`.

simple_expr: METAOCAML_ESCAPE . simple_expr
Expected an expression after escape sequence.

simple_expr: METAOCAML_BRACKET_OPEN . seq_expr METAOCAML_BRACKET_CLOSE
Expected an expression inside brackets.

fun_expr: MATCH . ext list_attribute_ seq_expr WITH reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expected an expression after `match`.

fun_expr: MATCH ext . list_attribute_ seq_expr WITH reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expected an expression after `match %attr`.

fun_expr: MATCH ext list_attribute_ . seq_expr WITH reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expected an expression.

constr_extra_nonprefix_ident: LPAREN . RPAREN
Expected `)` after `(`.

simple_expr: LBRACKETBAR . _*
Expected a function expression after `[|`.

fun_expr: LIDENT LESSMINUS . _*
Expected a value expression after `<-`.

fun_expr: LETOP . letop_bindings IN seq_expr
Expected let bindings after `let*`.

labeled_tuple_pat_element_list_pattern_no_exn_: TILDE . _*
Expected a label after `~`.

labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN . _*
Expected `label: type` after `~ (`.

labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT . _*
Expected `:` after label name.

labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON . _*
Expected a type after `:`.

labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . _*
Expected `)` after type.

labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type RPAREN . _*
Expected `,` or another labeled element.

labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA . _*
Expected a labeled element after `,`.

labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE . LIDENT
Expected a label after `~`.

labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN . LIDENT
Expected `label: type` after `,`.

## Pattern 200

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT . _*
```

Expected `:` after label name in tuple pattern.

## Pattern 201

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON . _*
```

Expected a type after `:` in tuple pattern.

## Pattern 202

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . _*
```

Expected `)` to end label pattern.

## Pattern 203

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LIDENT COMMA . _*
```

Expected another tuple label.

## Pattern 204

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE . _*
```

Expected identifier for tilde-labeled tuple element.

## Pattern 205

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LIDENT COMMA TILDE LPAREN . LIDENT COLON core_type RPAREN
```

Expected identifier for tilde-labeled parenthesized pattern.

## Pattern 206

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LIDENT COMMA TILDE LPAREN LIDENT . COLON core_type RPAREN
```

Expected `:` after label name in tuple pattern.

## Pattern 207

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LIDENT COMMA TILDE LPAREN LIDENT COLON . core_type RPAREN
```

Expected a type after `:` in tuple pattern.

## Pattern 208

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LIDENT COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN
```

Expected `)` to end label pattern.

## Pattern 209

```
simple_delimited_pattern: LBRACKETBAR . _*
```

Expected a pattern inside `[]|`.

## Pattern 210

```
constr_extra_nonprefix_ident: LBRACKET . RBRACKET
simple_delimited_pattern: LBRACKET . separated_or_terminated_nonempty_list_SEMI_pattern_ RBRACKET
```

Expected a record or tuple pattern inside `[]`.

## Pattern 211

```
simple_delimited_pattern: LBRACE . listx_SEMI_record_pat_field_UNDERSCORE_ RBRACE
```

Expected a field name, `=` or `:` inside record pattern.

## Pattern 212

```
simple_delimited_pattern: LBRACE listx_SEMI_record_pat_field_UNDERSCORE_ . RBRACE
```

Expected `}` to end record pattern.

## Pattern 213

```
option_preceded_COLON_core_type__: COLON . core_type
```

Expected a type for the field.

## Pattern 214

```
option_preceded_EQUAL_pattern__: EQUAL . pattern
```

Expected a pattern for the field.

## Pattern 215

```
pattern_gen: LAZY . ext list_attribute_ simple_pattern
```

Expected a pattern after `lazy`.

## Pattern 216

```
pattern_gen: LAZY ext . list_attribute_ simple_pattern
```

Expected an attribute and a pattern after `lazy %`.

## Pattern 217

```
pattern_gen: LAZY ext list_attribute_ . simple_pattern
```

Expected an attribute identifier.

## Pattern 218

```
simple_pattern_not_ident: HASH . type_longident
```

Expected a type after `#`.

## Pattern 219

```
simple_pattern_not_ident: signed_constant DOTDOT . signed_constant
```

Expected a second constant after `..`.

## Pattern 220

```
constr_longident: mod_longident DOT . LPAREN COLONCOLON RPAREN
mk_longident_mod_longident_UIDENT_: mod_longident DOT . UIDENT
simple_pattern_not_ident: mod_longident DOT . _*
```

Expected a constructor name after `.`.

## Pattern 221

```
constr_longident: mod_longident DOT LPAREN . COLONCOLON RPAREN
simple_pattern_not_ident: mod_longident DOT LPAREN . _*
```

Expected `::` inside a constructor name.

## Pattern 222

```
labeled_tuple_pat_element_list_pattern_: LABEL . _*
```

Expected a pattern name or `..` after `~`.

## Pattern 223

```
labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern . _*
```

Expected a comma or `..` to separate tuple labels.

## Pattern 224

```
labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA . _*
```

Expected a tuple label.

## Pattern 225

```
labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE . _*
labeled_tuple_pat_element_list_pattern_: TILDE . _*
```

Expected identifier for tilde-labeled tuple element.

## Pattern 226

```
labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN . LIDENT COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN . _*
```

Expected identifier for tilde-labeled parenthesized pattern.

## Pattern 227

```
labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT . COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT . _*
```

Expected `:` after label name in tuple pattern.

## Pattern 228

```
labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON . core_type RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON . _*
```

Expected a type after `:` in tuple pattern.

## Pattern 229

```
labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type . _*
```

Expected `)` to end label pattern.

## Pattern 230

```
labeled_tuple_pat_element_list_pattern_: LABEL . _*
labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA LABEL . simple_pattern
labeled_tuple_pat_element_list_pattern_: LABEL . _*
```

Expected a pattern name or `..` after `~`.

## Pattern 231

```
pattern: EXCEPTION . ext list_attribute_ pattern
```

Expected a constructor name after `exception`.

## Pattern 232

```
pattern: EXCEPTION ext . list_attribute_ pattern
```

Expected an attribute and a pattern after `exception %`.

## Pattern 233

```
pattern: EXCEPTION ext list_attribute_ . pattern
```

Expected an attribute identifier.

## Pattern 234

```
pattern: EFFECT . pattern_gen COMMA simple_pattern
```

Expected a pattern and a comma after `effect`.

## Pattern 235

```
pattern: EFFECT pattern_gen . COMMA simple_pattern
```

Expected a comma after the effect constructor.

## Pattern 236

```
pattern: EFFECT pattern_gen COMMA . simple_pattern
```

Expected a pattern for the effect side.

## Pattern 237

```
labeled_tuple_pat_element_list_pattern_: pattern COMMA . _*
```

Expected a tuple label.

## Pattern 238

```
labeled_tuple_pat_element_list_pattern_: pattern COMMA TILDE . _*
labeled_tuple_pat_element_list_pattern_: TILDE . _*
```

Expected identifier for tilde-labeled tuple element.

## Pattern 239

```
labeled_tuple_pat_element_list_pattern_: pattern COMMA TILDE LPAREN . LIDENT COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN . _*
```

Expected identifier for tilde-labeled parenthesized pattern.

## Pattern 240

```
labeled_tuple_pat_element_list_pattern_: pattern COMMA TILDE LPAREN LIDENT . COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT . _*
```

Expected `:` after label name in tuple pattern.

## Pattern 241

```
labeled_tuple_pat_element_list_pattern_: pattern COMMA TILDE LPAREN LIDENT COLON . core_type RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON . _*
```

Expected a type after `:` in tuple pattern.

## Pattern 242

```
labeled_tuple_pat_element_list_pattern_: pattern COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type . _*
```

Expected `)` to end label pattern.

## Pattern 243

```
labeled_tuple_pat_element_list_pattern_: pattern COMMA LABEL . simple_pattern
labeled_tuple_pat_element_list_pattern_: LABEL . _*
```

Expected a pattern name after `~label:`.

## Pattern 244

```
pattern: pattern COLONCOLON . pattern
```

Expected a pattern after `::`.

## Pattern 245

```
pattern: pattern BAR . pattern
```

Expected a pattern after `|`.

## Pattern 246

```
pattern: pattern AS . val_ident
```

Expected a variable name after `as`.

## Pattern 247

```
labeled_tuple_pat_element_list_pattern_: labeled_tuple_pat_element_list_pattern_ COMMA . _*
```

Expected a tuple label.

## Pattern 248

```
labeled_tuple_pat_element_list_pattern_: labeled_tuple_pat_element_list_pattern_ COMMA TILDE . _*
labeled_tuple_pat_element_list_pattern_: TILDE . _*
```

Expected identifier for tilde-labeled tuple element.

## Pattern 249

```
labeled_tuple_pat_element_list_pattern_: labeled_tuple_pat_element_list_pattern_ COMMA TILDE LPAREN . LIDENT COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN . _*
```

Expected identifier for tilde-labeled parenthesized pattern.

Pattern 250: labeled_tuple_pat_element_list_pattern_: labeled_tuple_pat_element_list_pattern_ COMMA TILDE LPAREN LIDENT . COLON core_type RPAREN labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT . _* reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT
Expected `:` after variable in pattern `~(`.

Pattern 251: labeled_tuple_pat_element_list_pattern_: labeled_tuple_pat_element_list_pattern_ COMMA TILDE LPAREN LIDENT COLON . core_type RPAREN labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON . _* reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON . core_type RPAREN COMMA DOTDOT
Expected a type after `:` in pattern.

Pattern 252: labeled_tuple_pat_element_list_pattern_: labeled_tuple_pat_element_list_pattern_ COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type . _* reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT
Expected `)` after pattern type in `~(`.

Pattern 253: labeled_tuple_pat_element_list_pattern_: labeled_tuple_pat_element_list_pattern_ COMMA LABEL . simple_pattern labeled_tuple_pat_element_list_pattern_: LABEL . _* reversed_labeled_tuple_pattern_pattern_: LABEL . simple_pattern COMMA DOTDOT
Expected a value after `:` in pattern.

Pattern 254: constr_extra_nonprefix_ident: LPAREN . RPAREN constr_longident: LPAREN . COLONCOLON RPAREN pattern_gen: constr_longident LPAREN . TYPE nonempty_list_mkrhs_LIDENT__ RPAREN simple_pattern_not_ident: LPAREN . _* val_extra_ident: LPAREN . operator RPAREN
Expected `)` or `::` after `(`.

Pattern 255: pattern_gen: constr_longident LPAREN TYPE . nonempty_list_mkrhs_LIDENT__ RPAREN simple_pattern
Expected a type constructor after `TYPE`.

Pattern 256: pattern_gen: constr_longident LPAREN TYPE nonempty_list_mkrhs_LIDENT__ . RPAREN simple_pattern
Expected `)` after type constructor name.

Pattern 257: pattern_gen: constr_longident LPAREN TYPE nonempty_list_mkrhs_LIDENT__ RPAREN . simple_pattern
Expected a pattern after `)`.

Pattern 258: constr_longident: LPAREN COLONCOLON . RPAREN
Expected `)` after `::`.

Pattern 259: labeled_tuple_pat_element_list_pattern_: pattern . _* reversed_labeled_tuple_pattern_pattern_: pattern . COMMA DOTDOT simple_pattern_not_ident: LPAREN pattern . _*
Expected `)` after `as` in pattern.

Pattern 260: simple_pattern_not_ident: LPAREN pattern . _*
Expected a type after `:` in pattern.

Pattern 261: simple_pattern_not_ident: LPAREN pattern COLON core_type . RPAREN
Expected `)` after pattern.

Pattern 262: constr_longident: mod_longident DOT LPAREN COLONCOLON . RPAREN
Expected `)` after `::`.

Pattern 263: labeled_tuple_pat_element_list_pattern_: pattern . _* reversed_labeled_tuple_pattern_pattern_: pattern . COMMA DOTDOT simple_pattern_not_ident: mod_longident DOT LPAREN pattern . RPAREN
Expected `)` after `as` in pattern.

Pattern 264: simple_delimited_pattern: LBRACKET . separated_or_terminated_nonempty_list_SEMI_pattern_ RBRACKET simple_pattern_not_ident: mod_longident DOT LBRACKET . RBRACKET
Expected `]` after `[` in pattern.

Pattern 265: simple_delimited_pattern: LBRACKET separated_or_terminated_nonempty_list_SEMI_pattern_ . RBRACKET
Expected `]` after `;` in pattern.

Pattern 266: simple_delimited_pattern: LBRACKETBAR separated_or_terminated_nonempty_list_SEMI_pattern_ . BARRBRACKET
Expected `|]` after `;` in pattern.

Pattern 267: labeled_tuple_pat_element_list_pattern_no_exn_: LABEL . _* labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LIDENT COMMA LABEL . simple_pattern reversed_labeled_tuple_pattern_pattern_no_exn_: LABEL . simple_pattern COMMA DOTDOT
Expected a value after `:` in pattern.

Pattern 268: labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA . _* reversed_labeled_tuple_pattern_pattern_no_exn_: LABEL simple_pattern COMMA . DOTDOT
Expected a pattern.

Pattern 269: labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA TILDE . _* labeled_tuple_pat_element_list_pattern_no_exn_: TILDE . _* reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE . _*
Expected a pattern.

Pattern 270: labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA TILDE LPAREN . LIDENT COLON core_type RPAREN labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN . _* reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN . LIDENT COLON core_type RPAREN COMMA DOTDOT
Expected `(` in pattern.

Pattern 271: labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT . COLON core_type RPAREN labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT . _* reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT
Expected `:` after label name in pattern.

Pattern 272: labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON . core_type RPAREN labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON . _* reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON . core_type RPAREN COMMA DOTDOT
Expected a type after `:` in pattern.

Pattern 273: labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . _* reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT
Expected `)` after pattern.

Pattern 274: labeled_tuple_pat_element_list_pattern_no_exn_: LABEL . _* labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA LABEL . simple_pattern reversed_labeled_tuple_pattern_pattern_no_exn_: LABEL . simple_pattern COMMA DOTDOT
Expected a value after `:` in pattern.

Pattern 275: labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA . _* reversed_labeled_tuple_pattern_pattern_no_exn_: pattern_no_exn COMMA . DOTDOT
Expected a pattern.

Pattern 276: labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA TILDE . _* labeled_tuple_pat_element_list_pattern_no_exn_: TILDE . _* reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE . _*
Expected a pattern.

Pattern 277: labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA TILDE LPAREN . LIDENT COLON core_type RPAREN labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN . _* reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN . LIDENT COLON core_type RPAREN COMMA DOTDOT
Expected `(` in pattern.

Pattern 278: labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA TILDE LPAREN LIDENT . COLON core_type RPAREN labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT . _* reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT
Expected `:` after label name in pattern.

Pattern 279: labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA TILDE LPAREN LIDENT COLON . core_type RPAREN labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON . _* reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON . core_type RPAREN COMMA DOTDOT
Expected a type after `:` in pattern.

Pattern 280: labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . _* reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT
Expected `)` after pattern.

Pattern 281: labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA LABEL . simple_pattern labeled_tuple_pat_element_list_pattern_no_exn_: LABEL . _* reversed_labeled_tuple_pattern_pattern_no_exn_: LABEL . simple_pattern COMMA DOTDOT
Expected a value after `:` in pattern.

Pattern 282: pattern_no_exn: pattern_no_exn COLONCOLON . pattern
Expected a pattern after `::`.

Pattern 283: pattern_no_exn: pattern_no_exn BAR . pattern
Expected a pattern after `|`.

Pattern 284: pattern_no_exn: pattern_no_exn AS . val_ident
Expected a pattern name after `as`.

Pattern 285: labeled_tuple_pat_element_list_pattern_no_exn_: labeled_tuple_pat_element_list_pattern_no_exn_ COMMA . _* reversed_labeled_tuple_pattern_pattern_no_exn_: labeled_tuple_pat_element_list_pattern_no_exn_ COMMA . DOTDOT
Expected a pattern.

Pattern 286: labeled_tuple_pat_element_list_pattern_no_exn_: labeled_tuple_pat_element_list_pattern_no_exn_ COMMA TILDE . _* labeled_tuple_pat_element_list_pattern_no_exn_: TILDE . _* reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE . _*
Expected a pattern.

Pattern 287: labeled_tuple_pat_element_list_pattern_no_exn_: labeled_tuple_pat_element_list_pattern_no_exn_ COMMA TILDE LPAREN . LIDENT COLON core_type RPAREN labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN . _* reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN . LIDENT COLON core_type RPAREN COMMA DOTDOT
Expected `(` in pattern.

Pattern 288: labeled_tuple_pat_element_list_pattern_no_exn_: labeled_tuple_pat_element_list_pattern_no_exn_ COMMA TILDE LPAREN LIDENT . COLON core_type RPAREN labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT . _* reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT
Expected `:` after label name in pattern.

Pattern 289: labeled_tuple_pat_element_list_pattern_no_exn_: labeled_tuple_pat_element_list_pattern_no_exn_ COMMA TILDE LPAREN LIDENT COLON . core_type RPAREN labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON . _* reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON . core_type RPAREN COMMA DOTDOT
Expected a type after `:` in pattern.

Pattern 290: labeled_tuple_pat_element_list_pattern_no_exn_: labeled_tuple_pat_element_list_pattern_no_exn_ COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . _* reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT
Expected `)` after pattern.

Pattern 291: labeled_tuple_pat_element_list_pattern_no_exn_: labeled_tuple_pat_element_list_pattern_no_exn_ COMMA LABEL . simple_pattern labeled_tuple_pat_element_list_pattern_no_exn_: LABEL . _* reversed_labeled_tuple_pattern_pattern_no_exn_: LABEL . simple_pattern COMMA DOTDOT
Expected a value after `:` in pattern.

Pattern 292: labeled_tuple_pat_element_list_pattern_no_exn_: LABEL . _* labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA LABEL . simple_pattern reversed_labeled_tuple_pattern_pattern_no_exn_: LABEL . simple_pattern COMMA DOTDOT
Expected a value after `:` in pattern.

Pattern 293: labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LIDENT . _* reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LIDENT . COMMA DOTDOT
Expected `)` or type in pattern.

Pattern 294: labeled_tuple_pat_element_list_pattern_no_exn_: LABEL . _* reversed_labeled_tuple_pattern_pattern_no_exn_: LABEL . simple_pattern COMMA DOTDOT
Expected a value after `:` in pattern.

Pattern 295: labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern . _* reversed_labeled_tuple_pattern_pattern_no_exn_: LABEL simple_pattern . COMMA DOTDOT
Expected a pattern.

Pattern 296: simple_param_pattern: TILDE . _*
Expected `(` or type in pattern.

Pattern 297: simple_param_pattern: TILDE LPAREN . label_let_pattern RPAREN
Expected `(` in pattern.

Pattern 298: label_let_pattern: LIDENT COLON . possibly_poly_core_type_
Expected a type after `:` in pattern.

Pattern 299: possibly_poly_core_type_: reversed_nonempty_llist_typevar_ . DOT core_type
Expected `.` after type variable.
/possibly_poly_core_type_: reversed_nonempty_llist_typevar_ DOT . core_type
Expected a type after `.` in `external` declaration.

/simple_param_pattern: TILDE LPAREN label_let_pattern . RPAREN
Expected a label identifier or pattern after `~(`.

/simple_param_pattern: QUESTION . _*
Expected a label identifier or pattern after `?`.

/simple_param_pattern: QUESTION LPAREN . label_let_pattern option_preceded_EQUAL_seq_expr__ RPAREN
Expected a label identifier or pattern after `?(`.

/simple_param_pattern: QUESTION LPAREN label_let_pattern . option_preceded_EQUAL_seq_expr__ RPAREN
Expected `=` after optional argument label name.

/option_preceded_EQUAL_seq_expr__: EQUAL . seq_expr
Expected expression after `=`.

/fun_expr: LET . ext list_attribute_ local_structure_item IN seq_expr
Expected an identifier after `let`.

/fun_expr: LET ext . list_attribute_ local_structure_item IN seq_expr
Expected an identifier after `let %`.

/fun_expr: LET ext list_attribute_ . local_structure_item IN seq_expr
Expected an identifier after `let [%...]`.

/local_structure_item: MODULE . _*
Expected a module name after `module`.

/module_type_declaration: MODULE TYPE . ext list_attribute_ ident option_preceded_EQUAL_module_type__ list_post_item_attribute_
Expected a module type name after `module type`.

/module_type_declaration: MODULE TYPE ext . list_attribute_ ident option_preceded_EQUAL_module_type__ list_post_item_attribute_
Expected a module type name after `module type %`.

/module_type_declaration: MODULE TYPE ext list_attribute_ . ident option_preceded_EQUAL_module_type__ list_post_item_attribute_
Expected a module type name after `module type [%...]`.

/option_preceded_EQUAL_module_type__: EQUAL . module_type
Expected a module type after `=`.

/functor_arg: LPAREN . _*
Expected a module type or functor argument after `(`.

/module_type: FUNCTOR . list_attribute_ reversed_nonempty_llist_functor_arg_ MINUSGREATER module_type
Expected a functor argument list after `functor`.

/module_type: FUNCTOR list_attribute_ . reversed_nonempty_llist_functor_arg_ MINUSGREATER module_type
Expected a functor argument list after `functor [%...]`.

/functor_arg: LPAREN . _*
Expected a module name after `(`.

/functor_arg: LPAREN module_name . COLON module_type RPAREN
Expected `:` after module name in functor argument.

/functor_arg: LPAREN module_name COLON . module_type RPAREN
Expected a module type after `:` in functor argument.

/module_type: reversed_nonempty_llist_functor_arg_ . MINUSGREATER module_type
Expected `->` after functor argument.

/module_type: reversed_nonempty_llist_functor_arg_ MINUSGREATER . module_type
Expected a module type after `->`.

/module_type: module_type WITH . reversed_separated_nonempty_llist_AND_with_constraint_
Expected `type`, `module`, or another constraint keyword after `with`.

/with_constraint: TYPE . _*
Expected a type identifier after `type` in `with` constraint.

/with_constraint: TYPE type_parameters . _*
Expected a type identifier after `type` parameters.

/with_constraint: TYPE type_parameters label_longident . _*
Expected `=` or `:=` after type identifier in `with` constraint.

/with_constraint: TYPE type_parameters label_longident COLONEQUAL . alias_type
Expected a type after `:=` in `with` constraint.

/with_constraint: TYPE type_parameters label_longident with_type_binder . alias_type reversed_llist_preceded_CONSTRAINT_constrain__
Expected a type after `= private` in `with` constraint.

/reversed_llist_preceded_CONSTRAINT_constrain__: reversed_llist_preceded_CONSTRAINT_constrain__ CONSTRAINT . core_type EQUAL core_type
Expected a type constraint after `constraint`.

/reversed_llist_preceded_CONSTRAINT_constrain__: reversed_llist_preceded_CONSTRAINT_constrain__ CONSTRAINT core_type . EQUAL core_type
Expected `=` after type constraint.

/reversed_llist_preceded_CONSTRAINT_constrain__: reversed_llist_preceded_CONSTRAINT_constrain__ CONSTRAINT core_type EQUAL . core_type
Expected a type after `=` in constraint.

/with_constraint: MODULE . _*
Expected a module name after `module` in `with` constraint.

/with_constraint: MODULE TYPE . _*
Expected a module type name after `module type` in `with` constraint.

/with_constraint: MODULE TYPE mty_longident . _*
Expected `=` or `:=` after module type name in `with` constraint.

/with_constraint: MODULE TYPE mty_longident EQUAL . module_type
Expected a module type after `=` in `with` constraint.

/module_type: module_type MINUSGREATER . module_type
Expected a module type after `->`.

/mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident . DOT UIDENT
Expected a module component after `.`.

/mk_longident_mod_ext_longident_ident_: mod_ext_longident . DOT ident
Expected a module component after `.`.

/with_constraint: MODULE TYPE mty_longident COLONEQUAL . module_type
Expected a module type after `:=` in `with` constraint.

/mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT
Expected a module component after `.`.

/with_constraint: MODULE mod_longident . _*
Expected a module name after `module` in `with` constraint.

/mk_longident_mod_longident_UIDENT_: mod_longident DOT . UIDENT
Expected a module component after `.`.

/with_constraint: MODULE mod_longident EQUAL . mod_ext_longident
Expected a module expression after `=` in `with` constraint.

/with_constraint: MODULE mod_longident COLONEQUAL . mod_ext_longident
Expected a module expression after `:=` in `with` constraint.

/reversed_separated_nonempty_llist_AND_with_constraint_: reversed_separated_nonempty_llist_AND_with_constraint_ AND . with_constraint
Expected a constraint keyword after `and`.

/functor_arg: LPAREN module_name COLON module_type . RPAREN
Expected `)` after functor argument module type.

/module_type: FUNCTOR list_attribute_ reversed_nonempty_llist_functor_arg_ . MINUSGREATER module_type
Expected `->` after functor argument.

/module_type: FUNCTOR list_attribute_ reversed_nonempty_llist_functor_arg_ MINUSGREATER . module_type
Expected a module type after `->`.

/module_type: LPAREN module_type . RPAREN
Expected `)` after parenthesized module type.

/local_structure_item: MODULE ext . _*
Expected a module name after `module %`.

/local_structure_item: MODULE ext list_attribute_ . _*
Expected a module name after `module [%...]`.

/local_structure_item: MODULE ext list_attribute_ REC . module_name _*
Expected a module name after 'module rec'.

/local_structure_item: MODULE ext list_attribute_ REC module_name . _*
Expected '=' or ':' after module name in 'module rec' declaration.

/module_binding_body: EQUAL . _*
Expected a module expression after '=' in module binding body.

/module_expr: FUNCTOR . _*
Expected 'functor' arguments after 'functor'.

/module_expr: FUNCTOR list_attribute_ . _*
Expected 'functor' arguments after 'functor' and attributes.

/module_expr: FUNCTOR list_attribute_ reversed_nonempty_llist_functor_arg_ . MINUSGREATER module_expr
Expected '->' to end functor arguments and start the body.

/module_expr: FUNCTOR list_attribute_ reversed_nonempty_llist_functor_arg_ MINUSGREATER . _*
Expected a module expression after 'functor' ->.

/module_expr: module_expr LPAREN . _*
Expected a module expression after '(' in parentheses.

/paren_module_expr: LPAREN module_expr . _*
Expected ')' after module expression.

/paren_module_expr: LPAREN module_expr COLON . _*
Expected a module type after ':' in module type annotation.

/paren_module_expr: LPAREN module_expr COLON module_type . _*
Expected ')' after module type in module type annotation.

/module_binding_body: COLON . _*
Expected a module type after ':' in module binding body.

/module_binding_body: COLON module_type . _*
Expected '=' after module type in module binding body.

/module_binding_body: COLON module_type EQUAL . _*
Expected a module expression after '='.

/list_and_module_binding_: AND . _*
Expected a module name or attributes after 'and'.

/list_and_module_binding_: AND list_attribute_ . _*
Expected a module name after 'and' and attributes.

/list_and_module_binding_: AND list_attribute_ module_name . _*
Expected '=' or ':' after module name in module list binding.

/module_binding_body: functor_arg . _*
Expected the module binding body or another argument after functor argument.

/local_structure_item: MODULE ext list_attribute_ module_name . _*
Expected '=' or ':' after module name.

/item_extension: LBRACKETPERCENTPERCENT . _*
Expected an identifier after '[%%'.

/item_extension: LBRACKETPERCENTPERCENT attr_id . _*
Expected a payload after '[%%' id.

/payload: QUESTION . _*
Expected a pattern or 'when' after '?'.

/payload: QUESTION pattern WHEN . _*
Expected a sequence expression after 'when'.

/constr_extra_nonprefix_ident: LBRACKET . _*
Expected expressions after '['.

/simple_expr: LBRACELESS . _*
Expected object fields or '>' after '<'.

/option_preceded_EQUAL_expr__: EQUAL . _*
Expected a value after '=' in record field.

/simple_expr: LBRACE . _*
Expected record fields or '}' after '{'.

/simple_expr: BEGIN . _*
Expected an expression or attributes after 'begin'.

/simple_expr: BEGIN ext . _*
Expected attributes or an expression after 'begin' and attributes.

/simple_expr: BEGIN ext list_attribute_ . _*
Expected an expression after 'begin' and its attributes.

/fun_expr: LAZY . _*
Expected an attribute after 'lazy'.

/fun_expr: LAZY ext . _*
Expected attributes after 'lazy' and attributes.

/fun_expr: LAZY ext list_attribute_ . _*
Expected a value after 'lazy'.

/simple_expr: BANG . _*
Expected a value after '!'.

/simple_expr: simple_expr HASHOP . _*
Expected a value after '##'.

/simple_expr: simple_expr HASH . _*
Expected an identifier after '#'.

/simple_expr: simple_expr DOTOP . _*
Expected '(' or '{' after '.' (method call or object).

/simple_expr: simple_expr DOTOP LPAREN . _*
Expected expressions after '.' (method call).

/reversed_labeled_tuple_body: LABEL . _*
Expected a value or expression after label in tuple.

/reversed_labeled_tuple_body: LABEL simple_expr . _*
Expected a comma after value in tuple.

/simple_expr: simple_expr DOT . _*
Expected '(' or '{' after '.'.

/simple_expr: simple_expr DOT LPAREN . _*
Expected expressions after '.' (method call).

/fun_expr: IF . _*
Expected an expression or attributes after 'if'.

/fun_expr: IF ext . _*
Expected attributes or an expression after 'if' and attributes.

/fun_expr: IF ext list_attribute_ . _*
Expected an expression after 'if' and its attributes.

/reversed_labeled_tuple_body: FUNCTION . _*
Expected an attribute or expression after 'function' in tuple.

/reversed_labeled_tuple_body: FUNCTION ext . _*
Expected attributes after 'function' and attributes.

/reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
Expected a value after 'function' and its attributes.

/reversed_preceded_or_separated_nonempty_llist_BAR_match_case_: BAR . _*
Expected a match arm after '|'.

/labeled_tuple_pat_element_list_pattern_: pattern . _*
Expected '->' after pattern in match expression or a comma in a tuple pattern.

Pattern 400: match_case: pattern WHEN . seq_expr MINUSGREATER seq_expr
Expected expression after 'when' in `match` case.

Pattern 401: fun_expr: FUN . ext list_attribute_ fun_params option_preceded_COLON_atomic_type__ MINUSGREATER fun_body
Expected parameters after 'fun'.

Pattern 402: fun_expr: FUN ext . list_attribute_ fun_params option_preceded_COLON_atomic_type__ MINUSGREATER fun_body
Expected function parameters after 'fun' attribute extension.

Pattern 403: fun_expr: FUN ext list_attribute_ . fun_params option_preceded_COLON_atomic_type__ MINUSGREATER fun_body
Expected function parameters after 'fun' attribute extension.

Pattern 404: simple_param_pattern: OPTLABEL . _*
Expected '=' after parameter in let binding.

Pattern 405: simple_param_pattern: OPTLABEL LPAREN . let_pattern option_preceded_EQUAL_seq_expr__ RPAREN
Expected pattern in parameter list after '('.

Pattern 406: let_pattern: pattern COLON . possibly_poly_core_type_
Expected type annotation after ':' in parameter declaration.

Pattern 407: simple_param_pattern: OPTLABEL LPAREN let_pattern . option_preceded_EQUAL_seq_expr__ RPAREN
Expected assignment expression after parameter in let binding.

Pattern 408: simple_param_pattern: OPTLABEL LPAREN let_pattern option_preceded_EQUAL_seq_expr__ . RPAREN
Expected ')' to end parameter list.

Pattern 409: fun_param_as_list: LPAREN TYPE . nonempty_list_mkrhs_LIDENT__ RPAREN
Expected type name after 'TYPE' in function parameter list.

Pattern 410: fun_param_as_list: LPAREN TYPE nonempty_list_mkrhs_LIDENT__ . RPAREN
Expected ')' to end function parameter list.

Pattern 411: labeled_tuple_pat_element_list_pattern_: pattern . _*
Expected class expression after '(' in class definition.

Pattern 412: labeled_tuple_pat_element_list_pattern_: pattern . _*
Expected expression after '(' in let binding.

Pattern 413: simple_param_pattern: LPAREN pattern COLON . reversed_nonempty_llist_typevar_ DOT core_type RPAREN
Expected type annotation after ':' in parameter declaration.

Pattern 414: simple_param_pattern: LPAREN pattern COLON reversed_nonempty_llist_typevar_ . DOT core_type RPAREN
Expected '.' after type variable in parameter declaration.

Pattern 415: simple_param_pattern: LPAREN pattern COLON reversed_nonempty_llist_typevar_ DOT . core_type RPAREN
Expected type after '.' in parameter declaration.

Pattern 416: simple_param_pattern: LPAREN pattern COLON reversed_nonempty_llist_typevar_ DOT core_type . RPAREN
Expected ')' to end parameter list.

Pattern 417: simple_param_pattern: LABEL . _*
Expected '=' after parameter in let binding.

Pattern 418: simple_param_pattern: LABEL LPAREN . pattern COLON reversed_nonempty_llist_typevar_ DOT core_type RPAREN
Expected pattern after '(' in labeled parameter declaration.

Pattern 419: labeled_tuple_pat_element_list_pattern_: pattern . _*
Expected '=' after parameter in let binding.

Pattern 420: simple_param_pattern: LABEL LPAREN pattern COLON . reversed_nonempty_llist_typevar_ DOT core_type RPAREN
Expected type annotation after ':' in parameter declaration.

Pattern 421: simple_param_pattern: LABEL LPAREN pattern COLON reversed_nonempty_llist_typevar_ . DOT core_type RPAREN
Expected '.' after type variable in parameter declaration.

Pattern 422: simple_param_pattern: LABEL LPAREN pattern COLON reversed_nonempty_llist_typevar_ DOT . core_type RPAREN
Expected type after '.' in parameter declaration.

Pattern 423: simple_param_pattern: LABEL LPAREN pattern COLON reversed_nonempty_llist_typevar_ DOT core_type . RPAREN
Expected ')' to end parameter list.

Pattern 424: fun_expr: FUN ext list_attribute_ fun_params . option_preceded_COLON_atomic_type__ MINUSGREATER fun_body
Expected type annotation or '->' after function parameters.

Pattern 425: option_preceded_COLON_atomic_type__: COLON . atomic_type
Expected type annotation after ':' in function parameter list.

Pattern 426: fun_expr: FUN ext list_attribute_ fun_params option_preceded_COLON_atomic_type__ . MINUSGREATER fun_body
Expected '->' after function type annotation.

Pattern 427: fun_expr: FUN ext list_attribute_ fun_params option_preceded_COLON_atomic_type__ MINUSGREATER . fun_body
Expected expression after '->' in function definition.

Pattern 428: fun_body: FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expected match case after 'function' keyword.

Pattern 429: fun_body: FUNCTION ext . list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expected match case after 'function' attribute extension.

Pattern 430: fun_body: FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
Expected match case after 'function' attribute extension.

Pattern 431: reversed_labeled_tuple_body: FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COMMA . _*
Expected '->' or another element after ',' in tuple element.

Pattern 432: reversed_labeled_tuple_body: FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COMMA TILDE . _*
Expected label name or '(' after '~' in function argument list.

Pattern 433: reversed_labeled_tuple_body: FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COMMA TILDE LPAREN . LIDENT type_constraint RPAREN
Expected label name after '(' in label argument.

Pattern 434: reversed_labeled_tuple_body: TILDE LPAREN LIDENT . type_constraint RPAREN
Expected type annotation or ')' after label name in argument list.

Pattern 435: reversed_labeled_tuple_body: FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COMMA TILDE LPAREN LIDENT type_constraint . RPAREN
Expected ')' to end label argument.

Pattern 436: reversed_labeled_tuple_body: LABEL . _*
Expected expression after label name in argument list.

Pattern 437: reversed_labeled_tuple_body: LABEL simple_expr COMMA . _*
Expected '->' or another element after ',' in function argument list.

Pattern 438: reversed_labeled_tuple_body: LABEL simple_expr COMMA TILDE . _*
Expected label name or '->' after ',' in function argument list.

Pattern 439: reversed_labeled_tuple_body: LABEL simple_expr COMMA TILDE LPAREN . LIDENT type_constraint RPAREN
Expected label name after '(' in label argument.

Pattern 440: reversed_labeled_tuple_body: TILDE LPAREN LIDENT . type_constraint RPAREN
Expected type annotation or ')' after label name in argument list.

Pattern 441: reversed_labeled_tuple_body: LABEL simple_expr COMMA TILDE LPAREN LIDENT type_constraint . RPAREN
Expected ')' to end label argument.

Pattern 442: reversed_labeled_tuple_body: LABEL . _*
Expected expression after label name in argument list.

Pattern 443: constr_longident: mod_longident DOT . LPAREN COLONCOLON RPAREN
Expected 'constructor' syntax after '.' in identifier.

Pattern 444: constr_longident: mod_longident DOT LPAREN . COLONCOLON RPAREN
Expected 'constructor' syntax after '(' in identifier.

Pattern 445: simple_expr: mod_longident DOT LPAREN MODULE . ext list_attribute_ module_expr COLON module_type RPAREN
Expected module name after '(' in qualified module expression.

Pattern 446: simple_expr: mod_longident DOT LPAREN MODULE ext . list_attribute_ module_expr COLON module_type RPAREN
Expected module name after 'module' in qualified module expression.

Pattern 447: simple_expr: mod_longident DOT LPAREN MODULE ext list_attribute_ . module_expr COLON module_type RPAREN
Expected module name after 'module' in qualified module expression.

Pattern 448: simple_expr: mod_longident DOT LPAREN MODULE ext list_attribute_ module_expr . COLON module_type RPAREN
Expected module type signature after ':' in qualified module expression.

Pattern 449: simple_expr: mod_longident DOT LPAREN MODULE ext list_attribute_ module_expr COLON . module_type RPAREN
Expected module type signature after ':' in qualified module expression.

Pattern 450: simple_expr: mod_longident DOT LPAREN MODULE ext list_attribute_ module_expr COLON module_type . RPAREN
Expected ')' to end qualified module expression.

Pattern 451: letop_binding_body: simple_pattern COLON . core_type EQUAL seq_expr
Expected type annotation after ':' in let binding declaration.

Pattern 452: letop_binding_body: simple_pattern COLON core_type . EQUAL seq_expr
Expected assignment expression after type annotation in let binding.

Pattern 453: letop_binding_body: simple_pattern COLON core_type EQUAL . seq_expr
Expected expression after '=' in let binding.

Pattern 454: fun_expr: FOR . ext list_attribute_ pattern EQUAL seq_expr direction_flag seq_expr DO seq_expr DONE
Expected loop variable pattern after 'for' keyword.

Pattern 455: fun_expr: FOR ext . list_attribute_ pattern EQUAL seq_expr direction_flag seq_expr DO seq_expr DONE
Expected loop variable pattern after 'for' attribute extension.

Pattern 456: fun_expr: FOR ext list_attribute_ . pattern EQUAL seq_expr direction_flag seq_expr DO seq_expr DONE
Expected loop variable pattern after 'for' attribute extension.

Pattern 457: fun_expr: FOR ext list_attribute_ pattern . EQUAL seq_expr direction_flag seq_expr DO seq_expr DONE
Expected assignment operator '=' after loop variable in 'for' loop.

Pattern 458: fun_expr: FOR ext list_attribute_ pattern EQUAL . seq_expr direction_flag seq_expr DO seq_expr DONE
Expected sequence expression after '=' in 'for' loop increment.

Pattern 459: fun_expr: ASSERT . ext list_attribute_ simple_expr
Expected expression after 'assert' keyword.

/Pattern 460: fun_expr: ASSERT ext . list_attribute_ simple_expr
Expected a simple expression after attribute in `assert`.

/Pattern 461: fun_expr: ASSERT ext list_attribute_ . simple_expr
Expected a simple expression after attribute in `assert`.

/Pattern 462: fun_expr: subtractive . _*
Expected an expression after `-%`.

/Pattern 463: fun_expr: subtractive FUNCTION . _*
Expected a function introduction after `-%`.

/Pattern 464: fun_expr: subtractive FUNCTION ext . list_attribute_ ...
Expected a function introduction after `-%`.

/Pattern 465: fun_expr: subtractive FUNCTION ext list_attribute_ . _*
Expected a function introduction after `-%`.

/Pattern 466: reversed_preceded_or_separated_nonempty_llist_BAR_match_case_: ... BAR . match_case
Expected a match case after `|`.

/Pattern 467: labeled_simple_expr: TILDE . _*
Expected a label name after `~`.

/Pattern 468: labeled_simple_expr: TILDE LPAREN . _*
Expected a label name after `~ (`.

/Pattern 469: labeled_simple_expr: TILDE LPAREN LIDENT . _*
Expected a type constraint after `~ ( label`.

/Pattern 470: labeled_simple_expr: TILDE LPAREN LIDENT type_constraint . RPAREN
Expected a closing parenthesis after `~ ( label : type`.

/Pattern 471: labeled_simple_expr: QUESTION . LIDENT
Expected a label name after `?`.

/Pattern 472: labeled_simple_expr: OPTLABEL . simple_expr
Expected an expression after `?label:`.

/Pattern 473: labeled_simple_expr: LABEL . simple_expr
Expected a simple expression after `~label:`.

/Pattern 474: fun_expr: simple_expr DOTOP . _*
Expected an expression after `.` followed by an infix operator.

/Pattern 475: simple_expr: simple_expr DOTOP LPAREN . _*
Expected an expression after `.+ (`.

/Pattern 476: reversed_labeled_tuple_body: FUNCTION . _*
Expected an expression after `function`.

/Pattern 477: reversed_labeled_tuple_body: FUNCTION ext . _*
Expected an expression after `function`.

/Pattern 478: reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
Expected an expression after `function`.

/Pattern 479: reversed_labeled_tuple_body: reversed_labeled_tuple_body COMMA . _*
Expected an expression after `,`.

/Pattern 480: reversed_labeled_tuple_body: reversed_labeled_tuple_body COMMA TILDE . _*
Expected a label name after `~`.

/Pattern 481: reversed_labeled_tuple_body: reversed_labeled_tuple_body COMMA TILDE LPAREN . _*
Expected a label name after `~ (`.

/Pattern 482: reversed_labeled_tuple_body: reversed_labeled_tuple_body COMMA TILDE LPAREN LIDENT . _*
Expected a type constraint after `~ ( label`.

/Pattern 483: reversed_labeled_tuple_body: reversed_labeled_tuple_body COMMA TILDE LPAREN LIDENT type_constraint . RPAREN
Expected a closing parenthesis after `~ ( label : type`.

/Pattern 484: reversed_labeled_tuple_body: reversed_labeled_tuple_body COMMA LABEL . simple_expr
Expected a simple expression after `~label:`.

/Pattern 485: reversed_labeled_tuple_body: reversed_labeled_tuple_body COMMA FUNCTION . _*
Expected an expression after `function`.

/Pattern 486: reversed_labeled_tuple_body: reversed_labeled_tuple_body COMMA FUNCTION ext . _*
Expected an expression after `function`.

/Pattern 487: reversed_labeled_tuple_body: reversed_labeled_tuple_body COMMA FUNCTION ext list_attribute_ . _*
Expected an expression after `function`.

/Pattern 488: fun_expr: let_bindings_ext_ . IN seq_expr
Expected `in` after `let`.

/Pattern 489: fun_expr: let_bindings_ext_ IN . seq_expr
Expected an expression after `let ... in`.

/Pattern 490: fun_expr: fun_expr STAR . _*
Expected an expression after `*`.

/Pattern 491: fun_expr: fun_expr STAR FUNCTION . _*
Expected an expression after `*`.

/Pattern 492: fun_expr: fun_expr STAR FUNCTION ext . _*
Expected an expression after `*`.

/Pattern 493: fun_expr: fun_expr STAR FUNCTION ext list_attribute_ . _*
Expected an expression after `*`.

/Pattern 494: fun_expr: fun_expr PLUSEQ . _*
Expected an expression after `+=`.

/Pattern 495: fun_expr: fun_expr PLUSEQ FUNCTION . _*
Expected an expression after `+=`.

/Pattern 496: fun_expr: fun_expr PLUSEQ FUNCTION ext . _*
Expected an expression after `+=`.

/Pattern 497: fun_expr: fun_expr PLUSEQ FUNCTION ext list_attribute_ . _*
Expected an expression after `+=`.

/Pattern 498: fun_expr: fun_expr PLUSDOT . _*
Expected an expression after `+.`.

/Pattern 499: fun_expr: fun_expr PLUSDOT FUNCTION . _*
Expected an expression after `+.`.

/Pattern 500: fun_expr: fun_expr PLUSDOT FUNCTION ext . _*
Expected an expression after `+.`.

/Pattern 501: fun_expr: fun_expr PLUSDOT FUNCTION ext list_attribute_ . _*
Expected an expression after `+.`.

/Pattern 502: fun_expr: fun_expr PLUS . _*
Expected an expression after `+`.

/Pattern 503: fun_expr: fun_expr PLUS FUNCTION . _*
Expected an expression after `+`.

/Pattern 504: fun_expr: fun_expr PLUS FUNCTION ext . _*
Expected an expression after `+`.

/Pattern 505: fun_expr: fun_expr PLUS FUNCTION ext list_attribute_ . _*
Expected an expression after `+`.

/Pattern 506: fun_expr: fun_expr PERCENT . _*
Expected an expression after `%`.

/Pattern 507: fun_expr: fun_expr PERCENT FUNCTION . _*
Expected an expression after `%`.

/Pattern 508: fun_expr: fun_expr PERCENT FUNCTION ext . _*
Expected an expression after `%`.

/Pattern 509: fun_expr: fun_expr PERCENT FUNCTION ext list_attribute_ . _*
Expected an expression after `%`.

/Pattern 510: fun_expr: fun_expr OR . _*
Expected an expression after `or`.

/Pattern 511: fun_expr: fun_expr OR FUNCTION . _*
Expected an expression after `or`.

/Pattern 512: fun_expr: fun_expr OR FUNCTION ext . _*
Expected an expression after `or`.

/Pattern 513: fun_expr: fun_expr OR FUNCTION ext list_attribute_ . _*
Expected an expression after `or`.

/Pattern 514: fun_expr: fun_expr MINUSDOT . _*
Expected an expression after `-.`.

/Pattern 515: fun_expr: fun_expr MINUSDOT FUNCTION . _*
Expected an expression after `-.`.

/Pattern 516: fun_expr: fun_expr MINUSDOT FUNCTION ext . _*
Expected an expression after `-.`.

/Pattern 517: fun_expr: fun_expr MINUSDOT FUNCTION ext list_attribute_ . _*
Expected an expression after `-.`.

/Pattern 518: fun_expr: fun_expr MINUS . _*
Expected an expression after `-%`.

/Pattern 519: fun_expr: fun_expr MINUS FUNCTION . _*
Expected an expression after `-%`.

/Pattern 520: fun_expr: fun_expr MINUS FUNCTION ext . _*
Expected an expression after `-%`.

/Pattern 521: fun_expr: fun_expr MINUS FUNCTION ext list_attribute_ . _*
Expected an expression after `-%`.

/Pattern 522: fun_expr: fun_expr LESS . _*
Expected an expression after `<`.

/Pattern 523: fun_expr: fun_expr LESS FUNCTION . _*
Expected an expression after `<`.

/Pattern 524: fun_expr: fun_expr LESS FUNCTION ext . _*
Expected an expression after `<`.

/Pattern 525: fun_expr: fun_expr LESS FUNCTION ext list_attribute_ . _*
Expected an expression after `<`.

/Pattern 526: fun_expr: fun_expr INFIXOP4 . _*
Expected an expression after `**`.

/Pattern 527: fun_expr: fun_expr INFIXOP4 FUNCTION . _*
Expected an expression after `**`.

/Pattern 528: fun_expr: fun_expr INFIXOP4 FUNCTION ext . _*
Expected an expression after `**`.

/Pattern 529: fun_expr: fun_expr INFIXOP4 FUNCTION ext list_attribute_ . _*
Expected an expression after `**`.

Pattern 530: fun_expr: fun_expr INFIXOP3 .
Expected a right-hand side expression after operator `INFIXOP3` (e.g., `land`).

Pattern 531: fun_expr: fun_expr INFIXOP3 FUNCTION .
Expected a function body (`match` case list or attributes).

Pattern 532: fun_expr: fun_expr INFIXOP3 FUNCTION ext .
Expected a function body (`match` case list or attributes).

Pattern 533: fun_expr: fun_expr INFIXOP3 FUNCTION ext list_attribute_ .
Expected a function body (`match` case list or attributes).

Pattern 534: fun_expr: fun_expr INFIXOP2 .
Expected a right-hand side expression after operator `INFIXOP2` (e.g., `+!`).

Pattern 535: fun_expr: fun_expr INFIXOP2 FUNCTION .
Expected a function body (`match` case list or attributes).

Pattern 536: fun_expr: fun_expr INFIXOP2 FUNCTION ext .
Expected a function body (`match` case list or attributes).

Pattern 537: fun_expr: fun_expr INFIXOP2 FUNCTION ext list_attribute_ .
Expected a function body (`match` case list or attributes).

Pattern 538: fun_expr: fun_expr INFIXOP1 .
Expected a right-hand side expression after operator `INFIXOP1` (e.g., `^`).

Pattern 539: fun_expr: fun_expr INFIXOP1 FUNCTION .
Expected a function body (`match` case list or attributes).

Pattern 540: fun_expr: fun_expr INFIXOP1 FUNCTION ext .
Expected a function body (`match` case list or attributes).

Pattern 541: fun_expr: fun_expr INFIXOP1 FUNCTION ext list_attribute_ .
Expected a function body (`match` case list or attributes).

Pattern 542: fun_expr: fun_expr INFIXOP0 .
Expected a right-hand side expression after operator `INFIXOP0` (e.g., `!=`).

Pattern 543: fun_expr: fun_expr INFIXOP0 FUNCTION .
Expected a function body (`match` case list or attributes).

Pattern 544: fun_expr: fun_expr INFIXOP0 FUNCTION ext .
Expected a function body (`match` case list or attributes).

Pattern 545: fun_expr: fun_expr INFIXOP0 FUNCTION ext list_attribute_ .
Expected a function body (`match` case list or attributes).

Pattern 546: fun_expr: fun_expr GREATER .
Expected a right-hand side expression after operator `>`

Pattern 547: fun_expr: fun_expr GREATER FUNCTION .
Expected a function body (`match` case list or attributes).

Pattern 548: fun_expr: fun_expr GREATER FUNCTION ext .
Expected a function body (`match` case list or attributes).

Pattern 549: fun_expr: fun_expr GREATER FUNCTION ext list_attribute_ .
Expected a function body (`match` case list or attributes).

Pattern 550: fun_expr: fun_expr EQUAL .
Expected a right-hand side expression after operator `=`

Pattern 551: fun_expr: fun_expr EQUAL FUNCTION .
Expected a function body (`match` case list or attributes).

Pattern 552: fun_expr: fun_expr EQUAL FUNCTION ext .
Expected a function body (`match` case list or attributes).

Pattern 553: fun_expr: fun_expr EQUAL FUNCTION ext list_attribute_ .
Expected a function body (`match` case list or attributes).

Pattern 554: reversed_labeled_tuple_body: fun_expr COMMA .
Expected another element in tuple or list.

Pattern 555: reversed_labeled_tuple_body: fun_expr COMMA TILDE .
Expected a label argument after `~`.

Pattern 556: reversed_labeled_tuple_body: fun_expr COMMA TILDE LPAREN .
Expected a label argument after `~`.

Pattern 557: reversed_labeled_tuple_body: fun_expr COMMA TILDE LPAREN LIDENT .
Expected a type constraint or closing parenthesis.

Pattern 558: reversed_labeled_tuple_body: fun_expr COMMA TILDE LPAREN LIDENT type_constraint .
Expected a closing parenthesis.

Pattern 559: reversed_labeled_tuple_body: fun_expr COMMA LABEL .
Expected a label argument after `LABEL`.

Pattern 560: reversed_labeled_tuple_body: fun_expr COMMA FUNCTION .
Expected a function body (`match` case list or attributes).

Pattern 561: reversed_labeled_tuple_body: fun_expr COMMA FUNCTION ext .
Expected a function body (`match` case list or attributes).

Pattern 562: reversed_labeled_tuple_body: fun_expr COMMA FUNCTION ext list_attribute_ .
Expected a function body (`match` case list or attributes).

Pattern 563: fun_expr: fun_expr COLONEQUAL .
Expected a right-hand side expression after operator `:=`

Pattern 564: fun_expr: fun_expr COLONEQUAL FUNCTION .
Expected a function body (`match` case list or attributes).

Pattern 565: fun_expr: fun_expr COLONEQUAL FUNCTION ext .
Expected a function body (`match` case list or attributes).

Pattern 566: fun_expr: fun_expr COLONEQUAL FUNCTION ext list_attribute_ .
Expected a function body (`match` case list or attributes).

Pattern 567: fun_expr: fun_expr COLONCOLON .
Expected a right-hand side expression after operator `::`

Pattern 568: fun_expr: fun_expr COLONCOLON FUNCTION .
Expected a function body (`match` case list or attributes).

Pattern 569: fun_expr: fun_expr COLONCOLON FUNCTION ext .
Expected a function body (`match` case list or attributes).

Pattern 570: fun_expr: fun_expr COLONCOLON FUNCTION ext list_attribute_ .
Expected a function body (`match` case list or attributes).

Pattern 571: fun_expr: fun_expr BARBAR .
Expected a right-hand side expression after operator `||`

Pattern 572: fun_expr: fun_expr BARBAR FUNCTION .
Expected a function body (`match` case list or attributes).

Pattern 573: fun_expr: fun_expr BARBAR FUNCTION ext .
Expected a function body (`match` case list or attributes).

Pattern 574: fun_expr: fun_expr BARBAR FUNCTION ext list_attribute_ .
Expected a function body (`match` case list or attributes).

Pattern 575: fun_expr: fun_expr AMPERSAND .
Expected a right-hand side expression after operator `&`

Pattern 576: fun_expr: fun_expr AMPERSAND FUNCTION .
Expected a function body (`match` case list or attributes).

Pattern 577: fun_expr: fun_expr AMPERSAND FUNCTION ext .
Expected a function body (`match` case list or attributes).

Pattern 578: fun_expr: fun_expr AMPERSAND FUNCTION ext list_attribute_ .
Expected a function body (`match` case list or attributes).

Pattern 579: fun_expr: fun_expr AMPERAMPER .
Expected a right-hand side expression after operator `&&`

Pattern 580: fun_expr: fun_expr AMPERAMPER FUNCTION .
Expected a function body (`match` case list or attributes).

Pattern 581: fun_expr: fun_expr AMPERAMPER FUNCTION ext .
Expected a function body (`match` case list or attributes).

Pattern 582: fun_expr: fun_expr AMPERAMPER FUNCTION ext list_attribute_ .
Expected a function body (`match` case list or attributes).

Pattern 583: fun_expr: additive .
Expected a valid expression.

Pattern 584: fun_expr: additive FUNCTION .
Expected a function body (`match` case list or attributes).

Pattern 585: fun_expr: additive FUNCTION ext .
Expected a function body (`match` case list or attributes).

Pattern 586: fun_expr: additive FUNCTION ext list_attribute_ .
Expected a function body (`match` case list or attributes).

Pattern 587: fun_seq_expr: fun_expr SEMI PERCENT .
Expected an attribute name after `%`.

Pattern 588: fun_seq_expr: fun_expr SEMI PERCENT attr_id .
Expected an expression after attribute.

Pattern 589: and_let_binding: AND .
Expected a `let` binding after `and`.

Pattern 590: and_let_binding: AND list_attribute_ .
Expected a `let` binding after `and`.

Pattern 591: strict_binding: EQUAL .
Expected an expression after `=`.

Pattern 592: let_binding_body_no_punning: val_ident COLON .
Expected a type after `:`.

Pattern 593: let_binding_body_no_punning: val_ident COLON TYPE .
Expected a type name after `type`.

Pattern 594: let_binding_body_no_punning: val_ident COLON TYPE nonempty_list_mkrhs_LIDENT__ .
Expected `.` after type name.

Pattern 595: let_binding_body_no_punning: val_ident COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT .
Expected a type name after `.`.

Pattern 596: let_binding_body_no_punning: val_ident COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type .
Expected `=` for the let binding.

Pattern 597: let_binding_body_no_punning: val_ident COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL .
Expected a right-hand side expression after `=`.

Pattern 598: let_binding_body_no_punning: val_ident COLON reversed_nonempty_llist_typevar_ .
Expected `.` after type variable.

Pattern 599: let_binding_body_no_punning: val_ident COLON reversed_nonempty_llist_typevar_ DOT .
Expected a type name after `.`.

Pattern 600: let_binding_body_no_punning: val_ident COLON reversed_nonempty_llist_typevar_ DOT core_type .
Expected `=` for the let binding.

Pattern 601: let_binding_body_no_punning: val_ident COLON reversed_nonempty_llist_typevar_ DOT core_type EQUAL .
Expected a right-hand side expression after `=`.

Pattern 602: let_binding_body_no_punning: val_ident type_constraint .
Expected `=` for the let binding.

Pattern 603: let_binding_body_no_punning: val_ident type_constraint EQUAL .
Expected a right-hand side expression after `=`.

Pattern 604: strict_binding: fun_params .
Expected type constraint or `=`.

Pattern 605: strict_binding: fun_params option_type_constraint_ .
Expected `=` for the let binding.

Pattern 606: strict_binding: fun_params option_type_constraint_ EQUAL .
Expected an expression after `=`.

Pattern 607: let_binding_body_no_punning: simple_pattern_not_ident COLON .
Expected a type after `:`.

Pattern 608: let_binding_body_no_punning: simple_pattern_not_ident COLON core_type .
Expected `=` for the let binding.

Pattern 609: let_binding_body_no_punning: simple_pattern_not_ident COLON core_type EQUAL .
Expected a right-hand side expression after `=`.

Below is a ready‑to‑use set of human‑readable error messages for every pattern you listed.
Each entry follows the same style as the examples you gave:

```
/<nonterminal>: <production> . <look‑ahead>
<Explanation>
```

The messages are written so that the user sees exactly what the parser was expecting at the point where it failed.

---

### Pattern 610

```
/labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn . _*
Expected a tuple‑pattern separator after a pattern element (either “,” or “..”).

/let_binding_body_no_punning: pattern_no_exn . EQUAL seq_expr
Expected ‘=’ after the pattern of a let‑binding.

/reversed_labeled_tuple_pattern_pattern_no_exn_: pattern_no_exn . COMMA DOTDOT
“..” may only appear after a comma in an open tuple pattern; a pattern must be followed by “,” before “..”.
```

---

### Pattern 611

```
/let_binding_body_no_punning: pattern_no_exn EQUAL . seq_expr
Expected an expression after ‘=’ in a let‑binding.
```

---

### Pattern 612

```
/fun_expr: simple_expr DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ . _*
/simple_expr: simple_expr DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ . RPAREN
Expected a closing ‘)’ (and possibly an expression) after the ‘.+( …’ operator call.
```

---

### Pattern 613

```
/fun_expr: simple_expr DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN LESSMINUS . _*
Expected an expression after the assignment operator ‘<-’ that follows a ‘.+( …)’ call.
```

---

### Pattern 614

```
/fun_expr: simple_expr DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN LESSMINUS FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
/reversed_labeled_tuple_body: FUNCTION . _*
Expected a pattern‑matching clause after the keyword ‘function’.
```

---

### Pattern 615

```
/fun_expr: simple_expr DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN LESSMINUS FUNCTION ext . list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
/reversed_labeled_tuple_body: FUNCTION ext . _*
Expected a pattern‑matching clause after ‘function’ (attributes may follow the keyword).
```

---

### Pattern 616

```
/fun_expr: simple_expr DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN LESSMINUS FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
/reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
Expected a pattern‑matching clause after ‘function’ with its attached attributes.
```

---

### Pattern 617

```
/fun_expr: simple_expr DOTOP LBRACKET . _*
/simple_expr: simple_expr DOTOP LBRACKET . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET
Expected an expression or ‘]’ after the ‘.+[’ start of a list literal.
```

---

### Pattern 618

```
/fun_expr: simple_expr DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ . _*
/simple_expr: simple_expr DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACKET
Expected ‘]’ to close the list literal started by ‘.+[’.
```

---

### Pattern 619

```
/fun_expr: simple_expr DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS . _*
Expected an expression after the assignment operator ‘<-’ that follows a list literal.
```

---

### Pattern 620

```
/fun_expr: simple_expr DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
/reversed_labeled_tuple_body: FUNCTION . _*
Expected a pattern‑matching clause after ‘function’ that follows a list literal.
```

---

### Pattern 621

```
/fun_expr: simple_expr DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS FUNCTION ext . list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
/reversed_labeled_tuple_body: FUNCTION ext . _*
Expected a pattern‑matching clause after ‘function’ (attributes may follow the keyword) that follows a list literal.
```

---

### Pattern 622

```
/fun_expr: simple_expr DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
/reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
Expected a pattern‑matching clause after ‘function’ with its attributes that follows a list literal.
```

---

### Pattern 623

```
/fun_expr: simple_expr DOTOP LBRACE . _*
/simple_expr: simple_expr DOTOP LBRACE . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE
Expected an expression or ‘}’ after the ‘.+{’ start of a record literal.
```

---

### Pattern 624

```
/fun_expr: simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ . _*
/simple_expr: simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACE
Expected ‘}’ to close the record literal started by ‘.+{’.
```

---

### Pattern 625

```
/fun_expr: simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS . _*
Expected an expression after the assignment operator ‘<-’ that follows a record literal.
```

---

### Pattern 626

```
/fun_expr: simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
/reversed_labeled_tuple_body: FUNCTION . _*
Expected a pattern‑matching clause after ‘function’ that follows a record literal.
```

---

### Pattern 627

```
/fun_expr: simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS FUNCTION ext . list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
/reversed_labeled_tuple_body: FUNCTION ext . _*
Expected a pattern‑matching clause after ‘function’ (attributes may follow) that follows a record literal.
```

---

### Pattern 628

```
/fun_expr: simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
/reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
Expected a pattern‑matching clause after ‘function’ with its attributes that follows a record literal.
```

---

### Pattern 629

```
/fun_expr: simple_expr DOT . _*
/simple_expr: simple_expr DOT . _*
Unexpected token after a dot. Expected a field name, a module name, or a parenthesised expression (e.g. “obj.field”, “M.x”, or “x.(e)”).
```

---

### Pattern 630

```
/fun_expr: simple_expr DOT LPAREN . _*
/simple_expr: simple_expr DOT LPAREN . seq_expr RPAREN
Expected an expression after ‘(’ (or a closing ‘)’).
In other words, “obj.(…)” needs the inner expression before the closing parenthesis.
```

---

### Pattern 631

```
/fun_expr: simple_expr DOT LPAREN seq_expr . _*
/simple_expr: simple_expr DOT LPAREN seq_expr . RPAREN
Expected ‘)’ to close the parenthesised expression after a dot.
```

---

### Pattern 632

```
/fun_expr: simple_expr DOT LPAREN seq_expr RPAREN LESSMINUS . _*
Expected an expression after the assignment operator ‘<-’ that follows a method‑call like “obj.(e)”.
```

---

### Pattern 633

```
/fun_expr: simple_expr DOT LPAREN seq_expr RPAREN LESSMINUS FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
/reversed_labeled_tuple_body: FUNCTION . _*
Expected a pattern‑matching clause after ‘function’ that follows a method‑call with ‘<-’.
```

---

### Pattern 634

```
/fun_expr: simple_expr DOT LPAREN seq_expr RPAREN LESSMINUS FUNCTION ext . list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
/reversed_labeled_tuple_body: FUNCTION ext . _*
Expected a pattern‑matching clause after ‘function’ (attributes may follow) that follows a method‑call with ‘<-’.
```

---

### Pattern 635

```
/fun_expr: simple_expr DOT LPAREN seq_expr RPAREN LESSMINUS FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
/reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
Expected a pattern‑matching clause after ‘function’ with its attributes that follows a method‑call with ‘<-’.
```

---

### Pattern 636

```
/fun_expr: simple_expr DOTOP LBRACKET . _*
/simple_expr: simple_expr DOTOP LBRACKET . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET
Expected an expression or ‘]’ after the ‘.+[’ start of a list literal.
```

---

### Pattern 637

```
/fun_expr: simple_expr DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ . _*
/simple_expr: simple_expr DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACKET
Expected ‘]’ to close the list literal started by ‘.+[’.
```

---

### Pattern 638

```
/fun_expr: simple_expr DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS . _*
Expected an expression after the assignment operator ‘<-’ that follows a list literal.
```

---

### Pattern 639

```
/fun_expr: simple_expr DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
/reversed_labeled_tuple_body: FUNCTION . _*
Expected a pattern‑matching clause after ‘function’ that follows a list literal with ‘<-’.
```

---

### Pattern 640

```
/fun_expr: simple_expr DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS FUNCTION ext . list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
/reversed_labeled_tuple_body: FUNCTION ext . _*
Expected a pattern‑matching clause after ‘function’ (attributes may follow) that follows a list literal with ‘<-’.
```

---

### Pattern 641

```
/fun_expr: simple_expr DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
/reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
Expected a pattern‑matching clause after ‘function’ with its attributes that follows a list literal with ‘<-’.
```

---

### Pattern 642

```
/fun_expr: simple_expr DOTOP LBRACE . _*
/simple_expr: simple_expr DOTOP LBRACE . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE
Expected an expression or ‘}’ after the ‘.+{’ start of a record literal.
```

---

### Pattern 643

```
/fun_expr: simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ . _*
/simple_expr: simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACE
Expected ‘}’ to close the record literal started by ‘.+{’.
```

---

### Pattern 644

```
/fun_expr: simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS . _*
Expected an expression after the assignment operator ‘<-’ that follows a record literal.
```

---

### Pattern 645

```
/fun_expr: simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
/reversed_labeled_tuple_body: FUNCTION . _*
Expected a pattern‑matching clause after ‘function’ that follows a record literal with ‘<-’.
```

---

### Pattern 646

```
/fun_expr: simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS FUNCTION ext . list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
/reversed_labeled_tuple_body: FUNCTION ext . _*
Expected a pattern‑matching clause after ‘function’ (attributes may follow) that follows a record literal with ‘<-’.
```

---

### Pattern 647

```
/fun_expr: simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
/reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
Expected a pattern‑matching clause after ‘function’ with its attributes that follows a record literal with ‘<-’.
```

---

### Pattern 648

```
/fun_expr: simple_expr DOT mod_longident . _*
/mk_longident_mod_longident_LIDENT_: mod_longident . DOT LIDENT
/mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT
/simple_expr: simple_expr DOT mod_longident . _*
Expected a field name, a module component, or ‘(…)’ after “expr . M”.
In other words, after a dot you must write either “field”, “M.x”, or a parenthesised expression.
```

---

### Pattern 649

```
/fun_expr: simple_expr DOT mod_longident DOTOP . _*
/simple_expr: simple_expr DOT mod_longident DOTOP . _*
Expected a list literal or a ‘(…)’ after the “.+” operator applied to a module path.
```

---

### Pattern 650

```
/fun_expr: simple_expr DOT mod_longident DOTOP LPAREN . _*
/simple_expr: simple_expr DOT mod_longident DOTOP LPAREN . separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN
Expected an expression (or ‘)’) after the ‘.+(’ that follows a module path.
```

---

### Pattern 651

```
/fun_expr: simple_expr DOT mod_longident DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ . _*
/simple_expr: simple_expr DOT mod_longident DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ . RPAREN
Expected ‘)’ to close the argument list after “.+( … )”.
```

---

### Pattern 652

```
/fun_expr: simple_expr DOT mod_longident DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN LESSMINUS . _*
Expected an expression after the assignment operator ‘<-’ that follows a “.+( … )” call.
```

---

### Pattern 653

```
/fun_expr: simple_expr DOT mod_longident DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN LESSMINUS FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
/reversed_labeled_tuple_body: FUNCTION . _*
Expected a pattern‑matching clause after ‘function’ that follows a “.+( … ) <-”.
```

---

### Pattern 654

```
/fun_expr: simple_expr DOT mod_longident DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN LESSMINUS FUNCTION ext . list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
/reversed_labeled_tuple_body: FUNCTION ext . _*
Expected a pattern‑matching clause after ‘function’ (attributes may follow) that follows a “.+( … ) <-”.
```

---

### Pattern 655

```
/fun_expr: simple_expr DOT mod_longident DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN LESSMINUS FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
/reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
Expected a pattern‑matching clause after ‘function’ with its attributes that follows a “.+( … ) <-”.
```

---

### Pattern 656

```
/fun_expr: simple_expr DOT mod_longident DOTOP LBRACKET . _*
/simple_expr: simple_expr DOT mod_longident DOTOP LBRACKET . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET
Expected an expression or ‘]’ after the ‘.+[’ that follows a module path.
```

---

### Pattern 657

```
/fun_expr: simple_expr DOT mod_longident DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ . _*
/simple_expr: simple_expr DOT mod_longident DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACKET
Expected ‘]’ to close the list literal started by “.+[ …”.
```

---

### Pattern 658

```
/fun_expr: simple_expr DOT mod_longident DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS . _*
Expected an expression after the assignment operator ‘<-’ that follows a list literal after a module path.
```

---

### Pattern 659

```
/fun_expr: simple_expr DOT mod_longident DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
/reversed_labeled_tuple_body: FUNCTION . _*
Expected a pattern‑matching clause after ‘function’ that follows a list literal with ‘<-’.
```

---

### Pattern 660

```
/fun_expr: simple_expr DOT mod_longident DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS FUNCTION ext . list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
/reversed_labeled_tuple_body: FUNCTION ext . _*
Expected a pattern‑matching clause after ‘function’ (attributes may follow) that follows a list literal with ‘<-’.
```

---

### Pattern 661

```
/fun_expr: simple_expr DOT mod_longident DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
/reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
Expected a pattern‑matching clause after ‘function’ with its attributes that follows a list literal with ‘<-’.
```

---

### Pattern 662

```
/fun_expr: simple_expr DOT mod_longident DOTOP LBRACE . _*
/simple_expr: simple_expr DOT mod_longident DOTOP LBRACE . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE
Expected an expression or ‘}’ after the ‘.+{’ that follows a module path.
```

---

### Pattern 663

```
/fun_expr: simple_expr DOT mod_longident DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ . _*
/simple_expr: simple_expr DOT mod_longident DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACE
Expected ‘}’ to close the record literal started by “.+{ …”.
```

---

### Pattern 664

```
/fun_expr: simple_expr DOT mod_longident DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS . _*
Expected an expression after the assignment operator ‘<-’ that follows a record literal after a module path.
```

---

### Pattern 665

```
/fun_expr: simple_expr DOT mod_longident DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
/reversed_labeled_tuple_body: FUNCTION . _*
Expected a pattern‑matching clause after ‘function’ that follows a record literal with ‘<-’.
```

---

### Pattern 666

```
/fun_expr: simple_expr DOT mod_longident DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS FUNCTION ext . list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
/reversed_labeled_tuple_body: FUNCTION ext . _*
Expected a pattern‑matching clause after ‘function’ (attributes may follow) that follows a record literal with ‘<-’.
```

---

### Pattern 667

```
/fun_expr: simple_expr DOT mod_longident DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
/reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
Expected a pattern‑matching clause after ‘function’ with its attributes that follows a record literal with ‘<-’.
```

---

### Pattern 668

```
/fun_expr: simple_expr DOT label_longident LESSMINUS . _*
Expected an expression after the assignment operator ‘<-’ that follows a field update “obj.field <-”.
```

---

### Pattern 669

```
/fun_expr: simple_expr DOT label_longident LESSMINUS FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
/reversed_labeled_tuple_body: FUNCTION . _*
Expected a pattern‑matching clause after ‘function’ that follows a field update with ‘<-’.
```

**Suggested diagnostic messages for the patterns 670‑699**

---

### 670
```
fun_expr: simple_expr DOT label_longident LESSMINUS FUNCTION ext . list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
```
**Message**
*After the arrow‑assignment `x <- function` the parser expected either an attribute list **or** a non‑empty list of match‑cases (`| …`).  Insert `[` … `]` attributes or start a match case with `|`.*

---

### 671
```
fun_expr: simple_expr DOT label_longident LESSMINUS FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
```
**Message**
*`function` may be followed by an attribute list, but after the attributes a match‑case list must appear.  Write `| pattern -> expr …` (or another `|` clause).*

---

### 672
```
fun_expr: FOR ext list_attribute_ pattern EQUAL seq_expr . direction_flag seq_expr DO seq_expr DONE
```
**Message**
*After the initial bound of a `for` loop the parser expects the direction keyword `to` or `downto`.  Add the missing direction keyword.*

---

### 673
```
fun_expr: FOR ext list_attribute_ pattern EQUAL seq_expr direction_flag . seq_expr DO seq_expr DONE
```
**Message**
*The loop bound after the direction keyword (`to`/`downto`) is missing.  Insert the limit expression.*

---

### 674
```
fun_expr: FOR ext list_attribute_ pattern EQUAL seq_expr direction_flag seq_expr . DO seq_expr DONE
```
**Message**
*The `do` keyword that starts the loop body is required here.  Insert `do` after the second bound.*

---

### 675
```
fun_expr: FOR ext list_attribute_ pattern EQUAL seq_expr direction_flag seq_expr DO . seq_expr DONE
```
**Message**
*The body of a `for` loop must be an expression.  Provide the expression (or a sequence of expressions) after `do`.*

---

### 676
```
fun_expr: FOR ext list_attribute_ pattern EQUAL seq_expr direction_flag seq_expr DO seq_expr . DONE
```
**Message**
*Every `for … do …` block must be closed with `done`.  Add the missing `done` keyword.*

---

### 677 – three sub‑patterns

| Sub‑pattern | Message |
|-------------|---------|
| `labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn . _*` | *After a pattern in a tuple you may only see a comma, a bar (`|`), or the tuple’s closing delimiter.  The current token is not allowed.* |
| `letop_binding_body: pattern_no_exn . EQUAL seq_expr` | *A `let*` binding requires an `=` after the pattern.  Insert `=`.* |
| `reversed_labeled_tuple_pattern_pattern_no_exn_: pattern_no_exn . COMMA DOTDOT` | *In a tuple pattern the token `..` can only appear after a comma‑separated element.  The comma is missing.* |

---

### 678
```
letop_binding_body: pattern_no_exn EQUAL . seq_expr
```
**Message**
*An expression must follow the `=` of a `let*` binding.  Write the expression you want to bind.*

---

### 679
```
fun_expr: LETOP letop_bindings . IN seq_expr
```
**Message**
*After a series of `let*` bindings the keyword `in` is required.  Insert `in` before the following expression.*

---

### 680
```
fun_expr: LETOP letop_bindings IN . seq_expr
```
**Message**
*An expression is expected after `in` in a `let* … in` construct.  Provide the body expression.*

---

### 681
```
letop_bindings: letop_bindings ANDOP . letop_binding_body
```
**Message**
*`and*` must be followed by another `let*` binding.  Write a new pattern (and optional `=` … expression) after `and*`.*

---

### 682
```
simple_expr: mod_longident DOT LPAREN seq_expr . RPAREN
```
**Message**
*Missing closing parenthesis after the argument list of a module method call.  Add `)`.*

---

### 683
```
simple_expr: mod_longident DOT LBRACKETBAR . _*
```
**Message**
*After `[|` the parser expects either list elements or the closing token `|]`.  Supply one of those.*

---

### 684
```
simple_expr: mod_longident DOT LBRACKETBAR separated_or_terminated_nonempty_list_SEMI_expr_ . BARRBRACKET
```
**Message**
*The array expression started with `[|` must be terminated by `|]`.  Insert `|]`.*

---

### 685
```
simple_expr: mod_longident DOT LBRACKET . _*
```
**Message**
*After `[` a list element or the closing `]` is required.  Provide an element or close the list.*

---

### 686
```
simple_expr: mod_longident DOT LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACKET
```
**Message**
*Missing closing `]` for the list expression.  Add `]`.*

---

### 687
```
simple_expr: mod_longident DOT LBRACELESS . separated_or_terminated_nonempty_list_SEMI_object_expr_field_ GREATERRBRACE
```
**Message**
*After `{<` the parser expects object fields (or immediately `>}` to close an empty object).  Insert a field definition or `>}`.*

---

### 688
```
simple_expr: mod_longident DOT LBRACELESS separated_or_terminated_nonempty_list_SEMI_object_expr_field_ . GREATERRBRACE
```
**Message**
*Object expression started with `{< …` must be closed by `>}`.  Add the missing `>}`.*

---

### 689
```
simple_expr: mod_longident DOT LBRACE . record_expr_content RBRACE
```
**Message**
*After `{` a record field, a `}` (empty record) or a `with` clause is expected.  Provide a field or close the record.*

---

### 690
```
record_expr_content: simple_expr . WITH separated_or_terminated_nonempty_list_SEMI_record_expr_field_
```
**Message**
*`with` is required to start a record update after the base record expression.  Insert `with`.*

---

### 691
```
record_expr_content: simple_expr WITH . separated_or_terminated_nonempty_list_SEMI_record_expr_field_
```
**Message**
*At least one field must follow `with` in a record update.  Add a field definition (`label = expr`).*

---

### 692
```
simple_expr: mod_longident DOT LBRACE record_expr_content . RBRACE
```
**Message**
*Missing closing `}` for the record expression.  Add `}`.*

---

### 693 – four sub‑patterns

| Sub‑pattern | Message |
|-------------|---------|
| `constr_longident: mod_longident DOT . LPAREN COLONCOLON RPAREN` | *After a module path and a dot, the parser expects an identifier (or a parenthesised `::`).  Write the constructor name.* |
| `mk_longident_mod_longident_LIDENT_: mod_longident DOT . LIDENT` | *A module path followed by `.` must be continued with a lowercase identifier (a value).  Insert the identifier.* |
| `mk_longident_mod_longident_UIDENT_: mod_longident DOT . UIDENT` | *A module path followed by `.` must be continued with an uppercase identifier (a module).  Insert the identifier.* |
| `mk_longident_mod_longident_val_ident_: mod_longident DOT . val_ident` | *After `.` a value identifier is expected.  Provide the identifier.* |
| `simple_expr: mod_longident DOT . _*` | *A dot after a module path must be followed by a field name, a sub‑module, a method call, etc.  Insert the appropriate construct.* |

---

### 694 – two sub‑patterns

| Sub‑pattern | Message |
|-------------|---------|
| `reversed_labeled_tuple_body: FUNCTION . _*` | *`function` must be followed by an optional attribute list **and** at least one match case (`| pat -> expr`).  Add a match case.* |
| `reversed_labeled_tuple_body: LABEL simple_expr COMMA FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_` | *After the comma, `function` again requires a match‑case list (and optional attributes).  Supply `| …` clauses.* |

---

### 695 – two sub‑patterns

| Sub‑pattern | Message |
|-------------|---------|
| `reversed_labeled_tuple_body: FUNCTION ext . _*` | *`function` with an attribute (`%…`) still needs a match‑case list.  Provide one or more `| pat -> expr` clauses.* |
| `reversed_labeled_tuple_body: LABEL simple_expr COMMA FUNCTION ext . list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_` | *Same as above, but after the preceding label/comma.  Add the match‑case list.* |

---

### 696 – two sub‑patterns

| Sub‑pattern | Message |
|-------------|---------|
| `reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*` | *Even after attributes, `function` must be followed by a non‑empty list of match cases.  Write `| pat -> expr …`.* |
| `reversed_labeled_tuple_body: LABEL simple_expr COMMA FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_` | *A labelled tuple element ending with `function` (and attributes) still requires match cases.  Add them.* |

---

### 697 – three sub‑patterns

| Sub‑pattern | Message |
|-------------|---------|
| `reversed_labeled_tuple_body: FUNCTION . _*` | *`function` without any following match case is incomplete.  Insert at least one `| pat -> expr` clause.* |
| `reversed_labeled_tuple_body: FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COMMA FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_` | *After a comma, a second `function` also needs its own match‑case list.  Provide the list.* |
| `reversed_labeled_tuple_body: FUNCTION . _*` (duplicate) – same as first entry. |

---

### 698 – three sub‑patterns

| Sub‑pattern | Message |
|-------------|---------|
| `reversed_labeled_tuple_body: FUNCTION ext . _*` | *`function` with an attribute must still be followed by match cases.  Add `| …` clauses.* |
| `reversed_labeled_tuple_body: FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COMMA FUNCTION ext . list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_` | *The second `function` after the comma also needs its own match‑case list.* |
| `reversed_labeled_tuple_body: FUNCTION ext . _*` – same as first entry. |

---

### 699 – three sub‑patterns

| Sub‑pattern | Message |
|-------------|---------|
| `reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*` | *Even with attributes, `function` must be followed by a non‑empty list of match cases.  Add them.* |
| `reversed_labeled_tuple_body: FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COMMA FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_` | *After the comma a second `function` (with its own attributes) still requires its match‑case list.* |
| `reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*` – same as first entry. |
**Pattern 700**
```
match_case: pattern WHEN seq_expr . MINUSGREATER seq_expr
```
*Error:* after a guard expression (`WHEN …`) the parser expects the arrow `->` that separates the guard from the case body.

---

**Pattern 701**
```
match_case: pattern WHEN seq_expr MINUSGREATER . seq_expr
```
*Error:* a guarded match case must be followed by an expression (the case body). The parser found none after `->`.

---

**Pattern 702**
```
match_case: pattern MINUSGREATER . _*
```
*Error:* after the `->` of a match case an expression (or a sequence of expressions) is required.

---

**Pattern 703**
```
fun_expr: IF ext list_attribute_ seq_expr . _*
```
*Error:* after the condition of an `if` expression the keyword `then` is mandatory.

---

**Pattern 704**
```
fun_expr: IF ext list_attribute_ seq_expr THEN . _*
```
*Error:* after `then` an expression (the “then‑branch”) or a `function` keyword must appear.

---

**Pattern 705**
```
fun_expr: IF ext list_attribute_ seq_expr THEN FUNCTION . _*
reversed_labeled_tuple_body: FUNCTION . _*
```
*Error:* after `function` in an `if … then function` construct the parser expects either an optional extension marker (`%`) or an attribute list, or the first match case of the function.

---

**Pattern 706**
```
fun_expr: IF ext list_attribute_ seq_expr THEN FUNCTION ext . _*
reversed_labeled_tuple_body: FUNCTION ext . _*
```
*Error:* after the optional extension marker (`%`) following `function` an attribute list (or directly a match case) is required.

---

**Pattern 707**
```
fun_expr: IF ext list_attribute_ seq_expr THEN FUNCTION ext list_attribute_ . _*
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```
*Error:* after the attribute list that follows `function` the parser expects the first match case (the pattern list).

---

**Pattern 708**
```
fun_expr: IF ext list_attribute_ seq_expr THEN FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ ELSE . _*
```
*Error:* after the `else` keyword an expression (the “else‑branch”) or a `function` definition must follow.

---

**Pattern 709**
```
fun_expr: IF ext list_attribute_ seq_expr THEN FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ ELSE FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . _*
```
*Error:* after the `else` keyword the parser saw `function` but then expected an optional extension marker (`%`) or an attribute list before the first match case.

---

**Pattern 710**
```
fun_expr: IF ext list_attribute_ seq_expr THEN FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ ELSE FUNCTION ext . list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext . _*
```
*Error:* after `function %` in the else‑branch an attribute list (or directly a match case) is required.

---

**Pattern 711**
```
fun_expr: IF ext list_attribute_ seq_expr THEN FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ ELSE FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```
*Error:* after the attribute list of the `else function` the parser expects the first match case.

---

**Pattern 712**
```
fun_expr: IF ext list_attribute_ seq_expr THEN fun_expr ELSE . _*
```
*Error:* after `else` an expression (or a `function` definition) is required.

---

**Pattern 713**
```
fun_expr: IF ext list_attribute_ seq_expr THEN fun_expr ELSE FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . _*
```
*Error:* after `else function` the parser expects an optional extension marker (`%`) or attribute list before the match cases.

---

**Pattern 714**
```
fun_expr: IF ext list_attribute_ seq_expr THEN fun_expr ELSE FUNCTION ext . list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext . _*
```
*Error:* after `else function %` an attribute list (or directly a match case) must follow.

---

**Pattern 715**
```
fun_expr: IF ext list_attribute_ seq_expr THEN fun_expr ELSE FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```
*Error:* after the attribute list of the `else function` the parser expects the first match case.

---

**Pattern 716**
```
simple_expr: simple_expr DOT LPAREN seq_expr . RPAREN
```
*Error:* a closing parenthesis `)` is required after the expression inside `(... )`.

---

**Pattern 717**
```
simple_expr: simple_expr DOT LBRACKET . seq_expr RBRACKET
```
*Error:* an expression is required after the opening bracket `[` in an array‑indexing expression.

---

**Pattern 718**
```
simple_expr: simple_expr DOT LBRACKET seq_expr . RBRACKET
```
*Error:* a closing bracket `]` is required after the index expression.

---

**Pattern 719**
```
simple_expr: simple_expr DOT LBRACE . seq_expr RBRACE
```
*Error:* after `{` an expression is expected in a record‑field or block expression.

---

**Pattern 720**
```
simple_expr: simple_expr DOT LBRACE seq_expr . RBRACE
```
*Error:* a closing brace `}` is required after the expression inside `{ … }`.

---

**Pattern 721**
```
mk_longident_mod_longident_LIDENT_:   mod_longident . DOT LIDENT
mk_longident_mod_longident_UIDENT_:   mod_longident . DOT UIDENT
simple_expr: simple_expr DOT mod_longident . _*
```
*Error:* after a dot following a module path, an identifier (field or sub‑module name) must appear.

---

**Pattern 722**
```
simple_expr: simple_expr DOT mod_longident DOTOP . _*
```
*Error:* after the operator `.( … )` (token `DOTOP`) the parser expects one of `(`, `[` or `{` to start the argument list.

---

**Pattern 723**
```
simple_expr: simple_expr DOT mod_longident DOTOP LPAREN . separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN
```
*Error:* an expression is required after the opening parenthesis in `.( … )`.

---

**Pattern 724**
```
simple_expr: simple_expr DOT mod_longident DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ . RPAREN
```
*Error:* a closing parenthesis `)` is required after the argument list of `.( … )`.

---

**Pattern 725**
```
simple_expr: simple_expr DOT mod_longident DOTOP LBRACKET . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET
```
*Error:* an expression is required after the opening bracket in `.[ … ]`.

---

**Pattern 726**
```
simple_expr: simple_expr DOT mod_longident DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACKET
```
*Error:* a closing bracket `]` is required after the index expression in `.[ … ]`.

---

**Pattern 727**
```
simple_expr: simple_expr DOT mod_longident DOTOP LBRACE . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE
```
*Error:* an expression is required after the opening brace in `. { … }`.

---

**Pattern 728**
```
simple_expr: simple_expr DOT mod_longident DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACE
```
*Error:* a closing brace `}` is required after the body of `. { … }`.

---

**Pattern 729**
```
simple_expr: DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ . RPAREN
```
*Error:* a closing parenthesis `)` is required after the argument list of `.( … )`.

---

**Pattern 730**
```
simple_expr: DOTOP LBRACKET . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET
```
*Error:* an expression is required after the opening bracket in `.[ … ]`.

---

**Pattern 731**
```
simple_expr: DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACKET
```
*Error:* a closing bracket `]` is required after the index expression in `.[ … ]`.

---

**Pattern 732**
```
simple_expr: DOTOP LBRACE . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE
```
*Error:* an expression is required after the opening brace in `. { … }`.

---

**Pattern 733**
```
simple_expr: DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACE
```
*Error:* a closing brace `}` is required after the body of `. { … }`.

---

**Pattern 734**
```
simple_expr: BEGIN ext list_attribute_ seq_expr . END
```
*Error:* a closing `end` keyword is required to terminate a `begin … end` block.

---

**Pattern 735**
```
simple_expr: LBRACE record_expr_content . RBRACE
```
*Error:* a closing brace `}` is required after the fields of a record literal.

---

**Pattern 736**
```
option_preceded_EQUAL_expr__: EQUAL FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . _*
```
*Error:* after the keyword `function` an optional extension marker (`%`), an attribute list, or the first match case must appear.

---

**Pattern 737**
```
option_preceded_EQUAL_expr__: EQUAL FUNCTION ext . list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext . _*
```
*Error:* after `function %` an attribute list (or directly a match case) is required.

---

**Pattern 738**
```
option_preceded_EQUAL_expr__: EQUAL FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```
*Error:* after the attribute list following `function %` the parser expects the first match case.

---

**Pattern 739**
```
simple_expr: LBRACELESS separated_or_terminated_nonempty_list_SEMI_object_expr_field_ . GREATERRBRACE
```
*Error:* a closing `}` (token `GREATERRBRACE`) is required to terminate a polymorphic record literal.

---

**Pattern 740**
```
simple_expr: LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACKET
```
*Error:* a closing bracket `]` is required after the elements of a list/array literal.

---

**Pattern 741**
```
floating_attribute: LBRACKETATATAT . attr_id attr_payload RBRACKET
```
*Error:* after `[@@@` an attribute identifier must follow.

---

**Pattern 742**
```
floating_attribute: LBRACKETATATAT attr_id . attr_payload RBRACKET
```
*Error:* after the attribute identifier an attribute payload (a colon‑type, a structure, etc.) is required.

---

**Pattern 743**
```
structure_item: INCLUDE . ext list_attribute_ module_expr list_post_item_attribute_
```
*Error:* after `include` the parser expects an optional extension marker (`%`), an attribute list, and then a module expression to include.

---

**Pattern 744**
```
structure_item: INCLUDE ext . list_attribute_ module_expr list_post_item_attribute_
```
*Error:* after `include %` an attribute list (or directly a module expression) is required.

---

**Pattern 745**
```
structure_item: INCLUDE ext list_attribute_ . module_expr list_post_item_attribute_
```
*Error:* after the attribute list following `include %` a module expression must appear.

---

**Pattern 746**
```
primitive_declaration: EXTERNAL . ext list_attribute_ val_ident COLON possibly_poly_core_type_ EQUAL nonempty_list_raw_string_ list_post_item_attribute_
```
*Error:* after `external` the parser expects an optional extension marker (`%`), an attribute list, a value identifier, `:` and the type, then `=` and a string literal.

---

**Pattern 747**
```
primitive_declaration: EXTERNAL ext . list_attribute_ val_ident COLON possibly_poly_core_type_ EQUAL nonempty_list_raw_string_ list_post_item_attribute_
```
*Error:* after `external %` an attribute list (or directly the identifier) is required.

---

**Pattern 748**
```
primitive_declaration: EXTERNAL ext list_attribute_ . val_ident COLON possibly_poly_core_type_ EQUAL nonempty_list_raw_string_ list_post_item_attribute_
```
*Error:* after the attribute list following `external %` a value identifier is required.

---

**Pattern 749**
```
primitive_declaration: EXTERNAL ext list_attribute_ val_ident . COLON possibly_poly_core_type_ EQUAL nonempty_list_raw_string_ list_post_item_attribute_
```
*Error:* after the external name the parser expects a colon `:` introducing the type of the external value.
```
/primitive_declaration: EXTERNAL ext list_attribute_ val_ident COLON . possibly_poly_core_type_ EQUAL nonempty_list_raw_string_ list_post_item_attribute_
Expected a type after ‘:’ in an external declaration.
```

```
/primitive_declaration: EXTERNAL ext list_attribute_ val_ident COLON possibly_poly_core_type_ . EQUAL nonempty_list_raw_string_ list_post_item_attribute_
Expected ‘=’ followed by one or more string literals after the type in an external declaration.
```

```
/primitive_declaration: EXTERNAL ext list_attribute_ val_ident COLON possibly_poly_core_type_ EQUAL . nonempty_list_raw_string_ list_post_item_attribute_
Expected at least one string literal after ‘=’ in an external declaration.
```

```
/sig_exception_declaration: EXCEPTION . ext list_attribute_ constr_ident generalized_constructor_arguments list_attribute_ list_post_item_attribute_
/str_exception_declaration: EXCEPTION . ext list_attribute_ constr_ident EQUAL constr_longident list_attribute_ list_post_item_attribute_
Expected an exception name after the keyword ‘exception’.
```

```
/sig_exception_declaration: EXCEPTION ext . list_attribute_ constr_ident generalized_constructor_arguments list_attribute_ list_post_item_attribute_
/str_exception_declaration: EXCEPTION ext . list_attribute_ constr_ident EQUAL constr_longident list_attribute_ list_post_item_attribute_
Expected an exception name after the optional attributes following ‘exception’.
```

```
/sig_exception_declaration: EXCEPTION ext list_attribute_ . constr_ident generalized_constructor_arguments list_attribute_ list_post_item_attribute_
/str_exception_declaration: EXCEPTION ext list_attribute_ . constr_ident EQUAL constr_longident list_attribute_ list_post_item_attribute_
Expected an exception name after the attributes.
```

```
/constr_extra_nonprefix_ident: LPAREN . RPAREN
Expected ‘)’ to close the empty constructor identifier ‘()’.
```

```
/constr_ident: LPAREN COLONCOLON . RPAREN
Expected ‘)’ after ‘::’ inside the parenthesised constructor identifier.
```

```
/constr_extra_nonprefix_ident: LBRACKET . RBRACKET
Expected ‘]’ to close the empty list constructor identifier ‘[]’.
```

```
/generalized_constructor_arguments: OF . constructor_arguments
Expected constructor arguments after the keyword ‘of’.
```

```
/constructor_arguments: LBRACE . label_declarations RBRACE
Expected a label declaration after ‘{’ in a record constructor argument.
```

```
/label_declaration: mutable_flag . LIDENT COLON possibly_poly_core_type_no_attr_ list_attribute_
/label_declaration_semi: mutable_flag . LIDENT COLON possibly_poly_core_type_no_attr_ list_attribute_ SEMI list_attribute_
Expected a field name (identifier) after an optional ‘mutable’ flag in a record type.
```

```
/label_declaration: mutable_flag LIDENT . COLON possibly_poly_core_type_no_attr_ list_attribute_
/label_declaration_semi: mutable_flag LIDENT . COLON possibly_poly_core_type_no_attr_ list_attribute_ SEMI list_attribute_
Expected ‘:’ after the field name in a record type.
```

```
/label_declaration: mutable_flag LIDENT COLON . possibly_poly_core_type_no_attr_ list_attribute_
/label_declaration_semi: mutable_flag LIDENT COLON . possibly_poly_core_type_no_attr_ list_attribute_ SEMI list_attribute_
Expected a type after ‘:’ in a record field declaration.
```

```
/possibly_poly_core_type_no_attr_: reversed_nonempty_llist_typevar_ . DOT alias_type
Expected ‘.’ followed by a type after the list of type variables in a polytype.
```

```
/possibly_poly_core_type_no_attr_: reversed_nonempty_llist_typevar_ DOT . alias_type
Expected a type after the dot in a polytype.
```

```
/constructor_arguments: LBRACE label_declarations . RBRACE
Expected ‘}’ to close the record constructor argument.
```

```
/reversed_separated_nonempty_llist_STAR_atomic_type_ . STAR atomic_type
Expected ‘*’ and another type after the preceding type in a constructor argument.
```

```
/reversed_separated_nonempty_llist_STAR_atomic_type_ STAR . atomic_type
Expected a type after ‘*’ in a constructor argument.
```

```
/str_exception_declaration: EXCEPTION ext list_attribute_ constr_ident EQUAL . constr_longident list_attribute_ list_post_item_attribute_
Expected a constructor name after ‘=’ in an exception definition.
```

```
/constr_longident: LPAREN . COLONCOLON RPAREN
Expected ‘)’ after the ‘::’ inside a parenthesised constructor long identifier.
```

```
/constr_longident: mod_longident DOT . LPAREN COLONCOLON RPAREN
Expected an identifier after the dot in a qualified constructor long identifier.
```

```
/generalized_constructor_arguments: COLON . _*
Expected something (type, arguments, or ‘:>’) after ‘:’ in a constructor argument list.
```

```
/generalized_constructor_arguments: COLON reversed_nonempty_llist_typevar_ . _*
Expected something after ‘:’ and the type‑variable list in a constructor argument.
```

```
/generalized_constructor_arguments: COLON reversed_nonempty_llist_typevar_ DOT . _*
Expected something after the dot following the type‑variable list in a constructor argument.
```

```
/generalized_constructor_arguments: COLON reversed_nonempty_llist_typevar_ DOT constructor_arguments . MINUSGREATER atomic_type
Expected ‘.’ before ‘->’ in a constructor result type.
```

```
/generalized_constructor_arguments: COLON reversed_nonempty_llist_typevar_ DOT constructor_arguments MINUSGREATER . atomic_type
Expected the result type after ‘->’ in a constructor definition.
```

```
/generalized_constructor_arguments: COLON constructor_arguments . MINUSGREATER atomic_type
Expected ‘.’ before ‘->’ in a constructor definition.
```

```
/generalized_constructor_arguments: COLON constructor_arguments MINUSGREATER . atomic_type
Expected the result type after ‘->’ in a constructor definition.
```

```
/open_description: OPEN . _*
Expected a module name after the keyword ‘open’.
```

```
/open_description: OPEN BANG . ext list_attribute_ mod_ext_longident list_post_item_attribute_
Expected a module name after ‘open !’.
```

```
/open_description: OPEN BANG ext . list_attribute_ mod_ext_longident list_post_item_attribute_
Expected a module name after ‘open !’ and its attribute list.
```

```
/open_description: OPEN BANG ext list_attribute_ . mod_ext_longident list_post_item_attribute_
Expected a module name after ‘open !’ and its attributes.
```

```
/open_description: OPEN ext . list_attribute_ mod_ext_longident list_post_item_attribute_
Expected a module name after ‘open %’ (or any other attribute).
```

```
/open_description: OPEN ext list_attribute_ . mod_ext_longident list_post_item_attribute_
Expected a module name after ‘open’ followed by attributes.
```

```
/module_subst: MODULE . ext list_attribute_ UIDENT COLONEQUAL mod_ext_longident list_post_item_attribute_
/module_type_declaration: MODULE . TYPE ext list_attribute_ ident option_preceded_EQUAL_module_type__ list_post_item_attribute_
/module_type_subst: MODULE . TYPE ext list_attribute_ ident COLONEQUAL module_type list_post_item_attribute_
/signature_item: MODULE . _*
Expected a module name (possibly preceded by attributes) after the keyword ‘module’.
```

```
/module_type_declaration: MODULE TYPE . ext list_attribute_ ident option_preceded_EQUAL_module_type__ list_post_item_attribute_
/module_type_subst: MODULE TYPE . ext list_attribute_ ident COLONEQUAL module_type list_post_item_attribute_
Expected optional attributes after ‘module type’.
```

```
/module_type_declaration: MODULE TYPE ext . list_attribute_ ident option_preceded_EQUAL_module_type__ list_post_item_attribute_
/module_type_subst: MODULE TYPE ext . list_attribute_ ident COLONEQUAL module_type list_post_item_attribute_
Expected attributes (or the identifier) after ‘module type %’.
```

```
/module_type_declaration: MODULE TYPE ext list_attribute_ . ident option_preceded_EQUAL_module_type__ list_post_item_attribute_
/module_type_subst: MODULE TYPE ext list_attribute_ . ident COLONEQUAL module_type list_post_item_attribute_
Expected an identifier after the attribute list in a ‘module type’ declaration.
```

```
/module_type_subst: MODULE TYPE ext list_attribute_ ident COLONEQUAL . module_type list_post_item_attribute_
Expected a module type after ‘:=’ in a module‑type substitution.
```

```
/module_subst: MODULE . ext list_attribute_ UIDENT COLONEQUAL mod_ext_longident list_post_item_attribute_
/signature_item: MODULE ext . _*
Expected optional attributes (or a module name) after the keyword ‘module’.
```

```
/module_type_declaration: MODULE TYPE ext . list_attribute_ ident option_preceded_EQUAL_module_type__ list_post_item_attribute_
/module_type_subst: MODULE TYPE ext . list_attribute_ ident COLONEQUAL module_type list_post_item_attribute_
Expected optional attributes after ‘module type’.
```

```
/module_type_declaration: MODULE TYPE ext list_attribute_ . ident option_preceded_EQUAL_module_type__ list_post_item_attribute_
/module_type_subst: MODULE TYPE ext list_attribute_ . ident COLONEQUAL module_type list_post_item_attribute_
Expected an identifier after the attribute list in a ‘module type’ declaration.
```

```
/module_subst: MODULE ext . list_attribute_ UIDENT COLONEQUAL mod_ext_longident list_post_item_attribute_
/signature_item: MODULE ext list_attribute_ . _*
Expected optional attributes (or a module name) after ‘module %’.
```

```
/module_subst: MODULE ext list_attribute_ . UIDENT COLONEQUAL mod_ext_longident list_post_item_attribute_
Expected a module name after the attribute list in a module substitution.
```

```
/module_subst: MODULE ext list_attribute_ UIDENT COLONEQUAL . mod_ext_longident list_post_item_attribute_
Expected a module path after ‘:=’ in a module substitution.
```

```
/signature_item: MODULE ext list_attribute_ REC . module_name COLON module_type list_post_item_attribute_ list_and_module_declaration_
Expected a module name after ‘module rec’.
```

```
/signature_item: MODULE ext list_attribute_ REC module_name . COLON module_type list_post_item_attribute_ list_and_module_declaration_
Expected ‘:’ after the module name in a recursive module declaration.
```

```
/signature_item: MODULE ext list_attribute_ REC module_name COLON . module_type list_post_item_attribute_ list_and_module_declaration_
Expected a module type after ‘:’ in a recursive module declaration.
```

```
/list_and_module_declaration_: AND . list_attribute_ module_name COLON module_type list_post_item_attribute_ list_and_module_declaration_
Expected optional attributes (or a module name) after ‘and’.
```

```
/list_and_module_declaration_: AND list_attribute_ . module_name COLON module_type list_post_item_attribute_ list_and_module_declaration_
Expected a module name after ‘and’ and its attributes.
```

```
/list_and_module_declaration_: AND list_attribute_ module_name . COLON module_type list_post_item_attribute_ list_and_module_declaration_
Expected ‘:’ after the module name in a module list.
```

```
/list_and_module_declaration_: AND list_attribute_ module_name COLON . module_type list_post_item_attribute_ list_and_module_declaration_
Expected a module type after ‘:’ in a module list.
```

```
/signature_item: MODULE ext list_attribute_ module_name . _*
Expected ‘=’, ‘:’, or ‘=’ after the module name in a signature item.
```

```
/signature_item: MODULE ext list_attribute_ module_name EQUAL . mod_longident list_post_item_attribute_
Expected a module path after ‘=’ in a module alias.
```

```
/module_declaration_body: COLON . module_type
Expected a module type after ‘:’ in a module definition.
```

```
/module_declaration_body: functor_arg . module_declaration_body
Expected another functor argument or the module body after a functor argument.
```

```
/signature_item: INCLUDE . ext list_attribute_ module_type list_post_item_attribute_
Expected a module type after the keyword ‘include’.
```

```
/signature_item: INCLUDE ext . list_attribute_ module_type list_post_item_attribute_
Expected a module type after ‘include’ and its attribute.
```

```
/signature_item: INCLUDE ext list_attribute_ . module_type list_post_item_attribute_
Expected a module type after ‘include’ and its attribute list.
```

```
/sig_exception_declaration: EXCEPTION . ext list_attribute_ constr_ident generalized_constructor_arguments list_attribute_ list_post_item_attribute_
Expected an attribute or an exception name after ‘exception’.
```

```
/sig_exception_declaration: EXCEPTION ext . list_attribute_ constr_ident generalized_constructor_arguments list_attribute_ list_post_item_attribute_
Expected attributes after ‘exception %’ before the name.
```

```
/sig_exception_declaration: EXCEPTION ext list_attribute_ . constr_ident generalized_constructor_arguments list_attribute_ list_post_item_attribute_
Expected an exception name after ‘exception’ and its attributes.
```

```
/class_type_declarations: CLASS . TYPE ext list_attribute_ virtual_flag formal_class_parameters LIDENT EQUAL class_signature list_post_item_attribute_ list_and_class_type_declaration_
Expected the keyword ‘type’ after ‘class’.
```

```
/class_type_declarations: CLASS TYPE . ext list_attribute_ virtual_flag formal_class_parameters LIDENT EQUAL class_signature list_post_item_attribute_ list_and_class_type_declaration_
Expected optional attributes after ‘class type’.
```

```
/class_type_declarations: CLASS TYPE ext . list_attribute_ virtual_flag formal_class_parameters LIDENT EQUAL class_signature list_post_item_attribute_ list_and_class_type_declaration_
Expected attributes after ‘class type %’.
```

```
/class_type_declarations: CLASS TYPE ext list_attribute_ . virtual_flag formal_class_parameters LIDENT EQUAL class_signature list_post_item_attribute_ list_and_class_type_declaration_
Expected a virtual flag (or other flag) after ‘class type’ attributes.
```

```
/class_type_declarations: CLASS TYPE ext list_attribute_ virtual_flag . formal_class_parameters LIDENT EQUAL class_signature list_post_item_attribute_ list_and_class_type_declaration_
Expected class type parameters after the optional ‘virtual’ flag.
```

```
/formal_class_parameters: LBRACKET . reversed_separated_nonempty_llist_COMMA_type_parameter_ RBRACKET
Expected at least one type parameter after ‘[’.
```

```
/formal_class_parameters: LBRACKET reversed_separated_nonempty_llist_COMMA_type_parameter_ . RBRACKET
Expected ‘]’ to close the class type parameter list.
```

```
/class_type_declarations: CLASS TYPE ext list_attribute_ virtual_flag formal_class_parameters . LIDENT EQUAL class_signature list_post_item_attribute_ list_and_class_type_declaration_
Expected the class name after the type‑parameter list.
```

```
/class_type_declarations: CLASS TYPE ext list_attribute_ virtual_flag formal_class_parameters LIDENT . EQUAL class_signature list_post_item_attribute_ list_and_class_type_declaration_
Expected ‘=’ after the class name in a class type declaration.
```
Below are human‑readable diagnostics that explain **exactly what the parser was expecting** at the point marked by “`. *`” in each pattern.
The wording follows the style of the examples you gave (a short description of the missing construct, together with the concrete keyword that should have appeared).

---

### 820 – `class_type_declarations`
```
/class_type_declarations: CLASS TYPE ext list_attribute_ virtual_flag formal_class_parameters LIDENT EQUAL . class_signature …
```
**Message** – After the “`=`” in a `class type` declaration the parser expects a *class signature* (the body of the class type).

> **Expected a class signature after `=` in a `class type` declaration.**

---

### 821 – `class_signature` (object header only)
```
/class_signature: OBJECT . list_attribute_ class_self_type list_text_csig_class_sig_field__ END
```
**Message** – After the keyword `object` the parser needs an optional attribute list, the optional self‑type and then the list of class‑signature fields.

> **Expected an optional attribute list, a self‑type (or nothing), and class‑signature fields after `object`.**

---

### 822 – `class_signature` (object header with attributes)
```
/class_signature: OBJECT list_attribute_ . class_self_type list_text_csig_class_sig_field__ END
```
**Message** – After the attribute list that follows `object` the parser expects the optional self‑type (or nothing) and then the class‑signature fields.

> **Expected a self‑type (or nothing) followed by class‑signature fields after the attribute list on `object`.**

---

### 823 – `class_self_type` (opening parenthesis)
```
/class_self_type: LPAREN . core_type RPAREN
```
**Message** – Inside an `object … end` the parentheses must contain a core type.

> **Expected a core type inside the parentheses of the object self‑type.**

---

### 824 – `class_self_type` (after the core type)
```
/class_self_type: LPAREN core_type . RPAREN
```
**Message** – The closing parenthesis of the object self‑type is missing.

> **Expected `)` to close the object self‑type.**

---

### 825 – `class_signature` (object with fields, missing field)
```
/class_signature: OBJECT list_attribute_ class_self_type . list_text_csig_class_sig_field__ END
```
**Message** – After the optional self‑type the parser expects at least one class‑signature field (or `END`).

> **Expected a class‑signature field (e.g. `val`, `method`, `inherit`, …) or `END`.**

---

### 826 – `class_sig_field` (value field, missing name)
```
/class_sig_field: VAL . list_attribute_ mutable_virtual_flags LIDENT COLON core_type …
```
**Message** – After the keyword `val` an optional attribute list may appear, then the optional mutability/virtual flags and the identifier of the value.

> **Expected an attribute list (or nothing), optional `mutable`/`virtual`, and a value name after `val`.**

---

### 827 – `class_sig_field` (value field, missing attribute list)
```
/class_sig_field: VAL list_attribute_ . mutable_virtual_flags LIDENT COLON core_type …
```
**Message** – After the attribute list that follows `val` the parser expects the optional mutability/virtual flags.

> **Expected optional `mutable`/`virtual` flags (or nothing) after the attribute list on `val`.**

---

### 828 – `class_sig_field` (value field, missing identifier)
```
/class_sig_field: VAL list_attribute_ mutable_virtual_flags . LIDENT COLON core_type …
```
**Message** – After the optional mutability/virtual flags the parser needs the identifier of the value.

> **Expected the name of the value after the mutability/virtual flags.**

---

### 829 – `class_sig_field` (value field, missing colon)
```
/class_sig_field: VAL list_attribute_ mutable_virtual_flags LIDENT . COLON core_type …
```
**Message** – After the value name the colon that separates the name from its type is missing.

> **Expected `:` after the value name.**

---

### 830 – `class_sig_field` (value field, missing type)
```
/class_sig_field: VAL list_attribute_ mutable_virtual_flags LIDENT COLON . core_type …
```
**Message** – After `:` the parser expects the core type of the value.

> **Expected a core type after `:` in the value declaration.**

---

### 831 – `class_sig_field` (method field, missing name)
```
/class_sig_field: METHOD . list_attribute_ private_virtual_flags LIDENT COLON …
```
**Message** – After the keyword `method` an optional attribute list may appear, then optional visibility flags and the method name.

> **Expected an attribute list (or nothing), optional visibility flags, and a method name after `method`.**

---

### 832 – `class_sig_field` (method field, missing attribute list)
```
/class_sig_field: METHOD list_attribute_ . private_virtual_flags LIDENT COLON …
```
**Message** – After the attribute list that follows `method` the parser expects the optional visibility flags.

> **Expected optional `private`/`virtual` flags (or nothing) after the attribute list on `method`.**

---

### 833 – `class_sig_field` (method field, missing visibility flags)
```
/class_sig_field: METHOD list_attribute_ private_virtual_flags . LIDENT COLON …
```
**Message** – After the visibility flags the parser expects the method identifier.

> **Expected a method name after the visibility flags.**

---

### 834 – `class_sig_field` (method field, missing colon)
```
/class_sig_field: METHOD list_attribute_ private_virtual_flags LIDENT . COLON possibly_poly_core_type_ …
```
**Message** – After the method name a colon is required before the method’s type.

> **Expected `:` after the method name.**

---

### 835 – `class_sig_field` (method field, missing type)
```
/class_sig_field: METHOD list_attribute_ private_virtual_flags LIDENT COLON . possibly_poly_core_type_ …
```
**Message** – After the colon the parser expects the method’s (possibly polymorphic) type.

> **Expected a type after `:` in the method declaration.**

---

### 836 – `class_sig_field` (inherit, missing attribute list)
```
/class_sig_field: INHERIT . list_attribute_ class_signature …
```
**Message** – After `inherit` an optional attribute list may appear, then the class signature to inherit from.

> **Expected an attribute list (or nothing) after `inherit`.**

---

### 837 – `class_sig_field` (inherit with attribute list, missing signature)
```
/class_sig_field: INHERIT list_attribute_ . class_signature …
```
**Message** – After the attribute list that follows `inherit` the parser expects the class signature being inherited.

> **Expected a class signature after the attribute list on `inherit`.**

---

### 838 – `class_signature` (illegal `let` at top level)
```
/class_signature: LET . _*
```
**Message** – `let` is not allowed directly inside a class signature; a class signature must consist of fields or be closed with `end`.

> **`let` cannot appear at the top level of a class signature. Expected a class field (`val`, `method`, `inherit`, …) or `END`.**

---

### 839 – `class_signature` (illegal `let open` at top level)
```
/class_signature: LET OPEN . _*
```
**Message** – `let open` is also illegal inside a class signature.

> **`let open` cannot appear at the top level of a class signature. Expected a class field or `END`.**

---

### 840 – `class_signature` (`let open !` without following module)
```
/class_signature: LET OPEN BANG . list_attribute_ mod_longident IN class_signature
```
**Message** – After `let open !` the parser expects an attribute list and a module identifier.

> **Expected an attribute list (or nothing) and a module name after `let open !`.**

---

### 841 – `class_signature` (`let open !` with attributes but missing module)
```
/class_signature: LET OPEN BANG list_attribute_ . mod_longident IN class_signature
```
**Message** – After the attribute list that follows `let open !` the parser needs the module identifier.

> **Expected a module name after the attribute list on `let open !`.**

---

### 842 – `class_signature` (`let open ! X . X`)
*(also the `mod_longident . DOT UIDENT` rule)*
```
/class_signature: LET OPEN BANG list_attribute_ mod_longident . IN class_signature
mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT
```
**Message** – After the qualified module name (`X.Y`) the parser expects a `.` followed by another identifier (the final component of the long identifier).

> **Expected `.` and the final identifier of the module path after `let open !`.**

---

### 843 – `class_signature` (`let open ! X in .`)
```
/class_signature: LET OPEN BANG list_attribute_ mod_longident IN . class_signature
```
**Message** – After the `in` keyword the parser expects the continuation of the class signature.

> **Expected a class signature after `in`.**

---

### 844 – `class_signature` (open‑bracket for a class type, missing type list)
```
/class_signature: LBRACKET . reversed_separated_nonempty_llist_COMMA_core_type_ RBRACKET clty_longident
```
**Message** – Inside a class type, an opening `[` must be followed by a (possibly empty) comma‑separated list of core types.

> **Expected a list of core types (or `]`) after `[` in a class type.**

---

### 845 – `class_signature` (closing `]` after a core type, missing `]`)
```
/class_signature: LBRACKET reversed_separated_nonempty_llist_COMMA_core_type_ . RBRACKET clty_longident
```
**Message** – After the comma‑separated core‑type list the parser expects the closing `]`.

> **Expected `]` to close the type list in the class type.**

---

### 846 – `class_signature` (missing the class‑type identifier after `]`)
```
/class_signature: LBRACKET reversed_separated_nonempty_llist_COMMA_core_type_ RBRACKET . clty_longident
```
**Message** – After the closing `]` the parser expects the class‑type identifier (`clty_longident`).

> **Expected a class‑type identifier after the type list (`]`).**

---

### 847 – `reversed_separated_nonempty_llist_COMMA_core_type_` (missing core type after a comma)
```
/reversed_separated_nonempty_llist_COMMA_core_type_: reversed_separated_nonempty_llist_COMMA_core_type_ COMMA . core_type
```
**Message** – A comma in a type list must be followed by another core type.

> **Expected a core type after `,`.**

---

### 848 – `class_signature` (`let open` with attributes, missing module)
```
/class_signature: LET OPEN . list_attribute_ mod_longident IN class_signature
```
**Message** – After `let open` the parser expects an optional attribute list and a module identifier.

> **Expected an attribute list (or nothing) and a module name after `let open`.**

---

### 849 – `class_signature` (`let open` with attributes, missing `.`)
```
/class_signature: LET OPEN list_attribute_ mod_longident . IN class_signature
mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT
```
**Message** – After a qualified module name (`M.N`) the parser expects the dot that continues the long identifier.

> **Expected `.` and the final identifier of the module path after `let open`.**

---

### 850 – `class_signature` (`let open … in` without following signature)
```
/class_signature: LET OPEN list_attribute_ mod_longident IN . class_signature
```
**Message** – After `in` the parser expects the rest of the class signature.

> **Expected a class signature after `in`.**

---

### 851 – `class_sig_field` (constraint, missing attribute list)
```
/class_sig_field: CONSTRAINT . list_attribute_ constrain_field …
```
**Message** – After `constraint` an optional attribute list may appear, then the constraint definition.

> **Expected an attribute list (or nothing) after `constraint`.**

---

### 852 – `class_sig_field` (constraint with attribute list, missing constraint)
```
/class_sig_field: CONSTRAINT list_attribute_ . constrain_field …
```
**Message** – After the attribute list that follows `constraint` the parser expects the actual constraint (`core_type = core_type`).

> **Expected a constraint (`core_type = core_type`) after the attribute list.**

---

### 853 – `constrain_field` (missing `=`)
```
/constrain_field: core_type . EQUAL core_type
```
**Message** – Inside a constraint the `=` that separates the two types is missing.

> **Expected `=` between the two core types of the constraint.**

---

### 854 – `constrain_field` (missing right‑hand type)
```
/constrain_field: core_type EQUAL . core_type
```
**Message** – After the `=` the parser expects the right‑hand side core type of the constraint.

> **Expected a core type after `=` in the constraint.**

---

### 855 – `class_signature` (missing fields before `END`)
```
/class_signature: OBJECT list_attribute_ class_self_type list_text_csig_class_sig_field__ . END
```
**Message** – After the optional self‑type and (possibly empty) list of fields the parser expects `END`.

> **Expected `END` to close the object class signature.**

---

### 856 – `list_and_class_type_declaration_` (missing “and” clause)
```
/list_and_class_type_declaration_: AND . list_attribute_ virtual_flag formal_class_parameters LIDENT EQUAL class_signature …
```
**Message** – After the `and` keyword the parser expects the optional attribute list that begins the next class‑type declaration.

> **Expected an attribute list (or nothing) after `and` in a series of class‑type declarations.**

---

### 857 – `list_and_class_type_declaration_` (missing attribute list after `and`)
```
/list_and_class_type_declaration_: AND list_attribute_ . virtual_flag formal_class_parameters LIDENT EQUAL class_signature …
```
**Message** – After the attribute list that follows `and` the parser expects the optional `virtual` flag.

> **Expected optional `virtual` flag (or nothing) after the attribute list on `and`.**

---

### 858 – `list_and_class_type_declaration_` (missing virtual flag)
```
/list_and_class_type_declaration_: AND list_attribute_ virtual_flag . formal_class_parameters LIDENT EQUAL class_signature …
```
**Message** – After the (optional) `virtual` keyword the parser expects the formal class parameters.

> **Expected formal class parameters after the optional `virtual` flag.**

---

### 859 – `list_and_class_type_declaration_` (missing formal parameters)
```
/list_and_class_type_declaration_: AND list_attribute_ virtual_flag formal_class_parameters . LIDENT EQUAL class_signature …
```
**Message** – After the optional formal class parameters the parser expects the class name (`LIDENT`).

> **Expected the class name after the formal class parameters.**

---

### 860 – `list_and_class_type_declaration_` (missing `=`)
```
/list_and_class_type_declaration_: AND list_attribute_ virtual_flag formal_class_parameters LIDENT . EQUAL class_signature …
```
**Message** – After the class name the `=` that introduces the class signature is missing.

> **Expected `=` after the class name.**

---

### 861 – `list_and_class_type_declaration_` (missing class signature after `=`)
```
/list_and_class_type_declaration_: AND list_attribute_ virtual_flag formal_class_parameters LIDENT EQUAL . class_signature …
```
**Message** – After `=` the parser expects the class signature that defines the class type.

> **Expected a class signature after `=`.**

---

### 862 – `signature_item` (class declaration, missing attribute list)
```
/signature_item: CLASS ext . list_attribute_ virtual_flag formal_class_parameters LIDENT COLON class_type …
```
**Message** – After the keyword `class` (possibly preceded by an extension) the parser expects an optional attribute list.

> **Expected an attribute list (or nothing) after `class`.**

---

### 863 – `signature_item` (class declaration, missing virtual flag)
```
/signature_item: CLASS ext list_attribute_ . virtual_flag formal_class_parameters LIDENT COLON class_type …
```
**Message** – After the optional attribute list the parser expects either `virtual` or nothing.

> **Expected optional `virtual` flag (or nothing) after the attribute list.**

---

### 864 – `signature_item` (class declaration, missing formal parameters)
```
/signature_item: CLASS ext list_attribute_ virtual_flag . formal_class_parameters LIDENT COLON class_type …
```
**Message** – After the optional `virtual` flag the parser expects the formal class parameters (or nothing).

> **Expected formal class parameters (or nothing) after the optional `virtual` flag.**

---

### 865 – `signature_item` (class declaration, missing class name)
```
/signature_item: CLASS ext list_attribute_ virtual_flag formal_class_parameters . LIDENT COLON class_type …
```
**Message** – After the (optional) formal parameters the parser expects the class identifier.

> **Expected the class name (`LIDENT`) after the formal parameters.**

---

### 866 – `signature_item` (class declaration, missing colon)
```
/signature_item: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT . COLON class_type …
```
**Message** – After the class name the colon that separates the name from the class type is missing.

> **Expected `:` after the class name.**

---

### 867 – `signature_item` (class declaration, missing class type)
```
/signature_item: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON . class_type …
```
**Message** – After the colon the parser expects the class type definition.

> **Expected a class type after `:`.**

---

### 868 – `class_type` (missing tuple type before arrow)
```
/class_type: LIDENT COLON . tuple_type MINUSGREATER class_type
```
**Message** – After `LIDENT :` the parser expects a tuple type (the argument type of the method).

> **Expected a tuple type after `:` in a class type declaration.**

---

### 869 – `class_type` (missing `->` after tuple type)
```
/class_type: LIDENT COLON tuple_type . MINUSGREATER class_type
```
**Message** – After the argument tuple type the arrow `->` that separates it from the result type is missing.

> **Expected `->` after the tuple type.**

---

### 870 – `class_type` (missing result type after `->`)
```
/class_type: LIDENT COLON tuple_type MINUSGREATER . class_type
```
**Message** – After the arrow the parser expects the result class type.

> **Expected a class type after `->`.**

---

### 871 – `class_signature` (bare opening bracket)
```
/class_signature: LBRACKET . reversed_separated_nonempty_llist_COMMA_core_type_ RBRACKET clty_longident
delimited_type_supporting_local_open: LBRACKET . _*
```
**Message** – Inside a class signature an opening `[` must be followed by a (possibly empty) list of core types or a closing `]`.

> **Expected a list of core types (or `]`) after `[` in a class signature.**

---

### 872 – `class_type` (tuple type alone, missing arrow)
```
/class_type: tuple_type . MINUSGREATER class_type
```
**Message** – After a bare tuple type the arrow `->` introducing the result type is missing.

> **Expected `->` after the tuple type.**

---

### 873 – `class_type` (tuple type followed by arrow, missing result)
```
/class_type: tuple_type MINUSGREATER . class_type
```
**Message** – After `->` the parser expects the result class type.

> **Expected a class type after `->`.**

---

### 874 – `class_type` (optional label before tuple, missing tuple)
```
/class_type: optlabel . tuple_type MINUSGREATER class_type
```
**Message** – After an optional label (`?label:` or `~label:`) the parser expects the tuple type.

> **Expected a tuple type after the optional label.**

---

### 875 – `class_type` (optional label and tuple, missing arrow)
```
/class_type: optlabel tuple_type . MINUSGREATER class_type
```
**Message** – After the tuple type the arrow `->` is required.

> **Expected `->` after the tuple type.**

---

### 876 – `class_type` (optional label, tuple, arrow, missing result)
```
/class_type: optlabel tuple_type MINUSGREATER . class_type
```
**Message** – After the arrow the parser expects the result class type.

> **Expected a class type after `->`.**

---

### 877 – `list_and_class_description_` (missing “and” clause)
```
/list_and_class_description_: AND . list_attribute_ virtual_flag formal_class_parameters LIDENT COLON class_type …
```
**Message** – After `and` the parser expects an optional attribute list that begins the next class description.

> **Expected an attribute list (or nothing) after `and` in a series of class descriptions.**

---

### 878 – `list_and_class_description_` (missing attribute list after `and`)
```
/list_and_class_description_: AND list_attribute_ . virtual_flag formal_class_parameters LIDENT COLON class_type …
```
**Message** – After the attribute list that follows `and` the parser expects the optional `virtual` flag.

> **Expected optional `virtual` flag (or nothing) after the attribute list on `and`.**

---

### 879 – `list_and_class_description_` (missing virtual flag)
```
/list_and_class_description_: AND list_attribute_ virtual_flag . formal_class_parameters LIDENT COLON class_type …
```
**Message** – After the (optional) `virtual` keyword the parser expects the formal class parameters.

> **Expected formal class parameters after the optional `virtual` flag.**
Below are concise, human‑readable diagnostics for each of the failing parser configurations you listed.
The messages follow the same style as the examples you gave: they point out **what the parser was expecting at the position marked by the dot**.

/list_and_class_description_: AND list_attribute_ virtual_flag formal_class_parameters . LIDENT COLON class_type…
Expected a class name (an identifier) after the optional attributes / virtual flag / class‑parameter list in a class description that follows an `and`.

/list_and_class_description_: AND list_attribute_ virtual_flag formal_class_parameters LIDENT . COLON class_type…
Expected ‘:’ after the class name in a class description that follows an `and`.

/list_and_class_description_: AND list_attribute_ virtual_flag formal_class_parameters LIDENT COLON . class_type…
Expected a class type after the ‘:` in a class description that follows an `and`.

/list_generic_and_type_declaration_type_kind__: AND . list_attribute_ type_parameters…
Expected an attribute list (or directly the type‑parameter list) after `and` in a type declaration.

/list_generic_and_type_declaration_type_kind__: AND list_attribute_ . type_parameters…
Expected the list of type parameters after the attribute list that follows `and` in a type declaration.

/list_generic_and_type_declaration_type_kind__: AND list_attribute_ type_parameters . LIDENT …
Expected the name of the new type (an identifier) after the type‑parameter list in a type declaration that follows `and`.

/list_generic_and_type_declaration_type_subst_kind__: AND . list_attribute_ type_parameters LIDENT COLONEQUAL…
Expected an attribute list (or nothing) after `and` in a type‑substitution declaration.

/list_generic_and_type_declaration_type_subst_kind__: AND list_attribute_ . type_parameters LIDENT COLONEQUAL…
Expected the list of type parameters after the attribute list that follows `and` in a type‑substitution declaration.

/list_generic_and_type_declaration_type_subst_kind__: AND list_attribute_ type_parameters . LIDENT COLONEQUAL…
Expected the name of the substituted type (an identifier) after the type‑parameter list in a type‑substitution declaration that follows `and`.

/list_generic_and_type_declaration_type_subst_kind__: AND list_attribute_ type_parameters LIDENT . COLONEQUAL…
Expected ‘:=’ after the type name in a type‑substitution declaration that follows `and`.

/list_generic_and_type_declaration_type_subst_kind__: AND list_attribute_ type_parameters LIDENT COLONEQUAL . nonempty_type_kind…
Expected a type definition (e.g. a core type, a record, a variant, `private`, etc.) after ‘:=’ in a type‑substitution declaration.

/list_generic_and_type_declaration_type_subst_kind__: AND list_attribute_ type_parameters LIDENT COLONEQUAL nonempty_type_kind . …
Expected end of the type definition (or a ‘;’/‘and’) after the type‑definition that follows ‘:=’.

/constr_extra_nonprefix_ident: LBRACKET . RBRACKET
/ delimited_type_supporting_local_open: LBRACKET . _*
Expected ‘]’ (or a type expression) after ‘[’.

/ nonempty_type_kind: LBRACE . label_declarations …
Expected a label declaration (e.g. `mutable x : int;`) after ‘{’ in a type definition.

/nonempty_type_kind: LBRACE label_declarations . RBRACE
Expected ‘}’ to close the record/variant definition after the label declarations.

/nonempty_type_kind: EXTERNAL . STRING
Expected a string literal (the external name) after the `external` keyword.

/generic_constructor_declaration_BAR_: BAR . constr_ident …
Expected a constructor identifier after the ‘|’ that starts an extension or variant declaration.

/nonempty_type_kind: core_type EQUAL . _*
Expected a type definition after ‘=’ (e.g. another core type, a record, a variant, `private`, etc.).

/nonempty_type_kind: core_type EQUAL PRIVATE . _*
Expected a type definition after the keyword `private` that follows ‘=’.

/nonempty_type_kind: core_type EQUAL PRIVATE LBRACE . label_declarations …
Expected a label declaration after the ‘{’ that follows `private` in a type definition.

/nonempty_type_kind: core_type EQUAL PRIVATE LBRACE label_declarations . RBRACE
Expected ‘}’ to close the private record/variant after its label declarations.

/nonempty_type_kind: core_type EQUAL LBRACE . label_declarations …
Expected a label declaration after ‘{’ in a type definition that follows ‘=’.

/nonempty_type_kind: core_type EQUAL LBRACE label_declarations . RBRACE
Expected ‘}’ to close the record/variant after its label declarations.

/class_type_declarations: CLASS . TYPE ext …
Expected the keyword `type` after `class` when declaring class types.

/local_structure_item: CLASS ext . list_attribute_ …
Expected an optional attribute list (or directly a `virtual`/`abstract` flag) after `class` and the optional `%` extension marker.

/local_structure_item: CLASS ext list_attribute_ . virtual_flag …
Expected a `virtual` flag (or `mutable`, `abstract`, etc.) after any attributes in a class declaration.

/local_structure_item: CLASS ext list_attribute_ virtual_flag . formal_class_parameters …
Expected a list of class parameters (`[ ... ]`) after `virtual` (or after the attributes if `virtual` is absent).

/local_structure_item: CLASS ext list_attribute_ virtual_flag formal_class_parameters . LIDENT …
Expected the class name (an identifier) after the optional parameter list.

/local_structure_item: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT . class_fun_binding …
Expected either ‘=’, ‘:’, or a class body after the class name (i.e. the start of a class definition).

/constr_extra_nonprefix_ident: LBRACKET . RBRACKET
Expected ‘]’ to close an empty variant constructor list.

/constr_longident: LPAREN . COLONCOLON RPAREN
Expected the constructor operator ‘::’ after ‘(’ in a long identifier.

/simple_param_pattern: LPAREN . pattern COLON reversed_nonempty_llist_typevar_ DOT core_type RPAREN
Expected a pattern after ‘(’ when writing a labelled/function parameter.

/simple_pattern_not_ident: LPAREN . _*
Expected a pattern (or ‘)’) after ‘(’ in a non‑identifier simple pattern.

/val_extra_ident: LPAREN . operator RPAREN
Expected an operator token (e.g. `+`, `~-`, etc.) after ‘(’ when defining an infix value identifier.

--------------------------------------------------------------------
/class_fun_binding: EQUAL . class_expr
Expected a class expression after ‘=’ in a class definition.

/class_simple_expr: OBJECT . list_attribute_ class_self_pattern list_text_cstr_class_field__ END
Expected an optional attribute list (or directly the class self‑pattern) after the keyword `object`.

/class_simple_expr: OBJECT list_attribute_ . class_self_pattern …
Expected the optional class self‑pattern (or the first class field) after any attributes following `object`.

/class_simple_expr: OBJECT list_attribute_ class_self_pattern . list_text_cstr_class_field__ END
Expected at least one class field after the optional self‑pattern in an object expression.

/class_field: VAL . value list_post_item_attribute_
Expected a value definition after the keyword `val` inside a class body.

/value: BANG . _*
Expected a mutable flag (`mutable` / `virtual`) or a value name after the `!` that marks a mutable instance variable.

/value: BANG list_attribute_ . _*
Expected a mutable flag or a value name after `!` followed by attributes.

/value: BANG list_attribute_ mutable_flag . _*
Expected the identifier of the mutable instance variable after `!` and a mutable flag.

/value: BANG list_attribute_ mutable_flag LIDENT . _*
Expected ‘:’ (type annotation) or ‘=’ (value definition) after the mutable value name.

/value: BANG list_attribute_ mutable_flag LIDENT EQUAL . seq_expr
Expected an expression after ‘=’ in a mutable value definition.

/value: BANG list_attribute_ mutable_flag LIDENT COLON core_type COLONGREATER core_type LBRACKETAT attr_payload RBRACKET . …
Expected the end of the value declaration (e.g. ‘;’ or another item) after the attribute list that follows a polymorphic type annotation.

/value: BANG list_attribute_ mutable_flag LIDENT COLON core_type COLONGREATER core_type EQUAL . seq_expr
Expected an expression after ‘=’ in a mutable value with a polymorphic type constraint.

/value: list_attribute_ . _*
Expected either `!` (mutable flag) or an identifier after a leading attribute list in a value declaration.

/value: list_attribute_ virtual_with_mutable_flag . LIDENT COLON core_type
Expected a value name (identifier) after the virtual/mutable modifiers in a value declaration.

/value: list_attribute_ virtual_with_mutable_flag LIDENT . COLON core_type
Expected ‘:’ after the value name when a type annotation follows virtual/mutable modifiers.

/value: list_attribute_ virtual_with_mutable_flag LIDENT COLON . core_type
Expected a core type after ‘:’ in a virtual/mutable value declaration.

/value: list_attribute_ mutable_flag . _*
Expected the name of the mutable value after the `mutable` flag (or an attribute list preceding it).

/value: list_attribute_ mutable_flag LIDENT . _*
Expected ‘:’ (type annotation) or ‘=’ (definition) after the mutable value name.

/value: list_attribute_ mutable_flag LIDENT EQUAL . seq_expr
Expected an expression after ‘=’ in a mutable value definition.

/value: list_attribute_ mutable_flag LIDENT COLON core_type COLONGREATER core_type LBRACKETAT attr_payload RBRACKET . …
Expected the end of the value declaration (or a separator) after the attribute list that follows a polymorphic type annotation.

/value: list_attribute_ mutable_flag LIDENT COLON core_type COLONGREATER core_type EQUAL . seq_expr
Expected an expression after ‘=’ in a mutable value with a polymorphic type.

/class_field: METHOD . method_ list_post_item_attribute_
Expected a method definition after the keyword `method` in a class body.

/method_: BANG . _*
Expected a method name (identifier) after the `!` that denotes a mutable method.

/method_: BANG list_attribute_ . _*
Expected a method name after `!` and any following attributes.

/method_: BANG list_attribute_ private_flag . _*
Expected a method name after `!` and the `private` flag (and any attributes).

/method_: BANG list_attribute_ private_flag LIDENT . _*
Expected ‘:’ (type annotation) or ‘=’ (definition) after the private method name.

/method_: BANG list_attribute_ private_flag LIDENT COLON . _*
Expected a core type (the method’s return type) after the colon in a private method declaration.

/method_: BANG list_attribute_ private_flag LIDENT COLON TYPE . nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL seq_expr
Expected the name of the method’s type constructor (or a list of them) after the keyword `type` in an explicit method type signature.

/method_: BANG list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ . DOT core_type EQUAL seq_expr
Expected a dot ‘.’ after the list of type constructor names in a method type signature.

/method_: BANG list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT . core_type EQUAL seq_expr
Expected the core type of the method’s result after the dot in an explicit method type signature.

/method_: BANG list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type . EQUAL seq_expr
Expected ‘=’ to start the method’s implementation after the method’s result type.
Below are concise, human‑readable diagnostics for every failure pattern you listed.
The part after the dot (`.`) is the token that the parser was **expecting** when it stopped; the message explains what should appear there.

---

### 940
```
method_: BANG list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL . seq_expr
```
**Message:**
*Expected a method body (an expression) after the `=`.*

---

### 941
```
method_: BANG list_attribute_ private_flag LIDENT COLON possibly_poly_core_type_ . EQUAL seq_expr
```
**Message:**
*Expected `=` after the method’s type annotation.*

---

### 942
```
method_: BANG list_attribute_ private_flag LIDENT COLON possibly_poly_core_type_ EQUAL . seq_expr
```
**Message:**
*Expected the method implementation expression after `=`.*

---

### 943
```
method_: list_attribute_ . _*
```
**Message:**
*After optional attributes, a method must start with `private`, `virtual` or an identifier.*

---

### 944
```
method_: list_attribute_ virtual_with_private_flag . LIDENT COLON possibly_poly_core_type_
```
**Message:**
*Expected a method name after `virtual`/`private` (or their combination).*

---

### 945
```
method_: list_attribute_ virtual_with_private_flag LIDENT . COLON possibly_poly_core_type_
```
**Message:**
*Expected `:` after the method name to introduce its type.*

---

### 946
```
method_: list_attribute_ virtual_with_private_flag LIDENT COLON . possibly_poly_core_type_
```
**Message:**
*Expected a type (or polymorphic type) after `:`.*

---

### 947
```
method_: list_attribute_ private_flag . _*
```
**Message:**
*After `private` a method name (or `virtual`) must follow.*

---

### 948
```
method_: list_attribute_ private_flag LIDENT . _*
```
**Message:**
*After the method name, either a `:` with a type annotation or `=` with a body is required.*

---

### 949
```
method_: list_attribute_ private_flag LIDENT COLON . _*
```
**Message:**
*Expected a type (or polymorphic type) after the colon.*

---

### 950
```
method_: list_attribute_ private_flag LIDENT COLON TYPE . nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL seq_expr
```
**Message:**
*Expected the name of the method’s result type after the keyword `type`.*

---

### 951
```
method_: list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ . DOT core_type EQUAL seq_expr
```
**Message:**
*Expected `.` after the result‑type name.*

---

### 952
```
method_: list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT . core_type EQUAL seq_expr
```
**Message:**
*Expected the result type after `.`.*

---

### 953
```
method_: list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type . EQUAL seq_expr
```
**Message:**
*Expected `=` after the method’s result type.*

---

### 954
```
method_: list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL . seq_expr
```
**Message:**
*Expected the method body (an expression) after `=`.*

---

### 955
```
method_: list_attribute_ private_flag LIDENT COLON possibly_poly_core_type_ . EQUAL seq_expr
```
**Message:**
*Expected `=` after the method’s type annotation.*

---

### 956
```
method_: list_attribute_ private_flag LIDENT COLON possibly_poly_core_type_ EQUAL . seq_expr
```
**Message:**
*Expected the method body (an expression) after `=`.*

---

### 957
```
class_field: INITIALIZER . list_attribute_ seq_expr list_post_item_attribute_
```
**Message:**
*Expected an optional attribute list after `initializer`.*

---

### 958
```
class_field: INITIALIZER list_attribute_ . seq_expr list_post_item_attribute_
```
**Message:**
*Expected the initializer expression after the optional attributes.*

---

### 959
```
class_field: INHERIT . _*
```
**Message:**
*Expected a class expression (the parent class) after `inherit`.*

---

### 960
```
class_field: INHERIT BANG . list_attribute_ class_expr option_preceded_AS_mkrhs_LIDENT___ list_post_item_attribute_
```
**Message:**
*Expected an attribute list after `inherit !`.*

---

### 961
```
class_field: INHERIT BANG list_attribute_ . class_expr option_preceded_AS_mkrhs_LIDENT___ list_post_item_attribute_
```
**Message:**
*Expected the parent class expression after `inherit !` and its attributes.*

---

### 962
```
class_simple_expr: LPAREN . _*
```
**Message:**
*Expected an expression or class expression after `(`.*

---

### 963
```
class_expr: LET . _*
let_bindings_no_ext_: LET . _*
```
**Message:**
*Expected a `let` binding (pattern = expr …) after `let`.*

---

### 964
```
let_bindings_no_ext_: LET PERCENT . attr_id list_attribute_ rec_flag let_binding_body list_post_item_attribute_
```
**Message:**
*Expected an attribute identifier after `let %`.*

---

### 965
```
let_bindings_no_ext_: LET PERCENT attr_id . list_attribute_ rec_flag let_binding_body list_post_item_attribute_
```
**Message:**
*Expected an attribute list after the identifier in `let % <attr>`.*

---

### 966
```
let_bindings_no_ext_: LET PERCENT attr_id list_attribute_ . rec_flag let_binding_body list_post_item_attribute_
```
**Message:**
*Expected `rec` (or the binding body) after the attribute list in `let % …`.*

---

### 967
```
let_bindings_no_ext_: LET PERCENT attr_id list_attribute_ rec_flag . let_binding_body list_post_item_attribute_
```
**Message:**
*Expected the let‑binding after `let % … rec`.*

---

### 968
```
class_simple_expr: LPAREN class_expr . _*
```
**Message:**
*After a parenthesised class expression, expect either `:` to give a type annotation or `)` to close the parentheses.*

---

### 989
```
class_simple_expr: LPAREN class_expr COLON class_type . RPAREN
```
**Message:**
*Expected `)` to close the parenthesised class expression with a type annotation.*

---

### 990
```
option_preceded_AS_mkrhs_LIDENT___: AS . LIDENT
```
**Message:**
*Expected an identifier after the keyword `as`.*

---

### 991
```
class_field: INHERIT list_attribute_ . class_expr option_preceded_AS_mkrhs_LIDENT___ list_post_item_attribute_
```
**Message:**
*Expected the parent class expression after `inherit` (and any attributes).*

---

### 992
```
class_field: CONSTRAINT . list_attribute_ constrain_field list_post_item_attribute_
```
**Message:**
*Expected an optional attribute list after `constraint`.*

---

### 993
```
class_field: CONSTRAINT list_attribute_ . constrain_field list_post_item_attribute_
```
**Message:**
*Expected the constraint definition after `constraint` and its attributes.*

---

### 994
```
class_simple_expr: OBJECT list_attribute_ class_self_pattern list_text_cstr_class_field__ . END
```
**Message:**
*Expected `end` to finish the object definition.*

---

### 995
```
class_fun_binding: COLON . class_type EQUAL class_expr
```
**Message:**
*Expected a class type after the colon in a method signature.*

---

### 996
```
class_fun_binding: COLON class_type . EQUAL class_expr
```
**Message:**
*Expected `=` after the method’s class type.*

---

### 997
```
class_fun_binding: COLON class_type EQUAL . class_expr
```
**Message:**
*Expected the method’s implementation (a class expression) after `=`.*

---

### 998
```
class_fun_binding: simple_param_pattern . class_fun_binding
```
**Message:**
*After a parameter pattern, another parameter or the method body (`=` …) is expected.*

---

### 999
```
list_and_class_declaration_: AND . list_attribute_ virtual_flag formal_class_parameters LIDENT class_fun_binding list_post_item_attribute_ list_and_class_declaration_
```
**Message:**
*Expected an attribute list after `and` when continuing a class declaration.*

---

These messages follow the style of your examples: they point out the missing or unexpected token and give a short hint about the construct that should appear. Feel free to adjust the wording to match the tone of the rest of your compiler diagnostics.
**Generated error messages for the supplied parser‑state patterns**

Below each pattern name the message that should be shown when the parser reaches the “dot” (`.`) position and the next token is not one of the allowed look‑aheads.  The wording follows the style of the examples you gave (concise, pointing out the missing/expected token or construct).

| # | Pattern (dot location) | Suggested error message |
|---|------------------------|------------------------|
| 1000 | `list_and_class_declaration_: AND list_attribute_ . virtual_flag formal_class_parameters LIDENT …` | **Expected “virtual” keyword, a class‑type parameter list (`[` … `]`), or the class name identifier.** |
| 1001 | `… virtual_flag . formal_class_parameters LIDENT …` | **Expected a class‑type parameter list (`[` … `]`) or the class name identifier.** |
| 1002 | `… formal_class_parameters . LIDENT …` | **Expected the class name (an identifier).** |
| 1003 | `… LIDENT . class_fun_binding …` | **Expected “=”, “:”, or a parameter pattern to start the class body.** |
| 1004 | `let_bindings_ext_: LET . ext list_attribute_ …` | **Expected an optional attribute (`%…`), a “[@…]” attribute, the optional `rec` keyword, or a binding identifier.** |
| 1005 | `LET ext . list_attribute_ …` | **Expected a “[@…]” attribute or the start of a let‑binding.** |
| 1006 | `LET ext list_attribute_ . rec_flag …` | **Expected optional `rec`.** |
| 1007 | `LET ext list_attribute_ rec_flag . let_binding_body …` | **Expected a value name (identifier) to begin the let‑binding.** |
| 1008 | `floating_attribute: LBRACKETATAT attr_id attr_payload . RBRACKET` | **Expected “]” to close the floating attribute.** |
| 1009 | `item_extension: LBRACKETPERCENTPERCENT attr_id payload . RBRACKET` | **Expected “]” to close the extension item.** |
| 1010 | `fun_expr: LET ext list_attribute_ local_structure_item . IN seq_expr` | **Expected “in” after the let‑binding.** |
| 1011 | `fun_expr: LET … IN . seq_expr` | **Expected an expression after “in”.** |
| 1012 | `simple_param_pattern: QUESTION LPAREN label_let_pattern option_preceded_EQUAL_seq_expr__ . RPAREN` | **Expected “)” to close the optional default value.** |
| 1013 | `fun_expr: LIDENT LESSMINUS FUNCTION . ext list_attribute_ …` | **Expected an optional attribute (`%…`), a “[@…]” attribute, or a match case (pattern) after “function”.** |
| 1014 | `fun_expr: … FUNCTION ext . list_attribute_ …` | **Expected a “[@…]” attribute or a match case after “function %”.** |
| 1015 | `fun_expr: … FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_` | **Expected a match case (pattern or “|”) after the attribute list.** |
| 1016 | `simple_expr: LBRACKETBAR separated_or_terminated_nonempty_list_SEMI_expr_ . BARRBRACKET` | **Expected “|]” to close the array expression.** |
| 1017 | `simple_expr: LPAREN MODULE . _*` | **Expected an optional attribute (`%…`), a “[@…]” attribute, or a module expression after “module”.** |
| 1018 | `LPAREN MODULE ext . _*` | **Expected an optional “[@…]” attribute or a module expression after the extension.** |
| 1019 | `LPAREN MODULE ext list_attribute_ . _*` | **Expected a module expression (`struct …`, `functor …`, a module identifier, …).** |
| 1020 | `LPAREN MODULE … module_expr . END` | **Expected “end” to close the module expression.** |
| 1021 | `LPAREN MODULE … module_expr COLON . module_type` | **Expected a module type after “:”.** |
| 1022 | `… module_type . RPAREN` | **Expected “)” to close the parenthesised module type annotation.** |
| 1023 | `simple_expr: LPAREN seq_expr . _*` | **Expected “)”, “:”, or a type‑constraint after the expression.** |
| 1024 | `LPAREN seq_expr type_constraint . RPAREN` | **Expected “)” to close the typed parenthesised expression.** |
| 1025 | `fun_expr: MATCH ext list_attribute_ seq_expr . WITH …` | **Expected “with” after the match expression.** |
| 1026 | `MATCH … seq_expr WITH . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_` | **Expected a match case (a pattern or “|”) after “with”.** |
| 1027 | `METAOCAML_BRACKET_OPEN seq_expr . METAOCAML_BRACKET_CLOSE` | **Expected the closing meta‑OCaml bracket.** |
| 1028 | `expr_colon_package_type: FUNCTION . _*` | **Expected an optional attribute (`%…`), a “[@…]” attribute, or a match case after “function”.** |
| 1029 | `FUNCTION ext . _*` | **Expected an optional “[@…]” attribute or a match case after “function %”.** |
| 1030 | `FUNCTION ext list_attribute_ . _*` | **Expected a match case after the attribute list.** |
| 1031 | `… COLONGREATER . module_type` | **Expected a module type after the “:>” operator.** |
| 1032 | `… COLON . _*` | **Expected a type after “:”.** |
| 1033 | `… COLON module_type COLONGREATER . module_type` | **Expected a module type after the second “:>”.** |
| 1034 | `fun_expr: fun_expr COLONGREATER . module_type` | **Expected a module type after “:>”.** |
| 1035 | `fun_expr: fun_expr COLON . _*` | **Expected a type after “:”.** |
| 1036 | `fun_expr: fun_expr COLON module_type COLONGREATER . module_type` | **Expected a module type after the “:>”.** |
| 1037 | `paren_module_expr: LPAREN VAL list_attribute_ expr_colon_package_type . RPAREN` | **Expected “)” to close the value‑module expression.** |
| 1038 | `open_declaration: OPEN ext . list_attribute_ …` | **Expected an optional “[@…]” attribute or a module expression to open.** |
| 1039 | `OPEN ext list_attribute_ . module_expr …` | **Expected a module expression after the attribute list.** |
| 1040 | `module_expr: STRUCT list_attribute_ structure . END` | **Expected “end” to close the structure.** |
| 1041 | `simple_pattern_not_ident: LPAREN MODULE … COLON module_type . RPAREN` | **Expected “)” to close the module pattern.** |
| 1042 | `labeled_tuple_pat_element_list_pattern_: LABEL . _*` | **Expected a pattern after the label.** |
| 1043 | `labeled_tuple_pat_element_list_pattern_: LABEL . _*` | **Expected a pattern after the label.** |
| 1044 | `labeled_tuple_pat_element_list_pattern_: TILDE LIDENT . _*` | **Expected “,” or the end of the tuple pattern after the labelled element.** |
| 1045 | `class_self_pattern: LPAREN pattern . _*` | **Expected “)” or “:” (type annotation) after the pattern.** |
| 1046 | `LPAREN pattern COLON . core_type RPAREN` | **Expected a core type after “:”.** |
| 1047 | `LPAREN pattern COLON core_type . RPAREN` | **Expected “)” to close the class‑self pattern.** |
| 1048 | `OBJECT … class_self_pattern . list_text_cstr_class_field__ END` | **Expected a class field or “end” after the class‑self pattern.** |
| 1049 | `OBJECT … class_self_pattern list_text_cstr_class_field__ . END` | **Expected “end” to close the object expression.** |
| 1050 | `reversed_labeled_tuple_body: LABEL . _*` | **Expected an expression after the label.** |
| 1051 | `reversed_labeled_tuple_body: FUNCTION . _*` | **Expected an optional attribute (`%…`), a “[@…]” attribute, or a match case after “function”.** |
| 1052 | `FUNCTION ext . _*` | **Expected an optional “[@…]” attribute or a match case after “function %”.** |
| 1053 | `FUNCTION ext list_attribute_ . _*` | **Expected a match case after the attribute list.** |
| 1054 | `reversed_labeled_tuple_body: LABEL . _*` (inside a type‑constraint tuple) | **Expected an expression after the label.** |
| 1055 | `reversed_labeled_tuple_body: FUNCTION . _*` (inside a type‑constraint tuple) | **Expected an optional attribute (`%…`), a “[@…]” attribute, or a match case after “function”.** |
| 1056 | `FUNCTION ext . _*` (inside a type‑constraint tuple) | **Expected an optional “[@…]” attribute or a match case after “function %”.** |
| 1057 | `FUNCTION ext list_attribute_ . _*` (inside a type‑constraint tuple) | **Expected a match case after the attribute list.** |
| 1058 | `reversed_labeled_tuple_body: TILDE LIDENT . _*` | **Expected “:”, “,” or the end of the tuple after the labelled element.** |
| 1059 | `fun_expr: TRY ext list_attribute_ seq_expr . WITH …` | **Expected “with” after the try expression.** |
| 1060 | `TRY … seq_expr WITH . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_` | **Expected a catch case (pattern) after “with”.** |
| 1061 | `post_item_attribute: LBRACKETATAT attr_id attr_payload . RBRACKET` | **Expected “]” to close the post‑item attribute.** |
| 1062 | `local_structure_item: TYPE … NONREC type_parameters type_longident . PLUSEQ …` | **Expected “+=” to start an extensible type definition.** |
| 1063 | `… PLUSEQ . private_flag …` | **Expected optional “private” before the constructors.** |
| 1064 | `… PLUSEQ private_flag . reversed_bar_llist_extension_constructor_ …` | **Expected type constructors after the “private” modifier.** |
| 1065 | `extension_constructor_rebind_BAR_: BAR . constr_ident …` | **Expected a constructor name after “|”.** |
| 1066 | `BAR constr_ident EQUAL . constr_longident …` | **Expected the long identifier of the constructor after “=”.** |
| 1067 | `constr_ident EQUAL . constr_longident …` | **Expected the long identifier of the constructor after “=”.** |
| 1068 | `generic_type_declaration_nonrec_flag_type_kind_: TYPE … type_parameters . LIDENT …` | **Expected the type name (identifier) after the parameter list.** |
| 1069 | `local_structure_item: TYPE … type_longident . PLUSEQ …` | **Expected “+=” after the type name.** |
| 1070 | `… PLUSEQ . private_flag …` | **Expected optional “private” before the constructors.** |
| 1071 | `… PLUSEQ private_flag . reversed_bar_llist_extension_constructor_ …` | **Expected type constructors after the “private” keyword.** |
| 1072 | `extension: LBRACKETPERCENT attr_id payload . RBRACKET` | **Expected “]” to close the extension.** |
| 1073 | `meth_list: LIDENT COLON . _*` | **Expected a type after the method name.** |
| 1074 | `object_type: LESS meth_list . GREATER` | **Expected “>” to close the object type.** |

*All messages are phrased to guide the user toward the next syntactic element that is required at the point where the parser failed.*

**Pattern 1075**
`function_type: LIDENT COLON LPAREN reversed_nonempty_llist_typevar_ . DOT core_type RPAREN MINUSGREATER function_type`
*Expected a ‘.’ after the list of type variables in a polymorphic type annotation.*

---

**Pattern 1076**
`function_type: LIDENT COLON LPAREN reversed_nonempty_llist_typevar_ DOT . core_type RPAREN MINUSGREATER function_type`
*Expected a type after the ‘.’ in a polymorphic type annotation.*

---

**Pattern 1077**
`function_type: LIDENT COLON LPAREN reversed_nonempty_llist_typevar_ DOT core_type . RPAREN MINUSGREATER function_type`
*Expected ‘)’ to close the parenthesised type annotation (or an attribute before ‘)’).*

---

**Pattern 1078**
`function_type: LIDENT COLON LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN . MINUSGREATER function_type`
*Expected ‘->’ after the closing ‘)’ of the type annotation.*

---

**Pattern 1079**
`function_type: LIDENT COLON LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER . function_type`
*Expected the return‑type of the function after ‘->’.*

---

**Pattern 1080**
`function_type: LIDENT COLON tuple_type . MINUSGREATER function_type`
*Expected ‘->’ after the argument tuple type.*

---

**Pattern 1081**
`function_type: LIDENT COLON tuple_type MINUSGREATER . function_type`
*Expected a return‑type after ‘->’.*

---

**Pattern 1082**
`function_type: LIDENT COLON atomic_type STAR . _*`
`tuple_type: atomic_type STAR . reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_`
*Expected a type after ‘*’ (the next component of the tuple type).*

---

**Pattern 1083**
`function_type: LIDENT COLON atomic_type STAR reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ MINUSGREATER . function_type`
*Expected a return‑type after the ‘->’ that follows the tuple type.*

---

**Pattern 1084**
`function_type: LPAREN reversed_nonempty_llist_typevar_ . DOT core_type RPAREN MINUSGREATER function_type`
*Expected a ‘.’ after the list of type variables in a parenthesised polymorphic type.*

---

**Pattern 1085**
`function_type: LPAREN reversed_nonempty_llist_typevar_ DOT . core_type RPAREN MINUSGREATER function_type`
*Expected a type after the ‘.’ in a parenthesised polymorphic type.*

---

**Pattern 1086**
`function_type: LPAREN reversed_nonempty_llist_typevar_ DOT core_type . RPAREN MINUSGREATER function_type`
*Expected ‘)’ to close the parenthesised type annotation (or an attribute before ‘)’).*

---

**Pattern 1087**
`function_type: LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN . MINUSGREATER function_type`
*Expected ‘->’ after the closing ‘)’ of the type annotation.*

---

**Pattern 1088**
`function_type: LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER . function_type`
*Expected a return‑type after ‘->’.*

---

**Pattern 1089**
`nonempty_type_kind: PRIVATE LBRACE . label_declarations RBRACE`
*Expected a label declaration inside the private record type.*

---

**Pattern 1090**
`nonempty_type_kind: PRIVATE LBRACE label_declarations . RBRACE`
*Expected ‘}’ to close the private record type.*

---

**Pattern 1091**
`generic_type_declaration_no_nonrec_flag_type_subst_kind_: TYPE ext list_attribute_ NONREC type_parameters LIDENT COLONEQUAL . nonempty_type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_`
*Expected a type after ‘=’ in a non‑rec type definition.*

---

**Pattern 1092**
`signature_item: TYPE ext list_attribute_ NONREC type_parameters type_longident . PLUSEQ private_flag reversed_bar_llist_extension_constructor_declaration_ list_post_item_attribute_`
*Expected ‘+=’ after the type name when declaring an extensible variant.*

---

**Pattern 1093**
`signature_item: TYPE ext list_attribute_ NONREC type_parameters type_longident PLUSEQ . private_flag reversed_bar_llist_extension_constructor_declaration_ list_post_item_attribute_`
*Expected an optional ‘private’ keyword (or directly the constructors) after ‘+=’.*

---

**Pattern 1094**
`generic_type_declaration_no_nonrec_flag_type_subst_kind_: TYPE ext list_attribute_ type_parameters . LIDENT COLONEQUAL nonempty_type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_`
*Expected a type name after the optional list of type parameters.*

---

**Pattern 1095**
`generic_type_declaration_no_nonrec_flag_type_subst_kind_: TYPE ext list_attribute_ type_parameters LIDENT COLONEQUAL . nonempty_type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_`
*Expected a type definition after ‘=’ in a type alias.*

---

**Pattern 1096**
`signature_item: TYPE ext list_attribute_ type_parameters type_longident . PLUSEQ private_flag reversed_bar_llist_extension_constructor_declaration_ list_post_item_attribute_`
*Expected ‘+=’ after the type name in a signature.*

---

**Pattern 1097**
`signature_item: TYPE ext list_attribute_ type_longident . PLUSEQ private_flag reversed_bar_llist_extension_constructor_declaration_ list_post_item_attribute_`
*Same as 1096 but without explicit type parameters – ‘+=’ is required after the type name.*

---

**Pattern 1098**
`signature_item: TYPE ext list_attribute_ type_longident PLUSEQ . private_flag reversed_bar_llist_extension_constructor_declaration_ list_post_item_attribute_`
*Expected optional ‘private’ (or constructors) after ‘+=’.*

---

**Pattern 1099**
`signature_item: TYPE ext list_attribute_ type_longident PLUSEQ PRIVATE . reversed_bar_llist_extension_constructor_declaration_ list_post_item_attribute_`
*Expected extension constructors after ‘private +=’.*

---

**Pattern 1100**
`module_type: SIG list_attribute_ signature . END`
*Expected ‘end’ to close the signature.*

---

**Pattern 1101**
`delimited_type_supporting_local_open: LPAREN MODULE ext list_attribute_ module_type . RPAREN`
*Expected ‘)’ to close the locally‑opened module type.*

---

**Pattern 1102**
`attribute: LBRACKETAT attr_id attr_payload . RBRACKET`
*Expected ‘]’ to close the attribute.*

---

**Pattern 1103**
`fun_expr: WHILE ext list_attribute_ . seq_expr DO seq_expr DONE`
*Expected a boolean expression after the keyword ‘while’.*

---

**Pattern 1104**
`fun_expr: WHILE ext list_attribute_ seq_expr . DO seq_expr DONE`
*Expected ‘do’ after the while‑condition.*

---

**Pattern 1105**
`fun_expr: WHILE ext list_attribute_ seq_expr DO . seq_expr DONE`
*Expected the loop body after ‘do’.*

---

**Pattern 1106**
`fun_expr: WHILE ext list_attribute_ seq_expr DO seq_expr . DONE`
*Expected ‘done’ to terminate the while loop.*

---

**Pattern 1107**
`implementation: structure . EOF`
*Expected end‑of‑file after the top‑level structure.*

---

**Pattern 1108**
`interface': . interface`
*Expected a signature (e.g. ‘sig … end’) at the start of the file.*

---

**Pattern 1109**
`interface: signature . EOF`
*Expected end‑of‑file after the interface.*

---

**Pattern 1110**
`parse_any_longident': . parse_any_longident`
*Expected a long identifier (module, value or constructor name).*

---

**Pattern 1111**
`constr_extra_nonprefix_ident: LPAREN . RPAREN`
*Expected ‘)’ to close the empty constructor argument list.*

`val_extra_ident: LPAREN . operator RPAREN`
*Expected an operator after ‘(’ in a value identifier.*

`mk_longident_mod_ext_longident___anonymous_42_: LPAREN . COLONCOLON RPAREN`
*Expected ‘)’ after the ‘(::)’ constructor.*

---

**Pattern 1112**
`mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident . DOT UIDENT`
*Expected ‘.’ followed by an identifier in a module path.*

---

**Pattern 1113**
`mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident . DOT . UIDENT`
*Expected an identifier after the ‘.’ in a module path.*

---

**Pattern 1114**
`mk_longident_mod_ext_longident___anonymous_42_: mod_ext_longident DOT LPAREN . COLONCOLON RPAREN`
*Expected ‘::’ after ‘(’ when writing a module path containing the ‘(::)’ constructor.*

---

**Pattern 1115**
`mk_longident_mod_ext_longident___anonymous_42_: mod_ext_longident DOT LPAREN COLONCOLON . RPAREN`
*Expected ‘)’ after the ‘(::)’ in a module path.*

---

**Pattern 1116**
`parse_any_longident: any_longident . EOF`
*Unexpected token after an identifier; the file should end here.*

---

**Pattern 1117**
`parse_constr_longident': . parse_constr_longident`
*Expected a constructor name.*

---

**Pattern 1118**
`parse_constr_longident: constr_longident . EOF`
*Unexpected token after a constructor name.*

---

**Pattern 1119**
`parse_core_type': . parse_core_type`
*Expected a type expression.*

---

**Pattern 1120**
`parse_core_type: core_type . EOF`
*Unexpected token after a type; end of input was expected.*

---

**Pattern 1121**
`parse_expression': . parse_expression`
*Expected an expression.*

---

**Pattern 1122**
`parse_expression: seq_expr . EOF`
*Unexpected token after an expression; a top‑level separator (`;;`) or end‑of‑file is required.*

---

**Pattern 1123**
`parse_mod_ext_longident': . parse_mod_ext_longident`
*Expected a module‑extended long identifier (e.g. `M.X`).*

---

**Pattern 1124**
`parse_mod_ext_longident: mod_ext_longident . DOT UIDENT`
*Expected an identifier after ‘.’ in a module path.*

---

**Pattern 1125**
`parse_mod_longident': . parse_mod_longident`
*Expected a module identifier.*

---

**Pattern 1126**
`parse_mod_longident: mod_longident . DOT UIDENT`
*Expected an identifier after ‘.’ in a module path.*

---

**Pattern 1127**
`parse_module_expr': . parse_module_expr`
*Expected a module expression (e.g. `struct … end`, a functor, etc.).*

---

**Pattern 1128**
`parse_module_expr: module_expr . EOF`
*Unexpected token after a module expression; the file should end here.*

---

**Pattern 1129**
`parse_module_type': . parse_module_type`
*Expected a module type.*

---

**Pattern 1130**
`parse_module_type: module_type . EOF`
*Unexpected token after a module type; end of input expected.*

---

**Pattern 1131**
`parse_mty_longident': . parse_mty_longident`
*Expected a module‑type identifier.*

---

**Pattern 1132**
`parse_mty_longident: mty_longident . EOF`
*Unexpected token after a module‑type identifier.*

---

**Pattern 1133**
`parse_pattern': . parse_pattern`
*Expected a pattern.*

---

**Pattern 1134**
`labeled_tuple_pat_element_list_pattern_: pattern . _*`
`parse_pattern: pattern . EOF`
`reversed_labeled_tuple_pattern_pattern_: pattern . COMMA DOTDOT`
*After a pattern the parser expected a separator (`|`, `when`, `,`, `...`) or the end of the pattern; none was found.*

---

**Pattern 1135**
`parse_val_longident': . parse_val_longident`
*Expected a value identifier (e.g. a variable or a module‑qualified name).*

---

**Pattern 1136**
`parse_val_longident: val_longident . EOF`
*Unexpected token after a value identifier; the phrase should end.*

---

**Pattern 1137**
`mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT`
*Expected ‘.’ followed by an identifier in a module path.*

---

**Pattern 1138**
`mk_longident_mod_longident_UIDENT_: mod_longident . DOT . UIDENT`
*Expected an identifier after the ‘.’ in a module path.*

---

**Pattern 1139**
`toplevel_phrase': . toplevel_phrase`
*Expected a top‑level phrase (expression, definition, or directive).*

---

**Pattern 1140**
`toplevel_directive: HASH . _*`
*Expected a directive name (identifier) after ‘#’.*

---

**Pattern 1141**
`toplevel_phrase: toplevel_directive . SEMISEMI`
*Expected ‘;;’ after a toplevel directive.*

---

**Pattern 1142**
`toplevel_phrase: seq_expr . list_post_item_attribute_ SEMISEMI`
*Expected ‘;;’ after a top‑level expression.*

---

**Pattern 1143**
`toplevel_phrase: seq_expr list_post_item_attribute_ . SEMISEMI`
*Same as 1142 – ‘;;’ is missing after the expression.*

---

**Pattern 1144**
`toplevel_phrase: list_text_str_structure_item__ . SEMISEMI`
*Expected ‘;;’ after a structure item.*

---

**Pattern 1145**
`use_file': . use_file`
*Expected a toplevel phrase (expression, definition, or directive).*

---

**Pattern 1146**
`use_file: seq_expr . list_post_item_attribute_ list_use_file_element_ EOF`
*After a top‑level expression the parser expected ‘;;’ or another phrase; none was found.*

---

**Pattern 1147**
`use_file: seq_expr list_post_item_attribute_ . list_use_file_element_ EOF`
*Same as 1146 – a separator or another phrase is missing.*

---

**Pattern 1148**
`use_file: seq_expr list_post_item_attribute_ list_use_file_element_ . EOF`
*Unexpected token after the last top‑level phrase; the file should end.*

---

**Pattern 1149**
`use_file: list_use_file_element_ . EOF`
*Unexpected token; a top‑level phrase (or end‑of‑file) was expected.*

---

**Pattern 1150**
`use_file: SEMISEMI`
*‘;;’ cannot appear alone; a top‑level phrase is required before it.*
