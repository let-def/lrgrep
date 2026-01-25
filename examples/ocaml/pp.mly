%start<unit> implementation
%start<unit> interface
%start<unit> parse_any_longident
%start<unit> parse_constr_longident
%start<unit> parse_core_type
%start<unit> parse_expression
%start<unit> parse_mod_ext_longident
%start<unit> parse_mod_longident
%start<unit> parse_module_expr
%start<unit> parse_module_type
%start<unit> parse_mty_longident
%start<unit> parse_pattern
%start<unit> parse_val_longident
%start<unit> toplevel_phrase
%start<unit> use_file
%token AMPERAMPER
%token AMPERSAND
%token AND
%token ANDOP
%token AS
%token ASSERT
%token BACKQUOTE
%token BANG
%token BAR
%token BARBAR
%token BARRBRACKET
%token BEGIN
%token CHAR
%token CLASS
%token COLON
%token COLONCOLON
%token COLONEQUAL
%token COLONGREATER
%token COMMA
%token COMMENT
%token CONSTRAINT
%token DO
%token DOCSTRING
%token DONE
%token DOT
%token DOTDOT
%token DOTOP
%token DOWNTO
%token EFFECT
%token ELSE
%token END
%token EOF
%token EOL
%token EQUAL
%token EXCEPTION
%token EXTERNAL
%token FALSE
%token FLOAT
%token FOR
%token FUN
%token FUNCTION
%token FUNCTOR
%token GREATER
%token GREATERRBRACE
%token GREATERRBRACKET
%token HASH
%token HASHOP
%token IF
%token IN
%token INCLUDE
%token INFIXOP0
%token INFIXOP1
%token INFIXOP2
%token INFIXOP3
%token INFIXOP4
%token INHERIT
%token INITIALIZER
%token INT
%token LABEL
%token LAZY
%token LBRACE
%token LBRACELESS
%token LBRACKET
%token LBRACKETAT
%token LBRACKETATAT
%token LBRACKETATATAT
%token LBRACKETBAR
%token LBRACKETGREATER
%token LBRACKETLESS
%token LBRACKETPERCENT
%token LBRACKETPERCENTPERCENT
%token LESS
%token LESSMINUS
%token LET
%token LETOP
%token LIDENT
%token LPAREN
%token MATCH
%token METAOCAML_BRACKET_CLOSE
%token METAOCAML_BRACKET_OPEN
%token METAOCAML_ESCAPE
%token METHOD
%token MINUS
%token MINUSDOT
%token MINUSGREATER
%token MODULE
%token MUTABLE
%token NEW
%token NONREC
%token OBJECT
%token OF
%token OPEN
%token OPTLABEL
%token OR
%token PERCENT
%token PLUS
%token PLUSDOT
%token PLUSEQ
%token PREFIXOP
%token PRIVATE
%token QUESTION
%token QUOTE
%token QUOTED_STRING_EXPR
%token QUOTED_STRING_ITEM
%token RBRACE
%token RBRACKET
%token REC
%token RPAREN
%token SEMI
%token SEMISEMI
%token SIG
%token STAR
%token STRING
%token STRUCT
%token THEN
%token TILDE
%token TO
%token TRUE
%token TRY
%token TYPE
%token UIDENT
%token UNDERSCORE
%token VAL
%token VIRTUAL
%token WHEN
%token WHILE
%token WITH
%nonassoc IN
%nonassoc below_SEMI
%nonassoc SEMI
%nonassoc LET
%nonassoc below_WITH
%nonassoc FUNCTION WITH
%nonassoc AND
%nonassoc THEN
%nonassoc ELSE
%nonassoc LESSMINUS
%right COLONEQUAL
%nonassoc AS
%left BAR
%nonassoc below_COMMA
%left COMMA
%right MINUSGREATER
%right BARBAR OR
%right AMPERAMPER AMPERSAND
%nonassoc below_EQUAL
%left EQUAL GREATER INFIXOP0 LESS
%right INFIXOP1
%nonassoc below_LBRACKETAT
%nonassoc LBRACKETAT
%right COLONCOLON
%left INFIXOP2 MINUS MINUSDOT PLUS PLUSDOT PLUSEQ
%left INFIXOP3 PERCENT STAR
%right INFIXOP4
%nonassoc prec_unary_minus prec_unary_plus
%nonassoc prec_constant_constructor
%nonassoc prec_constr_appl
%nonassoc below_HASH
%nonassoc HASH
%left HASHOP
%nonassoc below_DOT
%nonassoc DOT DOTOP
%nonassoc BACKQUOTE BANG BEGIN CHAR FALSE FLOAT INT LBRACE LBRACELESS LBRACKET LBRACKETBAR LBRACKETPERCENT LIDENT LPAREN METAOCAML_BRACKET_OPEN METAOCAML_ESCAPE NEW OBJECT PREFIXOP QUOTED_STRING_EXPR STRING TRUE UIDENT
%%

option_BAR_:
| 
| BAR
  {}

option_SEMI_:
| 
| SEMI
  {}

option_preceded_AS_mkrhs_LIDENT___:
| 
| AS LIDENT
  {}

option_preceded_COLON_atomic_type__:
| 
| COLON atomic_type
  {}

option_preceded_COLON_core_type__:
| 
| COLON core_type
  {}

option_preceded_EQUAL_expr__:
| 
| EQUAL fun_expr
| EQUAL FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
  {}

option_preceded_EQUAL_module_type__:
| 
| EQUAL module_type
  {}

option_preceded_EQUAL_pattern__:
| 
| EQUAL pattern
  {}

option_preceded_EQUAL_seq_expr__:
| 
| EQUAL seq_expr
  {}

option_type_constraint_:
| 
| type_constraint
  {}

list_and_class_declaration_:
| 
| AND list_attribute_ virtual_flag formal_class_parameters LIDENT class_fun_binding list_post_item_attribute_ list_and_class_declaration_
  {}

list_and_class_description_:
| 
| AND list_attribute_ virtual_flag formal_class_parameters LIDENT COLON class_type list_post_item_attribute_ list_and_class_description_
  {}

list_and_class_type_declaration_:
| 
| AND list_attribute_ virtual_flag formal_class_parameters LIDENT EQUAL class_signature list_post_item_attribute_ list_and_class_type_declaration_
  {}

list_and_module_binding_:
| 
| AND list_attribute_ module_name module_binding_body list_post_item_attribute_ list_and_module_binding_
  {}

list_and_module_declaration_:
| 
| AND list_attribute_ module_name COLON module_type list_post_item_attribute_ list_and_module_declaration_
  {}

list_attribute_:
| 
| attribute list_attribute_
  {}

list_generic_and_type_declaration_type_kind__:
| 
| AND list_attribute_ type_parameters LIDENT type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_ list_generic_and_type_declaration_type_kind__
  {}

list_generic_and_type_declaration_type_subst_kind__:
| 
| AND list_attribute_ type_parameters LIDENT COLONEQUAL nonempty_type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_ list_generic_and_type_declaration_type_subst_kind__
  {}

list_post_item_attribute_:
| 
| post_item_attribute list_post_item_attribute_
  {}

list_signature_element_:
| 
| SEMISEMI list_signature_element_
| signature_item list_signature_element_
  {}

list_structure_element_:
| 
| SEMISEMI list_structure_element_
| SEMISEMI seq_expr list_post_item_attribute_ list_structure_element_
| structure_item list_structure_element_
  {}

list_text_csig_class_sig_field__:
| 
| class_sig_field list_text_csig_class_sig_field__
  {}

list_text_cstr_class_field__:
| 
| class_field list_text_cstr_class_field__
  {}

list_text_str_structure_item__:
| 
| structure_item list_text_str_structure_item__
  {}

list_use_file_element_:
| 
| SEMISEMI list_use_file_element_
| SEMISEMI seq_expr list_post_item_attribute_ list_use_file_element_
| structure_item list_use_file_element_
| toplevel_directive list_use_file_element_
  {}

nonempty_list_mkrhs_LIDENT__:
| LIDENT
| LIDENT nonempty_list_mkrhs_LIDENT__
  {}

nonempty_list_raw_string_:
| STRING
| STRING nonempty_list_raw_string_
  {}

reversed_llist_preceded_CONSTRAINT_constrain__:
| 
| reversed_llist_preceded_CONSTRAINT_constrain__ CONSTRAINT core_type EQUAL core_type
  {}

reversed_nonempty_llist_functor_arg_:
| functor_arg
| reversed_nonempty_llist_functor_arg_ functor_arg
  {}

reversed_nonempty_llist_labeled_simple_expr_:
| labeled_simple_expr
| reversed_nonempty_llist_labeled_simple_expr_ labeled_simple_expr
  {}

reversed_nonempty_llist_name_tag_:
| name_tag
| reversed_nonempty_llist_name_tag_ name_tag
  {}

reversed_nonempty_llist_typevar_:
| QUOTE ident
| reversed_nonempty_llist_typevar_ QUOTE ident
  {}

reversed_nonempty_concat_fun_param_as_list_:
| fun_param_as_list
| reversed_nonempty_concat_fun_param_as_list_ fun_param_as_list
  {}

reversed_separated_nonempty_llist_AMPERSAND_core_type_no_attr_:
| alias_type
| reversed_separated_nonempty_llist_AMPERSAND_core_type_no_attr_ AMPERSAND alias_type
  {}

reversed_separated_nonempty_llist_AND_with_constraint_:
| with_constraint
| reversed_separated_nonempty_llist_AND_with_constraint_ AND with_constraint
  {}

reversed_separated_nonempty_llist_BAR_row_field_:
| row_field
| reversed_separated_nonempty_llist_BAR_row_field_ BAR row_field
  {}

reversed_separated_nonempty_llist_COMMA_core_type_:
| core_type
| reversed_separated_nonempty_llist_COMMA_core_type_ COMMA core_type
  {}

reversed_separated_nonempty_llist_COMMA_type_parameter_:
| type_parameter
| reversed_separated_nonempty_llist_COMMA_type_parameter_ COMMA type_parameter
  {}

reversed_separated_nonempty_llist_STAR_atomic_type_:
| atomic_type
| reversed_separated_nonempty_llist_STAR_atomic_type_ STAR atomic_type
  {}

reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_:
| atomic_type %prec STAR
| LIDENT COLON atomic_type %prec STAR
| reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ STAR atomic_type %prec STAR
| reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ STAR LIDENT COLON atomic_type %prec STAR
  {}

reversed_separated_nontrivial_llist_COMMA_core_type_:
| reversed_separated_nontrivial_llist_COMMA_core_type_ COMMA core_type
| core_type COMMA core_type
  {}

separated_or_terminated_nonempty_list_SEMI_expr_:
| fun_expr
| fun_expr SEMI
| FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ SEMI
| fun_expr SEMI separated_or_terminated_nonempty_list_SEMI_expr_
| FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ SEMI separated_or_terminated_nonempty_list_SEMI_expr_
  {}

separated_or_terminated_nonempty_list_SEMI_object_expr_field_:
| LIDENT option_preceded_EQUAL_expr__
| LIDENT option_preceded_EQUAL_expr__ SEMI
| LIDENT option_preceded_EQUAL_expr__ SEMI separated_or_terminated_nonempty_list_SEMI_object_expr_field_
  {}

separated_or_terminated_nonempty_list_SEMI_pattern_:
| pattern
| pattern SEMI
| pattern SEMI separated_or_terminated_nonempty_list_SEMI_pattern_
  {}

separated_or_terminated_nonempty_list_SEMI_record_expr_field_:
| label_longident option_type_constraint_ option_preceded_EQUAL_expr__
| label_longident option_type_constraint_ option_preceded_EQUAL_expr__ SEMI
| label_longident option_type_constraint_ option_preceded_EQUAL_expr__ SEMI separated_or_terminated_nonempty_list_SEMI_record_expr_field_
  {}

reversed_preceded_or_separated_nonempty_llist_BAR_match_case_:
| match_case
| BAR match_case
| reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ BAR match_case
  {}

reversed_bar_llist_constructor_declaration_:
| generic_constructor_declaration_epsilon_
| generic_constructor_declaration_BAR_
| reversed_bar_llist_constructor_declaration_ generic_constructor_declaration_BAR_
  {}

reversed_bar_llist_extension_constructor_:
| generic_constructor_declaration_epsilon_
| extension_constructor_rebind_epsilon_
| generic_constructor_declaration_BAR_
| extension_constructor_rebind_BAR_
| reversed_bar_llist_extension_constructor_ generic_constructor_declaration_BAR_
| reversed_bar_llist_extension_constructor_ extension_constructor_rebind_BAR_
  {}

reversed_bar_llist_extension_constructor_declaration_:
| generic_constructor_declaration_epsilon_
| generic_constructor_declaration_BAR_
| reversed_bar_llist_extension_constructor_declaration_ generic_constructor_declaration_BAR_
  {}

listx_SEMI_record_pat_field_UNDERSCORE_:
| label_longident option_preceded_COLON_core_type__ option_preceded_EQUAL_pattern__
| label_longident option_preceded_COLON_core_type__ option_preceded_EQUAL_pattern__ SEMI
| label_longident option_preceded_COLON_core_type__ option_preceded_EQUAL_pattern__ SEMI UNDERSCORE option_SEMI_
| label_longident option_preceded_COLON_core_type__ option_preceded_EQUAL_pattern__ SEMI listx_SEMI_record_pat_field_UNDERSCORE_
  {}

implementation:
| structure EOF
  {}

interface:
| signature EOF
  {}

toplevel_phrase:
| seq_expr list_post_item_attribute_ SEMISEMI
| list_text_str_structure_item__ SEMISEMI
| toplevel_directive SEMISEMI
| EOF
  {}

use_file:
| list_use_file_element_ EOF
| seq_expr list_post_item_attribute_ list_use_file_element_ EOF
  {}

parse_module_type:
| module_type EOF
  {}

parse_module_expr:
| module_expr EOF
  {}

parse_core_type:
| core_type EOF
  {}

parse_expression:
| seq_expr EOF
  {}

parse_pattern:
| pattern EOF
  {}

parse_mty_longident:
| mty_longident EOF
  {}

parse_val_longident:
| val_longident EOF
  {}

parse_constr_longident:
| constr_longident EOF
  {}

parse_mod_ext_longident:
| mod_ext_longident EOF
  {}

parse_mod_longident:
| mod_longident EOF
  {}

parse_any_longident:
| any_longident EOF
  {}

functor_arg:
| LPAREN RPAREN
| LPAREN module_name COLON module_type RPAREN
  {}

module_name:
| UIDENT
| UNDERSCORE
  {}

module_expr:
| STRUCT list_attribute_ structure END
| FUNCTOR list_attribute_ reversed_nonempty_llist_functor_arg_ MINUSGREATER module_expr
| paren_module_expr
| module_expr attribute
| mod_longident
| module_expr paren_module_expr
| module_expr LPAREN RPAREN
| extension
  {}

paren_module_expr:
| LPAREN module_expr COLON module_type RPAREN
| LPAREN module_expr RPAREN
| LPAREN VAL list_attribute_ expr_colon_package_type RPAREN
  {}

expr_colon_package_type:
| fun_expr
| FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| fun_expr COLON module_type
| FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COLON module_type
| fun_expr COLON module_type COLONGREATER module_type
| FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COLON module_type COLONGREATER module_type
| fun_expr COLONGREATER module_type
| FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COLONGREATER module_type
  {}

structure:
| list_structure_element_
| seq_expr list_post_item_attribute_ list_structure_element_
  {}

structure_item:
| let_bindings_ext_
| INCLUDE ext list_attribute_ module_expr list_post_item_attribute_
| local_structure_item
  {}

local_structure_item:
| item_extension list_post_item_attribute_
| floating_attribute
| primitive_declaration
| value_description
| generic_type_declaration_nonrec_flag_type_kind_ list_generic_and_type_declaration_type_kind__
| TYPE ext list_attribute_ type_parameters type_longident PLUSEQ private_flag reversed_bar_llist_extension_constructor_ list_post_item_attribute_
| TYPE ext list_attribute_ NONREC type_parameters type_longident PLUSEQ private_flag reversed_bar_llist_extension_constructor_ list_post_item_attribute_
| str_exception_declaration
| MODULE ext list_attribute_ REC module_name module_binding_body list_post_item_attribute_ list_and_module_binding_
| module_type_declaration
| CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT class_fun_binding list_post_item_attribute_ list_and_class_declaration_
| class_type_declarations
| sig_exception_declaration
| MODULE ext list_attribute_ module_name module_binding_body list_post_item_attribute_
| open_declaration
  {}

module_binding_body:
| EQUAL module_expr
| COLON module_type EQUAL module_expr
| functor_arg module_binding_body
  {}

module_type_declaration:
| MODULE TYPE ext list_attribute_ ident option_preceded_EQUAL_module_type__ list_post_item_attribute_
  {}

open_declaration:
| OPEN ext list_attribute_ module_expr list_post_item_attribute_
| OPEN BANG ext list_attribute_ module_expr list_post_item_attribute_
  {}

open_description:
| OPEN ext list_attribute_ mod_ext_longident list_post_item_attribute_
| OPEN BANG ext list_attribute_ mod_ext_longident list_post_item_attribute_
  {}

module_type:
| SIG list_attribute_ signature END
| FUNCTOR list_attribute_ reversed_nonempty_llist_functor_arg_ MINUSGREATER module_type %prec below_WITH
| reversed_nonempty_llist_functor_arg_ MINUSGREATER module_type %prec below_WITH
| MODULE TYPE OF list_attribute_ module_expr %prec below_LBRACKETAT
| LPAREN module_type RPAREN
| module_type attribute
| mty_longident
| module_type MINUSGREATER module_type %prec below_WITH
| module_type WITH reversed_separated_nonempty_llist_AND_with_constraint_
| extension
  {}

signature:
| list_signature_element_
  {}

signature_item:
| item_extension list_post_item_attribute_
| floating_attribute
| value_description
| primitive_declaration
| generic_type_declaration_nonrec_flag_type_kind_ list_generic_and_type_declaration_type_kind__
| generic_type_declaration_no_nonrec_flag_type_subst_kind_ list_generic_and_type_declaration_type_subst_kind__
| TYPE ext list_attribute_ type_parameters type_longident PLUSEQ private_flag reversed_bar_llist_extension_constructor_declaration_ list_post_item_attribute_
| TYPE ext list_attribute_ NONREC type_parameters type_longident PLUSEQ private_flag reversed_bar_llist_extension_constructor_declaration_ list_post_item_attribute_
| sig_exception_declaration
| MODULE ext list_attribute_ module_name module_declaration_body list_post_item_attribute_
| MODULE ext list_attribute_ module_name EQUAL mod_longident list_post_item_attribute_
| module_subst
| MODULE ext list_attribute_ REC module_name COLON module_type list_post_item_attribute_ list_and_module_declaration_
| module_type_declaration
| module_type_subst
| open_description
| INCLUDE ext list_attribute_ module_type list_post_item_attribute_
| CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON class_type list_post_item_attribute_ list_and_class_description_
| class_type_declarations
  {}

module_declaration_body:
| COLON module_type
| functor_arg module_declaration_body
  {}

module_subst:
| MODULE ext list_attribute_ UIDENT COLONEQUAL mod_ext_longident list_post_item_attribute_
  {}

module_type_subst:
| MODULE TYPE ext list_attribute_ ident COLONEQUAL module_type list_post_item_attribute_
  {}

class_fun_binding:
| EQUAL class_expr
| COLON class_type EQUAL class_expr
| simple_param_pattern class_fun_binding
  {}

formal_class_parameters:
| 
| LBRACKET reversed_separated_nonempty_llist_COMMA_type_parameter_ RBRACKET
  {}

class_expr:
| class_simple_expr
| FUN list_attribute_ class_fun_def
| let_bindings_no_ext_ IN class_expr
| LET OPEN list_attribute_ mod_longident IN class_expr
| LET OPEN BANG list_attribute_ mod_longident IN class_expr
| class_expr attribute
| class_simple_expr reversed_nonempty_llist_labeled_simple_expr_
| extension
  {}

class_simple_expr:
| LPAREN class_expr RPAREN
| class_longident
| LBRACKET reversed_separated_nonempty_llist_COMMA_core_type_ RBRACKET class_longident
| LPAREN class_expr COLON class_type RPAREN
| OBJECT list_attribute_ class_self_pattern list_text_cstr_class_field__ END
  {}

class_fun_def:
| simple_param_pattern MINUSGREATER class_expr
| simple_param_pattern class_fun_def
  {}

class_self_pattern:
| LPAREN pattern RPAREN
| LPAREN pattern COLON core_type RPAREN
| 
  {}

class_field:
| INHERIT list_attribute_ class_expr option_preceded_AS_mkrhs_LIDENT___ list_post_item_attribute_
| INHERIT BANG list_attribute_ class_expr option_preceded_AS_mkrhs_LIDENT___ list_post_item_attribute_
| VAL value list_post_item_attribute_
| METHOD method_ list_post_item_attribute_
| CONSTRAINT list_attribute_ constrain_field list_post_item_attribute_
| INITIALIZER list_attribute_ seq_expr list_post_item_attribute_
| item_extension list_post_item_attribute_
| floating_attribute
  {}

value:
| list_attribute_ virtual_with_mutable_flag LIDENT COLON core_type
| list_attribute_ mutable_flag LIDENT EQUAL seq_expr
| BANG list_attribute_ mutable_flag LIDENT EQUAL seq_expr
| list_attribute_ mutable_flag LIDENT type_constraint EQUAL seq_expr
| BANG list_attribute_ mutable_flag LIDENT type_constraint EQUAL seq_expr
  {}

method_:
| list_attribute_ virtual_with_private_flag LIDENT COLON possibly_poly_core_type_
| list_attribute_ private_flag LIDENT strict_binding
| BANG list_attribute_ private_flag LIDENT strict_binding
| list_attribute_ private_flag LIDENT COLON possibly_poly_core_type_ EQUAL seq_expr
| BANG list_attribute_ private_flag LIDENT COLON possibly_poly_core_type_ EQUAL seq_expr
| list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL seq_expr
| BANG list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL seq_expr
  {}

class_type:
| class_signature
| optlabel tuple_type MINUSGREATER class_type
| LIDENT COLON tuple_type MINUSGREATER class_type
| tuple_type MINUSGREATER class_type
  {}

class_signature:
| clty_longident
| LBRACKET reversed_separated_nonempty_llist_COMMA_core_type_ RBRACKET clty_longident
| extension
| OBJECT list_attribute_ class_self_type list_text_csig_class_sig_field__ END
| class_signature attribute
| LET OPEN list_attribute_ mod_longident IN class_signature
| LET OPEN BANG list_attribute_ mod_longident IN class_signature
  {}

class_self_type:
| LPAREN core_type RPAREN
| 
  {}

class_sig_field:
| INHERIT list_attribute_ class_signature list_post_item_attribute_
| VAL list_attribute_ mutable_virtual_flags LIDENT COLON core_type list_post_item_attribute_
| METHOD list_attribute_ private_virtual_flags LIDENT COLON possibly_poly_core_type_ list_post_item_attribute_
| CONSTRAINT list_attribute_ constrain_field list_post_item_attribute_
| item_extension list_post_item_attribute_
| floating_attribute
  {}

constrain_field:
| core_type EQUAL core_type
  {}

class_type_declarations:
| CLASS TYPE ext list_attribute_ virtual_flag formal_class_parameters LIDENT EQUAL class_signature list_post_item_attribute_ list_and_class_type_declaration_
  {}

fun_seq_expr:
| fun_expr %prec below_SEMI
| fun_expr SEMI
| fun_expr SEMI seq_expr
| fun_expr SEMI PERCENT attr_id seq_expr
  {}

seq_expr:
| fun_seq_expr
| FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
  {}

simple_param_pattern:
| QUESTION LPAREN label_let_pattern option_preceded_EQUAL_seq_expr__ RPAREN
| QUESTION LIDENT
| OPTLABEL LPAREN let_pattern option_preceded_EQUAL_seq_expr__ RPAREN
| OPTLABEL pattern_var
| TILDE LPAREN label_let_pattern RPAREN
| TILDE LIDENT
| LABEL simple_pattern
| simple_pattern
| LABEL LPAREN pattern COLON reversed_nonempty_llist_typevar_ DOT core_type RPAREN
| LPAREN pattern COLON reversed_nonempty_llist_typevar_ DOT core_type RPAREN
  {}

pattern_var:
| LIDENT
| UNDERSCORE
  {}

label_let_pattern:
| LIDENT
| LIDENT COLON possibly_poly_core_type_
  {}

let_pattern:
| pattern
| pattern COLON possibly_poly_core_type_
  {}

fun_expr:
| simple_expr %prec below_HASH
| LET ext list_attribute_ local_structure_item IN seq_expr
| FUN ext list_attribute_ fun_params option_preceded_COLON_atomic_type__ MINUSGREATER fun_body
| MATCH ext list_attribute_ seq_expr WITH reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| TRY ext list_attribute_ seq_expr WITH reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| IF ext list_attribute_ seq_expr THEN fun_expr ELSE fun_expr
| IF ext list_attribute_ seq_expr THEN fun_expr ELSE FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| IF ext list_attribute_ seq_expr THEN FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ ELSE fun_expr
| IF ext list_attribute_ seq_expr THEN FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ ELSE FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| IF ext list_attribute_ seq_expr THEN fun_expr
| IF ext list_attribute_ seq_expr THEN FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| WHILE ext list_attribute_ seq_expr DO seq_expr DONE
| FOR ext list_attribute_ pattern EQUAL seq_expr direction_flag seq_expr DO seq_expr DONE
| ASSERT ext list_attribute_ simple_expr %prec below_HASH
| LAZY ext list_attribute_ simple_expr %prec below_HASH
| simple_expr reversed_nonempty_llist_labeled_simple_expr_
| reversed_labeled_tuple_body %prec below_COMMA
| constr_longident simple_expr %prec below_HASH
| name_tag simple_expr %prec below_HASH
| fun_expr INFIXOP0 fun_expr
| fun_expr INFIXOP0 FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| fun_expr INFIXOP1 fun_expr
| fun_expr INFIXOP1 FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| fun_expr INFIXOP2 fun_expr
| fun_expr INFIXOP2 FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| fun_expr INFIXOP3 fun_expr
| fun_expr INFIXOP3 FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| fun_expr INFIXOP4 fun_expr
| fun_expr INFIXOP4 FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| fun_expr PLUS fun_expr
| fun_expr PLUS FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| fun_expr PLUSDOT fun_expr
| fun_expr PLUSDOT FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| fun_expr PLUSEQ fun_expr
| fun_expr PLUSEQ FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| fun_expr MINUS fun_expr
| fun_expr MINUS FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| fun_expr MINUSDOT fun_expr
| fun_expr MINUSDOT FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| fun_expr STAR fun_expr
| fun_expr STAR FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| fun_expr PERCENT fun_expr
| fun_expr PERCENT FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| fun_expr EQUAL fun_expr
| fun_expr EQUAL FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| fun_expr LESS fun_expr
| fun_expr LESS FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| fun_expr GREATER fun_expr
| fun_expr GREATER FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| fun_expr OR fun_expr
| fun_expr OR FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| fun_expr BARBAR fun_expr
| fun_expr BARBAR FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| fun_expr AMPERSAND fun_expr
| fun_expr AMPERSAND FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| fun_expr AMPERAMPER fun_expr
| fun_expr AMPERAMPER FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| fun_expr COLONEQUAL fun_expr
| fun_expr COLONEQUAL FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| subtractive fun_expr %prec prec_unary_minus
| subtractive FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ %prec prec_unary_minus
| additive fun_expr %prec prec_unary_plus
| additive FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ %prec prec_unary_plus
| let_bindings_ext_ IN seq_expr
| LETOP letop_bindings IN seq_expr
| fun_expr COLONCOLON fun_expr
| fun_expr COLONCOLON FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| LIDENT LESSMINUS fun_expr
| LIDENT LESSMINUS FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| simple_expr DOT label_longident LESSMINUS fun_expr
| simple_expr DOT label_longident LESSMINUS FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| simple_expr DOT LPAREN seq_expr RPAREN LESSMINUS fun_expr
| simple_expr DOT LPAREN seq_expr RPAREN LESSMINUS FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| simple_expr DOT LBRACE seq_expr RBRACE LESSMINUS fun_expr
| simple_expr DOT LBRACE seq_expr RBRACE LESSMINUS FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| simple_expr DOT LBRACKET seq_expr RBRACKET LESSMINUS fun_expr
| simple_expr DOT LBRACKET seq_expr RBRACKET LESSMINUS FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| simple_expr DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN LESSMINUS fun_expr
| simple_expr DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN LESSMINUS FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| simple_expr DOT mod_longident DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN LESSMINUS fun_expr
| simple_expr DOT mod_longident DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN LESSMINUS FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS fun_expr
| simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| simple_expr DOT mod_longident DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS fun_expr
| simple_expr DOT mod_longident DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| simple_expr DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS fun_expr
| simple_expr DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| simple_expr DOT mod_longident DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS fun_expr
| simple_expr DOT mod_longident DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| fun_expr attribute
| UNDERSCORE
  {}

simple_expr:
| LPAREN seq_expr RPAREN
| LPAREN seq_expr type_constraint RPAREN
| simple_expr DOT LPAREN seq_expr RPAREN
| simple_expr DOT LBRACE seq_expr RBRACE
| simple_expr DOT LBRACKET seq_expr RBRACKET
| simple_expr DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN
| simple_expr DOT mod_longident DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN
| simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE
| simple_expr DOT mod_longident DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE
| simple_expr DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET
| simple_expr DOT mod_longident DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET
| METAOCAML_ESCAPE simple_expr
| METAOCAML_BRACKET_OPEN seq_expr METAOCAML_BRACKET_CLOSE
| BEGIN ext list_attribute_ seq_expr END
| BEGIN ext list_attribute_ END
| NEW ext list_attribute_ class_longident
| LPAREN MODULE ext list_attribute_ module_expr RPAREN
| LPAREN MODULE ext list_attribute_ module_expr COLON module_type RPAREN
| OBJECT ext list_attribute_ class_self_pattern list_text_cstr_class_field__ END
| val_longident
| constant
| constr_longident %prec prec_constant_constructor
| name_tag %prec prec_constant_constructor
| PREFIXOP simple_expr
| BANG simple_expr
| LBRACELESS separated_or_terminated_nonempty_list_SEMI_object_expr_field_ GREATERRBRACE
| LBRACELESS GREATERRBRACE
| simple_expr DOT label_longident
| mod_longident DOT LPAREN seq_expr RPAREN
| mod_longident DOT LBRACELESS separated_or_terminated_nonempty_list_SEMI_object_expr_field_ GREATERRBRACE
| simple_expr HASH LIDENT
| simple_expr HASHOP simple_expr
| extension
| mod_longident DOT LPAREN RPAREN
| LBRACE record_expr_content RBRACE
| mod_longident DOT LBRACE record_expr_content RBRACE
| LBRACKETBAR separated_or_terminated_nonempty_list_SEMI_expr_ BARRBRACKET
| LBRACKETBAR BARRBRACKET
| mod_longident DOT LBRACKETBAR separated_or_terminated_nonempty_list_SEMI_expr_ BARRBRACKET
| mod_longident DOT LBRACKETBAR BARRBRACKET
| LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET
| mod_longident DOT LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET
| mod_longident DOT LBRACKET RBRACKET
| mod_longident DOT LPAREN MODULE ext list_attribute_ module_expr COLON module_type RPAREN
  {}

labeled_simple_expr:
| simple_expr %prec below_HASH
| LABEL simple_expr %prec below_HASH
| TILDE LIDENT
| TILDE LPAREN LIDENT type_constraint RPAREN
| QUESTION LIDENT
| OPTLABEL simple_expr %prec below_HASH
  {}

let_binding_body_no_punning:
| val_ident strict_binding
| val_ident type_constraint EQUAL seq_expr
| val_ident COLON reversed_nonempty_llist_typevar_ DOT core_type EQUAL seq_expr
| val_ident COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL seq_expr
| pattern_no_exn EQUAL seq_expr
| simple_pattern_not_ident COLON core_type EQUAL seq_expr
  {}

let_binding_body:
| let_binding_body_no_punning
| val_ident %prec below_HASH
  {}

let_bindings_ext_:
| LET ext list_attribute_ rec_flag let_binding_body list_post_item_attribute_
| let_bindings_ext_ and_let_binding
  {}

let_bindings_no_ext_:
| LET list_attribute_ rec_flag let_binding_body list_post_item_attribute_
| LET PERCENT attr_id list_attribute_ rec_flag let_binding_body list_post_item_attribute_
| let_bindings_no_ext_ and_let_binding
  {}

and_let_binding:
| AND list_attribute_ let_binding_body list_post_item_attribute_
  {}

letop_binding_body:
| val_ident strict_binding
| val_ident
| simple_pattern COLON core_type EQUAL seq_expr
| pattern_no_exn EQUAL seq_expr
  {}

letop_bindings:
| letop_binding_body
| letop_bindings ANDOP letop_binding_body
  {}

strict_binding:
| EQUAL seq_expr
| fun_params option_type_constraint_ EQUAL fun_body
  {}

fun_body:
| FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| fun_seq_expr
  {}

match_case:
| pattern MINUSGREATER seq_expr
| pattern WHEN seq_expr MINUSGREATER seq_expr
| pattern MINUSGREATER DOT
  {}

fun_param_as_list:
| LPAREN TYPE nonempty_list_mkrhs_LIDENT__ RPAREN
| simple_param_pattern
  {}

fun_params:
| reversed_nonempty_concat_fun_param_as_list_
  {}

reversed_labeled_tuple_body:
| reversed_labeled_tuple_body COMMA fun_expr
| reversed_labeled_tuple_body COMMA FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| reversed_labeled_tuple_body COMMA LABEL simple_expr %prec below_HASH
| reversed_labeled_tuple_body COMMA TILDE LIDENT
| reversed_labeled_tuple_body COMMA TILDE LPAREN LIDENT type_constraint RPAREN %prec below_HASH
| fun_expr COMMA fun_expr
| fun_expr COMMA FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| fun_expr COMMA LABEL simple_expr %prec below_HASH
| fun_expr COMMA TILDE LIDENT
| fun_expr COMMA TILDE LPAREN LIDENT type_constraint RPAREN %prec below_HASH
| FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COMMA fun_expr
| FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COMMA FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COMMA LABEL simple_expr %prec below_HASH
| FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COMMA TILDE LIDENT
| FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COMMA TILDE LPAREN LIDENT type_constraint RPAREN %prec below_HASH
| LABEL simple_expr COMMA fun_expr
| LABEL simple_expr COMMA FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| LABEL simple_expr COMMA LABEL simple_expr %prec below_HASH
| LABEL simple_expr COMMA TILDE LIDENT
| LABEL simple_expr COMMA TILDE LPAREN LIDENT type_constraint RPAREN %prec below_HASH
| TILDE LIDENT COMMA fun_expr
| TILDE LIDENT COMMA FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| TILDE LIDENT COMMA LABEL simple_expr %prec below_HASH
| TILDE LIDENT COMMA TILDE LIDENT
| TILDE LIDENT COMMA TILDE LPAREN LIDENT type_constraint RPAREN %prec below_HASH
| TILDE LPAREN LIDENT type_constraint RPAREN COMMA fun_expr
| TILDE LPAREN LIDENT type_constraint RPAREN COMMA FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
| TILDE LPAREN LIDENT type_constraint RPAREN COMMA LABEL simple_expr %prec below_HASH
| TILDE LPAREN LIDENT type_constraint RPAREN COMMA TILDE LIDENT
| TILDE LPAREN LIDENT type_constraint RPAREN COMMA TILDE LPAREN LIDENT type_constraint RPAREN %prec below_HASH
  {}

record_expr_content:
| separated_or_terminated_nonempty_list_SEMI_record_expr_field_
| simple_expr WITH separated_or_terminated_nonempty_list_SEMI_record_expr_field_
  {}

type_constraint:
| COLON core_type
| COLON core_type COLONGREATER core_type
| COLONGREATER core_type
  {}

pattern:
| pattern COLONCOLON pattern
| pattern attribute
| pattern_gen
| pattern AS val_ident
| labeled_tuple_pattern_pattern_
| pattern BAR pattern
| EXCEPTION ext list_attribute_ pattern %prec prec_constr_appl
| EFFECT pattern_gen COMMA simple_pattern
  {}

pattern_no_exn:
| pattern_no_exn COLONCOLON pattern
| pattern_no_exn attribute
| pattern_gen
| pattern_no_exn AS val_ident
| labeled_tuple_pattern_pattern_no_exn_
| pattern_no_exn BAR pattern
  {}

pattern_gen:
| simple_pattern
| constr_longident pattern %prec prec_constr_appl
| constr_longident LPAREN TYPE nonempty_list_mkrhs_LIDENT__ RPAREN simple_pattern
| name_tag pattern %prec prec_constr_appl
| LAZY ext list_attribute_ simple_pattern
  {}

simple_pattern:
| val_ident %prec below_EQUAL
| simple_pattern_not_ident
  {}

simple_pattern_not_ident:
| LPAREN pattern RPAREN
| simple_delimited_pattern
| LPAREN MODULE ext list_attribute_ module_name RPAREN
| LPAREN MODULE ext list_attribute_ module_name COLON module_type RPAREN
| UNDERSCORE
| signed_constant
| signed_constant DOTDOT signed_constant
| constr_longident
| name_tag
| HASH type_longident
| mod_longident DOT simple_delimited_pattern
| mod_longident DOT LBRACKET RBRACKET
| mod_longident DOT LPAREN RPAREN
| mod_longident DOT LPAREN pattern RPAREN
| LPAREN pattern COLON core_type RPAREN
| extension
  {}

simple_delimited_pattern:
| LBRACE listx_SEMI_record_pat_field_UNDERSCORE_ RBRACE
| LBRACKET separated_or_terminated_nonempty_list_SEMI_pattern_ RBRACKET
| LBRACKETBAR separated_or_terminated_nonempty_list_SEMI_pattern_ BARRBRACKET
| LBRACKETBAR BARRBRACKET
  {}

labeled_tuple_pat_element_list_pattern_:
| labeled_tuple_pat_element_list_pattern_ COMMA pattern
| labeled_tuple_pat_element_list_pattern_ COMMA LABEL simple_pattern %prec COMMA
| labeled_tuple_pat_element_list_pattern_ COMMA TILDE LIDENT
| labeled_tuple_pat_element_list_pattern_ COMMA TILDE LPAREN LIDENT COLON core_type RPAREN %prec COMMA
| pattern COMMA pattern
| pattern COMMA LABEL simple_pattern %prec COMMA
| pattern COMMA TILDE LIDENT
| pattern COMMA TILDE LPAREN LIDENT COLON core_type RPAREN %prec COMMA
| LABEL simple_pattern COMMA pattern
| LABEL simple_pattern COMMA LABEL simple_pattern %prec COMMA
| LABEL simple_pattern COMMA TILDE LIDENT
| LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON core_type RPAREN %prec COMMA
| TILDE LIDENT COMMA pattern
| TILDE LIDENT COMMA LABEL simple_pattern %prec COMMA
| TILDE LIDENT COMMA TILDE LIDENT
| TILDE LIDENT COMMA TILDE LPAREN LIDENT COLON core_type RPAREN %prec COMMA
| TILDE LPAREN LIDENT COLON core_type RPAREN COMMA pattern
| TILDE LPAREN LIDENT COLON core_type RPAREN COMMA LABEL simple_pattern %prec COMMA
| TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LIDENT
| TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN LIDENT COLON core_type RPAREN %prec COMMA
  {}

labeled_tuple_pat_element_list_pattern_no_exn_:
| labeled_tuple_pat_element_list_pattern_no_exn_ COMMA pattern_no_exn
| labeled_tuple_pat_element_list_pattern_no_exn_ COMMA LABEL simple_pattern %prec COMMA
| labeled_tuple_pat_element_list_pattern_no_exn_ COMMA TILDE LIDENT
| labeled_tuple_pat_element_list_pattern_no_exn_ COMMA TILDE LPAREN LIDENT COLON core_type RPAREN %prec COMMA
| pattern_no_exn COMMA pattern_no_exn
| pattern_no_exn COMMA LABEL simple_pattern %prec COMMA
| pattern_no_exn COMMA TILDE LIDENT
| pattern_no_exn COMMA TILDE LPAREN LIDENT COLON core_type RPAREN %prec COMMA
| LABEL simple_pattern COMMA pattern_no_exn
| LABEL simple_pattern COMMA LABEL simple_pattern %prec COMMA
| LABEL simple_pattern COMMA TILDE LIDENT
| LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON core_type RPAREN %prec COMMA
| TILDE LIDENT COMMA pattern_no_exn
| TILDE LIDENT COMMA LABEL simple_pattern %prec COMMA
| TILDE LIDENT COMMA TILDE LIDENT
| TILDE LIDENT COMMA TILDE LPAREN LIDENT COLON core_type RPAREN %prec COMMA
| TILDE LPAREN LIDENT COLON core_type RPAREN COMMA pattern_no_exn
| TILDE LPAREN LIDENT COLON core_type RPAREN COMMA LABEL simple_pattern %prec COMMA
| TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LIDENT
| TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN LIDENT COLON core_type RPAREN %prec COMMA
  {}

reversed_labeled_tuple_pattern_pattern_:
| labeled_tuple_pat_element_list_pattern_ %prec below_COMMA
| labeled_tuple_pat_element_list_pattern_ COMMA DOTDOT
| pattern COMMA DOTDOT
| LABEL simple_pattern COMMA DOTDOT
| TILDE LIDENT COMMA DOTDOT
| TILDE LPAREN LIDENT COLON core_type RPAREN COMMA DOTDOT
  {}

reversed_labeled_tuple_pattern_pattern_no_exn_:
| labeled_tuple_pat_element_list_pattern_no_exn_ %prec below_COMMA
| labeled_tuple_pat_element_list_pattern_no_exn_ COMMA DOTDOT
| pattern_no_exn COMMA DOTDOT
| LABEL simple_pattern COMMA DOTDOT
| TILDE LIDENT COMMA DOTDOT
| TILDE LPAREN LIDENT COLON core_type RPAREN COMMA DOTDOT
  {}

labeled_tuple_pattern_pattern_:
| reversed_labeled_tuple_pattern_pattern_
  {}

labeled_tuple_pattern_pattern_no_exn_:
| reversed_labeled_tuple_pattern_pattern_no_exn_
  {}

value_description:
| VAL ext list_attribute_ val_ident COLON possibly_poly_core_type_ list_post_item_attribute_
  {}

primitive_declaration:
| EXTERNAL ext list_attribute_ val_ident COLON possibly_poly_core_type_ EQUAL nonempty_list_raw_string_ list_post_item_attribute_
  {}

generic_type_declaration_no_nonrec_flag_type_subst_kind_:
| TYPE ext list_attribute_ type_parameters LIDENT COLONEQUAL nonempty_type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_
| TYPE ext list_attribute_ NONREC type_parameters LIDENT COLONEQUAL nonempty_type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_
  {}

generic_type_declaration_nonrec_flag_type_kind_:
| TYPE ext list_attribute_ type_parameters LIDENT type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_
| TYPE ext list_attribute_ NONREC type_parameters LIDENT type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_
  {}

nonempty_type_kind:
| core_type
| PRIVATE core_type
| constructor_declarations
| PRIVATE constructor_declarations
| core_type EQUAL constructor_declarations
| core_type EQUAL PRIVATE constructor_declarations
| DOTDOT
| PRIVATE DOTDOT
| core_type EQUAL DOTDOT
| core_type EQUAL PRIVATE DOTDOT
| LBRACE label_declarations RBRACE
| PRIVATE LBRACE label_declarations RBRACE
| core_type EQUAL LBRACE label_declarations RBRACE
| core_type EQUAL PRIVATE LBRACE label_declarations RBRACE
| EXTERNAL STRING
  {}

type_kind:
| 
| EQUAL nonempty_type_kind
  {}

type_parameters:
| 
| type_parameter
| LPAREN reversed_separated_nonempty_llist_COMMA_type_parameter_ RPAREN
  {}

type_parameter:
| type_variance type_variable
  {}

type_variable:
| QUOTE ident
| UNDERSCORE
  {}

type_variance:
| 
| PLUS
| MINUS
| BANG
| PLUS BANG
| BANG PLUS
| MINUS BANG
| BANG MINUS
| INFIXOP2
| PREFIXOP
  {}

constructor_declarations:
| BAR
| reversed_bar_llist_constructor_declaration_
  {}

generic_constructor_declaration_BAR_:
| BAR constr_ident generalized_constructor_arguments list_attribute_
  {}

generic_constructor_declaration_epsilon_:
| constr_ident generalized_constructor_arguments list_attribute_
  {}

str_exception_declaration:
| EXCEPTION ext list_attribute_ constr_ident EQUAL constr_longident list_attribute_ list_post_item_attribute_
  {}

sig_exception_declaration:
| EXCEPTION ext list_attribute_ constr_ident generalized_constructor_arguments list_attribute_ list_post_item_attribute_
  {}

generalized_constructor_arguments:
| 
| OF constructor_arguments
| COLON constructor_arguments MINUSGREATER atomic_type %prec below_HASH
| COLON reversed_nonempty_llist_typevar_ DOT constructor_arguments MINUSGREATER atomic_type %prec below_HASH
| COLON atomic_type %prec below_HASH
| COLON reversed_nonempty_llist_typevar_ DOT atomic_type %prec below_HASH
  {}

constructor_arguments:
| atomic_type %prec below_HASH
| reversed_separated_nonempty_llist_STAR_atomic_type_ STAR atomic_type %prec below_HASH
| LBRACE label_declarations RBRACE
  {}

label_declarations:
| label_declaration
| label_declaration_semi
| label_declaration_semi label_declarations
  {}

label_declaration:
| mutable_flag LIDENT COLON possibly_poly_core_type_no_attr_ list_attribute_
  {}

label_declaration_semi:
| mutable_flag LIDENT COLON possibly_poly_core_type_no_attr_ list_attribute_ SEMI list_attribute_
  {}

extension_constructor_rebind_BAR_:
| BAR constr_ident EQUAL constr_longident list_attribute_
  {}

extension_constructor_rebind_epsilon_:
| constr_ident EQUAL constr_longident list_attribute_
  {}

with_constraint:
| TYPE type_parameters label_longident with_type_binder alias_type reversed_llist_preceded_CONSTRAINT_constrain__
| TYPE type_parameters label_longident COLONEQUAL alias_type
| MODULE mod_longident EQUAL mod_ext_longident
| MODULE mod_longident COLONEQUAL mod_ext_longident
| MODULE TYPE mty_longident EQUAL module_type
| MODULE TYPE mty_longident COLONEQUAL module_type
  {}

with_type_binder:
| EQUAL
| EQUAL PRIVATE
  {}

possibly_poly_core_type_:
| core_type
| reversed_nonempty_llist_typevar_ DOT core_type
  {}

possibly_poly_core_type_no_attr_:
| alias_type
| reversed_nonempty_llist_typevar_ DOT alias_type
  {}

core_type:
| alias_type
| core_type attribute
  {}

alias_type:
| function_type
| alias_type AS QUOTE ident
  {}

function_type:
| tuple_type %prec MINUSGREATER
| optlabel LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER function_type
| optlabel tuple_type MINUSGREATER function_type
| LIDENT COLON LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER function_type
| LIDENT COLON tuple_type MINUSGREATER function_type
| LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER function_type
| tuple_type MINUSGREATER function_type
| LIDENT COLON atomic_type STAR reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ MINUSGREATER function_type
| LIDENT COLON atomic_type STAR reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ %prec MINUSGREATER
  {}

tuple_type:
| atomic_type %prec below_HASH
| atomic_type STAR reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ %prec below_WITH
  {}

delimited_type_supporting_local_open:
| LPAREN core_type RPAREN
| LPAREN MODULE ext list_attribute_ module_type RPAREN
| LBRACKET tag_field RBRACKET
| LBRACKET BAR reversed_separated_nonempty_llist_BAR_row_field_ RBRACKET
| LBRACKET row_field BAR reversed_separated_nonempty_llist_BAR_row_field_ RBRACKET
| LBRACKETGREATER option_BAR_ reversed_separated_nonempty_llist_BAR_row_field_ RBRACKET
| LBRACKETGREATER RBRACKET
| LBRACKETLESS option_BAR_ reversed_separated_nonempty_llist_BAR_row_field_ RBRACKET
| LBRACKETLESS option_BAR_ reversed_separated_nonempty_llist_BAR_row_field_ GREATER reversed_nonempty_llist_name_tag_ RBRACKET
  {}

object_type:
| LESS meth_list GREATER
| LESS GREATER
  {}

extension_type:
| extension
  {}

delimited_type:
| object_type
| extension_type
| delimited_type_supporting_local_open
  {}

atomic_type:
| delimited_type
| type_longident
| atomic_type type_longident
| LPAREN reversed_separated_nontrivial_llist_COMMA_core_type_ RPAREN type_longident
| HASH clty_longident
| atomic_type HASH clty_longident
| LPAREN reversed_separated_nontrivial_llist_COMMA_core_type_ RPAREN HASH clty_longident
| mod_ext_longident DOT delimited_type_supporting_local_open
| QUOTE ident
| UNDERSCORE
  {}

row_field:
| tag_field
| core_type
  {}

tag_field:
| name_tag OF opt_ampersand reversed_separated_nonempty_llist_AMPERSAND_core_type_no_attr_ list_attribute_
| name_tag list_attribute_
  {}

opt_ampersand:
| AMPERSAND
| 
  {}

meth_list:
| LIDENT COLON possibly_poly_core_type_no_attr_ list_attribute_ SEMI list_attribute_ meth_list
| atomic_type SEMI meth_list
| LIDENT COLON possibly_poly_core_type_no_attr_ list_attribute_ SEMI list_attribute_
| atomic_type SEMI
| LIDENT COLON possibly_poly_core_type_no_attr_ list_attribute_
| atomic_type
| DOTDOT
  {}

constant:
| INT
| CHAR
| STRING
| FLOAT
  {}

signed_constant:
| constant
| MINUS INT
| MINUS FLOAT
| PLUS INT
| PLUS FLOAT
  {}

ident:
| UIDENT
| LIDENT
  {}

val_extra_ident:
| LPAREN operator RPAREN
  {}

val_ident:
| LIDENT
| val_extra_ident
  {}

operator:
| PREFIXOP
| LETOP
| ANDOP
| DOTOP LPAREN index_mod RPAREN
| DOTOP LPAREN index_mod RPAREN LESSMINUS
| DOTOP LBRACKET index_mod RBRACKET
| DOTOP LBRACKET index_mod RBRACKET LESSMINUS
| DOTOP LBRACE index_mod RBRACE
| DOTOP LBRACE index_mod RBRACE LESSMINUS
| HASHOP
| BANG
| INFIXOP0
| INFIXOP1
| INFIXOP2
| INFIXOP3
| INFIXOP4
| PLUS
| PLUSDOT
| PLUSEQ
| MINUS
| MINUSDOT
| STAR
| PERCENT
| EQUAL
| LESS
| GREATER
| OR
| BARBAR
| AMPERSAND
| AMPERAMPER
| COLONEQUAL
  {}

index_mod:
| 
| SEMI DOTDOT
  {}

constr_extra_nonprefix_ident:
| LBRACKET RBRACKET
| LPAREN RPAREN
| FALSE
| TRUE
  {}

constr_ident:
| UIDENT
| LPAREN COLONCOLON RPAREN
| constr_extra_nonprefix_ident
  {}

constr_longident:
| mod_longident %prec below_DOT
| mod_longident DOT LPAREN COLONCOLON RPAREN
| LPAREN COLONCOLON RPAREN
| constr_extra_nonprefix_ident
  {}

mk_longident_mod_ext_longident_LIDENT_:
| LIDENT
| mod_ext_longident DOT LIDENT
  {}

mk_longident_mod_ext_longident_UIDENT_:
| UIDENT
| mod_ext_longident DOT UIDENT
  {}

mk_longident_mod_ext_longident___anonymous_42_:
| ident
| LPAREN COLONCOLON RPAREN
| val_extra_ident
| mod_ext_longident DOT ident
| mod_ext_longident DOT LPAREN COLONCOLON RPAREN
| mod_ext_longident DOT val_extra_ident
  {}

mk_longident_mod_ext_longident_ident_:
| ident
| mod_ext_longident DOT ident
  {}

mk_longident_mod_longident_LIDENT_:
| LIDENT
| mod_longident DOT LIDENT
  {}

mk_longident_mod_longident_UIDENT_:
| UIDENT
| mod_longident DOT UIDENT
  {}

mk_longident_mod_longident_val_ident_:
| val_ident
| mod_longident DOT val_ident
  {}

val_longident:
| mk_longident_mod_longident_val_ident_
  {}

label_longident:
| mk_longident_mod_longident_LIDENT_
  {}

type_longident:
| mk_longident_mod_ext_longident_LIDENT_
  {}

mod_longident:
| mk_longident_mod_longident_UIDENT_
  {}

mod_ext_longident:
| mk_longident_mod_ext_longident_UIDENT_
| mod_ext_longident LPAREN mod_ext_longident RPAREN
  {}

mty_longident:
| mk_longident_mod_ext_longident_ident_
  {}

clty_longident:
| mk_longident_mod_ext_longident_LIDENT_
  {}

class_longident:
| mk_longident_mod_longident_LIDENT_
  {}

any_longident:
| mk_longident_mod_ext_longident___anonymous_42_
| constr_extra_nonprefix_ident
  {}

toplevel_directive:
| HASH ident
| HASH ident STRING
| HASH ident INT
| HASH ident val_longident
| HASH ident mod_longident
| HASH ident FALSE
| HASH ident TRUE
  {}

name_tag:
| BACKQUOTE ident
  {}

rec_flag:
| 
| REC
  {}

direction_flag:
| TO
| DOWNTO
  {}

private_flag:
| 
| PRIVATE
  {}

mutable_flag:
| 
| MUTABLE
  {}

virtual_flag:
| 
| VIRTUAL
  {}

mutable_virtual_flags:
| 
| MUTABLE
| VIRTUAL
| MUTABLE VIRTUAL
| VIRTUAL MUTABLE
  {}

private_virtual_flags:
| 
| PRIVATE
| VIRTUAL
| PRIVATE VIRTUAL
| VIRTUAL PRIVATE
  {}

virtual_with_mutable_flag:
| VIRTUAL
| MUTABLE VIRTUAL
| VIRTUAL MUTABLE
  {}

virtual_with_private_flag:
| VIRTUAL
| PRIVATE VIRTUAL
| VIRTUAL PRIVATE
  {}

subtractive:
| MINUS
| MINUSDOT
  {}

additive:
| PLUS
| PLUSDOT
  {}

optlabel:
| OPTLABEL
| QUESTION LIDENT COLON
  {}

single_attr_id:
| LIDENT
| UIDENT
| AND
| AS
| ASSERT
| BEGIN
| CLASS
| CONSTRAINT
| DO
| DONE
| DOWNTO
| EFFECT
| ELSE
| END
| EXCEPTION
| EXTERNAL
| FALSE
| FOR
| FUN
| FUNCTION
| FUNCTOR
| IF
| IN
| INCLUDE
| INHERIT
| INITIALIZER
| LAZY
| LET
| MATCH
| METHOD
| MODULE
| MUTABLE
| NEW
| NONREC
| OBJECT
| OF
| OPEN
| OR
| PRIVATE
| REC
| SIG
| STRUCT
| THEN
| TO
| TRUE
| TRY
| TYPE
| VAL
| VIRTUAL
| WHEN
| WHILE
| WITH
  {}

attr_id:
| single_attr_id
| single_attr_id DOT attr_id
  {}

attribute:
| LBRACKETAT attr_id attr_payload RBRACKET
  {}

post_item_attribute:
| LBRACKETATAT attr_id attr_payload RBRACKET
  {}

floating_attribute:
| LBRACKETATATAT attr_id attr_payload RBRACKET
  {}

ext:
| 
| PERCENT attr_id
  {}

extension:
| LBRACKETPERCENT attr_id payload RBRACKET
| QUOTED_STRING_EXPR
  {}

item_extension:
| LBRACKETPERCENTPERCENT attr_id payload RBRACKET
| QUOTED_STRING_ITEM
  {}

payload:
| structure
| COLON signature
| COLON core_type
| QUESTION pattern
| QUESTION pattern WHEN seq_expr
  {}

attr_payload:
| payload
  {}
