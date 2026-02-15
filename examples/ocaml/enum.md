## Pattern 1

```
| /use_file': . use_file
```

### Sample 1

Sentence:
```

```
Stack:
```
use_file:
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end
 = functor > >} >] ## in != ^ +! land ** inherit initializer [@ [@@ [> [< < <- >. method -> mutable nonrec of ?label: or % += private ? ' } ] rec ) ; sig * struct then to _ virtual when with
```

## Pattern 2

```
| /implementation': . implementation
```

### Sample 1

Sentence:
```

```
Stack:
```
implementation:
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end
 = functor > >} >] # ## in != ^ +! land ** inherit initializer [@ [@@ [> [< < <- >. method -> mutable nonrec of ?label: or % += private ? ' } ] rec ) ; sig * struct then to _ virtual when with
```

## Pattern 3

```
| /interface': . interface
```

### Sample 1

Sentence:
```

```
Stack:
```
interface:
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end
 = false 1.0 for fun function functor > >} >] # ## if in != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [| [> [< [% < <- let let* x ( match >. .< .~ method - -. -> mutable new nonrec object of ?label: or % + +. += !+ private ? ' {%%ext|s|} } ] rec ) ; sig * "s" struct then ~ to true try X _ virtual when while with
```

## Pattern 4

```
| /parse_any_longident': . parse_any_longident
```

### Sample 1

Sentence:
```

```
Stack:
```
parse_any_longident:
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```

## Pattern 5

```
| /parse_constr_longident': . parse_constr_longident
```

### Sample 1

Sentence:
```

```
Stack:
```
parse_constr_longident:
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```

## Pattern 6

```
| /parse_core_type': . parse_core_type
```

### Sample 1

Sentence:
```

```
Stack:
```
parse_core_type:
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

## Pattern 7

```
| /parse_expression': . parse_expression
```

### Sample 1

Sentence:
```

```
Stack:
```
parse_expression:
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

## Pattern 8

```
| /parse_mod_ext_longident': . parse_mod_ext_longident
```

### Sample 1

Sentence:
```

```
Stack:
```
parse_mod_ext_longident:
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 9

```
| /parse_mod_longident': . parse_mod_longident
```

### Sample 1

Sentence:
```

```
Stack:
```
parse_mod_longident:
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 10

```
| /parse_module_expr': . parse_module_expr
```

### Sample 1

Sentence:
```

```
Stack:
```
parse_module_expr:
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ val virtual when while with
```

## Pattern 11

```
| /parse_module_type': . parse_module_type
```

### Sample 1

Sentence:
```

```
Stack:
```
parse_module_type:
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 12

```
| /parse_mty_longident': . parse_mty_longident
```

### Sample 1

Sentence:
```

```
Stack:
```
parse_mty_longident:
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 13

```
| /parse_pattern': . parse_pattern
```

### Sample 1

Sentence:
```

```
Stack:
```
parse_pattern:
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

## Pattern 14

```
| /parse_val_longident': . parse_val_longident
```

### Sample 1

Sentence:
```

```
Stack:
```
parse_val_longident:
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 15

```
| /toplevel_phrase': . toplevel_phrase
```

### Sample 1

Sentence:
```

```
Stack:
```
toplevel_phrase:
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end
 = functor > >} >] ## in != ^ +! land ** inherit initializer [@ [@@ [> [< < <- >. method -> mutable nonrec of ?label: or % += private ? ' } ] rec ) ; sig * struct then to _ virtual when with
```

## Pattern 16

```
| [_* /value_description: VAL ext list_attribute_ . val_ident COLON possibly_poly_core_type_ list_post_item_attribute_]
```

### Sample 1

Sentence:
```
val [@ and ]
```
Stack:
```
use_file: VAL ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
val % and
```
Stack:
```
use_file: VAL PERCENT AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 17

```
| [_* /operator: DOTOP LPAREN index_mod . RPAREN _*]
```

### Sample 1

Sentence:
```
( .+ ( ; ..
```
Stack:
```
parse_any_longident: LPAREN DOTOP LPAREN SEMI DOTDOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 18

```
| [_* /operator: DOTOP LBRACKET index_mod . RBRACKET _*]
```

### Sample 1

Sentence:
```
( .+ [ ; ..
```
Stack:
```
parse_any_longident: LPAREN DOTOP LBRACKET SEMI DOTDOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 19

```
| [_* /operator: DOTOP LBRACE index_mod . RBRACE _*]
```

### Sample 1

Sentence:
```
( .+ { ; ..
```
Stack:
```
parse_any_longident: LPAREN DOTOP LBRACE SEMI DOTDOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 20

```
| [_* /val_extra_ident: LPAREN operator . RPAREN]
```

### Sample 1

Sentence:
```
( &&
```
Stack:
```
parse_val_longident: LPAREN AMPERAMPER
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
( -
```
Stack:
```
parse_pattern: LPAREN MINUS
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
( let*
```
Stack:
```
use_file: LPAREN LETOP
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ; ;; sig * struct then to try type val virtual when while with
```

### Sample 4

Sentence:
```
( !
```
Stack:
```
use_file: LPAREN BANG
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy [@ [@@ [@@@ [> [< [%% < <- let let* match >. method - -. -> module mutable nonrec of open ?label: or % + +. += private ? ' {%%%%ext|s|} } ] rec ; ;; sig * struct then ~ to try type _ val virtual when while with
```

## Pattern 21

```
| [_* /value_description: VAL ext list_attribute_ val_ident . COLON possibly_poly_core_type_ list_post_item_attribute_]
```

### Sample 1

Sentence:
```
val x
```
Stack:
```
use_file: VAL ext list_attribute_ LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 22

```
| [_* /delimited_type_supporting_local_open: LPAREN MODULE ext list_attribute_ . module_type RPAREN]
```

### Sample 1

Sentence:
```
( module [@ and ]
```
Stack:
```
parse_core_type: LPAREN MODULE ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 2

Sentence:
```
( module % and
```
Stack:
```
parse_core_type: LPAREN MODULE PERCENT AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 23

```
| [_* /type_parameter: type_variance . type_variable]
```

### Sample 1

Sentence:
```
type +!
```
Stack:
```
interface: TYPE ext list_attribute_ INFIXOP2
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X val virtual when while with
```

### Sample 2

Sentence:
```
type !
```
Stack:
```
interface: TYPE ext list_attribute_ BANG
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X val virtual when while with
```

## Pattern 24

```
| [_* /type_parameters: LPAREN reversed_separated_nonempty_llist_COMMA_type_parameter_ . RPAREN]
```

### Sample 1

Sentence:
```
type x := false and ( _
```
Stack:
```
interface: generic_type_declaration_no_nonrec_flag_type_subst_kind_ AND list_attribute_ LPAREN type_variance UNDERSCORE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 25

```
| [_* /generic_type_declaration_nonrec_flag_type_kind_: TYPE ext list_attribute_ NONREC type_parameters . LIDENT type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_]
```

### Sample 1

Sentence:
```
type nonrec _
```
Stack:
```
interface: TYPE ext list_attribute_ NONREC type_variance UNDERSCORE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 26

```
| [_* /generic_type_declaration_nonrec_flag_type_kind_: TYPE ext list_attribute_ type_parameters . LIDENT type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_
      /local_structure_item: TYPE ext list_attribute_ type_parameters . type_longident PLUSEQ private_flag reversed_bar_llist_extension_constructor_ list_post_item_attribute_]
```

### Sample 1

Sentence:
```
type _
```
Stack:
```
use_file: TYPE ext list_attribute_ type_variance UNDERSCORE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 2

Sentence:
```
type % and
```
Stack:
```
use_file: TYPE PERCENT AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method -. -> module mutable new object of open ?label: or % +. += private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

## Pattern 27

```
| [_* /fun_expr: TRY ext list_attribute_ . seq_expr WITH reversed_preceded_or_separated_nonempty_llist_BAR_match_case_]
```

### Sample 1

Sentence:
```
try [@ and ]
```
Stack:
```
use_file: TRY ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

### Sample 2

Sentence:
```
try % and
```
Stack:
```
use_file: TRY PERCENT AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

## Pattern 28

```
| [_* /delimited_type_supporting_local_open: LBRACKETLESS option_BAR_ . reversed_separated_nonempty_llist_BAR_row_field_ _*]
```

### Sample 1

Sentence:
```
[< |
```
Stack:
```
parse_core_type: LBRACKETLESS BAR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

## Pattern 29

```
| [_* /delimited_type_supporting_local_open: LBRACKETGREATER option_BAR_ . reversed_separated_nonempty_llist_BAR_row_field_ RBRACKET]
```

### Sample 1

Sentence:
```
[> |
```
Stack:
```
parse_core_type: LBRACKETGREATER BAR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

## Pattern 30

```
| [_* /mk_longident_mod_ext_longident_LIDENT_: mod_ext_longident . DOT LIDENT
      /mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident . DOT UIDENT]
```

### Sample 1

Sentence:
```
type X
```
Stack:
```
use_file: TYPE ext list_attribute_ type_parameters UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 31

```
| [_* /mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident . DOT UIDENT
      /mod_ext_longident: mod_ext_longident LPAREN mod_ext_longident . RPAREN]
```

### Sample 1

Sentence:
```
X ( X
```
Stack:
```
parse_any_longident: mod_ext_longident LPAREN UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 32

```
| [_* /function_type: optlabel . LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER function_type
      /function_type: optlabel . tuple_type MINUSGREATER function_type]
```

### Sample 1

Sentence:
```
?label:
```
Stack:
```
parse_core_type: OPTLABEL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

## Pattern 33

```
| [_* /atomic_type: LPAREN reversed_separated_nontrivial_llist_COMMA_core_type_ . RPAREN _*]
```

### Sample 1

Sentence:
```
( {%%ext|s|} , {%%ext|s|} [@ and ]
```
Stack:
```
parse_core_type: LPAREN core_type COMMA core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
( {%%ext|s|} , {%%ext|s|} , {%%ext|s|} as ' x
```
Stack:
```
parse_core_type: LPAREN reversed_separated_nontrivial_llist_COMMA_core_type_ COMMA alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
( {%%ext|s|} , {%%ext|s|} , {%%ext|s|}
```
Stack:
```
parse_core_type: LPAREN reversed_separated_nontrivial_llist_COMMA_core_type_ COMMA QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
( {%%ext|s|} , {%%ext|s|} , x
```
Stack:
```
parse_core_type: LPAREN reversed_separated_nontrivial_llist_COMMA_core_type_ COMMA LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 34

```
| [_* /atomic_type: mod_ext_longident . DOT delimited_type_supporting_local_open
      /mk_longident_mod_ext_longident_LIDENT_: mod_ext_longident . DOT LIDENT
      /mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident . DOT UIDENT]
```

### Sample 1

Sentence:
```
X
```
Stack:
```
parse_core_type: UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 35

```
| [_* /delimited_type_supporting_local_open: LPAREN core_type . RPAREN]
```

### Sample 1

Sentence:
```
X . ( {%%ext|s|} [@ and ]
```
Stack:
```
parse_core_type: mod_ext_longident DOT LPAREN core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
X . ( {%%ext|s|} as ' x
```
Stack:
```
parse_core_type: mod_ext_longident DOT LPAREN alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
X . ( {%%ext|s|}
```
Stack:
```
parse_core_type: mod_ext_longident DOT LPAREN QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
X . ( x
```
Stack:
```
parse_core_type: mod_ext_longident DOT LPAREN LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 36

```
| [_* /function_type: optlabel LPAREN reversed_nonempty_llist_typevar_ . DOT core_type RPAREN MINUSGREATER function_type]
```

### Sample 1

Sentence:
```
?label: ( ' x ' x
```
Stack:
```
parse_core_type: optlabel LPAREN reversed_nonempty_llist_typevar_ QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
?label: ( ' x
```
Stack:
```
parse_core_type: optlabel LPAREN QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 37

```
| [_* /function_type: optlabel LPAREN reversed_nonempty_llist_typevar_ DOT core_type . RPAREN MINUSGREATER function_type]
```

### Sample 1

Sentence:
```
?label: ( ' x . {%%ext|s|} [@ and ]
```
Stack:
```
parse_core_type: optlabel LPAREN reversed_nonempty_llist_typevar_ DOT core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
?label: ( ' x . {%%ext|s|} as ' x
```
Stack:
```
parse_core_type: optlabel LPAREN reversed_nonempty_llist_typevar_ DOT alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
?label: ( ' x . {%%ext|s|}
```
Stack:
```
parse_core_type: optlabel LPAREN reversed_nonempty_llist_typevar_ DOT QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
?label: ( ' x . x
```
Stack:
```
parse_core_type: optlabel LPAREN reversed_nonempty_llist_typevar_ DOT LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 38

```
| [_* /delimited_type_supporting_local_open: LPAREN core_type . RPAREN
      /reversed_separated_nontrivial_llist_COMMA_core_type_: core_type . COMMA core_type]
```

### Sample 1

Sentence:
```
( {%%ext|s|} [@ and ]
```
Stack:
```
parse_core_type: LPAREN core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
( {%%ext|s|} as ' x
```
Stack:
```
parse_core_type: LPAREN alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
( {%%ext|s|}
```
Stack:
```
parse_core_type: LPAREN QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
( x
```
Stack:
```
parse_core_type: LPAREN LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 39

```
| [_* /function_type: optlabel tuple_type . MINUSGREATER function_type]
```

### Sample 1

Sentence:
```
?label: x
```
Stack:
```
parse_core_type: optlabel LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 40

```
| [_* /delimited_type_supporting_local_open: LBRACKET BAR reversed_separated_nonempty_llist_BAR_row_field_ . RBRACKET]
```

### Sample 1

Sentence:
```
[ | ` x
```
Stack:
```
parse_core_type: LBRACKET BAR BACKQUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
[ | {%%ext|s|}
```
Stack:
```
parse_core_type: LBRACKET BAR QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 3

Sentence:
```
[ | x
```
Stack:
```
parse_core_type: LBRACKET BAR LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 41

```
| [_* /tag_field: name_tag OF opt_ampersand . reversed_separated_nonempty_llist_AMPERSAND_core_type_no_attr_ list_attribute_]
```

### Sample 1

Sentence:
```
[ ` x of &
```
Stack:
```
parse_core_type: LBRACKET name_tag OF AMPERSAND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

## Pattern 42

```
| [_* /delimited_type_supporting_local_open: LBRACKET row_field . BAR reversed_separated_nonempty_llist_BAR_row_field_ RBRACKET]
```

### Sample 1

Sentence:
```
[ ` x
```
Stack:
```
parse_core_type: LBRACKET BACKQUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
[ {%%ext|s|}
```
Stack:
```
parse_core_type: LBRACKET QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 3

Sentence:
```
[ x
```
Stack:
```
parse_core_type: LBRACKET LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 43

```
| [_* /delimited_type_supporting_local_open: LBRACKET row_field BAR reversed_separated_nonempty_llist_BAR_row_field_ . RBRACKET]
```

### Sample 1

Sentence:
```
[ {%%ext|s|} | ` x
```
Stack:
```
parse_core_type: LBRACKET row_field BAR BACKQUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
[ {%%ext|s|} | {%%ext|s|}
```
Stack:
```
parse_core_type: LBRACKET row_field BAR QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 3

Sentence:
```
[ {%%ext|s|} | x
```
Stack:
```
parse_core_type: LBRACKET row_field BAR LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 44

```
| [_* /delimited_type_supporting_local_open: LBRACKETGREATER option_BAR_ reversed_separated_nonempty_llist_BAR_row_field_ . RBRACKET]
```

### Sample 1

Sentence:
```
[> ` x
```
Stack:
```
parse_core_type: LBRACKETGREATER option_BAR_ BACKQUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
[> {%%ext|s|}
```
Stack:
```
parse_core_type: LBRACKETGREATER option_BAR_ QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 3

Sentence:
```
[> x
```
Stack:
```
parse_core_type: LBRACKETGREATER option_BAR_ LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 45

```
| [_* /delimited_type_supporting_local_open: LBRACKETLESS option_BAR_ reversed_separated_nonempty_llist_BAR_row_field_ . RBRACKET
      /delimited_type_supporting_local_open: LBRACKETLESS option_BAR_ reversed_separated_nonempty_llist_BAR_row_field_ . GREATER reversed_nonempty_llist_name_tag_ RBRACKET]
```

### Sample 1

Sentence:
```
[< ` x
```
Stack:
```
parse_core_type: LBRACKETLESS option_BAR_ BACKQUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
[< {%%ext|s|}
```
Stack:
```
parse_core_type: LBRACKETLESS option_BAR_ QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 3

Sentence:
```
[< x
```
Stack:
```
parse_core_type: LBRACKETLESS option_BAR_ LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 46

```
| [_* /delimited_type_supporting_local_open: LBRACKETLESS option_BAR_ reversed_separated_nonempty_llist_BAR_row_field_ GREATER reversed_nonempty_llist_name_tag_ . RBRACKET]
```

### Sample 1

Sentence:
```
[< {%%ext|s|} > ` x
```
Stack:
```
parse_core_type: LBRACKETLESS option_BAR_ reversed_separated_nonempty_llist_BAR_row_field_ GREATER BACKQUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 47

```
| [_* /reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint . RPAREN _*]
```

### Sample 1

Sentence:
```
~ ( x :> {%%ext|s|} [@ and ]
```
Stack:
```
use_file: TILDE LPAREN LIDENT COLONGREATER core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
~ ( x : {%%ext|s|} as ' x
```
Stack:
```
use_file: TILDE LPAREN LIDENT COLON alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
~ ( x :> {%%ext|s|}
```
Stack:
```
use_file: TILDE LPAREN LIDENT COLONGREATER QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
~ ( x : {%%ext|s|}
```
Stack:
```
use_file: TILDE LPAREN LIDENT COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 48

```
| [_* /reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint . RPAREN _*
      /reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint RPAREN COMMA TILDE LPAREN LIDENT type_constraint . RPAREN]
```

### Sample 1

Sentence:
```
~ ( x : {%%ext|s|} ) , ~ ( x :> {%%ext|s|} [@ and ]
```
Stack:
```
use_file: TILDE LPAREN LIDENT type_constraint RPAREN COMMA TILDE LPAREN LIDENT COLONGREATER core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
~ ( x : {%%ext|s|} ) , ~ ( x : {%%ext|s|} as ' x
```
Stack:
```
use_file: TILDE LPAREN LIDENT type_constraint RPAREN COMMA TILDE LPAREN LIDENT COLON alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
~ ( x : {%%ext|s|} ) , ~ ( x :> {%%ext|s|}
```
Stack:
```
use_file: TILDE LPAREN LIDENT type_constraint RPAREN COMMA TILDE LPAREN LIDENT COLONGREATER QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
~ ( x : {%%ext|s|} ) , ~ ( x : {%%ext|s|}
```
Stack:
```
use_file: TILDE LPAREN LIDENT type_constraint RPAREN COMMA TILDE LPAREN LIDENT COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 49

```
| [_* /reversed_labeled_tuple_body: TILDE LIDENT COMMA TILDE LPAREN LIDENT type_constraint . RPAREN
      /reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint . RPAREN _*]
```

### Sample 1

Sentence:
```
~ x , ~ ( x :> {%%ext|s|} [@ and ]
```
Stack:
```
use_file: TILDE LIDENT COMMA TILDE LPAREN LIDENT COLONGREATER core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
~ x , ~ ( x : {%%ext|s|} as ' x
```
Stack:
```
use_file: TILDE LIDENT COMMA TILDE LPAREN LIDENT COLON alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
~ x , ~ ( x :> {%%ext|s|}
```
Stack:
```
use_file: TILDE LIDENT COMMA TILDE LPAREN LIDENT COLONGREATER QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
~ x , ~ ( x : {%%ext|s|}
```
Stack:
```
use_file: TILDE LIDENT COMMA TILDE LPAREN LIDENT COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 50

```
| [_* /labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN _*
      /reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT]
```

### Sample 1

Sentence:
```
~ ( x : {%%ext|s|} [@ and ]
```
Stack:
```
parse_pattern: TILDE LPAREN LIDENT COLON core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
~ ( x : {%%ext|s|} as ' x
```
Stack:
```
parse_pattern: TILDE LPAREN LIDENT COLON alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
~ ( x : {%%ext|s|}
```
Stack:
```
parse_pattern: TILDE LPAREN LIDENT COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
~ ( x : x
```
Stack:
```
parse_pattern: TILDE LPAREN LIDENT COLON LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 51

```
| [_* /labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN _*
      /labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN
      /reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT]
```

### Sample 1

Sentence:
```
~ ( x : {%%ext|s|} ) , ~ ( x : {%%ext|s|} [@ and ]
```
Stack:
```
parse_pattern: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN LIDENT COLON core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
~ ( x : {%%ext|s|} ) , ~ ( x : {%%ext|s|} as ' x
```
Stack:
```
parse_pattern: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN LIDENT COLON alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
~ ( x : {%%ext|s|} ) , ~ ( x : {%%ext|s|}
```
Stack:
```
parse_pattern: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN LIDENT COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
~ ( x : {%%ext|s|} ) , ~ ( x : x
```
Stack:
```
parse_pattern: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN LIDENT COLON LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 52

```
| [_* /labeled_tuple_pat_element_list_pattern_: TILDE LIDENT COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN
      /labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN _*
      /reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT]
```

### Sample 1

Sentence:
```
~ x , ~ ( x : {%%ext|s|} [@ and ]
```
Stack:
```
parse_pattern: TILDE LIDENT COMMA TILDE LPAREN LIDENT COLON core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
~ x , ~ ( x : {%%ext|s|} as ' x
```
Stack:
```
parse_pattern: TILDE LIDENT COMMA TILDE LPAREN LIDENT COLON alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
~ x , ~ ( x : {%%ext|s|}
```
Stack:
```
parse_pattern: TILDE LIDENT COMMA TILDE LPAREN LIDENT COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
~ x , ~ ( x : x
```
Stack:
```
parse_pattern: TILDE LIDENT COMMA TILDE LPAREN LIDENT COLON LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 53

```
| [_* /simple_pattern_not_ident: LPAREN MODULE ext list_attribute_ . module_name _*]
```

### Sample 1

Sentence:
```
( module [@ and ]
```
Stack:
```
parse_pattern: LPAREN MODULE ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

### Sample 2

Sentence:
```
( module % and
```
Stack:
```
parse_pattern: LPAREN MODULE PERCENT AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

## Pattern 54

```
| [_* /simple_pattern_not_ident: LPAREN MODULE ext list_attribute_ module_name . RPAREN
      /simple_pattern_not_ident: LPAREN MODULE ext list_attribute_ module_name . COLON module_type RPAREN]
```

### Sample 1

Sentence:
```
( module X
```
Stack:
```
parse_pattern: LPAREN MODULE ext list_attribute_ UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 55

```
| [_* /module_type: MODULE TYPE OF list_attribute_ . module_expr]
```

### Sample 1

Sentence:
```
module type of [@ and ]
```
Stack:
```
parse_module_type: MODULE TYPE OF LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ val virtual when while with
```

## Pattern 56

```
| [_* /open_declaration: OPEN BANG ext list_attribute_ . module_expr list_post_item_attribute_]
```

### Sample 1

Sentence:
```
open ! [@ and ]
```
Stack:
```
use_file: OPEN BANG ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ val virtual when while with
```

### Sample 2

Sentence:
```
open ! % and
```
Stack:
```
use_file: OPEN BANG PERCENT AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ val virtual when while with
```

## Pattern 57

```
| [_* /paren_module_expr: LPAREN VAL list_attribute_ . expr_colon_package_type RPAREN]
```

### Sample 1

Sentence:
```
( val [@ and ]
```
Stack:
```
parse_module_expr: LPAREN VAL LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

## Pattern 58

```
| [_* /simple_expr: NEW ext list_attribute_ . class_longident]
```

### Sample 1

Sentence:
```
new [@ and ]
```
Stack:
```
use_file: NEW ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 2

Sentence:
```
new % and
```
Stack:
```
use_file: NEW PERCENT AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 59

```
| [_* /mk_longident_mod_longident_LIDENT_: mod_longident . DOT LIDENT
      /mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT]
```

### Sample 1

Sentence:
```
new X
```
Stack:
```
use_file: NEW ext list_attribute_ UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 60

```
| [_* /fun_expr: MATCH ext list_attribute_ . seq_expr WITH reversed_preceded_or_separated_nonempty_llist_BAR_match_case_]
```

### Sample 1

Sentence:
```
match [@ and ]
```
Stack:
```
use_file: MATCH ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

### Sample 2

Sentence:
```
match % and
```
Stack:
```
use_file: MATCH PERCENT AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

## Pattern 61

```
| [_* /labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . RPAREN _*
      /reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT]
```

### Sample 1

Sentence:
```
let* ~ ( x : {%%ext|s|} [@ and ]
```
Stack:
```
use_file: LETOP TILDE LPAREN LIDENT COLON core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
let* ~ ( x : {%%ext|s|} as ' x
```
Stack:
```
use_file: LETOP TILDE LPAREN LIDENT COLON alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
let* ~ ( x : {%%ext|s|}
```
Stack:
```
use_file: LETOP TILDE LPAREN LIDENT COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
let* ~ ( x : x
```
Stack:
```
use_file: LETOP TILDE LPAREN LIDENT COLON LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 62

```
| [_* /labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . RPAREN _*
      /labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN
      /reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT]
```

### Sample 1

Sentence:
```
let* ~ ( x : {%%ext|s|} ) , ~ ( x : {%%ext|s|} [@ and ]
```
Stack:
```
use_file: LETOP TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN LIDENT COLON core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
let* ~ ( x : {%%ext|s|} ) , ~ ( x : {%%ext|s|} as ' x
```
Stack:
```
use_file: LETOP TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN LIDENT COLON alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
let* ~ ( x : {%%ext|s|} ) , ~ ( x : {%%ext|s|}
```
Stack:
```
use_file: LETOP TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN LIDENT COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
let* ~ ( x : {%%ext|s|} ) , ~ ( x : x
```
Stack:
```
use_file: LETOP TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN LIDENT COLON LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 63

```
| [_* /labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LIDENT COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN
      /labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . RPAREN _*
      /reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT]
```

### Sample 1

Sentence:
```
let* ~ x , ~ ( x : {%%ext|s|} [@ and ]
```
Stack:
```
use_file: LETOP TILDE LIDENT COMMA TILDE LPAREN LIDENT COLON core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
let* ~ x , ~ ( x : {%%ext|s|} as ' x
```
Stack:
```
use_file: LETOP TILDE LIDENT COMMA TILDE LPAREN LIDENT COLON alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
let* ~ x , ~ ( x : {%%ext|s|}
```
Stack:
```
use_file: LETOP TILDE LIDENT COMMA TILDE LPAREN LIDENT COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
let* ~ x , ~ ( x : x
```
Stack:
```
use_file: LETOP TILDE LIDENT COMMA TILDE LPAREN LIDENT COLON LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 64

```
| [_* /simple_delimited_pattern: LBRACE listx_SEMI_record_pat_field_UNDERSCORE_ . RBRACE]
```

### Sample 1

Sentence:
```
{ x ;
```
Stack:
```
parse_pattern: LBRACE label_longident option_preceded_COLON_core_type__ option_preceded_EQUAL_pattern__ SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

### Sample 2

Sentence:
```
{ x = X
```
Stack:
```
parse_pattern: LBRACE label_longident option_preceded_COLON_core_type__ EQUAL UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ! || |] begin class : := :> (*comment*) constraint do (**documentation *) done .. .+ downto else end = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} ] rec ) ;; sig * struct then to try type val virtual when while with
```

### Sample 3

Sentence:
```
{ x : {%%ext|s|}
```
Stack:
```
parse_pattern: LBRACE label_longident COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
{ x
```
Stack:
```
parse_pattern: LBRACE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 65

```
| [_* /pattern_gen: LAZY ext list_attribute_ . simple_pattern]
```

### Sample 1

Sentence:
```
lazy [@ and ]
```
Stack:
```
parse_pattern: LAZY ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer ~label: lazy {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type val virtual when while with
```

### Sample 2

Sentence:
```
lazy % and
```
Stack:
```
parse_pattern: LAZY PERCENT AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer ~label: lazy {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type val virtual when while with
```

## Pattern 66

```
| [_* /labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern . COMMA _*
      /reversed_labeled_tuple_pattern_pattern_: LABEL simple_pattern . COMMA DOTDOT]
```

### Sample 1

Sentence:
```
~label: false
```
Stack:
```
parse_pattern: LABEL FALSE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
~label: 'a'
```
Stack:
```
parse_pattern: LABEL CHAR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 67

```
| [_* /labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN
      /labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN _*
      /reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT]
```

### Sample 1

Sentence:
```
~label: false , ~ ( x : {%%ext|s|} [@ and ]
```
Stack:
```
parse_pattern: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
~label: false , ~ ( x : {%%ext|s|} as ' x
```
Stack:
```
parse_pattern: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
~label: false , ~ ( x : {%%ext|s|}
```
Stack:
```
parse_pattern: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
~label: false , ~ ( x : x
```
Stack:
```
parse_pattern: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 68

```
| [_* /pattern: EXCEPTION ext list_attribute_ . pattern]
```

### Sample 1

Sentence:
```
exception [@ and ]
```
Stack:
```
parse_pattern: EXCEPTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

### Sample 2

Sentence:
```
exception % and
```
Stack:
```
parse_pattern: EXCEPTION PERCENT AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto else end = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

## Pattern 69

```
| [_* /pattern: EFFECT pattern_gen . COMMA simple_pattern]
```

### Sample 1

Sentence:
```
effect false
```
Stack:
```
parse_pattern: EFFECT FALSE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto else end = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

### Sample 2

Sentence:
```
effect 'a'
```
Stack:
```
parse_pattern: EFFECT CHAR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 70

```
| [_* /labeled_tuple_pat_element_list_pattern_: pattern COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN
      /labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN _*
      /reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT]
```

### Sample 1

Sentence:
```
false , ~ ( x : {%%ext|s|} [@ and ]
```
Stack:
```
parse_pattern: pattern COMMA TILDE LPAREN LIDENT COLON core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
false , ~ ( x : {%%ext|s|} as ' x
```
Stack:
```
parse_pattern: pattern COMMA TILDE LPAREN LIDENT COLON alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
false , ~ ( x : {%%ext|s|}
```
Stack:
```
parse_pattern: pattern COMMA TILDE LPAREN LIDENT COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
false , ~ ( x : x
```
Stack:
```
parse_pattern: pattern COMMA TILDE LPAREN LIDENT COLON LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 71

```
| [_* /labeled_tuple_pat_element_list_pattern_: labeled_tuple_pat_element_list_pattern_ COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN
      /labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN _*
      /reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT]
```

### Sample 1

Sentence:
```
false , false , ~ ( x : {%%ext|s|} [@ and ]
```
Stack:
```
parse_pattern: labeled_tuple_pat_element_list_pattern_ COMMA TILDE LPAREN LIDENT COLON core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
false , false , ~ ( x : {%%ext|s|} as ' x
```
Stack:
```
parse_pattern: labeled_tuple_pat_element_list_pattern_ COMMA TILDE LPAREN LIDENT COLON alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
false , false , ~ ( x : {%%ext|s|}
```
Stack:
```
parse_pattern: labeled_tuple_pat_element_list_pattern_ COMMA TILDE LPAREN LIDENT COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
false , false , ~ ( x : x
```
Stack:
```
parse_pattern: labeled_tuple_pat_element_list_pattern_ COMMA TILDE LPAREN LIDENT COLON LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 72

```
| [_* /pattern_gen: constr_longident LPAREN TYPE nonempty_list_mkrhs_LIDENT__ . RPAREN simple_pattern]
```

### Sample 1

Sentence:
```
false ( type x
```
Stack:
```
parse_pattern: constr_longident LPAREN TYPE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 73

```
| [_* /labeled_tuple_pat_element_list_pattern_: pattern . COMMA _*
      /reversed_labeled_tuple_pattern_pattern_: pattern . COMMA DOTDOT
      /simple_pattern_not_ident: LPAREN pattern . RPAREN
      /simple_pattern_not_ident: LPAREN pattern . COLON core_type RPAREN]
```

### Sample 1

Sentence:
```
( false
```
Stack:
```
parse_pattern: LPAREN FALSE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ! || |] begin class := :> (*comment*) constraint do (**documentation *) done . .. .+ downto else end = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ; ;; sig * struct then to try type val virtual when while with
```

### Sample 2

Sentence:
```
( 'a'
```
Stack:
```
parse_pattern: LPAREN CHAR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! || |] begin 'a' class := :> (*comment*) constraint do (**documentation *) done . .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 74

```
| [_* /simple_pattern_not_ident: LPAREN pattern COLON core_type . RPAREN]
```

### Sample 1

Sentence:
```
( false : {%%ext|s|} [@ and ]
```
Stack:
```
parse_pattern: LPAREN pattern COLON core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
( false : {%%ext|s|} as ' x
```
Stack:
```
parse_pattern: LPAREN pattern COLON alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
( false : {%%ext|s|}
```
Stack:
```
parse_pattern: LPAREN pattern COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
( false : x
```
Stack:
```
parse_pattern: LPAREN pattern COLON LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 75

```
| [_* /labeled_tuple_pat_element_list_pattern_: pattern . COMMA _*
      /reversed_labeled_tuple_pattern_pattern_: pattern . COMMA DOTDOT
      /simple_pattern_not_ident: mod_longident DOT LPAREN pattern . RPAREN]
```

### Sample 1

Sentence:
```
X . ( false
```
Stack:
```
parse_pattern: mod_longident DOT LPAREN FALSE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ! || |] begin class : := :> (*comment*) constraint do (**documentation *) done . .. .+ downto else end = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ; ;; sig * struct then to try type val virtual when while with
```

### Sample 2

Sentence:
```
X . ( 'a'
```
Stack:
```
parse_pattern: mod_longident DOT LPAREN CHAR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! || |] begin 'a' class : := :> (*comment*) constraint do (**documentation *) done . .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 76

```
| [_* /simple_delimited_pattern: LBRACKET separated_or_terminated_nonempty_list_SEMI_pattern_ . RBRACKET]
```

### Sample 1

Sentence:
```
[ false ;
```
Stack:
```
parse_pattern: LBRACKET pattern SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } rec ) ; ;; sig * struct then to try type val virtual when while with
```

### Sample 2

Sentence:
```
[ false
```
Stack:
```
parse_pattern: LBRACKET FALSE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ! || |] begin class : := :> (*comment*) constraint do (**documentation *) done . .. .+ downto else end = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } rec ) ;; sig * struct then to try type val virtual when while with
```

### Sample 3

Sentence:
```
[ 'a'
```
Stack:
```
parse_pattern: LBRACKET CHAR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! || |] begin 'a' class : := :> (*comment*) constraint do (**documentation *) done . .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 77

```
| [_* /simple_delimited_pattern: LBRACKETBAR separated_or_terminated_nonempty_list_SEMI_pattern_ . BARRBRACKET]
```

### Sample 1

Sentence:
```
[| false ;
```
Stack:
```
parse_pattern: LBRACKETBAR pattern SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

### Sample 2

Sentence:
```
[| false
```
Stack:
```
parse_pattern: LBRACKETBAR FALSE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ! || begin class : := :> (*comment*) constraint do (**documentation *) done . .. .+ downto else end = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ;; sig * struct then to try type val virtual when while with
```

### Sample 3

Sentence:
```
[| 'a'
```
Stack:
```
parse_pattern: LBRACKETBAR CHAR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! || begin 'a' class : := :> (*comment*) constraint do (**documentation *) done . .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 78

```
| [_* /labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN
      /labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . RPAREN _*
      /reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT]
```

### Sample 1

Sentence:
```
let* ~label: false , ~ ( x : {%%ext|s|} [@ and ]
```
Stack:
```
use_file: LETOP LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
let* ~label: false , ~ ( x : {%%ext|s|} as ' x
```
Stack:
```
use_file: LETOP LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
let* ~label: false , ~ ( x : {%%ext|s|}
```
Stack:
```
use_file: LETOP LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
let* ~label: false , ~ ( x : x
```
Stack:
```
use_file: LETOP LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 79

```
| [_* /labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN
      /labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . RPAREN _*
      /reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT]
```

### Sample 1

Sentence:
```
let* x , ~ ( x : {%%ext|s|} [@ and ]
```
Stack:
```
use_file: LETOP pattern_no_exn COMMA TILDE LPAREN LIDENT COLON core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
let* x , ~ ( x : {%%ext|s|} as ' x
```
Stack:
```
use_file: LETOP pattern_no_exn COMMA TILDE LPAREN LIDENT COLON alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
let* x , ~ ( x : {%%ext|s|}
```
Stack:
```
use_file: LETOP pattern_no_exn COMMA TILDE LPAREN LIDENT COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
let* x , ~ ( x : x
```
Stack:
```
use_file: LETOP pattern_no_exn COMMA TILDE LPAREN LIDENT COLON LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 80

```
| [_* /labeled_tuple_pat_element_list_pattern_no_exn_: labeled_tuple_pat_element_list_pattern_no_exn_ COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN
      /labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . RPAREN _*
      /reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT]
```

### Sample 1

Sentence:
```
let* x , false , ~ ( x : {%%ext|s|} [@ and ]
```
Stack:
```
use_file: LETOP labeled_tuple_pat_element_list_pattern_no_exn_ COMMA TILDE LPAREN LIDENT COLON core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
let* x , false , ~ ( x : {%%ext|s|} as ' x
```
Stack:
```
use_file: LETOP labeled_tuple_pat_element_list_pattern_no_exn_ COMMA TILDE LPAREN LIDENT COLON alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
let* x , false , ~ ( x : {%%ext|s|}
```
Stack:
```
use_file: LETOP labeled_tuple_pat_element_list_pattern_no_exn_ COMMA TILDE LPAREN LIDENT COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
let* x , false , ~ ( x : x
```
Stack:
```
use_file: LETOP labeled_tuple_pat_element_list_pattern_no_exn_ COMMA TILDE LPAREN LIDENT COLON LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 81

```
| [_* /labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern . COMMA _*
      /reversed_labeled_tuple_pattern_pattern_no_exn_: LABEL simple_pattern . COMMA DOTDOT]
```

### Sample 1

Sentence:
```
let* ~label: false
```
Stack:
```
use_file: LETOP LABEL FALSE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
let* ~label: 'a'
```
Stack:
```
use_file: LETOP LABEL CHAR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 82

```
| [_* /possibly_poly_core_type_: reversed_nonempty_llist_typevar_ . DOT core_type]
```

### Sample 1

Sentence:
```
val x : ' x ' x
```
Stack:
```
use_file: VAL ext list_attribute_ val_ident COLON reversed_nonempty_llist_typevar_ QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
external x : ' x
```
Stack:
```
use_file: EXTERNAL ext list_attribute_ val_ident COLON QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 83

```
| [_* /simple_param_pattern: TILDE LPAREN label_let_pattern . RPAREN]
```

### Sample 1

Sentence:
```
let* x ~ ( x : {%%ext|s|}
```
Stack:
```
use_file: LETOP val_ident TILDE LPAREN LIDENT COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 2

Sentence:
```
let* x ~ ( x
```
Stack:
```
use_file: LETOP val_ident TILDE LPAREN LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 84

```
| [_* /module_type_declaration: MODULE TYPE ext list_attribute_ . ident option_preceded_EQUAL_module_type__ list_post_item_attribute_]
```

### Sample 1

Sentence:
```
module type [@ and ]
```
Stack:
```
use_file: MODULE TYPE ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 2

Sentence:
```
module type % and
```
Stack:
```
use_file: MODULE TYPE PERCENT AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 85

```
| [_* /module_type: FUNCTOR list_attribute_ . reversed_nonempty_llist_functor_arg_ MINUSGREATER module_type]
```

### Sample 1

Sentence:
```
functor [@ and ]
```
Stack:
```
parse_module_type: FUNCTOR LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 86

```
| [_* /functor_arg: LPAREN module_name . COLON module_type RPAREN]
```

### Sample 1

Sentence:
```
( ) ( X
```
Stack:
```
parse_module_type: reversed_nonempty_llist_functor_arg_ LPAREN UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
( X
```
Stack:
```
parse_module_type: LPAREN UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

## Pattern 87

```
| [_* /module_type: reversed_nonempty_llist_functor_arg_ . MINUSGREATER module_type]
```

### Sample 1

Sentence:
```
( )
```
Stack:
```
parse_module_type: LPAREN RPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 88

```
| [_* /with_constraint: TYPE type_parameters . label_longident _*]
```

### Sample 1

Sentence:
```
{%%ext|s|} with type _
```
Stack:
```
parse_module_type: module_type WITH TYPE type_variance UNDERSCORE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 89

```
| [_* /with_constraint: TYPE type_parameters label_longident . with_type_binder alias_type reversed_llist_preceded_CONSTRAINT_constrain__
      /with_constraint: TYPE type_parameters label_longident . COLONEQUAL alias_type]
```

### Sample 1

Sentence:
```
{%%ext|s|} with type x
```
Stack:
```
parse_module_type: module_type WITH TYPE type_parameters LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 90

```
| [_* /with_constraint: TYPE type_parameters label_longident with_type_binder . alias_type reversed_llist_preceded_CONSTRAINT_constrain__]
```

### Sample 1

Sentence:
```
{%%ext|s|} with type x = private
```
Stack:
```
parse_module_type: module_type WITH TYPE type_parameters label_longident EQUAL PRIVATE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

### Sample 2

Sentence:
```
{%%ext|s|} with type x =
```
Stack:
```
parse_module_type: module_type WITH TYPE type_parameters label_longident EQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

## Pattern 91

```
| [_* /reversed_llist_preceded_CONSTRAINT_constrain__: reversed_llist_preceded_CONSTRAINT_constrain__ CONSTRAINT core_type . EQUAL core_type]
```

### Sample 1

Sentence:
```
type x and x constraint {%%ext|s|} [@ and ]
```
Stack:
```
interface: generic_type_declaration_nonrec_flag_type_kind_ AND list_attribute_ type_parameters LIDENT type_kind reversed_llist_preceded_CONSTRAINT_constrain__ CONSTRAINT core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
type x and x constraint {%%ext|s|} as ' x
```
Stack:
```
interface: generic_type_declaration_nonrec_flag_type_kind_ AND list_attribute_ type_parameters LIDENT type_kind reversed_llist_preceded_CONSTRAINT_constrain__ CONSTRAINT alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
type x and x constraint {%%ext|s|}
```
Stack:
```
interface: generic_type_declaration_nonrec_flag_type_kind_ AND list_attribute_ type_parameters LIDENT type_kind reversed_llist_preceded_CONSTRAINT_constrain__ CONSTRAINT QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
type x and x constraint x
```
Stack:
```
interface: generic_type_declaration_nonrec_flag_type_kind_ AND list_attribute_ type_parameters LIDENT type_kind reversed_llist_preceded_CONSTRAINT_constrain__ CONSTRAINT LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 92

```
| [_* /with_constraint: MODULE TYPE mty_longident . EQUAL module_type
      /with_constraint: MODULE TYPE mty_longident . COLONEQUAL module_type]
```

### Sample 1

Sentence:
```
{%%ext|s|} with module type x
```
Stack:
```
parse_module_type: module_type WITH MODULE TYPE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
{%%ext|s|} with module type X
```
Stack:
```
parse_module_type: module_type WITH MODULE TYPE UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 93

```
| [_* /mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident . DOT UIDENT
      /mk_longident_mod_ext_longident_ident_: mod_ext_longident . DOT ident]
```

### Sample 1

Sentence:
```
X
```
Stack:
```
parse_mty_longident: UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
X
```
Stack:
```
parse_module_type: UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

### Sample 3

Sentence:
```
( X
```
Stack:
```
parse_module_type: LPAREN UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

## Pattern 94

```
| [_* /mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT
      /with_constraint: MODULE mod_longident . EQUAL mod_ext_longident
      /with_constraint: MODULE mod_longident . COLONEQUAL mod_ext_longident]
```

### Sample 1

Sentence:
```
{%%ext|s|} with module X
```
Stack:
```
parse_module_type: module_type WITH MODULE UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 95

```
| [_* /functor_arg: LPAREN module_name COLON module_type . RPAREN]
```

### Sample 1

Sentence:
```
( X : x
```
Stack:
```
parse_module_type: LPAREN module_name COLON LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

### Sample 2

Sentence:
```
( X : X
```
Stack:
```
parse_module_type: LPAREN module_name COLON UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

## Pattern 96

```
| [_* /module_type: FUNCTOR list_attribute_ reversed_nonempty_llist_functor_arg_ . MINUSGREATER module_type]
```

### Sample 1

Sentence:
```
functor ( )
```
Stack:
```
parse_module_type: FUNCTOR list_attribute_ LPAREN RPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 97

```
| [_* /module_type: LPAREN module_type . RPAREN]
```

### Sample 1

Sentence:
```
( x
```
Stack:
```
parse_module_type: LPAREN LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

### Sample 2

Sentence:
```
( X
```
Stack:
```
parse_module_type: LPAREN UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

## Pattern 98

```
| [_* /local_structure_item: MODULE ext list_attribute_ . REC module_name module_binding_body list_post_item_attribute_ list_and_module_binding_
      /local_structure_item: MODULE ext list_attribute_ . module_name module_binding_body list_post_item_attribute_]
```

### Sample 1

Sentence:
```
module [@ and ]
```
Stack:
```
use_file: MODULE ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

### Sample 2

Sentence:
```
module % and
```
Stack:
```
use_file: MODULE PERCENT AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

## Pattern 99

```
| [_* /local_structure_item: MODULE ext list_attribute_ REC module_name . module_binding_body list_post_item_attribute_ list_and_module_binding_]
```

### Sample 1

Sentence:
```
module rec X
```
Stack:
```
use_file: MODULE ext list_attribute_ REC UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 100

```
| [_* /module_expr: FUNCTOR list_attribute_ . reversed_nonempty_llist_functor_arg_ MINUSGREATER module_expr]
```

### Sample 1

Sentence:
```
functor [@ and ]
```
Stack:
```
parse_module_expr: FUNCTOR LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 101

```
| [_* /module_expr: FUNCTOR list_attribute_ reversed_nonempty_llist_functor_arg_ . MINUSGREATER module_expr]
```

### Sample 1

Sentence:
```
functor ( )
```
Stack:
```
parse_module_expr: FUNCTOR list_attribute_ LPAREN RPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 102

```
| [_* /paren_module_expr: LPAREN module_expr . COLON module_type RPAREN
      /paren_module_expr: LPAREN module_expr . RPAREN]
```

### Sample 1

Sentence:
```
( {%%ext|s|}
```
Stack:
```
parse_module_expr: LPAREN QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
( X
```
Stack:
```
parse_module_expr: LPAREN UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 103

```
| [_* /paren_module_expr: LPAREN module_expr COLON module_type . RPAREN]
```

### Sample 1

Sentence:
```
( {%%ext|s|} : x
```
Stack:
```
parse_module_expr: LPAREN module_expr COLON LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

### Sample 2

Sentence:
```
( {%%ext|s|} : X
```
Stack:
```
parse_module_expr: LPAREN module_expr COLON UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

## Pattern 104

```
| [_* /module_binding_body: COLON module_type . EQUAL module_expr]
```

### Sample 1

Sentence:
```
module X : x
```
Stack:
```
use_file: MODULE ext list_attribute_ module_name COLON LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

### Sample 2

Sentence:
```
module X : X
```
Stack:
```
use_file: MODULE ext list_attribute_ module_name COLON UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

## Pattern 105

```
| [_* /list_and_module_binding_: AND list_attribute_ . module_name module_binding_body list_post_item_attribute_ list_and_module_binding_]
```

### Sample 1

Sentence:
```
module rec X = {%%ext|s|} and [@ and ]
```
Stack:
```
use_file: MODULE ext list_attribute_ REC module_name module_binding_body list_post_item_attribute_ AND LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

## Pattern 106

```
| [_* /list_and_module_binding_: AND list_attribute_ module_name . module_binding_body list_post_item_attribute_ list_and_module_binding_]
```

### Sample 1

Sentence:
```
module rec X = {%%ext|s|} and X
```
Stack:
```
use_file: MODULE ext list_attribute_ REC module_name module_binding_body list_post_item_attribute_ AND list_attribute_ UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 107

```
| [_* /module_binding_body: functor_arg . module_binding_body]
```

### Sample 1

Sentence:
```
module rec X ( )
```
Stack:
```
use_file: MODULE ext list_attribute_ REC module_name LPAREN RPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 108

```
| [_* /local_structure_item: MODULE ext list_attribute_ module_name . module_binding_body list_post_item_attribute_]
```

### Sample 1

Sentence:
```
module X
```
Stack:
```
use_file: MODULE ext list_attribute_ UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 109

```
| [_* /simple_expr: BEGIN ext list_attribute_ . seq_expr END
      /simple_expr: BEGIN ext list_attribute_ . END]
```

### Sample 1

Sentence:
```
begin [@ and ]
```
Stack:
```
use_file: BEGIN ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

### Sample 2

Sentence:
```
begin % and
```
Stack:
```
use_file: BEGIN PERCENT AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

## Pattern 110

```
| [_* /fun_expr: LAZY ext list_attribute_ . simple_expr]
```

### Sample 1

Sentence:
```
lazy [@ and ]
```
Stack:
```
use_file: LAZY ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy [@@ [@@@ [> [< [%% < <- let let* match >. method - -. -> module mutable nonrec of open ?label: or % + +. += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type _ val virtual when while with
```

### Sample 2

Sentence:
```
lazy % and
```
Stack:
```
use_file: LAZY PERCENT AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy [@@ [@@@ [> [< [%% < <- let let* match >. method - -. -> module mutable nonrec of open ?label: or % + +. += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type _ val virtual when while with
```

## Pattern 111

```
| [_* /reversed_labeled_tuple_body: LABEL simple_expr . COMMA _*]
```

### Sample 1

Sentence:
```
~label: 'a'
```
Stack:
```
use_file: LABEL CHAR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done .. downto effect else end = exception external false 1.0 for fun function functor > >} >] if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 112

```
| [_* /fun_expr: IF ext list_attribute_ . seq_expr _*]
```

### Sample 1

Sentence:
```
if [@ and ]
```
Stack:
```
use_file: IF ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

### Sample 2

Sentence:
```
if % and
```
Stack:
```
use_file: IF PERCENT AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

## Pattern 113

```
| [_* /expr: FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
      /seq_expr: FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_]
```

### Sample 1

Sentence:
```
function [@ and ]
```
Stack:
```
use_file: FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

### Sample 2

Sentence:
```
function % and
```
Stack:
```
use_file: FUNCTION PERCENT AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto else end = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

## Pattern 114

```
| [_* /labeled_tuple_pat_element_list_pattern_: pattern . COMMA _*
      /match_case: pattern . MINUSGREATER seq_expr
      /match_case: pattern . WHEN seq_expr MINUSGREATER seq_expr
      /match_case: pattern . MINUSGREATER DOT
      /reversed_labeled_tuple_pattern_pattern_: pattern . COMMA DOTDOT]
```

### Sample 1

Sentence:
```
function false
```
Stack:
```
use_file: FUNCTION ext list_attribute_ FALSE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ! || |] begin class : := :> (*comment*) constraint do (**documentation *) done . .. .+ downto else end = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual while with
```

### Sample 2

Sentence:
```
function 'a'
```
Stack:
```
use_file: FUNCTION ext list_attribute_ CHAR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! || |] begin 'a' class : := :> (*comment*) constraint do (**documentation *) done . .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual while with
```

## Pattern 115

```
| [_* /fun_expr: FUN ext list_attribute_ . fun_params option_preceded_COLON_atomic_type__ MINUSGREATER fun_body]
```

### Sample 1

Sentence:
```
fun [@ and ]
```
Stack:
```
use_file: FUN ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer lazy {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open or % +. += !+ private ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

### Sample 2

Sentence:
```
fun % and
```
Stack:
```
use_file: FUN PERCENT AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer lazy {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open or % +. += !+ private ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

## Pattern 116

```
| [_* /simple_param_pattern: OPTLABEL LPAREN let_pattern option_preceded_EQUAL_seq_expr__ . RPAREN]
```

### Sample 1

Sentence:
```
let* x ?label: ( false = X ;
```
Stack:
```
use_file: LETOP val_ident OPTLABEL LPAREN let_pattern EQUAL fun_expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or += private ? ' {%%%%ext|s|} } ] rec ; ;; sig * struct then to type _ val virtual when with
```

### Sample 2

Sentence:
```
let* x ?label: ( false = x
```
Stack:
```
use_file: LETOP val_ident OPTLABEL LPAREN let_pattern EQUAL LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] class : :> , (*comment*) constraint do (**documentation *) done .. downto effect else end exception external for fun function functor >} >] if in include inherit initializer lazy [@@ [@@@ [> [< [%% let let* match >. method -> module mutable nonrec of open private ' {%%%%ext|s|} } ] rec ;; sig struct then to try type _ val virtual when while with
```

### Sample 3

Sentence:
```
let* x ?label: ( false
```
Stack:
```
use_file: LETOP val_ident OPTLABEL LPAREN FALSE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ! || |] begin class := :> (*comment*) constraint do (**documentation *) done . .. .+ downto else end external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ; ;; sig * struct then to try type val virtual when while with
```

### Sample 4

Sentence:
```
let* x ?label: ( 'a'
```
Stack:
```
use_file: LETOP val_ident OPTLABEL LPAREN CHAR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! || |] begin 'a' class := :> (*comment*) constraint do (**documentation *) done . .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 117

```
| [_* /fun_param_as_list: LPAREN TYPE nonempty_list_mkrhs_LIDENT__ . RPAREN]
```

### Sample 1

Sentence:
```
let* x ( type x
```
Stack:
```
use_file: LETOP val_ident LPAREN TYPE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 118

```
| [_* /labeled_tuple_pat_element_list_pattern_: pattern . COMMA _*
      /reversed_labeled_tuple_pattern_pattern_: pattern . COMMA DOTDOT
      /simple_param_pattern: LPAREN pattern . COLON reversed_nonempty_llist_typevar_ DOT core_type RPAREN
      /simple_pattern_not_ident: LPAREN pattern . RPAREN
      /simple_pattern_not_ident: LPAREN pattern . COLON core_type RPAREN]
```

### Sample 1

Sentence:
```
let* x ( false
```
Stack:
```
use_file: LETOP val_ident LPAREN FALSE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ! || |] begin class := :> (*comment*) constraint do (**documentation *) done . .. .+ downto else end = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ; ;; sig * struct then to try type val virtual when while with
```

### Sample 2

Sentence:
```
let* x ( 'a'
```
Stack:
```
use_file: LETOP val_ident LPAREN CHAR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! || |] begin 'a' class := :> (*comment*) constraint do (**documentation *) done . .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 119

```
| [_* /simple_param_pattern: LPAREN pattern COLON reversed_nonempty_llist_typevar_ . DOT core_type RPAREN]
```

### Sample 1

Sentence:
```
let* x ( false : ' x ' x
```
Stack:
```
use_file: LETOP val_ident LPAREN pattern COLON reversed_nonempty_llist_typevar_ QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
let* x ( false : ' x
```
Stack:
```
use_file: LETOP val_ident LPAREN pattern COLON QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 120

```
| [_* /simple_param_pattern: LPAREN pattern COLON reversed_nonempty_llist_typevar_ DOT core_type . RPAREN]
```

### Sample 1

Sentence:
```
let* x ( false : ' x . {%%ext|s|} [@ and ]
```
Stack:
```
use_file: LETOP val_ident LPAREN pattern COLON reversed_nonempty_llist_typevar_ DOT core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
let* x ( false : ' x . {%%ext|s|} as ' x
```
Stack:
```
use_file: LETOP val_ident LPAREN pattern COLON reversed_nonempty_llist_typevar_ DOT alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
let* x ( false : ' x . {%%ext|s|}
```
Stack:
```
use_file: LETOP val_ident LPAREN pattern COLON reversed_nonempty_llist_typevar_ DOT QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
let* x ( false : ' x . x
```
Stack:
```
use_file: LETOP val_ident LPAREN pattern COLON reversed_nonempty_llist_typevar_ DOT LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 121

```
| [_* /labeled_tuple_pat_element_list_pattern_: pattern . COMMA _*
      /reversed_labeled_tuple_pattern_pattern_: pattern . COMMA DOTDOT
      /simple_param_pattern: LABEL LPAREN pattern . COLON reversed_nonempty_llist_typevar_ DOT core_type RPAREN
      /simple_pattern_not_ident: LPAREN pattern . RPAREN
      /simple_pattern_not_ident: LPAREN pattern . COLON core_type RPAREN]
```

### Sample 1

Sentence:
```
let* x ~label: ( false
```
Stack:
```
use_file: LETOP val_ident LABEL LPAREN FALSE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ! || |] begin class := :> (*comment*) constraint do (**documentation *) done . .. .+ downto else end = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ; ;; sig * struct then to try type val virtual when while with
```

### Sample 2

Sentence:
```
let* x ~label: ( 'a'
```
Stack:
```
use_file: LETOP val_ident LABEL LPAREN CHAR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! || |] begin 'a' class := :> (*comment*) constraint do (**documentation *) done . .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 122

```
| [_* /simple_param_pattern: LABEL LPAREN pattern COLON reversed_nonempty_llist_typevar_ . DOT core_type RPAREN]
```

### Sample 1

Sentence:
```
let* x ~label: ( false : ' x ' x
```
Stack:
```
use_file: LETOP val_ident LABEL LPAREN pattern COLON reversed_nonempty_llist_typevar_ QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
let* x ~label: ( false : ' x
```
Stack:
```
use_file: LETOP val_ident LABEL LPAREN pattern COLON QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 123

```
| [_* /simple_param_pattern: LABEL LPAREN pattern COLON reversed_nonempty_llist_typevar_ DOT core_type . RPAREN]
```

### Sample 1

Sentence:
```
let* x ~label: ( false : ' x . {%%ext|s|} [@ and ]
```
Stack:
```
use_file: LETOP val_ident LABEL LPAREN pattern COLON reversed_nonempty_llist_typevar_ DOT core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
let* x ~label: ( false : ' x . {%%ext|s|} as ' x
```
Stack:
```
use_file: LETOP val_ident LABEL LPAREN pattern COLON reversed_nonempty_llist_typevar_ DOT alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
let* x ~label: ( false : ' x . {%%ext|s|}
```
Stack:
```
use_file: LETOP val_ident LABEL LPAREN pattern COLON reversed_nonempty_llist_typevar_ DOT QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
let* x ~label: ( false : ' x . x
```
Stack:
```
use_file: LETOP val_ident LABEL LPAREN pattern COLON reversed_nonempty_llist_typevar_ DOT LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 124

```
| [_* /fun_expr: FUN ext list_attribute_ fun_params option_preceded_COLON_atomic_type__ . MINUSGREATER fun_body]
```

### Sample 1

Sentence:
```
fun false : x
```
Stack:
```
use_file: FUN ext list_attribute_ fun_params COLON LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 2

Sentence:
```
fun false
```
Stack:
```
use_file: FUN ext list_attribute_ FALSE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. module mutable new nonrec object of open or % +. += !+ private ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

### Sample 3

Sentence:
```
fun 'a'
```
Stack:
```
use_file: FUN ext list_attribute_ CHAR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class :: := :> , (*comment*) constraint do (**documentation *) done . .+ downto effect else end = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. module mutable new nonrec object of open or % +. += !+ private ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

## Pattern 125

```
| [_* /expr: FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
      /fun_body: FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_]
```

### Sample 1

Sentence:
```
let* x false = function [@ and ]
```
Stack:
```
use_file: LETOP val_ident fun_params option_type_constraint_ EQUAL FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

### Sample 2

Sentence:
```
let* x false = function % and
```
Stack:
```
use_file: LETOP val_ident fun_params option_type_constraint_ EQUAL FUNCTION PERCENT AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto else end = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

## Pattern 126

```
| [_* /fun_expr: FOR ext list_attribute_ . pattern EQUAL seq_expr direction_flag seq_expr DO seq_expr DONE]
```

### Sample 1

Sentence:
```
for [@ and ]
```
Stack:
```
use_file: FOR ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

### Sample 2

Sentence:
```
for % and
```
Stack:
```
use_file: FOR PERCENT AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto else end = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

## Pattern 127

```
| [_* /fun_expr: FOR ext list_attribute_ pattern . EQUAL seq_expr direction_flag seq_expr DO seq_expr DONE
      /labeled_tuple_pat_element_list_pattern_: pattern . COMMA _*
      /reversed_labeled_tuple_pattern_pattern_: pattern . COMMA DOTDOT]
```

### Sample 1

Sentence:
```
for false
```
Stack:
```
use_file: FOR ext list_attribute_ FALSE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ! || |] begin class : := :> (*comment*) constraint do (**documentation *) done . .. .+ downto else end external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

### Sample 2

Sentence:
```
for 'a'
```
Stack:
```
use_file: FOR ext list_attribute_ CHAR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! || |] begin 'a' class : := :> (*comment*) constraint do (**documentation *) done . .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 128

```
| [_* /fun_expr: ASSERT ext list_attribute_ . simple_expr]
```

### Sample 1

Sentence:
```
assert [@ and ]
```
Stack:
```
use_file: ASSERT ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy [@@ [@@@ [> [< [%% < <- let let* match >. method - -. -> module mutable nonrec of open ?label: or % + +. += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type _ val virtual when while with
```

### Sample 2

Sentence:
```
assert % and
```
Stack:
```
use_file: ASSERT PERCENT AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy [@@ [@@@ [> [< [%% < <- let let* match >. method - -. -> module mutable nonrec of open ?label: or % + +. += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type _ val virtual when while with
```

## Pattern 129

```
| [_* /simple_expr: mod_longident DOT LPAREN MODULE ext list_attribute_ . module_expr COLON module_type RPAREN]
```

### Sample 1

Sentence:
```
X . ( module [@ and ]
```
Stack:
```
use_file: mod_longident DOT LPAREN MODULE ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ val virtual when while with
```

### Sample 2

Sentence:
```
X . ( module % and
```
Stack:
```
use_file: mod_longident DOT LPAREN MODULE PERCENT AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ val virtual when while with
```

## Pattern 130

```
| [_* /simple_expr: mod_longident DOT LPAREN MODULE ext list_attribute_ module_expr . COLON module_type RPAREN]
```

### Sample 1

Sentence:
```
X . ( module {%%ext|s|}
```
Stack:
```
use_file: mod_longident DOT LPAREN MODULE ext list_attribute_ QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
X . ( module X
```
Stack:
```
use_file: mod_longident DOT LPAREN MODULE ext list_attribute_ UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 131

```
| [_* /simple_expr: mod_longident DOT LPAREN MODULE ext list_attribute_ module_expr COLON module_type . RPAREN]
```

### Sample 1

Sentence:
```
X . ( module {%%ext|s|} : x
```
Stack:
```
use_file: mod_longident DOT LPAREN MODULE ext list_attribute_ module_expr COLON LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

### Sample 2

Sentence:
```
X . ( module {%%ext|s|} : X
```
Stack:
```
use_file: mod_longident DOT LPAREN MODULE ext list_attribute_ module_expr COLON UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

## Pattern 132

```
| [_* /letop_binding_body: simple_pattern COLON core_type . EQUAL seq_expr]
```

### Sample 1

Sentence:
```
let* x : {%%ext|s|} [@ and ]
```
Stack:
```
use_file: LETOP simple_pattern COLON core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
let* x : {%%ext|s|} as ' x
```
Stack:
```
use_file: LETOP simple_pattern COLON alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
let* x : {%%ext|s|}
```
Stack:
```
use_file: LETOP simple_pattern COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
let* x : x
```
Stack:
```
use_file: LETOP simple_pattern COLON LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 133

```
| [_* /fun_expr: subtractive . expr]
```

### Sample 1

Sentence:
```
-
```
Stack:
```
use_file: MINUS
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

### Sample 2

Sentence:
```
( -
```
Stack:
```
use_file: LPAREN MINUS
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ; ;; sig * struct then to type _ val virtual when with
```

## Pattern 134

```
| [_* /expr: FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_]
```

### Sample 1

Sentence:
```
+ function [@ and ]
```
Stack:
```
use_file: additive FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

### Sample 2

Sentence:
```
+ function % and
```
Stack:
```
use_file: additive FUNCTION PERCENT AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto else end = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

## Pattern 135

```
| [_* /labeled_simple_expr: TILDE LPAREN LIDENT type_constraint . RPAREN]
```

### Sample 1

Sentence:
```
X ~ ( x :> {%%ext|s|} [@ and ]
```
Stack:
```
use_file: simple_expr TILDE LPAREN LIDENT COLONGREATER core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
X ~ ( x : {%%ext|s|} as ' x
```
Stack:
```
use_file: simple_expr TILDE LPAREN LIDENT COLON alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
X ~ ( x :> {%%ext|s|}
```
Stack:
```
use_file: simple_expr TILDE LPAREN LIDENT COLONGREATER QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
X ~ ( x : {%%ext|s|}
```
Stack:
```
use_file: simple_expr TILDE LPAREN LIDENT COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 136

```
| [_* /fun_expr: simple_expr DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ . RPAREN LESSMINUS expr
      /simple_expr: simple_expr DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ . RPAREN]
```

### Sample 1

Sentence:
```
X .+ ( function false -> .
```
Stack:
```
use_file: simple_expr DOTOP LPAREN FUNCTION ext list_attribute_ pattern MINUSGREATER DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
X .+ ( X ;
```
Stack:
```
use_file: simple_expr DOTOP LPAREN expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ; ;; sig * struct then to type _ val virtual when with
```

### Sample 3

Sentence:
```
X .+ ( X 'a'
```
Stack:
```
use_file: simple_expr DOTOP LPAREN constr_longident CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert ` ! | |] begin 'a' class : :> (*comment*) constraint do (**documentation *) done .. downto effect else end exception external false 1.0 for fun function functor >} >] if in include inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% <- let let* x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ;; sig "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 4

Sentence:
```
X .+ ( 'a'
```
Stack:
```
use_file: simple_expr DOTOP LPAREN CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] class : :> (*comment*) constraint do (**documentation *) done .. downto effect else end exception external for fun function functor >} >] if in include inherit initializer lazy [@@ [@@@ [> [< [%% <- let let* match >. method -> module mutable nonrec of open private ' {%%%%ext|s|} } ] rec ;; sig struct then to try type _ val virtual when while with
```

## Pattern 137

```
| [_* /reversed_labeled_tuple_body: reversed_labeled_tuple_body COMMA TILDE LPAREN LIDENT type_constraint . RPAREN
      /reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint . RPAREN _*]
```

### Sample 1

Sentence:
```
~ x , X , ~ ( x :> {%%ext|s|} [@ and ]
```
Stack:
```
use_file: reversed_labeled_tuple_body COMMA TILDE LPAREN LIDENT COLONGREATER core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
~ x , X , ~ ( x : {%%ext|s|} as ' x
```
Stack:
```
use_file: reversed_labeled_tuple_body COMMA TILDE LPAREN LIDENT COLON alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
~ x , X , ~ ( x :> {%%ext|s|}
```
Stack:
```
use_file: reversed_labeled_tuple_body COMMA TILDE LPAREN LIDENT COLONGREATER QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
~ x , X , ~ ( x : {%%ext|s|}
```
Stack:
```
use_file: reversed_labeled_tuple_body COMMA TILDE LPAREN LIDENT COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 138

```
| [_* /reversed_labeled_tuple_body: LABEL simple_expr COMMA TILDE LPAREN LIDENT type_constraint . RPAREN
      /reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint . RPAREN _*]
```

### Sample 1

Sentence:
```
~label: 'a' , ~ ( x :> {%%ext|s|} [@ and ]
```
Stack:
```
use_file: LABEL simple_expr COMMA TILDE LPAREN LIDENT COLONGREATER core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
~label: 'a' , ~ ( x : {%%ext|s|} as ' x
```
Stack:
```
use_file: LABEL simple_expr COMMA TILDE LPAREN LIDENT COLON alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
~label: 'a' , ~ ( x :> {%%ext|s|}
```
Stack:
```
use_file: LABEL simple_expr COMMA TILDE LPAREN LIDENT COLONGREATER QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
~label: 'a' , ~ ( x : {%%ext|s|}
```
Stack:
```
use_file: LABEL simple_expr COMMA TILDE LPAREN LIDENT COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 139

```
| [_* /fun_expr: let_bindings_ext_ . IN seq_expr]
```

### Sample 1

Sentence:
```
let x and x [@@ and ]
```
Stack:
```
parse_expression: let_bindings_ext_ AND list_attribute_ let_binding_body LBRACKETATAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
let x and x = X 'a'
```
Stack:
```
parse_expression: let_bindings_ext_ AND list_attribute_ val_ident EQUAL constr_longident CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and* as assert ` ! | |] begin 'a' class : :> , (*comment*) constraint do (**documentation *) done .. downto effect else end exception external false 1.0 for fun function functor >} >] if include inherit initializer 1 ~label: lazy { {< [ [@@@ [| [> [< [% [%% <- let let* x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ;; sig "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
let x and x = X ;
```
Stack:
```
parse_expression: let_bindings_ext_ AND list_attribute_ val_ident EQUAL fun_expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## include != ^ +! land ** inherit initializer [@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

### Sample 4

Sentence:
```
let x and x = x
```
Stack:
```
parse_expression: let_bindings_ext_ AND list_attribute_ val_ident EQUAL LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
and* as assert | |] class : :> , (*comment*) constraint do (**documentation *) done .. downto effect else end exception external for fun function functor >} >] if include inherit initializer lazy [@@@ [> [< [%% let let* match >. method -> module mutable nonrec of open private ' {%%%%ext|s|} } ] rec ) ;; sig struct then to try type _ val virtual when while with
```

### Sample 5

Sentence:
```
let x and x
```
Stack:
```
parse_expression: let_bindings_ext_ AND list_attribute_ LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and* assert ! || |] begin class := (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external for fun function functor > >} >] ## if include != ^ +! land ** inherit initializer lazy {< [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open or % +. += !+ private ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

## Pattern 140

```
| [_* /reversed_labeled_tuple_body: expr COMMA TILDE LPAREN LIDENT type_constraint . RPAREN
      /reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint . RPAREN _*]
```

### Sample 1

Sentence:
```
[ X , ~ ( x :> {%%ext|s|} [@ and ]
```
Stack:
```
use_file: LBRACKET expr COMMA TILDE LPAREN LIDENT COLONGREATER core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
[ X , ~ ( x : {%%ext|s|} as ' x
```
Stack:
```
use_file: LBRACKET expr COMMA TILDE LPAREN LIDENT COLON alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
[ X , ~ ( x :> {%%ext|s|}
```
Stack:
```
use_file: LBRACKET expr COMMA TILDE LPAREN LIDENT COLONGREATER QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
[ X , ~ ( x : {%%ext|s|}
```
Stack:
```
use_file: LBRACKET expr COMMA TILDE LPAREN LIDENT COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 141

```
| [_* /fun_expr: additive . expr]
```

### Sample 1

Sentence:
```
+
```
Stack:
```
use_file: PLUS
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

### Sample 2

Sentence:
```
( +
```
Stack:
```
use_file: LPAREN PLUS
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ; ;; sig * struct then to type _ val virtual when with
```

## Pattern 142

```
| [_* /fun_seq_expr: fun_expr SEMI PERCENT attr_id . seq_expr]
```

### Sample 1

Sentence:
```
X ; % and
```
Stack:
```
use_file: fun_expr SEMI PERCENT AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

## Pattern 143

```
| [_* /and_let_binding: AND list_attribute_ . let_binding_body list_post_item_attribute_]
```

### Sample 1

Sentence:
```
let x and [@ and ]
```
Stack:
```
parse_expression: let_bindings_ext_ AND LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

## Pattern 144

```
| [_* /let_binding_body_no_punning: val_ident COLON TYPE nonempty_list_mkrhs_LIDENT__ . DOT core_type EQUAL seq_expr]
```

### Sample 1

Sentence:
```
let x and x : type x
```
Stack:
```
parse_expression: let_bindings_ext_ AND list_attribute_ val_ident COLON TYPE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 145

```
| [_* /let_binding_body_no_punning: val_ident COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type . EQUAL seq_expr]
```

### Sample 1

Sentence:
```
let x and x : type x . {%%ext|s|} [@ and ]
```
Stack:
```
parse_expression: let_bindings_ext_ AND list_attribute_ val_ident COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
let x and x : type x . {%%ext|s|} as ' x
```
Stack:
```
parse_expression: let_bindings_ext_ AND list_attribute_ val_ident COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
let x and x : type x . {%%ext|s|}
```
Stack:
```
parse_expression: let_bindings_ext_ AND list_attribute_ val_ident COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
let x and x : type x . x
```
Stack:
```
parse_expression: let_bindings_ext_ AND list_attribute_ val_ident COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 146

```
| [_* /let_binding_body_no_punning: val_ident COLON reversed_nonempty_llist_typevar_ . DOT core_type EQUAL seq_expr]
```

### Sample 1

Sentence:
```
let x and x : ' x ' x
```
Stack:
```
parse_expression: let_bindings_ext_ AND list_attribute_ val_ident COLON reversed_nonempty_llist_typevar_ QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 147

```
| [_* /let_binding_body_no_punning: val_ident COLON reversed_nonempty_llist_typevar_ DOT core_type . EQUAL seq_expr]
```

### Sample 1

Sentence:
```
let x and x : ' x . {%%ext|s|} [@ and ]
```
Stack:
```
parse_expression: let_bindings_ext_ AND list_attribute_ val_ident COLON reversed_nonempty_llist_typevar_ DOT core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
let x and x : ' x . {%%ext|s|} as ' x
```
Stack:
```
parse_expression: let_bindings_ext_ AND list_attribute_ val_ident COLON reversed_nonempty_llist_typevar_ DOT alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
let x and x : ' x . {%%ext|s|}
```
Stack:
```
parse_expression: let_bindings_ext_ AND list_attribute_ val_ident COLON reversed_nonempty_llist_typevar_ DOT QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
let x and x : ' x . x
```
Stack:
```
parse_expression: let_bindings_ext_ AND list_attribute_ val_ident COLON reversed_nonempty_llist_typevar_ DOT LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 148

```
| [_* /let_binding_body_no_punning: val_ident type_constraint . EQUAL seq_expr]
```

### Sample 1

Sentence:
```
let x and x :> {%%ext|s|} [@ and ]
```
Stack:
```
parse_expression: let_bindings_ext_ AND list_attribute_ val_ident COLONGREATER core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
let x and x : {%%ext|s|} as ' x
```
Stack:
```
parse_expression: let_bindings_ext_ AND list_attribute_ val_ident COLON alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
let x and x :> {%%ext|s|}
```
Stack:
```
parse_expression: let_bindings_ext_ AND list_attribute_ val_ident COLONGREATER QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
let x and x : {%%ext|s|}
```
Stack:
```
parse_expression: let_bindings_ext_ AND list_attribute_ val_ident COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 149

```
| [_* /strict_binding: fun_params option_type_constraint_ . EQUAL fun_body]
```

### Sample 1

Sentence:
```
let* x false : {%%ext|s|} as ' x
```
Stack:
```
use_file: LETOP val_ident fun_params COLON alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
let* x false :> {%%ext|s|}
```
Stack:
```
use_file: LETOP val_ident fun_params COLONGREATER QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 3

Sentence:
```
let* x false : {%%ext|s|}
```
Stack:
```
use_file: LETOP val_ident fun_params COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
let* x false
```
Stack:
```
use_file: LETOP val_ident FALSE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open or % +. += !+ private ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

### Sample 5

Sentence:
```
let* x 'a'
```
Stack:
```
use_file: LETOP val_ident CHAR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class :: := , (*comment*) constraint do (**documentation *) done . .+ downto effect else end exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open or % +. += !+ private ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

## Pattern 150

```
| [_* /let_binding_body_no_punning: simple_pattern_not_ident COLON core_type . EQUAL seq_expr]
```

### Sample 1

Sentence:
```
let x and false : {%%ext|s|} [@ and ]
```
Stack:
```
parse_expression: let_bindings_ext_ AND list_attribute_ simple_pattern_not_ident COLON core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
let x and false : {%%ext|s|} as ' x
```
Stack:
```
parse_expression: let_bindings_ext_ AND list_attribute_ simple_pattern_not_ident COLON alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
let x and false : {%%ext|s|}
```
Stack:
```
parse_expression: let_bindings_ext_ AND list_attribute_ simple_pattern_not_ident COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
let x and false : x
```
Stack:
```
parse_expression: let_bindings_ext_ AND list_attribute_ simple_pattern_not_ident COLON LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 151

```
| [_* /labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn . COMMA _*
      /let_binding_body_no_punning: pattern_no_exn . EQUAL seq_expr
      /reversed_labeled_tuple_pattern_pattern_no_exn_: pattern_no_exn . COMMA DOTDOT]
```

### Sample 1

Sentence:
```
let x and false X
```
Stack:
```
parse_expression: let_bindings_ext_ AND list_attribute_ constr_longident UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ! || |] begin class : := :> (*comment*) constraint do (**documentation *) done .. .+ downto else end external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

### Sample 2

Sentence:
```
let x and false
```
Stack:
```
parse_expression: let_bindings_ext_ AND list_attribute_ FALSE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ! || |] begin class := :> (*comment*) constraint do (**documentation *) done . .. .+ downto else end external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

### Sample 3

Sentence:
```
let x and 'a'
```
Stack:
```
parse_expression: let_bindings_ext_ AND list_attribute_ CHAR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! || |] begin 'a' class := :> (*comment*) constraint do (**documentation *) done . .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 152

```
| [_* /fun_expr: simple_expr DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACKET LESSMINUS expr
      /simple_expr: simple_expr DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACKET]
```

### Sample 1

Sentence:
```
X .+ [ function false -> .
```
Stack:
```
use_file: simple_expr DOTOP LBRACKET FUNCTION ext list_attribute_ pattern MINUSGREATER DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
X .+ [ X ;
```
Stack:
```
use_file: simple_expr DOTOP LBRACKET expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } rec ) ; ;; sig * struct then to type _ val virtual when with
```

### Sample 3

Sentence:
```
X .+ [ X 'a'
```
Stack:
```
use_file: simple_expr DOTOP LBRACKET constr_longident CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert ` ! | |] begin 'a' class : :> (*comment*) constraint do (**documentation *) done .. downto effect else end exception external false 1.0 for fun function functor >} >] if in include inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% <- let let* x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ;; sig "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 4

Sentence:
```
X .+ [ 'a'
```
Stack:
```
use_file: simple_expr DOTOP LBRACKET CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] class : :> (*comment*) constraint do (**documentation *) done .. downto effect else end exception external for fun function functor >} >] if in include inherit initializer lazy [@@ [@@@ [> [< [%% <- let let* match >. method -> module mutable nonrec of open private ' {%%%%ext|s|} } rec ) ;; sig struct then to try type _ val virtual when while with
```

## Pattern 153

```
| [_* /fun_expr: simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACE LESSMINUS expr
      /simple_expr: simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACE]
```

### Sample 1

Sentence:
```
X .+ { function false -> .
```
Stack:
```
use_file: simple_expr DOTOP LBRACE FUNCTION ext list_attribute_ pattern MINUSGREATER DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
X .+ { X ;
```
Stack:
```
use_file: simple_expr DOTOP LBRACE expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

### Sample 3

Sentence:
```
X .+ { X 'a'
```
Stack:
```
use_file: simple_expr DOTOP LBRACE constr_longident CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert ` ! | |] begin 'a' class : :> (*comment*) constraint do (**documentation *) done .. downto effect else end exception external false 1.0 for fun function functor >} >] if in include inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% <- let let* x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ;; sig "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 4

Sentence:
```
X .+ { 'a'
```
Stack:
```
use_file: simple_expr DOTOP LBRACE CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] class : :> (*comment*) constraint do (**documentation *) done .. downto effect else end exception external for fun function functor >} >] if in include inherit initializer lazy [@@ [@@@ [> [< [%% <- let let* match >. method -> module mutable nonrec of open private ' {%%%%ext|s|} ] rec ) ;; sig struct then to try type _ val virtual when while with
```

## Pattern 154

```
| [_* /fun_expr: simple_expr DOT LPAREN seq_expr . RPAREN LESSMINUS expr
      /simple_expr: simple_expr DOT LPAREN seq_expr . RPAREN]
```

### Sample 1

Sentence:
```
false . ( function false -> .
```
Stack:
```
use_file: simple_expr DOT LPAREN FUNCTION ext list_attribute_ pattern MINUSGREATER DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
false . ( X ;
```
Stack:
```
use_file: simple_expr DOT LPAREN fun_expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or += private ? ' {%%%%ext|s|} } ] rec ; ;; sig * struct then to type _ val virtual when with
```

### Sample 3

Sentence:
```
false . ( X 'a'
```
Stack:
```
use_file: simple_expr DOT LPAREN constr_longident CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert ` ! | |] begin 'a' class : :> , (*comment*) constraint do (**documentation *) done .. downto effect else end exception external false 1.0 for fun function functor >} >] if in include inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% <- let let* x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ;; sig "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 4

Sentence:
```
false . ( 'a'
```
Stack:
```
use_file: simple_expr DOT LPAREN CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] class : :> , (*comment*) constraint do (**documentation *) done .. downto effect else end exception external for fun function functor >} >] if in include inherit initializer lazy [@@ [@@@ [> [< [%% <- let let* match >. method -> module mutable nonrec of open private ' {%%%%ext|s|} } ] rec ;; sig struct then to try type _ val virtual when while with
```

## Pattern 155

```
| [_* /fun_expr: simple_expr DOT LBRACKET seq_expr . RBRACKET LESSMINUS expr
      /simple_expr: simple_expr DOT LBRACKET seq_expr . RBRACKET]
```

### Sample 1

Sentence:
```
false . [ function false -> .
```
Stack:
```
use_file: simple_expr DOT LBRACKET FUNCTION ext list_attribute_ pattern MINUSGREATER DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
false . [ X ;
```
Stack:
```
use_file: simple_expr DOT LBRACKET fun_expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or += private ? ' {%%%%ext|s|} } rec ) ; ;; sig * struct then to type _ val virtual when with
```

### Sample 3

Sentence:
```
false . [ X 'a'
```
Stack:
```
use_file: simple_expr DOT LBRACKET constr_longident CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert ` ! | |] begin 'a' class : :> , (*comment*) constraint do (**documentation *) done .. downto effect else end exception external false 1.0 for fun function functor >} >] if in include inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% <- let let* x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ;; sig "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 4

Sentence:
```
false . [ 'a'
```
Stack:
```
use_file: simple_expr DOT LBRACKET CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] class : :> , (*comment*) constraint do (**documentation *) done .. downto effect else end exception external for fun function functor >} >] if in include inherit initializer lazy [@@ [@@@ [> [< [%% <- let let* match >. method -> module mutable nonrec of open private ' {%%%%ext|s|} } rec ) ;; sig struct then to try type _ val virtual when while with
```

## Pattern 156

```
| [_* /fun_expr: simple_expr DOT LBRACE seq_expr . RBRACE LESSMINUS expr
      /simple_expr: simple_expr DOT LBRACE seq_expr . RBRACE]
```

### Sample 1

Sentence:
```
false . { function false -> .
```
Stack:
```
use_file: simple_expr DOT LBRACE FUNCTION ext list_attribute_ pattern MINUSGREATER DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
false . { X ;
```
Stack:
```
use_file: simple_expr DOT LBRACE fun_expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or += private ? ' {%%%%ext|s|} ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

### Sample 3

Sentence:
```
false . { X 'a'
```
Stack:
```
use_file: simple_expr DOT LBRACE constr_longident CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert ` ! | |] begin 'a' class : :> , (*comment*) constraint do (**documentation *) done .. downto effect else end exception external false 1.0 for fun function functor >} >] if in include inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% <- let let* x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ;; sig "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 4

Sentence:
```
false . { 'a'
```
Stack:
```
use_file: simple_expr DOT LBRACE CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] class : :> , (*comment*) constraint do (**documentation *) done .. downto effect else end exception external for fun function functor >} >] if in include inherit initializer lazy [@@ [@@@ [> [< [%% <- let let* match >. method -> module mutable nonrec of open private ' {%%%%ext|s|} ] rec ) ;; sig struct then to try type _ val virtual when while with
```

## Pattern 157

```
| [_* /fun_expr: simple_expr DOT mod_longident . DOTOP _*
      /mk_longident_mod_longident_LIDENT_: mod_longident . DOT LIDENT
      /mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT
      /simple_expr: simple_expr DOT mod_longident . DOTOP _*]
```

### Sample 1

Sentence:
```
false . X
```
Stack:
```
use_file: simple_expr DOT UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 158

```
| [_* /fun_expr: simple_expr DOT mod_longident DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ . RPAREN LESSMINUS expr
      /simple_expr: simple_expr DOT mod_longident DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ . RPAREN]
```

### Sample 1

Sentence:
```
false . X .+ ( function false -> .
```
Stack:
```
use_file: simple_expr DOT mod_longident DOTOP LPAREN FUNCTION ext list_attribute_ pattern MINUSGREATER DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
false . X .+ ( X ;
```
Stack:
```
use_file: simple_expr DOT mod_longident DOTOP LPAREN expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ; ;; sig * struct then to type _ val virtual when with
```

### Sample 3

Sentence:
```
false . X .+ ( X 'a'
```
Stack:
```
use_file: simple_expr DOT mod_longident DOTOP LPAREN constr_longident CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert ` ! | |] begin 'a' class : :> (*comment*) constraint do (**documentation *) done .. downto effect else end exception external false 1.0 for fun function functor >} >] if in include inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% <- let let* x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ;; sig "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 4

Sentence:
```
false . X .+ ( 'a'
```
Stack:
```
use_file: simple_expr DOT mod_longident DOTOP LPAREN CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] class : :> (*comment*) constraint do (**documentation *) done .. downto effect else end exception external for fun function functor >} >] if in include inherit initializer lazy [@@ [@@@ [> [< [%% <- let let* match >. method -> module mutable nonrec of open private ' {%%%%ext|s|} } ] rec ;; sig struct then to try type _ val virtual when while with
```

## Pattern 159

```
| [_* /fun_expr: simple_expr DOT mod_longident DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACKET LESSMINUS expr
      /simple_expr: simple_expr DOT mod_longident DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACKET]
```

### Sample 1

Sentence:
```
false . X .+ [ function false -> .
```
Stack:
```
use_file: simple_expr DOT mod_longident DOTOP LBRACKET FUNCTION ext list_attribute_ pattern MINUSGREATER DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
false . X .+ [ X ;
```
Stack:
```
use_file: simple_expr DOT mod_longident DOTOP LBRACKET expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } rec ) ; ;; sig * struct then to type _ val virtual when with
```

### Sample 3

Sentence:
```
false . X .+ [ X 'a'
```
Stack:
```
use_file: simple_expr DOT mod_longident DOTOP LBRACKET constr_longident CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert ` ! | |] begin 'a' class : :> (*comment*) constraint do (**documentation *) done .. downto effect else end exception external false 1.0 for fun function functor >} >] if in include inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% <- let let* x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ;; sig "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 4

Sentence:
```
false . X .+ [ 'a'
```
Stack:
```
use_file: simple_expr DOT mod_longident DOTOP LBRACKET CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] class : :> (*comment*) constraint do (**documentation *) done .. downto effect else end exception external for fun function functor >} >] if in include inherit initializer lazy [@@ [@@@ [> [< [%% <- let let* match >. method -> module mutable nonrec of open private ' {%%%%ext|s|} } rec ) ;; sig struct then to try type _ val virtual when while with
```

## Pattern 160

```
| [_* /fun_expr: simple_expr DOT mod_longident DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACE LESSMINUS expr
      /simple_expr: simple_expr DOT mod_longident DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACE]
```

### Sample 1

Sentence:
```
false . X .+ { function false -> .
```
Stack:
```
use_file: simple_expr DOT mod_longident DOTOP LBRACE FUNCTION ext list_attribute_ pattern MINUSGREATER DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
false . X .+ { X ;
```
Stack:
```
use_file: simple_expr DOT mod_longident DOTOP LBRACE expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

### Sample 3

Sentence:
```
false . X .+ { X 'a'
```
Stack:
```
use_file: simple_expr DOT mod_longident DOTOP LBRACE constr_longident CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert ` ! | |] begin 'a' class : :> (*comment*) constraint do (**documentation *) done .. downto effect else end exception external false 1.0 for fun function functor >} >] if in include inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% <- let let* x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ;; sig "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 4

Sentence:
```
false . X .+ { 'a'
```
Stack:
```
use_file: simple_expr DOT mod_longident DOTOP LBRACE CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] class : :> (*comment*) constraint do (**documentation *) done .. downto effect else end exception external for fun function functor >} >] if in include inherit initializer lazy [@@ [@@@ [> [< [%% <- let let* match >. method -> module mutable nonrec of open private ' {%%%%ext|s|} ] rec ) ;; sig struct then to try type _ val virtual when while with
```

## Pattern 161

```
| [_* /labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn . COMMA _*
      /letop_binding_body: pattern_no_exn . EQUAL seq_expr
      /reversed_labeled_tuple_pattern_pattern_no_exn_: pattern_no_exn . COMMA DOTDOT]
```

### Sample 1

Sentence:
```
let* false X
```
Stack:
```
use_file: LETOP constr_longident UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ! || |] begin class : := :> (*comment*) constraint do (**documentation *) done .. .+ downto else end external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

### Sample 2

Sentence:
```
let* false
```
Stack:
```
use_file: LETOP FALSE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ! || |] begin class := :> (*comment*) constraint do (**documentation *) done . .. .+ downto else end external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

### Sample 3

Sentence:
```
let* 'a'
```
Stack:
```
use_file: LETOP CHAR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! || |] begin 'a' class := :> (*comment*) constraint do (**documentation *) done . .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 162

```
| [_* /fun_expr: LETOP letop_bindings . IN seq_expr]
```

### Sample 1

Sentence:
```
let* x = function false -> .
```
Stack:
```
use_file: LETOP val_ident EQUAL FUNCTION ext list_attribute_ pattern MINUSGREATER DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
let* x = X ;
```
Stack:
```
use_file: LETOP val_ident EQUAL fun_expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

### Sample 3

Sentence:
```
let* x = X 'a'
```
Stack:
```
use_file: LETOP val_ident EQUAL constr_longident CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and as assert ` ! | |] begin 'a' class : :> , (*comment*) constraint do (**documentation *) done .. downto effect else end exception external false 1.0 for fun function functor >} >] if include inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% <- let let* x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ;; sig "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 4

Sentence:
```
let* x = 'a'
```
Stack:
```
use_file: LETOP val_ident EQUAL CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and as assert | |] class : :> , (*comment*) constraint do (**documentation *) done .. downto effect else end exception external for fun function functor >} >] if include inherit initializer lazy [@@ [@@@ [> [< [%% <- let let* match >. method -> module mutable nonrec of open private ' {%%%%ext|s|} } ] rec ) ;; sig struct then to try type _ val virtual when while with
```

### Sample 5

Sentence:
```
let* x
```
Stack:
```
use_file: LETOP LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and assert ! || |] begin class := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external for fun function functor > >} >] ## if include != ^ +! land ** inherit initializer lazy {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open or % +. += !+ private ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

## Pattern 163

```
| [_* /simple_expr: mod_longident DOT LPAREN seq_expr . RPAREN]
```

### Sample 1

Sentence:
```
X . ( function false -> .
```
Stack:
```
use_file: mod_longident DOT LPAREN FUNCTION ext list_attribute_ pattern MINUSGREATER DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
X . ( X ;
```
Stack:
```
use_file: mod_longident DOT LPAREN fun_expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or += private ? ' {%%%%ext|s|} } ] rec ; ;; sig * struct then to type _ val virtual when with
```

### Sample 3

Sentence:
```
X . ( X 'a'
```
Stack:
```
use_file: mod_longident DOT LPAREN constr_longident CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert ` ! | |] begin 'a' class : :> , (*comment*) constraint do (**documentation *) done .. downto effect else end exception external false 1.0 for fun function functor >} >] if in include inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% <- let let* x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ;; sig "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 4

Sentence:
```
X . ( 'a'
```
Stack:
```
use_file: mod_longident DOT LPAREN CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] class : :> , (*comment*) constraint do (**documentation *) done .. downto effect else end exception external for fun function functor >} >] if in include inherit initializer lazy [@@ [@@@ [> [< [%% <- let let* match >. method -> module mutable nonrec of open private ' {%%%%ext|s|} } ] rec ;; sig struct then to try type _ val virtual when while with
```

## Pattern 164

```
| [_* /simple_expr: mod_longident DOT LBRACKETBAR separated_or_terminated_nonempty_list_SEMI_expr_ . BARRBRACKET]
```

### Sample 1

Sentence:
```
X . [| function false -> .
```
Stack:
```
use_file: mod_longident DOT LBRACKETBAR FUNCTION ext list_attribute_ pattern MINUSGREATER DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
X . [| X ;
```
Stack:
```
use_file: mod_longident DOT LBRACKETBAR expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

### Sample 3

Sentence:
```
X . [| X 'a'
```
Stack:
```
use_file: mod_longident DOT LBRACKETBAR constr_longident CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert ` ! | begin 'a' class : :> (*comment*) constraint do (**documentation *) done .. downto effect else end exception external false 1.0 for fun function functor >} >] if in include inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% <- let let* x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ;; sig "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 4

Sentence:
```
X . [| 'a'
```
Stack:
```
use_file: mod_longident DOT LBRACKETBAR CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | class : :> (*comment*) constraint do (**documentation *) done .. downto effect else end exception external for fun function functor >} >] if in include inherit initializer lazy [@@ [@@@ [> [< [%% <- let let* match >. method -> module mutable nonrec of open private ' {%%%%ext|s|} } ] rec ) ;; sig struct then to try type _ val virtual when while with
```

## Pattern 165

```
| [_* /simple_expr: mod_longident DOT LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACKET]
```

### Sample 1

Sentence:
```
X . [ function false -> .
```
Stack:
```
use_file: mod_longident DOT LBRACKET FUNCTION ext list_attribute_ pattern MINUSGREATER DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
X . [ X ;
```
Stack:
```
use_file: mod_longident DOT LBRACKET expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } rec ) ; ;; sig * struct then to type _ val virtual when with
```

### Sample 3

Sentence:
```
X . [ X 'a'
```
Stack:
```
use_file: mod_longident DOT LBRACKET constr_longident CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert ` ! | |] begin 'a' class : :> (*comment*) constraint do (**documentation *) done .. downto effect else end exception external false 1.0 for fun function functor >} >] if in include inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% <- let let* x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ;; sig "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 4

Sentence:
```
X . [ 'a'
```
Stack:
```
use_file: mod_longident DOT LBRACKET CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] class : :> (*comment*) constraint do (**documentation *) done .. downto effect else end exception external for fun function functor >} >] if in include inherit initializer lazy [@@ [@@@ [> [< [%% <- let let* match >. method -> module mutable nonrec of open private ' {%%%%ext|s|} } rec ) ;; sig struct then to try type _ val virtual when while with
```

## Pattern 166

```
| [_* /simple_expr: mod_longident DOT LBRACELESS separated_or_terminated_nonempty_list_SEMI_object_expr_field_ . GREATERRBRACE]
```

### Sample 1

Sentence:
```
X . {< x ;
```
Stack:
```
use_file: mod_longident DOT LBRACELESS LIDENT option_preceded_EQUAL_expr__ SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
X . {< x
```
Stack:
```
use_file: mod_longident DOT LBRACELESS LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 167

```
| [_* /record_expr_content: simple_expr . WITH separated_or_terminated_nonempty_list_SEMI_record_expr_field_]
```

### Sample 1

Sentence:
```
{ 'a'
```
Stack:
```
use_file: LBRACE CHAR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. downto effect else end = exception external false 1.0 for fun function functor > >} >] if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

### Sample 2

Sentence:
```
{ x
```
Stack:
```
use_file: LBRACE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := , (*comment*) constraint do (**documentation *) done .. downto effect else end exception external false 1.0 for fun function functor > >} >] if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

## Pattern 168

```
| [_* /simple_expr: mod_longident DOT LBRACE record_expr_content . RBRACE]
```

### Sample 1

Sentence:
```
X . { x ;
```
Stack:
```
use_file: mod_longident DOT LBRACE label_longident option_type_constraint_ option_preceded_EQUAL_expr__ SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 2

Sentence:
```
X . { x :> {%%ext|s|}
```
Stack:
```
use_file: mod_longident DOT LBRACE label_longident COLONGREATER QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 3

Sentence:
```
X . { x :> x
```
Stack:
```
use_file: mod_longident DOT LBRACE label_longident COLONGREATER LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
X . { 'a' with x
```
Stack:
```
use_file: mod_longident DOT LBRACE simple_expr WITH LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 5

Sentence:
```
X . { x
```
Stack:
```
use_file: mod_longident DOT LBRACE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := , (*comment*) constraint do (**documentation *) done .. downto effect else end exception external false 1.0 for fun function functor > >} >] if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

## Pattern 169

```
| [_* /fun_expr: FOR ext list_attribute_ pattern EQUAL seq_expr . direction_flag seq_expr DO seq_expr DONE]
```

### Sample 1

Sentence:
```
for false = function false -> .
```
Stack:
```
use_file: FOR ext list_attribute_ pattern EQUAL FUNCTION ext list_attribute_ pattern MINUSGREATER DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
for false = X ;
```
Stack:
```
use_file: FOR ext list_attribute_ pattern EQUAL fun_expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then type _ val virtual when with
```

### Sample 3

Sentence:
```
for false = X 'a'
```
Stack:
```
use_file: FOR ext list_attribute_ pattern EQUAL constr_longident CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert ` ! | |] begin 'a' class : :> , (*comment*) constraint do (**documentation *) done .. effect else end exception external false 1.0 for fun function functor >} >] if in include inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% <- let let* x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ;; sig "s" struct then ~ true try type X _ val virtual when while with
```

### Sample 4

Sentence:
```
for false = 'a'
```
Stack:
```
use_file: FOR ext list_attribute_ pattern EQUAL CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] class : :> , (*comment*) constraint do (**documentation *) done .. effect else end exception external for fun function functor >} >] if in include inherit initializer lazy [@@ [@@@ [> [< [%% <- let let* match >. method -> module mutable nonrec of open private ' {%%%%ext|s|} } ] rec ) ;; sig struct then try type _ val virtual when while with
```

## Pattern 170

```
| [_* /fun_expr: FOR ext list_attribute_ pattern EQUAL seq_expr direction_flag . seq_expr DO seq_expr DONE]
```

### Sample 1

Sentence:
```
for false = X downto
```
Stack:
```
use_file: FOR ext list_attribute_ pattern EQUAL seq_expr DOWNTO
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

## Pattern 171

```
| [_* /fun_expr: FOR ext list_attribute_ pattern EQUAL seq_expr direction_flag seq_expr . DO seq_expr DONE]
```

### Sample 1

Sentence:
```
for false = X downto function false -> .
```
Stack:
```
use_file: FOR ext list_attribute_ pattern EQUAL seq_expr direction_flag FUNCTION ext list_attribute_ pattern MINUSGREATER DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
for false = X downto X ;
```
Stack:
```
use_file: FOR ext list_attribute_ pattern EQUAL seq_expr direction_flag fun_expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

### Sample 3

Sentence:
```
for false = X downto X 'a'
```
Stack:
```
use_file: FOR ext list_attribute_ pattern EQUAL seq_expr direction_flag constr_longident CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert ` ! | |] begin 'a' class : :> , (*comment*) constraint (**documentation *) done .. downto effect else end exception external false 1.0 for fun function functor >} >] if in include inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% <- let let* x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ;; sig "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 4

Sentence:
```
for false = X downto 'a'
```
Stack:
```
use_file: FOR ext list_attribute_ pattern EQUAL seq_expr direction_flag CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] class : :> , (*comment*) constraint (**documentation *) done .. downto effect else end exception external for fun function functor >} >] if in include inherit initializer lazy [@@ [@@@ [> [< [%% <- let let* match >. method -> module mutable nonrec of open private ' {%%%%ext|s|} } ] rec ) ;; sig struct then to try type _ val virtual when while with
```

## Pattern 172

```
| [_* /fun_expr: FOR ext list_attribute_ pattern EQUAL seq_expr direction_flag seq_expr DO seq_expr . DONE]
```

### Sample 1

Sentence:
```
for false = X downto X do function false -> .
```
Stack:
```
use_file: FOR ext list_attribute_ pattern EQUAL seq_expr direction_flag seq_expr DO FUNCTION ext list_attribute_ pattern MINUSGREATER DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
for false = X downto X do X ;
```
Stack:
```
use_file: FOR ext list_attribute_ pattern EQUAL seq_expr direction_flag seq_expr DO fun_expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

### Sample 3

Sentence:
```
for false = X downto X do X 'a'
```
Stack:
```
use_file: FOR ext list_attribute_ pattern EQUAL seq_expr direction_flag seq_expr DO constr_longident CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert ` ! | |] begin 'a' class : :> , (*comment*) constraint do (**documentation *) .. downto effect else end exception external false 1.0 for fun function functor >} >] if in include inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% <- let let* x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ;; sig "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 4

Sentence:
```
for false = X downto X do 'a'
```
Stack:
```
use_file: FOR ext list_attribute_ pattern EQUAL seq_expr direction_flag seq_expr DO CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] class : :> , (*comment*) constraint do (**documentation *) .. downto effect else end exception external for fun function functor >} >] if in include inherit initializer lazy [@@ [@@@ [> [< [%% <- let let* match >. method -> module mutable nonrec of open private ' {%%%%ext|s|} } ] rec ) ;; sig struct then to try type _ val virtual when while with
```

## Pattern 173

```
| [_* /match_case: pattern WHEN seq_expr . MINUSGREATER seq_expr]
```

### Sample 1

Sentence:
```
function false when function false -> .
```
Stack:
```
use_file: FUNCTION ext list_attribute_ pattern WHEN FUNCTION ext list_attribute_ pattern MINUSGREATER DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
function false when X ;
```
Stack:
```
use_file: FUNCTION ext list_attribute_ pattern WHEN fun_expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method module mutable nonrec of open ?label: or += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

### Sample 3

Sentence:
```
function false when X 'a'
```
Stack:
```
use_file: FUNCTION ext list_attribute_ pattern WHEN constr_longident CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert ` ! | |] begin 'a' class : :> , (*comment*) constraint do (**documentation *) done .. downto effect else end exception external false 1.0 for fun function functor >} >] if in include inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% <- let let* x ( match >. .< .~ method module mutable new nonrec object of open ?label: !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ;; sig "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 4

Sentence:
```
function false when 'a'
```
Stack:
```
use_file: FUNCTION ext list_attribute_ pattern WHEN CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] class : :> , (*comment*) constraint do (**documentation *) done .. downto effect else end exception external for fun function functor >} >] if in include inherit initializer lazy [@@ [@@@ [> [< [%% <- let let* match >. method module mutable nonrec of open private ' {%%%%ext|s|} } ] rec ) ;; sig struct then to try type _ val virtual when while with
```

## Pattern 174

```
| [_* /fun_expr: IF ext list_attribute_ seq_expr . THEN _*]
```

### Sample 1

Sentence:
```
if function false -> .
```
Stack:
```
use_file: IF ext list_attribute_ FUNCTION ext list_attribute_ pattern MINUSGREATER DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
if X ;
```
Stack:
```
use_file: IF ext list_attribute_ fun_expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct to type _ val virtual when with
```

### Sample 3

Sentence:
```
if X 'a'
```
Stack:
```
use_file: IF ext list_attribute_ constr_longident CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert ` ! | |] begin 'a' class : :> , (*comment*) constraint do (**documentation *) done .. downto effect else end exception external false 1.0 for fun function functor >} >] if in include inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% <- let let* x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ;; sig "s" struct ~ to true try type X _ val virtual when while with
```

### Sample 4

Sentence:
```
if 'a'
```
Stack:
```
use_file: IF ext list_attribute_ CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] class : :> , (*comment*) constraint do (**documentation *) done .. downto effect else end exception external for fun function functor >} >] if in include inherit initializer lazy [@@ [@@@ [> [< [%% <- let let* match >. method -> module mutable nonrec of open private ' {%%%%ext|s|} } ] rec ) ;; sig struct to try type _ val virtual when while with
```

## Pattern 175

```
| [_* /simple_expr: simple_expr DOT LPAREN seq_expr . RPAREN]
```

### Sample 1

Sentence:
```
{ 'a' . ( function false -> .
```
Stack:
```
use_file: LBRACE simple_expr DOT LPAREN FUNCTION ext list_attribute_ pattern MINUSGREATER DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
{ 'a' . ( X ;
```
Stack:
```
use_file: LBRACE simple_expr DOT LPAREN fun_expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or += private ? ' {%%%%ext|s|} } ] rec ; ;; sig * struct then to type _ val virtual when with
```

### Sample 3

Sentence:
```
{ 'a' . ( X 'a'
```
Stack:
```
use_file: LBRACE simple_expr DOT LPAREN constr_longident CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert ` ! | |] begin 'a' class : :> , (*comment*) constraint do (**documentation *) done .. downto effect else end exception external false 1.0 for fun function functor >} >] if in include inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% <- let let* x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ;; sig "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 4

Sentence:
```
{ 'a' . ( 'a'
```
Stack:
```
use_file: LBRACE simple_expr DOT LPAREN CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] class : :> , (*comment*) constraint do (**documentation *) done .. downto effect else end exception external for fun function functor >} >] if in include inherit initializer lazy [@@ [@@@ [> [< [%% <- let let* match >. method -> module mutable nonrec of open private ' {%%%%ext|s|} } ] rec ;; sig struct then to try type _ val virtual when while with
```

## Pattern 176

```
| [_* /simple_expr: simple_expr DOT LBRACKET seq_expr . RBRACKET]
```

### Sample 1

Sentence:
```
{ 'a' . [ function false -> .
```
Stack:
```
use_file: LBRACE simple_expr DOT LBRACKET FUNCTION ext list_attribute_ pattern MINUSGREATER DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
{ 'a' . [ X ;
```
Stack:
```
use_file: LBRACE simple_expr DOT LBRACKET fun_expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or += private ? ' {%%%%ext|s|} } rec ) ; ;; sig * struct then to type _ val virtual when with
```

### Sample 3

Sentence:
```
{ 'a' . [ X 'a'
```
Stack:
```
use_file: LBRACE simple_expr DOT LBRACKET constr_longident CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert ` ! | |] begin 'a' class : :> , (*comment*) constraint do (**documentation *) done .. downto effect else end exception external false 1.0 for fun function functor >} >] if in include inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% <- let let* x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ;; sig "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 4

Sentence:
```
{ 'a' . [ 'a'
```
Stack:
```
use_file: LBRACE simple_expr DOT LBRACKET CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] class : :> , (*comment*) constraint do (**documentation *) done .. downto effect else end exception external for fun function functor >} >] if in include inherit initializer lazy [@@ [@@@ [> [< [%% <- let let* match >. method -> module mutable nonrec of open private ' {%%%%ext|s|} } rec ) ;; sig struct then to try type _ val virtual when while with
```

## Pattern 177

```
| [_* /simple_expr: simple_expr DOT LBRACE seq_expr . RBRACE]
```

### Sample 1

Sentence:
```
{ 'a' . { function false -> .
```
Stack:
```
use_file: LBRACE simple_expr DOT LBRACE FUNCTION ext list_attribute_ pattern MINUSGREATER DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
{ 'a' . { X ;
```
Stack:
```
use_file: LBRACE simple_expr DOT LBRACE fun_expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or += private ? ' {%%%%ext|s|} ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

### Sample 3

Sentence:
```
{ 'a' . { X 'a'
```
Stack:
```
use_file: LBRACE simple_expr DOT LBRACE constr_longident CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert ` ! | |] begin 'a' class : :> , (*comment*) constraint do (**documentation *) done .. downto effect else end exception external false 1.0 for fun function functor >} >] if in include inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% <- let let* x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ;; sig "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 4

Sentence:
```
{ 'a' . { 'a'
```
Stack:
```
use_file: LBRACE simple_expr DOT LBRACE CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] class : :> , (*comment*) constraint do (**documentation *) done .. downto effect else end exception external for fun function functor >} >] if in include inherit initializer lazy [@@ [@@@ [> [< [%% <- let let* match >. method -> module mutable nonrec of open private ' {%%%%ext|s|} ] rec ) ;; sig struct then to try type _ val virtual when while with
```

## Pattern 178

```
| [_* /mk_longident_mod_longident_LIDENT_: mod_longident . DOT LIDENT
      /mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT
      /simple_expr: simple_expr DOT mod_longident . DOTOP _*]
```

### Sample 1

Sentence:
```
{ 'a' . X
```
Stack:
```
use_file: LBRACE simple_expr DOT UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 179

```
| [_* /simple_expr: simple_expr DOT mod_longident DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ . RPAREN]
```

### Sample 1

Sentence:
```
{ 'a' . X .+ ( function false -> .
```
Stack:
```
use_file: LBRACE simple_expr DOT mod_longident DOTOP LPAREN FUNCTION ext list_attribute_ pattern MINUSGREATER DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
{ 'a' . X .+ ( X ;
```
Stack:
```
use_file: LBRACE simple_expr DOT mod_longident DOTOP LPAREN expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ; ;; sig * struct then to type _ val virtual when with
```

### Sample 3

Sentence:
```
{ 'a' . X .+ ( X 'a'
```
Stack:
```
use_file: LBRACE simple_expr DOT mod_longident DOTOP LPAREN constr_longident CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert ` ! | |] begin 'a' class : :> (*comment*) constraint do (**documentation *) done .. downto effect else end exception external false 1.0 for fun function functor >} >] if in include inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% <- let let* x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ;; sig "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 4

Sentence:
```
{ 'a' . X .+ ( 'a'
```
Stack:
```
use_file: LBRACE simple_expr DOT mod_longident DOTOP LPAREN CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] class : :> (*comment*) constraint do (**documentation *) done .. downto effect else end exception external for fun function functor >} >] if in include inherit initializer lazy [@@ [@@@ [> [< [%% <- let let* match >. method -> module mutable nonrec of open private ' {%%%%ext|s|} } ] rec ;; sig struct then to try type _ val virtual when while with
```

## Pattern 180

```
| [_* /simple_expr: simple_expr DOT mod_longident DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACKET]
```

### Sample 1

Sentence:
```
{ 'a' . X .+ [ function false -> .
```
Stack:
```
use_file: LBRACE simple_expr DOT mod_longident DOTOP LBRACKET FUNCTION ext list_attribute_ pattern MINUSGREATER DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
{ 'a' . X .+ [ X ;
```
Stack:
```
use_file: LBRACE simple_expr DOT mod_longident DOTOP LBRACKET expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } rec ) ; ;; sig * struct then to type _ val virtual when with
```

### Sample 3

Sentence:
```
{ 'a' . X .+ [ X 'a'
```
Stack:
```
use_file: LBRACE simple_expr DOT mod_longident DOTOP LBRACKET constr_longident CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert ` ! | |] begin 'a' class : :> (*comment*) constraint do (**documentation *) done .. downto effect else end exception external false 1.0 for fun function functor >} >] if in include inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% <- let let* x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ;; sig "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 4

Sentence:
```
{ 'a' . X .+ [ 'a'
```
Stack:
```
use_file: LBRACE simple_expr DOT mod_longident DOTOP LBRACKET CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] class : :> (*comment*) constraint do (**documentation *) done .. downto effect else end exception external for fun function functor >} >] if in include inherit initializer lazy [@@ [@@@ [> [< [%% <- let let* match >. method -> module mutable nonrec of open private ' {%%%%ext|s|} } rec ) ;; sig struct then to try type _ val virtual when while with
```

## Pattern 181

```
| [_* /simple_expr: simple_expr DOT mod_longident DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACE]
```

### Sample 1

Sentence:
```
{ 'a' . X .+ { function false -> .
```
Stack:
```
use_file: LBRACE simple_expr DOT mod_longident DOTOP LBRACE FUNCTION ext list_attribute_ pattern MINUSGREATER DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
{ 'a' . X .+ { X ;
```
Stack:
```
use_file: LBRACE simple_expr DOT mod_longident DOTOP LBRACE expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

### Sample 3

Sentence:
```
{ 'a' . X .+ { X 'a'
```
Stack:
```
use_file: LBRACE simple_expr DOT mod_longident DOTOP LBRACE constr_longident CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert ` ! | |] begin 'a' class : :> (*comment*) constraint do (**documentation *) done .. downto effect else end exception external false 1.0 for fun function functor >} >] if in include inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% <- let let* x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ;; sig "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 4

Sentence:
```
{ 'a' . X .+ { 'a'
```
Stack:
```
use_file: LBRACE simple_expr DOT mod_longident DOTOP LBRACE CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] class : :> (*comment*) constraint do (**documentation *) done .. downto effect else end exception external for fun function functor >} >] if in include inherit initializer lazy [@@ [@@@ [> [< [%% <- let let* match >. method -> module mutable nonrec of open private ' {%%%%ext|s|} ] rec ) ;; sig struct then to try type _ val virtual when while with
```

## Pattern 182

```
| [_* /simple_expr: simple_expr DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ . RPAREN]
```

### Sample 1

Sentence:
```
{ 'a' .+ ( function false -> .
```
Stack:
```
use_file: LBRACE simple_expr DOTOP LPAREN FUNCTION ext list_attribute_ pattern MINUSGREATER DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
{ 'a' .+ ( X ;
```
Stack:
```
use_file: LBRACE simple_expr DOTOP LPAREN expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ; ;; sig * struct then to type _ val virtual when with
```

### Sample 3

Sentence:
```
{ 'a' .+ ( X 'a'
```
Stack:
```
use_file: LBRACE simple_expr DOTOP LPAREN constr_longident CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert ` ! | |] begin 'a' class : :> (*comment*) constraint do (**documentation *) done .. downto effect else end exception external false 1.0 for fun function functor >} >] if in include inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% <- let let* x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ;; sig "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 4

Sentence:
```
{ 'a' .+ ( 'a'
```
Stack:
```
use_file: LBRACE simple_expr DOTOP LPAREN CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] class : :> (*comment*) constraint do (**documentation *) done .. downto effect else end exception external for fun function functor >} >] if in include inherit initializer lazy [@@ [@@@ [> [< [%% <- let let* match >. method -> module mutable nonrec of open private ' {%%%%ext|s|} } ] rec ;; sig struct then to try type _ val virtual when while with
```

## Pattern 183

```
| [_* /simple_expr: simple_expr DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACKET]
```

### Sample 1

Sentence:
```
{ 'a' .+ [ function false -> .
```
Stack:
```
use_file: LBRACE simple_expr DOTOP LBRACKET FUNCTION ext list_attribute_ pattern MINUSGREATER DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
{ 'a' .+ [ X ;
```
Stack:
```
use_file: LBRACE simple_expr DOTOP LBRACKET expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } rec ) ; ;; sig * struct then to type _ val virtual when with
```

### Sample 3

Sentence:
```
{ 'a' .+ [ X 'a'
```
Stack:
```
use_file: LBRACE simple_expr DOTOP LBRACKET constr_longident CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert ` ! | |] begin 'a' class : :> (*comment*) constraint do (**documentation *) done .. downto effect else end exception external false 1.0 for fun function functor >} >] if in include inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% <- let let* x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ;; sig "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 4

Sentence:
```
{ 'a' .+ [ 'a'
```
Stack:
```
use_file: LBRACE simple_expr DOTOP LBRACKET CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] class : :> (*comment*) constraint do (**documentation *) done .. downto effect else end exception external for fun function functor >} >] if in include inherit initializer lazy [@@ [@@@ [> [< [%% <- let let* match >. method -> module mutable nonrec of open private ' {%%%%ext|s|} } rec ) ;; sig struct then to try type _ val virtual when while with
```

## Pattern 184

```
| [_* /simple_expr: simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACE]
```

### Sample 1

Sentence:
```
{ 'a' .+ { function false -> .
```
Stack:
```
use_file: LBRACE simple_expr DOTOP LBRACE FUNCTION ext list_attribute_ pattern MINUSGREATER DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
{ 'a' .+ { X ;
```
Stack:
```
use_file: LBRACE simple_expr DOTOP LBRACE expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

### Sample 3

Sentence:
```
{ 'a' .+ { X 'a'
```
Stack:
```
use_file: LBRACE simple_expr DOTOP LBRACE constr_longident CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert ` ! | |] begin 'a' class : :> (*comment*) constraint do (**documentation *) done .. downto effect else end exception external false 1.0 for fun function functor >} >] if in include inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% <- let let* x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ;; sig "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 4

Sentence:
```
{ 'a' .+ { 'a'
```
Stack:
```
use_file: LBRACE simple_expr DOTOP LBRACE CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] class : :> (*comment*) constraint do (**documentation *) done .. downto effect else end exception external for fun function functor >} >] if in include inherit initializer lazy [@@ [@@@ [> [< [%% <- let let* match >. method -> module mutable nonrec of open private ' {%%%%ext|s|} ] rec ) ;; sig struct then to try type _ val virtual when while with
```

## Pattern 185

```
| [_* /simple_expr: BEGIN ext list_attribute_ seq_expr . END]
```

### Sample 1

Sentence:
```
begin function false -> .
```
Stack:
```
use_file: BEGIN ext list_attribute_ FUNCTION ext list_attribute_ pattern MINUSGREATER DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
begin X ;
```
Stack:
```
use_file: BEGIN ext list_attribute_ fun_expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

### Sample 3

Sentence:
```
begin X 'a'
```
Stack:
```
use_file: BEGIN ext list_attribute_ constr_longident CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert ` ! | |] begin 'a' class : :> , (*comment*) constraint do (**documentation *) done .. downto effect else exception external false 1.0 for fun function functor >} >] if in include inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% <- let let* x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ;; sig "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 4

Sentence:
```
begin 'a'
```
Stack:
```
use_file: BEGIN ext list_attribute_ CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] class : :> , (*comment*) constraint do (**documentation *) done .. downto effect else exception external for fun function functor >} >] if in include inherit initializer lazy [@@ [@@@ [> [< [%% <- let let* match >. method -> module mutable nonrec of open private ' {%%%%ext|s|} } ] rec ) ;; sig struct then to try type _ val virtual when while with
```

## Pattern 186

```
| [_* /simple_expr: LBRACE record_expr_content . RBRACE]
```

### Sample 1

Sentence:
```
{ x ;
```
Stack:
```
use_file: LBRACE label_longident option_type_constraint_ option_preceded_EQUAL_expr__ SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 2

Sentence:
```
{ x :> {%%ext|s|}
```
Stack:
```
use_file: LBRACE label_longident COLONGREATER QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 3

Sentence:
```
{ x :> x
```
Stack:
```
use_file: LBRACE label_longident COLONGREATER LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
{ 'a' with x
```
Stack:
```
use_file: LBRACE simple_expr WITH LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 5

Sentence:
```
{ x
```
Stack:
```
use_file: LBRACE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := , (*comment*) constraint do (**documentation *) done .. downto effect else end exception external false 1.0 for fun function functor > >} >] if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

## Pattern 187

```
| [_* /simple_expr: LBRACELESS separated_or_terminated_nonempty_list_SEMI_object_expr_field_ . GREATERRBRACE]
```

### Sample 1

Sentence:
```
{< x ;
```
Stack:
```
use_file: LBRACELESS LIDENT option_preceded_EQUAL_expr__ SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
{< x
```
Stack:
```
use_file: LBRACELESS LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 188

```
| [_* /simple_expr: LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACKET]
```

### Sample 1

Sentence:
```
[ function false -> .
```
Stack:
```
use_file: LBRACKET FUNCTION ext list_attribute_ pattern MINUSGREATER DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
[ X ;
```
Stack:
```
use_file: LBRACKET expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } rec ) ; ;; sig * struct then to type _ val virtual when with
```

### Sample 3

Sentence:
```
[ X 'a'
```
Stack:
```
use_file: LBRACKET constr_longident CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert ` ! | |] begin 'a' class : :> (*comment*) constraint do (**documentation *) done .. downto effect else end exception external false 1.0 for fun function functor >} >] if in include inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% <- let let* x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ;; sig "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 4

Sentence:
```
[ 'a'
```
Stack:
```
use_file: LBRACKET CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] class : :> (*comment*) constraint do (**documentation *) done .. downto effect else end exception external for fun function functor >} >] if in include inherit initializer lazy [@@ [@@@ [> [< [%% <- let let* match >. method -> module mutable nonrec of open private ' {%%%%ext|s|} } rec ) ;; sig struct then to try type _ val virtual when while with
```

## Pattern 189

```
| [_* /structure_item: INCLUDE ext list_attribute_ . module_expr list_post_item_attribute_]
```

### Sample 1

Sentence:
```
include [@ and ]
```
Stack:
```
use_file: INCLUDE ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ val virtual when while with
```

### Sample 2

Sentence:
```
include % and
```
Stack:
```
use_file: INCLUDE PERCENT AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ val virtual when while with
```

## Pattern 190

```
| [_* /primitive_declaration: EXTERNAL ext list_attribute_ . val_ident COLON possibly_poly_core_type_ EQUAL nonempty_list_raw_string_ list_post_item_attribute_]
```

### Sample 1

Sentence:
```
external [@ and ]
```
Stack:
```
use_file: EXTERNAL ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
external % and
```
Stack:
```
use_file: EXTERNAL PERCENT AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 191

```
| [_* /primitive_declaration: EXTERNAL ext list_attribute_ val_ident . COLON possibly_poly_core_type_ EQUAL nonempty_list_raw_string_ list_post_item_attribute_]
```

### Sample 1

Sentence:
```
external x
```
Stack:
```
use_file: EXTERNAL ext list_attribute_ LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 192

```
| [_* /primitive_declaration: EXTERNAL ext list_attribute_ val_ident COLON possibly_poly_core_type_ . EQUAL nonempty_list_raw_string_ list_post_item_attribute_]
```

### Sample 1

Sentence:
```
external x : {%%ext|s|} [@ and ]
```
Stack:
```
use_file: EXTERNAL ext list_attribute_ val_ident COLON core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
external x : {%%ext|s|} as ' x
```
Stack:
```
use_file: EXTERNAL ext list_attribute_ val_ident COLON alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
external x : {%%ext|s|}
```
Stack:
```
use_file: EXTERNAL ext list_attribute_ val_ident COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
external x : x
```
Stack:
```
use_file: EXTERNAL ext list_attribute_ val_ident COLON LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 193

```
| [_* /sig_exception_declaration: EXCEPTION ext list_attribute_ . constr_ident generalized_constructor_arguments list_attribute_ list_post_item_attribute_
      /str_exception_declaration: EXCEPTION ext list_attribute_ . constr_ident EQUAL constr_longident list_attribute_ list_post_item_attribute_]
```

### Sample 1

Sentence:
```
exception [@ and ]
```
Stack:
```
use_file: EXCEPTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```

### Sample 2

Sentence:
```
exception % and
```
Stack:
```
use_file: EXCEPTION PERCENT AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```

## Pattern 194

```
| [_* /label_declaration: mutable_flag . LIDENT COLON possibly_poly_core_type_no_attr_ list_attribute_
      /label_declaration_semi: mutable_flag . LIDENT COLON possibly_poly_core_type_no_attr_ list_attribute_ SEMI list_attribute_]
```

### Sample 1

Sentence:
```
exception false : { mutable
```
Stack:
```
use_file: EXCEPTION ext list_attribute_ constr_ident COLON LBRACE MUTABLE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 195

```
| [_* /possibly_poly_core_type_no_attr_: reversed_nonempty_llist_typevar_ . DOT alias_type]
```

### Sample 1

Sentence:
```
exception false : { x : ' x ' x
```
Stack:
```
use_file: EXCEPTION ext list_attribute_ constr_ident COLON LBRACE mutable_flag LIDENT COLON reversed_nonempty_llist_typevar_ QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 196

```
| [_* /constructor_arguments: LBRACE label_declarations . RBRACE]
```

### Sample 1

Sentence:
```
exception false : { x : {%%ext|s|} as ' x
```
Stack:
```
use_file: EXCEPTION ext list_attribute_ constr_ident COLON LBRACE mutable_flag LIDENT COLON alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
exception false : { x : {%%ext|s|} ;
```
Stack:
```
use_file: EXCEPTION ext list_attribute_ constr_ident COLON LBRACE mutable_flag LIDENT COLON possibly_poly_core_type_no_attr_ list_attribute_ SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
exception false : { x : {%%ext|s|}
```
Stack:
```
use_file: EXCEPTION ext list_attribute_ constr_ident COLON LBRACE mutable_flag LIDENT COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 197

```
| [_* /constructor_arguments: reversed_separated_nonempty_llist_STAR_atomic_type_ . STAR atomic_type]
```

### Sample 1

Sentence:
```
exception false : {%%ext|s|} * x
```
Stack:
```
use_file: EXCEPTION ext list_attribute_ constr_ident COLON reversed_separated_nonempty_llist_STAR_atomic_type_ STAR LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 198

```
| [_* /generalized_constructor_arguments: COLON reversed_nonempty_llist_typevar_ . DOT _*]
```

### Sample 1

Sentence:
```
exception false : ' x ' x
```
Stack:
```
use_file: EXCEPTION ext list_attribute_ constr_ident COLON reversed_nonempty_llist_typevar_ QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 199

```
| [_* /generalized_constructor_arguments: COLON reversed_nonempty_llist_typevar_ DOT constructor_arguments . MINUSGREATER atomic_type]
```

### Sample 1

Sentence:
```
exception false : ' x . { x : {%%ext|s|} }
```
Stack:
```
use_file: EXCEPTION ext list_attribute_ constr_ident COLON reversed_nonempty_llist_typevar_ DOT LBRACE label_declarations RBRACE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 200

```
| [_* /generalized_constructor_arguments: COLON constructor_arguments . MINUSGREATER atomic_type]
```

### Sample 1

Sentence:
```
exception false : { x : {%%ext|s|} }
```
Stack:
```
use_file: EXCEPTION ext list_attribute_ constr_ident COLON LBRACE label_declarations RBRACE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 201

```
| [_* /open_description: OPEN BANG ext list_attribute_ . mod_ext_longident list_post_item_attribute_]
```

### Sample 1

Sentence:
```
open ! [@ and ]
```
Stack:
```
interface: OPEN BANG ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 2

Sentence:
```
open ! % and
```
Stack:
```
interface: OPEN BANG PERCENT AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 202

```
| [_* /open_description: OPEN ext list_attribute_ . mod_ext_longident list_post_item_attribute_]
```

### Sample 1

Sentence:
```
open [@ and ]
```
Stack:
```
interface: OPEN ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 2

Sentence:
```
open % and
```
Stack:
```
interface: OPEN PERCENT AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 203

```
| [_* /module_type_declaration: MODULE TYPE ext list_attribute_ . ident option_preceded_EQUAL_module_type__ list_post_item_attribute_
      /module_type_subst: MODULE TYPE ext list_attribute_ . ident COLONEQUAL module_type list_post_item_attribute_]
```

### Sample 1

Sentence:
```
module type [@ and ]
```
Stack:
```
interface: MODULE TYPE ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 2

Sentence:
```
module type % and
```
Stack:
```
interface: MODULE TYPE PERCENT AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 204

```
| [_* /module_subst: MODULE ext list_attribute_ . UIDENT COLONEQUAL mod_ext_longident list_post_item_attribute_
      /signature_item: MODULE ext list_attribute_ . module_name _*
      /signature_item: MODULE ext list_attribute_ . REC module_name COLON module_type list_post_item_attribute_ list_and_module_declaration_]
```

### Sample 1

Sentence:
```
module [@ and ]
```
Stack:
```
interface: MODULE ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

### Sample 2

Sentence:
```
module % and
```
Stack:
```
interface: MODULE PERCENT AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

## Pattern 205

```
| [_* /signature_item: MODULE ext list_attribute_ REC module_name . COLON module_type list_post_item_attribute_ list_and_module_declaration_]
```

### Sample 1

Sentence:
```
module rec X
```
Stack:
```
interface: MODULE ext list_attribute_ REC UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 206

```
| [_* /list_and_module_declaration_: AND list_attribute_ . module_name COLON module_type list_post_item_attribute_ list_and_module_declaration_]
```

### Sample 1

Sentence:
```
module rec X : {%%ext|s|} and [@ and ]
```
Stack:
```
interface: MODULE ext list_attribute_ REC module_name COLON module_type list_post_item_attribute_ AND LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

## Pattern 207

```
| [_* /list_and_module_declaration_: AND list_attribute_ module_name . COLON module_type list_post_item_attribute_ list_and_module_declaration_]
```

### Sample 1

Sentence:
```
module rec X : {%%ext|s|} and X
```
Stack:
```
interface: MODULE ext list_attribute_ REC module_name COLON module_type list_post_item_attribute_ AND list_attribute_ UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 208

```
| [_* /signature_item: MODULE ext list_attribute_ module_name . module_declaration_body list_post_item_attribute_
      /signature_item: MODULE ext list_attribute_ module_name . EQUAL mod_longident list_post_item_attribute_]
```

### Sample 1

Sentence:
```
module _
```
Stack:
```
interface: MODULE ext list_attribute_ UNDERSCORE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
module X
```
Stack:
```
interface: MODULE ext list_attribute_ UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 209

```
| [_* /module_declaration_body: functor_arg . module_declaration_body]
```

### Sample 1

Sentence:
```
module X ( )
```
Stack:
```
interface: MODULE ext list_attribute_ module_name LPAREN RPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 210

```
| [_* /signature_item: INCLUDE ext list_attribute_ . module_type list_post_item_attribute_]
```

### Sample 1

Sentence:
```
include [@ and ]
```
Stack:
```
interface: INCLUDE ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 2

Sentence:
```
include % and
```
Stack:
```
interface: INCLUDE PERCENT AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 211

```
| [_* /sig_exception_declaration: EXCEPTION ext list_attribute_ . constr_ident generalized_constructor_arguments list_attribute_ list_post_item_attribute_]
```

### Sample 1

Sentence:
```
exception [@ and ]
```
Stack:
```
interface: EXCEPTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```

### Sample 2

Sentence:
```
exception % and
```
Stack:
```
interface: EXCEPTION PERCENT AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```

## Pattern 212

```
| [_* /formal_class_parameters: LBRACKET reversed_separated_nonempty_llist_COMMA_type_parameter_ . RBRACKET]
```

### Sample 1

Sentence:
```
class [ _
```
Stack:
```
use_file: CLASS ext list_attribute_ virtual_flag LBRACKET type_variance UNDERSCORE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 213

```
| [_* /class_type_declarations: CLASS TYPE ext list_attribute_ virtual_flag formal_class_parameters . LIDENT EQUAL class_signature list_post_item_attribute_ list_and_class_type_declaration_]
```

### Sample 1

Sentence:
```
class type [ _ ]
```
Stack:
```
interface: CLASS TYPE ext list_attribute_ virtual_flag LBRACKET reversed_separated_nonempty_llist_COMMA_type_parameter_ RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
class type virtual
```
Stack:
```
interface: CLASS TYPE ext list_attribute_ VIRTUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 214

```
| [_* /class_self_type: LPAREN core_type . RPAREN]
```

### Sample 1

Sentence:
```
class x : object ( {%%ext|s|} [@ and ]
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON OBJECT list_attribute_ LPAREN core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
class x : object ( {%%ext|s|} as ' x
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON OBJECT list_attribute_ LPAREN alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
class x : object ( {%%ext|s|}
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON OBJECT list_attribute_ LPAREN QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
class x : object ( x
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON OBJECT list_attribute_ LPAREN LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 215

```
| [_* /class_sig_field: VAL list_attribute_ mutable_virtual_flags . LIDENT COLON core_type list_post_item_attribute_]
```

### Sample 1

Sentence:
```
class x : object val virtual
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON OBJECT list_attribute_ class_self_type VAL list_attribute_ VIRTUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
class x : object val mutable
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON OBJECT list_attribute_ class_self_type VAL list_attribute_ MUTABLE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val when while with
```

## Pattern 216

```
| [_* /class_sig_field: METHOD list_attribute_ private_virtual_flags . LIDENT COLON possibly_poly_core_type_ list_post_item_attribute_]
```

### Sample 1

Sentence:
```
class x : object method virtual
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON OBJECT list_attribute_ class_self_type METHOD list_attribute_ VIRTUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
class x : object method private
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON OBJECT list_attribute_ class_self_type METHOD list_attribute_ PRIVATE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val when while with
```

## Pattern 217

```
| [_* /class_sig_field: INHERIT list_attribute_ . class_signature list_post_item_attribute_]
```

### Sample 1

Sentence:
```
class x : object inherit [@ and ]
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON OBJECT list_attribute_ class_self_type INHERIT LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@@ [@@@ [| [> [< [%% < <- let* ( match >. .< .~ method - -. -> module mutable new nonrec of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 218

```
| [_* /class_signature: LET OPEN BANG list_attribute_ . mod_longident IN class_signature]
```

### Sample 1

Sentence:
```
class x : let open ! [@ and ]
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON LET OPEN BANG LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 219

```
| [_* /class_signature: LET OPEN BANG list_attribute_ mod_longident . IN class_signature
      /mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT]
```

### Sample 1

Sentence:
```
class x : let open ! X
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON LET OPEN BANG list_attribute_ UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 220

```
| [_* /class_signature: LBRACKET reversed_separated_nonempty_llist_COMMA_core_type_ . RBRACKET clty_longident]
```

### Sample 1

Sentence:
```
class x : [ {%%ext|s|} [@ and ]
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON LBRACKET core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
class type x = [ {%%ext|s|} as ' x
```
Stack:
```
interface: CLASS TYPE ext list_attribute_ virtual_flag formal_class_parameters LIDENT EQUAL LBRACKET alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
class type x = [ {%%ext|s|}
```
Stack:
```
interface: CLASS TYPE ext list_attribute_ virtual_flag formal_class_parameters LIDENT EQUAL LBRACKET QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
class type x = [ x
```
Stack:
```
interface: CLASS TYPE ext list_attribute_ virtual_flag formal_class_parameters LIDENT EQUAL LBRACKET LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 221

```
| [_* /class_signature: LET OPEN list_attribute_ . mod_longident IN class_signature]
```

### Sample 1

Sentence:
```
class x : let open [@ and ]
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON LET OPEN LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 222

```
| [_* /class_signature: LET OPEN list_attribute_ mod_longident . IN class_signature
      /mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT]
```

### Sample 1

Sentence:
```
class x : let open X
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON LET OPEN list_attribute_ UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 223

```
| [_* /class_sig_field: CONSTRAINT list_attribute_ . constrain_field list_post_item_attribute_]
```

### Sample 1

Sentence:
```
class x : object constraint [@ and ]
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON OBJECT list_attribute_ class_self_type CONSTRAINT LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

## Pattern 224

```
| [_* /constrain_field: core_type . EQUAL core_type]
```

### Sample 1

Sentence:
```
class x : object constraint {%%ext|s|} [@ and ]
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON OBJECT list_attribute_ class_self_type CONSTRAINT list_attribute_ core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
class x : object constraint {%%ext|s|} as ' x
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON OBJECT list_attribute_ class_self_type CONSTRAINT list_attribute_ alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
class x : object constraint {%%ext|s|}
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON OBJECT list_attribute_ class_self_type CONSTRAINT list_attribute_ QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
class x : object constraint x
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON OBJECT list_attribute_ class_self_type CONSTRAINT list_attribute_ LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 225

```
| [_* /class_signature: OBJECT list_attribute_ class_self_type list_text_csig_class_sig_field__ . END]
```

### Sample 1

Sentence:
```
class x : object ( {%%ext|s|} )
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON OBJECT list_attribute_ LPAREN core_type RPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) do (**documentation *) done . .. .+ downto effect else = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** initializer 1 ~label: lazy { {< [ [@ [@@ [| [> [< [% < <- let let* x ( match >. .< .~ - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ virtual when while with
```

### Sample 2

Sentence:
```
class x : object {%%%%ext|s|}
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON OBJECT list_attribute_ class_self_type QUOTED_STRING_ITEM
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) do (**documentation *) done . .. .+ downto effect else = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** initializer 1 ~label: lazy { {< [ [@ [| [> [< [% < <- let let* x ( match >. .< .~ - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ virtual when while with
```

## Pattern 226

```
| [_* /list_and_class_type_declaration_: AND list_attribute_ virtual_flag formal_class_parameters . LIDENT EQUAL class_signature list_post_item_attribute_ list_and_class_type_declaration_]
```

### Sample 1

Sentence:
```
class type x = x and [ _ ]
```
Stack:
```
interface: CLASS TYPE ext list_attribute_ virtual_flag formal_class_parameters LIDENT EQUAL class_signature list_post_item_attribute_ AND list_attribute_ virtual_flag LBRACKET reversed_separated_nonempty_llist_COMMA_type_parameter_ RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
class type x = x and virtual
```
Stack:
```
interface: CLASS TYPE ext list_attribute_ virtual_flag formal_class_parameters LIDENT EQUAL class_signature list_post_item_attribute_ AND list_attribute_ VIRTUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 227

```
| [_* /signature_item: CLASS ext list_attribute_ virtual_flag formal_class_parameters . LIDENT COLON class_type list_post_item_attribute_ list_and_class_description_]
```

### Sample 1

Sentence:
```
class [ _ ]
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag LBRACKET reversed_separated_nonempty_llist_COMMA_type_parameter_ RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
class virtual
```
Stack:
```
interface: CLASS ext list_attribute_ VIRTUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 228

```
| [_* /class_type: LIDENT COLON tuple_type . MINUSGREATER class_type]
```

### Sample 1

Sentence:
```
class x : x : x
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON LIDENT COLON LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 229

```
| [_* /class_type: tuple_type . MINUSGREATER class_type]
```

### Sample 1

Sentence:
```
class x : _
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON UNDERSCORE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 2

Sentence:
```
object inherit ( x : {%%ext|s|}
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ LPAREN class_expr COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 3

Sentence:
```
class x : x
```
Stack:
```
use_file: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
object inherit ( x : x
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ LPAREN class_expr COLON LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 230

```
| [_* /class_type: optlabel . tuple_type MINUSGREATER class_type]
```

### Sample 1

Sentence:
```
class x : ?label:
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON OPTLABEL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

## Pattern 231

```
| [_* /class_type: optlabel tuple_type . MINUSGREATER class_type]
```

### Sample 1

Sentence:
```
class x : ?label: x
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON optlabel LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 232

```
| [_* /list_and_class_description_: AND list_attribute_ virtual_flag formal_class_parameters . LIDENT COLON class_type list_post_item_attribute_ list_and_class_description_]
```

### Sample 1

Sentence:
```
class x : {%%ext|s|} and [ _ ]
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON class_type list_post_item_attribute_ AND list_attribute_ virtual_flag LBRACKET reversed_separated_nonempty_llist_COMMA_type_parameter_ RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
class x : {%%ext|s|} and virtual
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON class_type list_post_item_attribute_ AND list_attribute_ VIRTUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 233

```
| [_* /list_generic_and_type_declaration_type_kind__: AND list_attribute_ type_parameters . LIDENT type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_ list_generic_and_type_declaration_type_kind__]
```

### Sample 1

Sentence:
```
type x and _
```
Stack:
```
interface: generic_type_declaration_nonrec_flag_type_kind_ AND list_attribute_ type_variance UNDERSCORE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 234

```
| [_* /list_generic_and_type_declaration_type_subst_kind__: AND list_attribute_ type_parameters . LIDENT COLONEQUAL nonempty_type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_ list_generic_and_type_declaration_type_subst_kind__]
```

### Sample 1

Sentence:
```
type x := false and _
```
Stack:
```
interface: generic_type_declaration_no_nonrec_flag_type_subst_kind_ AND list_attribute_ type_variance UNDERSCORE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 235

```
| [_* /nonempty_type_kind: LBRACE label_declarations . RBRACE]
```

### Sample 1

Sentence:
```
type x := { x : {%%ext|s|} as ' x
```
Stack:
```
interface: TYPE ext list_attribute_ type_parameters LIDENT COLONEQUAL LBRACE mutable_flag LIDENT COLON alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
type x := { x : {%%ext|s|} ;
```
Stack:
```
interface: TYPE ext list_attribute_ type_parameters LIDENT COLONEQUAL LBRACE mutable_flag LIDENT COLON possibly_poly_core_type_no_attr_ list_attribute_ SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
type x := { x : {%%ext|s|}
```
Stack:
```
interface: TYPE ext list_attribute_ type_parameters LIDENT COLONEQUAL LBRACE mutable_flag LIDENT COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 236

```
| [_* /nonempty_type_kind: core_type EQUAL PRIVATE LBRACE label_declarations . RBRACE]
```

### Sample 1

Sentence:
```
type x := {%%ext|s|} = private { x : {%%ext|s|} as ' x
```
Stack:
```
interface: TYPE ext list_attribute_ type_parameters LIDENT COLONEQUAL core_type EQUAL PRIVATE LBRACE mutable_flag LIDENT COLON alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
type x := {%%ext|s|} = private { x : {%%ext|s|} ;
```
Stack:
```
interface: TYPE ext list_attribute_ type_parameters LIDENT COLONEQUAL core_type EQUAL PRIVATE LBRACE mutable_flag LIDENT COLON possibly_poly_core_type_no_attr_ list_attribute_ SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
type x := {%%ext|s|} = private { x : {%%ext|s|}
```
Stack:
```
interface: TYPE ext list_attribute_ type_parameters LIDENT COLONEQUAL core_type EQUAL PRIVATE LBRACE mutable_flag LIDENT COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 237

```
| [_* /nonempty_type_kind: core_type EQUAL LBRACE label_declarations . RBRACE]
```

### Sample 1

Sentence:
```
type x := {%%ext|s|} = { x : {%%ext|s|} as ' x
```
Stack:
```
interface: TYPE ext list_attribute_ type_parameters LIDENT COLONEQUAL core_type EQUAL LBRACE mutable_flag LIDENT COLON alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
type x := {%%ext|s|} = { x : {%%ext|s|} ;
```
Stack:
```
interface: TYPE ext list_attribute_ type_parameters LIDENT COLONEQUAL core_type EQUAL LBRACE mutable_flag LIDENT COLON possibly_poly_core_type_no_attr_ list_attribute_ SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
type x := {%%ext|s|} = { x : {%%ext|s|}
```
Stack:
```
interface: TYPE ext list_attribute_ type_parameters LIDENT COLONEQUAL core_type EQUAL LBRACE mutable_flag LIDENT COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 238

```
| [_* /local_structure_item: CLASS ext list_attribute_ virtual_flag formal_class_parameters . LIDENT class_fun_binding list_post_item_attribute_ list_and_class_declaration_]
```

### Sample 1

Sentence:
```
class [ _ ]
```
Stack:
```
use_file: CLASS ext list_attribute_ virtual_flag LBRACKET reversed_separated_nonempty_llist_COMMA_type_parameter_ RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
class virtual
```
Stack:
```
use_file: CLASS ext list_attribute_ VIRTUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 239

```
| [_* /value: BANG list_attribute_ mutable_flag . LIDENT _*]
```

### Sample 1

Sentence:
```
object val ! mutable
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern VAL BANG list_attribute_ MUTABLE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 240

```
| [_* /value: BANG list_attribute_ mutable_flag LIDENT type_constraint . EQUAL seq_expr]
```

### Sample 1

Sentence:
```
object val ! x :> {%%ext|s|} [@ and ]
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern VAL BANG list_attribute_ mutable_flag LIDENT COLONGREATER core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
object val ! x : {%%ext|s|} as ' x
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern VAL BANG list_attribute_ mutable_flag LIDENT COLON alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
object val ! x :> {%%ext|s|}
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern VAL BANG list_attribute_ mutable_flag LIDENT COLONGREATER QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
object val ! x : {%%ext|s|}
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern VAL BANG list_attribute_ mutable_flag LIDENT COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 241

```
| [_* /value: list_attribute_ virtual_with_mutable_flag . LIDENT COLON core_type]
```

### Sample 1

Sentence:
```
object val virtual mutable
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern VAL list_attribute_ VIRTUAL MUTABLE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
object val virtual
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern VAL list_attribute_ VIRTUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 242

```
| [_* /value: list_attribute_ mutable_flag . LIDENT _*]
```

### Sample 1

Sentence:
```
object val mutable
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern VAL list_attribute_ MUTABLE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val when while with
```

## Pattern 243

```
| [_* /value: list_attribute_ mutable_flag LIDENT type_constraint . EQUAL seq_expr]
```

### Sample 1

Sentence:
```
object val x :> {%%ext|s|} [@ and ]
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern VAL list_attribute_ mutable_flag LIDENT COLONGREATER core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
object val x : {%%ext|s|} as ' x
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern VAL list_attribute_ mutable_flag LIDENT COLON alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
object val x :> {%%ext|s|}
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern VAL list_attribute_ mutable_flag LIDENT COLONGREATER QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
object val x : {%%ext|s|}
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern VAL list_attribute_ mutable_flag LIDENT COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 244

```
| [_* /method_: BANG list_attribute_ private_flag . LIDENT _*]
```

### Sample 1

Sentence:
```
object method ! private
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern METHOD BANG list_attribute_ PRIVATE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 245

```
| [_* /method_: BANG list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ . DOT core_type EQUAL seq_expr]
```

### Sample 1

Sentence:
```
object method ! x : type x
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern METHOD BANG list_attribute_ private_flag LIDENT COLON TYPE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 246

```
| [_* /method_: BANG list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type . EQUAL seq_expr]
```

### Sample 1

Sentence:
```
object method ! x : type x . {%%ext|s|} [@ and ]
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern METHOD BANG list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
object method ! x : type x . {%%ext|s|} as ' x
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern METHOD BANG list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
object method ! x : type x . {%%ext|s|}
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern METHOD BANG list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
object method ! x : type x . x
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern METHOD BANG list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 247

```
| [_* /method_: BANG list_attribute_ private_flag LIDENT COLON possibly_poly_core_type_ . EQUAL seq_expr]
```

### Sample 1

Sentence:
```
object method ! x : {%%ext|s|} [@ and ]
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern METHOD BANG list_attribute_ private_flag LIDENT COLON core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
object method ! x : {%%ext|s|} as ' x
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern METHOD BANG list_attribute_ private_flag LIDENT COLON alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
object method ! x : {%%ext|s|}
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern METHOD BANG list_attribute_ private_flag LIDENT COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
object method ! x : x
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern METHOD BANG list_attribute_ private_flag LIDENT COLON LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 248

```
| [_* /method_: list_attribute_ virtual_with_private_flag . LIDENT COLON possibly_poly_core_type_]
```

### Sample 1

Sentence:
```
object method virtual private
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern METHOD list_attribute_ VIRTUAL PRIVATE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
object method virtual
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern METHOD list_attribute_ VIRTUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 249

```
| [_* /method_: list_attribute_ private_flag . LIDENT _*]
```

### Sample 1

Sentence:
```
object method private
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern METHOD list_attribute_ PRIVATE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val when while with
```

## Pattern 250

```
| [_* /method_: list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ . DOT core_type EQUAL seq_expr]
```

### Sample 1

Sentence:
```
object method x : type x
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern METHOD list_attribute_ private_flag LIDENT COLON TYPE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 251

```
| [_* /method_: list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type . EQUAL seq_expr]
```

### Sample 1

Sentence:
```
object method x : type x . {%%ext|s|} [@ and ]
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern METHOD list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
object method x : type x . {%%ext|s|} as ' x
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern METHOD list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
object method x : type x . {%%ext|s|}
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern METHOD list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
object method x : type x . x
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern METHOD list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 252

```
| [_* /method_: list_attribute_ private_flag LIDENT COLON possibly_poly_core_type_ . EQUAL seq_expr]
```

### Sample 1

Sentence:
```
object method x : {%%ext|s|} [@ and ]
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern METHOD list_attribute_ private_flag LIDENT COLON core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
object method x : {%%ext|s|} as ' x
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern METHOD list_attribute_ private_flag LIDENT COLON alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
object method x : {%%ext|s|}
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern METHOD list_attribute_ private_flag LIDENT COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
object method x : x
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern METHOD list_attribute_ private_flag LIDENT COLON LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 253

```
| [_* /class_field: INITIALIZER list_attribute_ . seq_expr list_post_item_attribute_]
```

### Sample 1

Sentence:
```
object initializer [@ and ]
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INITIALIZER LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

## Pattern 254

```
| [_* /class_field: INHERIT BANG list_attribute_ . class_expr option_preceded_AS_mkrhs_LIDENT___ list_post_item_attribute_]
```

### Sample 1

Sentence:
```
object inherit ! [@ and ]
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT BANG LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@@ [@@@ [| [> [< [%% < <- let* match >. .< .~ method - -. -> module mutable new nonrec of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 255

```
| [_* /class_expr: LET OPEN BANG list_attribute_ . mod_longident IN class_expr]
```

### Sample 1

Sentence:
```
object inherit let open ! [@ and ]
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ LET OPEN BANG LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 256

```
| [_* /class_expr: LET OPEN BANG list_attribute_ mod_longident . IN class_expr
      /mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT]
```

### Sample 1

Sentence:
```
object inherit let open ! X
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ LET OPEN BANG list_attribute_ UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 257

```
| [_* /class_simple_expr: LBRACKET reversed_separated_nonempty_llist_COMMA_core_type_ . RBRACKET class_longident]
```

### Sample 1

Sentence:
```
object inherit [ {%%ext|s|} [@ and ]
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ LBRACKET core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
object inherit [ {%%ext|s|} as ' x
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ LBRACKET alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
object inherit [ {%%ext|s|}
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ LBRACKET QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
object inherit [ x
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ LBRACKET LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 258

```
| [_* /class_expr: FUN list_attribute_ . class_fun_def]
```

### Sample 1

Sentence:
```
object inherit fun [@ and ]
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ FUN LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer lazy {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open or % +. += !+ private ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

## Pattern 259

```
| [_* /class_fun_def: simple_param_pattern . MINUSGREATER class_expr
      /class_fun_def: simple_param_pattern . class_fun_def]
```

### Sample 1

Sentence:
```
object inherit fun false
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ FUN list_attribute_ FALSE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. module mutable new nonrec object of open or % +. += !+ private ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

### Sample 2

Sentence:
```
object inherit fun 'a'
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ FUN list_attribute_ CHAR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .+ downto effect else end = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. module mutable new nonrec object of open or % +. += !+ private ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

## Pattern 260

```
| [_* /class_expr: let_bindings_no_ext_ . IN class_expr]
```

### Sample 1

Sentence:
```
class x = let x and x [@@ and ]
```
Stack:
```
use_file: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT EQUAL let_bindings_no_ext_ AND list_attribute_ let_binding_body LBRACKETATAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
class x = let x = X 'a'
```
Stack:
```
use_file: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT EQUAL LET list_attribute_ rec_flag val_ident EQUAL constr_longident CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and* as assert ` ! | |] begin 'a' class : :> , (*comment*) constraint do (**documentation *) done .. downto effect else end exception external false 1.0 for fun function functor >} >] if include inherit initializer 1 ~label: lazy { {< [ [@@@ [| [> [< [% [%% <- let let* x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ;; sig "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
class x = let x = X ;
```
Stack:
```
use_file: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT EQUAL LET list_attribute_ rec_flag val_ident EQUAL fun_expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## include != ^ +! land ** inherit initializer [@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

### Sample 4

Sentence:
```
class x = let x = x
```
Stack:
```
use_file: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT EQUAL LET list_attribute_ rec_flag val_ident EQUAL LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
and* as assert | |] class : :> , (*comment*) constraint do (**documentation *) done .. downto effect else end exception external for fun function functor >} >] if include inherit initializer lazy [@@@ [> [< [%% let let* match >. method -> module mutable nonrec of open private ' {%%%%ext|s|} } ] rec ) ;; sig struct then to try type _ val virtual when while with
```

### Sample 5

Sentence:
```
class x = let x
```
Stack:
```
use_file: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT EQUAL LET list_attribute_ rec_flag LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and* assert ! || |] begin class := (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external for fun function functor > >} >] ## if include != ^ +! land ** inherit initializer lazy {< [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open or % +. += !+ private ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

## Pattern 261

```
| [_* /class_expr: LET OPEN list_attribute_ . mod_longident IN class_expr]
```

### Sample 1

Sentence:
```
object inherit let open [@ and ]
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ LET OPEN LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 262

```
| [_* /class_expr: LET OPEN list_attribute_ mod_longident . IN class_expr
      /mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT]
```

### Sample 1

Sentence:
```
object inherit let open X
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ LET OPEN list_attribute_ UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 263

```
| [_* /let_bindings_no_ext_: LET list_attribute_ rec_flag . let_binding_body list_post_item_attribute_]
```

### Sample 1

Sentence:
```
object inherit let rec
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ LET list_attribute_ REC
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

## Pattern 264

```
| [_* /class_simple_expr: LPAREN class_expr . RPAREN
      /class_simple_expr: LPAREN class_expr . COLON class_type RPAREN]
```

### Sample 1

Sentence:
```
object inherit ( {%%ext|s|}
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ LPAREN QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
object inherit ( x
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ LPAREN LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert | || |] class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer lazy [@@ [@@@ [> [< [%% < <- let let* match >. method - -. -> module mutable nonrec of open or % + +. += private ' {%%%%ext|s|} } ] rec ; ;; sig * struct then to try type _ val virtual when while with
```

## Pattern 265

```
| [_* /class_simple_expr: LPAREN class_expr COLON class_type . RPAREN]
```

### Sample 1

Sentence:
```
object inherit ( x : [ {%%ext|s|} ] x
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ LPAREN class_expr COLON LBRACKET reversed_separated_nonempty_llist_COMMA_core_type_ RBRACKET LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
object inherit ( x : {%%ext|s|}
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ LPAREN class_expr COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 3

Sentence:
```
object inherit ( x : x
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ LPAREN class_expr COLON LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 266

```
| [_* /class_field: INHERIT list_attribute_ . class_expr option_preceded_AS_mkrhs_LIDENT___ list_post_item_attribute_]
```

### Sample 1

Sentence:
```
object inherit [@ and ]
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@@ [@@@ [| [> [< [%% < <- let* match >. .< .~ method - -. -> module mutable new nonrec of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 267

```
| [_* /class_field: CONSTRAINT list_attribute_ . constrain_field list_post_item_attribute_]
```

### Sample 1

Sentence:
```
object constraint [@ and ]
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern CONSTRAINT LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

## Pattern 268

```
| [_* /class_simple_expr: OBJECT list_attribute_ class_self_pattern list_text_cstr_class_field__ . END]
```

### Sample 1

Sentence:
```
object inherit object ( false )
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ OBJECT list_attribute_ LPAREN pattern RPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) do (**documentation *) done . .. .+ downto effect else = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** 1 ~label: lazy { {< [ [@ [@@ [| [> [< [% < <- let let* x ( match >. .< .~ - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ virtual when while with
```

### Sample 2

Sentence:
```
object inherit object {%%%%ext|s|}
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ OBJECT list_attribute_ class_self_pattern QUOTED_STRING_ITEM
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) do (**documentation *) done . .. .+ downto effect else = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** 1 ~label: lazy { {< [ [@ [| [> [< [% < <- let let* x ( match >. .< .~ - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ virtual when while with
```

## Pattern 269

```
| [_* /class_fun_binding: COLON class_type . EQUAL class_expr]
```

### Sample 1

Sentence:
```
class x : [ {%%ext|s|} ] x
```
Stack:
```
use_file: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON LBRACKET reversed_separated_nonempty_llist_COMMA_core_type_ RBRACKET LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
class x : {%%ext|s|}
```
Stack:
```
use_file: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 3

Sentence:
```
class x : x
```
Stack:
```
use_file: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 270

```
| [_* /class_fun_binding: simple_param_pattern . class_fun_binding]
```

### Sample 1

Sentence:
```
class x false
```
Stack:
```
use_file: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT FALSE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open or % +. += !+ private ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

### Sample 2

Sentence:
```
class x 'a'
```
Stack:
```
use_file: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT CHAR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class :: := :> , (*comment*) constraint do (**documentation *) done . .+ downto effect else end exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open or % +. += !+ private ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

## Pattern 271

```
| [_* /list_and_class_declaration_: AND list_attribute_ virtual_flag formal_class_parameters . LIDENT class_fun_binding list_post_item_attribute_ list_and_class_declaration_]
```

### Sample 1

Sentence:
```
class x = x and [ _ ]
```
Stack:
```
use_file: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT class_fun_binding list_post_item_attribute_ AND list_attribute_ virtual_flag LBRACKET reversed_separated_nonempty_llist_COMMA_type_parameter_ RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
class x = x and virtual
```
Stack:
```
use_file: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT class_fun_binding list_post_item_attribute_ AND list_attribute_ VIRTUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 272

```
| [_* /let_bindings_ext_: LET ext list_attribute_ rec_flag . let_binding_body list_post_item_attribute_]
```

### Sample 1

Sentence:
```
let rec
```
Stack:
```
use_file: LET ext list_attribute_ REC
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

## Pattern 273

```
| [_* /floating_attribute: LBRACKETATATAT attr_id attr_payload . RBRACKET]
```

### Sample 1

Sentence:
```
[@@@ and : {%%ext|s|}
```
Stack:
```
use_file: LBRACKETATATAT attr_id COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 2

Sentence:
```
[@@@ and {%%%%ext|s|}
```
Stack:
```
use_file: LBRACKETATATAT attr_id QUOTED_STRING_ITEM
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = false 1.0 for fun function functor > >} >] # ## if in != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [| [> [< [% < <- let* x ( match >. .< .~ method - -. -> mutable new nonrec object of ?label: or % + +. += !+ private ? ' {%%ext|s|} } rec ) ; sig * "s" struct then ~ to true try X _ virtual when while with
```

### Sample 3

Sentence:
```
[@@@ and 'a'
```
Stack:
```
use_file: LBRACKETATATAT attr_id CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] : :> , (*comment*) constraint do (**documentation *) done .. downto effect else end for fun function functor >} >] if in inherit initializer lazy [> [< <- let* match >. method -> mutable nonrec of private ' } rec ) sig struct then to try _ virtual when while with
```

### Sample 4

Sentence:
```
[@@@ and :
```
Stack:
```
use_file: LBRACKETATATAT attr_id COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = false 1.0 for fun function functor > >} >] ## if in != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [| <- let let* match >. .< .~ method - -. -> mutable new nonrec object of or % + +. += !+ private } rec ) ; sig * "s" struct then ~ to true try virtual when while with
```

## Pattern 274

```
| [_* /item_extension: LBRACKETPERCENTPERCENT attr_id payload . RBRACKET]
```

### Sample 1

Sentence:
```
[%% and : {%%ext|s|}
```
Stack:
```
use_file: LBRACKETPERCENTPERCENT attr_id COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 2

Sentence:
```
[%% and {%%%%ext|s|}
```
Stack:
```
use_file: LBRACKETPERCENTPERCENT attr_id QUOTED_STRING_ITEM
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = false 1.0 for fun function functor > >} >] # ## if in != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [| [> [< [% < <- let* x ( match >. .< .~ method - -. -> mutable new nonrec object of ?label: or % + +. += !+ private ? ' {%%ext|s|} } rec ) ; sig * "s" struct then ~ to true try X _ virtual when while with
```

### Sample 3

Sentence:
```
[%% and 'a'
```
Stack:
```
use_file: LBRACKETPERCENTPERCENT attr_id CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] : :> , (*comment*) constraint do (**documentation *) done .. downto effect else end for fun function functor >} >] if in inherit initializer lazy [> [< <- let* match >. method -> mutable nonrec of private ' } rec ) sig struct then to try _ virtual when while with
```

### Sample 4

Sentence:
```
[%% and :
```
Stack:
```
use_file: LBRACKETPERCENTPERCENT attr_id COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = false 1.0 for fun function functor > >} >] ## if in != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [| <- let let* match >. .< .~ method - -. -> mutable new nonrec object of or % + +. += !+ private } rec ) ; sig * "s" struct then ~ to true try virtual when while with
```

## Pattern 275

```
| [_* /fun_expr: LET ext list_attribute_ local_structure_item . IN seq_expr]
```

### Sample 1

Sentence:
```
let [@@@ and ]
```
Stack:
```
use_file: LET ext list_attribute_ LBRACKETATATAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
let {%%%%ext|s|}
```
Stack:
```
use_file: LET ext list_attribute_ QUOTED_STRING_ITEM
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 276

```
| [_* /simple_param_pattern: QUESTION LPAREN label_let_pattern option_preceded_EQUAL_seq_expr__ . RPAREN]
```

### Sample 1

Sentence:
```
let* x ? ( x = X ;
```
Stack:
```
use_file: LETOP val_ident QUESTION LPAREN label_let_pattern EQUAL fun_expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or += private ? ' {%%%%ext|s|} } ] rec ; ;; sig * struct then to type _ val virtual when with
```

### Sample 2

Sentence:
```
let* x ? ( x = x
```
Stack:
```
use_file: LETOP val_ident QUESTION LPAREN label_let_pattern EQUAL LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] class : :> , (*comment*) constraint do (**documentation *) done .. downto effect else end exception external for fun function functor >} >] if in include inherit initializer lazy [@@ [@@@ [> [< [%% let let* match >. method -> module mutable nonrec of open private ' {%%%%ext|s|} } ] rec ;; sig struct then to try type _ val virtual when while with
```

### Sample 3

Sentence:
```
let* x ? ( x
```
Stack:
```
use_file: LETOP val_ident QUESTION LPAREN LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 277

```
| [_* /simple_expr: LBRACKETBAR separated_or_terminated_nonempty_list_SEMI_expr_ . BARRBRACKET]
```

### Sample 1

Sentence:
```
[| function false -> .
```
Stack:
```
use_file: LBRACKETBAR FUNCTION ext list_attribute_ pattern MINUSGREATER DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
[| X ;
```
Stack:
```
use_file: LBRACKETBAR expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

### Sample 3

Sentence:
```
[| X 'a'
```
Stack:
```
use_file: LBRACKETBAR constr_longident CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert ` ! | begin 'a' class : :> (*comment*) constraint do (**documentation *) done .. downto effect else end exception external false 1.0 for fun function functor >} >] if in include inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% <- let let* x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ;; sig "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 4

Sentence:
```
[| 'a'
```
Stack:
```
use_file: LBRACKETBAR CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | class : :> (*comment*) constraint do (**documentation *) done .. downto effect else end exception external for fun function functor >} >] if in include inherit initializer lazy [@@ [@@@ [> [< [%% <- let let* match >. method -> module mutable nonrec of open private ' {%%%%ext|s|} } ] rec ) ;; sig struct then to try type _ val virtual when while with
```

## Pattern 278

```
| [_* /simple_expr: LPAREN MODULE ext list_attribute_ . module_expr _*]
```

### Sample 1

Sentence:
```
( module [@ and ]
```
Stack:
```
use_file: LPAREN MODULE ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ val virtual when while with
```

### Sample 2

Sentence:
```
( module % and
```
Stack:
```
use_file: LPAREN MODULE PERCENT AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ val virtual when while with
```

## Pattern 279

```
| [_* /simple_expr: LPAREN MODULE ext list_attribute_ module_expr . RPAREN
      /simple_expr: LPAREN MODULE ext list_attribute_ module_expr . COLON module_type RPAREN]
```

### Sample 1

Sentence:
```
( module {%%ext|s|}
```
Stack:
```
use_file: LPAREN MODULE ext list_attribute_ QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
( module X
```
Stack:
```
use_file: LPAREN MODULE ext list_attribute_ UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 280

```
| [_* /simple_expr: LPAREN MODULE ext list_attribute_ module_expr COLON module_type . RPAREN]
```

### Sample 1

Sentence:
```
( module {%%ext|s|} : x
```
Stack:
```
use_file: LPAREN MODULE ext list_attribute_ module_expr COLON LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

### Sample 2

Sentence:
```
( module {%%ext|s|} : X
```
Stack:
```
use_file: LPAREN MODULE ext list_attribute_ module_expr COLON UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

## Pattern 281

```
| [_* /simple_expr: LPAREN seq_expr . RPAREN
      /simple_expr: LPAREN seq_expr . type_constraint RPAREN]
```

### Sample 1

Sentence:
```
( function false -> .
```
Stack:
```
use_file: LPAREN FUNCTION ext list_attribute_ pattern MINUSGREATER DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
( X ;
```
Stack:
```
use_file: LPAREN fun_expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or += private ? ' {%%%%ext|s|} } ] rec ; ;; sig * struct then to type _ val virtual when with
```

### Sample 3

Sentence:
```
( X 'a'
```
Stack:
```
use_file: LPAREN constr_longident CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert ` ! | |] begin 'a' class , (*comment*) constraint do (**documentation *) done .. downto effect else end exception external false 1.0 for fun function functor >} >] if in include inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% <- let let* x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ;; sig "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 4

Sentence:
```
( 'a'
```
Stack:
```
use_file: LPAREN CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] class , (*comment*) constraint do (**documentation *) done .. downto effect else end exception external for fun function functor >} >] if in include inherit initializer lazy [@@ [@@@ [> [< [%% <- let let* match >. method -> module mutable nonrec of open private ' {%%%%ext|s|} } ] rec ;; sig struct then to try type _ val virtual when while with
```

## Pattern 282

```
| [_* /simple_expr: LPAREN seq_expr type_constraint . RPAREN]
```

### Sample 1

Sentence:
```
( X :> {%%ext|s|} [@ and ]
```
Stack:
```
use_file: LPAREN seq_expr COLONGREATER core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
( X : {%%ext|s|} as ' x
```
Stack:
```
use_file: LPAREN seq_expr COLON alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
( X :> {%%ext|s|}
```
Stack:
```
use_file: LPAREN seq_expr COLONGREATER QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
( X : {%%ext|s|}
```
Stack:
```
use_file: LPAREN seq_expr COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 283

```
| [_* /fun_expr: MATCH ext list_attribute_ seq_expr . WITH reversed_preceded_or_separated_nonempty_llist_BAR_match_case_]
```

### Sample 1

Sentence:
```
match function false -> .
```
Stack:
```
use_file: MATCH ext list_attribute_ FUNCTION ext list_attribute_ pattern MINUSGREATER DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

### Sample 2

Sentence:
```
match X ;
```
Stack:
```
use_file: MATCH ext list_attribute_ fun_expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type _ val virtual when
```

### Sample 3

Sentence:
```
match X 'a'
```
Stack:
```
use_file: MATCH ext list_attribute_ constr_longident CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert ` ! | |] begin 'a' class : :> , (*comment*) constraint do (**documentation *) done .. downto effect else end exception external false 1.0 for fun function functor >} >] if in include inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% <- let let* x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ;; sig "s" struct then ~ to true try type X _ val virtual when while
```

### Sample 4

Sentence:
```
match 'a'
```
Stack:
```
use_file: MATCH ext list_attribute_ CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] class : :> , (*comment*) constraint do (**documentation *) done .. downto effect else end exception external for fun function functor >} >] if in include inherit initializer lazy [@@ [@@@ [> [< [%% <- let let* match >. method -> module mutable nonrec of open private ' {%%%%ext|s|} } ] rec ) ;; sig struct then to try type _ val virtual when while
```

## Pattern 284

```
| [_* /simple_expr: METAOCAML_BRACKET_OPEN seq_expr . METAOCAML_BRACKET_CLOSE]
```

### Sample 1

Sentence:
```
.< function false -> .
```
Stack:
```
use_file: METAOCAML_BRACKET_OPEN FUNCTION ext list_attribute_ pattern MINUSGREATER DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
.< X ;
```
Stack:
```
use_file: METAOCAML_BRACKET_OPEN fun_expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- method -> module mutable nonrec of open ?label: or += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

### Sample 3

Sentence:
```
.< X 'a'
```
Stack:
```
use_file: METAOCAML_BRACKET_OPEN constr_longident CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert ` ! | |] begin 'a' class : :> , (*comment*) constraint do (**documentation *) done .. downto effect else end exception external false 1.0 for fun function functor >} >] if in include inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% <- let let* x ( match .< .~ method -> module mutable new nonrec object of open ?label: !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ;; sig "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 4

Sentence:
```
.< 'a'
```
Stack:
```
use_file: METAOCAML_BRACKET_OPEN CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] class : :> , (*comment*) constraint do (**documentation *) done .. downto effect else end exception external for fun function functor >} >] if in include inherit initializer lazy [@@ [@@@ [> [< [%% <- let let* match method -> module mutable nonrec of open private ' {%%%%ext|s|} } ] rec ) ;; sig struct then to try type _ val virtual when while with
```

## Pattern 285

```
| [_* /paren_module_expr: LPAREN VAL list_attribute_ expr_colon_package_type . RPAREN]
```

### Sample 1

Sentence:
```
( val function false -> .
```
Stack:
```
parse_module_expr: LPAREN VAL list_attribute_ FUNCTION ext list_attribute_ pattern MINUSGREATER DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class :: := (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
( val X :> X
```
Stack:
```
parse_module_expr: LPAREN VAL list_attribute_ expr COLONGREATER UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

### Sample 3

Sentence:
```
( val X ~ x
```
Stack:
```
parse_module_expr: LPAREN VAL list_attribute_ simple_expr TILDE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] class (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end exception external for fun function functor >} >] # ## if in include inherit initializer lazy [@@ [@@@ [> [< [%% <- let let* match >. method -> module mutable nonrec of open private ' {%%%%ext|s|} } ] rec ; ;; sig struct then to try type _ val virtual when while with
```

### Sample 4

Sentence:
```
( val X 'a'
```
Stack:
```
parse_module_expr: LPAREN VAL list_attribute_ constr_longident CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert ` ! | |] begin 'a' class (*comment*) constraint do (**documentation *) done .. downto effect else end exception external false 1.0 for fun function functor >} >] if in include inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% <- let let* x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 5

Sentence:
```
( val 'a'
```
Stack:
```
parse_module_expr: LPAREN VAL list_attribute_ CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] class (*comment*) constraint do (**documentation *) done .. downto effect else end exception external for fun function functor >} >] if in include inherit initializer lazy [@@ [@@@ [> [< [%% <- let let* match >. method -> module mutable nonrec of open private ' {%%%%ext|s|} } ] rec ; ;; sig struct then to try type _ val virtual when while with
```

## Pattern 286

```
| [_* /open_declaration: OPEN ext list_attribute_ . module_expr list_post_item_attribute_]
```

### Sample 1

Sentence:
```
open [@ and ]
```
Stack:
```
use_file: OPEN ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ val virtual when while with
```

### Sample 2

Sentence:
```
open % and
```
Stack:
```
use_file: OPEN PERCENT AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ val virtual when while with
```

## Pattern 287

```
| [_* /module_expr: STRUCT list_attribute_ structure . END]
```

### Sample 1

Sentence:
```
struct ;;
```
Stack:
```
parse_module_expr: STRUCT list_attribute_ SEMISEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else = functor > >} >] # ## in != ^ +! land ** inherit initializer [@ [@@ [> [< < <- >. method -> mutable nonrec of ?label: or % += private ? ' } ] rec ) ; sig * struct then to _ virtual when with
```

### Sample 2

Sentence:
```
struct {%%%%ext|s|}
```
Stack:
```
parse_module_expr: STRUCT list_attribute_ QUOTED_STRING_ITEM
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else = false 1.0 for fun function functor > >} >] # ## if in != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [| [> [< [% < <- let* x ( match >. .< .~ method - -. -> mutable new nonrec object of ?label: or % + +. += !+ private ? ' {%%ext|s|} } ] rec ) ; sig * "s" struct then ~ to true try X _ virtual when while with
```

### Sample 3

Sentence:
```
struct 'a'
```
Stack:
```
parse_module_expr: STRUCT list_attribute_ CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] : :> , (*comment*) constraint do (**documentation *) done .. downto effect else for fun function functor >} >] if in inherit initializer lazy [> [< <- let* match >. method -> mutable nonrec of private ' } ] rec ) sig struct then to try _ virtual when while with
```

## Pattern 288

```
| [_* /simple_pattern_not_ident: LPAREN MODULE ext list_attribute_ module_name COLON module_type . RPAREN]
```

### Sample 1

Sentence:
```
( module X : x
```
Stack:
```
parse_pattern: LPAREN MODULE ext list_attribute_ module_name COLON LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

### Sample 2

Sentence:
```
( module X : X
```
Stack:
```
parse_pattern: LPAREN MODULE ext list_attribute_ module_name COLON UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

## Pattern 289

```
| [_* /class_self_pattern: LPAREN pattern . RPAREN
      /class_self_pattern: LPAREN pattern . COLON core_type RPAREN
      /labeled_tuple_pat_element_list_pattern_: pattern . COMMA _*
      /reversed_labeled_tuple_pattern_pattern_: pattern . COMMA DOTDOT]
```

### Sample 1

Sentence:
```
object ( false
```
Stack:
```
use_file: OBJECT ext list_attribute_ LPAREN FALSE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ! || |] begin class := :> (*comment*) constraint do (**documentation *) done . .. .+ downto else end = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ; ;; sig * struct then to try type val virtual when while with
```

### Sample 2

Sentence:
```
object ( 'a'
```
Stack:
```
use_file: OBJECT ext list_attribute_ LPAREN CHAR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! || |] begin 'a' class := :> (*comment*) constraint do (**documentation *) done . .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 290

```
| [_* /class_self_pattern: LPAREN pattern COLON core_type . RPAREN]
```

### Sample 1

Sentence:
```
object ( false : {%%ext|s|} [@ and ]
```
Stack:
```
use_file: OBJECT ext list_attribute_ LPAREN pattern COLON core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
object ( false : {%%ext|s|} as ' x
```
Stack:
```
use_file: OBJECT ext list_attribute_ LPAREN pattern COLON alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
object ( false : {%%ext|s|}
```
Stack:
```
use_file: OBJECT ext list_attribute_ LPAREN pattern COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
object ( false : x
```
Stack:
```
use_file: OBJECT ext list_attribute_ LPAREN pattern COLON LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 291

```
| [_* /simple_expr: OBJECT ext list_attribute_ class_self_pattern list_text_cstr_class_field__ . END]
```

### Sample 1

Sentence:
```
object % and
```
Stack:
```
use_file: OBJECT PERCENT AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) do (**documentation *) done .. .+ downto effect else = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** 1 ~label: lazy { {< [ [@@ [| [> [< [% < <- let let* x match >. .< .~ - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ virtual when while with
```

### Sample 2

Sentence:
```
object {%%%%ext|s|}
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern QUOTED_STRING_ITEM
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) do (**documentation *) done . .. .+ downto effect else = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** 1 ~label: lazy { {< [ [@ [| [> [< [% < <- let let* x ( match >. .< .~ - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ virtual when while with
```

## Pattern 292

```
| [_* /fun_expr: TRY ext list_attribute_ seq_expr . WITH reversed_preceded_or_separated_nonempty_llist_BAR_match_case_]
```

### Sample 1

Sentence:
```
try function false -> .
```
Stack:
```
use_file: TRY ext list_attribute_ FUNCTION ext list_attribute_ pattern MINUSGREATER DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

### Sample 2

Sentence:
```
try X ;
```
Stack:
```
use_file: TRY ext list_attribute_ fun_expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type _ val virtual when
```

### Sample 3

Sentence:
```
try X 'a'
```
Stack:
```
use_file: TRY ext list_attribute_ constr_longident CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert ` ! | |] begin 'a' class : :> , (*comment*) constraint do (**documentation *) done .. downto effect else end exception external false 1.0 for fun function functor >} >] if in include inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% <- let let* x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ;; sig "s" struct then ~ to true try type X _ val virtual when while
```

### Sample 4

Sentence:
```
try 'a'
```
Stack:
```
use_file: TRY ext list_attribute_ CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] class : :> , (*comment*) constraint do (**documentation *) done .. downto effect else end exception external for fun function functor >} >] if in include inherit initializer lazy [@@ [@@@ [> [< [%% <- let let* match >. method -> module mutable nonrec of open private ' {%%%%ext|s|} } ] rec ) ;; sig struct then to try type _ val virtual when while
```

## Pattern 293

```
| [_* /post_item_attribute: LBRACKETATAT attr_id attr_payload . RBRACKET]
```

### Sample 1

Sentence:
```
X [@@ and : {%%ext|s|}
```
Stack:
```
implementation: seq_expr LBRACKETATAT attr_id COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 2

Sentence:
```
X [@@ and {%%%%ext|s|}
```
Stack:
```
implementation: seq_expr LBRACKETATAT attr_id QUOTED_STRING_ITEM
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = false 1.0 for fun function functor > >} >] # ## if in != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [| [> [< [% < <- let* x ( match >. .< .~ method - -. -> mutable new nonrec object of ?label: or % + +. += !+ private ? ' {%%ext|s|} } rec ) ; sig * "s" struct then ~ to true try X _ virtual when while with
```

### Sample 3

Sentence:
```
X [@@ and 'a'
```
Stack:
```
implementation: seq_expr LBRACKETATAT attr_id CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] : :> , (*comment*) constraint do (**documentation *) done .. downto effect else end for fun function functor >} >] if in inherit initializer lazy [> [< <- let* match >. method -> mutable nonrec of private ' } rec ) sig struct then to try _ virtual when while with
```

### Sample 4

Sentence:
```
X [@@ and :
```
Stack:
```
implementation: seq_expr LBRACKETATAT attr_id COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = false 1.0 for fun function functor > >} >] ## if in != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [| <- let let* match >. .< .~ method - -. -> mutable new nonrec object of or % + +. += !+ private } rec ) ; sig * "s" struct then ~ to true try virtual when while with
```

## Pattern 294

```
| [_* /local_structure_item: TYPE ext list_attribute_ type_parameters type_longident . PLUSEQ private_flag reversed_bar_llist_extension_constructor_ list_post_item_attribute_]
```

### Sample 1

Sentence:
```
type X . x
```
Stack:
```
use_file: TYPE ext list_attribute_ type_parameters mod_ext_longident DOT LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 295

```
| [_* /local_structure_item: TYPE ext list_attribute_ type_parameters type_longident PLUSEQ private_flag . reversed_bar_llist_extension_constructor_ list_post_item_attribute_]
```

### Sample 1

Sentence:
```
type x += private
```
Stack:
```
use_file: TYPE ext list_attribute_ type_parameters type_longident PLUSEQ PRIVATE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```

## Pattern 296

```
| [_* /extension: LBRACKETPERCENT attr_id payload . RBRACKET]
```

### Sample 1

Sentence:
```
[% and : {%%ext|s|}
```
Stack:
```
use_file: LBRACKETPERCENT attr_id COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 2

Sentence:
```
[% and {%%%%ext|s|}
```
Stack:
```
use_file: LBRACKETPERCENT attr_id QUOTED_STRING_ITEM
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = false 1.0 for fun function functor > >} >] # ## if in != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [| [> [< [% < <- let* x ( match >. .< .~ method - -. -> mutable new nonrec object of ?label: or % + +. += !+ private ? ' {%%ext|s|} } rec ) ; sig * "s" struct then ~ to true try X _ virtual when while with
```

### Sample 3

Sentence:
```
[% and 'a'
```
Stack:
```
use_file: LBRACKETPERCENT attr_id CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] : :> , (*comment*) constraint do (**documentation *) done .. downto effect else end for fun function functor >} >] if in inherit initializer lazy [> [< <- let* match >. method -> mutable nonrec of private ' } rec ) sig struct then to try _ virtual when while with
```

### Sample 4

Sentence:
```
[% and :
```
Stack:
```
use_file: LBRACKETPERCENT attr_id COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = false 1.0 for fun function functor > >} >] ## if in != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [| <- let let* match >. .< .~ method - -. -> mutable new nonrec object of or % + +. += !+ private } rec ) ; sig * "s" struct then ~ to true try virtual when while with
```

## Pattern 297

```
| [_* /object_type: LESS meth_list . GREATER]
```

### Sample 1

Sentence:
```
< ..
```
Stack:
```
parse_core_type: LESS DOTDOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 298

```
| [_* /function_type: LIDENT COLON LPAREN reversed_nonempty_llist_typevar_ . DOT core_type RPAREN MINUSGREATER function_type]
```

### Sample 1

Sentence:
```
x : ( ' x ' x
```
Stack:
```
parse_core_type: LIDENT COLON LPAREN reversed_nonempty_llist_typevar_ QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
x : ( ' x
```
Stack:
```
parse_core_type: LIDENT COLON LPAREN QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 299

```
| [_* /function_type: LIDENT COLON LPAREN reversed_nonempty_llist_typevar_ DOT core_type . RPAREN MINUSGREATER function_type]
```

### Sample 1

Sentence:
```
x : ( ' x . {%%ext|s|} [@ and ]
```
Stack:
```
parse_core_type: LIDENT COLON LPAREN reversed_nonempty_llist_typevar_ DOT core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
x : ( ' x . {%%ext|s|} as ' x
```
Stack:
```
parse_core_type: LIDENT COLON LPAREN reversed_nonempty_llist_typevar_ DOT alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
x : ( ' x . {%%ext|s|}
```
Stack:
```
parse_core_type: LIDENT COLON LPAREN reversed_nonempty_llist_typevar_ DOT QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
x : ( ' x . x
```
Stack:
```
parse_core_type: LIDENT COLON LPAREN reversed_nonempty_llist_typevar_ DOT LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 300

```
| [_* /function_type: LIDENT COLON tuple_type . MINUSGREATER function_type]
```

### Sample 1

Sentence:
```
x : x
```
Stack:
```
parse_core_type: LIDENT COLON LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 301

```
| [_* /function_type: LPAREN reversed_nonempty_llist_typevar_ . DOT core_type RPAREN MINUSGREATER function_type]
```

### Sample 1

Sentence:
```
( ' x ' x
```
Stack:
```
parse_core_type: LPAREN reversed_nonempty_llist_typevar_ QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
( ' x
```
Stack:
```
parse_core_type: LPAREN QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 302

```
| [_* /function_type: LPAREN reversed_nonempty_llist_typevar_ DOT core_type . RPAREN MINUSGREATER function_type]
```

### Sample 1

Sentence:
```
( ' x . {%%ext|s|} [@ and ]
```
Stack:
```
parse_core_type: LPAREN reversed_nonempty_llist_typevar_ DOT core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
( ' x . {%%ext|s|} as ' x
```
Stack:
```
parse_core_type: LPAREN reversed_nonempty_llist_typevar_ DOT alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
( ' x . {%%ext|s|}
```
Stack:
```
parse_core_type: LPAREN reversed_nonempty_llist_typevar_ DOT QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
( ' x . x
```
Stack:
```
parse_core_type: LPAREN reversed_nonempty_llist_typevar_ DOT LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 303

```
| [_* /nonempty_type_kind: PRIVATE LBRACE label_declarations . RBRACE]
```

### Sample 1

Sentence:
```
type x := private { x : {%%ext|s|} as ' x
```
Stack:
```
interface: TYPE ext list_attribute_ type_parameters LIDENT COLONEQUAL PRIVATE LBRACE mutable_flag LIDENT COLON alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
type x := private { x : {%%ext|s|} ;
```
Stack:
```
interface: TYPE ext list_attribute_ type_parameters LIDENT COLONEQUAL PRIVATE LBRACE mutable_flag LIDENT COLON possibly_poly_core_type_no_attr_ list_attribute_ SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
type x := private { x : {%%ext|s|}
```
Stack:
```
interface: TYPE ext list_attribute_ type_parameters LIDENT COLONEQUAL PRIVATE LBRACE mutable_flag LIDENT COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 304

```
| [_* /generic_type_declaration_no_nonrec_flag_type_subst_kind_: TYPE ext list_attribute_ type_parameters . LIDENT COLONEQUAL nonempty_type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_
      /generic_type_declaration_nonrec_flag_type_kind_: TYPE ext list_attribute_ type_parameters . LIDENT type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_
      /signature_item: TYPE ext list_attribute_ type_parameters . type_longident PLUSEQ private_flag reversed_bar_llist_extension_constructor_declaration_ list_post_item_attribute_]
```

### Sample 1

Sentence:
```
type _
```
Stack:
```
interface: TYPE ext list_attribute_ type_variance UNDERSCORE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 2

Sentence:
```
type % and
```
Stack:
```
interface: TYPE PERCENT AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method -. -> module mutable new object of open ?label: or % +. += private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

## Pattern 305

```
| [_* /signature_item: TYPE ext list_attribute_ type_parameters type_longident . PLUSEQ private_flag reversed_bar_llist_extension_constructor_declaration_ list_post_item_attribute_]
```

### Sample 1

Sentence:
```
type X . x
```
Stack:
```
interface: TYPE ext list_attribute_ type_parameters mod_ext_longident DOT LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 306

```
| [_* /signature_item: TYPE ext list_attribute_ type_parameters type_longident PLUSEQ private_flag . reversed_bar_llist_extension_constructor_declaration_ list_post_item_attribute_]
```

### Sample 1

Sentence:
```
type x += private
```
Stack:
```
interface: TYPE ext list_attribute_ type_parameters type_longident PLUSEQ PRIVATE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```

## Pattern 307

```
| [_* /module_type: SIG list_attribute_ signature . END]
```

### Sample 1

Sentence:
```
sig ;;
```
Stack:
```
parse_module_type: SIG list_attribute_ SEMISEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else = false 1.0 for fun function functor > >} >] # ## if in != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [| [> [< [% < <- let let* x ( match >. .< .~ method - -. -> mutable new nonrec object of ?label: or % + +. += !+ private ? ' {%%ext|s|} } ] rec ) ; sig * "s" struct then ~ to true try X _ virtual when while with
```

## Pattern 308

```
| [_* /delimited_type_supporting_local_open: LPAREN MODULE ext list_attribute_ module_type . RPAREN]
```

### Sample 1

Sentence:
```
( module x
```
Stack:
```
parse_core_type: LPAREN MODULE ext list_attribute_ LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

### Sample 2

Sentence:
```
( module X
```
Stack:
```
parse_core_type: LPAREN MODULE ext list_attribute_ UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

## Pattern 309

```
| [_* /attribute: LBRACKETAT attr_id attr_payload . RBRACKET]
```

### Sample 1

Sentence:
```
{%%ext|s|} [@ and : {%%ext|s|}
```
Stack:
```
parse_core_type: core_type LBRACKETAT attr_id COLON QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 2

Sentence:
```
{%%ext|s|} [@ and {%%%%ext|s|}
```
Stack:
```
parse_core_type: core_type LBRACKETAT attr_id QUOTED_STRING_ITEM
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = false 1.0 for fun function functor > >} >] # ## if in != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [| [> [< [% < <- let* x ( match >. .< .~ method - -. -> mutable new nonrec object of ?label: or % + +. += !+ private ? ' {%%ext|s|} } rec ) ; sig * "s" struct then ~ to true try X _ virtual when while with
```

### Sample 3

Sentence:
```
{%%ext|s|} [@ and 'a'
```
Stack:
```
parse_core_type: core_type LBRACKETAT attr_id CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] : :> , (*comment*) constraint do (**documentation *) done .. downto effect else end for fun function functor >} >] if in inherit initializer lazy [> [< <- let* match >. method -> mutable nonrec of private ' } rec ) sig struct then to try _ virtual when while with
```

### Sample 4

Sentence:
```
{%%ext|s|} [@ and :
```
Stack:
```
parse_core_type: core_type LBRACKETAT attr_id COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = false 1.0 for fun function functor > >} >] ## if in != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [| <- let let* match >. .< .~ method - -. -> mutable new nonrec object of or % + +. += !+ private } rec ) ; sig * "s" struct then ~ to true try virtual when while with
```

## Pattern 310

```
| [_* /fun_expr: WHILE ext list_attribute_ . seq_expr DO seq_expr DONE]
```

### Sample 1

Sentence:
```
while [@ and ]
```
Stack:
```
use_file: WHILE ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

### Sample 2

Sentence:
```
while % and
```
Stack:
```
use_file: WHILE PERCENT AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

## Pattern 311

```
| [_* /fun_expr: WHILE ext list_attribute_ seq_expr . DO seq_expr DONE]
```

### Sample 1

Sentence:
```
while function false -> .
```
Stack:
```
use_file: WHILE ext list_attribute_ FUNCTION ext list_attribute_ pattern MINUSGREATER DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
while X ;
```
Stack:
```
use_file: WHILE ext list_attribute_ fun_expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

### Sample 3

Sentence:
```
while X 'a'
```
Stack:
```
use_file: WHILE ext list_attribute_ constr_longident CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert ` ! | |] begin 'a' class : :> , (*comment*) constraint (**documentation *) done .. downto effect else end exception external false 1.0 for fun function functor >} >] if in include inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% <- let let* x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ;; sig "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 4

Sentence:
```
while 'a'
```
Stack:
```
use_file: WHILE ext list_attribute_ CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] class : :> , (*comment*) constraint (**documentation *) done .. downto effect else end exception external for fun function functor >} >] if in include inherit initializer lazy [@@ [@@@ [> [< [%% <- let let* match >. method -> module mutable nonrec of open private ' {%%%%ext|s|} } ] rec ) ;; sig struct then to try type _ val virtual when while with
```

## Pattern 312

```
| [_* /fun_expr: WHILE ext list_attribute_ seq_expr DO seq_expr . DONE]
```

### Sample 1

Sentence:
```
while X do function false -> .
```
Stack:
```
use_file: WHILE ext list_attribute_ seq_expr DO FUNCTION ext list_attribute_ pattern MINUSGREATER DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
while X do X ;
```
Stack:
```
use_file: WHILE ext list_attribute_ seq_expr DO fun_expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

### Sample 3

Sentence:
```
while X do X 'a'
```
Stack:
```
use_file: WHILE ext list_attribute_ seq_expr DO constr_longident CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert ` ! | |] begin 'a' class : :> , (*comment*) constraint do (**documentation *) .. downto effect else end exception external false 1.0 for fun function functor >} >] if in include inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% <- let let* x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ;; sig "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 4

Sentence:
```
while X do 'a'
```
Stack:
```
use_file: WHILE ext list_attribute_ seq_expr DO CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] class : :> , (*comment*) constraint do (**documentation *) .. downto effect else end exception external for fun function functor >} >] if in include inherit initializer lazy [@@ [@@@ [> [< [%% <- let let* match >. method -> module mutable nonrec of open private ' {%%%%ext|s|} } ] rec ) ;; sig struct then to try type _ val virtual when while with
```

## Pattern 313

```
| [_* /implementation: structure . EOF]
```

### Sample 1

Sentence:
```
;;
```
Stack:
```
implementation: SEMISEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end
 = functor > >} >] # ## in != ^ +! land ** inherit initializer [@ [@@ [> [< < <- >. method -> mutable nonrec of ?label: or % += private ? ' } ] rec ) ; sig * struct then to _ virtual when with
```

### Sample 2

Sentence:
```
{%%%%ext|s|}
```
Stack:
```
implementation: QUOTED_STRING_ITEM
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end
 = false 1.0 for fun function functor > >} >] # ## if in != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [| [> [< [% < <- let* x ( match >. .< .~ method - -. -> mutable new nonrec object of ?label: or % + +. += !+ private ? ' {%%ext|s|} } ] rec ) ; sig * "s" struct then ~ to true try X _ virtual when while with
```

### Sample 3

Sentence:
```
'a'
```
Stack:
```
implementation: CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] : :> , (*comment*) constraint do (**documentation *) done .. downto effect else end
 for fun function functor >} >] if in inherit initializer lazy [> [< <- let* match >. method -> mutable nonrec of private ' } ] rec ) sig struct then to try _ virtual when while with
```

## Pattern 314

```
| [_* /interface: signature . EOF]
```

### Sample 1

Sentence:
```
;;
```
Stack:
```
interface: SEMISEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end
 = false 1.0 for fun function functor > >} >] # ## if in != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [| [> [< [% < <- let let* x ( match >. .< .~ method - -. -> mutable new nonrec object of ?label: or % + +. += !+ private ? ' {%%ext|s|} } ] rec ) ; sig * "s" struct then ~ to true try X _ virtual when while with
```

## Pattern 315

```
| [_* /mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident . DOT UIDENT
      /mk_longident_mod_ext_longident___anonymous_42_: mod_ext_longident . DOT _*]
```

### Sample 1

Sentence:
```
X ( X )
```
Stack:
```
parse_any_longident: mod_ext_longident LPAREN mod_ext_longident RPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
X
```
Stack:
```
parse_any_longident: UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 316

```
| [_* /parse_any_longident: any_longident . EOF]
```

### Sample 1

Sentence:
```
false
```
Stack:
```
parse_any_longident: FALSE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 317

```
| [_* /parse_constr_longident: constr_longident . EOF]
```

### Sample 1

Sentence:
```
false
```
Stack:
```
parse_constr_longident: FALSE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 318

```
| [_* /parse_core_type: core_type . EOF]
```

### Sample 1

Sentence:
```
{%%ext|s|} [@ and ]
```
Stack:
```
parse_core_type: core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
{%%ext|s|} as ' x
```
Stack:
```
parse_core_type: alias_type AS QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 3

Sentence:
```
{%%ext|s|}
```
Stack:
```
parse_core_type: QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

### Sample 4

Sentence:
```
x
```
Stack:
```
parse_core_type: LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type _ val virtual when while with
```

## Pattern 319

```
| [_* /parse_expression: seq_expr . EOF]
```

### Sample 1

Sentence:
```
function false -> .
```
Stack:
```
parse_expression: FUNCTION ext list_attribute_ pattern MINUSGREATER DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
X ;
```
Stack:
```
parse_expression: fun_expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

### Sample 3

Sentence:
```
X 'a'
```
Stack:
```
parse_expression: constr_longident CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert ` ! | |] begin 'a' class : :> , (*comment*) constraint do (**documentation *) done .. downto effect else end
 exception external false 1.0 for fun function functor >} >] if in include inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% <- let let* x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ;; sig "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 4

Sentence:
```
'a'
```
Stack:
```
parse_expression: CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] class : :> , (*comment*) constraint do (**documentation *) done .. downto effect else end
 exception external for fun function functor >} >] if in include inherit initializer lazy [@@ [@@@ [> [< [%% <- let let* match >. method -> module mutable nonrec of open private ' {%%%%ext|s|} } ] rec ) ;; sig struct then to try type _ val virtual when while with
```

## Pattern 320

```
| [_* /mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident . DOT UIDENT
      /parse_mod_ext_longident: mod_ext_longident . EOF]
```

### Sample 1

Sentence:
```
X
```
Stack:
```
parse_mod_ext_longident: UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 321

```
| [_* /mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT
      /parse_mod_longident: mod_longident . EOF]
```

### Sample 1

Sentence:
```
X
```
Stack:
```
parse_mod_longident: UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 322

```
| [_* /parse_module_expr: module_expr . EOF]
```

### Sample 1

Sentence:
```
{%%ext|s|}
```
Stack:
```
parse_module_expr: QUOTED_STRING_EXPR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
X
```
Stack:
```
parse_module_expr: UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 323

```
| [_* /parse_module_type: module_type . EOF]
```

### Sample 1

Sentence:
```
x
```
Stack:
```
parse_module_type: LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

### Sample 2

Sentence:
```
X
```
Stack:
```
parse_module_type: UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

## Pattern 324

```
| [_* /parse_mty_longident: mty_longident . EOF]
```

### Sample 1

Sentence:
```
x
```
Stack:
```
parse_mty_longident: LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
X
```
Stack:
```
parse_mty_longident: UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 325

```
| [_* /labeled_tuple_pat_element_list_pattern_: pattern . COMMA _*
      /parse_pattern: pattern . EOF
      /reversed_labeled_tuple_pattern_pattern_: pattern . COMMA DOTDOT]
```

### Sample 1

Sentence:
```
false
```
Stack:
```
parse_pattern: FALSE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ! || |] begin class : := :> (*comment*) constraint do (**documentation *) done . .. .+ downto else end
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

### Sample 2

Sentence:
```
'a'
```
Stack:
```
parse_pattern: CHAR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! || |] begin 'a' class : := :> (*comment*) constraint do (**documentation *) done . .+ downto effect else end
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 326

```
| [_* /parse_val_longident: val_longident . EOF]
```

### Sample 1

Sentence:
```
x
```
Stack:
```
parse_val_longident: LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 327

```
| [_* /mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT
      /mk_longident_mod_longident_val_ident_: mod_longident . DOT val_ident]
```

### Sample 1

Sentence:
```
X
```
Stack:
```
parse_val_longident: UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Pattern 328

```
| [_* /toplevel_phrase: toplevel_directive . SEMISEMI]
```

### Sample 1

Sentence:
```
# x false
```
Stack:
```
toplevel_phrase: HASH ident FALSE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
# x
```
Stack:
```
toplevel_phrase: HASH LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; sig * struct then ~ to try type _ val virtual when while with
```

## Pattern 329

```
| [_* /toplevel_phrase: seq_expr list_post_item_attribute_ . SEMISEMI]
```

### Sample 1

Sentence:
```
X [@@ and ]
```
Stack:
```
toplevel_phrase: seq_expr LBRACKETATAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 2

Sentence:
```
X ;
```
Stack:
```
toplevel_phrase: fun_expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or += private ? ' {%%%%ext|s|} } ] rec ) ; sig * struct then to type _ val virtual when with
```

### Sample 3

Sentence:
```
X 'a'
```
Stack:
```
toplevel_phrase: constr_longident CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert ` ! | |] begin 'a' class : :> , (*comment*) constraint do (**documentation *) done .. downto effect else end exception external false 1.0 for fun function functor >} >] if in include inherit initializer 1 ~label: lazy { {< [ [@@@ [| [> [< [% [%% <- let let* x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) sig "s" struct then ~ to true try type X _ val virtual when while with
```

### Sample 4

Sentence:
```
'a'
```
Stack:
```
toplevel_phrase: CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] class : :> , (*comment*) constraint do (**documentation *) done .. downto effect else end exception external for fun function functor >} >] if in include inherit initializer lazy [@@@ [> [< [%% <- let let* match >. method -> module mutable nonrec of open private ' {%%%%ext|s|} } ] rec ) sig struct then to try type _ val virtual when while with
```

## Pattern 330

```
| [_* /toplevel_phrase: list_text_str_structure_item__ . SEMISEMI]
```

### Sample 1

Sentence:
```
[@@@ and ]
```
Stack:
```
toplevel_phrase: LBRACKETATATAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = false 1.0 for fun function functor > >} >] # ## if in != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [| [> [< [% < <- let* x ( match >. .< .~ method - -. -> mutable new nonrec object of ?label: or % + +. += !+ private ? ' {%%ext|s|} } ] rec ) ; sig * "s" struct then ~ to true try X _ virtual when while with
```

### Sample 2

Sentence:
```
{%%%%ext|s|}
```
Stack:
```
toplevel_phrase: QUOTED_STRING_ITEM
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = false 1.0 for fun function functor > >} >] # ## if in != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [| [> [< [% < <- let* x ( match >. .< .~ method - -. -> mutable new nonrec object of ?label: or % + +. += !+ private ? ' {%%ext|s|} } ] rec ) ; sig * "s" struct then ~ to true try X _ virtual when while with
```

## Pattern 331

```
| [_* /use_file: seq_expr list_post_item_attribute_ list_use_file_element_ . EOF]
```

### Sample 1

Sentence:
```
X {%%%%ext|s|}
```
Stack:
```
use_file: seq_expr list_post_item_attribute_ QUOTED_STRING_ITEM
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end
 = false 1.0 for fun function functor > >} >] ## if in != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [| [> [< [% < <- let* x ( match >. .< .~ method - -. -> mutable new nonrec object of ?label: or % + +. += !+ private ? ' {%%ext|s|} } ] rec ) ; sig * "s" struct then ~ to true try X _ virtual when while with
```

### Sample 2

Sentence:
```
'a'
```
Stack:
```
use_file: CHAR
```
Rejected when looking ahead at any of the terminals in:
```
and and* as assert | |] : :> , (*comment*) constraint do (**documentation *) done .. downto effect else end
 for fun function functor >} >] if in inherit initializer lazy [> [< <- let* match >. method -> mutable nonrec of private ' } ] rec ) sig struct then to try _ virtual when while with
```

### Sample 3

Sentence:
```
X ;;
```
Stack:
```
use_file: seq_expr list_post_item_attribute_ SEMISEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end
 = functor > >} >] ## in != ^ +! land ** inherit initializer [@ [@@ [> [< < <- >. method -> mutable nonrec of ?label: or % += private ? ' } ] rec ) ; sig * struct then to _ virtual when with
```

## Pattern 332

```
| [_* /use_file: list_use_file_element_ . EOF]
```

### Sample 1

Sentence:
```
{%%%%ext|s|}
```
Stack:
```
use_file: QUOTED_STRING_ITEM
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end
 = false 1.0 for fun function functor > >} >] ## if in != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [| [> [< [% < <- let* x ( match >. .< .~ method - -. -> mutable new nonrec object of ?label: or % + +. += !+ private ? ' {%%ext|s|} } ] rec ) ; sig * "s" struct then ~ to true try X _ virtual when while with
```

### Sample 2

Sentence:
```
;;
```
Stack:
```
use_file: SEMISEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end
 = functor > >} >] ## in != ^ +! land ** inherit initializer [@ [@@ [> [< < <- >. method -> mutable nonrec of ?label: or % += private ? ' } ] rec ) ; sig * struct then to _ virtual when with
```
