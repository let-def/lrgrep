Common set of failing lookaheads:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

# Pattern 1

```
ext: PERCENT . attr_id
```

## Sample 1

Sentence:
```
class %
```
Stack:
```
interface: CLASS PERCENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and* ` ! | || |] 'a' : :: := :> , (*comment*) (**documentation *) . .. .+  = 1.0 > >} >] # ## != ^ +! land ** 1 ~label: { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let* ( >. .< .~ - -. -> ?label: % + +. += !+ ? ' {%%ext|s|} {%%%%ext|s|} } ] ) ; ;; * "s" ~ _
```

# Pattern 2

```
attr_id: single_attr_id DOT . attr_id
```

## Sample 1

Sentence:
```
[@@@ and .
```
Stack:
```
use_file: LBRACKETATATAT single_attr_id DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and* ` ! | || |] 'a' : :: := :> , (*comment*) (**documentation *) . .. .+  = 1.0 > >} >] # ## != ^ +! land ** 1 ~label: { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let* ( >. .< .~ - -. -> ?label: % + +. += !+ ? ' {%%ext|s|} {%%%%ext|s|} } ] ) ; ;; * "s" ~ _
```

# Pattern 3

```
attribute: LBRACKETAT . attr_id attr_payload RBRACKET
```

## Sample 1

Sentence:
```
{%%ext|s|} [@
```
Stack:
```
parse_core_type: core_type LBRACKETAT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and* ` ! | || |] 'a' : :: := :> , (*comment*) (**documentation *) . .. .+  = 1.0 > >} >] # ## != ^ +! land ** 1 ~label: { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let* ( >. .< .~ - -. -> ?label: % + +. += !+ ? ' {%%ext|s|} {%%%%ext|s|} } ] ) ; ;; * "s" ~ _
```

# Pattern 4

```
value_description: VAL ext list_attribute_ . val_ident COLON possibly_poly_core_type_ list_post_item_attribute_
```

## Sample 1

Sentence:
```
val
```
Stack:
```
use_file: VAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 5

```
val_extra_ident: LPAREN . operator RPAREN
```

## Sample 1

Sentence:
```
(
```
Stack:
```
parse_val_longident: LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
and as assert ` | |] begin 'a' class : :: :> , (*comment*) constraint do (**documentation *) done . .. downto effect else end  exception external false 1.0 for fun function functor >} >] # if in include inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% <- let x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 6

```
operator: DOTOP . LPAREN _*
operator: DOTOP . LBRACKET _*
operator: DOTOP . LBRACE _*
```

## Sample 1

Sentence:
```
( .+
```
Stack:
```
parse_any_longident: LPAREN DOTOP
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 7

```
index_mod: SEMI . DOTDOT
```

## Sample 1

Sentence:
```
( .+ { ;
```
Stack:
```
parse_any_longident: LPAREN DOTOP LBRACE SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 8

```
operator: DOTOP LPAREN index_mod . RPAREN _*
```

## Sample 1

Sentence:
```
( .+ (
```
Stack:
```
parse_any_longident: LPAREN DOTOP LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 9

```
operator: DOTOP LBRACKET index_mod . RBRACKET _*
```

## Sample 1

Sentence:
```
( .+ [
```
Stack:
```
parse_any_longident: LPAREN DOTOP LBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 10

```
operator: DOTOP LBRACE index_mod . RBRACE _*
```

## Sample 1

Sentence:
```
( .+ {
```
Stack:
```
parse_any_longident: LPAREN DOTOP LBRACE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 11

```
val_extra_ident: LPAREN operator . RPAREN
```

## Sample 1

Sentence:
```
X . ( !
```
Stack:
```
use_file: mod_longident DOT LPAREN BANG
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 12

```
value_description: VAL ext list_attribute_ val_ident . COLON possibly_poly_core_type_ list_post_item_attribute_
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 13

```
value_description: VAL ext list_attribute_ val_ident COLON . possibly_poly_core_type_ list_post_item_attribute_
```

## Sample 1

Sentence:
```
val x :
```
Stack:
```
use_file: VAL ext list_attribute_ val_ident COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 14

```
atomic_type: QUOTE . ident
reversed_nonempty_llist_typevar_: QUOTE . ident
```

## Sample 1

Sentence:
```
( '
```
Stack:
```
parse_core_type: LPAREN QUOTE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 15

```
optlabel: QUESTION . LIDENT COLON
```

## Sample 1

Sentence:
```
?
```
Stack:
```
parse_core_type: QUESTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 16

```
optlabel: QUESTION LIDENT . COLON
```

## Sample 1

Sentence:
```
? x
```
Stack:
```
parse_core_type: QUESTION LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 17

```
atomic_type: LPAREN . reversed_separated_nontrivial_llist_COMMA_core_type_ _*
delimited_type_supporting_local_open: LPAREN . core_type RPAREN
delimited_type_supporting_local_open: LPAREN . MODULE ext list_attribute_ module_type RPAREN
function_type: LPAREN . reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER function_type
```

## Sample 1

Sentence:
```
(
```
Stack:
```
parse_core_type: LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 18

```
delimited_type_supporting_local_open: LPAREN MODULE ext list_attribute_ . module_type RPAREN
```

## Sample 1

Sentence:
```
( module
```
Stack:
```
parse_core_type: LPAREN MODULE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 19

```
type_parameter: type_variance . type_variable
```

## Sample 1

Sentence:
```
type
```
Stack:
```
interface: TYPE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method -. -> module mutable new object of open ?label: or +. += private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

## Sample 2

Sentence:
```
type nonrec
```
Stack:
```
interface: TYPE ext list_attribute_ NONREC
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X val virtual when while with
```

## Sample 3

Sentence:
```
type x := false and (
```
Stack:
```
interface: generic_type_declaration_no_nonrec_flag_type_subst_kind_ AND list_attribute_ LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X val virtual when while with
```

## Sample 4

Sentence:
```
type x := false and ! -
```
Stack:
```
interface: generic_type_declaration_no_nonrec_flag_type_subst_kind_ AND list_attribute_ BANG MINUS
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X val virtual when while with
```

# Pattern 20

```
type_variable: QUOTE . ident
```

## Sample 1

Sentence:
```
type x := false and '
```
Stack:
```
interface: generic_type_declaration_no_nonrec_flag_type_subst_kind_ AND list_attribute_ type_variance QUOTE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 21

```
type_parameters: LPAREN reversed_separated_nonempty_llist_COMMA_type_parameter_ . RPAREN
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 22

```
generic_type_declaration_nonrec_flag_type_kind_: TYPE ext list_attribute_ NONREC type_parameters . LIDENT type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_
```

## Sample 1

Sentence:
```
type nonrec
```
Stack:
```
interface: TYPE ext list_attribute_ NONREC
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X val virtual when while with
```

## Sample 2

Sentence:
```
type nonrec ( _ )
```
Stack:
```
interface: TYPE ext list_attribute_ NONREC LPAREN reversed_separated_nonempty_llist_COMMA_type_parameter_ RPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 23

```
type_kind: EQUAL . nonempty_type_kind
```

## Sample 1

Sentence:
```
type x and x =
```
Stack:
```
interface: generic_type_declaration_nonrec_flag_type_kind_ AND list_attribute_ type_parameters LIDENT EQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .+ downto effect else end  = exception 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type val virtual when while with
```

# Pattern 24

```
atomic_type: QUOTE . ident
```

## Sample 1

Sentence:
```
'
```
Stack:
```
parse_core_type: QUOTE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 25

```
nonempty_type_kind: PRIVATE . core_type
nonempty_type_kind: PRIVATE . constructor_declarations
nonempty_type_kind: PRIVATE . DOTDOT
nonempty_type_kind: PRIVATE . LBRACE label_declarations RBRACE
```

## Sample 1

Sentence:
```
type x := private
```
Stack:
```
interface: TYPE ext list_attribute_ type_parameters LIDENT COLONEQUAL PRIVATE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .+ downto effect else end  = exception external 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type val virtual when while with
```

# Pattern 26

```
atomic_type: LPAREN . reversed_separated_nontrivial_llist_COMMA_core_type_ _*
constr_extra_nonprefix_ident: LPAREN . RPAREN
constr_ident: LPAREN . COLONCOLON RPAREN
delimited_type_supporting_local_open: LPAREN . core_type RPAREN
delimited_type_supporting_local_open: LPAREN . MODULE ext list_attribute_ module_type RPAREN
function_type: LPAREN . reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER function_type
```

## Sample 1

Sentence:
```
type x := (
```
Stack:
```
interface: TYPE ext list_attribute_ type_parameters LIDENT COLONEQUAL LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 27

```
function_type: LIDENT COLON . LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER function_type
function_type: LIDENT COLON . tuple_type MINUSGREATER function_type
function_type: LIDENT COLON . atomic_type _*
```

## Sample 1

Sentence:
```
x :
```
Stack:
```
parse_core_type: LIDENT COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 28

```
atomic_type: LPAREN . reversed_separated_nontrivial_llist_COMMA_core_type_ _*
delimited_type_supporting_local_open: LPAREN . core_type RPAREN
delimited_type_supporting_local_open: LPAREN . MODULE ext list_attribute_ module_type RPAREN
function_type: LIDENT COLON LPAREN . reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER function_type
```

## Sample 1

Sentence:
```
x : (
```
Stack:
```
parse_core_type: LIDENT COLON LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 29

```
object_type: LESS . meth_list GREATER
object_type: LESS . GREATER
```

## Sample 1

Sentence:
```
<
```
Stack:
```
parse_core_type: LESS
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .+ downto effect else end  = exception external false 1.0 for fun function functor >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 30

```
atomic_type: LPAREN . reversed_separated_nontrivial_llist_COMMA_core_type_ _*
delimited_type_supporting_local_open: LPAREN . core_type RPAREN
delimited_type_supporting_local_open: LPAREN . MODULE ext list_attribute_ module_type RPAREN
```

## Sample 1

Sentence:
```
< (
```
Stack:
```
parse_core_type: LESS LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 31

```
extension: LBRACKETPERCENT . attr_id payload RBRACKET
```

## Sample 1

Sentence:
```
[%
```
Stack:
```
use_file: LBRACKETPERCENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and* ` ! | || |] 'a' : :: := :> , (*comment*) (**documentation *) . .. .+  = 1.0 > >} >] # ## != ^ +! land ** 1 ~label: { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let* ( >. .< .~ - -. -> ?label: % + +. += !+ ? ' {%%ext|s|} {%%%%ext|s|} } ] ) ; ;; * "s" ~ _
```

# Pattern 32

```
generic_type_declaration_nonrec_flag_type_kind_: TYPE ext list_attribute_ type_parameters . LIDENT type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_
local_structure_item: TYPE ext list_attribute_ type_parameters . type_longident PLUSEQ private_flag reversed_bar_llist_extension_constructor_ list_post_item_attribute_
```

## Sample 1

Sentence:
```
type
```
Stack:
```
use_file: TYPE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method -. -> module mutable new object of open ?label: or +. += private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

## Sample 2

Sentence:
```
type ( _ )
```
Stack:
```
use_file: TYPE ext list_attribute_ LPAREN reversed_separated_nonempty_llist_COMMA_type_parameter_ RPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 33

```
post_item_attribute: LBRACKETATAT . attr_id attr_payload RBRACKET
```

## Sample 1

Sentence:
```
X [@@
```
Stack:
```
implementation: seq_expr LBRACKETATAT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and* ` ! | || |] 'a' : :: := :> , (*comment*) (**documentation *) . .. .+  = 1.0 > >} >] # ## != ^ +! land ** 1 ~label: { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let* ( >. .< .~ - -. -> ?label: % + +. += !+ ? ' {%%ext|s|} {%%%%ext|s|} } ] ) ; ;; * "s" ~ _
```

# Pattern 34

```
fun_expr: TRY ext list_attribute_ . seq_expr WITH reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
```

## Sample 1

Sentence:
```
try
```
Stack:
```
use_file: TRY
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

## Sample 2

Sentence:
```
try [@ and ]
```
Stack:
```
use_file: TRY ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 35

```
reversed_labeled_tuple_body: TILDE . LIDENT _*
reversed_labeled_tuple_body: TILDE . LPAREN _*
```

## Sample 1

Sentence:
```
~
```
Stack:
```
use_file: TILDE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 36

```
reversed_labeled_tuple_body: TILDE LPAREN . LIDENT _*
```

## Sample 1

Sentence:
```
~ (
```
Stack:
```
use_file: TILDE LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 37

```
reversed_labeled_tuple_body: TILDE LPAREN LIDENT . type_constraint _*
```

## Sample 1

Sentence:
```
~ ( x
```
Stack:
```
use_file: TILDE LPAREN LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 38

```
type_constraint: COLONGREATER . core_type
```

## Sample 1

Sentence:
```
( X :>
```
Stack:
```
use_file: LPAREN seq_expr COLONGREATER
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 39

```
delimited_type_supporting_local_open: LBRACKETLESS option_BAR_ . reversed_separated_nonempty_llist_BAR_row_field_ _*
```

## Sample 1

Sentence:
```
[<
```
Stack:
```
parse_core_type: LBRACKETLESS
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

## Sample 2

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
&& & and and* as assert ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 40

```
delimited_type_supporting_local_open: LBRACKETGREATER option_BAR_ . reversed_separated_nonempty_llist_BAR_row_field_ RBRACKET
```

## Sample 1

Sentence:
```
[>
```
Stack:
```
parse_core_type: LBRACKETGREATER
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

## Sample 2

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
&& & and and* as assert ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 41

```
delimited_type_supporting_local_open: LBRACKET . tag_field RBRACKET
delimited_type_supporting_local_open: LBRACKET . BAR reversed_separated_nonempty_llist_BAR_row_field_ RBRACKET
delimited_type_supporting_local_open: LBRACKET . row_field BAR reversed_separated_nonempty_llist_BAR_row_field_ RBRACKET
```

## Sample 1

Sentence:
```
[
```
Stack:
```
parse_core_type: LBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 42

```
atomic_type: HASH . clty_longident
```

## Sample 1

Sentence:
```

```
Stack:
```
parse_core_type: HASH
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 43

```
mk_longident_mod_ext_longident_LIDENT_: mod_ext_longident . DOT LIDENT
mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident . DOT UIDENT
```

## Sample 1

Sentence:
```
type X ( X )
```
Stack:
```
interface: TYPE ext list_attribute_ type_parameters mod_ext_longident LPAREN mod_ext_longident RPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 44

```
mod_ext_longident: mod_ext_longident LPAREN . mod_ext_longident RPAREN
```

## Sample 1

Sentence:
```
X (
```
Stack:
```
parse_any_longident: mod_ext_longident LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 45

```
mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident . DOT UIDENT
mod_ext_longident: mod_ext_longident LPAREN mod_ext_longident . RPAREN
```

## Sample 1

Sentence:
```
X ( X ( X )
```
Stack:
```
parse_any_longident: mod_ext_longident LPAREN mod_ext_longident LPAREN mod_ext_longident RPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 46

```
mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident DOT . UIDENT
```

## Sample 1

Sentence:
```
X .
```
Stack:
```
parse_mod_ext_longident: mod_ext_longident DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 47

```
mk_longident_mod_ext_longident_LIDENT_: mod_ext_longident DOT . LIDENT
mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident DOT . UIDENT
```

## Sample 1

Sentence:
```
{%%ext|s|} X .
```
Stack:
```
parse_core_type: atomic_type mod_ext_longident DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 48

```
delimited_type_supporting_local_open: LBRACKET BAR . reversed_separated_nonempty_llist_BAR_row_field_ RBRACKET
```

## Sample 1

Sentence:
```
[ |
```
Stack:
```
parse_core_type: LBRACKET BAR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 49

```
name_tag: BACKQUOTE . ident
```

## Sample 1

Sentence:
```
`
```
Stack:
```
use_file: BACKQUOTE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 50

```
function_type: tuple_type MINUSGREATER . function_type
```

## Sample 1

Sentence:
```
{%%ext|s|} ->
```
Stack:
```
parse_core_type: tuple_type MINUSGREATER
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 51

```
function_type: optlabel . LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER function_type
function_type: optlabel . tuple_type MINUSGREATER function_type
```

## Sample 1

Sentence:
```
type x := ?label:
```
Stack:
```
interface: TYPE ext list_attribute_ type_parameters LIDENT COLONEQUAL OPTLABEL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 52

```
atomic_type: LPAREN . reversed_separated_nontrivial_llist_COMMA_core_type_ _*
delimited_type_supporting_local_open: LPAREN . core_type RPAREN
delimited_type_supporting_local_open: LPAREN . MODULE ext list_attribute_ module_type RPAREN
function_type: optlabel LPAREN . reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER function_type
```

## Sample 1

Sentence:
```
?label: (
```
Stack:
```
parse_core_type: optlabel LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 53

```
atomic_type: LPAREN reversed_separated_nontrivial_llist_COMMA_core_type_ . RPAREN _*
```

## Sample 1

Sentence:
```
?label: ( {%%ext|s|} , {%%ext|s|} [@ and ]
```
Stack:
```
parse_core_type: optlabel LPAREN core_type COMMA core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 54

```
atomic_type: LPAREN reversed_separated_nontrivial_llist_COMMA_core_type_ RPAREN . type_longident
atomic_type: LPAREN reversed_separated_nontrivial_llist_COMMA_core_type_ RPAREN . HASH clty_longident
```

## Sample 1

Sentence:
```
( {%%ext|s|} , {%%ext|s|} )
```
Stack:
```
parse_core_type: LPAREN reversed_separated_nontrivial_llist_COMMA_core_type_ RPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 55

```
atomic_type: LPAREN reversed_separated_nontrivial_llist_COMMA_core_type_ RPAREN HASH . clty_longident
```

## Sample 1

Sentence:
```
( {%%ext|s|} , {%%ext|s|} ) #
```
Stack:
```
parse_core_type: LPAREN reversed_separated_nontrivial_llist_COMMA_core_type_ RPAREN HASH
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 56

```
reversed_separated_nontrivial_llist_COMMA_core_type_: reversed_separated_nontrivial_llist_COMMA_core_type_ COMMA . core_type
```

## Sample 1

Sentence:
```
( {%%ext|s|} , {%%ext|s|} ,
```
Stack:
```
parse_core_type: LPAREN reversed_separated_nontrivial_llist_COMMA_core_type_ COMMA
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 57

```
atomic_type: mod_ext_longident . DOT delimited_type_supporting_local_open
mk_longident_mod_ext_longident_LIDENT_: mod_ext_longident . DOT LIDENT
mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident . DOT UIDENT
```

## Sample 1

Sentence:
```
type x := X ( X )
```
Stack:
```
interface: TYPE ext list_attribute_ type_parameters LIDENT COLONEQUAL mod_ext_longident LPAREN mod_ext_longident RPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 58

```
atomic_type: mod_ext_longident DOT . delimited_type_supporting_local_open
mk_longident_mod_ext_longident_LIDENT_: mod_ext_longident DOT . LIDENT
mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident DOT . UIDENT
```

## Sample 1

Sentence:
```
X .
```
Stack:
```
parse_core_type: mod_ext_longident DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 59

```
delimited_type_supporting_local_open: LPAREN . core_type RPAREN
delimited_type_supporting_local_open: LPAREN . MODULE ext list_attribute_ module_type RPAREN
```

## Sample 1

Sentence:
```
X . (
```
Stack:
```
parse_core_type: mod_ext_longident DOT LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 60

```
delimited_type_supporting_local_open: LPAREN core_type . RPAREN
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 61

```
tuple_type: atomic_type STAR . reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_
```

## Sample 1

Sentence:
```
{%%ext|s|} *
```
Stack:
```
parse_core_type: atomic_type STAR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 62

```
reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_: LIDENT COLON . atomic_type
```

## Sample 1

Sentence:
```
{%%ext|s|} * x :
```
Stack:
```
parse_core_type: atomic_type STAR LIDENT COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 63

```
atomic_type: atomic_type HASH . clty_longident
```

## Sample 1

Sentence:
```
{%%ext|s|} #
```
Stack:
```
parse_core_type: atomic_type HASH
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 64

```
reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_: reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ STAR . atomic_type
reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_: reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ STAR . LIDENT COLON atomic_type
```

## Sample 1

Sentence:
```
{%%ext|s|} * {%%ext|s|} *
```
Stack:
```
parse_core_type: atomic_type STAR reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ STAR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 65

```
reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_: reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ STAR LIDENT COLON . atomic_type
```

## Sample 1

Sentence:
```
{%%ext|s|} * {%%ext|s|} * x :
```
Stack:
```
parse_core_type: atomic_type STAR reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ STAR LIDENT COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 66

```
alias_type: alias_type AS . QUOTE ident
```

## Sample 1

Sentence:
```
{%%ext|s|} as
```
Stack:
```
parse_core_type: alias_type AS
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 67

```
alias_type: alias_type AS QUOTE . ident
```

## Sample 1

Sentence:
```
{%%ext|s|} as '
```
Stack:
```
parse_core_type: alias_type AS QUOTE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 68

```
function_type: optlabel LPAREN reversed_nonempty_llist_typevar_ . DOT core_type RPAREN MINUSGREATER function_type
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 69

```
reversed_nonempty_llist_typevar_: reversed_nonempty_llist_typevar_ QUOTE . ident
```

## Sample 1

Sentence:
```
( ' x '
```
Stack:
```
parse_core_type: LPAREN reversed_nonempty_llist_typevar_ QUOTE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 70

```
function_type: optlabel LPAREN reversed_nonempty_llist_typevar_ DOT . core_type RPAREN MINUSGREATER function_type
```

## Sample 1

Sentence:
```
?label: ( ' x .
```
Stack:
```
parse_core_type: optlabel LPAREN reversed_nonempty_llist_typevar_ DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 71

```
function_type: optlabel LPAREN reversed_nonempty_llist_typevar_ DOT core_type . RPAREN MINUSGREATER function_type
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 72

```
function_type: optlabel LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN . MINUSGREATER function_type
```

## Sample 1

Sentence:
```
?label: ( ' x . {%%ext|s|} )
```
Stack:
```
parse_core_type: optlabel LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 73

```
function_type: optlabel LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER . function_type
```

## Sample 1

Sentence:
```
?label: ( ' x . {%%ext|s|} ) ->
```
Stack:
```
parse_core_type: optlabel LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 74

```
delimited_type_supporting_local_open: LPAREN core_type . RPAREN
reversed_separated_nontrivial_llist_COMMA_core_type_: core_type . COMMA core_type
```

## Sample 1

Sentence:
```
?label: ( {%%ext|s|} [@ and ]
```
Stack:
```
parse_core_type: optlabel LPAREN core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 75

```
reversed_separated_nontrivial_llist_COMMA_core_type_: core_type COMMA . core_type
```

## Sample 1

Sentence:
```
( {%%ext|s|} ,
```
Stack:
```
parse_core_type: LPAREN core_type COMMA
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 76

```
function_type: optlabel tuple_type . MINUSGREATER function_type
```

## Sample 1

Sentence:
```
?label: _
```
Stack:
```
parse_core_type: optlabel UNDERSCORE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 77

```
function_type: optlabel tuple_type MINUSGREATER . function_type
```

## Sample 1

Sentence:
```
?label: {%%ext|s|} ->
```
Stack:
```
parse_core_type: optlabel tuple_type MINUSGREATER
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 78

```
delimited_type_supporting_local_open: LBRACKET BAR reversed_separated_nonempty_llist_BAR_row_field_ . RBRACKET
```

## Sample 1

Sentence:
```
[ | {%%ext|s|} | {%%ext|s|} [@ and ]
```
Stack:
```
parse_core_type: LBRACKET BAR reversed_separated_nonempty_llist_BAR_row_field_ BAR core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 79

```
reversed_separated_nonempty_llist_BAR_row_field_: reversed_separated_nonempty_llist_BAR_row_field_ BAR . row_field
```

## Sample 1

Sentence:
```
[ | {%%ext|s|} |
```
Stack:
```
parse_core_type: LBRACKET BAR reversed_separated_nonempty_llist_BAR_row_field_ BAR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 80

```
tag_field: name_tag OF opt_ampersand . reversed_separated_nonempty_llist_AMPERSAND_core_type_no_attr_ list_attribute_
```

## Sample 1

Sentence:
```
[ ` x of
```
Stack:
```
parse_core_type: LBRACKET name_tag OF
```
Rejected when looking ahead at any of the terminals in:
```
&& and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 81

```
reversed_separated_nonempty_llist_AMPERSAND_core_type_no_attr_: reversed_separated_nonempty_llist_AMPERSAND_core_type_no_attr_ AMPERSAND . alias_type
```

## Sample 1

Sentence:
```
[ ` x of {%%ext|s|} &
```
Stack:
```
parse_core_type: LBRACKET name_tag OF opt_ampersand reversed_separated_nonempty_llist_AMPERSAND_core_type_no_attr_ AMPERSAND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 82

```
delimited_type_supporting_local_open: LBRACKET row_field . BAR reversed_separated_nonempty_llist_BAR_row_field_ RBRACKET
```

## Sample 1

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
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 83

```
delimited_type_supporting_local_open: LBRACKET row_field BAR . reversed_separated_nonempty_llist_BAR_row_field_ RBRACKET
```

## Sample 1

Sentence:
```
[ {%%ext|s|} |
```
Stack:
```
parse_core_type: LBRACKET row_field BAR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 84

```
delimited_type_supporting_local_open: LBRACKET row_field BAR reversed_separated_nonempty_llist_BAR_row_field_ . RBRACKET
```

## Sample 1

Sentence:
```
[ {%%ext|s|} | {%%ext|s|} [@ and ]
```
Stack:
```
parse_core_type: LBRACKET row_field BAR core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 85

```
delimited_type_supporting_local_open: LBRACKETGREATER option_BAR_ reversed_separated_nonempty_llist_BAR_row_field_ . RBRACKET
```

## Sample 1

Sentence:
```
[> {%%ext|s|} | {%%ext|s|} [@ and ]
```
Stack:
```
parse_core_type: LBRACKETGREATER option_BAR_ reversed_separated_nonempty_llist_BAR_row_field_ BAR core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 86

```
delimited_type_supporting_local_open: LBRACKETLESS option_BAR_ reversed_separated_nonempty_llist_BAR_row_field_ . RBRACKET
delimited_type_supporting_local_open: LBRACKETLESS option_BAR_ reversed_separated_nonempty_llist_BAR_row_field_ . GREATER reversed_nonempty_llist_name_tag_ RBRACKET
```

## Sample 1

Sentence:
```
[< {%%ext|s|} | {%%ext|s|} [@ and ]
```
Stack:
```
parse_core_type: LBRACKETLESS option_BAR_ reversed_separated_nonempty_llist_BAR_row_field_ BAR core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 87

```
delimited_type_supporting_local_open: LBRACKETLESS option_BAR_ reversed_separated_nonempty_llist_BAR_row_field_ GREATER . reversed_nonempty_llist_name_tag_ RBRACKET
```

## Sample 1

Sentence:
```
[< {%%ext|s|} >
```
Stack:
```
parse_core_type: LBRACKETLESS option_BAR_ reversed_separated_nonempty_llist_BAR_row_field_ GREATER
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 88

```
delimited_type_supporting_local_open: LBRACKETLESS option_BAR_ reversed_separated_nonempty_llist_BAR_row_field_ GREATER reversed_nonempty_llist_name_tag_ . RBRACKET
```

## Sample 1

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
&& & and and* as assert ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 89

```
type_constraint: COLON . core_type _*
```

## Sample 1

Sentence:
```
( X :
```
Stack:
```
use_file: LPAREN seq_expr COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 90

```
type_constraint: COLON core_type COLONGREATER . core_type
```

## Sample 1

Sentence:
```
( X : {%%ext|s|} :>
```
Stack:
```
use_file: LPAREN seq_expr COLON core_type COLONGREATER
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 91

```
reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint . RPAREN _*
```

## Sample 1

Sentence:
```
~ ( x : {%%ext|s|} :> {%%ext|s|} [@ and ]
```
Stack:
```
use_file: TILDE LPAREN LIDENT COLON core_type COLONGREATER core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 92

```
reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint RPAREN . COMMA _*
```

## Sample 1

Sentence:
```
~ ( x : {%%ext|s|} )
```
Stack:
```
use_file: TILDE LPAREN LIDENT type_constraint RPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 93

```
reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint RPAREN COMMA . expr
reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint RPAREN COMMA . LABEL simple_expr
reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint RPAREN COMMA . TILDE _*
```

## Sample 1

Sentence:
```
~ ( x : {%%ext|s|} ) ,
```
Stack:
```
use_file: TILDE LPAREN LIDENT type_constraint RPAREN COMMA
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 94

```
reversed_labeled_tuple_body: TILDE . LIDENT _*
reversed_labeled_tuple_body: TILDE . LPAREN _*
reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint RPAREN COMMA TILDE . LIDENT
reversed_labeled_tuple_body: TILDE . LPAREN LIDENT type_constraint RPAREN COMMA TILDE LPAREN LIDENT type_constraint RPAREN
reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint RPAREN COMMA TILDE . LPAREN LIDENT type_constraint RPAREN
```

## Sample 1

Sentence:
```
~ ( x : {%%ext|s|} ) , ~
```
Stack:
```
use_file: TILDE LPAREN LIDENT type_constraint RPAREN COMMA TILDE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 95

```
reversed_labeled_tuple_body: TILDE LPAREN . LIDENT _*
reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint RPAREN COMMA TILDE LPAREN . LIDENT type_constraint RPAREN
```

## Sample 1

Sentence:
```
~ ( x : {%%ext|s|} ) , ~ (
```
Stack:
```
use_file: TILDE LPAREN LIDENT type_constraint RPAREN COMMA TILDE LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 96

```
reversed_labeled_tuple_body: TILDE LPAREN LIDENT . type_constraint _*
reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint RPAREN COMMA TILDE LPAREN LIDENT . type_constraint RPAREN
```

## Sample 1

Sentence:
```
~ ( x : {%%ext|s|} ) , ~ ( x
```
Stack:
```
use_file: TILDE LPAREN LIDENT type_constraint RPAREN COMMA TILDE LPAREN LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 97

```
reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint . RPAREN _*
reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint RPAREN COMMA TILDE LPAREN LIDENT type_constraint . RPAREN
```

## Sample 1

Sentence:
```
~ ( x : {%%ext|s|} ) , ~ ( x : {%%ext|s|} :> {%%ext|s|} [@ and ]
```
Stack:
```
use_file: TILDE LPAREN LIDENT type_constraint RPAREN COMMA TILDE LPAREN LIDENT COLON core_type COLONGREATER core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 98

```
reversed_labeled_tuple_body: TILDE LIDENT COMMA . expr
reversed_labeled_tuple_body: TILDE LIDENT COMMA . LABEL simple_expr
reversed_labeled_tuple_body: TILDE LIDENT COMMA . TILDE _*
```

## Sample 1

Sentence:
```
~ x ,
```
Stack:
```
use_file: TILDE LIDENT COMMA
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 99

```
reversed_labeled_tuple_body: TILDE . LIDENT _*
reversed_labeled_tuple_body: TILDE LIDENT COMMA TILDE . LIDENT
reversed_labeled_tuple_body: TILDE . LIDENT COMMA TILDE LPAREN LIDENT type_constraint RPAREN
reversed_labeled_tuple_body: TILDE LIDENT COMMA TILDE . LPAREN LIDENT type_constraint RPAREN
reversed_labeled_tuple_body: TILDE . LPAREN _*
```

## Sample 1

Sentence:
```
~ x , ~
```
Stack:
```
use_file: TILDE LIDENT COMMA TILDE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 100

```
reversed_labeled_tuple_body: TILDE LIDENT COMMA TILDE LPAREN . LIDENT type_constraint RPAREN
reversed_labeled_tuple_body: TILDE LPAREN . LIDENT _*
```

## Sample 1

Sentence:
```
~ x , ~ (
```
Stack:
```
use_file: TILDE LIDENT COMMA TILDE LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 101

```
reversed_labeled_tuple_body: TILDE LIDENT COMMA TILDE LPAREN LIDENT . type_constraint RPAREN
reversed_labeled_tuple_body: TILDE LPAREN LIDENT . type_constraint _*
```

## Sample 1

Sentence:
```
~ x , ~ ( x
```
Stack:
```
use_file: TILDE LIDENT COMMA TILDE LPAREN LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 102

```
reversed_labeled_tuple_body: TILDE LIDENT COMMA TILDE LPAREN LIDENT type_constraint . RPAREN
reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint . RPAREN _*
```

## Sample 1

Sentence:
```
~ x , ~ ( x : {%%ext|s|} :> {%%ext|s|} [@ and ]
```
Stack:
```
use_file: TILDE LIDENT COMMA TILDE LPAREN LIDENT COLON core_type COLONGREATER core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 103

```
simple_expr: PREFIXOP . simple_expr
```

## Sample 1

Sentence:
```
!+
```
Stack:
```
use_file: PREFIXOP
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy [@ [@@ [@@@ [> [< [%% < <- let let* match >. method - -. -> module mutable nonrec of open ?label: or % + +. += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type _ val virtual when while with
```

# Pattern 104

```
class_self_pattern: LPAREN . pattern _*
```

## Sample 1

Sentence:
```
object (
```
Stack:
```
use_file: OBJECT ext list_attribute_ LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 105

```
labeled_tuple_pat_element_list_pattern_: TILDE . LIDENT _*
labeled_tuple_pat_element_list_pattern_: TILDE . LPAREN _*
reversed_labeled_tuple_pattern_pattern_: TILDE . LIDENT COMMA DOTDOT
reversed_labeled_tuple_pattern_pattern_: TILDE . LPAREN LIDENT COLON core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
~
```
Stack:
```
parse_pattern: TILDE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 106

```
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN . LIDENT _*
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN . LIDENT COLON core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
~ (
```
Stack:
```
parse_pattern: TILDE LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 107

```
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT . COLON _*
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
~ ( x
```
Stack:
```
parse_pattern: TILDE LPAREN LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 108

```
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON . core_type _*
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON . core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
~ ( x :
```
Stack:
```
parse_pattern: TILDE LPAREN LIDENT COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 109

```
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN _*
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 110

```
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN . COMMA _*
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN . COMMA DOTDOT
```

## Sample 1

Sentence:
```
~ ( x : {%%ext|s|} )
```
Stack:
```
parse_pattern: TILDE LPAREN LIDENT COLON core_type RPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 111

```
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA . pattern
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA . LABEL simple_pattern
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA . TILDE _*
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA . DOTDOT
```

## Sample 1

Sentence:
```
~ ( x : {%%ext|s|} ) ,
```
Stack:
```
parse_pattern: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .+ downto else end  = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 112

```
labeled_tuple_pat_element_list_pattern_: TILDE . LIDENT _*
labeled_tuple_pat_element_list_pattern_: TILDE . LPAREN _*
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE . LIDENT
labeled_tuple_pat_element_list_pattern_: TILDE . LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN LIDENT COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE . LPAREN LIDENT COLON core_type RPAREN
reversed_labeled_tuple_pattern_pattern_: TILDE . LIDENT COMMA DOTDOT
reversed_labeled_tuple_pattern_pattern_: TILDE . LPAREN LIDENT COLON core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
~ ( x : {%%ext|s|} ) , ~
```
Stack:
```
parse_pattern: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 113

```
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN . LIDENT _*
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN . LIDENT COLON core_type RPAREN
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN . LIDENT COLON core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
~ ( x : {%%ext|s|} ) , ~ (
```
Stack:
```
parse_pattern: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 114

```
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT . COLON _*
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN LIDENT . COLON core_type RPAREN
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
~ ( x : {%%ext|s|} ) , ~ ( x
```
Stack:
```
parse_pattern: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 115

```
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON . core_type _*
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN LIDENT COLON . core_type RPAREN
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON . core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
~ ( x : {%%ext|s|} ) , ~ ( x :
```
Stack:
```
parse_pattern: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN LIDENT COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 116

```
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN _*
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 117

```
labeled_tuple_pat_element_list_pattern_: TILDE LIDENT COMMA . pattern
labeled_tuple_pat_element_list_pattern_: TILDE LIDENT COMMA . LABEL simple_pattern
labeled_tuple_pat_element_list_pattern_: TILDE LIDENT COMMA . TILDE _*
reversed_labeled_tuple_pattern_pattern_: TILDE LIDENT COMMA . DOTDOT
```

## Sample 1

Sentence:
```
~ x ,
```
Stack:
```
parse_pattern: TILDE LIDENT COMMA
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .+ downto else end  = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 118

```
labeled_tuple_pat_element_list_pattern_: TILDE . LIDENT _*
labeled_tuple_pat_element_list_pattern_: TILDE LIDENT COMMA TILDE . LIDENT
labeled_tuple_pat_element_list_pattern_: TILDE . LIDENT COMMA TILDE LPAREN LIDENT COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LIDENT COMMA TILDE . LPAREN LIDENT COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE . LPAREN _*
reversed_labeled_tuple_pattern_pattern_: TILDE . LIDENT COMMA DOTDOT
reversed_labeled_tuple_pattern_pattern_: TILDE . LPAREN LIDENT COLON core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
~ x , ~
```
Stack:
```
parse_pattern: TILDE LIDENT COMMA TILDE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 119

```
labeled_tuple_pat_element_list_pattern_: TILDE LIDENT COMMA TILDE LPAREN . LIDENT COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN . LIDENT _*
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN . LIDENT COLON core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
~ x , ~ (
```
Stack:
```
parse_pattern: TILDE LIDENT COMMA TILDE LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 120

```
labeled_tuple_pat_element_list_pattern_: TILDE LIDENT COMMA TILDE LPAREN LIDENT . COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT . COLON _*
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
~ x , ~ ( x
```
Stack:
```
parse_pattern: TILDE LIDENT COMMA TILDE LPAREN LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 121

```
labeled_tuple_pat_element_list_pattern_: TILDE LIDENT COMMA TILDE LPAREN LIDENT COLON . core_type RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON . core_type _*
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON . core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
~ x , ~ ( x :
```
Stack:
```
parse_pattern: TILDE LIDENT COMMA TILDE LPAREN LIDENT COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 122

```
labeled_tuple_pat_element_list_pattern_: TILDE LIDENT COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN _*
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 123

```
signed_constant: PLUS . INT
signed_constant: PLUS . FLOAT
```

## Sample 1

Sentence:
```
+
```
Stack:
```
parse_pattern: PLUS
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 124

```
signed_constant: MINUS . INT
signed_constant: MINUS . FLOAT
```

## Sample 1

Sentence:
```
-
```
Stack:
```
parse_pattern: MINUS
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 125

```
constr_extra_nonprefix_ident: LPAREN . RPAREN
constr_longident: LPAREN . COLONCOLON RPAREN
simple_pattern_not_ident: LPAREN . pattern RPAREN
simple_pattern_not_ident: LPAREN . MODULE _*
simple_pattern_not_ident: LPAREN . pattern COLON core_type RPAREN
val_extra_ident: LPAREN . operator RPAREN
```

## Sample 1

Sentence:
```
(
```
Stack:
```
parse_pattern: LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
and as assert | |] begin class : :> , (*comment*) constraint do (**documentation *) done . .. downto else end  external for fun function functor >} >] if in include inherit initializer {< [@ [@@ [@@@ [> [< [%% <- let match >. .< .~ method -> mutable new nonrec object of open ?label: private ? ' {%%%%ext|s|} } ] rec ; ;; sig struct then to try type val virtual when while with
```

# Pattern 126

```
simple_pattern_not_ident: LPAREN MODULE ext list_attribute_ . module_name _*
```

## Sample 1

Sentence:
```
( module
```
Stack:
```
parse_pattern: LPAREN MODULE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 127

```
simple_pattern_not_ident: LPAREN MODULE ext list_attribute_ module_name . RPAREN
simple_pattern_not_ident: LPAREN MODULE ext list_attribute_ module_name . COLON module_type RPAREN
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 128

```
simple_pattern_not_ident: LPAREN MODULE ext list_attribute_ module_name COLON . module_type RPAREN
```

## Sample 1

Sentence:
```
( module X :
```
Stack:
```
parse_pattern: LPAREN MODULE ext list_attribute_ module_name COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 129

```
module_type: MODULE . TYPE OF list_attribute_ module_expr
```

## Sample 1

Sentence:
```
module
```
Stack:
```
parse_module_type: MODULE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try X _ val virtual when while with
```

# Pattern 130

```
module_type: MODULE TYPE . OF list_attribute_ module_expr
```

## Sample 1

Sentence:
```
module type
```
Stack:
```
parse_module_type: MODULE TYPE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 131

```
module_type: MODULE TYPE OF list_attribute_ . module_expr
```

## Sample 1

Sentence:
```
module type of
```
Stack:
```
parse_module_type: MODULE TYPE OF
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ val virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ val virtual when while with
```

# Pattern 132

```
open_declaration: OPEN BANG ext list_attribute_ . module_expr list_post_item_attribute_
```

## Sample 1

Sentence:
```
open !
```
Stack:
```
use_file: OPEN BANG
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ val virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ val virtual when while with
```

# Pattern 133

```
paren_module_expr: LPAREN . module_expr _*
paren_module_expr: LPAREN . VAL list_attribute_ expr_colon_package_type RPAREN
```

## Sample 1

Sentence:
```
(
```
Stack:
```
parse_module_expr: LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ virtual when while with
```

# Pattern 134

```
paren_module_expr: LPAREN VAL list_attribute_ . expr_colon_package_type RPAREN
```

## Sample 1

Sentence:
```
( val
```
Stack:
```
parse_module_expr: LPAREN VAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

## Sample 2

Sentence:
```
( val [@ and ]
```
Stack:
```
parse_module_expr: LPAREN VAL LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 135

```
simple_expr: NEW ext list_attribute_ . class_longident
```

## Sample 1

Sentence:
```
new
```
Stack:
```
use_file: NEW
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 136

```
mk_longident_mod_longident_LIDENT_: mod_longident . DOT LIDENT
mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT
```

## Sample 1

Sentence:
```
new X . X
```
Stack:
```
use_file: NEW ext list_attribute_ mod_longident DOT UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 137

```
mk_longident_mod_longident_LIDENT_: mod_longident DOT . LIDENT
mk_longident_mod_longident_UIDENT_: mod_longident DOT . UIDENT
```

## Sample 1

Sentence:
```
{ X .
```
Stack:
```
parse_pattern: LBRACE mod_longident DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 138

```
simple_expr: METAOCAML_ESCAPE . simple_expr
```

## Sample 1

Sentence:
```
.~
```
Stack:
```
use_file: METAOCAML_ESCAPE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy [@ [@@ [@@@ [> [< [%% < <- let let* match >. method - -. -> module mutable nonrec of open ?label: or % + +. += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type _ val virtual when while with
```

# Pattern 139

```
simple_expr: METAOCAML_BRACKET_OPEN . seq_expr METAOCAML_BRACKET_CLOSE
```

## Sample 1

Sentence:
```
.<
```
Stack:
```
use_file: METAOCAML_BRACKET_OPEN
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 140

```
fun_expr: MATCH ext list_attribute_ . seq_expr WITH reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
```

## Sample 1

Sentence:
```
match
```
Stack:
```
use_file: MATCH
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

## Sample 2

Sentence:
```
match [@ and ]
```
Stack:
```
use_file: MATCH ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 141

```
constr_extra_nonprefix_ident: LPAREN . RPAREN
constr_longident: LPAREN . COLONCOLON RPAREN
simple_expr: LPAREN . seq_expr _*
simple_expr: LPAREN . MODULE _*
val_extra_ident: LPAREN . operator RPAREN
```

## Sample 1

Sentence:
```
(
```
Stack:
```
use_file: LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
and as | |] class : :> , (*comment*) constraint do (**documentation *) done . .. downto effect else end  exception external functor >} >] # in include inherit initializer [@ [@@ [@@@ [> [< [%% <- >. method -> mutable nonrec of open ?label: private ? ' {%%%%ext|s|} } ] rec ; ;; sig struct then to type _ val virtual when with
```

# Pattern 142

```
simple_expr: LBRACKETBAR . separated_or_terminated_nonempty_list_SEMI_expr_ BARRBRACKET
simple_expr: LBRACKETBAR . BARRBRACKET
```

## Sample 1

Sentence:
```
[|
```
Stack:
```
use_file: LBRACKETBAR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

# Pattern 143

```
fun_expr: LIDENT LESSMINUS . expr
```

## Sample 1

Sentence:
```
x <-
```
Stack:
```
use_file: LIDENT LESSMINUS
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 144

```
fun_expr: LETOP . letop_bindings IN seq_expr
```

## Sample 1

Sentence:
```
let*
```
Stack:
```
use_file: LETOP
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 145

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE . LIDENT _*
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE . LPAREN _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE . LIDENT COMMA DOTDOT
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE . LPAREN LIDENT COLON core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
let* ~
```
Stack:
```
use_file: LETOP TILDE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 146

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN . LIDENT _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN . LIDENT COLON core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
let* ~ (
```
Stack:
```
use_file: LETOP TILDE LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 147

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT . COLON _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
let* ~ ( x
```
Stack:
```
use_file: LETOP TILDE LPAREN LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 148

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON . core_type _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON . core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
let* ~ ( x :
```
Stack:
```
use_file: LETOP TILDE LPAREN LIDENT COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 149

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . RPAREN _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 150

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type RPAREN . COMMA _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type RPAREN . COMMA DOTDOT
```

## Sample 1

Sentence:
```
let* ~ ( x : {%%ext|s|} )
```
Stack:
```
use_file: LETOP TILDE LPAREN LIDENT COLON core_type RPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 151

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA . pattern_no_exn
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA . LABEL simple_pattern
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA . TILDE _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA . DOTDOT
```

## Sample 1

Sentence:
```
let* ~ ( x : {%%ext|s|} ) ,
```
Stack:
```
use_file: LETOP TILDE LPAREN LIDENT COLON core_type RPAREN COMMA
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .+ downto effect else end  = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 152

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE . LIDENT _*
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE . LPAREN _*
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE . LIDENT
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE . LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN LIDENT COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE . LPAREN LIDENT COLON core_type RPAREN
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE . LIDENT COMMA DOTDOT
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE . LPAREN LIDENT COLON core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
let* ~ ( x : {%%ext|s|} ) , ~
```
Stack:
```
use_file: LETOP TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 153

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN . LIDENT _*
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN . LIDENT COLON core_type RPAREN
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN . LIDENT COLON core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
let* ~ ( x : {%%ext|s|} ) , ~ (
```
Stack:
```
use_file: LETOP TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 154

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT . COLON _*
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN LIDENT . COLON core_type RPAREN
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
let* ~ ( x : {%%ext|s|} ) , ~ ( x
```
Stack:
```
use_file: LETOP TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 155

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON . core_type _*
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN LIDENT COLON . core_type RPAREN
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON . core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
let* ~ ( x : {%%ext|s|} ) , ~ ( x :
```
Stack:
```
use_file: LETOP TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN LIDENT COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 156

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . RPAREN _*
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 157

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LIDENT COMMA . pattern_no_exn
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LIDENT COMMA . LABEL simple_pattern
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LIDENT COMMA . TILDE _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LIDENT COMMA . DOTDOT
```

## Sample 1

Sentence:
```
let* ~ x ,
```
Stack:
```
use_file: LETOP TILDE LIDENT COMMA
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .+ downto effect else end  = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 158

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE . LIDENT _*
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LIDENT COMMA TILDE . LIDENT
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE . LIDENT COMMA TILDE LPAREN LIDENT COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LIDENT COMMA TILDE . LPAREN LIDENT COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE . LPAREN _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE . LIDENT COMMA DOTDOT
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE . LPAREN LIDENT COLON core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
let* ~ x , ~
```
Stack:
```
use_file: LETOP TILDE LIDENT COMMA TILDE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 159

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LIDENT COMMA TILDE LPAREN . LIDENT COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN . LIDENT _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN . LIDENT COLON core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
let* ~ x , ~ (
```
Stack:
```
use_file: LETOP TILDE LIDENT COMMA TILDE LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 160

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LIDENT COMMA TILDE LPAREN LIDENT . COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT . COLON _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
let* ~ x , ~ ( x
```
Stack:
```
use_file: LETOP TILDE LIDENT COMMA TILDE LPAREN LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 161

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LIDENT COMMA TILDE LPAREN LIDENT COLON . core_type RPAREN
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON . core_type _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON . core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
let* ~ x , ~ ( x :
```
Stack:
```
use_file: LETOP TILDE LIDENT COMMA TILDE LPAREN LIDENT COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 162

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LIDENT COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . RPAREN _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 163

```
simple_delimited_pattern: LBRACKETBAR . separated_or_terminated_nonempty_list_SEMI_pattern_ BARRBRACKET
simple_delimited_pattern: LBRACKETBAR . BARRBRACKET
```

## Sample 1

Sentence:
```
[|
```
Stack:
```
parse_pattern: LBRACKETBAR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 164

```
constr_extra_nonprefix_ident: LBRACKET . RBRACKET
simple_delimited_pattern: LBRACKET . separated_or_terminated_nonempty_list_SEMI_pattern_ RBRACKET
```

## Sample 1

Sentence:
```
[
```
Stack:
```
parse_pattern: LBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } rec ) ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 165

```
simple_delimited_pattern: LBRACE . listx_SEMI_record_pat_field_UNDERSCORE_ RBRACE
```

## Sample 1

Sentence:
```
{
```
Stack:
```
parse_pattern: LBRACE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 166

```
simple_delimited_pattern: LBRACE listx_SEMI_record_pat_field_UNDERSCORE_ . RBRACE
```

## Sample 1

Sentence:
```
{ x ; _
```
Stack:
```
parse_pattern: LBRACE label_longident option_preceded_COLON_core_type__ option_preceded_EQUAL_pattern__ SEMI UNDERSCORE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 167

```
option_preceded_COLON_core_type__: COLON . core_type
```

## Sample 1

Sentence:
```
{ x :
```
Stack:
```
parse_pattern: LBRACE label_longident COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 168

```
option_preceded_EQUAL_pattern__: EQUAL . pattern
```

## Sample 1

Sentence:
```
{ x =
```
Stack:
```
parse_pattern: LBRACE label_longident option_preceded_COLON_core_type__ EQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 169

```
pattern_gen: LAZY ext list_attribute_ . simple_pattern
```

## Sample 1

Sentence:
```
lazy
```
Stack:
```
parse_pattern: LAZY
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer ~label: lazy {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type val virtual when while with
```

## Sample 2

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer ~label: lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type val virtual when while with
```

# Pattern 170

```
simple_pattern_not_ident: HASH . type_longident
```

## Sample 1

Sentence:
```

```
Stack:
```
parse_pattern: HASH
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 171

```
simple_pattern_not_ident: signed_constant DOTDOT . signed_constant
```

## Sample 1

Sentence:
```
'a' ..
```
Stack:
```
parse_pattern: signed_constant DOTDOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to true try type X _ val virtual when while with
```

# Pattern 172

```
constr_longident: mod_longident DOT . LPAREN COLONCOLON RPAREN
mk_longident_mod_longident_UIDENT_: mod_longident DOT . UIDENT
simple_pattern_not_ident: mod_longident DOT . simple_delimited_pattern
simple_pattern_not_ident: mod_longident DOT . LBRACKET RBRACKET
simple_pattern_not_ident: mod_longident DOT . LPAREN _*
```

## Sample 1

Sentence:
```
X .
```
Stack:
```
parse_pattern: mod_longident DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy {< [@ [@@ [@@@ [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 173

```
constr_longident: mod_longident DOT LPAREN . COLONCOLON RPAREN
simple_pattern_not_ident: mod_longident DOT LPAREN . RPAREN
simple_pattern_not_ident: mod_longident DOT LPAREN . pattern RPAREN
```

## Sample 1

Sentence:
```
X . (
```
Stack:
```
parse_pattern: mod_longident DOT LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 174

```
labeled_tuple_pat_element_list_pattern_: LABEL . simple_pattern _*
reversed_labeled_tuple_pattern_pattern_: LABEL . simple_pattern COMMA DOTDOT
```

## Sample 1

Sentence:
```
~label:
```
Stack:
```
parse_pattern: LABEL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer ~label: lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type val virtual when while with
```

# Pattern 175

```
labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern . COMMA _*
reversed_labeled_tuple_pattern_pattern_: LABEL simple_pattern . COMMA DOTDOT
```

## Sample 1

Sentence:
```
~label: x
```
Stack:
```
parse_pattern: LABEL LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 176

```
labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA . pattern
labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA . LABEL simple_pattern
labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA . TILDE _*
reversed_labeled_tuple_pattern_pattern_: LABEL simple_pattern COMMA . DOTDOT
```

## Sample 1

Sentence:
```
~label: false ,
```
Stack:
```
parse_pattern: LABEL simple_pattern COMMA
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .+ downto else end  = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 177

```
labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE . LIDENT
labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE . LPAREN LIDENT COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE . LIDENT _*
labeled_tuple_pat_element_list_pattern_: TILDE . LPAREN _*
reversed_labeled_tuple_pattern_pattern_: TILDE . LIDENT COMMA DOTDOT
reversed_labeled_tuple_pattern_pattern_: TILDE . LPAREN LIDENT COLON core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
~label: false , ~
```
Stack:
```
parse_pattern: LABEL simple_pattern COMMA TILDE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 178

```
labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN . LIDENT COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN . LIDENT _*
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN . LIDENT COLON core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
~label: false , ~ (
```
Stack:
```
parse_pattern: LABEL simple_pattern COMMA TILDE LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 179

```
labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT . COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT . COLON _*
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
~label: false , ~ ( x
```
Stack:
```
parse_pattern: LABEL simple_pattern COMMA TILDE LPAREN LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 180

```
labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON . core_type RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON . core_type _*
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON . core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
~label: false , ~ ( x :
```
Stack:
```
parse_pattern: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 181

```
labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN _*
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 182

```
labeled_tuple_pat_element_list_pattern_: LABEL . simple_pattern _*
labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA LABEL . simple_pattern
labeled_tuple_pat_element_list_pattern_: LABEL . simple_pattern _*
reversed_labeled_tuple_pattern_pattern_: LABEL . simple_pattern COMMA DOTDOT
```

## Sample 1

Sentence:
```
~label: false , ~label:
```
Stack:
```
parse_pattern: LABEL simple_pattern COMMA LABEL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer ~label: lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type val virtual when while with
```

# Pattern 183

```
pattern: EXCEPTION ext list_attribute_ . pattern
```

## Sample 1

Sentence:
```
exception
```
Stack:
```
parse_pattern: EXCEPTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

## Sample 2

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 184

```
pattern: EFFECT . pattern_gen COMMA simple_pattern
```

## Sample 1

Sentence:
```
effect
```
Stack:
```
parse_pattern: EFFECT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer ~label: {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type val virtual when while with
```

# Pattern 185

```
pattern: EFFECT pattern_gen . COMMA simple_pattern
```

## Sample 1

Sentence:
```
effect false ( type x ) x
```
Stack:
```
parse_pattern: EFFECT constr_longident LPAREN TYPE nonempty_list_mkrhs_LIDENT__ RPAREN LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 186

```
pattern: EFFECT pattern_gen COMMA . simple_pattern
```

## Sample 1

Sentence:
```
effect false ,
```
Stack:
```
parse_pattern: EFFECT pattern_gen COMMA
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer ~label: lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type val virtual when while with
```

# Pattern 187

```
labeled_tuple_pat_element_list_pattern_: pattern COMMA . pattern
labeled_tuple_pat_element_list_pattern_: pattern COMMA . LABEL simple_pattern
labeled_tuple_pat_element_list_pattern_: pattern COMMA . TILDE _*
reversed_labeled_tuple_pattern_pattern_: pattern COMMA . DOTDOT
```

## Sample 1

Sentence:
```
false ,
```
Stack:
```
parse_pattern: pattern COMMA
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .+ downto else end  = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 188

```
labeled_tuple_pat_element_list_pattern_: pattern COMMA TILDE . LIDENT
labeled_tuple_pat_element_list_pattern_: pattern COMMA TILDE . LPAREN LIDENT COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE . LIDENT _*
labeled_tuple_pat_element_list_pattern_: TILDE . LPAREN _*
reversed_labeled_tuple_pattern_pattern_: TILDE . LIDENT COMMA DOTDOT
reversed_labeled_tuple_pattern_pattern_: TILDE . LPAREN LIDENT COLON core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
false , ~
```
Stack:
```
parse_pattern: pattern COMMA TILDE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 189

```
labeled_tuple_pat_element_list_pattern_: pattern COMMA TILDE LPAREN . LIDENT COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN . LIDENT _*
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN . LIDENT COLON core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
false , ~ (
```
Stack:
```
parse_pattern: pattern COMMA TILDE LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 190

```
labeled_tuple_pat_element_list_pattern_: pattern COMMA TILDE LPAREN LIDENT . COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT . COLON _*
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
false , ~ ( x
```
Stack:
```
parse_pattern: pattern COMMA TILDE LPAREN LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 191

```
labeled_tuple_pat_element_list_pattern_: pattern COMMA TILDE LPAREN LIDENT COLON . core_type RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON . core_type _*
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON . core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
false , ~ ( x :
```
Stack:
```
parse_pattern: pattern COMMA TILDE LPAREN LIDENT COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 192

```
labeled_tuple_pat_element_list_pattern_: pattern COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN _*
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 193

```
labeled_tuple_pat_element_list_pattern_: pattern COMMA LABEL . simple_pattern
labeled_tuple_pat_element_list_pattern_: LABEL . simple_pattern _*
reversed_labeled_tuple_pattern_pattern_: LABEL . simple_pattern COMMA DOTDOT
```

## Sample 1

Sentence:
```
false , ~label:
```
Stack:
```
parse_pattern: pattern COMMA LABEL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer ~label: lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type val virtual when while with
```

# Pattern 194

```
pattern: pattern COLONCOLON . pattern
```

## Sample 1

Sentence:
```
false ::
```
Stack:
```
parse_pattern: pattern COLONCOLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 195

```
pattern: pattern BAR . pattern
```

## Sample 1

Sentence:
```
false |
```
Stack:
```
parse_pattern: pattern BAR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 196

```
pattern: pattern AS . val_ident
```

## Sample 1

Sentence:
```
false as
```
Stack:
```
parse_pattern: pattern AS
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 197

```
labeled_tuple_pat_element_list_pattern_: labeled_tuple_pat_element_list_pattern_ COMMA . pattern
labeled_tuple_pat_element_list_pattern_: labeled_tuple_pat_element_list_pattern_ COMMA . LABEL simple_pattern
labeled_tuple_pat_element_list_pattern_: labeled_tuple_pat_element_list_pattern_ COMMA . TILDE _*
reversed_labeled_tuple_pattern_pattern_: labeled_tuple_pat_element_list_pattern_ COMMA . DOTDOT
```

## Sample 1

Sentence:
```
false , false ,
```
Stack:
```
parse_pattern: labeled_tuple_pat_element_list_pattern_ COMMA
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .+ downto else end  = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 198

```
labeled_tuple_pat_element_list_pattern_: labeled_tuple_pat_element_list_pattern_ COMMA TILDE . LIDENT
labeled_tuple_pat_element_list_pattern_: labeled_tuple_pat_element_list_pattern_ COMMA TILDE . LPAREN LIDENT COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE . LIDENT _*
labeled_tuple_pat_element_list_pattern_: TILDE . LPAREN _*
reversed_labeled_tuple_pattern_pattern_: TILDE . LIDENT COMMA DOTDOT
reversed_labeled_tuple_pattern_pattern_: TILDE . LPAREN LIDENT COLON core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
false , false , ~
```
Stack:
```
parse_pattern: labeled_tuple_pat_element_list_pattern_ COMMA TILDE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 199

```
labeled_tuple_pat_element_list_pattern_: labeled_tuple_pat_element_list_pattern_ COMMA TILDE LPAREN . LIDENT COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN . LIDENT _*
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN . LIDENT COLON core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
false , false , ~ (
```
Stack:
```
parse_pattern: labeled_tuple_pat_element_list_pattern_ COMMA TILDE LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 200

```
labeled_tuple_pat_element_list_pattern_: labeled_tuple_pat_element_list_pattern_ COMMA TILDE LPAREN LIDENT . COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT . COLON _*
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
false , false , ~ ( x
```
Stack:
```
parse_pattern: labeled_tuple_pat_element_list_pattern_ COMMA TILDE LPAREN LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 201

```
labeled_tuple_pat_element_list_pattern_: labeled_tuple_pat_element_list_pattern_ COMMA TILDE LPAREN LIDENT COLON . core_type RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON . core_type _*
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON . core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
false , false , ~ ( x :
```
Stack:
```
parse_pattern: labeled_tuple_pat_element_list_pattern_ COMMA TILDE LPAREN LIDENT COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 202

```
labeled_tuple_pat_element_list_pattern_: labeled_tuple_pat_element_list_pattern_ COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN _*
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 203

```
labeled_tuple_pat_element_list_pattern_: labeled_tuple_pat_element_list_pattern_ COMMA LABEL . simple_pattern
labeled_tuple_pat_element_list_pattern_: LABEL . simple_pattern _*
reversed_labeled_tuple_pattern_pattern_: LABEL . simple_pattern COMMA DOTDOT
```

## Sample 1

Sentence:
```
false , false , ~label:
```
Stack:
```
parse_pattern: labeled_tuple_pat_element_list_pattern_ COMMA LABEL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer ~label: lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type val virtual when while with
```

# Pattern 204

```
constr_extra_nonprefix_ident: LPAREN . RPAREN
constr_longident: LPAREN . COLONCOLON RPAREN
pattern_gen: constr_longident LPAREN . TYPE nonempty_list_mkrhs_LIDENT__ RPAREN simple_pattern
simple_pattern_not_ident: LPAREN . pattern RPAREN
simple_pattern_not_ident: LPAREN . MODULE _*
simple_pattern_not_ident: LPAREN . pattern COLON core_type RPAREN
val_extra_ident: LPAREN . operator RPAREN
```

## Sample 1

Sentence:
```
false (
```
Stack:
```
parse_pattern: constr_longident LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
and as assert | |] begin class : :> , (*comment*) constraint do (**documentation *) done . .. downto else end  external for fun function functor >} >] if in include inherit initializer {< [@ [@@ [@@@ [> [< [%% <- let match >. .< .~ method -> mutable new nonrec object of open ?label: private ? ' {%%%%ext|s|} } ] rec ; ;; sig struct then to try val virtual when while with
```

# Pattern 205

```
pattern_gen: constr_longident LPAREN TYPE . nonempty_list_mkrhs_LIDENT__ RPAREN simple_pattern
```

## Sample 1

Sentence:
```
false ( type
```
Stack:
```
parse_pattern: constr_longident LPAREN TYPE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 206

```
pattern_gen: constr_longident LPAREN TYPE nonempty_list_mkrhs_LIDENT__ . RPAREN simple_pattern
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 207

```
pattern_gen: constr_longident LPAREN TYPE nonempty_list_mkrhs_LIDENT__ RPAREN . simple_pattern
```

## Sample 1

Sentence:
```
false ( type x )
```
Stack:
```
parse_pattern: constr_longident LPAREN TYPE nonempty_list_mkrhs_LIDENT__ RPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer ~label: lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type val virtual when while with
```

# Pattern 208

```
constr_longident: LPAREN COLONCOLON . RPAREN
```

## Sample 1

Sentence:
```
( ::
```
Stack:
```
parse_constr_longident: LPAREN COLONCOLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 209

```
labeled_tuple_pat_element_list_pattern_: pattern . COMMA _*
reversed_labeled_tuple_pattern_pattern_: pattern . COMMA DOTDOT
simple_pattern_not_ident: LPAREN pattern . RPAREN
simple_pattern_not_ident: LPAREN pattern . COLON core_type RPAREN
```

## Sample 1

Sentence:
```
false ( false as x
```
Stack:
```
parse_pattern: constr_longident LPAREN pattern AS LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! || |] begin 'a' class := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 210

```
simple_pattern_not_ident: LPAREN pattern COLON . core_type RPAREN
```

## Sample 1

Sentence:
```
( false :
```
Stack:
```
parse_pattern: LPAREN pattern COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 211

```
simple_pattern_not_ident: LPAREN pattern COLON core_type . RPAREN
```

## Sample 1

Sentence:
```
let* x ~label: ( false : {%%ext|s|} [@ and ]
```
Stack:
```
use_file: LETOP val_ident LABEL LPAREN pattern COLON core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 212

```
constr_longident: mod_longident DOT LPAREN COLONCOLON . RPAREN
```

## Sample 1

Sentence:
```
X . ( ::
```
Stack:
```
parse_constr_longident: mod_longident DOT LPAREN COLONCOLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 213

```
labeled_tuple_pat_element_list_pattern_: pattern . COMMA _*
reversed_labeled_tuple_pattern_pattern_: pattern . COMMA DOTDOT
simple_pattern_not_ident: mod_longident DOT LPAREN pattern . RPAREN
```

## Sample 1

Sentence:
```
X . ( false as x
```
Stack:
```
parse_pattern: mod_longident DOT LPAREN pattern AS LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! || |] begin 'a' class : := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 214

```
simple_delimited_pattern: LBRACKET . separated_or_terminated_nonempty_list_SEMI_pattern_ RBRACKET
simple_pattern_not_ident: mod_longident DOT LBRACKET . RBRACKET
```

## Sample 1

Sentence:
```
X . [
```
Stack:
```
parse_pattern: mod_longident DOT LBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } rec ) ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 215

```
simple_delimited_pattern: LBRACKET separated_or_terminated_nonempty_list_SEMI_pattern_ . RBRACKET
```

## Sample 1

Sentence:
```
X . [ false ;
```
Stack:
```
parse_pattern: mod_longident DOT LBRACKET pattern SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 216

```
simple_delimited_pattern: LBRACKETBAR separated_or_terminated_nonempty_list_SEMI_pattern_ . BARRBRACKET
```

## Sample 1

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
&& & and and* as assert ` ! | || begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 217

```
labeled_tuple_pat_element_list_pattern_no_exn_: LABEL . simple_pattern _*
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LIDENT COMMA LABEL . simple_pattern
reversed_labeled_tuple_pattern_pattern_no_exn_: LABEL . simple_pattern COMMA DOTDOT
```

## Sample 1

Sentence:
```
let* ~ x , ~label:
```
Stack:
```
use_file: LETOP TILDE LIDENT COMMA LABEL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer ~label: lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type val virtual when while with
```

# Pattern 218

```
labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA . pattern_no_exn
labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA . LABEL simple_pattern
labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA . TILDE _*
reversed_labeled_tuple_pattern_pattern_no_exn_: LABEL simple_pattern COMMA . DOTDOT
```

## Sample 1

Sentence:
```
let* ~label: false ,
```
Stack:
```
use_file: LETOP LABEL simple_pattern COMMA
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .+ downto effect else end  = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 219

```
labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA TILDE . LIDENT
labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA TILDE . LPAREN LIDENT COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE . LIDENT _*
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE . LPAREN _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE . LIDENT COMMA DOTDOT
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE . LPAREN LIDENT COLON core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
let* ~label: false , ~
```
Stack:
```
use_file: LETOP LABEL simple_pattern COMMA TILDE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 220

```
labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA TILDE LPAREN . LIDENT COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN . LIDENT _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN . LIDENT COLON core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
let* ~label: false , ~ (
```
Stack:
```
use_file: LETOP LABEL simple_pattern COMMA TILDE LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 221

```
labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT . COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT . COLON _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
let* ~label: false , ~ ( x
```
Stack:
```
use_file: LETOP LABEL simple_pattern COMMA TILDE LPAREN LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 222

```
labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON . core_type RPAREN
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON . core_type _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON . core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
let* ~label: false , ~ ( x :
```
Stack:
```
use_file: LETOP LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 223

```
labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . RPAREN _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 224

```
labeled_tuple_pat_element_list_pattern_no_exn_: LABEL . simple_pattern _*
labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA LABEL . simple_pattern
labeled_tuple_pat_element_list_pattern_no_exn_: LABEL . simple_pattern _*
reversed_labeled_tuple_pattern_pattern_no_exn_: LABEL . simple_pattern COMMA DOTDOT
```

## Sample 1

Sentence:
```
let* ~label: false , ~label:
```
Stack:
```
use_file: LETOP LABEL simple_pattern COMMA LABEL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer ~label: lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type val virtual when while with
```

# Pattern 225

```
labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA . pattern_no_exn
labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA . LABEL simple_pattern
labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA . TILDE _*
reversed_labeled_tuple_pattern_pattern_no_exn_: pattern_no_exn COMMA . DOTDOT
```

## Sample 1

Sentence:
```
let* x ,
```
Stack:
```
use_file: LETOP pattern_no_exn COMMA
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .+ downto effect else end  = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 226

```
labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA TILDE . LIDENT
labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA TILDE . LPAREN LIDENT COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE . LIDENT _*
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE . LPAREN _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE . LIDENT COMMA DOTDOT
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE . LPAREN LIDENT COLON core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
let* x , ~
```
Stack:
```
use_file: LETOP pattern_no_exn COMMA TILDE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 227

```
labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA TILDE LPAREN . LIDENT COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN . LIDENT _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN . LIDENT COLON core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
let* x , ~ (
```
Stack:
```
use_file: LETOP pattern_no_exn COMMA TILDE LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 228

```
labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA TILDE LPAREN LIDENT . COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT . COLON _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
let* x , ~ ( x
```
Stack:
```
use_file: LETOP pattern_no_exn COMMA TILDE LPAREN LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 229

```
labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA TILDE LPAREN LIDENT COLON . core_type RPAREN
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON . core_type _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON . core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
let* x , ~ ( x :
```
Stack:
```
use_file: LETOP pattern_no_exn COMMA TILDE LPAREN LIDENT COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 230

```
labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . RPAREN _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 231

```
labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA LABEL . simple_pattern
labeled_tuple_pat_element_list_pattern_no_exn_: LABEL . simple_pattern _*
reversed_labeled_tuple_pattern_pattern_no_exn_: LABEL . simple_pattern COMMA DOTDOT
```

## Sample 1

Sentence:
```
let* x , ~label:
```
Stack:
```
use_file: LETOP pattern_no_exn COMMA LABEL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer ~label: lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type val virtual when while with
```

# Pattern 232

```
pattern_no_exn: pattern_no_exn COLONCOLON . pattern
```

## Sample 1

Sentence:
```
let* x ::
```
Stack:
```
use_file: LETOP pattern_no_exn COLONCOLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 233

```
pattern_no_exn: pattern_no_exn BAR . pattern
```

## Sample 1

Sentence:
```
let* x |
```
Stack:
```
use_file: LETOP pattern_no_exn BAR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 234

```
pattern_no_exn: pattern_no_exn AS . val_ident
```

## Sample 1

Sentence:
```
let* x as
```
Stack:
```
use_file: LETOP pattern_no_exn AS
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 235

```
labeled_tuple_pat_element_list_pattern_no_exn_: labeled_tuple_pat_element_list_pattern_no_exn_ COMMA . pattern_no_exn
labeled_tuple_pat_element_list_pattern_no_exn_: labeled_tuple_pat_element_list_pattern_no_exn_ COMMA . LABEL simple_pattern
labeled_tuple_pat_element_list_pattern_no_exn_: labeled_tuple_pat_element_list_pattern_no_exn_ COMMA . TILDE _*
reversed_labeled_tuple_pattern_pattern_no_exn_: labeled_tuple_pat_element_list_pattern_no_exn_ COMMA . DOTDOT
```

## Sample 1

Sentence:
```
let* x , false ,
```
Stack:
```
use_file: LETOP labeled_tuple_pat_element_list_pattern_no_exn_ COMMA
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .+ downto effect else end  = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 236

```
labeled_tuple_pat_element_list_pattern_no_exn_: labeled_tuple_pat_element_list_pattern_no_exn_ COMMA TILDE . LIDENT
labeled_tuple_pat_element_list_pattern_no_exn_: labeled_tuple_pat_element_list_pattern_no_exn_ COMMA TILDE . LPAREN LIDENT COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE . LIDENT _*
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE . LPAREN _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE . LIDENT COMMA DOTDOT
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE . LPAREN LIDENT COLON core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
let* x , false , ~
```
Stack:
```
use_file: LETOP labeled_tuple_pat_element_list_pattern_no_exn_ COMMA TILDE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 237

```
labeled_tuple_pat_element_list_pattern_no_exn_: labeled_tuple_pat_element_list_pattern_no_exn_ COMMA TILDE LPAREN . LIDENT COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN . LIDENT _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN . LIDENT COLON core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
let* x , false , ~ (
```
Stack:
```
use_file: LETOP labeled_tuple_pat_element_list_pattern_no_exn_ COMMA TILDE LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 238

```
labeled_tuple_pat_element_list_pattern_no_exn_: labeled_tuple_pat_element_list_pattern_no_exn_ COMMA TILDE LPAREN LIDENT . COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT . COLON _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
let* x , false , ~ ( x
```
Stack:
```
use_file: LETOP labeled_tuple_pat_element_list_pattern_no_exn_ COMMA TILDE LPAREN LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 239

```
labeled_tuple_pat_element_list_pattern_no_exn_: labeled_tuple_pat_element_list_pattern_no_exn_ COMMA TILDE LPAREN LIDENT COLON . core_type RPAREN
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON . core_type _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON . core_type RPAREN COMMA DOTDOT
```

## Sample 1

Sentence:
```
let* x , false , ~ ( x :
```
Stack:
```
use_file: LETOP labeled_tuple_pat_element_list_pattern_no_exn_ COMMA TILDE LPAREN LIDENT COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 240

```
labeled_tuple_pat_element_list_pattern_no_exn_: labeled_tuple_pat_element_list_pattern_no_exn_ COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . RPAREN _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 241

```
labeled_tuple_pat_element_list_pattern_no_exn_: labeled_tuple_pat_element_list_pattern_no_exn_ COMMA LABEL . simple_pattern
labeled_tuple_pat_element_list_pattern_no_exn_: LABEL . simple_pattern _*
reversed_labeled_tuple_pattern_pattern_no_exn_: LABEL . simple_pattern COMMA DOTDOT
```

## Sample 1

Sentence:
```
let* x , false , ~label:
```
Stack:
```
use_file: LETOP labeled_tuple_pat_element_list_pattern_no_exn_ COMMA LABEL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer ~label: lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type val virtual when while with
```

# Pattern 242

```
labeled_tuple_pat_element_list_pattern_no_exn_: LABEL . simple_pattern _*
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA LABEL . simple_pattern
reversed_labeled_tuple_pattern_pattern_no_exn_: LABEL . simple_pattern COMMA DOTDOT
```

## Sample 1

Sentence:
```
let* ~ ( x : {%%ext|s|} ) , ~label:
```
Stack:
```
use_file: LETOP TILDE LPAREN LIDENT COLON core_type RPAREN COMMA LABEL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer ~label: lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type val virtual when while with
```

# Pattern 243

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LIDENT . COMMA _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LIDENT . COMMA DOTDOT
```

## Sample 1

Sentence:
```
let* ~ x
```
Stack:
```
use_file: LETOP TILDE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 244

```
labeled_tuple_pat_element_list_pattern_no_exn_: LABEL . simple_pattern _*
reversed_labeled_tuple_pattern_pattern_no_exn_: LABEL . simple_pattern COMMA DOTDOT
```

## Sample 1

Sentence:
```
let* ~label:
```
Stack:
```
use_file: LETOP LABEL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer ~label: lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type val virtual when while with
```

# Pattern 245

```
labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern . COMMA _*
reversed_labeled_tuple_pattern_pattern_no_exn_: LABEL simple_pattern . COMMA DOTDOT
```

## Sample 1

Sentence:
```
let* ~label: x
```
Stack:
```
use_file: LETOP LABEL LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 246

```
simple_param_pattern: TILDE . LPAREN label_let_pattern RPAREN
simple_param_pattern: TILDE . LIDENT
```

## Sample 1

Sentence:
```
let* x ~
```
Stack:
```
use_file: LETOP val_ident TILDE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 247

```
simple_param_pattern: TILDE LPAREN . label_let_pattern RPAREN
```

## Sample 1

Sentence:
```
let* x ~ (
```
Stack:
```
use_file: LETOP val_ident TILDE LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 248

```
label_let_pattern: LIDENT COLON . possibly_poly_core_type_
```

## Sample 1

Sentence:
```
let* x ~ ( x :
```
Stack:
```
use_file: LETOP val_ident TILDE LPAREN LIDENT COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 249

```
possibly_poly_core_type_: reversed_nonempty_llist_typevar_ . DOT core_type
```

## Sample 1

Sentence:
```
object method x : ' x
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern METHOD list_attribute_ private_flag LIDENT COLON QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 250

```
possibly_poly_core_type_: reversed_nonempty_llist_typevar_ DOT . core_type
```

## Sample 1

Sentence:
```
external x : ' x .
```
Stack:
```
use_file: EXTERNAL ext list_attribute_ val_ident COLON reversed_nonempty_llist_typevar_ DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 251

```
simple_param_pattern: TILDE LPAREN label_let_pattern . RPAREN
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 252

```
simple_param_pattern: QUESTION . LPAREN label_let_pattern option_preceded_EQUAL_seq_expr__ RPAREN
simple_param_pattern: QUESTION . LIDENT
```

## Sample 1

Sentence:
```
let* x ?
```
Stack:
```
use_file: LETOP val_ident QUESTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 253

```
simple_param_pattern: QUESTION LPAREN . label_let_pattern option_preceded_EQUAL_seq_expr__ RPAREN
```

## Sample 1

Sentence:
```
let* x ? (
```
Stack:
```
use_file: LETOP val_ident QUESTION LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 254

```
option_preceded_EQUAL_seq_expr__: EQUAL . seq_expr
```

## Sample 1

Sentence:
```
let* x ?label: ( false =
```
Stack:
```
use_file: LETOP val_ident OPTLABEL LPAREN let_pattern EQUAL
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 255

```
module_type_declaration: MODULE TYPE ext list_attribute_ . ident option_preceded_EQUAL_module_type__ list_post_item_attribute_
```

## Sample 1

Sentence:
```
module type
```
Stack:
```
use_file: MODULE TYPE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 256

```
option_preceded_EQUAL_module_type__: EQUAL . module_type
```

## Sample 1

Sentence:
```
module type x =
```
Stack:
```
interface: MODULE TYPE ext list_attribute_ ident EQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 257

```
functor_arg: LPAREN . RPAREN
functor_arg: LPAREN . module_name COLON module_type RPAREN
module_type: LPAREN . module_type RPAREN
```

## Sample 1

Sentence:
```
(
```
Stack:
```
parse_module_type: LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ; ;; * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 258

```
module_type: FUNCTOR list_attribute_ . reversed_nonempty_llist_functor_arg_ MINUSGREATER module_type
```

## Sample 1

Sentence:
```
functor
```
Stack:
```
parse_module_type: FUNCTOR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 259

```
functor_arg: LPAREN . RPAREN
functor_arg: LPAREN . module_name COLON module_type RPAREN
```

## Sample 1

Sentence:
```
( ) (
```
Stack:
```
parse_module_type: reversed_nonempty_llist_functor_arg_ LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 260

```
functor_arg: LPAREN module_name . COLON module_type RPAREN
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 261

```
functor_arg: LPAREN module_name COLON . module_type RPAREN
```

## Sample 1

Sentence:
```
( X :
```
Stack:
```
parse_module_type: LPAREN module_name COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 262

```
module_type: reversed_nonempty_llist_functor_arg_ . MINUSGREATER module_type
```

## Sample 1

Sentence:
```
( val X : {%%ext|s|} :> ( ) ( X : {%%ext|s|} )
```
Stack:
```
parse_module_expr: LPAREN VAL list_attribute_ expr COLON module_type COLONGREATER reversed_nonempty_llist_functor_arg_ LPAREN module_name COLON module_type RPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 263

```
module_type: reversed_nonempty_llist_functor_arg_ MINUSGREATER . module_type
```

## Sample 1

Sentence:
```
( ) ->
```
Stack:
```
parse_module_type: reversed_nonempty_llist_functor_arg_ MINUSGREATER
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 264

```
module_type: module_type WITH . reversed_separated_nonempty_llist_AND_with_constraint_
```

## Sample 1

Sentence:
```
{%%ext|s|} with
```
Stack:
```
parse_module_type: module_type WITH
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try X _ val virtual when while with
```

# Pattern 265

```
with_constraint: TYPE type_parameters . label_longident _*
```

## Sample 1

Sentence:
```
{%%ext|s|} with type
```
Stack:
```
parse_module_type: module_type WITH TYPE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

## Sample 2

Sentence:
```
{%%ext|s|} with type ( _ )
```
Stack:
```
parse_module_type: module_type WITH TYPE LPAREN reversed_separated_nonempty_llist_COMMA_type_parameter_ RPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 266

```
with_constraint: TYPE type_parameters label_longident . with_type_binder alias_type reversed_llist_preceded_CONSTRAINT_constrain__
with_constraint: TYPE type_parameters label_longident . COLONEQUAL alias_type
```

## Sample 1

Sentence:
```
{%%ext|s|} with type X . x
```
Stack:
```
parse_module_type: module_type WITH TYPE type_parameters mod_longident DOT LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 267

```
with_constraint: TYPE type_parameters label_longident COLONEQUAL . alias_type
```

## Sample 1

Sentence:
```
{%%ext|s|} with type x :=
```
Stack:
```
parse_module_type: module_type WITH TYPE type_parameters label_longident COLONEQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 268

```
with_constraint: TYPE type_parameters label_longident with_type_binder . alias_type reversed_llist_preceded_CONSTRAINT_constrain__
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 269

```
reversed_llist_preceded_CONSTRAINT_constrain__: reversed_llist_preceded_CONSTRAINT_constrain__ CONSTRAINT . core_type EQUAL core_type
```

## Sample 1

Sentence:
```
type x and x constraint
```
Stack:
```
interface: generic_type_declaration_nonrec_flag_type_kind_ AND list_attribute_ type_parameters LIDENT type_kind reversed_llist_preceded_CONSTRAINT_constrain__ CONSTRAINT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 270

```
reversed_llist_preceded_CONSTRAINT_constrain__: reversed_llist_preceded_CONSTRAINT_constrain__ CONSTRAINT core_type . EQUAL core_type
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 271

```
reversed_llist_preceded_CONSTRAINT_constrain__: reversed_llist_preceded_CONSTRAINT_constrain__ CONSTRAINT core_type EQUAL . core_type
```

## Sample 1

Sentence:
```
type x and x constraint {%%ext|s|} =
```
Stack:
```
interface: generic_type_declaration_nonrec_flag_type_kind_ AND list_attribute_ type_parameters LIDENT type_kind reversed_llist_preceded_CONSTRAINT_constrain__ CONSTRAINT core_type EQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 272

```
with_constraint: MODULE . mod_longident _*
with_constraint: MODULE . TYPE _*
```

## Sample 1

Sentence:
```
{%%ext|s|} with module
```
Stack:
```
parse_module_type: module_type WITH MODULE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try _ val virtual when while with
```

# Pattern 273

```
with_constraint: MODULE TYPE . mty_longident _*
```

## Sample 1

Sentence:
```
{%%ext|s|} with module type
```
Stack:
```
parse_module_type: module_type WITH MODULE TYPE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 274

```
with_constraint: MODULE TYPE mty_longident . EQUAL module_type
with_constraint: MODULE TYPE mty_longident . COLONEQUAL module_type
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 275

```
with_constraint: MODULE TYPE mty_longident EQUAL . module_type
```

## Sample 1

Sentence:
```
{%%ext|s|} with module type X =
```
Stack:
```
parse_module_type: module_type WITH MODULE TYPE mty_longident EQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 276

```
module_type: module_type MINUSGREATER . module_type
```

## Sample 1

Sentence:
```
{%%ext|s|} ->
```
Stack:
```
parse_module_type: module_type MINUSGREATER
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 277

```
mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident . DOT UIDENT
mk_longident_mod_ext_longident_ident_: mod_ext_longident . DOT ident
```

## Sample 1

Sentence:
```
( val X : {%%ext|s|} :> X ( X )
```
Stack:
```
parse_module_expr: LPAREN VAL list_attribute_ expr COLON module_type COLONGREATER mod_ext_longident LPAREN mod_ext_longident RPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 278

```
mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident DOT . UIDENT
mk_longident_mod_ext_longident_ident_: mod_ext_longident DOT . ident
```

## Sample 1

Sentence:
```
X .
```
Stack:
```
parse_mty_longident: mod_ext_longident DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 279

```
with_constraint: MODULE TYPE mty_longident COLONEQUAL . module_type
```

## Sample 1

Sentence:
```
{%%ext|s|} with module type X :=
```
Stack:
```
parse_module_type: module_type WITH MODULE TYPE mty_longident COLONEQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 280

```
mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT
with_constraint: MODULE mod_longident . EQUAL mod_ext_longident
with_constraint: MODULE mod_longident . COLONEQUAL mod_ext_longident
```

## Sample 1

Sentence:
```
{%%ext|s|} with module X . X
```
Stack:
```
parse_module_type: module_type WITH MODULE mod_longident DOT UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 281

```
with_constraint: MODULE mod_longident EQUAL . mod_ext_longident
```

## Sample 1

Sentence:
```
{%%ext|s|} with module X =
```
Stack:
```
parse_module_type: module_type WITH MODULE mod_longident EQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 282

```
mk_longident_mod_longident_UIDENT_: mod_longident DOT . UIDENT
```

## Sample 1

Sentence:
```
X .
```
Stack:
```
parse_mod_longident: mod_longident DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 283

```
with_constraint: MODULE mod_longident COLONEQUAL . mod_ext_longident
```

## Sample 1

Sentence:
```
{%%ext|s|} with module X :=
```
Stack:
```
parse_module_type: module_type WITH MODULE mod_longident COLONEQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 284

```
reversed_separated_nonempty_llist_AND_with_constraint_: reversed_separated_nonempty_llist_AND_with_constraint_ AND . with_constraint
```

## Sample 1

Sentence:
```
{%%ext|s|} with module X := X and
```
Stack:
```
parse_module_type: module_type WITH reversed_separated_nonempty_llist_AND_with_constraint_ AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try X _ val virtual when while with
```

# Pattern 285

```
functor_arg: LPAREN module_name COLON module_type . RPAREN
```

## Sample 1

Sentence:
```
( X : sig end
```
Stack:
```
parse_module_type: LPAREN module_name COLON SIG list_attribute_ signature END
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

# Pattern 286

```
module_type: FUNCTOR list_attribute_ reversed_nonempty_llist_functor_arg_ . MINUSGREATER module_type
```

## Sample 1

Sentence:
```
functor ( X : {%%ext|s|} )
```
Stack:
```
parse_module_type: FUNCTOR list_attribute_ LPAREN module_name COLON module_type RPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 287

```
module_type: FUNCTOR list_attribute_ reversed_nonempty_llist_functor_arg_ MINUSGREATER . module_type
```

## Sample 1

Sentence:
```
functor ( ) ->
```
Stack:
```
parse_module_type: FUNCTOR list_attribute_ reversed_nonempty_llist_functor_arg_ MINUSGREATER
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 288

```
module_type: LPAREN module_type . RPAREN
```

## Sample 1

Sentence:
```
( sig end
```
Stack:
```
parse_module_type: LPAREN SIG list_attribute_ signature END
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

# Pattern 289

```
local_structure_item: MODULE ext list_attribute_ . REC module_name module_binding_body list_post_item_attribute_ list_and_module_binding_
local_structure_item: MODULE ext list_attribute_ . module_name module_binding_body list_post_item_attribute_
```

## Sample 1

Sentence:
```
module
```
Stack:
```
use_file: MODULE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] ) ; ;; sig * "s" struct then ~ to true try val virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 290

```
local_structure_item: MODULE ext list_attribute_ REC . module_name module_binding_body list_post_item_attribute_ list_and_module_binding_
```

## Sample 1

Sentence:
```
module rec
```
Stack:
```
use_file: MODULE ext list_attribute_ REC
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 291

```
local_structure_item: MODULE ext list_attribute_ REC module_name . module_binding_body list_post_item_attribute_ list_and_module_binding_
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 292

```
module_binding_body: EQUAL . module_expr
```

## Sample 1

Sentence:
```
module X =
```
Stack:
```
use_file: MODULE ext list_attribute_ module_name EQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ val virtual when while with
```

# Pattern 293

```
module_expr: FUNCTOR list_attribute_ . reversed_nonempty_llist_functor_arg_ MINUSGREATER module_expr
```

## Sample 1

Sentence:
```
functor
```
Stack:
```
parse_module_expr: FUNCTOR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 294

```
module_expr: FUNCTOR list_attribute_ reversed_nonempty_llist_functor_arg_ . MINUSGREATER module_expr
```

## Sample 1

Sentence:
```
functor ( X : {%%ext|s|} )
```
Stack:
```
parse_module_expr: FUNCTOR list_attribute_ LPAREN module_name COLON module_type RPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 295

```
module_expr: FUNCTOR list_attribute_ reversed_nonempty_llist_functor_arg_ MINUSGREATER . module_expr
```

## Sample 1

Sentence:
```
functor ( ) ->
```
Stack:
```
parse_module_expr: FUNCTOR list_attribute_ reversed_nonempty_llist_functor_arg_ MINUSGREATER
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ val virtual when while with
```

# Pattern 296

```
module_expr: module_expr LPAREN . RPAREN
paren_module_expr: LPAREN . module_expr _*
paren_module_expr: LPAREN . VAL list_attribute_ expr_colon_package_type RPAREN
```

## Sample 1

Sentence:
```
{%%ext|s|} (
```
Stack:
```
parse_module_expr: module_expr LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ; ;; sig * "s" then ~ to true try type _ virtual when while with
```

# Pattern 297

```
paren_module_expr: LPAREN module_expr . COLON module_type RPAREN
paren_module_expr: LPAREN module_expr . RPAREN
```

## Sample 1

Sentence:
```
{%%ext|s|} ( struct end
```
Stack:
```
parse_module_expr: module_expr LPAREN STRUCT list_attribute_ structure END
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 298

```
paren_module_expr: LPAREN module_expr COLON . module_type RPAREN
```

## Sample 1

Sentence:
```
( {%%ext|s|} :
```
Stack:
```
parse_module_expr: LPAREN module_expr COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 299

```
paren_module_expr: LPAREN module_expr COLON module_type . RPAREN
```

## Sample 1

Sentence:
```
( {%%ext|s|} : sig end
```
Stack:
```
parse_module_expr: LPAREN module_expr COLON SIG list_attribute_ signature END
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

# Pattern 300

```
module_binding_body: COLON . module_type EQUAL module_expr
```

## Sample 1

Sentence:
```
module X :
```
Stack:
```
use_file: MODULE ext list_attribute_ module_name COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 301

```
module_binding_body: COLON module_type . EQUAL module_expr
```

## Sample 1

Sentence:
```
module X : sig end
```
Stack:
```
use_file: MODULE ext list_attribute_ module_name COLON SIG list_attribute_ signature END
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

# Pattern 302

```
module_binding_body: COLON module_type EQUAL . module_expr
```

## Sample 1

Sentence:
```
module X : {%%ext|s|} =
```
Stack:
```
use_file: MODULE ext list_attribute_ module_name COLON module_type EQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ val virtual when while with
```

# Pattern 303

```
list_and_module_binding_: AND list_attribute_ . module_name module_binding_body list_post_item_attribute_ list_and_module_binding_
```

## Sample 1

Sentence:
```
module rec X = {%%ext|s|} and
```
Stack:
```
use_file: MODULE ext list_attribute_ REC module_name module_binding_body list_post_item_attribute_ AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 304

```
list_and_module_binding_: AND list_attribute_ module_name . module_binding_body list_post_item_attribute_ list_and_module_binding_
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 305

```
module_binding_body: functor_arg . module_binding_body
```

## Sample 1

Sentence:
```
module X ( X : {%%ext|s|} )
```
Stack:
```
use_file: MODULE ext list_attribute_ module_name LPAREN module_name COLON module_type RPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 306

```
local_structure_item: MODULE ext list_attribute_ module_name . module_binding_body list_post_item_attribute_
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 307

```
item_extension: LBRACKETPERCENTPERCENT . attr_id payload RBRACKET
```

## Sample 1

Sentence:
```
[%%
```
Stack:
```
use_file: LBRACKETPERCENTPERCENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and* ` ! | || |] 'a' : :: := :> , (*comment*) (**documentation *) . .. .+  = 1.0 > >} >] # ## != ^ +! land ** 1 ~label: { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let* ( >. .< .~ - -. -> ?label: % + +. += !+ ? ' {%%ext|s|} {%%%%ext|s|} } ] ) ; ;; * "s" ~ _
```

# Pattern 308

```
payload: QUESTION . pattern _*
```

## Sample 1

Sentence:
```
[% and ?
```
Stack:
```
use_file: LBRACKETPERCENT attr_id QUESTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 309

```
payload: QUESTION pattern WHEN . seq_expr
```

## Sample 1

Sentence:
```
[% and ? false when
```
Stack:
```
use_file: LBRACKETPERCENT attr_id QUESTION pattern WHEN
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 310

```
constr_extra_nonprefix_ident: LBRACKET . RBRACKET
simple_expr: LBRACKET . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET
```

## Sample 1

Sentence:
```
[
```
Stack:
```
use_file: LBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } rec ) ; ;; sig * struct then to type _ val virtual when with
```

# Pattern 311

```
simple_expr: LBRACELESS . separated_or_terminated_nonempty_list_SEMI_object_expr_field_ GREATERRBRACE
simple_expr: LBRACELESS . GREATERRBRACE
```

## Sample 1

Sentence:
```
{<
```
Stack:
```
use_file: LBRACELESS
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 312

```
option_preceded_EQUAL_expr__: EQUAL . expr
```

## Sample 1

Sentence:
```
{< x =
```
Stack:
```
use_file: LBRACELESS LIDENT EQUAL
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 313

```
simple_expr: LBRACE . record_expr_content RBRACE
```

## Sample 1

Sentence:
```
{
```
Stack:
```
use_file: LBRACE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy [@ [@@ [@@@ [> [< [%% < <- let let* match >. method - -. -> module mutable nonrec of open ?label: or % + +. += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type _ val virtual when while with
```

# Pattern 314

```
simple_expr: BEGIN ext list_attribute_ . seq_expr END
simple_expr: BEGIN ext list_attribute_ . END
```

## Sample 1

Sentence:
```
begin
```
Stack:
```
use_file: BEGIN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else  = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

## Sample 2

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
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else  = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

# Pattern 315

```
fun_expr: LAZY ext list_attribute_ . simple_expr
```

## Sample 1

Sentence:
```
lazy
```
Stack:
```
use_file: LAZY
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy [@@ [@@@ [> [< [%% < <- let let* match >. method - -. -> module mutable nonrec of open ?label: or + +. += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type _ val virtual when while with
```

## Sample 2

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
&& & and and* as assert | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy [@ [@@ [@@@ [> [< [%% < <- let let* match >. method - -. -> module mutable nonrec of open ?label: or % + +. += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type _ val virtual when while with
```

# Pattern 316

```
simple_expr: BANG . simple_expr
```

## Sample 1

Sentence:
```
!
```
Stack:
```
use_file: BANG
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy [@ [@@ [@@@ [> [< [%% < <- let let* match >. method - -. -> module mutable nonrec of open ?label: or % + +. += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type _ val virtual when while with
```

# Pattern 317

```
simple_expr: simple_expr HASHOP . simple_expr
```

## Sample 1

Sentence:
```
X ##
```
Stack:
```
use_file: simple_expr HASHOP
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy [@ [@@ [@@@ [> [< [%% < <- let let* match >. method - -. -> module mutable nonrec of open ?label: or % + +. += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type _ val virtual when while with
```

# Pattern 318

```
simple_expr: simple_expr HASH . LIDENT
```

## Sample 1

Sentence:
```
X #
```
Stack:
```
use_file: simple_expr HASH
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 319

```
simple_expr: simple_expr DOTOP . LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN
simple_expr: simple_expr DOTOP . LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE
simple_expr: simple_expr DOTOP . LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET
```

## Sample 1

Sentence:
```
{ 'a' .+
```
Stack:
```
use_file: LBRACE simple_expr DOTOP
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 320

```
simple_expr: simple_expr DOTOP LPAREN . separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN
```

## Sample 1

Sentence:
```
{ 'a' .+ (
```
Stack:
```
use_file: LBRACE simple_expr DOTOP LPAREN
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 321

```
reversed_labeled_tuple_body: LABEL . simple_expr _*
```

## Sample 1

Sentence:
```
~label:
```
Stack:
```
use_file: LABEL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy [@ [@@ [@@@ [> [< [%% < <- let let* match >. method - -. -> module mutable nonrec of open ?label: or % + +. += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type _ val virtual when while with
```

# Pattern 322

```
reversed_labeled_tuple_body: LABEL simple_expr . COMMA _*
```

## Sample 1

Sentence:
```
~label: object end
```
Stack:
```
use_file: LABEL OBJECT ext list_attribute_ class_self_pattern list_text_cstr_class_field__ END
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done .. downto effect else end  = exception external false 1.0 for fun function functor > >} >] if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 323

```
simple_expr: simple_expr DOT . LPAREN seq_expr RPAREN
simple_expr: simple_expr DOT . LBRACE seq_expr RBRACE
simple_expr: simple_expr DOT . LBRACKET seq_expr RBRACKET
simple_expr: simple_expr DOT . mod_longident _*
simple_expr: simple_expr DOT . label_longident
```

## Sample 1

Sentence:
```
{ 'a' .
```
Stack:
```
use_file: LBRACE simple_expr DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 324

```
simple_expr: simple_expr DOT LPAREN . seq_expr RPAREN
```

## Sample 1

Sentence:
```
{ 'a' . (
```
Stack:
```
use_file: LBRACE simple_expr DOT LPAREN
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 325

```
fun_expr: IF ext list_attribute_ . seq_expr _*
```

## Sample 1

Sentence:
```
if
```
Stack:
```
use_file: IF
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

## Sample 2

Sentence:
```
if [@ and ]
```
Stack:
```
use_file: IF ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 326

```
expr: FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
seq_expr: FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
```

## Sample 1

Sentence:
```
function
```
Stack:
```
use_file: FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

## Sample 2

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
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 327

```
reversed_preceded_or_separated_nonempty_llist_BAR_match_case_: BAR . match_case
```

## Sample 1

Sentence:
```
function |
```
Stack:
```
use_file: FUNCTION ext list_attribute_ BAR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 328

```
labeled_tuple_pat_element_list_pattern_: pattern . COMMA _*
match_case: pattern . MINUSGREATER seq_expr
match_case: pattern . WHEN seq_expr MINUSGREATER seq_expr
match_case: pattern . MINUSGREATER DOT
reversed_labeled_tuple_pattern_pattern_: pattern . COMMA DOTDOT
```

## Sample 1

Sentence:
```
try X with false as x
```
Stack:
```
use_file: TRY ext list_attribute_ seq_expr WITH pattern AS LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! || |] begin 'a' class : := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual while with
```

# Pattern 329

```
match_case: pattern WHEN . seq_expr MINUSGREATER seq_expr
```

## Sample 1

Sentence:
```
function false when
```
Stack:
```
use_file: FUNCTION ext list_attribute_ pattern WHEN
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 330

```
fun_expr: FUN ext list_attribute_ . fun_params option_preceded_COLON_atomic_type__ MINUSGREATER fun_body
```

## Sample 1

Sentence:
```
fun
```
Stack:
```
use_file: FUN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer lazy {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open or +. += !+ private ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

## Sample 2

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open or % +. += !+ private ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 331

```
simple_param_pattern: OPTLABEL . LPAREN let_pattern option_preceded_EQUAL_seq_expr__ RPAREN
simple_param_pattern: OPTLABEL . pattern_var
```

## Sample 1

Sentence:
```
let* x ?label:
```
Stack:
```
use_file: LETOP val_ident OPTLABEL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X val virtual when while with
```

# Pattern 332

```
simple_param_pattern: OPTLABEL LPAREN . let_pattern option_preceded_EQUAL_seq_expr__ RPAREN
```

## Sample 1

Sentence:
```
let* x ?label: (
```
Stack:
```
use_file: LETOP val_ident OPTLABEL LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 333

```
let_pattern: pattern COLON . possibly_poly_core_type_
```

## Sample 1

Sentence:
```
let* x ?label: ( false :
```
Stack:
```
use_file: LETOP val_ident OPTLABEL LPAREN pattern COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 334

```
simple_param_pattern: OPTLABEL LPAREN let_pattern option_preceded_EQUAL_seq_expr__ . RPAREN
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 335

```
constr_extra_nonprefix_ident: LPAREN . RPAREN
constr_longident: LPAREN . COLONCOLON RPAREN
fun_param_as_list: LPAREN . TYPE nonempty_list_mkrhs_LIDENT__ RPAREN
simple_param_pattern: LPAREN . pattern COLON reversed_nonempty_llist_typevar_ DOT core_type RPAREN
simple_pattern_not_ident: LPAREN . pattern RPAREN
simple_pattern_not_ident: LPAREN . MODULE _*
simple_pattern_not_ident: LPAREN . pattern COLON core_type RPAREN
val_extra_ident: LPAREN . operator RPAREN
```

## Sample 1

Sentence:
```
let* x (
```
Stack:
```
use_file: LETOP val_ident LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
and as assert | |] begin class : :> , (*comment*) constraint do (**documentation *) done . .. downto else end  external for fun function functor >} >] if in include inherit initializer {< [@ [@@ [@@@ [> [< [%% <- let match >. .< .~ method -> mutable new nonrec object of open ?label: private ? ' {%%%%ext|s|} } ] rec ; ;; sig struct then to try val virtual when while with
```

# Pattern 336

```
fun_param_as_list: LPAREN TYPE . nonempty_list_mkrhs_LIDENT__ RPAREN
```

## Sample 1

Sentence:
```
let* x ( type
```
Stack:
```
use_file: LETOP val_ident LPAREN TYPE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 337

```
fun_param_as_list: LPAREN TYPE nonempty_list_mkrhs_LIDENT__ . RPAREN
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 338

```
labeled_tuple_pat_element_list_pattern_: pattern . COMMA _*
reversed_labeled_tuple_pattern_pattern_: pattern . COMMA DOTDOT
simple_param_pattern: LPAREN pattern . COLON reversed_nonempty_llist_typevar_ DOT core_type RPAREN
simple_pattern_not_ident: LPAREN pattern . RPAREN
simple_pattern_not_ident: LPAREN pattern . COLON core_type RPAREN
```

## Sample 1

Sentence:
```
class x ( false as x
```
Stack:
```
use_file: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT LPAREN pattern AS LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! || |] begin 'a' class := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 339

```
simple_param_pattern: LPAREN pattern COLON . reversed_nonempty_llist_typevar_ DOT core_type RPAREN
simple_pattern_not_ident: LPAREN pattern COLON . core_type RPAREN
```

## Sample 1

Sentence:
```
let* x ( false :
```
Stack:
```
use_file: LETOP val_ident LPAREN pattern COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 340

```
simple_param_pattern: LPAREN pattern COLON reversed_nonempty_llist_typevar_ . DOT core_type RPAREN
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 341

```
simple_param_pattern: LPAREN pattern COLON reversed_nonempty_llist_typevar_ DOT . core_type RPAREN
```

## Sample 1

Sentence:
```
let* x ( false : ' x .
```
Stack:
```
use_file: LETOP val_ident LPAREN pattern COLON reversed_nonempty_llist_typevar_ DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 342

```
simple_param_pattern: LPAREN pattern COLON reversed_nonempty_llist_typevar_ DOT core_type . RPAREN
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 343

```
simple_param_pattern: LABEL . simple_pattern
simple_param_pattern: LABEL . LPAREN pattern COLON reversed_nonempty_llist_typevar_ DOT core_type RPAREN
```

## Sample 1

Sentence:
```
let* x ~label:
```
Stack:
```
use_file: LETOP val_ident LABEL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer ~label: lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type val virtual when while with
```

# Pattern 344

```
constr_extra_nonprefix_ident: LPAREN . RPAREN
constr_longident: LPAREN . COLONCOLON RPAREN
simple_param_pattern: LABEL LPAREN . pattern COLON reversed_nonempty_llist_typevar_ DOT core_type RPAREN
simple_pattern_not_ident: LPAREN . pattern RPAREN
simple_pattern_not_ident: LPAREN . MODULE _*
simple_pattern_not_ident: LPAREN . pattern COLON core_type RPAREN
val_extra_ident: LPAREN . operator RPAREN
```

## Sample 1

Sentence:
```
let* x ~label: (
```
Stack:
```
use_file: LETOP val_ident LABEL LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
and as assert | |] begin class : :> , (*comment*) constraint do (**documentation *) done . .. downto else end  external for fun function functor >} >] if in include inherit initializer {< [@ [@@ [@@@ [> [< [%% <- let match >. .< .~ method -> mutable new nonrec object of open ?label: private ? ' {%%%%ext|s|} } ] rec ; ;; sig struct then to try type val virtual when while with
```

# Pattern 345

```
labeled_tuple_pat_element_list_pattern_: pattern . COMMA _*
reversed_labeled_tuple_pattern_pattern_: pattern . COMMA DOTDOT
simple_param_pattern: LABEL LPAREN pattern . COLON reversed_nonempty_llist_typevar_ DOT core_type RPAREN
simple_pattern_not_ident: LPAREN pattern . RPAREN
simple_pattern_not_ident: LPAREN pattern . COLON core_type RPAREN
```

## Sample 1

Sentence:
```
let* x ~label: ( false as x
```
Stack:
```
use_file: LETOP val_ident LABEL LPAREN pattern AS LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! || |] begin 'a' class := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 346

```
simple_param_pattern: LABEL LPAREN pattern COLON . reversed_nonempty_llist_typevar_ DOT core_type RPAREN
simple_pattern_not_ident: LPAREN pattern COLON . core_type RPAREN
```

## Sample 1

Sentence:
```
let* x ~label: ( false :
```
Stack:
```
use_file: LETOP val_ident LABEL LPAREN pattern COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 347

```
simple_param_pattern: LABEL LPAREN pattern COLON reversed_nonempty_llist_typevar_ . DOT core_type RPAREN
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 348

```
simple_param_pattern: LABEL LPAREN pattern COLON reversed_nonempty_llist_typevar_ DOT . core_type RPAREN
```

## Sample 1

Sentence:
```
let* x ~label: ( false : ' x .
```
Stack:
```
use_file: LETOP val_ident LABEL LPAREN pattern COLON reversed_nonempty_llist_typevar_ DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 349

```
simple_param_pattern: LABEL LPAREN pattern COLON reversed_nonempty_llist_typevar_ DOT core_type . RPAREN
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 350

```
option_preceded_COLON_atomic_type__: COLON . atomic_type
```

## Sample 1

Sentence:
```
fun false :
```
Stack:
```
use_file: FUN ext list_attribute_ fun_params COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 351

```
fun_expr: FUN ext list_attribute_ fun_params option_preceded_COLON_atomic_type__ . MINUSGREATER fun_body
```

## Sample 1

Sentence:
```
fun false : _
```
Stack:
```
use_file: FUN ext list_attribute_ fun_params COLON UNDERSCORE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 352

```
fun_expr: FUN ext list_attribute_ fun_params option_preceded_COLON_atomic_type__ MINUSGREATER . fun_body
```

## Sample 1

Sentence:
```
fun false ->
```
Stack:
```
use_file: FUN ext list_attribute_ fun_params option_preceded_COLON_atomic_type__ MINUSGREATER
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 353

```
expr: FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
fun_body: FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
```

## Sample 1

Sentence:
```
let* x false = function
```
Stack:
```
use_file: LETOP val_ident fun_params option_type_constraint_ EQUAL FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

## Sample 2

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
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 354

```
reversed_preceded_or_separated_nonempty_llist_BAR_match_case_: reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ BAR . match_case
```

## Sample 1

Sentence:
```
function false -> X |
```
Stack:
```
use_file: FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ BAR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 355

```
fun_expr: FOR ext list_attribute_ . pattern EQUAL seq_expr direction_flag seq_expr DO seq_expr DONE
```

## Sample 1

Sentence:
```
for
```
Stack:
```
use_file: FOR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

## Sample 2

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 356

```
fun_expr: FOR ext list_attribute_ pattern . EQUAL seq_expr direction_flag seq_expr DO seq_expr DONE
labeled_tuple_pat_element_list_pattern_: pattern . COMMA _*
reversed_labeled_tuple_pattern_pattern_: pattern . COMMA DOTDOT
```

## Sample 1

Sentence:
```
for false as x
```
Stack:
```
use_file: FOR ext list_attribute_ pattern AS LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! || |] begin 'a' class : := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 357

```
fun_expr: FOR ext list_attribute_ pattern EQUAL . seq_expr direction_flag seq_expr DO seq_expr DONE
```

## Sample 1

Sentence:
```
for false =
```
Stack:
```
use_file: FOR ext list_attribute_ pattern EQUAL
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 358

```
fun_expr: ASSERT ext list_attribute_ . simple_expr
```

## Sample 1

Sentence:
```
assert
```
Stack:
```
use_file: ASSERT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy [@@ [@@@ [> [< [%% < <- let let* match >. method - -. -> module mutable nonrec of open ?label: or + +. += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type _ val virtual when while with
```

## Sample 2

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
&& & and and* as assert | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy [@ [@@ [@@@ [> [< [%% < <- let let* match >. method - -. -> module mutable nonrec of open ?label: or % + +. += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type _ val virtual when while with
```

# Pattern 359

```
constr_longident: mod_longident DOT . LPAREN COLONCOLON RPAREN
mk_longident_mod_longident_UIDENT_: mod_longident DOT . UIDENT
mk_longident_mod_longident_val_ident_: mod_longident DOT . val_ident
simple_expr: mod_longident DOT . LPAREN seq_expr RPAREN
simple_expr: mod_longident DOT . LBRACELESS separated_or_terminated_nonempty_list_SEMI_object_expr_field_ GREATERRBRACE
simple_expr: mod_longident DOT . LPAREN RPAREN
simple_expr: mod_longident DOT . LBRACE record_expr_content RBRACE
simple_expr: mod_longident DOT . LBRACKETBAR _*
simple_expr: mod_longident DOT . LBRACKET _*
simple_expr: mod_longident DOT . LPAREN MODULE ext list_attribute_ module_expr COLON module_type RPAREN
```

## Sample 1

Sentence:
```
X .
```
Stack:
```
use_file: mod_longident DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy [@ [@@ [@@@ [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 360

```
constr_longident: mod_longident DOT LPAREN . COLONCOLON RPAREN
simple_expr: mod_longident DOT LPAREN . seq_expr RPAREN
simple_expr: mod_longident DOT LPAREN . RPAREN
simple_expr: mod_longident DOT LPAREN . MODULE ext list_attribute_ module_expr COLON module_type RPAREN
val_extra_ident: LPAREN . operator RPAREN
```

## Sample 1

Sentence:
```
X . (
```
Stack:
```
use_file: mod_longident DOT LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
and as | |] class : :> , (*comment*) constraint do (**documentation *) done . .. downto effect else end  exception external functor >} >] # in include inherit initializer [@ [@@ [@@@ [> [< [%% <- >. method -> mutable nonrec of open ?label: private ? ' {%%%%ext|s|} } ] rec ; ;; sig struct then to type _ val virtual when with
```

# Pattern 361

```
simple_expr: mod_longident DOT LPAREN MODULE ext list_attribute_ . module_expr COLON module_type RPAREN
```

## Sample 1

Sentence:
```
X . ( module
```
Stack:
```
use_file: mod_longident DOT LPAREN MODULE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ val virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ val virtual when while with
```

# Pattern 362

```
simple_expr: mod_longident DOT LPAREN MODULE ext list_attribute_ module_expr . COLON module_type RPAREN
```

## Sample 1

Sentence:
```
X . ( module struct end
```
Stack:
```
use_file: mod_longident DOT LPAREN MODULE ext list_attribute_ STRUCT list_attribute_ structure END
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 363

```
simple_expr: mod_longident DOT LPAREN MODULE ext list_attribute_ module_expr COLON . module_type RPAREN
```

## Sample 1

Sentence:
```
X . ( module {%%ext|s|} :
```
Stack:
```
use_file: mod_longident DOT LPAREN MODULE ext list_attribute_ module_expr COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 364

```
simple_expr: mod_longident DOT LPAREN MODULE ext list_attribute_ module_expr COLON module_type . RPAREN
```

## Sample 1

Sentence:
```
X . ( module {%%ext|s|} : sig end
```
Stack:
```
use_file: mod_longident DOT LPAREN MODULE ext list_attribute_ module_expr COLON SIG list_attribute_ signature END
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

# Pattern 365

```
letop_binding_body: simple_pattern COLON . core_type EQUAL seq_expr
```

## Sample 1

Sentence:
```
let* x :
```
Stack:
```
use_file: LETOP simple_pattern COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 366

```
letop_binding_body: simple_pattern COLON core_type . EQUAL seq_expr
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 367

```
letop_binding_body: simple_pattern COLON core_type EQUAL . seq_expr
```

## Sample 1

Sentence:
```
let* x : {%%ext|s|} =
```
Stack:
```
use_file: LETOP simple_pattern COLON core_type EQUAL
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 368

```
fun_expr: subtractive . expr
```

## Sample 1

Sentence:
```
X . ( -
```
Stack:
```
use_file: mod_longident DOT LPAREN MINUS
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 369

```
expr: FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
```

## Sample 1

Sentence:
```
+ function
```
Stack:
```
use_file: additive FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

## Sample 2

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
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 370

```
labeled_simple_expr: TILDE . LIDENT
labeled_simple_expr: TILDE . LPAREN LIDENT type_constraint RPAREN
```

## Sample 1

Sentence:
```
X ~
```
Stack:
```
use_file: simple_expr TILDE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 371

```
labeled_simple_expr: TILDE LPAREN . LIDENT type_constraint RPAREN
```

## Sample 1

Sentence:
```
X ~ (
```
Stack:
```
use_file: simple_expr TILDE LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 372

```
labeled_simple_expr: TILDE LPAREN LIDENT . type_constraint RPAREN
```

## Sample 1

Sentence:
```
X ~ ( x
```
Stack:
```
use_file: simple_expr TILDE LPAREN LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 373

```
labeled_simple_expr: TILDE LPAREN LIDENT type_constraint . RPAREN
```

## Sample 1

Sentence:
```
X ~ ( x : {%%ext|s|} :> {%%ext|s|} [@ and ]
```
Stack:
```
use_file: simple_expr TILDE LPAREN LIDENT COLON core_type COLONGREATER core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 374

```
labeled_simple_expr: QUESTION . LIDENT
```

## Sample 1

Sentence:
```
X ?
```
Stack:
```
use_file: simple_expr QUESTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 375

```
labeled_simple_expr: OPTLABEL . simple_expr
```

## Sample 1

Sentence:
```
X ?label:
```
Stack:
```
use_file: simple_expr OPTLABEL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy [@ [@@ [@@@ [> [< [%% < <- let let* match >. method - -. -> module mutable nonrec of open ?label: or % + +. += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type _ val virtual when while with
```

# Pattern 376

```
labeled_simple_expr: LABEL . simple_expr
```

## Sample 1

Sentence:
```
X ~label:
```
Stack:
```
use_file: simple_expr LABEL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy [@ [@@ [@@@ [> [< [%% < <- let let* match >. method - -. -> module mutable nonrec of open ?label: or % + +. += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type _ val virtual when while with
```

# Pattern 377

```
fun_expr: simple_expr DOTOP . LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN LESSMINUS expr
fun_expr: simple_expr DOTOP . LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS expr
fun_expr: simple_expr DOTOP . LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS expr
simple_expr: simple_expr DOTOP . LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN
simple_expr: simple_expr DOTOP . LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE
simple_expr: simple_expr DOTOP . LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET
```

## Sample 1

Sentence:
```
X .+
```
Stack:
```
use_file: simple_expr DOTOP
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 378

```
fun_expr: simple_expr DOTOP LPAREN . separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN LESSMINUS expr
simple_expr: simple_expr DOTOP LPAREN . separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN
```

## Sample 1

Sentence:
```
X .+ (
```
Stack:
```
use_file: simple_expr DOTOP LPAREN
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 379

```
fun_expr: simple_expr DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ . RPAREN LESSMINUS expr
simple_expr: simple_expr DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ . RPAREN
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 380

```
fun_expr: simple_expr DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN LESSMINUS . expr
```

## Sample 1

Sentence:
```
X .+ ( X ) <-
```
Stack:
```
use_file: simple_expr DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN LESSMINUS
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 381

```
reversed_labeled_tuple_body: reversed_labeled_tuple_body COMMA . expr
reversed_labeled_tuple_body: reversed_labeled_tuple_body COMMA . LABEL simple_expr
reversed_labeled_tuple_body: reversed_labeled_tuple_body COMMA . TILDE _*
```

## Sample 1

Sentence:
```
~ x , X ,
```
Stack:
```
use_file: reversed_labeled_tuple_body COMMA
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 382

```
reversed_labeled_tuple_body: reversed_labeled_tuple_body COMMA TILDE . LIDENT
reversed_labeled_tuple_body: reversed_labeled_tuple_body COMMA TILDE . LPAREN LIDENT type_constraint RPAREN
reversed_labeled_tuple_body: TILDE . LIDENT _*
reversed_labeled_tuple_body: TILDE . LPAREN _*
```

## Sample 1

Sentence:
```
~ x , X , ~
```
Stack:
```
use_file: reversed_labeled_tuple_body COMMA TILDE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 383

```
reversed_labeled_tuple_body: reversed_labeled_tuple_body COMMA TILDE LPAREN . LIDENT type_constraint RPAREN
reversed_labeled_tuple_body: TILDE LPAREN . LIDENT _*
```

## Sample 1

Sentence:
```
~ x , X , ~ (
```
Stack:
```
use_file: reversed_labeled_tuple_body COMMA TILDE LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 384

```
reversed_labeled_tuple_body: reversed_labeled_tuple_body COMMA TILDE LPAREN LIDENT . type_constraint RPAREN
reversed_labeled_tuple_body: TILDE LPAREN LIDENT . type_constraint _*
```

## Sample 1

Sentence:
```
~ x , X , ~ ( x
```
Stack:
```
use_file: reversed_labeled_tuple_body COMMA TILDE LPAREN LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 385

```
reversed_labeled_tuple_body: reversed_labeled_tuple_body COMMA TILDE LPAREN LIDENT type_constraint . RPAREN
reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint . RPAREN _*
```

## Sample 1

Sentence:
```
~ x , X , ~ ( x : {%%ext|s|} :> {%%ext|s|} [@ and ]
```
Stack:
```
use_file: reversed_labeled_tuple_body COMMA TILDE LPAREN LIDENT COLON core_type COLONGREATER core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 386

```
reversed_labeled_tuple_body: reversed_labeled_tuple_body COMMA LABEL . simple_expr
reversed_labeled_tuple_body: LABEL . simple_expr _*
```

## Sample 1

Sentence:
```
~ x , X , ~label:
```
Stack:
```
use_file: reversed_labeled_tuple_body COMMA LABEL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy [@ [@@ [@@@ [> [< [%% < <- let let* match >. method - -. -> module mutable nonrec of open ?label: or % + +. += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type _ val virtual when while with
```

# Pattern 387

```
reversed_labeled_tuple_body: LABEL simple_expr COMMA . expr
reversed_labeled_tuple_body: LABEL simple_expr COMMA . LABEL simple_expr
reversed_labeled_tuple_body: LABEL simple_expr COMMA . TILDE _*
```

## Sample 1

Sentence:
```
~label: 'a' ,
```
Stack:
```
use_file: LABEL simple_expr COMMA
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 388

```
reversed_labeled_tuple_body: LABEL simple_expr COMMA TILDE . LIDENT
reversed_labeled_tuple_body: LABEL simple_expr COMMA TILDE . LPAREN LIDENT type_constraint RPAREN
reversed_labeled_tuple_body: TILDE . LIDENT _*
reversed_labeled_tuple_body: TILDE . LPAREN _*
```

## Sample 1

Sentence:
```
~label: 'a' , ~
```
Stack:
```
use_file: LABEL simple_expr COMMA TILDE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 389

```
reversed_labeled_tuple_body: LABEL simple_expr COMMA TILDE LPAREN . LIDENT type_constraint RPAREN
reversed_labeled_tuple_body: TILDE LPAREN . LIDENT _*
```

## Sample 1

Sentence:
```
~label: 'a' , ~ (
```
Stack:
```
use_file: LABEL simple_expr COMMA TILDE LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 390

```
reversed_labeled_tuple_body: LABEL simple_expr COMMA TILDE LPAREN LIDENT . type_constraint RPAREN
reversed_labeled_tuple_body: TILDE LPAREN LIDENT . type_constraint _*
```

## Sample 1

Sentence:
```
~label: 'a' , ~ ( x
```
Stack:
```
use_file: LABEL simple_expr COMMA TILDE LPAREN LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 391

```
reversed_labeled_tuple_body: LABEL simple_expr COMMA TILDE LPAREN LIDENT type_constraint . RPAREN
reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint . RPAREN _*
```

## Sample 1

Sentence:
```
~label: 'a' , ~ ( x : {%%ext|s|} :> {%%ext|s|} [@ and ]
```
Stack:
```
use_file: LABEL simple_expr COMMA TILDE LPAREN LIDENT COLON core_type COLONGREATER core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 392

```
reversed_labeled_tuple_body: LABEL . simple_expr _*
reversed_labeled_tuple_body: LABEL simple_expr COMMA LABEL . simple_expr
reversed_labeled_tuple_body: LABEL . simple_expr _*
```

## Sample 1

Sentence:
```
~label: 'a' , ~label:
```
Stack:
```
use_file: LABEL simple_expr COMMA LABEL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy [@ [@@ [@@@ [> [< [%% < <- let let* match >. method - -. -> module mutable nonrec of open ?label: or % + +. += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type _ val virtual when while with
```

# Pattern 393

```
fun_expr: let_bindings_ext_ . IN seq_expr
```

## Sample 1

Sentence:
```
while X do let x [@@ and ]
```
Stack:
```
use_file: WHILE ext list_attribute_ seq_expr DO LET ext list_attribute_ rec_flag let_binding_body LBRACKETATAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 394

```
fun_expr: let_bindings_ext_ IN . seq_expr
```

## Sample 1

Sentence:
```
let x in
```
Stack:
```
parse_expression: let_bindings_ext_ IN
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 395

```
fun_expr: fun_expr STAR . expr
```

## Sample 1

Sentence:
```
X *
```
Stack:
```
use_file: fun_expr STAR
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 396

```
fun_expr: fun_expr PLUSEQ . expr
```

## Sample 1

Sentence:
```
X +=
```
Stack:
```
use_file: fun_expr PLUSEQ
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 397

```
reversed_labeled_tuple_body: expr COMMA . expr
reversed_labeled_tuple_body: expr COMMA . LABEL simple_expr
reversed_labeled_tuple_body: expr COMMA . TILDE _*
```

## Sample 1

Sentence:
```
[ X ,
```
Stack:
```
use_file: LBRACKET expr COMMA
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 398

```
reversed_labeled_tuple_body: expr COMMA TILDE . LIDENT
reversed_labeled_tuple_body: expr COMMA TILDE . LPAREN LIDENT type_constraint RPAREN
reversed_labeled_tuple_body: TILDE . LIDENT _*
reversed_labeled_tuple_body: TILDE . LPAREN _*
```

## Sample 1

Sentence:
```
[ X , ~
```
Stack:
```
use_file: LBRACKET expr COMMA TILDE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 399

```
reversed_labeled_tuple_body: expr COMMA TILDE LPAREN . LIDENT type_constraint RPAREN
reversed_labeled_tuple_body: TILDE LPAREN . LIDENT _*
```

## Sample 1

Sentence:
```
[ X , ~ (
```
Stack:
```
use_file: LBRACKET expr COMMA TILDE LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 400

```
reversed_labeled_tuple_body: expr COMMA TILDE LPAREN LIDENT . type_constraint RPAREN
reversed_labeled_tuple_body: TILDE LPAREN LIDENT . type_constraint _*
```

## Sample 1

Sentence:
```
[ X , ~ ( x
```
Stack:
```
use_file: LBRACKET expr COMMA TILDE LPAREN LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 401

```
reversed_labeled_tuple_body: expr COMMA TILDE LPAREN LIDENT type_constraint . RPAREN
reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint . RPAREN _*
```

## Sample 1

Sentence:
```
[ X , ~ ( x : {%%ext|s|} :> {%%ext|s|} [@ and ]
```
Stack:
```
use_file: LBRACKET expr COMMA TILDE LPAREN LIDENT COLON core_type COLONGREATER core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 402

```
reversed_labeled_tuple_body: expr COMMA LABEL . simple_expr
reversed_labeled_tuple_body: LABEL . simple_expr _*
```

## Sample 1

Sentence:
```
[ X , ~label:
```
Stack:
```
use_file: LBRACKET expr COMMA LABEL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy [@ [@@ [@@@ [> [< [%% < <- let let* match >. method - -. -> module mutable nonrec of open ?label: or % + +. += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type _ val virtual when while with
```

# Pattern 403

```
fun_expr: additive . expr
```

## Sample 1

Sentence:
```
X . ( +
```
Stack:
```
use_file: mod_longident DOT LPAREN PLUS
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 404

```
fun_expr: fun_expr PLUSDOT . expr
```

## Sample 1

Sentence:
```
X +.
```
Stack:
```
use_file: fun_expr PLUSDOT
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 405

```
fun_expr: fun_expr PLUS . expr
```

## Sample 1

Sentence:
```
X +
```
Stack:
```
use_file: fun_expr PLUS
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 406

```
fun_expr: fun_expr PERCENT . expr
```

## Sample 1

Sentence:
```
X %
```
Stack:
```
use_file: fun_expr PERCENT
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 407

```
fun_expr: fun_expr OR . expr
```

## Sample 1

Sentence:
```
X or
```
Stack:
```
use_file: fun_expr OR
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 408

```
fun_expr: fun_expr MINUSDOT . expr
```

## Sample 1

Sentence:
```
X -.
```
Stack:
```
use_file: fun_expr MINUSDOT
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 409

```
fun_expr: fun_expr MINUS . expr
```

## Sample 1

Sentence:
```
X -
```
Stack:
```
use_file: fun_expr MINUS
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 410

```
fun_expr: fun_expr LESS . expr
```

## Sample 1

Sentence:
```
X <
```
Stack:
```
use_file: fun_expr LESS
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 411

```
fun_expr: fun_expr INFIXOP4 . expr
```

## Sample 1

Sentence:
```
X **
```
Stack:
```
use_file: fun_expr INFIXOP4
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 412

```
fun_expr: fun_expr INFIXOP3 . expr
```

## Sample 1

Sentence:
```
X land
```
Stack:
```
use_file: fun_expr INFIXOP3
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 413

```
fun_expr: fun_expr INFIXOP2 . expr
```

## Sample 1

Sentence:
```
X +!
```
Stack:
```
use_file: fun_expr INFIXOP2
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 414

```
fun_expr: fun_expr INFIXOP1 . expr
```

## Sample 1

Sentence:
```
X ^
```
Stack:
```
use_file: fun_expr INFIXOP1
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 415

```
fun_expr: fun_expr INFIXOP0 . expr
```

## Sample 1

Sentence:
```
X !=
```
Stack:
```
use_file: fun_expr INFIXOP0
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 416

```
fun_expr: fun_expr GREATER . expr
```

## Sample 1

Sentence:
```
X >
```
Stack:
```
use_file: fun_expr GREATER
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 417

```
fun_expr: fun_expr EQUAL . expr
```

## Sample 1

Sentence:
```
X =
```
Stack:
```
use_file: fun_expr EQUAL
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 418

```
fun_expr: fun_expr COLONEQUAL . expr
```

## Sample 1

Sentence:
```
X :=
```
Stack:
```
use_file: fun_expr COLONEQUAL
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 419

```
fun_expr: fun_expr COLONCOLON . expr
```

## Sample 1

Sentence:
```
X ::
```
Stack:
```
use_file: fun_expr COLONCOLON
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 420

```
fun_expr: fun_expr BARBAR . expr
```

## Sample 1

Sentence:
```
X ||
```
Stack:
```
use_file: fun_expr BARBAR
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 421

```
fun_expr: fun_expr AMPERSAND . expr
```

## Sample 1

Sentence:
```
X &
```
Stack:
```
use_file: fun_expr AMPERSAND
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 422

```
fun_expr: fun_expr AMPERAMPER . expr
```

## Sample 1

Sentence:
```
X &&
```
Stack:
```
use_file: fun_expr AMPERAMPER
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 423

```
fun_seq_expr: fun_expr SEMI PERCENT . attr_id seq_expr
```

## Sample 1

Sentence:
```
X ; %
```
Stack:
```
use_file: fun_expr SEMI PERCENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and* ` ! | || |] 'a' : :: := :> , (*comment*) (**documentation *) . .. .+  = 1.0 > >} >] # ## != ^ +! land ** 1 ~label: { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let* ( >. .< .~ - -. -> ?label: % + +. += !+ ? ' {%%ext|s|} {%%%%ext|s|} } ] ) ; ;; * "s" ~ _
```

# Pattern 424

```
fun_seq_expr: fun_expr SEMI PERCENT attr_id . seq_expr
```

## Sample 1

Sentence:
```
X ; % and
```
Stack:
```
use_file: fun_expr SEMI PERCENT AND
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 425

```
reversed_labeled_tuple_body: expr . COMMA _*
```

## Sample 1

Sentence:
```
let* x false = function | false -> .
```
Stack:
```
use_file: LETOP val_ident fun_params option_type_constraint_ EQUAL FUNCTION ext list_attribute_ BAR pattern MINUSGREATER DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 426

```
and_let_binding: AND list_attribute_ . let_binding_body list_post_item_attribute_
```

## Sample 1

Sentence:
```
let x and
```
Stack:
```
parse_expression: let_bindings_ext_ AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

## Sample 2

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 427

```
strict_binding: EQUAL . seq_expr
```

## Sample 1

Sentence:
```
let* x =
```
Stack:
```
use_file: LETOP val_ident EQUAL
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 428

```
let_binding_body_no_punning: val_ident COLON . reversed_nonempty_llist_typevar_ DOT core_type EQUAL seq_expr
let_binding_body_no_punning: val_ident COLON . TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL seq_expr
type_constraint: COLON . core_type _*
```

## Sample 1

Sentence:
```
let x and x :
```
Stack:
```
parse_expression: let_bindings_ext_ AND list_attribute_ val_ident COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try val virtual when while with
```

# Pattern 429

```
let_binding_body_no_punning: val_ident COLON TYPE . nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL seq_expr
```

## Sample 1

Sentence:
```
let x and x : type
```
Stack:
```
parse_expression: let_bindings_ext_ AND list_attribute_ val_ident COLON TYPE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 430

```
let_binding_body_no_punning: val_ident COLON TYPE nonempty_list_mkrhs_LIDENT__ . DOT core_type EQUAL seq_expr
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 431

```
let_binding_body_no_punning: val_ident COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT . core_type EQUAL seq_expr
```

## Sample 1

Sentence:
```
let x and x : type x .
```
Stack:
```
parse_expression: let_bindings_ext_ AND list_attribute_ val_ident COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 432

```
let_binding_body_no_punning: val_ident COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type . EQUAL seq_expr
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 433

```
let_binding_body_no_punning: val_ident COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL . seq_expr
```

## Sample 1

Sentence:
```
let x and x : type x . {%%ext|s|} =
```
Stack:
```
parse_expression: let_bindings_ext_ AND list_attribute_ val_ident COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 434

```
let_binding_body_no_punning: val_ident COLON reversed_nonempty_llist_typevar_ . DOT core_type EQUAL seq_expr
```

## Sample 1

Sentence:
```
let x and x : ' x
```
Stack:
```
parse_expression: let_bindings_ext_ AND list_attribute_ val_ident COLON QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 435

```
let_binding_body_no_punning: val_ident COLON reversed_nonempty_llist_typevar_ DOT . core_type EQUAL seq_expr
```

## Sample 1

Sentence:
```
let x and x : ' x .
```
Stack:
```
parse_expression: let_bindings_ext_ AND list_attribute_ val_ident COLON reversed_nonempty_llist_typevar_ DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 436

```
let_binding_body_no_punning: val_ident COLON reversed_nonempty_llist_typevar_ DOT core_type . EQUAL seq_expr
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 437

```
let_binding_body_no_punning: val_ident COLON reversed_nonempty_llist_typevar_ DOT core_type EQUAL . seq_expr
```

## Sample 1

Sentence:
```
let x and x : ' x . {%%ext|s|} =
```
Stack:
```
parse_expression: let_bindings_ext_ AND list_attribute_ val_ident COLON reversed_nonempty_llist_typevar_ DOT core_type EQUAL
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 438

```
let_binding_body_no_punning: val_ident type_constraint . EQUAL seq_expr
```

## Sample 1

Sentence:
```
let x and x : {%%ext|s|} :> {%%ext|s|} [@ and ]
```
Stack:
```
parse_expression: let_bindings_ext_ AND list_attribute_ val_ident COLON core_type COLONGREATER core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 439

```
let_binding_body_no_punning: val_ident type_constraint EQUAL . seq_expr
```

## Sample 1

Sentence:
```
let x and x : {%%ext|s|} =
```
Stack:
```
parse_expression: let_bindings_ext_ AND list_attribute_ val_ident type_constraint EQUAL
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 440

```
strict_binding: fun_params option_type_constraint_ . EQUAL fun_body
```

## Sample 1

Sentence:
```
let x and x false ( type x )
```
Stack:
```
parse_expression: let_bindings_ext_ AND list_attribute_ val_ident reversed_nonempty_concat_fun_param_as_list_ LPAREN TYPE nonempty_list_mkrhs_LIDENT__ RPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Sample 2

Sentence:
```
let* x false : {%%ext|s|} :> {%%ext|s|} [@ and ]
```
Stack:
```
use_file: LETOP val_ident fun_params COLON core_type COLONGREATER core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 441

```
strict_binding: fun_params option_type_constraint_ EQUAL . fun_body
```

## Sample 1

Sentence:
```
let* x false =
```
Stack:
```
use_file: LETOP val_ident fun_params option_type_constraint_ EQUAL
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 442

```
let_binding_body_no_punning: simple_pattern_not_ident COLON . core_type EQUAL seq_expr
```

## Sample 1

Sentence:
```
let x and false :
```
Stack:
```
parse_expression: let_bindings_ext_ AND list_attribute_ simple_pattern_not_ident COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 443

```
let_binding_body_no_punning: simple_pattern_not_ident COLON core_type . EQUAL seq_expr
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 444

```
let_binding_body_no_punning: simple_pattern_not_ident COLON core_type EQUAL . seq_expr
```

## Sample 1

Sentence:
```
let x and false : {%%ext|s|} =
```
Stack:
```
parse_expression: let_bindings_ext_ AND list_attribute_ simple_pattern_not_ident COLON core_type EQUAL
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 445

```
labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn . COMMA _*
let_binding_body_no_punning: pattern_no_exn . EQUAL seq_expr
reversed_labeled_tuple_pattern_pattern_no_exn_: pattern_no_exn . COMMA DOTDOT
```

## Sample 1

Sentence:
```
let false as x
```
Stack:
```
use_file: LET ext list_attribute_ rec_flag pattern_no_exn AS LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! || |] begin 'a' class : := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 446

```
let_binding_body_no_punning: pattern_no_exn EQUAL . seq_expr
```

## Sample 1

Sentence:
```
let x and false =
```
Stack:
```
parse_expression: let_bindings_ext_ AND list_attribute_ pattern_no_exn EQUAL
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 447

```
fun_expr: simple_expr DOTOP LBRACKET . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS expr
simple_expr: simple_expr DOTOP LBRACKET . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET
```

## Sample 1

Sentence:
```
X .+ [
```
Stack:
```
use_file: simple_expr DOTOP LBRACKET
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 448

```
fun_expr: simple_expr DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACKET LESSMINUS expr
simple_expr: simple_expr DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACKET
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 449

```
fun_expr: simple_expr DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS . expr
```

## Sample 1

Sentence:
```
X .+ [ X ] <-
```
Stack:
```
use_file: simple_expr DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 450

```
fun_expr: simple_expr DOTOP LBRACE . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS expr
simple_expr: simple_expr DOTOP LBRACE . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE
```

## Sample 1

Sentence:
```
X .+ {
```
Stack:
```
use_file: simple_expr DOTOP LBRACE
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 451

```
fun_expr: simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACE LESSMINUS expr
simple_expr: simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACE
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 452

```
fun_expr: simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS . expr
```

## Sample 1

Sentence:
```
X .+ { X } <-
```
Stack:
```
use_file: simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 453

```
fun_expr: simple_expr DOT . label_longident LESSMINUS expr
fun_expr: simple_expr DOT . LPAREN seq_expr RPAREN LESSMINUS expr
fun_expr: simple_expr DOT . LBRACE seq_expr RBRACE LESSMINUS expr
fun_expr: simple_expr DOT . LBRACKET seq_expr RBRACKET LESSMINUS expr
fun_expr: simple_expr DOT . mod_longident _*
simple_expr: simple_expr DOT . LPAREN seq_expr RPAREN
simple_expr: simple_expr DOT . LBRACE seq_expr RBRACE
simple_expr: simple_expr DOT . LBRACKET seq_expr RBRACKET
simple_expr: simple_expr DOT . mod_longident _*
simple_expr: simple_expr DOT . label_longident
```

## Sample 1

Sentence:
```
false .
```
Stack:
```
use_file: simple_expr DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 454

```
fun_expr: simple_expr DOT LPAREN . seq_expr RPAREN LESSMINUS expr
simple_expr: simple_expr DOT LPAREN . seq_expr RPAREN
```

## Sample 1

Sentence:
```
false . (
```
Stack:
```
use_file: simple_expr DOT LPAREN
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 455

```
fun_expr: simple_expr DOT LPAREN seq_expr . RPAREN LESSMINUS expr
simple_expr: simple_expr DOT LPAREN seq_expr . RPAREN
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 456

```
fun_expr: simple_expr DOT LPAREN seq_expr RPAREN LESSMINUS . expr
```

## Sample 1

Sentence:
```
false . ( X ) <-
```
Stack:
```
use_file: simple_expr DOT LPAREN seq_expr RPAREN LESSMINUS
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 457

```
fun_expr: simple_expr DOT LBRACKET . seq_expr RBRACKET LESSMINUS expr
simple_expr: simple_expr DOT LBRACKET . seq_expr RBRACKET
```

## Sample 1

Sentence:
```
false . [
```
Stack:
```
use_file: simple_expr DOT LBRACKET
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 458

```
fun_expr: simple_expr DOT LBRACKET seq_expr . RBRACKET LESSMINUS expr
simple_expr: simple_expr DOT LBRACKET seq_expr . RBRACKET
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 459

```
fun_expr: simple_expr DOT LBRACKET seq_expr RBRACKET LESSMINUS . expr
```

## Sample 1

Sentence:
```
false . [ X ] <-
```
Stack:
```
use_file: simple_expr DOT LBRACKET seq_expr RBRACKET LESSMINUS
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 460

```
fun_expr: simple_expr DOT LBRACE . seq_expr RBRACE LESSMINUS expr
simple_expr: simple_expr DOT LBRACE . seq_expr RBRACE
```

## Sample 1

Sentence:
```
false . {
```
Stack:
```
use_file: simple_expr DOT LBRACE
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 461

```
fun_expr: simple_expr DOT LBRACE seq_expr . RBRACE LESSMINUS expr
simple_expr: simple_expr DOT LBRACE seq_expr . RBRACE
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 462

```
fun_expr: simple_expr DOT LBRACE seq_expr RBRACE LESSMINUS . expr
```

## Sample 1

Sentence:
```
false . { X } <-
```
Stack:
```
use_file: simple_expr DOT LBRACE seq_expr RBRACE LESSMINUS
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 463

```
fun_expr: simple_expr DOT mod_longident . DOTOP _*
mk_longident_mod_longident_LIDENT_: mod_longident . DOT LIDENT
mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT
simple_expr: simple_expr DOT mod_longident . DOTOP _*
```

## Sample 1

Sentence:
```
false . X . X
```
Stack:
```
use_file: simple_expr DOT mod_longident DOT UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 464

```
fun_expr: simple_expr DOT mod_longident DOTOP . LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN LESSMINUS expr
fun_expr: simple_expr DOT mod_longident DOTOP . LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS expr
fun_expr: simple_expr DOT mod_longident DOTOP . LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS expr
simple_expr: simple_expr DOT mod_longident DOTOP . LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN
simple_expr: simple_expr DOT mod_longident DOTOP . LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE
simple_expr: simple_expr DOT mod_longident DOTOP . LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET
```

## Sample 1

Sentence:
```
false . X .+
```
Stack:
```
use_file: simple_expr DOT mod_longident DOTOP
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 465

```
fun_expr: simple_expr DOT mod_longident DOTOP LPAREN . separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN LESSMINUS expr
simple_expr: simple_expr DOT mod_longident DOTOP LPAREN . separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN
```

## Sample 1

Sentence:
```
false . X .+ (
```
Stack:
```
use_file: simple_expr DOT mod_longident DOTOP LPAREN
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 466

```
fun_expr: simple_expr DOT mod_longident DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ . RPAREN LESSMINUS expr
simple_expr: simple_expr DOT mod_longident DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ . RPAREN
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 467

```
fun_expr: simple_expr DOT mod_longident DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN LESSMINUS . expr
```

## Sample 1

Sentence:
```
false . X .+ ( X ) <-
```
Stack:
```
use_file: simple_expr DOT mod_longident DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN LESSMINUS
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 468

```
fun_expr: simple_expr DOT mod_longident DOTOP LBRACKET . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS expr
simple_expr: simple_expr DOT mod_longident DOTOP LBRACKET . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET
```

## Sample 1

Sentence:
```
false . X .+ [
```
Stack:
```
use_file: simple_expr DOT mod_longident DOTOP LBRACKET
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 469

```
fun_expr: simple_expr DOT mod_longident DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACKET LESSMINUS expr
simple_expr: simple_expr DOT mod_longident DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACKET
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 470

```
fun_expr: simple_expr DOT mod_longident DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS . expr
```

## Sample 1

Sentence:
```
false . X .+ [ X ] <-
```
Stack:
```
use_file: simple_expr DOT mod_longident DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 471

```
fun_expr: simple_expr DOT mod_longident DOTOP LBRACE . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS expr
simple_expr: simple_expr DOT mod_longident DOTOP LBRACE . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE
```

## Sample 1

Sentence:
```
false . X .+ {
```
Stack:
```
use_file: simple_expr DOT mod_longident DOTOP LBRACE
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 472

```
fun_expr: simple_expr DOT mod_longident DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACE LESSMINUS expr
simple_expr: simple_expr DOT mod_longident DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACE
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 473

```
fun_expr: simple_expr DOT mod_longident DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS . expr
```

## Sample 1

Sentence:
```
false . X .+ { X } <-
```
Stack:
```
use_file: simple_expr DOT mod_longident DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 474

```
fun_expr: simple_expr DOT label_longident LESSMINUS . expr
```

## Sample 1

Sentence:
```
false . x <-
```
Stack:
```
use_file: simple_expr DOT label_longident LESSMINUS
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 475

```
labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn . COMMA _*
letop_binding_body: pattern_no_exn . EQUAL seq_expr
reversed_labeled_tuple_pattern_pattern_no_exn_: pattern_no_exn . COMMA DOTDOT
```

## Sample 1

Sentence:
```
let* x and* x as x
```
Stack:
```
use_file: LETOP letop_bindings ANDOP pattern_no_exn AS LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! || |] begin 'a' class : := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 476

```
letop_binding_body: pattern_no_exn EQUAL . seq_expr
```

## Sample 1

Sentence:
```
let* false =
```
Stack:
```
use_file: LETOP pattern_no_exn EQUAL
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 477

```
fun_expr: LETOP letop_bindings . IN seq_expr
```

## Sample 1

Sentence:
```
( let* x and* x
```
Stack:
```
use_file: LPAREN LETOP letop_bindings ANDOP LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 478

```
fun_expr: LETOP letop_bindings IN . seq_expr
```

## Sample 1

Sentence:
```
let* x in
```
Stack:
```
use_file: LETOP letop_bindings IN
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 479

```
letop_bindings: letop_bindings ANDOP . letop_binding_body
```

## Sample 1

Sentence:
```
let* x and*
```
Stack:
```
use_file: LETOP letop_bindings ANDOP
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 480

```
simple_expr: mod_longident DOT LPAREN seq_expr . RPAREN
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 481

```
simple_expr: mod_longident DOT LBRACKETBAR . separated_or_terminated_nonempty_list_SEMI_expr_ BARRBRACKET
simple_expr: mod_longident DOT LBRACKETBAR . BARRBRACKET
```

## Sample 1

Sentence:
```
X . [|
```
Stack:
```
use_file: mod_longident DOT LBRACKETBAR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

# Pattern 482

```
simple_expr: mod_longident DOT LBRACKETBAR separated_or_terminated_nonempty_list_SEMI_expr_ . BARRBRACKET
```

## Sample 1

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
&& & and and* as assert ` ! | || begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 483

```
simple_expr: mod_longident DOT LBRACKET . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET
simple_expr: mod_longident DOT LBRACKET . RBRACKET
```

## Sample 1

Sentence:
```
X . [
```
Stack:
```
use_file: mod_longident DOT LBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } rec ) ; ;; sig * struct then to type _ val virtual when with
```

# Pattern 484

```
simple_expr: mod_longident DOT LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACKET
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 485

```
simple_expr: mod_longident DOT LBRACELESS . separated_or_terminated_nonempty_list_SEMI_object_expr_field_ GREATERRBRACE
```

## Sample 1

Sentence:
```
X . {<
```
Stack:
```
use_file: mod_longident DOT LBRACELESS
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 486

```
simple_expr: mod_longident DOT LBRACELESS separated_or_terminated_nonempty_list_SEMI_object_expr_field_ . GREATERRBRACE
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 487

```
simple_expr: mod_longident DOT LBRACE . record_expr_content RBRACE
```

## Sample 1

Sentence:
```
X . {
```
Stack:
```
use_file: mod_longident DOT LBRACE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy [@ [@@ [@@@ [> [< [%% < <- let let* match >. method - -. -> module mutable nonrec of open ?label: or % + +. += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type _ val virtual when while with
```

# Pattern 488

```
record_expr_content: simple_expr . WITH separated_or_terminated_nonempty_list_SEMI_record_expr_field_
```

## Sample 1

Sentence:
```
X . { object end
```
Stack:
```
use_file: mod_longident DOT LBRACE OBJECT ext list_attribute_ class_self_pattern list_text_cstr_class_field__ END
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. downto effect else end  = exception external false 1.0 for fun function functor > >} >] if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

# Pattern 489

```
record_expr_content: simple_expr WITH . separated_or_terminated_nonempty_list_SEMI_record_expr_field_
```

## Sample 1

Sentence:
```
{ 'a' with
```
Stack:
```
use_file: LBRACE simple_expr WITH
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 490

```
simple_expr: mod_longident DOT LBRACE record_expr_content . RBRACE
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 491

```
constr_longident: mod_longident DOT . LPAREN COLONCOLON RPAREN
mk_longident_mod_longident_LIDENT_: mod_longident DOT . LIDENT
mk_longident_mod_longident_UIDENT_: mod_longident DOT . UIDENT
mk_longident_mod_longident_val_ident_: mod_longident DOT . val_ident
simple_expr: mod_longident DOT . LPAREN seq_expr RPAREN
simple_expr: mod_longident DOT . LBRACELESS separated_or_terminated_nonempty_list_SEMI_object_expr_field_ GREATERRBRACE
simple_expr: mod_longident DOT . LPAREN RPAREN
simple_expr: mod_longident DOT . LBRACE record_expr_content RBRACE
simple_expr: mod_longident DOT . LBRACKETBAR _*
simple_expr: mod_longident DOT . LBRACKET _*
simple_expr: mod_longident DOT . LPAREN MODULE ext list_attribute_ module_expr COLON module_type RPAREN
```

## Sample 1

Sentence:
```
{ X .
```
Stack:
```
use_file: LBRACE mod_longident DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy [@ [@@ [@@@ [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 492

```
fun_expr: FOR ext list_attribute_ pattern EQUAL seq_expr . direction_flag seq_expr DO seq_expr DONE
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ true try type X _ val virtual when while with
```

# Pattern 493

```
fun_expr: FOR ext list_attribute_ pattern EQUAL seq_expr direction_flag . seq_expr DO seq_expr DONE
```

## Sample 1

Sentence:
```
for false = X downto
```
Stack:
```
use_file: FOR ext list_attribute_ pattern EQUAL seq_expr DOWNTO
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 494

```
fun_expr: FOR ext list_attribute_ pattern EQUAL seq_expr direction_flag seq_expr . DO seq_expr DONE
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 495

```
fun_expr: FOR ext list_attribute_ pattern EQUAL seq_expr direction_flag seq_expr DO . seq_expr DONE
```

## Sample 1

Sentence:
```
for false = X downto X do
```
Stack:
```
use_file: FOR ext list_attribute_ pattern EQUAL seq_expr direction_flag seq_expr DO
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 496

```
fun_expr: FOR ext list_attribute_ pattern EQUAL seq_expr direction_flag seq_expr DO seq_expr . DONE
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 497

```
match_case: pattern WHEN seq_expr . MINUSGREATER seq_expr
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 498

```
match_case: pattern WHEN seq_expr MINUSGREATER . seq_expr
```

## Sample 1

Sentence:
```
function false when X ->
```
Stack:
```
use_file: FUNCTION ext list_attribute_ pattern WHEN seq_expr MINUSGREATER
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 499

```
match_case: pattern MINUSGREATER . seq_expr
match_case: pattern MINUSGREATER . DOT
```

## Sample 1

Sentence:
```
function false ->
```
Stack:
```
use_file: FUNCTION ext list_attribute_ pattern MINUSGREATER
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

# Pattern 500

```
fun_expr: IF ext list_attribute_ seq_expr . THEN _*
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct ~ to true try type X _ val virtual when while with
```

# Pattern 501

```
fun_expr: IF ext list_attribute_ seq_expr THEN . expr _*
```

## Sample 1

Sentence:
```
if X then
```
Stack:
```
use_file: IF ext list_attribute_ seq_expr THEN
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 502

```
fun_expr: IF ext list_attribute_ seq_expr THEN expr ELSE . expr
```

## Sample 1

Sentence:
```
if X then X else
```
Stack:
```
use_file: IF ext list_attribute_ seq_expr THEN expr ELSE
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 503

```
simple_expr: simple_expr DOT LPAREN seq_expr . RPAREN
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 504

```
simple_expr: simple_expr DOT LBRACKET . seq_expr RBRACKET
```

## Sample 1

Sentence:
```
{ 'a' . [
```
Stack:
```
use_file: LBRACE simple_expr DOT LBRACKET
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 505

```
simple_expr: simple_expr DOT LBRACKET seq_expr . RBRACKET
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 506

```
simple_expr: simple_expr DOT LBRACE . seq_expr RBRACE
```

## Sample 1

Sentence:
```
{ 'a' . {
```
Stack:
```
use_file: LBRACE simple_expr DOT LBRACE
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 507

```
simple_expr: simple_expr DOT LBRACE seq_expr . RBRACE
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 508

```
mk_longident_mod_longident_LIDENT_: mod_longident . DOT LIDENT
mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT
simple_expr: simple_expr DOT mod_longident . DOTOP _*
```

## Sample 1

Sentence:
```
{ 'a' . X . X
```
Stack:
```
use_file: LBRACE simple_expr DOT mod_longident DOT UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 509

```
simple_expr: simple_expr DOT mod_longident DOTOP . LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN
simple_expr: simple_expr DOT mod_longident DOTOP . LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE
simple_expr: simple_expr DOT mod_longident DOTOP . LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET
```

## Sample 1

Sentence:
```
{ 'a' . X .+
```
Stack:
```
use_file: LBRACE simple_expr DOT mod_longident DOTOP
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 510

```
simple_expr: simple_expr DOT mod_longident DOTOP LPAREN . separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN
```

## Sample 1

Sentence:
```
{ 'a' . X .+ (
```
Stack:
```
use_file: LBRACE simple_expr DOT mod_longident DOTOP LPAREN
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 511

```
simple_expr: simple_expr DOT mod_longident DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ . RPAREN
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 512

```
simple_expr: simple_expr DOT mod_longident DOTOP LBRACKET . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET
```

## Sample 1

Sentence:
```
{ 'a' . X .+ [
```
Stack:
```
use_file: LBRACE simple_expr DOT mod_longident DOTOP LBRACKET
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 513

```
simple_expr: simple_expr DOT mod_longident DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACKET
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 514

```
simple_expr: simple_expr DOT mod_longident DOTOP LBRACE . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE
```

## Sample 1

Sentence:
```
{ 'a' . X .+ {
```
Stack:
```
use_file: LBRACE simple_expr DOT mod_longident DOTOP LBRACE
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 515

```
simple_expr: simple_expr DOT mod_longident DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACE
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 516

```
simple_expr: simple_expr DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ . RPAREN
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 517

```
simple_expr: simple_expr DOTOP LBRACKET . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET
```

## Sample 1

Sentence:
```
{ 'a' .+ [
```
Stack:
```
use_file: LBRACE simple_expr DOTOP LBRACKET
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 518

```
simple_expr: simple_expr DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACKET
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 519

```
simple_expr: simple_expr DOTOP LBRACE . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE
```

## Sample 1

Sentence:
```
{ 'a' .+ {
```
Stack:
```
use_file: LBRACE simple_expr DOTOP LBRACE
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 520

```
simple_expr: simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACE
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 521

```
simple_expr: BEGIN ext list_attribute_ seq_expr . END
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 522

```
simple_expr: LBRACE record_expr_content . RBRACE
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 523

```
simple_expr: LBRACELESS separated_or_terminated_nonempty_list_SEMI_object_expr_field_ . GREATERRBRACE
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 524

```
simple_expr: LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACKET
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 525

```
floating_attribute: LBRACKETATATAT . attr_id attr_payload RBRACKET
```

## Sample 1

Sentence:
```
[@@@
```
Stack:
```
use_file: LBRACKETATATAT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and* ` ! | || |] 'a' : :: := :> , (*comment*) (**documentation *) . .. .+  = 1.0 > >} >] # ## != ^ +! land ** 1 ~label: { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let* ( >. .< .~ - -. -> ?label: % + +. += !+ ? ' {%%ext|s|} {%%%%ext|s|} } ] ) ; ;; * "s" ~ _
```

# Pattern 526

```
structure_item: INCLUDE ext list_attribute_ . module_expr list_post_item_attribute_
```

## Sample 1

Sentence:
```
include
```
Stack:
```
use_file: INCLUDE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ val virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ val virtual when while with
```

# Pattern 527

```
primitive_declaration: EXTERNAL ext list_attribute_ . val_ident COLON possibly_poly_core_type_ EQUAL nonempty_list_raw_string_ list_post_item_attribute_
```

## Sample 1

Sentence:
```
external
```
Stack:
```
use_file: EXTERNAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 528

```
primitive_declaration: EXTERNAL ext list_attribute_ val_ident . COLON possibly_poly_core_type_ EQUAL nonempty_list_raw_string_ list_post_item_attribute_
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 529

```
primitive_declaration: EXTERNAL ext list_attribute_ val_ident COLON . possibly_poly_core_type_ EQUAL nonempty_list_raw_string_ list_post_item_attribute_
```

## Sample 1

Sentence:
```
external x :
```
Stack:
```
use_file: EXTERNAL ext list_attribute_ val_ident COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 530

```
primitive_declaration: EXTERNAL ext list_attribute_ val_ident COLON possibly_poly_core_type_ . EQUAL nonempty_list_raw_string_ list_post_item_attribute_
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 531

```
primitive_declaration: EXTERNAL ext list_attribute_ val_ident COLON possibly_poly_core_type_ EQUAL . nonempty_list_raw_string_ list_post_item_attribute_
```

## Sample 1

Sentence:
```
external x : {%%ext|s|} =
```
Stack:
```
use_file: EXTERNAL ext list_attribute_ val_ident COLON possibly_poly_core_type_ EQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to true try type X _ val virtual when while with
```

# Pattern 532

```
sig_exception_declaration: EXCEPTION ext list_attribute_ . constr_ident generalized_constructor_arguments list_attribute_ list_post_item_attribute_
str_exception_declaration: EXCEPTION ext list_attribute_ . constr_ident EQUAL constr_longident list_attribute_ list_post_item_attribute_
```

## Sample 1

Sentence:
```
exception
```
Stack:
```
use_file: EXCEPTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```

# Pattern 533

```
constr_extra_nonprefix_ident: LPAREN . RPAREN
constr_ident: LPAREN . COLONCOLON RPAREN
```

## Sample 1

Sentence:
```
exception (
```
Stack:
```
interface: EXCEPTION ext list_attribute_ LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 534

```
constr_ident: LPAREN COLONCOLON . RPAREN
```

## Sample 1

Sentence:
```
exception ( ::
```
Stack:
```
interface: EXCEPTION ext list_attribute_ LPAREN COLONCOLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 535

```
constr_extra_nonprefix_ident: LBRACKET . RBRACKET
```

## Sample 1

Sentence:
```
[
```
Stack:
```
parse_constr_longident: LBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 536

```
generalized_constructor_arguments: OF . constructor_arguments
```

## Sample 1

Sentence:
```
exception false of
```
Stack:
```
use_file: EXCEPTION ext list_attribute_ constr_ident OF
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 537

```
label_declaration: mutable_flag . LIDENT COLON possibly_poly_core_type_no_attr_ list_attribute_
label_declaration_semi: mutable_flag . LIDENT COLON possibly_poly_core_type_no_attr_ list_attribute_ SEMI list_attribute_
```

## Sample 1

Sentence:
```
exception false : {
```
Stack:
```
use_file: EXCEPTION ext list_attribute_ constr_ident COLON LBRACE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Sample 2

Sentence:
```
type x := private { mutable
```
Stack:
```
interface: TYPE ext list_attribute_ type_parameters LIDENT COLONEQUAL PRIVATE LBRACE MUTABLE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 538

```
label_declaration: mutable_flag LIDENT . COLON possibly_poly_core_type_no_attr_ list_attribute_
label_declaration_semi: mutable_flag LIDENT . COLON possibly_poly_core_type_no_attr_ list_attribute_ SEMI list_attribute_
```

## Sample 1

Sentence:
```
exception false : { x
```
Stack:
```
use_file: EXCEPTION ext list_attribute_ constr_ident COLON LBRACE mutable_flag LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 539

```
label_declaration: mutable_flag LIDENT COLON . possibly_poly_core_type_no_attr_ list_attribute_
label_declaration_semi: mutable_flag LIDENT COLON . possibly_poly_core_type_no_attr_ list_attribute_ SEMI list_attribute_
```

## Sample 1

Sentence:
```
exception false : { x :
```
Stack:
```
use_file: EXCEPTION ext list_attribute_ constr_ident COLON LBRACE mutable_flag LIDENT COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 540

```
possibly_poly_core_type_no_attr_: reversed_nonempty_llist_typevar_ . DOT alias_type
```

## Sample 1

Sentence:
```
< x : ' x
```
Stack:
```
parse_core_type: LESS LIDENT COLON QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 541

```
possibly_poly_core_type_no_attr_: reversed_nonempty_llist_typevar_ DOT . alias_type
```

## Sample 1

Sentence:
```
< x : ' x .
```
Stack:
```
parse_core_type: LESS LIDENT COLON reversed_nonempty_llist_typevar_ DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 542

```
constructor_arguments: LBRACE label_declarations . RBRACE
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 543

```
constructor_arguments: reversed_separated_nonempty_llist_STAR_atomic_type_ . STAR atomic_type
```

## Sample 1

Sentence:
```
exception false of _
```
Stack:
```
use_file: EXCEPTION ext list_attribute_ constr_ident OF UNDERSCORE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 544

```
constructor_arguments: reversed_separated_nonempty_llist_STAR_atomic_type_ STAR . atomic_type
reversed_separated_nonempty_llist_STAR_atomic_type_: reversed_separated_nonempty_llist_STAR_atomic_type_ STAR . atomic_type
```

## Sample 1

Sentence:
```
exception false : {%%ext|s|} *
```
Stack:
```
use_file: EXCEPTION ext list_attribute_ constr_ident COLON reversed_separated_nonempty_llist_STAR_atomic_type_ STAR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 545

```
str_exception_declaration: EXCEPTION ext list_attribute_ constr_ident EQUAL . constr_longident list_attribute_ list_post_item_attribute_
```

## Sample 1

Sentence:
```
exception false =
```
Stack:
```
use_file: EXCEPTION ext list_attribute_ constr_ident EQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```

# Pattern 546

```
constr_extra_nonprefix_ident: LPAREN . RPAREN
constr_longident: LPAREN . COLONCOLON RPAREN
```

## Sample 1

Sentence:
```
(
```
Stack:
```
parse_constr_longident: LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 547

```
constr_longident: mod_longident DOT . LPAREN COLONCOLON RPAREN
mk_longident_mod_longident_UIDENT_: mod_longident DOT . UIDENT
```

## Sample 1

Sentence:
```
X .
```
Stack:
```
parse_constr_longident: mod_longident DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 548

```
constr_longident: mod_longident DOT LPAREN . COLONCOLON RPAREN
```

## Sample 1

Sentence:
```
X . (
```
Stack:
```
parse_constr_longident: mod_longident DOT LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 549

```
generalized_constructor_arguments: COLON . constructor_arguments MINUSGREATER atomic_type
generalized_constructor_arguments: COLON . reversed_nonempty_llist_typevar_ DOT constructor_arguments MINUSGREATER atomic_type
generalized_constructor_arguments: COLON . atomic_type
generalized_constructor_arguments: COLON . reversed_nonempty_llist_typevar_ DOT atomic_type
```

## Sample 1

Sentence:
```
exception false :
```
Stack:
```
use_file: EXCEPTION ext list_attribute_ constr_ident COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 550

```
generalized_constructor_arguments: COLON reversed_nonempty_llist_typevar_ . DOT _*
```

## Sample 1

Sentence:
```
exception false : ' x
```
Stack:
```
use_file: EXCEPTION ext list_attribute_ constr_ident COLON QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 551

```
generalized_constructor_arguments: COLON reversed_nonempty_llist_typevar_ DOT . constructor_arguments MINUSGREATER atomic_type
generalized_constructor_arguments: COLON reversed_nonempty_llist_typevar_ DOT . atomic_type
```

## Sample 1

Sentence:
```
exception false : ' x .
```
Stack:
```
use_file: EXCEPTION ext list_attribute_ constr_ident COLON reversed_nonempty_llist_typevar_ DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 552

```
generalized_constructor_arguments: COLON reversed_nonempty_llist_typevar_ DOT constructor_arguments . MINUSGREATER atomic_type
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 553

```
generalized_constructor_arguments: COLON reversed_nonempty_llist_typevar_ DOT constructor_arguments MINUSGREATER . atomic_type
```

## Sample 1

Sentence:
```
exception false : ' x . {%%ext|s|} ->
```
Stack:
```
use_file: EXCEPTION ext list_attribute_ constr_ident COLON reversed_nonempty_llist_typevar_ DOT constructor_arguments MINUSGREATER
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 554

```
generalized_constructor_arguments: COLON constructor_arguments . MINUSGREATER atomic_type
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 555

```
generalized_constructor_arguments: COLON constructor_arguments MINUSGREATER . atomic_type
```

## Sample 1

Sentence:
```
exception false : {%%ext|s|} ->
```
Stack:
```
use_file: EXCEPTION ext list_attribute_ constr_ident COLON constructor_arguments MINUSGREATER
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 556

```
open_description: OPEN BANG ext list_attribute_ . mod_ext_longident list_post_item_attribute_
```

## Sample 1

Sentence:
```
open !
```
Stack:
```
interface: OPEN BANG
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 557

```
open_description: OPEN ext list_attribute_ . mod_ext_longident list_post_item_attribute_
```

## Sample 1

Sentence:
```
open
```
Stack:
```
interface: OPEN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 558

```
module_type_declaration: MODULE TYPE ext list_attribute_ . ident option_preceded_EQUAL_module_type__ list_post_item_attribute_
module_type_subst: MODULE TYPE ext list_attribute_ . ident COLONEQUAL module_type list_post_item_attribute_
```

## Sample 1

Sentence:
```
module type
```
Stack:
```
interface: MODULE TYPE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 559

```
module_type_subst: MODULE TYPE ext list_attribute_ ident COLONEQUAL . module_type list_post_item_attribute_
```

## Sample 1

Sentence:
```
module type x :=
```
Stack:
```
interface: MODULE TYPE ext list_attribute_ ident COLONEQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 560

```
module_subst: MODULE ext list_attribute_ . UIDENT COLONEQUAL mod_ext_longident list_post_item_attribute_
signature_item: MODULE ext list_attribute_ . module_name _*
signature_item: MODULE ext list_attribute_ . REC module_name COLON module_type list_post_item_attribute_ list_and_module_declaration_
```

## Sample 1

Sentence:
```
module
```
Stack:
```
interface: MODULE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] ) ; ;; sig * "s" struct then ~ to true try val virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 561

```
module_subst: MODULE ext list_attribute_ UIDENT COLONEQUAL . mod_ext_longident list_post_item_attribute_
```

## Sample 1

Sentence:
```
module X :=
```
Stack:
```
interface: MODULE ext list_attribute_ UIDENT COLONEQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 562

```
signature_item: MODULE ext list_attribute_ REC . module_name COLON module_type list_post_item_attribute_ list_and_module_declaration_
```

## Sample 1

Sentence:
```
module rec
```
Stack:
```
interface: MODULE ext list_attribute_ REC
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 563

```
signature_item: MODULE ext list_attribute_ REC module_name . COLON module_type list_post_item_attribute_ list_and_module_declaration_
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 564

```
signature_item: MODULE ext list_attribute_ REC module_name COLON . module_type list_post_item_attribute_ list_and_module_declaration_
```

## Sample 1

Sentence:
```
module rec X :
```
Stack:
```
interface: MODULE ext list_attribute_ REC module_name COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 565

```
list_and_module_declaration_: AND list_attribute_ . module_name COLON module_type list_post_item_attribute_ list_and_module_declaration_
```

## Sample 1

Sentence:
```
module rec X : {%%ext|s|} and
```
Stack:
```
interface: MODULE ext list_attribute_ REC module_name COLON module_type list_post_item_attribute_ AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 566

```
list_and_module_declaration_: AND list_attribute_ module_name . COLON module_type list_post_item_attribute_ list_and_module_declaration_
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 567

```
list_and_module_declaration_: AND list_attribute_ module_name COLON . module_type list_post_item_attribute_ list_and_module_declaration_
```

## Sample 1

Sentence:
```
module rec X : {%%ext|s|} and X :
```
Stack:
```
interface: MODULE ext list_attribute_ REC module_name COLON module_type list_post_item_attribute_ AND list_attribute_ module_name COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 568

```
signature_item: MODULE ext list_attribute_ module_name . module_declaration_body list_post_item_attribute_
signature_item: MODULE ext list_attribute_ module_name . EQUAL mod_longident list_post_item_attribute_
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 569

```
signature_item: MODULE ext list_attribute_ module_name EQUAL . mod_longident list_post_item_attribute_
```

## Sample 1

Sentence:
```
module X =
```
Stack:
```
interface: MODULE ext list_attribute_ module_name EQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 570

```
module_declaration_body: COLON . module_type
```

## Sample 1

Sentence:
```
module X :
```
Stack:
```
interface: MODULE ext list_attribute_ module_name COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 571

```
module_declaration_body: functor_arg . module_declaration_body
```

## Sample 1

Sentence:
```
module X ( ) ( X : {%%ext|s|} )
```
Stack:
```
interface: MODULE ext list_attribute_ module_name functor_arg LPAREN module_name COLON module_type RPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 572

```
signature_item: INCLUDE ext list_attribute_ . module_type list_post_item_attribute_
```

## Sample 1

Sentence:
```
include
```
Stack:
```
interface: INCLUDE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 573

```
sig_exception_declaration: EXCEPTION ext list_attribute_ . constr_ident generalized_constructor_arguments list_attribute_ list_post_item_attribute_
```

## Sample 1

Sentence:
```
exception
```
Stack:
```
interface: EXCEPTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```

# Pattern 574

```
formal_class_parameters: LBRACKET reversed_separated_nonempty_llist_COMMA_type_parameter_ . RBRACKET
```

## Sample 1

Sentence:
```
class [ _ , _
```
Stack:
```
use_file: CLASS ext list_attribute_ virtual_flag LBRACKET reversed_separated_nonempty_llist_COMMA_type_parameter_ COMMA type_variance UNDERSCORE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 575

```
class_type_declarations: CLASS TYPE ext list_attribute_ virtual_flag formal_class_parameters . LIDENT EQUAL class_signature list_post_item_attribute_ list_and_class_type_declaration_
```

## Sample 1

Sentence:
```
class type
```
Stack:
```
interface: CLASS TYPE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 576

```
class_type_declarations: CLASS TYPE ext list_attribute_ virtual_flag formal_class_parameters LIDENT . EQUAL class_signature list_post_item_attribute_ list_and_class_type_declaration_
```

## Sample 1

Sentence:
```
class type x
```
Stack:
```
interface: CLASS TYPE ext list_attribute_ virtual_flag formal_class_parameters LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 577

```
class_type_declarations: CLASS TYPE ext list_attribute_ virtual_flag formal_class_parameters LIDENT EQUAL . class_signature list_post_item_attribute_ list_and_class_type_declaration_
```

## Sample 1

Sentence:
```
class type x =
```
Stack:
```
interface: CLASS TYPE ext list_attribute_ virtual_flag formal_class_parameters LIDENT EQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [%% < <- let* ( match >. .< .~ method - -. -> module mutable new nonrec of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 578

```
class_self_type: LPAREN . core_type RPAREN
```

## Sample 1

Sentence:
```
class x : object (
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON OBJECT list_attribute_ LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 579

```
class_self_type: LPAREN core_type . RPAREN
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 580

```
class_sig_field: VAL list_attribute_ mutable_virtual_flags . LIDENT COLON core_type list_post_item_attribute_
```

## Sample 1

Sentence:
```
class x : object val
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON OBJECT list_attribute_ class_self_type VAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val when while with
```

## Sample 2

Sentence:
```
class x : object val mutable virtual
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON OBJECT list_attribute_ class_self_type VAL list_attribute_ MUTABLE VIRTUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 581

```
class_sig_field: VAL list_attribute_ mutable_virtual_flags LIDENT . COLON core_type list_post_item_attribute_
```

## Sample 1

Sentence:
```
class x : object val x
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON OBJECT list_attribute_ class_self_type VAL list_attribute_ mutable_virtual_flags LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 582

```
class_sig_field: VAL list_attribute_ mutable_virtual_flags LIDENT COLON . core_type list_post_item_attribute_
```

## Sample 1

Sentence:
```
class x : object val x :
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON OBJECT list_attribute_ class_self_type VAL list_attribute_ mutable_virtual_flags LIDENT COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 583

```
class_sig_field: METHOD list_attribute_ private_virtual_flags . LIDENT COLON possibly_poly_core_type_ list_post_item_attribute_
```

## Sample 1

Sentence:
```
class x : object method
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON OBJECT list_attribute_ class_self_type METHOD
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val when while with
```

## Sample 2

Sentence:
```
class x : object method private virtual
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON OBJECT list_attribute_ class_self_type METHOD list_attribute_ PRIVATE VIRTUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 584

```
class_sig_field: METHOD list_attribute_ private_virtual_flags LIDENT . COLON possibly_poly_core_type_ list_post_item_attribute_
```

## Sample 1

Sentence:
```
class x : object method x
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON OBJECT list_attribute_ class_self_type METHOD list_attribute_ private_virtual_flags LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 585

```
class_sig_field: METHOD list_attribute_ private_virtual_flags LIDENT COLON . possibly_poly_core_type_ list_post_item_attribute_
```

## Sample 1

Sentence:
```
class x : object method x :
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON OBJECT list_attribute_ class_self_type METHOD list_attribute_ private_virtual_flags LIDENT COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 586

```
class_sig_field: INHERIT list_attribute_ . class_signature list_post_item_attribute_
```

## Sample 1

Sentence:
```
class x : object inherit
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON OBJECT list_attribute_ class_self_type INHERIT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@@ [@@@ [| [> [< [%% < <- let* ( match >. .< .~ method - -. -> module mutable new nonrec of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [%% < <- let* ( match >. .< .~ method - -. -> module mutable new nonrec of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 587

```
class_signature: LET . OPEN _*
```

## Sample 1

Sentence:
```
class x : let
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON LET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 588

```
class_signature: LET OPEN BANG list_attribute_ . mod_longident IN class_signature
```

## Sample 1

Sentence:
```
class x : let open !
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON LET OPEN BANG
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 589

```
class_signature: LET OPEN BANG list_attribute_ mod_longident . IN class_signature
mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT
```

## Sample 1

Sentence:
```
class x : let open ! X . X
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON LET OPEN BANG list_attribute_ mod_longident DOT UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 590

```
class_signature: LET OPEN BANG list_attribute_ mod_longident IN . class_signature
```

## Sample 1

Sentence:
```
class x : let open ! X in
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON LET OPEN BANG list_attribute_ mod_longident IN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [%% < <- let* ( match >. .< .~ method - -. -> module mutable new nonrec of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 591

```
class_signature: LBRACKET . reversed_separated_nonempty_llist_COMMA_core_type_ RBRACKET clty_longident
```

## Sample 1

Sentence:
```
class type x = [
```
Stack:
```
interface: CLASS TYPE ext list_attribute_ virtual_flag formal_class_parameters LIDENT EQUAL LBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 592

```
class_signature: LBRACKET reversed_separated_nonempty_llist_COMMA_core_type_ . RBRACKET clty_longident
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 593

```
class_signature: LBRACKET reversed_separated_nonempty_llist_COMMA_core_type_ RBRACKET . clty_longident
```

## Sample 1

Sentence:
```
class x : [ {%%ext|s|} ]
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON LBRACKET reversed_separated_nonempty_llist_COMMA_core_type_ RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 594

```
reversed_separated_nonempty_llist_COMMA_core_type_: reversed_separated_nonempty_llist_COMMA_core_type_ COMMA . core_type
```

## Sample 1

Sentence:
```
object inherit [ {%%ext|s|} ,
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ LBRACKET reversed_separated_nonempty_llist_COMMA_core_type_ COMMA
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 595

```
class_signature: LET OPEN list_attribute_ . mod_longident IN class_signature
```

## Sample 1

Sentence:
```
class x : let open
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON LET OPEN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 596

```
class_signature: LET OPEN list_attribute_ mod_longident . IN class_signature
mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT
```

## Sample 1

Sentence:
```
class x : let open X . X
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON LET OPEN list_attribute_ mod_longident DOT UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 597

```
class_signature: LET OPEN list_attribute_ mod_longident IN . class_signature
```

## Sample 1

Sentence:
```
class x : let open X in
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON LET OPEN list_attribute_ mod_longident IN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [%% < <- let* ( match >. .< .~ method - -. -> module mutable new nonrec of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 598

```
class_sig_field: CONSTRAINT list_attribute_ . constrain_field list_post_item_attribute_
```

## Sample 1

Sentence:
```
class x : object constraint
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON OBJECT list_attribute_ class_self_type CONSTRAINT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 599

```
constrain_field: core_type . EQUAL core_type
```

## Sample 1

Sentence:
```
object constraint {%%ext|s|} [@ and ]
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern CONSTRAINT list_attribute_ core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 600

```
constrain_field: core_type EQUAL . core_type
```

## Sample 1

Sentence:
```
object constraint {%%ext|s|} =
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern CONSTRAINT list_attribute_ core_type EQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 601

```
class_signature: OBJECT list_attribute_ class_self_type list_text_csig_class_sig_field__ . END
```

## Sample 1

Sentence:
```
class x : object
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON OBJECT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) do (**documentation *) done . .. .+ downto effect else  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** initializer 1 ~label: lazy { {< [ [@@ [| [> [< [% < <- let let* x match >. .< .~ - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) do (**documentation *) done . .. .+ downto effect else  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** initializer 1 ~label: lazy { {< [ [@ [@@ [| [> [< [% < <- let let* x ( match >. .< .~ - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ virtual when while with
```

## Sample 3

Sentence:
```
class x : object inherit object end
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON OBJECT list_attribute_ class_self_type INHERIT list_attribute_ OBJECT list_attribute_ class_self_type list_text_csig_class_sig_field__ END
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 602

```
list_and_class_type_declaration_: AND list_attribute_ virtual_flag formal_class_parameters . LIDENT EQUAL class_signature list_post_item_attribute_ list_and_class_type_declaration_
```

## Sample 1

Sentence:
```
class type x = x and
```
Stack:
```
interface: CLASS TYPE ext list_attribute_ virtual_flag formal_class_parameters LIDENT EQUAL class_signature list_post_item_attribute_ AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 603

```
list_and_class_type_declaration_: AND list_attribute_ virtual_flag formal_class_parameters LIDENT . EQUAL class_signature list_post_item_attribute_ list_and_class_type_declaration_
```

## Sample 1

Sentence:
```
class type x = x and x
```
Stack:
```
interface: CLASS TYPE ext list_attribute_ virtual_flag formal_class_parameters LIDENT EQUAL class_signature list_post_item_attribute_ AND list_attribute_ virtual_flag formal_class_parameters LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 604

```
list_and_class_type_declaration_: AND list_attribute_ virtual_flag formal_class_parameters LIDENT EQUAL . class_signature list_post_item_attribute_ list_and_class_type_declaration_
```

## Sample 1

Sentence:
```
class type x = x and x =
```
Stack:
```
interface: CLASS TYPE ext list_attribute_ virtual_flag formal_class_parameters LIDENT EQUAL class_signature list_post_item_attribute_ AND list_attribute_ virtual_flag formal_class_parameters LIDENT EQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [%% < <- let* ( match >. .< .~ method - -. -> module mutable new nonrec of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 605

```
signature_item: CLASS ext list_attribute_ virtual_flag formal_class_parameters . LIDENT COLON class_type list_post_item_attribute_ list_and_class_description_
```

## Sample 1

Sentence:
```
class
```
Stack:
```
interface: CLASS
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try X _ val when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 606

```
signature_item: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT . COLON class_type list_post_item_attribute_ list_and_class_description_
```

## Sample 1

Sentence:
```
class x
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 607

```
signature_item: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON . class_type list_post_item_attribute_ list_and_class_description_
```

## Sample 1

Sentence:
```
class x :
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let* match >. .< .~ method - -. -> module mutable new nonrec of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 608

```
class_type: LIDENT COLON . tuple_type MINUSGREATER class_type
```

## Sample 1

Sentence:
```
class x : x :
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON LIDENT COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 609

```
class_type: LIDENT COLON tuple_type . MINUSGREATER class_type
```

## Sample 1

Sentence:
```
class x : x : _
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON LIDENT COLON UNDERSCORE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 610

```
class_type: LIDENT COLON tuple_type MINUSGREATER . class_type
```

## Sample 1

Sentence:
```
class x : x : {%%ext|s|} ->
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON LIDENT COLON tuple_type MINUSGREATER
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let* match >. .< .~ method - -. -> module mutable new nonrec of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 611

```
class_signature: LBRACKET . reversed_separated_nonempty_llist_COMMA_core_type_ RBRACKET clty_longident
delimited_type_supporting_local_open: LBRACKET . tag_field RBRACKET
delimited_type_supporting_local_open: LBRACKET . BAR reversed_separated_nonempty_llist_BAR_row_field_ RBRACKET
delimited_type_supporting_local_open: LBRACKET . row_field BAR reversed_separated_nonempty_llist_BAR_row_field_ RBRACKET
```

## Sample 1

Sentence:
```
class x : [
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON LBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 612

```
class_type: tuple_type . MINUSGREATER class_type
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 613

```
class_type: tuple_type MINUSGREATER . class_type
```

## Sample 1

Sentence:
```
class x : {%%ext|s|} ->
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON tuple_type MINUSGREATER
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let* match >. .< .~ method - -. -> module mutable new nonrec of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 614

```
class_type: optlabel . tuple_type MINUSGREATER class_type
```

## Sample 1

Sentence:
```
class x : ?label:
```
Stack:
```
use_file: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON OPTLABEL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 615

```
class_type: optlabel tuple_type . MINUSGREATER class_type
```

## Sample 1

Sentence:
```
class x : ?label: _
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON optlabel UNDERSCORE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 616

```
class_type: optlabel tuple_type MINUSGREATER . class_type
```

## Sample 1

Sentence:
```
class x : ?label: {%%ext|s|} ->
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON optlabel tuple_type MINUSGREATER
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let* match >. .< .~ method - -. -> module mutable new nonrec of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 617

```
list_and_class_description_: AND list_attribute_ virtual_flag formal_class_parameters . LIDENT COLON class_type list_post_item_attribute_ list_and_class_description_
```

## Sample 1

Sentence:
```
class x : {%%ext|s|} and
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON class_type list_post_item_attribute_ AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 618

```
list_and_class_description_: AND list_attribute_ virtual_flag formal_class_parameters LIDENT . COLON class_type list_post_item_attribute_ list_and_class_description_
```

## Sample 1

Sentence:
```
class x : {%%ext|s|} and x
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON class_type list_post_item_attribute_ AND list_attribute_ virtual_flag formal_class_parameters LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 619

```
list_and_class_description_: AND list_attribute_ virtual_flag formal_class_parameters LIDENT COLON . class_type list_post_item_attribute_ list_and_class_description_
```

## Sample 1

Sentence:
```
class x : {%%ext|s|} and x :
```
Stack:
```
interface: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON class_type list_post_item_attribute_ AND list_attribute_ virtual_flag formal_class_parameters LIDENT COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let* match >. .< .~ method - -. -> module mutable new nonrec of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 620

```
list_generic_and_type_declaration_type_kind__: AND list_attribute_ type_parameters . LIDENT type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_ list_generic_and_type_declaration_type_kind__
```

## Sample 1

Sentence:
```
type x and
```
Stack:
```
interface: generic_type_declaration_nonrec_flag_type_kind_ AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X val virtual when while with
```

## Sample 2

Sentence:
```
type x and ( _ )
```
Stack:
```
interface: generic_type_declaration_nonrec_flag_type_kind_ AND list_attribute_ LPAREN reversed_separated_nonempty_llist_COMMA_type_parameter_ RPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 621

```
list_generic_and_type_declaration_type_subst_kind__: AND list_attribute_ type_parameters . LIDENT COLONEQUAL nonempty_type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_ list_generic_and_type_declaration_type_subst_kind__
```

## Sample 1

Sentence:
```
type x := false and
```
Stack:
```
interface: generic_type_declaration_no_nonrec_flag_type_subst_kind_ AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X val virtual when while with
```

## Sample 2

Sentence:
```
type x := false and ( _ )
```
Stack:
```
interface: generic_type_declaration_no_nonrec_flag_type_subst_kind_ AND list_attribute_ LPAREN reversed_separated_nonempty_llist_COMMA_type_parameter_ RPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 622

```
list_generic_and_type_declaration_type_subst_kind__: AND list_attribute_ type_parameters LIDENT . COLONEQUAL nonempty_type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_ list_generic_and_type_declaration_type_subst_kind__
```

## Sample 1

Sentence:
```
type x := false and x
```
Stack:
```
interface: generic_type_declaration_no_nonrec_flag_type_subst_kind_ AND list_attribute_ type_parameters LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 623

```
list_generic_and_type_declaration_type_subst_kind__: AND list_attribute_ type_parameters LIDENT COLONEQUAL . nonempty_type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_ list_generic_and_type_declaration_type_subst_kind__
```

## Sample 1

Sentence:
```
type x := false and x :=
```
Stack:
```
interface: generic_type_declaration_no_nonrec_flag_type_subst_kind_ AND list_attribute_ type_parameters LIDENT COLONEQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .+ downto effect else end  = exception 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type val virtual when while with
```

# Pattern 624

```
constr_extra_nonprefix_ident: LBRACKET . RBRACKET
delimited_type_supporting_local_open: LBRACKET . tag_field RBRACKET
delimited_type_supporting_local_open: LBRACKET . BAR reversed_separated_nonempty_llist_BAR_row_field_ RBRACKET
delimited_type_supporting_local_open: LBRACKET . row_field BAR reversed_separated_nonempty_llist_BAR_row_field_ RBRACKET
```

## Sample 1

Sentence:
```
type x := [
```
Stack:
```
interface: TYPE ext list_attribute_ type_parameters LIDENT COLONEQUAL LBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 625

```
nonempty_type_kind: LBRACE label_declarations . RBRACE
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 626

```
nonempty_type_kind: EXTERNAL . STRING
```

## Sample 1

Sentence:
```
type x := external
```
Stack:
```
interface: TYPE ext list_attribute_ type_parameters LIDENT COLONEQUAL EXTERNAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to true try type X _ val virtual when while with
```

# Pattern 627

```
generic_constructor_declaration_BAR_: BAR . constr_ident generalized_constructor_arguments list_attribute_
```

## Sample 1

Sentence:
```
type x += |
```
Stack:
```
interface: TYPE ext list_attribute_ type_parameters type_longident PLUSEQ private_flag BAR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```

# Pattern 628

```
nonempty_type_kind: core_type EQUAL . constructor_declarations
nonempty_type_kind: core_type EQUAL . PRIVATE constructor_declarations
nonempty_type_kind: core_type EQUAL . DOTDOT
nonempty_type_kind: core_type EQUAL . PRIVATE DOTDOT
nonempty_type_kind: core_type EQUAL . LBRACE label_declarations RBRACE
nonempty_type_kind: core_type EQUAL . PRIVATE LBRACE label_declarations RBRACE
```

## Sample 1

Sentence:
```
type x := {%%ext|s|} =
```
Stack:
```
interface: TYPE ext list_attribute_ type_parameters LIDENT COLONEQUAL core_type EQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .+ downto effect else end  = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```

# Pattern 629

```
nonempty_type_kind: core_type EQUAL PRIVATE . constructor_declarations
nonempty_type_kind: core_type EQUAL PRIVATE . DOTDOT
nonempty_type_kind: core_type EQUAL PRIVATE . LBRACE label_declarations RBRACE
```

## Sample 1

Sentence:
```
type x := {%%ext|s|} = private
```
Stack:
```
interface: TYPE ext list_attribute_ type_parameters LIDENT COLONEQUAL core_type EQUAL PRIVATE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .+ downto effect else end  = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```

# Pattern 630

```
nonempty_type_kind: core_type EQUAL PRIVATE LBRACE label_declarations . RBRACE
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 631

```
nonempty_type_kind: core_type EQUAL LBRACE label_declarations . RBRACE
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 632

```
local_structure_item: CLASS ext list_attribute_ virtual_flag formal_class_parameters . LIDENT class_fun_binding list_post_item_attribute_ list_and_class_declaration_
```

## Sample 1

Sentence:
```
class
```
Stack:
```
use_file: CLASS
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try X _ val when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 633

```
local_structure_item: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT . class_fun_binding list_post_item_attribute_ list_and_class_declaration_
```

## Sample 1

Sentence:
```
class x
```
Stack:
```
use_file: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open or % +. += !+ private ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 634

```
constr_extra_nonprefix_ident: LPAREN . RPAREN
constr_longident: LPAREN . COLONCOLON RPAREN
simple_param_pattern: LPAREN . pattern COLON reversed_nonempty_llist_typevar_ DOT core_type RPAREN
simple_pattern_not_ident: LPAREN . pattern RPAREN
simple_pattern_not_ident: LPAREN . MODULE _*
simple_pattern_not_ident: LPAREN . pattern COLON core_type RPAREN
val_extra_ident: LPAREN . operator RPAREN
```

## Sample 1

Sentence:
```
class x (
```
Stack:
```
use_file: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
and as assert | |] begin class : :> , (*comment*) constraint do (**documentation *) done . .. downto else end  external for fun function functor >} >] if in include inherit initializer {< [@ [@@ [@@@ [> [< [%% <- let match >. .< .~ method -> mutable new nonrec object of open ?label: private ? ' {%%%%ext|s|} } ] rec ; ;; sig struct then to try type val virtual when while with
```

# Pattern 635

```
class_fun_binding: EQUAL . class_expr
```

## Sample 1

Sentence:
```
class x =
```
Stack:
```
use_file: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT EQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [%% < <- let* match >. .< .~ method - -. -> module mutable new nonrec of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 636

```
value: BANG list_attribute_ mutable_flag . LIDENT _*
```

## Sample 1

Sentence:
```
object val !
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern VAL BANG
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 637

```
value: BANG list_attribute_ mutable_flag LIDENT . EQUAL seq_expr
value: BANG list_attribute_ mutable_flag LIDENT . type_constraint EQUAL seq_expr
```

## Sample 1

Sentence:
```
object val ! x
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern VAL BANG list_attribute_ mutable_flag LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 638

```
value: BANG list_attribute_ mutable_flag LIDENT EQUAL . seq_expr
```

## Sample 1

Sentence:
```
object val ! x =
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern VAL BANG list_attribute_ mutable_flag LIDENT EQUAL
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 639

```
value: BANG list_attribute_ mutable_flag LIDENT type_constraint . EQUAL seq_expr
```

## Sample 1

Sentence:
```
object val ! x : {%%ext|s|} :> {%%ext|s|} [@ and ]
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern VAL BANG list_attribute_ mutable_flag LIDENT COLON core_type COLONGREATER core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 640

```
value: BANG list_attribute_ mutable_flag LIDENT type_constraint EQUAL . seq_expr
```

## Sample 1

Sentence:
```
object val ! x : {%%ext|s|} =
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern VAL BANG list_attribute_ mutable_flag LIDENT type_constraint EQUAL
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 641

```
value: list_attribute_ virtual_with_mutable_flag . LIDENT COLON core_type
```

## Sample 1

Sentence:
```
object val mutable virtual
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern VAL list_attribute_ MUTABLE VIRTUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 642

```
value: list_attribute_ virtual_with_mutable_flag LIDENT . COLON core_type
```

## Sample 1

Sentence:
```
object val virtual x
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern VAL list_attribute_ virtual_with_mutable_flag LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 643

```
value: list_attribute_ virtual_with_mutable_flag LIDENT COLON . core_type
```

## Sample 1

Sentence:
```
object val virtual x :
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern VAL list_attribute_ virtual_with_mutable_flag LIDENT COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 644

```
value: list_attribute_ mutable_flag . LIDENT _*
```

## Sample 1

Sentence:
```
object val
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern VAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 645

```
value: list_attribute_ mutable_flag LIDENT . EQUAL seq_expr
value: list_attribute_ mutable_flag LIDENT . type_constraint EQUAL seq_expr
```

## Sample 1

Sentence:
```
object val x
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern VAL list_attribute_ mutable_flag LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 646

```
value: list_attribute_ mutable_flag LIDENT EQUAL . seq_expr
```

## Sample 1

Sentence:
```
object val x =
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern VAL list_attribute_ mutable_flag LIDENT EQUAL
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 647

```
value: list_attribute_ mutable_flag LIDENT type_constraint . EQUAL seq_expr
```

## Sample 1

Sentence:
```
object val x : {%%ext|s|} :> {%%ext|s|} [@ and ]
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern VAL list_attribute_ mutable_flag LIDENT COLON core_type COLONGREATER core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 648

```
value: list_attribute_ mutable_flag LIDENT type_constraint EQUAL . seq_expr
```

## Sample 1

Sentence:
```
object val x : {%%ext|s|} =
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern VAL list_attribute_ mutable_flag LIDENT type_constraint EQUAL
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 649

```
method_: BANG list_attribute_ private_flag . LIDENT _*
```

## Sample 1

Sentence:
```
object method !
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern METHOD BANG
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 650

```
method_: BANG list_attribute_ private_flag LIDENT . strict_binding
method_: BANG list_attribute_ private_flag LIDENT . COLON _*
```

## Sample 1

Sentence:
```
object method ! x
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern METHOD BANG list_attribute_ private_flag LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open or % +. += !+ private ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 651

```
method_: BANG list_attribute_ private_flag LIDENT COLON . possibly_poly_core_type_ EQUAL seq_expr
method_: BANG list_attribute_ private_flag LIDENT COLON . TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL seq_expr
```

## Sample 1

Sentence:
```
object method ! x :
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern METHOD BANG list_attribute_ private_flag LIDENT COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try val virtual when while with
```

# Pattern 652

```
method_: BANG list_attribute_ private_flag LIDENT COLON TYPE . nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL seq_expr
```

## Sample 1

Sentence:
```
object method ! x : type
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern METHOD BANG list_attribute_ private_flag LIDENT COLON TYPE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 653

```
method_: BANG list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ . DOT core_type EQUAL seq_expr
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 654

```
method_: BANG list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT . core_type EQUAL seq_expr
```

## Sample 1

Sentence:
```
object method ! x : type x .
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern METHOD BANG list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 655

```
method_: BANG list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type . EQUAL seq_expr
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 656

```
method_: BANG list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL . seq_expr
```

## Sample 1

Sentence:
```
object method ! x : type x . {%%ext|s|} =
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern METHOD BANG list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 657

```
method_: BANG list_attribute_ private_flag LIDENT COLON possibly_poly_core_type_ . EQUAL seq_expr
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 658

```
method_: BANG list_attribute_ private_flag LIDENT COLON possibly_poly_core_type_ EQUAL . seq_expr
```

## Sample 1

Sentence:
```
object method ! x : {%%ext|s|} =
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern METHOD BANG list_attribute_ private_flag LIDENT COLON possibly_poly_core_type_ EQUAL
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 659

```
method_: list_attribute_ virtual_with_private_flag . LIDENT COLON possibly_poly_core_type_
```

## Sample 1

Sentence:
```
object method private virtual
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern METHOD list_attribute_ PRIVATE VIRTUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 660

```
method_: list_attribute_ virtual_with_private_flag LIDENT . COLON possibly_poly_core_type_
```

## Sample 1

Sentence:
```
object method virtual x
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern METHOD list_attribute_ virtual_with_private_flag LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 661

```
method_: list_attribute_ virtual_with_private_flag LIDENT COLON . possibly_poly_core_type_
```

## Sample 1

Sentence:
```
object method virtual x :
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern METHOD list_attribute_ virtual_with_private_flag LIDENT COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 662

```
method_: list_attribute_ private_flag . LIDENT _*
```

## Sample 1

Sentence:
```
object method
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern METHOD
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 663

```
method_: list_attribute_ private_flag LIDENT . strict_binding
method_: list_attribute_ private_flag LIDENT . COLON _*
```

## Sample 1

Sentence:
```
object method x
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern METHOD list_attribute_ private_flag LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open or % +. += !+ private ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 664

```
method_: list_attribute_ private_flag LIDENT COLON . possibly_poly_core_type_ EQUAL seq_expr
method_: list_attribute_ private_flag LIDENT COLON . TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL seq_expr
```

## Sample 1

Sentence:
```
object method x :
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern METHOD list_attribute_ private_flag LIDENT COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try val virtual when while with
```

# Pattern 665

```
method_: list_attribute_ private_flag LIDENT COLON TYPE . nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL seq_expr
```

## Sample 1

Sentence:
```
object method x : type
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern METHOD list_attribute_ private_flag LIDENT COLON TYPE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 666

```
method_: list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ . DOT core_type EQUAL seq_expr
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 667

```
method_: list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT . core_type EQUAL seq_expr
```

## Sample 1

Sentence:
```
object method x : type x .
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern METHOD list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 668

```
method_: list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type . EQUAL seq_expr
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 669

```
method_: list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL . seq_expr
```

## Sample 1

Sentence:
```
object method x : type x . {%%ext|s|} =
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern METHOD list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 670

```
method_: list_attribute_ private_flag LIDENT COLON possibly_poly_core_type_ . EQUAL seq_expr
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 671

```
method_: list_attribute_ private_flag LIDENT COLON possibly_poly_core_type_ EQUAL . seq_expr
```

## Sample 1

Sentence:
```
object method x : {%%ext|s|} =
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern METHOD list_attribute_ private_flag LIDENT COLON possibly_poly_core_type_ EQUAL
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 672

```
class_field: INITIALIZER list_attribute_ . seq_expr list_post_item_attribute_
```

## Sample 1

Sentence:
```
object initializer
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INITIALIZER
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

## Sample 2

Sentence:
```
object initializer [@ and ]
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INITIALIZER LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 673

```
class_field: INHERIT BANG list_attribute_ . class_expr option_preceded_AS_mkrhs_LIDENT___ list_post_item_attribute_
```

## Sample 1

Sentence:
```
object inherit !
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT BANG
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@@ [@@@ [| [> [< [%% < <- let* match >. .< .~ method - -. -> module mutable new nonrec of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [%% < <- let* match >. .< .~ method - -. -> module mutable new nonrec of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 674

```
class_simple_expr: LPAREN . class_expr _*
```

## Sample 1

Sentence:
```
object inherit (
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [%% < <- let* match >. .< .~ method - -. -> module mutable new nonrec of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 675

```
class_expr: LET OPEN BANG list_attribute_ . mod_longident IN class_expr
```

## Sample 1

Sentence:
```
object inherit let open !
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ LET OPEN BANG
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 676

```
class_expr: LET OPEN BANG list_attribute_ mod_longident . IN class_expr
mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT
```

## Sample 1

Sentence:
```
object inherit let open ! X . X
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ LET OPEN BANG list_attribute_ mod_longident DOT UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 677

```
class_expr: LET OPEN BANG list_attribute_ mod_longident IN . class_expr
```

## Sample 1

Sentence:
```
object inherit let open ! X in
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ LET OPEN BANG list_attribute_ mod_longident IN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [%% < <- let* match >. .< .~ method - -. -> module mutable new nonrec of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 678

```
class_simple_expr: LBRACKET . reversed_separated_nonempty_llist_COMMA_core_type_ RBRACKET class_longident
```

## Sample 1

Sentence:
```
object inherit [
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ LBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 679

```
class_simple_expr: LBRACKET reversed_separated_nonempty_llist_COMMA_core_type_ . RBRACKET class_longident
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 680

```
class_simple_expr: LBRACKET reversed_separated_nonempty_llist_COMMA_core_type_ RBRACKET . class_longident
```

## Sample 1

Sentence:
```
object inherit [ {%%ext|s|} ]
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ LBRACKET reversed_separated_nonempty_llist_COMMA_core_type_ RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 681

```
class_expr: FUN list_attribute_ . class_fun_def
```

## Sample 1

Sentence:
```
object inherit fun
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ FUN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer lazy {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open or % +. += !+ private ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

## Sample 2

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open or % +. += !+ private ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 682

```
class_fun_def: simple_param_pattern . MINUSGREATER class_expr
class_fun_def: simple_param_pattern . class_fun_def
```

## Sample 1

Sentence:
```
object inherit fun false ? x
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ FUN list_attribute_ simple_param_pattern QUESTION LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. module mutable new nonrec object of open or % +. += !+ private ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 683

```
class_fun_def: simple_param_pattern MINUSGREATER . class_expr
```

## Sample 1

Sentence:
```
object inherit fun false ->
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ FUN list_attribute_ simple_param_pattern MINUSGREATER
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [%% < <- let* match >. .< .~ method - -. -> module mutable new nonrec of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 684

```
class_expr: let_bindings_no_ext_ . IN class_expr
```

## Sample 1

Sentence:
```
class x : {%%ext|s|} = let x [@@ and ]
```
Stack:
```
use_file: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON class_type EQUAL LET list_attribute_ rec_flag let_binding_body LBRACKETATAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 685

```
class_expr: let_bindings_no_ext_ IN . class_expr
```

## Sample 1

Sentence:
```
object inherit let x in
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ let_bindings_no_ext_ IN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [%% < <- let* match >. .< .~ method - -. -> module mutable new nonrec of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 686

```
class_expr: LET OPEN list_attribute_ . mod_longident IN class_expr
```

## Sample 1

Sentence:
```
object inherit let open
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ LET OPEN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 687

```
class_expr: LET OPEN list_attribute_ mod_longident . IN class_expr
mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT
```

## Sample 1

Sentence:
```
object inherit let open X . X
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ LET OPEN list_attribute_ mod_longident DOT UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 688

```
class_expr: LET OPEN list_attribute_ mod_longident IN . class_expr
```

## Sample 1

Sentence:
```
object inherit let open X in
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ LET OPEN list_attribute_ mod_longident IN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [%% < <- let* match >. .< .~ method - -. -> module mutable new nonrec of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 689

```
let_bindings_no_ext_: LET list_attribute_ rec_flag . let_binding_body list_post_item_attribute_
```

## Sample 1

Sentence:
```
object inherit let
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ LET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] ) ; ;; sig * struct then to try type val virtual when while with
```

## Sample 2

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 690

```
class_simple_expr: LPAREN class_expr . RPAREN
class_simple_expr: LPAREN class_expr . COLON class_type RPAREN
```

## Sample 1

Sentence:
```
object inherit ( object end
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ LPAREN OBJECT list_attribute_ class_self_pattern list_text_cstr_class_field__ END
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 691

```
class_simple_expr: LPAREN class_expr COLON . class_type RPAREN
```

## Sample 1

Sentence:
```
object inherit ( x :
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ LPAREN class_expr COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let* match >. .< .~ method - -. -> module mutable new nonrec of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 692

```
class_simple_expr: LPAREN class_expr COLON class_type . RPAREN
```

## Sample 1

Sentence:
```
object inherit ( x : object end
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ LPAREN class_expr COLON OBJECT list_attribute_ class_self_type list_text_csig_class_sig_field__ END
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 693

```
option_preceded_AS_mkrhs_LIDENT___: AS . LIDENT
```

## Sample 1

Sentence:
```
object inherit x as
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ class_expr AS
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 694

```
class_field: INHERIT list_attribute_ . class_expr option_preceded_AS_mkrhs_LIDENT___ list_post_item_attribute_
```

## Sample 1

Sentence:
```
object inherit
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@@ [@@@ [| [> [< [%% < <- let* match >. .< .~ method - -. -> module mutable new nonrec of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [%% < <- let* match >. .< .~ method - -. -> module mutable new nonrec of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 695

```
class_field: CONSTRAINT list_attribute_ . constrain_field list_post_item_attribute_
```

## Sample 1

Sentence:
```
object constraint
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern CONSTRAINT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 696

```
class_simple_expr: OBJECT list_attribute_ class_self_pattern list_text_cstr_class_field__ . END
```

## Sample 1

Sentence:
```
object inherit object
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ OBJECT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) do (**documentation *) done . .. .+ downto effect else  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** 1 ~label: lazy { {< [ [@@ [| [> [< [% < <- let let* x match >. .< .~ - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ virtual when while with
```

## Sample 2

Sentence:
```
object inherit object ( false : {%%ext|s|} )
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ OBJECT list_attribute_ LPAREN pattern COLON core_type RPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) do (**documentation *) done . .. .+ downto effect else  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** 1 ~label: lazy { {< [ [@ [@@ [| [> [< [% < <- let let* x ( match >. .< .~ - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ virtual when while with
```

## Sample 3

Sentence:
```
object inherit object inherit ! x as x
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ OBJECT list_attribute_ class_self_pattern INHERIT BANG list_attribute_ class_expr AS LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 697

```
class_fun_binding: COLON . class_type EQUAL class_expr
```

## Sample 1

Sentence:
```
class x :
```
Stack:
```
use_file: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let* match >. .< .~ method - -. -> module mutable new nonrec of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 698

```
class_fun_binding: COLON class_type . EQUAL class_expr
```

## Sample 1

Sentence:
```
class x : object end
```
Stack:
```
use_file: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON OBJECT list_attribute_ class_self_type list_text_csig_class_sig_field__ END
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 699

```
class_fun_binding: COLON class_type EQUAL . class_expr
```

## Sample 1

Sentence:
```
class x : {%%ext|s|} =
```
Stack:
```
use_file: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON class_type EQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [%% < <- let* match >. .< .~ method - -. -> module mutable new nonrec of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 700

```
class_fun_binding: simple_param_pattern . class_fun_binding
```

## Sample 1

Sentence:
```
class x = x and x ? x
```
Stack:
```
use_file: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT class_fun_binding list_post_item_attribute_ AND list_attribute_ virtual_flag formal_class_parameters LIDENT QUESTION LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open or % +. += !+ private ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 701

```
list_and_class_declaration_: AND list_attribute_ virtual_flag formal_class_parameters . LIDENT class_fun_binding list_post_item_attribute_ list_and_class_declaration_
```

## Sample 1

Sentence:
```
class x = x and
```
Stack:
```
use_file: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT class_fun_binding list_post_item_attribute_ AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 702

```
list_and_class_declaration_: AND list_attribute_ virtual_flag formal_class_parameters LIDENT . class_fun_binding list_post_item_attribute_ list_and_class_declaration_
```

## Sample 1

Sentence:
```
class x = x and x
```
Stack:
```
use_file: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT class_fun_binding list_post_item_attribute_ AND list_attribute_ virtual_flag formal_class_parameters LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open or % +. += !+ private ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 703

```
let_bindings_ext_: LET ext list_attribute_ rec_flag . let_binding_body list_post_item_attribute_
```

## Sample 1

Sentence:
```
let
```
Stack:
```
use_file: LET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [> [< < <- let let* match >. .< .~ method -. -> mutable new nonrec object of ?label: or +. += !+ private ? ' } ] ) ; ;; sig * struct then to try virtual when while with
```

## Sample 2

Sentence:
```
{%%%%ext|s|} let
```
Stack:
```
implementation: structure_item LET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] ) ; ;; sig * struct then to try type val virtual when while with
```

## Sample 3

Sentence:
```
{%%%%ext|s|} let rec
```
Stack:
```
implementation: structure_item LET ext list_attribute_ REC
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 704

```
floating_attribute: LBRACKETATATAT attr_id attr_payload . RBRACKET
```

## Sample 1

Sentence:
```
[@@@ and
```
Stack:
```
use_file: LBRACKETATATAT AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = functor > >} >] # ## in != ^ +! land ** inherit initializer [@ [@@ [> [< < <- >. method -> mutable nonrec of ?label: or % += private ' } rec ) ; sig * struct then to _ virtual when with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 705

```
item_extension: LBRACKETPERCENTPERCENT attr_id payload . RBRACKET
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 706

```
fun_expr: LET ext list_attribute_ local_structure_item . IN seq_expr
```

## Sample 1

Sentence:
```
let type x
```
Stack:
```
use_file: LET ext list_attribute_ TYPE ext list_attribute_ type_parameters LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 707

```
fun_expr: LET ext list_attribute_ local_structure_item IN . seq_expr
```

## Sample 1

Sentence:
```
let {%%%%ext|s|} in
```
Stack:
```
use_file: LET ext list_attribute_ local_structure_item IN
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 708

```
simple_param_pattern: QUESTION LPAREN label_let_pattern option_preceded_EQUAL_seq_expr__ . RPAREN
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 709

```
simple_expr: LBRACKETBAR separated_or_terminated_nonempty_list_SEMI_expr_ . BARRBRACKET
```

## Sample 1

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
&& & and and* as assert ` ! | || begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 710

```
simple_expr: LPAREN MODULE ext list_attribute_ . module_expr _*
```

## Sample 1

Sentence:
```
( module
```
Stack:
```
use_file: LPAREN MODULE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ val virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ val virtual when while with
```

# Pattern 711

```
simple_expr: LPAREN MODULE ext list_attribute_ module_expr . RPAREN
simple_expr: LPAREN MODULE ext list_attribute_ module_expr . COLON module_type RPAREN
```

## Sample 1

Sentence:
```
( module struct end
```
Stack:
```
use_file: LPAREN MODULE ext list_attribute_ STRUCT list_attribute_ structure END
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 712

```
simple_expr: LPAREN MODULE ext list_attribute_ module_expr COLON . module_type RPAREN
```

## Sample 1

Sentence:
```
( module {%%ext|s|} :
```
Stack:
```
use_file: LPAREN MODULE ext list_attribute_ module_expr COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 713

```
simple_expr: LPAREN MODULE ext list_attribute_ module_expr COLON module_type . RPAREN
```

## Sample 1

Sentence:
```
( module {%%ext|s|} : sig end
```
Stack:
```
use_file: LPAREN MODULE ext list_attribute_ module_expr COLON SIG list_attribute_ signature END
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

# Pattern 714

```
simple_expr: LPAREN seq_expr . RPAREN
simple_expr: LPAREN seq_expr . type_constraint RPAREN
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 715

```
simple_expr: LPAREN seq_expr type_constraint . RPAREN
```

## Sample 1

Sentence:
```
( X : {%%ext|s|} :> {%%ext|s|} [@ and ]
```
Stack:
```
use_file: LPAREN seq_expr COLON core_type COLONGREATER core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 716

```
fun_expr: MATCH ext list_attribute_ seq_expr . WITH reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

# Pattern 717

```
fun_expr: MATCH ext list_attribute_ seq_expr WITH . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
```

## Sample 1

Sentence:
```
match X with
```
Stack:
```
use_file: MATCH ext list_attribute_ seq_expr WITH
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 718

```
simple_expr: METAOCAML_BRACKET_OPEN seq_expr . METAOCAML_BRACKET_CLOSE
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 719

```
paren_module_expr: LPAREN VAL list_attribute_ expr_colon_package_type . RPAREN
```

## Sample 1

Sentence:
```
( val X :> sig end
```
Stack:
```
parse_module_expr: LPAREN VAL list_attribute_ expr COLONGREATER SIG list_attribute_ signature END
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 720

```
expr_colon_package_type: expr COLONGREATER . module_type
```

## Sample 1

Sentence:
```
( val X :>
```
Stack:
```
parse_module_expr: LPAREN VAL list_attribute_ expr COLONGREATER
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 721

```
expr_colon_package_type: expr COLON . module_type _*
```

## Sample 1

Sentence:
```
( val X :
```
Stack:
```
parse_module_expr: LPAREN VAL list_attribute_ expr COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 722

```
expr_colon_package_type: expr COLON module_type COLONGREATER . module_type
```

## Sample 1

Sentence:
```
( val X : {%%ext|s|} :>
```
Stack:
```
parse_module_expr: LPAREN VAL list_attribute_ expr COLON module_type COLONGREATER
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 723

```
open_declaration: OPEN ext list_attribute_ . module_expr list_post_item_attribute_
```

## Sample 1

Sentence:
```
open
```
Stack:
```
use_file: OPEN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ val virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ val virtual when while with
```

# Pattern 724

```
module_expr: STRUCT list_attribute_ structure . END
```

## Sample 1

Sentence:
```
struct
```
Stack:
```
parse_module_expr: STRUCT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else  = functor > >} >] # ## in != ^ +! land ** inherit initializer [@@ [> [< < <- >. method -> mutable nonrec of ?label: or % += private ? ' } ] rec ) ; sig * struct then to _ virtual when with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 725

```
simple_pattern_not_ident: LPAREN MODULE ext list_attribute_ module_name COLON module_type . RPAREN
```

## Sample 1

Sentence:
```
( module X : sig end
```
Stack:
```
parse_pattern: LPAREN MODULE ext list_attribute_ module_name COLON SIG list_attribute_ signature END
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

# Pattern 726

```
labeled_tuple_pat_element_list_pattern_: LABEL . simple_pattern _*
labeled_tuple_pat_element_list_pattern_: TILDE LIDENT COMMA LABEL . simple_pattern
reversed_labeled_tuple_pattern_pattern_: LABEL . simple_pattern COMMA DOTDOT
```

## Sample 1

Sentence:
```
~ x , ~label:
```
Stack:
```
parse_pattern: TILDE LIDENT COMMA LABEL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer ~label: lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type val virtual when while with
```

# Pattern 727

```
labeled_tuple_pat_element_list_pattern_: LABEL . simple_pattern _*
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA LABEL . simple_pattern
reversed_labeled_tuple_pattern_pattern_: LABEL . simple_pattern COMMA DOTDOT
```

## Sample 1

Sentence:
```
~ ( x : {%%ext|s|} ) , ~label:
```
Stack:
```
parse_pattern: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA LABEL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer ~label: lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type val virtual when while with
```

# Pattern 728

```
labeled_tuple_pat_element_list_pattern_: TILDE LIDENT . COMMA _*
reversed_labeled_tuple_pattern_pattern_: TILDE LIDENT . COMMA DOTDOT
```

## Sample 1

Sentence:
```
~ x
```
Stack:
```
parse_pattern: TILDE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 729

```
class_self_pattern: LPAREN pattern . RPAREN
class_self_pattern: LPAREN pattern . COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_: pattern . COMMA _*
reversed_labeled_tuple_pattern_pattern_: pattern . COMMA DOTDOT
```

## Sample 1

Sentence:
```
object ( false as x
```
Stack:
```
use_file: OBJECT ext list_attribute_ LPAREN pattern AS LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! || |] begin 'a' class := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 730

```
class_self_pattern: LPAREN pattern COLON . core_type RPAREN
```

## Sample 1

Sentence:
```
object ( false :
```
Stack:
```
use_file: OBJECT ext list_attribute_ LPAREN pattern COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 731

```
class_self_pattern: LPAREN pattern COLON core_type . RPAREN
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 732

```
simple_expr: OBJECT ext list_attribute_ class_self_pattern list_text_cstr_class_field__ . END
```

## Sample 1

Sentence:
```
object
```
Stack:
```
use_file: OBJECT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) do (**documentation *) done . .. .+ downto effect else  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** 1 ~label: lazy { {< [ [@@ [| [> [< [% < <- let let* x match >. .< .~ - -. -> module mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ virtual when while with
```

## Sample 2

Sentence:
```
object ( false : {%%ext|s|} )
```
Stack:
```
use_file: OBJECT ext list_attribute_ LPAREN pattern COLON core_type RPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) do (**documentation *) done . .. .+ downto effect else  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** 1 ~label: lazy { {< [ [@ [@@ [| [> [< [% < <- let let* x ( match >. .< .~ - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ virtual when while with
```

## Sample 3

Sentence:
```
object inherit ! x as x
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT BANG list_attribute_ class_expr AS LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 733

```
reversed_labeled_tuple_body: LABEL . simple_expr _*
reversed_labeled_tuple_body: TILDE LIDENT COMMA LABEL . simple_expr
```

## Sample 1

Sentence:
```
~ x , ~label:
```
Stack:
```
use_file: TILDE LIDENT COMMA LABEL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy [@ [@@ [@@@ [> [< [%% < <- let let* match >. method - -. -> module mutable nonrec of open ?label: or % + +. += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type _ val virtual when while with
```

# Pattern 734

```
reversed_labeled_tuple_body: LABEL . simple_expr _*
reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint RPAREN COMMA LABEL . simple_expr
```

## Sample 1

Sentence:
```
~ ( x : {%%ext|s|} ) , ~label:
```
Stack:
```
use_file: TILDE LPAREN LIDENT type_constraint RPAREN COMMA LABEL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy [@ [@@ [@@@ [> [< [%% < <- let let* match >. method - -. -> module mutable nonrec of open ?label: or % + +. += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type _ val virtual when while with
```

# Pattern 735

```
reversed_labeled_tuple_body: TILDE LIDENT . COMMA _*
```

## Sample 1

Sentence:
```
~ x
```
Stack:
```
use_file: TILDE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 736

```
fun_expr: TRY ext list_attribute_ seq_expr . WITH reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

# Pattern 737

```
fun_expr: TRY ext list_attribute_ seq_expr WITH . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
```

## Sample 1

Sentence:
```
try X with
```
Stack:
```
use_file: TRY ext list_attribute_ seq_expr WITH
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 738

```
post_item_attribute: LBRACKETATAT attr_id attr_payload . RBRACKET
```

## Sample 1

Sentence:
```
X [@@ and
```
Stack:
```
implementation: seq_expr LBRACKETATAT AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = functor > >} >] # ## in != ^ +! land ** inherit initializer [@ [@@ [> [< < <- >. method -> mutable nonrec of ?label: or % += private ' } rec ) ; sig * struct then to _ virtual when with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 739

```
local_structure_item: TYPE ext list_attribute_ type_parameters type_longident . PLUSEQ private_flag reversed_bar_llist_extension_constructor_ list_post_item_attribute_
```

## Sample 1

Sentence:
```
type x
```
Stack:
```
use_file: TYPE ext list_attribute_ type_parameters LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 740

```
local_structure_item: TYPE ext list_attribute_ type_parameters type_longident PLUSEQ private_flag . reversed_bar_llist_extension_constructor_ list_post_item_attribute_
```

## Sample 1

Sentence:
```
type x +=
```
Stack:
```
use_file: TYPE ext list_attribute_ type_parameters type_longident PLUSEQ
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```

# Pattern 741

```
extension_constructor_rebind_BAR_: BAR . constr_ident EQUAL constr_longident list_attribute_
generic_constructor_declaration_BAR_: BAR . constr_ident generalized_constructor_arguments list_attribute_
```

## Sample 1

Sentence:
```
type x += |
```
Stack:
```
use_file: TYPE ext list_attribute_ type_parameters type_longident PLUSEQ private_flag BAR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```

# Pattern 742

```
extension_constructor_rebind_BAR_: BAR constr_ident EQUAL . constr_longident list_attribute_
```

## Sample 1

Sentence:
```
type x += | false =
```
Stack:
```
use_file: TYPE ext list_attribute_ type_parameters type_longident PLUSEQ private_flag BAR constr_ident EQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```

# Pattern 743

```
extension_constructor_rebind_epsilon_: constr_ident EQUAL . constr_longident list_attribute_
```

## Sample 1

Sentence:
```
type x += false =
```
Stack:
```
use_file: TYPE ext list_attribute_ type_parameters type_longident PLUSEQ private_flag constr_ident EQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```

# Pattern 744

```
extension: LBRACKETPERCENT attr_id payload . RBRACKET
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 745

```
meth_list: LIDENT COLON . possibly_poly_core_type_no_attr_ _*
```

## Sample 1

Sentence:
```
< x :
```
Stack:
```
parse_core_type: LESS LIDENT COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 746

```
object_type: LESS meth_list . GREATER
```

## Sample 1

Sentence:
```
< {%%ext|s|} ;
```
Stack:
```
parse_core_type: LESS atomic_type SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 747

```
function_type: LIDENT COLON LPAREN reversed_nonempty_llist_typevar_ . DOT core_type RPAREN MINUSGREATER function_type
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 748

```
function_type: LIDENT COLON LPAREN reversed_nonempty_llist_typevar_ DOT . core_type RPAREN MINUSGREATER function_type
```

## Sample 1

Sentence:
```
x : ( ' x .
```
Stack:
```
parse_core_type: LIDENT COLON LPAREN reversed_nonempty_llist_typevar_ DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 749

```
function_type: LIDENT COLON LPAREN reversed_nonempty_llist_typevar_ DOT core_type . RPAREN MINUSGREATER function_type
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 750

```
function_type: LIDENT COLON LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN . MINUSGREATER function_type
```

## Sample 1

Sentence:
```
x : ( ' x . {%%ext|s|} )
```
Stack:
```
parse_core_type: LIDENT COLON LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 751

```
function_type: LIDENT COLON LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER . function_type
```

## Sample 1

Sentence:
```
x : ( ' x . {%%ext|s|} ) ->
```
Stack:
```
parse_core_type: LIDENT COLON LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 752

```
function_type: LIDENT COLON tuple_type . MINUSGREATER function_type
```

## Sample 1

Sentence:
```
x : _
```
Stack:
```
parse_core_type: LIDENT COLON UNDERSCORE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 753

```
function_type: LIDENT COLON tuple_type MINUSGREATER . function_type
```

## Sample 1

Sentence:
```
x : {%%ext|s|} ->
```
Stack:
```
parse_core_type: LIDENT COLON tuple_type MINUSGREATER
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 754

```
function_type: LIDENT COLON atomic_type STAR . reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ _*
tuple_type: atomic_type STAR . reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_
```

## Sample 1

Sentence:
```
x : {%%ext|s|} *
```
Stack:
```
parse_core_type: LIDENT COLON atomic_type STAR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 755

```
function_type: LIDENT COLON atomic_type STAR reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ MINUSGREATER . function_type
```

## Sample 1

Sentence:
```
x : {%%ext|s|} * {%%ext|s|} ->
```
Stack:
```
parse_core_type: LIDENT COLON atomic_type STAR reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ MINUSGREATER
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 756

```
function_type: LPAREN reversed_nonempty_llist_typevar_ . DOT core_type RPAREN MINUSGREATER function_type
```

## Sample 1

Sentence:
```
type x := ( ' x
```
Stack:
```
interface: TYPE ext list_attribute_ type_parameters LIDENT COLONEQUAL LPAREN QUOTE LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 757

```
function_type: LPAREN reversed_nonempty_llist_typevar_ DOT . core_type RPAREN MINUSGREATER function_type
```

## Sample 1

Sentence:
```
( ' x .
```
Stack:
```
parse_core_type: LPAREN reversed_nonempty_llist_typevar_ DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 758

```
function_type: LPAREN reversed_nonempty_llist_typevar_ DOT core_type . RPAREN MINUSGREATER function_type
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 759

```
function_type: LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN . MINUSGREATER function_type
```

## Sample 1

Sentence:
```
( ' x . {%%ext|s|} )
```
Stack:
```
parse_core_type: LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 760

```
function_type: LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER . function_type
```

## Sample 1

Sentence:
```
( ' x . {%%ext|s|} ) ->
```
Stack:
```
parse_core_type: LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 761

```
nonempty_type_kind: PRIVATE LBRACE label_declarations . RBRACE
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 762

```
generic_type_declaration_no_nonrec_flag_type_subst_kind_: TYPE ext list_attribute_ type_parameters . LIDENT COLONEQUAL nonempty_type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_
generic_type_declaration_nonrec_flag_type_kind_: TYPE ext list_attribute_ type_parameters . LIDENT type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_
signature_item: TYPE ext list_attribute_ type_parameters . type_longident PLUSEQ private_flag reversed_bar_llist_extension_constructor_declaration_ list_post_item_attribute_
```

## Sample 1

Sentence:
```
type
```
Stack:
```
interface: TYPE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method -. -> module mutable new object of open ?label: or +. += private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

## Sample 2

Sentence:
```
type ( _ )
```
Stack:
```
interface: TYPE ext list_attribute_ LPAREN reversed_separated_nonempty_llist_COMMA_type_parameter_ RPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 763

```
generic_type_declaration_no_nonrec_flag_type_subst_kind_: TYPE ext list_attribute_ type_parameters LIDENT COLONEQUAL . nonempty_type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_
```

## Sample 1

Sentence:
```
type x :=
```
Stack:
```
interface: TYPE ext list_attribute_ type_parameters LIDENT COLONEQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .+ downto effect else end  = exception 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type val virtual when while with
```

# Pattern 764

```
signature_item: TYPE ext list_attribute_ type_parameters type_longident . PLUSEQ private_flag reversed_bar_llist_extension_constructor_declaration_ list_post_item_attribute_
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 765

```
signature_item: TYPE ext list_attribute_ type_parameters type_longident PLUSEQ private_flag . reversed_bar_llist_extension_constructor_declaration_ list_post_item_attribute_
```

## Sample 1

Sentence:
```
type x +=
```
Stack:
```
interface: TYPE ext list_attribute_ type_parameters type_longident PLUSEQ
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```

# Pattern 766

```
module_type: SIG list_attribute_ signature . END
```

## Sample 1

Sentence:
```
sig
```
Stack:
```
parse_module_type: SIG
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else  = false 1.0 for fun function functor > >} >] # ## if in != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [| [> [< [% < <- let let* x ( match >. .< .~ method - -. -> mutable new nonrec object of ?label: or % + +. += !+ private ? ' {%%ext|s|} } ] rec ) ; sig * "s" struct then ~ to true try X _ virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 767

```
delimited_type_supporting_local_open: LPAREN MODULE ext list_attribute_ module_type . RPAREN
```

## Sample 1

Sentence:
```
( module sig end
```
Stack:
```
parse_core_type: LPAREN MODULE ext list_attribute_ SIG list_attribute_ signature END
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

# Pattern 768

```
attribute: LBRACKETAT attr_id attr_payload . RBRACKET
```

## Sample 1

Sentence:
```
{%%ext|s|} [@ and
```
Stack:
```
parse_core_type: core_type LBRACKETAT AND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = functor > >} >] # ## in != ^ +! land ** inherit initializer [@ [@@ [> [< < <- >. method -> mutable nonrec of ?label: or % += private ' } rec ) ; sig * struct then to _ virtual when with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 769

```
fun_expr: WHILE ext list_attribute_ . seq_expr DO seq_expr DONE
```

## Sample 1

Sentence:
```
while
```
Stack:
```
use_file: WHILE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type _ val virtual when with
```

## Sample 2

Sentence:
```
while [@ and ]
```
Stack:
```
use_file: WHILE ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 770

```
fun_expr: WHILE ext list_attribute_ seq_expr . DO seq_expr DONE
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 771

```
fun_expr: WHILE ext list_attribute_ seq_expr DO . seq_expr DONE
```

## Sample 1

Sentence:
```
while X do
```
Stack:
```
use_file: WHILE ext list_attribute_ seq_expr DO
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 772

```
fun_expr: WHILE ext list_attribute_ seq_expr DO seq_expr . DONE
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 773

```
implementation: structure . EOF
```

## Sample 1

Sentence:
```

```
Stack:
```
implementation:
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = functor > >} >] # ## in != ^ +! land ** inherit initializer [@ [@@ [> [< < <- >. method -> mutable nonrec of ?label: or % += private ? ' } ] rec ) ; sig * struct then to _ virtual when with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 774

```
interface: signature . EOF
```

## Sample 1

Sentence:
```

```
Stack:
```
interface:
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = false 1.0 for fun function functor > >} >] # ## if in != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [| [> [< [% < <- let let* x ( match >. .< .~ method - -. -> mutable new nonrec object of ?label: or % + +. += !+ private ? ' {%%ext|s|} } ] rec ) ; sig * "s" struct then ~ to true try X _ virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 775

```
parse_any_longident': . parse_any_longident
```

## Sample 1

Sentence:
```

```
Stack:
```
parse_any_longident:
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```

# Pattern 776

```
constr_extra_nonprefix_ident: LPAREN . RPAREN
mk_longident_mod_ext_longident___anonymous_42_: LPAREN . COLONCOLON RPAREN
val_extra_ident: LPAREN . operator RPAREN
```

## Sample 1

Sentence:
```
(
```
Stack:
```
parse_any_longident: LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
and as assert ` | |] begin 'a' class : :> , (*comment*) constraint do (**documentation *) done . .. downto effect else end  exception external false 1.0 for fun function functor >} >] # if in include inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% <- let x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 777

```
mk_longident_mod_ext_longident___anonymous_42_: LPAREN COLONCOLON . RPAREN
```

## Sample 1

Sentence:
```
( ::
```
Stack:
```
parse_any_longident: LPAREN COLONCOLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 778

```
mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident . DOT UIDENT
mk_longident_mod_ext_longident___anonymous_42_: mod_ext_longident . DOT _*
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 779

```
mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident DOT . UIDENT
mk_longident_mod_ext_longident___anonymous_42_: mod_ext_longident DOT . ident
mk_longident_mod_ext_longident___anonymous_42_: mod_ext_longident DOT . LPAREN COLONCOLON RPAREN
mk_longident_mod_ext_longident___anonymous_42_: mod_ext_longident DOT . val_extra_ident
```

## Sample 1

Sentence:
```
X .
```
Stack:
```
parse_any_longident: mod_ext_longident DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 780

```
mk_longident_mod_ext_longident___anonymous_42_: mod_ext_longident DOT LPAREN . COLONCOLON RPAREN
val_extra_ident: LPAREN . operator RPAREN
```

## Sample 1

Sentence:
```
X . (
```
Stack:
```
parse_any_longident: mod_ext_longident DOT LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
and as assert ` | |] begin 'a' class : :> , (*comment*) constraint do (**documentation *) done . .. downto effect else end  exception external false 1.0 for fun function functor >} >] # if in include inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% <- let x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 781

```
mk_longident_mod_ext_longident___anonymous_42_: mod_ext_longident DOT LPAREN COLONCOLON . RPAREN
```

## Sample 1

Sentence:
```
X . ( ::
```
Stack:
```
parse_any_longident: mod_ext_longident DOT LPAREN COLONCOLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 782

```
parse_any_longident: any_longident . EOF
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 783

```
parse_constr_longident': . parse_constr_longident
```

## Sample 1

Sentence:
```

```
Stack:
```
parse_constr_longident:
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```

# Pattern 784

```
parse_constr_longident: constr_longident . EOF
```

## Sample 1

Sentence:
```
X . ( :: )
```
Stack:
```
parse_constr_longident: mod_longident DOT LPAREN COLONCOLON RPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 785

```
parse_core_type': . parse_core_type
```

## Sample 1

Sentence:
```

```
Stack:
```
parse_core_type:
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

# Pattern 786

```
parse_core_type: core_type . EOF
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 787

```
parse_expression': . parse_expression
```

## Sample 1

Sentence:
```

```
Stack:
```
parse_expression:
```
Rejected when looking ahead at the common set of failing lookaheads.

# Pattern 788

```
parse_expression: seq_expr . EOF
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 789

```
parse_mod_ext_longident': . parse_mod_ext_longident
```

## Sample 1

Sentence:
```

```
Stack:
```
parse_mod_ext_longident:
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 790

```
mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident . DOT UIDENT
parse_mod_ext_longident: mod_ext_longident . EOF
```

## Sample 1

Sentence:
```
X ( X )
```
Stack:
```
parse_mod_ext_longident: mod_ext_longident LPAREN mod_ext_longident RPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 791

```
parse_mod_longident': . parse_mod_longident
```

## Sample 1

Sentence:
```

```
Stack:
```
parse_mod_longident:
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 792

```
mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT
parse_mod_longident: mod_longident . EOF
```

## Sample 1

Sentence:
```
X . X
```
Stack:
```
parse_mod_longident: mod_longident DOT UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 793

```
parse_module_expr': . parse_module_expr
```

## Sample 1

Sentence:
```

```
Stack:
```
parse_module_expr:
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ val virtual when while with
```

# Pattern 794

```
parse_module_expr: module_expr . EOF
```

## Sample 1

Sentence:
```
struct end
```
Stack:
```
parse_module_expr: STRUCT list_attribute_ structure END
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 795

```
parse_module_type': . parse_module_type
```

## Sample 1

Sentence:
```

```
Stack:
```
parse_module_type:
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 796

```
parse_module_type: module_type . EOF
```

## Sample 1

Sentence:
```
sig end
```
Stack:
```
parse_module_type: SIG list_attribute_ signature END
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

# Pattern 797

```
parse_mty_longident': . parse_mty_longident
```

## Sample 1

Sentence:
```

```
Stack:
```
parse_mty_longident:
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 798

```
parse_mty_longident: mty_longident . EOF
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 799

```
parse_pattern': . parse_pattern
```

## Sample 1

Sentence:
```

```
Stack:
```
parse_pattern:
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

# Pattern 800

```
labeled_tuple_pat_element_list_pattern_: pattern . COMMA _*
parse_pattern: pattern . EOF
reversed_labeled_tuple_pattern_pattern_: pattern . COMMA DOTDOT
```

## Sample 1

Sentence:
```
false as x
```
Stack:
```
parse_pattern: pattern AS LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* assert ` ! || |] begin 'a' class : := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 801

```
parse_val_longident': . parse_val_longident
```

## Sample 1

Sentence:
```

```
Stack:
```
parse_val_longident:
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 802

```
parse_val_longident: val_longident . EOF
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 803

```
mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT
mk_longident_mod_longident_val_ident_: mod_longident . DOT val_ident
```

## Sample 1

Sentence:
```
X . X
```
Stack:
```
parse_val_longident: mod_longident DOT UIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 804

```
mk_longident_mod_longident_UIDENT_: mod_longident DOT . UIDENT
mk_longident_mod_longident_val_ident_: mod_longident DOT . val_ident
```

## Sample 1

Sentence:
```
X .
```
Stack:
```
parse_val_longident: mod_longident DOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 805

```
toplevel_directive: HASH . ident _*
```

## Sample 1

Sentence:
```

```
Stack:
```
use_file: HASH
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

# Pattern 806

```
toplevel_phrase: toplevel_directive . SEMISEMI
```

## Sample 1

Sentence:
```
 x false
```
Stack:
```
toplevel_phrase: HASH ident FALSE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 807

```
toplevel_phrase: seq_expr list_post_item_attribute_ . SEMISEMI
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 808

```
toplevel_phrase: list_text_str_structure_item__ . SEMISEMI
```

## Sample 1

Sentence:
```

```
Stack:
```
toplevel_phrase:
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = functor > >} >] ## in != ^ +! land ** inherit initializer [@ [@@ [> [< < <- >. method -> mutable nonrec of ?label: or % += private ? ' } ] rec ) ; sig * struct then to _ virtual when with
```

## Sample 2

Sentence:
```
include struct end
```
Stack:
```
toplevel_phrase: INCLUDE ext list_attribute_ STRUCT list_attribute_ structure END
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 809

```
use_file: seq_expr list_post_item_attribute_ list_use_file_element_ . EOF
```

## Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

# Pattern 810

```
use_file: list_use_file_element_ . EOF
```

## Sample 1

Sentence:
```

```
Stack:
```
use_file:
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = functor > >} >] ## in != ^ +! land ** inherit initializer [@ [@@ [> [< < <- >. method -> mutable nonrec of ?label: or % += private ? ' } ] rec ) ; sig * struct then to _ virtual when with
```

## Sample 2

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```
