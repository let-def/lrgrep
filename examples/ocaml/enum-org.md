# Maximal patterns

## Pattern 1

```
implementation': . implementation
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
 = functor > >} >] # ## in != ^ +! land ** inherit initializer [@ [@@ [> [< < <- >. method -> mutable nonrec of ?label: or % += private ? ' } ] rec ) ; sig * struct then to virtual when with
```


## Pattern 2

```
fun_expr: WHILE . ext list_attribute_ seq_expr DO seq_expr DONE
```

### Sample 1

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
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 3

```
ext: PERCENT . attr_id
```

### Sample 1

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
&& & and* ` ! | || |] 'a' : :: := :> , (*comment*) (**documentation *) . .. .+  
 = 1.0 > >} >] # ## != ^ +! land ** 1 ~label: { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let* ( >. .< .~ - -. -> ?label: % + +. += !+ ? ' {%%ext|s|} {%%%%ext|s|} } ] ) ; ;; * "s" ~ _
```


## Pattern 4

```
attr_id: single_attr_id DOT . attr_id
```

### Sample 1

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
&& & and* ` ! | || |] 'a' : :: := :> , (*comment*) (**documentation *) . .. .+  
 = 1.0 > >} >] # ## != ^ +! land ** 1 ~label: { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let* ( >. .< .~ - -. -> ?label: % + +. += !+ ? ' {%%ext|s|} {%%%%ext|s|} } ] ) ; ;; * "s" ~ _
```


## Pattern 5

```
attribute: LBRACKETAT . attr_id attr_payload RBRACKET
```

### Sample 1

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
&& & and* ` ! | || |] 'a' : :: := :> , (*comment*) (**documentation *) . .. .+  
 = 1.0 > >} >] # ## != ^ +! land ** 1 ~label: { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let* ( >. .< .~ - -. -> ?label: % + +. += !+ ? ' {%%ext|s|} {%%%%ext|s|} } ] ) ; ;; * "s" ~ _
```


## Pattern 6

```
attribute: LBRACKETAT attr_id . attr_payload RBRACKET
```

### Sample 1

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
&& & and and* as | || |] :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = functor > >} >] # ## in != ^ +! land ** inherit initializer [@ [@@ [> [< < <- >. method -> mutable nonrec of ?label: or % += private ' } rec ) ; sig * struct then to virtual when with
```

Also covered by these intermediate patterns:
```
attr_id: single_attr_id . _*
single_attr_id: AND .
```

## Pattern 7

```
value_description: VAL . ext list_attribute_ val_ident COLON possibly_poly_core_type_ list_post_item_attribute_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 8

```
value_description: VAL ext list_attribute_ . val_ident COLON possibly_poly_core_type_ list_post_item_attribute_
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 9

```
val_extra_ident: LPAREN . operator RPAREN
```

### Sample 1

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
and as assert ` | |] begin 'a' class : :: :> , (*comment*) constraint do (**documentation *) done . .. downto effect else end  
 exception external false 1.0 for fun function functor >} >] # if in include inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% <- let x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 10

```
operator: DOTOP . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 11

```
operator: DOTOP LPAREN . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 12

```
index_mod: SEMI . DOTDOT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 13

```
operator: DOTOP LPAREN index_mod . _*
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
index_mod: SEMI DOTDOT .
```

## Pattern 14

```
operator: DOTOP LBRACKET . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 15

```
operator: DOTOP LBRACKET index_mod . _*
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
index_mod: SEMI DOTDOT .
```

## Pattern 16

```
operator: DOTOP LBRACE . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 17

```
operator: DOTOP LBRACE index_mod . _*
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
index_mod: SEMI DOTDOT .
```

## Pattern 18

```
val_extra_ident: LPAREN operator . RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
operator: BANG .
simple_expr: BANG . simple_expr
```

## Pattern 19

```
value_description: VAL ext list_attribute_ val_ident . COLON possibly_poly_core_type_ list_post_item_attribute_
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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
val_ident: LIDENT .
```

## Pattern 20

```
value_description: VAL ext list_attribute_ val_ident COLON . possibly_poly_core_type_ list_post_item_attribute_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 21

```
atomic_type: QUOTE . ident
reversed_nonempty_llist_typevar_: QUOTE . ident
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 22

```
optlabel: QUESTION . LIDENT COLON
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 23

```
optlabel: QUESTION LIDENT . COLON
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 24

```
atomic_type: LPAREN . _*
delimited_type_supporting_local_open: LPAREN . _*
function_type: LPAREN . reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER function_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 25

```
delimited_type_supporting_local_open: LPAREN MODULE . ext list_attribute_ module_type RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 26

```
delimited_type_supporting_local_open: LPAREN MODULE ext list_attribute_ . module_type RPAREN
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 27

```
module_type: SIG . list_attribute_ signature END
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else  
 = false 1.0 for fun function functor > >} >] # ## if in != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [| [> [< [% < <- let let* x ( match >. .< .~ method - -. -> mutable new nonrec object of ?label: or % + +. += !+ private ? ' {%%ext|s|} } ] rec ) ; sig * "s" struct then ~ to true try X _ virtual when while with
```


## Pattern 28

```
generic_type_declaration_no_nonrec_flag_type_subst_kind_: TYPE . _*
generic_type_declaration_nonrec_flag_type_kind_: TYPE . _*
signature_item: TYPE . _*
```

### Sample 1

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
&& & and and* as assert ` | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method -. -> module mutable new object of open ?label: or +. += private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


### Sample 2

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
&& & and and* as assert ` | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method -. -> module mutable new object of open ?label: or +. += private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 29

```
generic_type_declaration_no_nonrec_flag_type_subst_kind_: TYPE ext list_attribute_ NONREC . type_parameters LIDENT COLONEQUAL nonempty_type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_
generic_type_declaration_nonrec_flag_type_kind_: TYPE ext list_attribute_ NONREC . type_parameters LIDENT type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_
signature_item: TYPE ext list_attribute_ NONREC . type_parameters type_longident PLUSEQ private_flag reversed_bar_llist_extension_constructor_declaration_ list_post_item_attribute_
```

### Sample 1

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
&& & and and* as assert ` | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


### Sample 2

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
&& & and and* as assert ` | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 30

```
type_parameters: LPAREN . reversed_separated_nonempty_llist_COMMA_type_parameter_ RPAREN
```

### Sample 1

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
&& & and and* as assert ` | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X val virtual when while with
```


## Pattern 31

```
type_parameter: type_variance . type_variable
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X val virtual when while with
```

Also covered by these intermediate patterns:
```
type_variance: BANG MINUS .
```

## Pattern 32

```
type_variable: QUOTE . ident
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 33

```
type_parameters: LPAREN reversed_separated_nonempty_llist_COMMA_type_parameter_ . RPAREN
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
reversed_separated_nonempty_llist_COMMA_type_parameter_: type_parameter .
type_parameter: type_variance type_variable .
type_variable: UNDERSCORE .
```

## Pattern 34

```
generic_type_declaration_no_nonrec_flag_type_subst_kind_: TYPE ext list_attribute_ NONREC type_parameters . LIDENT COLONEQUAL nonempty_type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_
generic_type_declaration_nonrec_flag_type_kind_: TYPE ext list_attribute_ NONREC type_parameters . LIDENT type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_
signature_item: TYPE ext list_attribute_ NONREC type_parameters . type_longident PLUSEQ private_flag reversed_bar_llist_extension_constructor_declaration_ list_post_item_attribute_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

Also covered by these intermediate patterns:
```
type_parameters: LPAREN reversed_separated_nonempty_llist_COMMA_type_parameter_ RPAREN .
```

## Pattern 35

```
type_kind: EQUAL . nonempty_type_kind
```

### Sample 1

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
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .+ downto effect else end  
 = exception 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type val virtual when while with
```


## Pattern 36

```
atomic_type: QUOTE . ident
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 37

```
nonempty_type_kind: PRIVATE . _*
```

### Sample 1

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
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .+ downto effect else end  
 = exception external 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type val virtual when while with
```


## Pattern 38

```
atomic_type: LPAREN . _*
constr_extra_nonprefix_ident: LPAREN . RPAREN
constr_ident: LPAREN . COLONCOLON RPAREN
delimited_type_supporting_local_open: LPAREN . _*
function_type: LPAREN . reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER function_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 39

```
function_type: LIDENT COLON . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 40

```
atomic_type: LPAREN . _*
delimited_type_supporting_local_open: LPAREN . _*
function_type: LIDENT COLON LPAREN . reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER function_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 41

```
object_type: LESS . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .+ downto effect else end  
 = exception external false 1.0 for fun function functor >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 42

```
atomic_type: LPAREN . _*
delimited_type_supporting_local_open: LPAREN . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 43

```
extension: LBRACKETPERCENT . attr_id payload RBRACKET
```

### Sample 1

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
&& & and* ` ! | || |] 'a' : :: := :> , (*comment*) (**documentation *) . .. .+  
 = 1.0 > >} >] # ## != ^ +! land ** 1 ~label: { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let* ( >. .< .~ - -. -> ?label: % + +. += !+ ? ' {%%ext|s|} {%%%%ext|s|} } ] ) ; ;; * "s" ~ _
```


## Pattern 44

```
generic_type_declaration_nonrec_flag_type_kind_: TYPE . _*
local_structure_item: TYPE . _*
```

### Sample 1

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
&& & and and* as assert ` | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method -. -> module mutable new object of open ?label: or +. += private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 45

```
generic_type_declaration_nonrec_flag_type_kind_: TYPE ext list_attribute_ NONREC . type_parameters LIDENT type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_
local_structure_item: TYPE ext list_attribute_ NONREC . type_parameters type_longident PLUSEQ private_flag reversed_bar_llist_extension_constructor_ list_post_item_attribute_
```

### Sample 1

Sentence:
```
type nonrec
```
Stack:
```
use_file: TYPE ext list_attribute_ NONREC
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 46

```
generic_type_declaration_nonrec_flag_type_kind_: TYPE ext list_attribute_ NONREC type_parameters . LIDENT type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_
local_structure_item: TYPE ext list_attribute_ NONREC type_parameters . type_longident PLUSEQ private_flag reversed_bar_llist_extension_constructor_ list_post_item_attribute_
```

### Sample 1

Sentence:
```
type nonrec ( _ )
```
Stack:
```
use_file: TYPE ext list_attribute_ NONREC LPAREN reversed_separated_nonempty_llist_COMMA_type_parameter_ RPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

Also covered by these intermediate patterns:
```
type_parameters: LPAREN reversed_separated_nonempty_llist_COMMA_type_parameter_ RPAREN .
```

## Pattern 47

```
post_item_attribute: LBRACKETATAT . attr_id attr_payload RBRACKET
```

### Sample 1

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
&& & and* ` ! | || |] 'a' : :: := :> , (*comment*) (**documentation *) . .. .+  
 = 1.0 > >} >] # ## != ^ +! land ** 1 ~label: { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let* ( >. .< .~ - -. -> ?label: % + +. += !+ ? ' {%%ext|s|} {%%%%ext|s|} } ] ) ; ;; * "s" ~ _
```


## Pattern 48

```
post_item_attribute: LBRACKETATAT attr_id . attr_payload RBRACKET
```

### Sample 1

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
&& & and and* as | || |] :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = functor > >} >] # ## in != ^ +! land ** inherit initializer [@ [@@ [> [< < <- >. method -> mutable nonrec of ?label: or % += private ' } rec ) ; sig * struct then to virtual when with
```

Also covered by these intermediate patterns:
```
attr_id: single_attr_id . _*
single_attr_id: AND .
```

## Pattern 49

```
fun_expr: TRY . ext list_attribute_ seq_expr WITH reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
```

### Sample 1

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
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 50

```
fun_expr: TRY ext list_attribute_ . seq_expr WITH reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
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
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 51

```
reversed_labeled_tuple_body: TILDE . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 52

```
reversed_labeled_tuple_body: TILDE LPAREN . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 53

```
reversed_labeled_tuple_body: TILDE LPAREN LIDENT . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 54

```
type_constraint: COLONGREATER . core_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 55

```
delimited_type_supporting_local_open: LBRACKETLESS . _*
```

### Sample 1

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
&& & and and* as assert ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 56

```
delimited_type_supporting_local_open: LBRACKETLESS option_BAR_ . _*
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
&& & and and* as assert ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

Also covered by these intermediate patterns:
```
option_BAR_: BAR .
```

## Pattern 57

```
delimited_type_supporting_local_open: LBRACKETGREATER . _*
```

### Sample 1

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
&& & and and* as assert ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 58

```
delimited_type_supporting_local_open: LBRACKETGREATER option_BAR_ . reversed_separated_nonempty_llist_BAR_row_field_ RBRACKET
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
&& & and and* as assert ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

Also covered by these intermediate patterns:
```
option_BAR_: BAR .
```

## Pattern 59

```
delimited_type_supporting_local_open: LBRACKET . _*
```

### Sample 1

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
&& & and and* as assert ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 60

```
atomic_type: HASH . clty_longident
```

### Sample 1

Sentence:
```
#
```
Stack:
```
parse_core_type: HASH
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 61

```
mk_longident_mod_ext_longident_LIDENT_: mod_ext_longident . DOT LIDENT
mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident . DOT UIDENT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
mod_ext_longident: mod_ext_longident LPAREN mod_ext_longident RPAREN .
```

## Pattern 62

```
mod_ext_longident: mod_ext_longident LPAREN . mod_ext_longident RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 63

```
mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident . DOT UIDENT
mod_ext_longident: mod_ext_longident LPAREN mod_ext_longident . RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
mod_ext_longident: mod_ext_longident LPAREN mod_ext_longident RPAREN .
```

## Pattern 64

```
mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident DOT . UIDENT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 65

```
mk_longident_mod_ext_longident_LIDENT_: mod_ext_longident DOT . LIDENT
mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident DOT . UIDENT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 66

```
delimited_type_supporting_local_open: LBRACKET BAR . reversed_separated_nonempty_llist_BAR_row_field_ RBRACKET
```

### Sample 1

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
&& & and and* as assert ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 67

```
name_tag: BACKQUOTE . ident
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 68

```
function_type: tuple_type MINUSGREATER . function_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 69

```
function_type: optlabel . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

Also covered by these intermediate patterns:
```
optlabel: OPTLABEL .
```

## Pattern 70

```
atomic_type: LPAREN . _*
delimited_type_supporting_local_open: LPAREN . _*
function_type: optlabel LPAREN . reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER function_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 71

```
atomic_type: LPAREN reversed_separated_nontrivial_llist_COMMA_core_type_ . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
reversed_separated_nontrivial_llist_COMMA_core_type_: core_type COMMA core_type .
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 72

```
atomic_type: LPAREN reversed_separated_nontrivial_llist_COMMA_core_type_ RPAREN . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 73

```
atomic_type: LPAREN reversed_separated_nontrivial_llist_COMMA_core_type_ RPAREN HASH . clty_longident
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 74

```
reversed_separated_nontrivial_llist_COMMA_core_type_: reversed_separated_nontrivial_llist_COMMA_core_type_ COMMA . core_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 75

```
atomic_type: mod_ext_longident . DOT delimited_type_supporting_local_open
mk_longident_mod_ext_longident_LIDENT_: mod_ext_longident . DOT LIDENT
mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident . DOT UIDENT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
mod_ext_longident: mod_ext_longident LPAREN mod_ext_longident RPAREN .
```

## Pattern 76

```
atomic_type: mod_ext_longident DOT . delimited_type_supporting_local_open
mk_longident_mod_ext_longident_LIDENT_: mod_ext_longident DOT . LIDENT
mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident DOT . UIDENT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 77

```
delimited_type_supporting_local_open: LPAREN . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 78

```
delimited_type_supporting_local_open: LPAREN core_type . RPAREN
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 79

```
tuple_type: atomic_type STAR . reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 80

```
reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_: LIDENT COLON . atomic_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 81

```
atomic_type: atomic_type HASH . clty_longident
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 82

```
reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_: reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ STAR . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 83

```
reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_: reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ STAR LIDENT COLON . atomic_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 84

```
alias_type: alias_type AS . QUOTE ident
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 85

```
alias_type: alias_type AS QUOTE . ident
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 86

```
function_type: optlabel LPAREN reversed_nonempty_llist_typevar_ . DOT core_type RPAREN MINUSGREATER function_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
atomic_type: QUOTE ident .
reversed_nonempty_llist_typevar_: QUOTE ident .
ident: LIDENT .
```

## Pattern 87

```
reversed_nonempty_llist_typevar_: reversed_nonempty_llist_typevar_ QUOTE . ident
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 88

```
function_type: optlabel LPAREN reversed_nonempty_llist_typevar_ DOT . core_type RPAREN MINUSGREATER function_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 89

```
function_type: optlabel LPAREN reversed_nonempty_llist_typevar_ DOT core_type . RPAREN MINUSGREATER function_type
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 90

```
function_type: optlabel LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN . MINUSGREATER function_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 91

```
function_type: optlabel LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER . function_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 92

```
delimited_type_supporting_local_open: LPAREN core_type . RPAREN
reversed_separated_nontrivial_llist_COMMA_core_type_: core_type . COMMA core_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 93

```
reversed_separated_nontrivial_llist_COMMA_core_type_: core_type COMMA . core_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 94

```
function_type: optlabel tuple_type . MINUSGREATER function_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
tuple_type: atomic_type . _*
atomic_type: UNDERSCORE .
```

## Pattern 95

```
function_type: optlabel tuple_type MINUSGREATER . function_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 96

```
delimited_type_supporting_local_open: LBRACKET BAR reversed_separated_nonempty_llist_BAR_row_field_ . RBRACKET
```

### Sample 1

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
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
reversed_separated_nonempty_llist_BAR_row_field_: reversed_separated_nonempty_llist_BAR_row_field_ BAR row_field .
row_field: core_type .
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 97

```
reversed_separated_nonempty_llist_BAR_row_field_: reversed_separated_nonempty_llist_BAR_row_field_ BAR . row_field
```

### Sample 1

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
&& & and and* as assert ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 98

```
tag_field: name_tag OF . opt_ampersand reversed_separated_nonempty_llist_AMPERSAND_core_type_no_attr_ list_attribute_
```

### Sample 1

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
&& and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 99

```
tag_field: name_tag OF opt_ampersand . reversed_separated_nonempty_llist_AMPERSAND_core_type_no_attr_ list_attribute_
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

Also covered by these intermediate patterns:
```
opt_ampersand: AMPERSAND .
```

## Pattern 100

```
reversed_separated_nonempty_llist_AMPERSAND_core_type_no_attr_: reversed_separated_nonempty_llist_AMPERSAND_core_type_no_attr_ AMPERSAND . alias_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 101

```
delimited_type_supporting_local_open: LBRACKET row_field . BAR reversed_separated_nonempty_llist_BAR_row_field_ RBRACKET
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
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
reversed_separated_nonempty_llist_COMMA_core_type_: core_type .
row_field: core_type .
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 102

```
delimited_type_supporting_local_open: LBRACKET row_field BAR . reversed_separated_nonempty_llist_BAR_row_field_ RBRACKET
```

### Sample 1

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
&& & and and* as assert ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 103

```
delimited_type_supporting_local_open: LBRACKET row_field BAR reversed_separated_nonempty_llist_BAR_row_field_ . RBRACKET
```

### Sample 1

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
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
reversed_separated_nonempty_llist_BAR_row_field_: row_field .
row_field: core_type .
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 104

```
delimited_type_supporting_local_open: LBRACKETGREATER option_BAR_ reversed_separated_nonempty_llist_BAR_row_field_ . RBRACKET
```

### Sample 1

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
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
reversed_separated_nonempty_llist_BAR_row_field_: reversed_separated_nonempty_llist_BAR_row_field_ BAR row_field .
row_field: core_type .
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 105

```
delimited_type_supporting_local_open: LBRACKETLESS option_BAR_ reversed_separated_nonempty_llist_BAR_row_field_ . _*
```

### Sample 1

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
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
reversed_separated_nonempty_llist_BAR_row_field_: reversed_separated_nonempty_llist_BAR_row_field_ BAR row_field .
row_field: core_type .
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 106

```
delimited_type_supporting_local_open: LBRACKETLESS option_BAR_ reversed_separated_nonempty_llist_BAR_row_field_ GREATER . reversed_nonempty_llist_name_tag_ RBRACKET
```

### Sample 1

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
&& & and and* as assert ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 107

```
delimited_type_supporting_local_open: LBRACKETLESS option_BAR_ reversed_separated_nonempty_llist_BAR_row_field_ GREATER reversed_nonempty_llist_name_tag_ . RBRACKET
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
&& & and and* as assert ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
reversed_nonempty_llist_name_tag_: name_tag .
name_tag: BACKQUOTE ident .
ident: LIDENT .
```

## Pattern 108

```
type_constraint: COLON . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 109

```
type_constraint: COLON core_type COLONGREATER . core_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 110

```
reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
type_constraint: COLON core_type COLONGREATER core_type .
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 111

```
reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint RPAREN . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 112

```
reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint RPAREN COMMA . _*
```

### Sample 1

Sentence:
```
~ ( x : {%%ext|s|} ) ,
```
Stack:
```
use_file: TILDE LPAREN LIDENT type_constraint RPAREN COMMA
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 113

```
reversed_labeled_tuple_body: TILDE . _*
reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint RPAREN COMMA TILDE . LIDENT
reversed_labeled_tuple_body: TILDE . LPAREN LIDENT type_constraint RPAREN COMMA TILDE LPAREN LIDENT type_constraint RPAREN
reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint RPAREN COMMA TILDE . LPAREN LIDENT type_constraint RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 114

```
reversed_labeled_tuple_body: TILDE LPAREN . _*
reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint RPAREN COMMA TILDE LPAREN . LIDENT type_constraint RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 115

```
reversed_labeled_tuple_body: TILDE LPAREN LIDENT . _*
reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint RPAREN COMMA TILDE LPAREN LIDENT . type_constraint RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 116

```
reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint . _*
reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint RPAREN COMMA TILDE LPAREN LIDENT type_constraint . RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
type_constraint: COLON core_type COLONGREATER core_type .
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 117

```
reversed_labeled_tuple_body: TILDE LIDENT COMMA . _*
```

### Sample 1

Sentence:
```
~ x ,
```
Stack:
```
use_file: TILDE LIDENT COMMA
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 118

```
reversed_labeled_tuple_body: TILDE . _*
reversed_labeled_tuple_body: TILDE LIDENT COMMA TILDE . LIDENT
reversed_labeled_tuple_body: TILDE . LIDENT COMMA TILDE LPAREN LIDENT type_constraint RPAREN
reversed_labeled_tuple_body: TILDE LIDENT COMMA TILDE . LPAREN LIDENT type_constraint RPAREN
reversed_labeled_tuple_body: TILDE . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 119

```
reversed_labeled_tuple_body: TILDE LIDENT COMMA TILDE LPAREN . LIDENT type_constraint RPAREN
reversed_labeled_tuple_body: TILDE LPAREN . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 120

```
reversed_labeled_tuple_body: TILDE LIDENT COMMA TILDE LPAREN LIDENT . type_constraint RPAREN
reversed_labeled_tuple_body: TILDE LPAREN LIDENT . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 121

```
reversed_labeled_tuple_body: TILDE LIDENT COMMA TILDE LPAREN LIDENT type_constraint . RPAREN
reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
type_constraint: COLON core_type COLONGREATER core_type .
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 122

```
simple_expr: PREFIXOP . simple_expr
```

### Sample 1

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
&& & and and* as assert | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy [@ [@@ [@@@ [> [< [%% < <- let let* match >. method - -. -> module mutable nonrec of open ?label: or % + +. += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type _ val virtual when while with
```


## Pattern 123

```
simple_expr: OBJECT . ext list_attribute_ class_self_pattern list_text_cstr_class_field__ END
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) do (**documentation *) done . .. .+ downto effect else  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** 1 ~label: lazy { {< [ [@@ [| [> [< [% < <- let let* x match >. .< .~ - -. -> module mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ virtual when while with
```


## Pattern 124

```
class_self_pattern: LPAREN . _*
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 125

```
labeled_tuple_pat_element_list_pattern_: TILDE . _*
reversed_labeled_tuple_pattern_pattern_: TILDE . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 126

```
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN . _*
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN . LIDENT COLON core_type RPAREN COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 127

```
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT . _*
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 128

```
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON . _*
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON . core_type RPAREN COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 129

```
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type . _*
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 130

```
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN . _*
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN . COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 131

```
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA . _*
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA . DOTDOT
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 132

```
labeled_tuple_pat_element_list_pattern_: TILDE . _*
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE . LIDENT
labeled_tuple_pat_element_list_pattern_: TILDE . LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN LIDENT COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE . LPAREN LIDENT COLON core_type RPAREN
reversed_labeled_tuple_pattern_pattern_: TILDE . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 133

```
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN . _*
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN . LIDENT COLON core_type RPAREN
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN . LIDENT COLON core_type RPAREN COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 134

```
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT . _*
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN LIDENT . COLON core_type RPAREN
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 135

```
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON . _*
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN LIDENT COLON . core_type RPAREN
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON . core_type RPAREN COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 136

```
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type . _*
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 137

```
labeled_tuple_pat_element_list_pattern_: TILDE LIDENT COMMA . _*
reversed_labeled_tuple_pattern_pattern_: TILDE LIDENT COMMA . DOTDOT
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 138

```
labeled_tuple_pat_element_list_pattern_: TILDE . _*
labeled_tuple_pat_element_list_pattern_: TILDE LIDENT COMMA TILDE . LIDENT
labeled_tuple_pat_element_list_pattern_: TILDE . LIDENT COMMA TILDE LPAREN LIDENT COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LIDENT COMMA TILDE . LPAREN LIDENT COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE . _*
reversed_labeled_tuple_pattern_pattern_: TILDE . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 139

```
labeled_tuple_pat_element_list_pattern_: TILDE LIDENT COMMA TILDE LPAREN . LIDENT COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN . _*
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN . LIDENT COLON core_type RPAREN COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 140

```
labeled_tuple_pat_element_list_pattern_: TILDE LIDENT COMMA TILDE LPAREN LIDENT . COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT . _*
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 141

```
labeled_tuple_pat_element_list_pattern_: TILDE LIDENT COMMA TILDE LPAREN LIDENT COLON . core_type RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON . _*
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON . core_type RPAREN COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 142

```
labeled_tuple_pat_element_list_pattern_: TILDE LIDENT COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type . _*
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 143

```
signed_constant: PLUS . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 144

```
signed_constant: MINUS . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 145

```
constr_extra_nonprefix_ident: LPAREN . RPAREN
constr_longident: LPAREN . COLONCOLON RPAREN
simple_pattern_not_ident: LPAREN . _*
val_extra_ident: LPAREN . operator RPAREN
```

### Sample 1

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
and as assert | |] begin class : :> , (*comment*) constraint do (**documentation *) done . .. downto else end  
 external for fun function functor >} >] if in include inherit initializer {< [@ [@@ [@@@ [> [< [%% <- let match >. .< .~ method -> mutable new nonrec object of open ?label: private ? ' {%%%%ext|s|} } ] rec ; ;; sig struct then to try type val virtual when while with
```


## Pattern 146

```
simple_pattern_not_ident: LPAREN MODULE . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 147

```
simple_pattern_not_ident: LPAREN MODULE ext list_attribute_ . _*
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 148

```
simple_pattern_not_ident: LPAREN MODULE ext list_attribute_ module_name . _*
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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
module_name: UIDENT .
```

## Pattern 149

```
simple_pattern_not_ident: LPAREN MODULE ext list_attribute_ module_name COLON . module_type RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 150

```
module_type: MODULE . TYPE OF list_attribute_ module_expr
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try X _ val virtual when while with
```


## Pattern 151

```
module_type: MODULE TYPE . OF list_attribute_ module_expr
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 152

```
module_type: MODULE TYPE OF . list_attribute_ module_expr
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ val virtual when while with
```


## Pattern 153

```
module_type: MODULE TYPE OF list_attribute_ . module_expr
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 154

```
module_expr: STRUCT . list_attribute_ structure END
```

### Sample 1

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
&& & and and* as | || |] : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else  
 = functor > >} >] # ## in != ^ +! land ** inherit initializer [@@ [> [< < <- >. method -> mutable nonrec of ?label: or % += private ? ' } ] rec ) ; sig * struct then to virtual when with
```


## Pattern 155

```
open_declaration: OPEN . _*
```

### Sample 1

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
&& & and and* as assert ` | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ val virtual when while with
```


## Pattern 156

```
open_declaration: OPEN BANG . ext list_attribute_ module_expr list_post_item_attribute_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ val virtual when while with
```


## Pattern 157

```
open_declaration: OPEN BANG ext list_attribute_ . module_expr list_post_item_attribute_
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 158

```
paren_module_expr: LPAREN . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ virtual when while with
```


## Pattern 159

```
paren_module_expr: LPAREN VAL . list_attribute_ expr_colon_package_type RPAREN
```

### Sample 1

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
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 160

```
paren_module_expr: LPAREN VAL list_attribute_ . expr_colon_package_type RPAREN
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
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 161

```
simple_expr: NEW . ext list_attribute_ class_longident
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 162

```
simple_expr: NEW ext list_attribute_ . class_longident
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 163

```
mk_longident_mod_longident_LIDENT_: mod_longident . DOT LIDENT
mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
mod_longident: mk_longident_mod_longident_UIDENT_ .
mk_longident_mod_longident_UIDENT_: mod_longident DOT UIDENT .
```

## Pattern 164

```
mk_longident_mod_longident_LIDENT_: mod_longident DOT . LIDENT
mk_longident_mod_longident_UIDENT_: mod_longident DOT . UIDENT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 165

```
simple_expr: METAOCAML_ESCAPE . simple_expr
```

### Sample 1

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
&& & and and* as assert | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy [@ [@@ [@@@ [> [< [%% < <- let let* match >. method - -. -> module mutable nonrec of open ?label: or % + +. += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type _ val virtual when while with
```


## Pattern 166

```
simple_expr: METAOCAML_BRACKET_OPEN . seq_expr METAOCAML_BRACKET_CLOSE
```

### Sample 1

Sentence:
```
.<
```
Stack:
```
use_file: METAOCAML_BRACKET_OPEN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 167

```
fun_expr: MATCH . ext list_attribute_ seq_expr WITH reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
```

### Sample 1

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
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 168

```
fun_expr: MATCH ext list_attribute_ . seq_expr WITH reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
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
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 169

```
constr_extra_nonprefix_ident: LPAREN . RPAREN
constr_longident: LPAREN . COLONCOLON RPAREN
simple_expr: LPAREN . _*
val_extra_ident: LPAREN . operator RPAREN
```

### Sample 1

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
and as | |] class : :> , (*comment*) constraint do (**documentation *) done . .. downto effect else end  
 exception external functor >} >] # in include inherit initializer [@ [@@ [@@@ [> [< [%% <- >. method -> mutable nonrec of open ?label: private ? ' {%%%%ext|s|} } ] rec ; ;; sig struct then to type val virtual when with
```


## Pattern 170

```
simple_expr: LBRACKETBAR . _*
```

### Sample 1

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
&& & and and* as | || class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 171

```
fun_expr: LIDENT LESSMINUS . _*
```

### Sample 1

Sentence:
```
x <-
```
Stack:
```
use_file: LIDENT LESSMINUS
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 172

```
fun_expr: LETOP . letop_bindings IN seq_expr
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 173

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE . _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 174

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN . _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN . LIDENT COLON core_type RPAREN COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 175

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT . _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 176

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON . _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON . core_type RPAREN COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 177

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 178

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type RPAREN . _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type RPAREN . COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 179

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA . _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA . DOTDOT
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .+ downto effect else end  
 = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 180

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE . _*
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE . LIDENT
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE . LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN LIDENT COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE . LPAREN LIDENT COLON core_type RPAREN
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 181

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN . _*
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN . LIDENT COLON core_type RPAREN
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN . LIDENT COLON core_type RPAREN COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 182

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT . _*
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN LIDENT . COLON core_type RPAREN
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 183

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON . _*
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN LIDENT COLON . core_type RPAREN
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON . core_type RPAREN COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 184

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . _*
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 185

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LIDENT COMMA . _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LIDENT COMMA . DOTDOT
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .+ downto effect else end  
 = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 186

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE . _*
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LIDENT COMMA TILDE . LIDENT
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE . LIDENT COMMA TILDE LPAREN LIDENT COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LIDENT COMMA TILDE . LPAREN LIDENT COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE . _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 187

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LIDENT COMMA TILDE LPAREN . LIDENT COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN . _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN . LIDENT COLON core_type RPAREN COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 188

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LIDENT COMMA TILDE LPAREN LIDENT . COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT . _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 189

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LIDENT COMMA TILDE LPAREN LIDENT COLON . core_type RPAREN
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON . _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON . core_type RPAREN COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 190

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LIDENT COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 191

```
simple_delimited_pattern: LBRACKETBAR . _*
```

### Sample 1

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
&& & and and* as assert ! | || begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 192

```
constr_extra_nonprefix_ident: LBRACKET . RBRACKET
simple_delimited_pattern: LBRACKET . separated_or_terminated_nonempty_list_SEMI_pattern_ RBRACKET
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 193

```
simple_delimited_pattern: LBRACE . listx_SEMI_record_pat_field_UNDERSCORE_ RBRACE
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 194

```
simple_delimited_pattern: LBRACE listx_SEMI_record_pat_field_UNDERSCORE_ . RBRACE
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
listx_SEMI_record_pat_field_UNDERSCORE_: label_longident option_preceded_COLON_core_type__ option_preceded_EQUAL_pattern__ SEMI UNDERSCORE . option_SEMI_
```

## Pattern 195

```
option_preceded_COLON_core_type__: COLON . core_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 196

```
option_preceded_EQUAL_pattern__: EQUAL . pattern
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 197

```
pattern_gen: LAZY . ext list_attribute_ simple_pattern
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer ~label: lazy {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type val virtual when while with
```


## Pattern 198

```
pattern_gen: LAZY ext list_attribute_ . simple_pattern
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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer ~label: lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 199

```
simple_pattern_not_ident: HASH . type_longident
```

### Sample 1

Sentence:
```
#
```
Stack:
```
parse_pattern: HASH
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 200

```
simple_pattern_not_ident: signed_constant DOTDOT . signed_constant
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to true try type X _ val virtual when while with
```


## Pattern 201

```
constr_longident: mod_longident DOT . LPAREN COLONCOLON RPAREN
mk_longident_mod_longident_UIDENT_: mod_longident DOT . UIDENT
simple_pattern_not_ident: mod_longident DOT . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy {< [@ [@@ [@@@ [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 202

```
constr_longident: mod_longident DOT LPAREN . COLONCOLON RPAREN
simple_pattern_not_ident: mod_longident DOT LPAREN . _*
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 203

```
labeled_tuple_pat_element_list_pattern_: LABEL . _*
reversed_labeled_tuple_pattern_pattern_: LABEL . simple_pattern COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer ~label: lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type val virtual when while with
```


## Pattern 204

```
labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern . _*
reversed_labeled_tuple_pattern_pattern_: LABEL simple_pattern . COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
simple_pattern: val_ident .
val_ident: LIDENT .
```

## Pattern 205

```
labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA . _*
reversed_labeled_tuple_pattern_pattern_: LABEL simple_pattern COMMA . DOTDOT
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 206

```
labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE . _*
labeled_tuple_pat_element_list_pattern_: TILDE . _*
reversed_labeled_tuple_pattern_pattern_: TILDE . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 207

```
labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN . LIDENT COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN . _*
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN . LIDENT COLON core_type RPAREN COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 208

```
labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT . COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT . _*
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 209

```
labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON . core_type RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON . _*
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON . core_type RPAREN COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 210

```
labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type . _*
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 211

```
labeled_tuple_pat_element_list_pattern_: LABEL . _*
labeled_tuple_pat_element_list_pattern_: LABEL simple_pattern COMMA LABEL . simple_pattern
labeled_tuple_pat_element_list_pattern_: LABEL . _*
reversed_labeled_tuple_pattern_pattern_: LABEL . simple_pattern COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer ~label: lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type val virtual when while with
```


## Pattern 212

```
pattern: EXCEPTION . ext list_attribute_ pattern
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 213

```
pattern: EXCEPTION ext list_attribute_ . pattern
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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 214

```
pattern: EFFECT . pattern_gen COMMA simple_pattern
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer ~label: {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type val virtual when while with
```


## Pattern 215

```
pattern: EFFECT pattern_gen . COMMA simple_pattern
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
pattern_gen: constr_longident LPAREN TYPE nonempty_list_mkrhs_LIDENT__ RPAREN simple_pattern .
simple_pattern: val_ident .
val_ident: LIDENT .
```

## Pattern 216

```
pattern: EFFECT pattern_gen COMMA . simple_pattern
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer ~label: lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type val virtual when while with
```


## Pattern 217

```
labeled_tuple_pat_element_list_pattern_: pattern COMMA . _*
reversed_labeled_tuple_pattern_pattern_: pattern COMMA . DOTDOT
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 218

```
labeled_tuple_pat_element_list_pattern_: pattern COMMA TILDE . _*
labeled_tuple_pat_element_list_pattern_: TILDE . _*
reversed_labeled_tuple_pattern_pattern_: TILDE . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 219

```
labeled_tuple_pat_element_list_pattern_: pattern COMMA TILDE LPAREN . LIDENT COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN . _*
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN . LIDENT COLON core_type RPAREN COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 220

```
labeled_tuple_pat_element_list_pattern_: pattern COMMA TILDE LPAREN LIDENT . COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT . _*
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 221

```
labeled_tuple_pat_element_list_pattern_: pattern COMMA TILDE LPAREN LIDENT COLON . core_type RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON . _*
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON . core_type RPAREN COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 222

```
labeled_tuple_pat_element_list_pattern_: pattern COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type . _*
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 223

```
labeled_tuple_pat_element_list_pattern_: pattern COMMA LABEL . simple_pattern
labeled_tuple_pat_element_list_pattern_: LABEL . _*
reversed_labeled_tuple_pattern_pattern_: LABEL . simple_pattern COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer ~label: lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type val virtual when while with
```


## Pattern 224

```
pattern: pattern COLONCOLON . pattern
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 225

```
pattern: pattern BAR . pattern
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 226

```
pattern: pattern AS . val_ident
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 227

```
labeled_tuple_pat_element_list_pattern_: labeled_tuple_pat_element_list_pattern_ COMMA . _*
reversed_labeled_tuple_pattern_pattern_: labeled_tuple_pat_element_list_pattern_ COMMA . DOTDOT
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 228

```
labeled_tuple_pat_element_list_pattern_: labeled_tuple_pat_element_list_pattern_ COMMA TILDE . _*
labeled_tuple_pat_element_list_pattern_: TILDE . _*
reversed_labeled_tuple_pattern_pattern_: TILDE . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 229

```
labeled_tuple_pat_element_list_pattern_: labeled_tuple_pat_element_list_pattern_ COMMA TILDE LPAREN . LIDENT COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN . _*
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN . LIDENT COLON core_type RPAREN COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 230

```
labeled_tuple_pat_element_list_pattern_: labeled_tuple_pat_element_list_pattern_ COMMA TILDE LPAREN LIDENT . COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT . _*
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 231

```
labeled_tuple_pat_element_list_pattern_: labeled_tuple_pat_element_list_pattern_ COMMA TILDE LPAREN LIDENT COLON . core_type RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON . _*
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON . core_type RPAREN COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 232

```
labeled_tuple_pat_element_list_pattern_: labeled_tuple_pat_element_list_pattern_ COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type . _*
reversed_labeled_tuple_pattern_pattern_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 233

```
labeled_tuple_pat_element_list_pattern_: labeled_tuple_pat_element_list_pattern_ COMMA LABEL . simple_pattern
labeled_tuple_pat_element_list_pattern_: LABEL . _*
reversed_labeled_tuple_pattern_pattern_: LABEL . simple_pattern COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer ~label: lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type val virtual when while with
```


## Pattern 234

```
constr_extra_nonprefix_ident: LPAREN . RPAREN
constr_longident: LPAREN . COLONCOLON RPAREN
pattern_gen: constr_longident LPAREN . TYPE nonempty_list_mkrhs_LIDENT__ RPAREN simple_pattern
simple_pattern_not_ident: LPAREN . _*
val_extra_ident: LPAREN . operator RPAREN
```

### Sample 1

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
and as assert | |] begin class : :> , (*comment*) constraint do (**documentation *) done . .. downto else end  
 external for fun function functor >} >] if in include inherit initializer {< [@ [@@ [@@@ [> [< [%% <- let match >. .< .~ method -> mutable new nonrec object of open ?label: private ? ' {%%%%ext|s|} } ] rec ; ;; sig struct then to try val virtual when while with
```


## Pattern 235

```
pattern_gen: constr_longident LPAREN TYPE . nonempty_list_mkrhs_LIDENT__ RPAREN simple_pattern
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 236

```
pattern_gen: constr_longident LPAREN TYPE nonempty_list_mkrhs_LIDENT__ . RPAREN simple_pattern
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
nonempty_list_mkrhs_LIDENT__: LIDENT . _*
```

## Pattern 237

```
pattern_gen: constr_longident LPAREN TYPE nonempty_list_mkrhs_LIDENT__ RPAREN . simple_pattern
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer ~label: lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type val virtual when while with
```


## Pattern 238

```
constr_longident: LPAREN COLONCOLON . RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 239

```
labeled_tuple_pat_element_list_pattern_: pattern . _*
reversed_labeled_tuple_pattern_pattern_: pattern . COMMA DOTDOT
simple_pattern_not_ident: LPAREN pattern . _*
```

### Sample 1

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
&& & and and* assert ` ! || |] begin 'a' class := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
pattern: pattern AS val_ident .
val_ident: LIDENT .
```

## Pattern 240

```
simple_pattern_not_ident: LPAREN pattern COLON . core_type RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 241

```
simple_pattern_not_ident: LPAREN pattern COLON core_type . RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 242

```
constr_longident: mod_longident DOT LPAREN COLONCOLON . RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 243

```
labeled_tuple_pat_element_list_pattern_: pattern . _*
reversed_labeled_tuple_pattern_pattern_: pattern . COMMA DOTDOT
simple_pattern_not_ident: mod_longident DOT LPAREN pattern . RPAREN
```

### Sample 1

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
&& & and and* assert ` ! || |] begin 'a' class : := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
pattern: pattern AS val_ident .
val_ident: LIDENT .
```

## Pattern 244

```
simple_delimited_pattern: LBRACKET . separated_or_terminated_nonempty_list_SEMI_pattern_ RBRACKET
simple_pattern_not_ident: mod_longident DOT LBRACKET . RBRACKET
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 245

```
simple_delimited_pattern: LBRACKET separated_or_terminated_nonempty_list_SEMI_pattern_ . RBRACKET
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
separated_or_terminated_nonempty_list_SEMI_pattern_: pattern SEMI . _*
```

## Pattern 246

```
simple_delimited_pattern: LBRACKETBAR separated_or_terminated_nonempty_list_SEMI_pattern_ . BARRBRACKET
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
&& & and and* as assert ` ! | || begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
separated_or_terminated_nonempty_list_SEMI_pattern_: pattern SEMI . _*
```

## Pattern 247

```
labeled_tuple_pat_element_list_pattern_no_exn_: LABEL . _*
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LIDENT COMMA LABEL . simple_pattern
reversed_labeled_tuple_pattern_pattern_no_exn_: LABEL . simple_pattern COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer ~label: lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type val virtual when while with
```


## Pattern 248

```
labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA . _*
reversed_labeled_tuple_pattern_pattern_no_exn_: LABEL simple_pattern COMMA . DOTDOT
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .+ downto effect else end  
 = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 249

```
labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA TILDE . _*
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE . _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 250

```
labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA TILDE LPAREN . LIDENT COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN . _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN . LIDENT COLON core_type RPAREN COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 251

```
labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT . COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT . _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 252

```
labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON . core_type RPAREN
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON . _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON . core_type RPAREN COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 253

```
labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 254

```
labeled_tuple_pat_element_list_pattern_no_exn_: LABEL . _*
labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern COMMA LABEL . simple_pattern
labeled_tuple_pat_element_list_pattern_no_exn_: LABEL . _*
reversed_labeled_tuple_pattern_pattern_no_exn_: LABEL . simple_pattern COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer ~label: lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type val virtual when while with
```


## Pattern 255

```
labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA . _*
reversed_labeled_tuple_pattern_pattern_no_exn_: pattern_no_exn COMMA . DOTDOT
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .+ downto effect else end  
 = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 256

```
labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA TILDE . _*
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE . _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 257

```
labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA TILDE LPAREN . LIDENT COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN . _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN . LIDENT COLON core_type RPAREN COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 258

```
labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA TILDE LPAREN LIDENT . COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT . _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 259

```
labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA TILDE LPAREN LIDENT COLON . core_type RPAREN
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON . _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON . core_type RPAREN COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 260

```
labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 261

```
labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn COMMA LABEL . simple_pattern
labeled_tuple_pat_element_list_pattern_no_exn_: LABEL . _*
reversed_labeled_tuple_pattern_pattern_no_exn_: LABEL . simple_pattern COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer ~label: lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type val virtual when while with
```


## Pattern 262

```
pattern_no_exn: pattern_no_exn COLONCOLON . pattern
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 263

```
pattern_no_exn: pattern_no_exn BAR . pattern
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 264

```
pattern_no_exn: pattern_no_exn AS . val_ident
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 265

```
labeled_tuple_pat_element_list_pattern_no_exn_: labeled_tuple_pat_element_list_pattern_no_exn_ COMMA . _*
reversed_labeled_tuple_pattern_pattern_no_exn_: labeled_tuple_pat_element_list_pattern_no_exn_ COMMA . DOTDOT
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .+ downto effect else end  
 = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 266

```
labeled_tuple_pat_element_list_pattern_no_exn_: labeled_tuple_pat_element_list_pattern_no_exn_ COMMA TILDE . _*
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE . _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 267

```
labeled_tuple_pat_element_list_pattern_no_exn_: labeled_tuple_pat_element_list_pattern_no_exn_ COMMA TILDE LPAREN . LIDENT COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN . _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN . LIDENT COLON core_type RPAREN COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 268

```
labeled_tuple_pat_element_list_pattern_no_exn_: labeled_tuple_pat_element_list_pattern_no_exn_ COMMA TILDE LPAREN LIDENT . COLON core_type RPAREN
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT . _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT . COLON core_type RPAREN COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 269

```
labeled_tuple_pat_element_list_pattern_no_exn_: labeled_tuple_pat_element_list_pattern_no_exn_ COMMA TILDE LPAREN LIDENT COLON . core_type RPAREN
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON . _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON . core_type RPAREN COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 270

```
labeled_tuple_pat_element_list_pattern_no_exn_: labeled_tuple_pat_element_list_pattern_no_exn_ COMMA TILDE LPAREN LIDENT COLON core_type . RPAREN
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type . RPAREN COMMA DOTDOT
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 271

```
labeled_tuple_pat_element_list_pattern_no_exn_: labeled_tuple_pat_element_list_pattern_no_exn_ COMMA LABEL . simple_pattern
labeled_tuple_pat_element_list_pattern_no_exn_: LABEL . _*
reversed_labeled_tuple_pattern_pattern_no_exn_: LABEL . simple_pattern COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer ~label: lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type val virtual when while with
```


## Pattern 272

```
labeled_tuple_pat_element_list_pattern_no_exn_: LABEL . _*
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA LABEL . simple_pattern
reversed_labeled_tuple_pattern_pattern_no_exn_: LABEL . simple_pattern COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer ~label: lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type val virtual when while with
```


## Pattern 273

```
labeled_tuple_pat_element_list_pattern_no_exn_: TILDE LIDENT . _*
reversed_labeled_tuple_pattern_pattern_no_exn_: TILDE LIDENT . COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 274

```
labeled_tuple_pat_element_list_pattern_no_exn_: LABEL . _*
reversed_labeled_tuple_pattern_pattern_no_exn_: LABEL . simple_pattern COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer ~label: lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type val virtual when while with
```


## Pattern 275

```
labeled_tuple_pat_element_list_pattern_no_exn_: LABEL simple_pattern . _*
reversed_labeled_tuple_pattern_pattern_no_exn_: LABEL simple_pattern . COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
simple_pattern: val_ident .
val_ident: LIDENT .
```

## Pattern 276

```
simple_param_pattern: TILDE . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 277

```
simple_param_pattern: TILDE LPAREN . label_let_pattern RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 278

```
label_let_pattern: LIDENT COLON . possibly_poly_core_type_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 279

```
possibly_poly_core_type_: reversed_nonempty_llist_typevar_ . DOT core_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
atomic_type: QUOTE ident .
reversed_nonempty_llist_typevar_: QUOTE ident .
ident: LIDENT .
```

## Pattern 280

```
possibly_poly_core_type_: reversed_nonempty_llist_typevar_ DOT . core_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 281

```
simple_param_pattern: TILDE LPAREN label_let_pattern . RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
label_let_pattern: LIDENT . _*
```

## Pattern 282

```
simple_param_pattern: QUESTION . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 283

```
simple_param_pattern: QUESTION LPAREN . label_let_pattern option_preceded_EQUAL_seq_expr__ RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 284

```
simple_param_pattern: QUESTION LPAREN label_let_pattern . option_preceded_EQUAL_seq_expr__ RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
label_let_pattern: LIDENT . _*
```

## Pattern 285

```
option_preceded_EQUAL_seq_expr__: EQUAL . seq_expr
```

### Sample 1

Sentence:
```
let* x ?label: ( false =
```
Stack:
```
use_file: LETOP val_ident OPTLABEL LPAREN let_pattern EQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 286

```
fun_expr: LET . ext list_attribute_ local_structure_item IN seq_expr
let_bindings_ext_: LET . ext list_attribute_ rec_flag let_binding_body list_post_item_attribute_
```

### Sample 1

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
&& & and and* as assert ! | || |] begin : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [> [< < <- let let* match >. .< .~ method -. -> mutable new nonrec object of ?label: or +. += !+ private ? ' } ] ) ; ;; sig * struct then to try virtual when while with
```


## Pattern 287

```
local_structure_item: MODULE . _*
module_type_declaration: MODULE . TYPE ext list_attribute_ ident option_preceded_EQUAL_module_type__ list_post_item_attribute_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] ) ; ;; sig * "s" struct then ~ to true try val virtual when while with
```


## Pattern 288

```
module_type_declaration: MODULE TYPE . ext list_attribute_ ident option_preceded_EQUAL_module_type__ list_post_item_attribute_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 289

```
module_type_declaration: MODULE TYPE ext list_attribute_ . ident option_preceded_EQUAL_module_type__ list_post_item_attribute_
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 290

```
option_preceded_EQUAL_module_type__: EQUAL . module_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 291

```
functor_arg: LPAREN . _*
module_type: LPAREN . module_type RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ; ;; * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 292

```
module_type: FUNCTOR . list_attribute_ reversed_nonempty_llist_functor_arg_ MINUSGREATER module_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 293

```
module_type: FUNCTOR list_attribute_ . reversed_nonempty_llist_functor_arg_ MINUSGREATER module_type
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 294

```
functor_arg: LPAREN . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 295

```
functor_arg: LPAREN module_name . COLON module_type RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
ident: UIDENT .
mk_longident_mod_ext_longident_UIDENT_: UIDENT .
module_name: UIDENT .
```

## Pattern 296

```
functor_arg: LPAREN module_name COLON . module_type RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 297

```
module_type: reversed_nonempty_llist_functor_arg_ . MINUSGREATER module_type
```

### Sample 1

Sentence:
```
( val X : {%%ext|s|} :> ( ) ( X : {%%ext|s|} )
```
Stack:
```
parse_module_expr: LPAREN VAL list_attribute_ fun_expr COLON module_type COLONGREATER reversed_nonempty_llist_functor_arg_ LPAREN module_name COLON module_type RPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
reversed_nonempty_llist_functor_arg_: reversed_nonempty_llist_functor_arg_ functor_arg .
functor_arg: LPAREN module_name COLON module_type RPAREN .
```

## Pattern 298

```
module_type: reversed_nonempty_llist_functor_arg_ MINUSGREATER . module_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 299

```
module_type: module_type WITH . reversed_separated_nonempty_llist_AND_with_constraint_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try X _ val virtual when while with
```


## Pattern 300

```
with_constraint: TYPE . _*
```

### Sample 1

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
&& & and and* as assert ` | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 301

```
with_constraint: TYPE type_parameters . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

Also covered by these intermediate patterns:
```
type_parameters: LPAREN reversed_separated_nonempty_llist_COMMA_type_parameter_ RPAREN .
```

## Pattern 302

```
with_constraint: TYPE type_parameters label_longident . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
label_longident: mk_longident_mod_longident_LIDENT_ .
mk_longident_mod_longident_LIDENT_: mod_longident DOT LIDENT .
```

## Pattern 303

```
with_constraint: TYPE type_parameters label_longident COLONEQUAL . alias_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 304

```
with_constraint: TYPE type_parameters label_longident with_type_binder . alias_type reversed_llist_preceded_CONSTRAINT_constrain__
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

Also covered by these intermediate patterns:
```
with_type_binder: EQUAL PRIVATE .
```

## Pattern 305

```
reversed_llist_preceded_CONSTRAINT_constrain__: reversed_llist_preceded_CONSTRAINT_constrain__ CONSTRAINT . core_type EQUAL core_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 306

```
reversed_llist_preceded_CONSTRAINT_constrain__: reversed_llist_preceded_CONSTRAINT_constrain__ CONSTRAINT core_type . EQUAL core_type
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 307

```
reversed_llist_preceded_CONSTRAINT_constrain__: reversed_llist_preceded_CONSTRAINT_constrain__ CONSTRAINT core_type EQUAL . core_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 308

```
with_constraint: MODULE . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try _ val virtual when while with
```


## Pattern 309

```
with_constraint: MODULE TYPE . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 310

```
with_constraint: MODULE TYPE mty_longident . _*
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
mty_longident: mk_longident_mod_ext_longident_ident_ .
mk_longident_mod_ext_longident_ident_: ident .
ident: LIDENT .
```

## Pattern 311

```
with_constraint: MODULE TYPE mty_longident EQUAL . module_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 312

```
module_type: module_type MINUSGREATER . module_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 313

```
mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident . DOT UIDENT
mk_longident_mod_ext_longident_ident_: mod_ext_longident . DOT ident
```

### Sample 1

Sentence:
```
( val X : {%%ext|s|} :> X ( X )
```
Stack:
```
parse_module_expr: LPAREN VAL list_attribute_ fun_expr COLON module_type COLONGREATER mod_ext_longident LPAREN mod_ext_longident RPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
mod_ext_longident: mod_ext_longident LPAREN mod_ext_longident RPAREN .
```

## Pattern 314

```
mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident DOT . UIDENT
mk_longident_mod_ext_longident_ident_: mod_ext_longident DOT . ident
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 315

```
with_constraint: MODULE TYPE mty_longident COLONEQUAL . module_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 316

```
mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT
with_constraint: MODULE mod_longident . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  
 exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
mod_longident: mk_longident_mod_longident_UIDENT_ .
mk_longident_mod_longident_UIDENT_: mod_longident DOT UIDENT .
```

## Pattern 317

```
with_constraint: MODULE mod_longident EQUAL . mod_ext_longident
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 318

```
mk_longident_mod_longident_UIDENT_: mod_longident DOT . UIDENT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 319

```
with_constraint: MODULE mod_longident COLONEQUAL . mod_ext_longident
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 320

```
reversed_separated_nonempty_llist_AND_with_constraint_: reversed_separated_nonempty_llist_AND_with_constraint_ AND . with_constraint
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try X _ val virtual when while with
```


## Pattern 321

```
functor_arg: LPAREN module_name COLON module_type . RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

Also covered by these intermediate patterns:
```
module_type: SIG list_attribute_ signature END .
```

## Pattern 322

```
module_type: FUNCTOR list_attribute_ reversed_nonempty_llist_functor_arg_ . MINUSGREATER module_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
reversed_nonempty_llist_functor_arg_: functor_arg .
functor_arg: LPAREN module_name COLON module_type RPAREN .
```

## Pattern 323

```
module_type: FUNCTOR list_attribute_ reversed_nonempty_llist_functor_arg_ MINUSGREATER . module_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 324

```
module_type: LPAREN module_type . RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

Also covered by these intermediate patterns:
```
module_type: SIG list_attribute_ signature END .
```

## Pattern 325

```
local_structure_item: MODULE ext list_attribute_ . _*
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 326

```
local_structure_item: MODULE ext list_attribute_ REC . module_name module_binding_body list_post_item_attribute_ list_and_module_binding_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 327

```
local_structure_item: MODULE ext list_attribute_ REC module_name . module_binding_body list_post_item_attribute_ list_and_module_binding_
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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
module_name: UIDENT .
```

## Pattern 328

```
module_binding_body: EQUAL . module_expr
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ val virtual when while with
```


## Pattern 329

```
module_expr: FUNCTOR . list_attribute_ reversed_nonempty_llist_functor_arg_ MINUSGREATER module_expr
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 330

```
module_expr: FUNCTOR list_attribute_ . reversed_nonempty_llist_functor_arg_ MINUSGREATER module_expr
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 331

```
module_expr: FUNCTOR list_attribute_ reversed_nonempty_llist_functor_arg_ . MINUSGREATER module_expr
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
reversed_nonempty_llist_functor_arg_: functor_arg .
functor_arg: LPAREN module_name COLON module_type RPAREN .
```

## Pattern 332

```
module_expr: FUNCTOR list_attribute_ reversed_nonempty_llist_functor_arg_ MINUSGREATER . module_expr
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ val virtual when while with
```


## Pattern 333

```
module_expr: module_expr LPAREN . RPAREN
paren_module_expr: LPAREN . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ; ;; sig * "s" then ~ to true try type _ virtual when while with
```


## Pattern 334

```
paren_module_expr: LPAREN module_expr . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
module_expr: STRUCT list_attribute_ structure END .
```

## Pattern 335

```
paren_module_expr: LPAREN module_expr COLON . module_type RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 336

```
paren_module_expr: LPAREN module_expr COLON module_type . RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

Also covered by these intermediate patterns:
```
module_type: SIG list_attribute_ signature END .
```

## Pattern 337

```
module_binding_body: COLON . module_type EQUAL module_expr
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 338

```
module_binding_body: COLON module_type . EQUAL module_expr
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

Also covered by these intermediate patterns:
```
module_type: SIG list_attribute_ signature END .
```

## Pattern 339

```
module_binding_body: COLON module_type EQUAL . module_expr
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ val virtual when while with
```


## Pattern 340

```
list_and_module_binding_: AND . list_attribute_ module_name module_binding_body list_post_item_attribute_ list_and_module_binding_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 341

```
list_and_module_binding_: AND list_attribute_ . module_name module_binding_body list_post_item_attribute_ list_and_module_binding_
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 342

```
list_and_module_binding_: AND list_attribute_ module_name . module_binding_body list_post_item_attribute_ list_and_module_binding_
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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
module_name: UIDENT .
```

## Pattern 343

```
module_binding_body: functor_arg . module_binding_body
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
functor_arg: LPAREN module_name COLON module_type RPAREN .
```

## Pattern 344

```
local_structure_item: MODULE ext list_attribute_ module_name . module_binding_body list_post_item_attribute_
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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
module_name: UIDENT .
```

## Pattern 345

```
item_extension: LBRACKETPERCENTPERCENT . attr_id payload RBRACKET
```

### Sample 1

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
&& & and* ` ! | || |] 'a' : :: := :> , (*comment*) (**documentation *) . .. .+  
 = 1.0 > >} >] # ## != ^ +! land ** 1 ~label: { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let* ( >. .< .~ - -. -> ?label: % + +. += !+ ? ' {%%ext|s|} {%%%%ext|s|} } ] ) ; ;; * "s" ~ _
```


## Pattern 346

```
payload: QUESTION . _*
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 347

```
payload: QUESTION pattern WHEN . seq_expr
```

### Sample 1

Sentence:
```
[% and ? false when
```
Stack:
```
use_file: LBRACKETPERCENT attr_id QUESTION pattern WHEN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 348

```
constr_extra_nonprefix_ident: LBRACKET . RBRACKET
simple_expr: LBRACKET . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET
```

### Sample 1

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
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 349

```
simple_expr: LBRACELESS . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 350

```
option_preceded_EQUAL_expr__: EQUAL . _*
```

### Sample 1

Sentence:
```
{< x =
```
Stack:
```
use_file: LBRACELESS LIDENT EQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 351

```
simple_expr: LBRACE . record_expr_content RBRACE
```

### Sample 1

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
&& & and and* as assert | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy [@ [@@ [@@@ [> [< [%% < <- let let* match >. method - -. -> module mutable nonrec of open ?label: or % + +. += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type _ val virtual when while with
```


## Pattern 352

```
simple_expr: BEGIN . _*
```

### Sample 1

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
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 353

```
simple_expr: BEGIN ext list_attribute_ . _*
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
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 354

```
fun_expr: LAZY . ext list_attribute_ simple_expr
```

### Sample 1

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
&& & and and* as assert | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy [@@ [@@@ [> [< [%% < <- let let* match >. method - -. -> module mutable nonrec of open ?label: or + +. += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type _ val virtual when while with
```


## Pattern 355

```
fun_expr: LAZY ext list_attribute_ . simple_expr
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
&& & and and* as assert | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy [@ [@@ [@@@ [> [< [%% < <- let let* match >. method - -. -> module mutable nonrec of open ?label: or % + +. += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type _ val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 356

```
simple_expr: BANG . simple_expr
```

### Sample 1

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
&& & and and* as assert | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy [@ [@@ [@@@ [> [< [%% < <- let let* match >. method - -. -> module mutable nonrec of open ?label: or % + +. += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type _ val virtual when while with
```


## Pattern 357

```
simple_expr: simple_expr HASHOP . simple_expr
```

### Sample 1

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
&& & and and* as assert | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy [@ [@@ [@@@ [> [< [%% < <- let let* match >. method - -. -> module mutable nonrec of open ?label: or % + +. += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type _ val virtual when while with
```


## Pattern 358

```
simple_expr: simple_expr HASH . LIDENT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 359

```
simple_expr: simple_expr DOTOP . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 360

```
simple_expr: simple_expr DOTOP LPAREN . separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN
```

### Sample 1

Sentence:
```
{ 'a' .+ (
```
Stack:
```
use_file: LBRACE simple_expr DOTOP LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 361

```
reversed_labeled_tuple_body: LABEL . _*
```

### Sample 1

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
&& & and and* as assert | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy [@ [@@ [@@@ [> [< [%% < <- let let* match >. method - -. -> module mutable nonrec of open ?label: or % + +. += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type _ val virtual when while with
```


## Pattern 362

```
reversed_labeled_tuple_body: LABEL simple_expr . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done .. downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
simple_expr: OBJECT ext list_attribute_ class_self_pattern list_text_cstr_class_field__ END .
```

## Pattern 363

```
simple_expr: simple_expr DOT . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 364

```
simple_expr: simple_expr DOT LPAREN . seq_expr RPAREN
```

### Sample 1

Sentence:
```
{ 'a' . (
```
Stack:
```
use_file: LBRACE simple_expr DOT LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 365

```
fun_expr: IF . _*
```

### Sample 1

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
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 366

```
fun_expr: IF ext list_attribute_ . _*
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
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 367

```
reversed_labeled_tuple_body: FUNCTION . _*
seq_expr: FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
```

### Sample 1

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
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 368

```
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
seq_expr: FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
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
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 369

```
reversed_preceded_or_separated_nonempty_llist_BAR_match_case_: BAR . match_case
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 370

```
labeled_tuple_pat_element_list_pattern_: pattern . _*
match_case: pattern . _*
reversed_labeled_tuple_pattern_pattern_: pattern . COMMA DOTDOT
```

### Sample 1

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
&& & and and* assert ` ! || |] begin 'a' class : := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual while with
```

Also covered by these intermediate patterns:
```
pattern: pattern AS val_ident .
val_ident: LIDENT .
```

## Pattern 371

```
match_case: pattern WHEN . seq_expr MINUSGREATER seq_expr
```

### Sample 1

Sentence:
```
function false when
```
Stack:
```
use_file: FUNCTION ext list_attribute_ pattern WHEN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 372

```
fun_expr: FUN . ext list_attribute_ fun_params option_preceded_COLON_atomic_type__ MINUSGREATER fun_body
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer lazy {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open or +. += !+ private ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 373

```
fun_expr: FUN ext list_attribute_ . fun_params option_preceded_COLON_atomic_type__ MINUSGREATER fun_body
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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open or % +. += !+ private ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 374

```
simple_param_pattern: OPTLABEL . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X val virtual when while with
```


## Pattern 375

```
simple_param_pattern: OPTLABEL LPAREN . let_pattern option_preceded_EQUAL_seq_expr__ RPAREN
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 376

```
let_pattern: pattern COLON . possibly_poly_core_type_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 377

```
simple_param_pattern: OPTLABEL LPAREN let_pattern option_preceded_EQUAL_seq_expr__ . RPAREN
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
option_preceded_EQUAL_seq_expr__: EQUAL seq_expr .
seq_expr: fun_seq_expr .
fun_seq_expr: fun_expr SEMI . _*
```

## Pattern 378

```
constr_extra_nonprefix_ident: LPAREN . RPAREN
constr_longident: LPAREN . COLONCOLON RPAREN
fun_param_as_list: LPAREN . TYPE nonempty_list_mkrhs_LIDENT__ RPAREN
simple_param_pattern: LPAREN . pattern COLON reversed_nonempty_llist_typevar_ DOT core_type RPAREN
simple_pattern_not_ident: LPAREN . _*
val_extra_ident: LPAREN . operator RPAREN
```

### Sample 1

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
and as assert | |] begin class : :> , (*comment*) constraint do (**documentation *) done . .. downto else end  
 external for fun function functor >} >] if in include inherit initializer {< [@ [@@ [@@@ [> [< [%% <- let match >. .< .~ method -> mutable new nonrec object of open ?label: private ? ' {%%%%ext|s|} } ] rec ; ;; sig struct then to try val virtual when while with
```


## Pattern 379

```
fun_param_as_list: LPAREN TYPE . nonempty_list_mkrhs_LIDENT__ RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 380

```
fun_param_as_list: LPAREN TYPE nonempty_list_mkrhs_LIDENT__ . RPAREN
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
nonempty_list_mkrhs_LIDENT__: LIDENT . _*
```

## Pattern 381

```
labeled_tuple_pat_element_list_pattern_: pattern . _*
reversed_labeled_tuple_pattern_pattern_: pattern . COMMA DOTDOT
simple_param_pattern: LPAREN pattern . COLON reversed_nonempty_llist_typevar_ DOT core_type RPAREN
simple_pattern_not_ident: LPAREN pattern . _*
```

### Sample 1

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
&& & and and* assert ` ! || |] begin 'a' class := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
pattern: pattern AS val_ident .
val_ident: LIDENT .
```

## Pattern 382

```
simple_param_pattern: LPAREN pattern COLON . reversed_nonempty_llist_typevar_ DOT core_type RPAREN
simple_pattern_not_ident: LPAREN pattern COLON . core_type RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 383

```
simple_param_pattern: LPAREN pattern COLON reversed_nonempty_llist_typevar_ . DOT core_type RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
atomic_type: QUOTE ident .
reversed_nonempty_llist_typevar_: QUOTE ident .
ident: LIDENT .
```

## Pattern 384

```
simple_param_pattern: LPAREN pattern COLON reversed_nonempty_llist_typevar_ DOT . core_type RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 385

```
simple_param_pattern: LPAREN pattern COLON reversed_nonempty_llist_typevar_ DOT core_type . RPAREN
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 386

```
simple_param_pattern: LABEL . _*
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer ~label: lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type val virtual when while with
```


## Pattern 387

```
constr_extra_nonprefix_ident: LPAREN . RPAREN
constr_longident: LPAREN . COLONCOLON RPAREN
simple_param_pattern: LABEL LPAREN . pattern COLON reversed_nonempty_llist_typevar_ DOT core_type RPAREN
simple_pattern_not_ident: LPAREN . _*
val_extra_ident: LPAREN . operator RPAREN
```

### Sample 1

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
and as assert | |] begin class : :> , (*comment*) constraint do (**documentation *) done . .. downto else end  
 external for fun function functor >} >] if in include inherit initializer {< [@ [@@ [@@@ [> [< [%% <- let match >. .< .~ method -> mutable new nonrec object of open ?label: private ? ' {%%%%ext|s|} } ] rec ; ;; sig struct then to try type val virtual when while with
```


## Pattern 388

```
labeled_tuple_pat_element_list_pattern_: pattern . _*
reversed_labeled_tuple_pattern_pattern_: pattern . COMMA DOTDOT
simple_param_pattern: LABEL LPAREN pattern . COLON reversed_nonempty_llist_typevar_ DOT core_type RPAREN
simple_pattern_not_ident: LPAREN pattern . _*
```

### Sample 1

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
&& & and and* assert ` ! || |] begin 'a' class := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
pattern: pattern AS val_ident .
val_ident: LIDENT .
```

## Pattern 389

```
simple_param_pattern: LABEL LPAREN pattern COLON . reversed_nonempty_llist_typevar_ DOT core_type RPAREN
simple_pattern_not_ident: LPAREN pattern COLON . core_type RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 390

```
simple_param_pattern: LABEL LPAREN pattern COLON reversed_nonempty_llist_typevar_ . DOT core_type RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
atomic_type: QUOTE ident .
reversed_nonempty_llist_typevar_: QUOTE ident .
ident: LIDENT .
```

## Pattern 391

```
simple_param_pattern: LABEL LPAREN pattern COLON reversed_nonempty_llist_typevar_ DOT . core_type RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 392

```
simple_param_pattern: LABEL LPAREN pattern COLON reversed_nonempty_llist_typevar_ DOT core_type . RPAREN
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 393

```
option_preceded_COLON_atomic_type__: COLON . atomic_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 394

```
fun_expr: FUN ext list_attribute_ fun_params option_preceded_COLON_atomic_type__ . MINUSGREATER fun_body
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
option_preceded_COLON_atomic_type__: COLON atomic_type .
atomic_type: UNDERSCORE .
```

## Pattern 395

```
fun_expr: FUN ext list_attribute_ fun_params option_preceded_COLON_atomic_type__ MINUSGREATER . fun_body
```

### Sample 1

Sentence:
```
fun false ->
```
Stack:
```
use_file: FUN ext list_attribute_ fun_params option_preceded_COLON_atomic_type__ MINUSGREATER
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 396

```
fun_body: FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . _*
```

### Sample 1

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
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 397

```
fun_body: FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
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
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 398

```
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COMMA . _*
```

### Sample 1

Sentence:
```
function false -> . ,
```
Stack:
```
use_file: FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COMMA
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 399

```
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COMMA TILDE . _*
reversed_labeled_tuple_body: TILDE . _*
```

### Sample 1

Sentence:
```
function false -> . , ~
```
Stack:
```
use_file: FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COMMA TILDE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 400

```
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COMMA TILDE LPAREN . LIDENT type_constraint RPAREN
reversed_labeled_tuple_body: TILDE LPAREN . _*
```

### Sample 1

Sentence:
```
function false -> . , ~ (
```
Stack:
```
use_file: FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COMMA TILDE LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 401

```
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COMMA TILDE LPAREN LIDENT . type_constraint RPAREN
reversed_labeled_tuple_body: TILDE LPAREN LIDENT . _*
```

### Sample 1

Sentence:
```
function false -> . , ~ ( x
```
Stack:
```
use_file: FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COMMA TILDE LPAREN LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 402

```
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COMMA TILDE LPAREN LIDENT type_constraint . RPAREN
reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint . _*
```

### Sample 1

Sentence:
```
function false -> . , ~ ( x : {%%ext|s|} :> {%%ext|s|} [@ and ]
```
Stack:
```
use_file: FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COMMA TILDE LPAREN LIDENT COLON core_type COLONGREATER core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
type_constraint: COLON core_type COLONGREATER core_type .
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 403

```
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COMMA LABEL . simple_expr
reversed_labeled_tuple_body: LABEL . _*
```

### Sample 1

Sentence:
```
function false -> . , ~label:
```
Stack:
```
use_file: FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COMMA LABEL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy [@ [@@ [@@@ [> [< [%% < <- let let* match >. method - -. -> module mutable nonrec of open ?label: or % + +. += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type _ val virtual when while with
```


## Pattern 404

```
reversed_labeled_tuple_body: LABEL simple_expr COMMA . _*
```

### Sample 1

Sentence:
```
~label: 'a' ,
```
Stack:
```
use_file: LABEL simple_expr COMMA
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 405

```
reversed_labeled_tuple_body: LABEL simple_expr COMMA TILDE . _*
reversed_labeled_tuple_body: TILDE . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 406

```
reversed_labeled_tuple_body: LABEL simple_expr COMMA TILDE LPAREN . LIDENT type_constraint RPAREN
reversed_labeled_tuple_body: TILDE LPAREN . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 407

```
reversed_labeled_tuple_body: LABEL simple_expr COMMA TILDE LPAREN LIDENT . type_constraint RPAREN
reversed_labeled_tuple_body: TILDE LPAREN LIDENT . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 408

```
reversed_labeled_tuple_body: LABEL simple_expr COMMA TILDE LPAREN LIDENT type_constraint . RPAREN
reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
type_constraint: COLON core_type COLONGREATER core_type .
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 409

```
reversed_labeled_tuple_body: LABEL . _*
reversed_labeled_tuple_body: LABEL simple_expr COMMA LABEL . simple_expr
reversed_labeled_tuple_body: LABEL . _*
```

### Sample 1

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
&& & and and* as assert | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy [@ [@@ [@@@ [> [< [%% < <- let let* match >. method - -. -> module mutable nonrec of open ?label: or % + +. += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type _ val virtual when while with
```


## Pattern 410

```
constr_longident: mod_longident DOT . LPAREN COLONCOLON RPAREN
mk_longident_mod_longident_UIDENT_: mod_longident DOT . UIDENT
mk_longident_mod_longident_val_ident_: mod_longident DOT . val_ident
simple_expr: mod_longident DOT . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy [@ [@@ [@@@ [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 411

```
constr_longident: mod_longident DOT LPAREN . COLONCOLON RPAREN
simple_expr: mod_longident DOT LPAREN . _*
val_extra_ident: LPAREN . operator RPAREN
```

### Sample 1

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
and as | |] class : :> , (*comment*) constraint do (**documentation *) done . .. downto effect else end  
 exception external functor >} >] # in include inherit initializer [@ [@@ [@@@ [> [< [%% <- >. method -> mutable nonrec of open ?label: private ? ' {%%%%ext|s|} } ] rec ; ;; sig struct then to type val virtual when with
```


## Pattern 412

```
simple_expr: mod_longident DOT LPAREN MODULE . ext list_attribute_ module_expr COLON module_type RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ val virtual when while with
```


## Pattern 413

```
simple_expr: mod_longident DOT LPAREN MODULE ext list_attribute_ . module_expr COLON module_type RPAREN
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 414

```
simple_expr: mod_longident DOT LPAREN MODULE ext list_attribute_ module_expr . COLON module_type RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
module_expr: STRUCT list_attribute_ structure END .
```

## Pattern 415

```
simple_expr: mod_longident DOT LPAREN MODULE ext list_attribute_ module_expr COLON . module_type RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 416

```
simple_expr: mod_longident DOT LPAREN MODULE ext list_attribute_ module_expr COLON module_type . RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

Also covered by these intermediate patterns:
```
module_type: SIG list_attribute_ signature END .
```

## Pattern 417

```
letop_binding_body: simple_pattern COLON . core_type EQUAL seq_expr
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 418

```
letop_binding_body: simple_pattern COLON core_type . EQUAL seq_expr
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 419

```
letop_binding_body: simple_pattern COLON core_type EQUAL . seq_expr
```

### Sample 1

Sentence:
```
let* x : {%%ext|s|} =
```
Stack:
```
use_file: LETOP simple_pattern COLON core_type EQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 420

```
fun_expr: FOR . ext list_attribute_ pattern EQUAL seq_expr direction_flag seq_expr DO seq_expr DONE
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 421

```
fun_expr: FOR ext list_attribute_ . pattern EQUAL seq_expr direction_flag seq_expr DO seq_expr DONE
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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 422

```
fun_expr: FOR ext list_attribute_ pattern . EQUAL seq_expr direction_flag seq_expr DO seq_expr DONE
labeled_tuple_pat_element_list_pattern_: pattern . _*
reversed_labeled_tuple_pattern_pattern_: pattern . COMMA DOTDOT
```

### Sample 1

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
&& & and and* assert ` ! || |] begin 'a' class : := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
pattern: pattern AS val_ident .
val_ident: LIDENT .
```

## Pattern 423

```
fun_expr: FOR ext list_attribute_ pattern EQUAL . seq_expr direction_flag seq_expr DO seq_expr DONE
```

### Sample 1

Sentence:
```
for false =
```
Stack:
```
use_file: FOR ext list_attribute_ pattern EQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 424

```
fun_expr: ASSERT . ext list_attribute_ simple_expr
```

### Sample 1

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
&& & and and* as assert | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy [@@ [@@@ [> [< [%% < <- let let* match >. method - -. -> module mutable nonrec of open ?label: or + +. += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type _ val virtual when while with
```


## Pattern 425

```
fun_expr: ASSERT ext list_attribute_ . simple_expr
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
&& & and and* as assert | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy [@ [@@ [@@@ [> [< [%% < <- let let* match >. method - -. -> module mutable nonrec of open ?label: or % + +. += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type _ val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 426

```
fun_expr: subtractive . _*
```

### Sample 1

Sentence:
```
X . ( -
```
Stack:
```
use_file: mod_longident DOT LPAREN MINUS
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```

Also covered by these intermediate patterns:
```
operator: MINUS .
subtractive: MINUS .
```

## Pattern 427

```
fun_expr: subtractive FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . _*
```

### Sample 1

Sentence:
```
- function
```
Stack:
```
use_file: subtractive FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 428

```
fun_expr: subtractive FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```

### Sample 1

Sentence:
```
- function [@ and ]
```
Stack:
```
use_file: subtractive FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 429

```
reversed_preceded_or_separated_nonempty_llist_BAR_match_case_: reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ BAR . match_case
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 430

```
labeled_simple_expr: TILDE . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 431

```
labeled_simple_expr: TILDE LPAREN . LIDENT type_constraint RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 432

```
labeled_simple_expr: TILDE LPAREN LIDENT . type_constraint RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 433

```
labeled_simple_expr: TILDE LPAREN LIDENT type_constraint . RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
type_constraint: COLON core_type COLONGREATER core_type .
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 434

```
labeled_simple_expr: QUESTION . LIDENT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 435

```
labeled_simple_expr: OPTLABEL . simple_expr
```

### Sample 1

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
&& & and and* as assert | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy [@ [@@ [@@@ [> [< [%% < <- let let* match >. method - -. -> module mutable nonrec of open ?label: or % + +. += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type _ val virtual when while with
```


## Pattern 436

```
labeled_simple_expr: LABEL . simple_expr
```

### Sample 1

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
&& & and and* as assert | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy [@ [@@ [@@@ [> [< [%% < <- let let* match >. method - -. -> module mutable nonrec of open ?label: or % + +. += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type _ val virtual when while with
```


## Pattern 437

```
fun_expr: simple_expr DOTOP . _*
simple_expr: simple_expr DOTOP . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 438

```
fun_expr: simple_expr DOTOP LPAREN . _*
simple_expr: simple_expr DOTOP LPAREN . separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN
```

### Sample 1

Sentence:
```
X .+ (
```
Stack:
```
use_file: simple_expr DOTOP LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 439

```
reversed_labeled_tuple_body: FUNCTION . _*
separated_or_terminated_nonempty_list_SEMI_expr_: FUNCTION . _*
```

### Sample 1

Sentence:
```
[ function
```
Stack:
```
use_file: LBRACKET FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 440

```
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
separated_or_terminated_nonempty_list_SEMI_expr_: FUNCTION ext list_attribute_ . _*
```

### Sample 1

Sentence:
```
[ function [@ and ]
```
Stack:
```
use_file: LBRACKET FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 441

```
reversed_labeled_tuple_body: reversed_labeled_tuple_body COMMA . _*
```

### Sample 1

Sentence:
```
X , X ,
```
Stack:
```
use_file: reversed_labeled_tuple_body COMMA
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 442

```
reversed_labeled_tuple_body: reversed_labeled_tuple_body COMMA TILDE . _*
reversed_labeled_tuple_body: TILDE . _*
```

### Sample 1

Sentence:
```
X , X , ~
```
Stack:
```
use_file: reversed_labeled_tuple_body COMMA TILDE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 443

```
reversed_labeled_tuple_body: reversed_labeled_tuple_body COMMA TILDE LPAREN . LIDENT type_constraint RPAREN
reversed_labeled_tuple_body: TILDE LPAREN . _*
```

### Sample 1

Sentence:
```
X , X , ~ (
```
Stack:
```
use_file: reversed_labeled_tuple_body COMMA TILDE LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 444

```
reversed_labeled_tuple_body: reversed_labeled_tuple_body COMMA TILDE LPAREN LIDENT . type_constraint RPAREN
reversed_labeled_tuple_body: TILDE LPAREN LIDENT . _*
```

### Sample 1

Sentence:
```
X , X , ~ ( x
```
Stack:
```
use_file: reversed_labeled_tuple_body COMMA TILDE LPAREN LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 445

```
reversed_labeled_tuple_body: reversed_labeled_tuple_body COMMA TILDE LPAREN LIDENT type_constraint . RPAREN
reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint . _*
```

### Sample 1

Sentence:
```
X , X , ~ ( x : {%%ext|s|} :> {%%ext|s|} [@ and ]
```
Stack:
```
use_file: reversed_labeled_tuple_body COMMA TILDE LPAREN LIDENT COLON core_type COLONGREATER core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
type_constraint: COLON core_type COLONGREATER core_type .
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 446

```
reversed_labeled_tuple_body: reversed_labeled_tuple_body COMMA LABEL . simple_expr
reversed_labeled_tuple_body: LABEL . _*
```

### Sample 1

Sentence:
```
X , X , ~label:
```
Stack:
```
use_file: reversed_labeled_tuple_body COMMA LABEL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy [@ [@@ [@@@ [> [< [%% < <- let let* match >. method - -. -> module mutable nonrec of open ?label: or % + +. += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type _ val virtual when while with
```


## Pattern 447

```
reversed_labeled_tuple_body: reversed_labeled_tuple_body COMMA FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . _*
```

### Sample 1

Sentence:
```
X , X , function
```
Stack:
```
use_file: reversed_labeled_tuple_body COMMA FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 448

```
reversed_labeled_tuple_body: reversed_labeled_tuple_body COMMA FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```

### Sample 1

Sentence:
```
X , X , function [@ and ]
```
Stack:
```
use_file: reversed_labeled_tuple_body COMMA FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 449

```
fun_expr: let_bindings_ext_ . IN seq_expr
```

### Sample 1

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
&& & and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
let_bindings_ext_: LET ext list_attribute_ rec_flag let_binding_body list_post_item_attribute_ .
list_post_item_attribute_: post_item_attribute . list_post_item_attribute_
post_item_attribute: LBRACKETATAT attr_id attr_payload RBRACKET .
```

## Pattern 450

```
fun_expr: let_bindings_ext_ IN . seq_expr
```

### Sample 1

Sentence:
```
let x in
```
Stack:
```
parse_expression: let_bindings_ext_ IN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 451

```
fun_expr: fun_expr STAR . _*
```

### Sample 1

Sentence:
```
X *
```
Stack:
```
use_file: fun_expr STAR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 452

```
fun_expr: fun_expr STAR FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . _*
```

### Sample 1

Sentence:
```
X * function
```
Stack:
```
use_file: fun_expr STAR FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 453

```
fun_expr: fun_expr STAR FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```

### Sample 1

Sentence:
```
X * function [@ and ]
```
Stack:
```
use_file: fun_expr STAR FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 454

```
fun_expr: fun_expr PLUSEQ . _*
```

### Sample 1

Sentence:
```
X +=
```
Stack:
```
use_file: fun_expr PLUSEQ
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 455

```
fun_expr: fun_expr PLUSEQ FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . _*
```

### Sample 1

Sentence:
```
X += function
```
Stack:
```
use_file: fun_expr PLUSEQ FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 456

```
fun_expr: fun_expr PLUSEQ FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```

### Sample 1

Sentence:
```
X += function [@ and ]
```
Stack:
```
use_file: fun_expr PLUSEQ FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 457

```
fun_expr: fun_expr PLUSDOT . _*
```

### Sample 1

Sentence:
```
X +.
```
Stack:
```
use_file: fun_expr PLUSDOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 458

```
fun_expr: fun_expr PLUSDOT FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . _*
```

### Sample 1

Sentence:
```
X +. function
```
Stack:
```
use_file: fun_expr PLUSDOT FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 459

```
fun_expr: fun_expr PLUSDOT FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```

### Sample 1

Sentence:
```
X +. function [@ and ]
```
Stack:
```
use_file: fun_expr PLUSDOT FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 460

```
fun_expr: fun_expr PLUS . _*
```

### Sample 1

Sentence:
```
X +
```
Stack:
```
use_file: fun_expr PLUS
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 461

```
fun_expr: fun_expr PLUS FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . _*
```

### Sample 1

Sentence:
```
X + function
```
Stack:
```
use_file: fun_expr PLUS FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 462

```
fun_expr: fun_expr PLUS FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```

### Sample 1

Sentence:
```
X + function [@ and ]
```
Stack:
```
use_file: fun_expr PLUS FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 463

```
fun_expr: fun_expr PERCENT . _*
```

### Sample 1

Sentence:
```
X %
```
Stack:
```
use_file: fun_expr PERCENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 464

```
fun_expr: fun_expr PERCENT FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . _*
```

### Sample 1

Sentence:
```
X % function
```
Stack:
```
use_file: fun_expr PERCENT FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 465

```
fun_expr: fun_expr PERCENT FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```

### Sample 1

Sentence:
```
X % function [@ and ]
```
Stack:
```
use_file: fun_expr PERCENT FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 466

```
fun_expr: fun_expr OR . _*
```

### Sample 1

Sentence:
```
X or
```
Stack:
```
use_file: fun_expr OR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 467

```
fun_expr: fun_expr OR FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . _*
```

### Sample 1

Sentence:
```
X or function
```
Stack:
```
use_file: fun_expr OR FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 468

```
fun_expr: fun_expr OR FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```

### Sample 1

Sentence:
```
X or function [@ and ]
```
Stack:
```
use_file: fun_expr OR FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 469

```
fun_expr: fun_expr MINUSDOT . _*
```

### Sample 1

Sentence:
```
X -.
```
Stack:
```
use_file: fun_expr MINUSDOT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 470

```
fun_expr: fun_expr MINUSDOT FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . _*
```

### Sample 1

Sentence:
```
X -. function
```
Stack:
```
use_file: fun_expr MINUSDOT FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 471

```
fun_expr: fun_expr MINUSDOT FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```

### Sample 1

Sentence:
```
X -. function [@ and ]
```
Stack:
```
use_file: fun_expr MINUSDOT FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 472

```
fun_expr: fun_expr MINUS . _*
```

### Sample 1

Sentence:
```
X -
```
Stack:
```
use_file: fun_expr MINUS
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 473

```
fun_expr: fun_expr MINUS FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . _*
```

### Sample 1

Sentence:
```
X - function
```
Stack:
```
use_file: fun_expr MINUS FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 474

```
fun_expr: fun_expr MINUS FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```

### Sample 1

Sentence:
```
X - function [@ and ]
```
Stack:
```
use_file: fun_expr MINUS FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 475

```
fun_expr: fun_expr LESS . _*
```

### Sample 1

Sentence:
```
X <
```
Stack:
```
use_file: fun_expr LESS
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 476

```
fun_expr: fun_expr LESS FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . _*
```

### Sample 1

Sentence:
```
X < function
```
Stack:
```
use_file: fun_expr LESS FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 477

```
fun_expr: fun_expr LESS FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```

### Sample 1

Sentence:
```
X < function [@ and ]
```
Stack:
```
use_file: fun_expr LESS FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 478

```
fun_expr: fun_expr INFIXOP4 . _*
```

### Sample 1

Sentence:
```
X **
```
Stack:
```
use_file: fun_expr INFIXOP4
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 479

```
fun_expr: fun_expr INFIXOP4 FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . _*
```

### Sample 1

Sentence:
```
X ** function
```
Stack:
```
use_file: fun_expr INFIXOP4 FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 480

```
fun_expr: fun_expr INFIXOP4 FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```

### Sample 1

Sentence:
```
X ** function [@ and ]
```
Stack:
```
use_file: fun_expr INFIXOP4 FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 481

```
fun_expr: fun_expr INFIXOP3 . _*
```

### Sample 1

Sentence:
```
X land
```
Stack:
```
use_file: fun_expr INFIXOP3
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 482

```
fun_expr: fun_expr INFIXOP3 FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . _*
```

### Sample 1

Sentence:
```
X land function
```
Stack:
```
use_file: fun_expr INFIXOP3 FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 483

```
fun_expr: fun_expr INFIXOP3 FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```

### Sample 1

Sentence:
```
X land function [@ and ]
```
Stack:
```
use_file: fun_expr INFIXOP3 FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 484

```
fun_expr: fun_expr INFIXOP2 . _*
```

### Sample 1

Sentence:
```
X +!
```
Stack:
```
use_file: fun_expr INFIXOP2
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 485

```
fun_expr: fun_expr INFIXOP2 FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . _*
```

### Sample 1

Sentence:
```
X +! function
```
Stack:
```
use_file: fun_expr INFIXOP2 FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 486

```
fun_expr: fun_expr INFIXOP2 FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```

### Sample 1

Sentence:
```
X +! function [@ and ]
```
Stack:
```
use_file: fun_expr INFIXOP2 FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 487

```
fun_expr: fun_expr INFIXOP1 . _*
```

### Sample 1

Sentence:
```
X ^
```
Stack:
```
use_file: fun_expr INFIXOP1
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 488

```
fun_expr: fun_expr INFIXOP1 FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . _*
```

### Sample 1

Sentence:
```
X ^ function
```
Stack:
```
use_file: fun_expr INFIXOP1 FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 489

```
fun_expr: fun_expr INFIXOP1 FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```

### Sample 1

Sentence:
```
X ^ function [@ and ]
```
Stack:
```
use_file: fun_expr INFIXOP1 FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 490

```
fun_expr: fun_expr INFIXOP0 . _*
```

### Sample 1

Sentence:
```
X !=
```
Stack:
```
use_file: fun_expr INFIXOP0
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 491

```
fun_expr: fun_expr INFIXOP0 FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . _*
```

### Sample 1

Sentence:
```
X != function
```
Stack:
```
use_file: fun_expr INFIXOP0 FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 492

```
fun_expr: fun_expr INFIXOP0 FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```

### Sample 1

Sentence:
```
X != function [@ and ]
```
Stack:
```
use_file: fun_expr INFIXOP0 FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 493

```
fun_expr: fun_expr GREATER . _*
```

### Sample 1

Sentence:
```
X >
```
Stack:
```
use_file: fun_expr GREATER
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 494

```
fun_expr: fun_expr GREATER FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . _*
```

### Sample 1

Sentence:
```
X > function
```
Stack:
```
use_file: fun_expr GREATER FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 495

```
fun_expr: fun_expr GREATER FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```

### Sample 1

Sentence:
```
X > function [@ and ]
```
Stack:
```
use_file: fun_expr GREATER FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 496

```
fun_expr: fun_expr EQUAL . _*
```

### Sample 1

Sentence:
```
X =
```
Stack:
```
use_file: fun_expr EQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 497

```
fun_expr: fun_expr EQUAL FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . _*
```

### Sample 1

Sentence:
```
X = function
```
Stack:
```
use_file: fun_expr EQUAL FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 498

```
fun_expr: fun_expr EQUAL FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```

### Sample 1

Sentence:
```
X = function [@ and ]
```
Stack:
```
use_file: fun_expr EQUAL FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 499

```
reversed_labeled_tuple_body: fun_expr COMMA . _*
```

### Sample 1

Sentence:
```
X ,
```
Stack:
```
use_file: fun_expr COMMA
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 500

```
reversed_labeled_tuple_body: fun_expr COMMA TILDE . _*
reversed_labeled_tuple_body: TILDE . _*
```

### Sample 1

Sentence:
```
X , ~
```
Stack:
```
use_file: fun_expr COMMA TILDE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 501

```
reversed_labeled_tuple_body: fun_expr COMMA TILDE LPAREN . LIDENT type_constraint RPAREN
reversed_labeled_tuple_body: TILDE LPAREN . _*
```

### Sample 1

Sentence:
```
X , ~ (
```
Stack:
```
use_file: fun_expr COMMA TILDE LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 502

```
reversed_labeled_tuple_body: fun_expr COMMA TILDE LPAREN LIDENT . type_constraint RPAREN
reversed_labeled_tuple_body: TILDE LPAREN LIDENT . _*
```

### Sample 1

Sentence:
```
X , ~ ( x
```
Stack:
```
use_file: fun_expr COMMA TILDE LPAREN LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 503

```
reversed_labeled_tuple_body: fun_expr COMMA TILDE LPAREN LIDENT type_constraint . RPAREN
reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint . _*
```

### Sample 1

Sentence:
```
X , ~ ( x : {%%ext|s|} :> {%%ext|s|} [@ and ]
```
Stack:
```
use_file: fun_expr COMMA TILDE LPAREN LIDENT COLON core_type COLONGREATER core_type LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
type_constraint: COLON core_type COLONGREATER core_type .
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 504

```
reversed_labeled_tuple_body: fun_expr COMMA LABEL . simple_expr
reversed_labeled_tuple_body: LABEL . _*
```

### Sample 1

Sentence:
```
X , ~label:
```
Stack:
```
use_file: fun_expr COMMA LABEL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy [@ [@@ [@@@ [> [< [%% < <- let let* match >. method - -. -> module mutable nonrec of open ?label: or % + +. += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type _ val virtual when while with
```


## Pattern 505

```
reversed_labeled_tuple_body: fun_expr COMMA FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . _*
```

### Sample 1

Sentence:
```
X , function
```
Stack:
```
use_file: fun_expr COMMA FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 506

```
reversed_labeled_tuple_body: fun_expr COMMA FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```

### Sample 1

Sentence:
```
X , function [@ and ]
```
Stack:
```
use_file: fun_expr COMMA FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 507

```
fun_expr: fun_expr COLONEQUAL . _*
```

### Sample 1

Sentence:
```
X :=
```
Stack:
```
use_file: fun_expr COLONEQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 508

```
fun_expr: fun_expr COLONEQUAL FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . _*
```

### Sample 1

Sentence:
```
X := function
```
Stack:
```
use_file: fun_expr COLONEQUAL FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 509

```
fun_expr: fun_expr COLONEQUAL FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```

### Sample 1

Sentence:
```
X := function [@ and ]
```
Stack:
```
use_file: fun_expr COLONEQUAL FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 510

```
fun_expr: fun_expr COLONCOLON . _*
```

### Sample 1

Sentence:
```
X ::
```
Stack:
```
use_file: fun_expr COLONCOLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 511

```
fun_expr: fun_expr COLONCOLON FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . _*
```

### Sample 1

Sentence:
```
X :: function
```
Stack:
```
use_file: fun_expr COLONCOLON FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 512

```
fun_expr: fun_expr COLONCOLON FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```

### Sample 1

Sentence:
```
X :: function [@ and ]
```
Stack:
```
use_file: fun_expr COLONCOLON FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 513

```
fun_expr: fun_expr BARBAR . _*
```

### Sample 1

Sentence:
```
X ||
```
Stack:
```
use_file: fun_expr BARBAR
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 514

```
fun_expr: fun_expr BARBAR FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . _*
```

### Sample 1

Sentence:
```
X || function
```
Stack:
```
use_file: fun_expr BARBAR FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 515

```
fun_expr: fun_expr BARBAR FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```

### Sample 1

Sentence:
```
X || function [@ and ]
```
Stack:
```
use_file: fun_expr BARBAR FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 516

```
fun_expr: fun_expr AMPERSAND . _*
```

### Sample 1

Sentence:
```
X &
```
Stack:
```
use_file: fun_expr AMPERSAND
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 517

```
fun_expr: fun_expr AMPERSAND FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . _*
```

### Sample 1

Sentence:
```
X & function
```
Stack:
```
use_file: fun_expr AMPERSAND FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 518

```
fun_expr: fun_expr AMPERSAND FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```

### Sample 1

Sentence:
```
X & function [@ and ]
```
Stack:
```
use_file: fun_expr AMPERSAND FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 519

```
fun_expr: fun_expr AMPERAMPER . _*
```

### Sample 1

Sentence:
```
X &&
```
Stack:
```
use_file: fun_expr AMPERAMPER
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 520

```
fun_expr: fun_expr AMPERAMPER FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . _*
```

### Sample 1

Sentence:
```
X && function
```
Stack:
```
use_file: fun_expr AMPERAMPER FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 521

```
fun_expr: fun_expr AMPERAMPER FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```

### Sample 1

Sentence:
```
X && function [@ and ]
```
Stack:
```
use_file: fun_expr AMPERAMPER FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 522

```
fun_expr: additive . _*
```

### Sample 1

Sentence:
```
X . ( +
```
Stack:
```
use_file: mod_longident DOT LPAREN PLUS
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```

Also covered by these intermediate patterns:
```
additive: PLUS .
operator: PLUS .
```

## Pattern 523

```
fun_expr: additive FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . _*
```

### Sample 1

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
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 524

```
fun_expr: additive FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
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
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 525

```
fun_seq_expr: fun_expr SEMI PERCENT . attr_id seq_expr
```

### Sample 1

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
&& & and* ` ! | || |] 'a' : :: := :> , (*comment*) (**documentation *) . .. .+  
 = 1.0 > >} >] # ## != ^ +! land ** 1 ~label: { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let* ( >. .< .~ - -. -> ?label: % + +. += !+ ? ' {%%ext|s|} {%%%%ext|s|} } ] ) ; ;; * "s" ~ _
```


## Pattern 526

```
fun_seq_expr: fun_expr SEMI PERCENT attr_id . seq_expr
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
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```

Also covered by these intermediate patterns:
```
attr_id: single_attr_id . _*
single_attr_id: AND .
```

## Pattern 527

```
and_let_binding: AND . list_attribute_ let_binding_body list_post_item_attribute_
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 528

```
and_let_binding: AND list_attribute_ . let_binding_body list_post_item_attribute_
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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 529

```
strict_binding: EQUAL . seq_expr
```

### Sample 1

Sentence:
```
let* x =
```
Stack:
```
use_file: LETOP val_ident EQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 530

```
let_binding_body_no_punning: val_ident COLON . _*
type_constraint: COLON . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try val virtual when while with
```


## Pattern 531

```
let_binding_body_no_punning: val_ident COLON TYPE . nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL seq_expr
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 532

```
let_binding_body_no_punning: val_ident COLON TYPE nonempty_list_mkrhs_LIDENT__ . DOT core_type EQUAL seq_expr
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
nonempty_list_mkrhs_LIDENT__: LIDENT . _*
```

## Pattern 533

```
let_binding_body_no_punning: val_ident COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT . core_type EQUAL seq_expr
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 534

```
let_binding_body_no_punning: val_ident COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type . EQUAL seq_expr
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 535

```
let_binding_body_no_punning: val_ident COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL . seq_expr
```

### Sample 1

Sentence:
```
let x and x : type x . {%%ext|s|} =
```
Stack:
```
parse_expression: let_bindings_ext_ AND list_attribute_ val_ident COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 536

```
let_binding_body_no_punning: val_ident COLON reversed_nonempty_llist_typevar_ . DOT core_type EQUAL seq_expr
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
atomic_type: QUOTE ident .
reversed_nonempty_llist_typevar_: QUOTE ident .
ident: LIDENT .
```

## Pattern 537

```
let_binding_body_no_punning: val_ident COLON reversed_nonempty_llist_typevar_ DOT . core_type EQUAL seq_expr
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 538

```
let_binding_body_no_punning: val_ident COLON reversed_nonempty_llist_typevar_ DOT core_type . EQUAL seq_expr
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 539

```
let_binding_body_no_punning: val_ident COLON reversed_nonempty_llist_typevar_ DOT core_type EQUAL . seq_expr
```

### Sample 1

Sentence:
```
let x and x : ' x . {%%ext|s|} =
```
Stack:
```
parse_expression: let_bindings_ext_ AND list_attribute_ val_ident COLON reversed_nonempty_llist_typevar_ DOT core_type EQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 540

```
let_binding_body_no_punning: val_ident type_constraint . EQUAL seq_expr
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
type_constraint: COLON core_type COLONGREATER core_type .
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 541

```
let_binding_body_no_punning: val_ident type_constraint EQUAL . seq_expr
```

### Sample 1

Sentence:
```
let x and x : {%%ext|s|} =
```
Stack:
```
parse_expression: let_bindings_ext_ AND list_attribute_ val_ident type_constraint EQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 542

```
strict_binding: fun_params . option_type_constraint_ EQUAL fun_body
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
fun_params: reversed_nonempty_concat_fun_param_as_list_ .
reversed_nonempty_concat_fun_param_as_list_: reversed_nonempty_concat_fun_param_as_list_ fun_param_as_list .
fun_param_as_list: LPAREN TYPE nonempty_list_mkrhs_LIDENT__ RPAREN .
```

## Pattern 543

```
strict_binding: fun_params option_type_constraint_ . EQUAL fun_body
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
option_type_constraint_: type_constraint .
type_constraint: COLON core_type COLONGREATER core_type .
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 544

```
strict_binding: fun_params option_type_constraint_ EQUAL . fun_body
```

### Sample 1

Sentence:
```
let* x false =
```
Stack:
```
use_file: LETOP val_ident fun_params option_type_constraint_ EQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 545

```
let_binding_body_no_punning: simple_pattern_not_ident COLON . core_type EQUAL seq_expr
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 546

```
let_binding_body_no_punning: simple_pattern_not_ident COLON core_type . EQUAL seq_expr
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 547

```
let_binding_body_no_punning: simple_pattern_not_ident COLON core_type EQUAL . seq_expr
```

### Sample 1

Sentence:
```
let x and false : {%%ext|s|} =
```
Stack:
```
parse_expression: let_bindings_ext_ AND list_attribute_ simple_pattern_not_ident COLON core_type EQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 548

```
labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn . _*
let_binding_body_no_punning: pattern_no_exn . EQUAL seq_expr
reversed_labeled_tuple_pattern_pattern_no_exn_: pattern_no_exn . COMMA DOTDOT
```

### Sample 1

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
&& & and and* assert ` ! || |] begin 'a' class : := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
pattern_no_exn: pattern_no_exn AS val_ident .
val_ident: LIDENT .
```

## Pattern 549

```
let_binding_body_no_punning: pattern_no_exn EQUAL . seq_expr
```

### Sample 1

Sentence:
```
let x and false =
```
Stack:
```
parse_expression: let_bindings_ext_ AND list_attribute_ pattern_no_exn EQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 550

```
fun_expr: simple_expr DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ . _*
simple_expr: simple_expr DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ . RPAREN
```

### Sample 1

Sentence:
```
X .+ ( X ;
```
Stack:
```
use_file: simple_expr DOTOP LPAREN fun_expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
separated_or_terminated_nonempty_list_SEMI_expr_: fun_expr SEMI . _*
```

## Pattern 551

```
fun_expr: simple_expr DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN LESSMINUS . _*
```

### Sample 1

Sentence:
```
X .+ ( X ) <-
```
Stack:
```
use_file: simple_expr DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN LESSMINUS
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 552

```
fun_expr: simple_expr DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN LESSMINUS FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . _*
```

### Sample 1

Sentence:
```
X .+ ( X ) <- function
```
Stack:
```
use_file: simple_expr DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN LESSMINUS FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 553

```
fun_expr: simple_expr DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN LESSMINUS FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```

### Sample 1

Sentence:
```
X .+ ( X ) <- function [@ and ]
```
Stack:
```
use_file: simple_expr DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN LESSMINUS FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 554

```
fun_expr: simple_expr DOTOP LBRACKET . _*
simple_expr: simple_expr DOTOP LBRACKET . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET
```

### Sample 1

Sentence:
```
X .+ [
```
Stack:
```
use_file: simple_expr DOTOP LBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 555

```
fun_expr: simple_expr DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ . _*
simple_expr: simple_expr DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACKET
```

### Sample 1

Sentence:
```
X .+ [ X ;
```
Stack:
```
use_file: simple_expr DOTOP LBRACKET fun_expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
separated_or_terminated_nonempty_list_SEMI_expr_: fun_expr SEMI . _*
```

## Pattern 556

```
fun_expr: simple_expr DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS . _*
```

### Sample 1

Sentence:
```
X .+ [ X ] <-
```
Stack:
```
use_file: simple_expr DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 557

```
fun_expr: simple_expr DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . _*
```

### Sample 1

Sentence:
```
X .+ [ X ] <- function
```
Stack:
```
use_file: simple_expr DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 558

```
fun_expr: simple_expr DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```

### Sample 1

Sentence:
```
X .+ [ X ] <- function [@ and ]
```
Stack:
```
use_file: simple_expr DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 559

```
fun_expr: simple_expr DOTOP LBRACE . _*
simple_expr: simple_expr DOTOP LBRACE . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE
```

### Sample 1

Sentence:
```
X .+ {
```
Stack:
```
use_file: simple_expr DOTOP LBRACE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 560

```
fun_expr: simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ . _*
simple_expr: simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACE
```

### Sample 1

Sentence:
```
X .+ { X ;
```
Stack:
```
use_file: simple_expr DOTOP LBRACE fun_expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
separated_or_terminated_nonempty_list_SEMI_expr_: fun_expr SEMI . _*
```

## Pattern 561

```
fun_expr: simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS . _*
```

### Sample 1

Sentence:
```
X .+ { X } <-
```
Stack:
```
use_file: simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 562

```
fun_expr: simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . _*
```

### Sample 1

Sentence:
```
X .+ { X } <- function
```
Stack:
```
use_file: simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 563

```
fun_expr: simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```

### Sample 1

Sentence:
```
X .+ { X } <- function [@ and ]
```
Stack:
```
use_file: simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 564

```
fun_expr: simple_expr DOT . _*
simple_expr: simple_expr DOT . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 565

```
fun_expr: simple_expr DOT LPAREN . _*
simple_expr: simple_expr DOT LPAREN . seq_expr RPAREN
```

### Sample 1

Sentence:
```
false . (
```
Stack:
```
use_file: simple_expr DOT LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 566

```
fun_expr: simple_expr DOT LPAREN seq_expr . _*
simple_expr: simple_expr DOT LPAREN seq_expr . RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
seq_expr: fun_seq_expr .
fun_seq_expr: fun_expr SEMI . _*
```

## Pattern 567

```
fun_expr: simple_expr DOT LPAREN seq_expr RPAREN LESSMINUS . _*
```

### Sample 1

Sentence:
```
false . ( X ) <-
```
Stack:
```
use_file: simple_expr DOT LPAREN seq_expr RPAREN LESSMINUS
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 568

```
fun_expr: simple_expr DOT LPAREN seq_expr RPAREN LESSMINUS FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . _*
```

### Sample 1

Sentence:
```
false . ( X ) <- function
```
Stack:
```
use_file: simple_expr DOT LPAREN seq_expr RPAREN LESSMINUS FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 569

```
fun_expr: simple_expr DOT LPAREN seq_expr RPAREN LESSMINUS FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```

### Sample 1

Sentence:
```
false . ( X ) <- function [@ and ]
```
Stack:
```
use_file: simple_expr DOT LPAREN seq_expr RPAREN LESSMINUS FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 570

```
fun_expr: simple_expr DOT LBRACKET . _*
simple_expr: simple_expr DOT LBRACKET . seq_expr RBRACKET
```

### Sample 1

Sentence:
```
false . [
```
Stack:
```
use_file: simple_expr DOT LBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 571

```
fun_expr: simple_expr DOT LBRACKET seq_expr . _*
simple_expr: simple_expr DOT LBRACKET seq_expr . RBRACKET
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
seq_expr: fun_seq_expr .
fun_seq_expr: fun_expr SEMI . _*
```

## Pattern 572

```
fun_expr: simple_expr DOT LBRACKET seq_expr RBRACKET LESSMINUS . _*
```

### Sample 1

Sentence:
```
false . [ X ] <-
```
Stack:
```
use_file: simple_expr DOT LBRACKET seq_expr RBRACKET LESSMINUS
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 573

```
fun_expr: simple_expr DOT LBRACKET seq_expr RBRACKET LESSMINUS FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . _*
```

### Sample 1

Sentence:
```
false . [ X ] <- function
```
Stack:
```
use_file: simple_expr DOT LBRACKET seq_expr RBRACKET LESSMINUS FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 574

```
fun_expr: simple_expr DOT LBRACKET seq_expr RBRACKET LESSMINUS FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```

### Sample 1

Sentence:
```
false . [ X ] <- function [@ and ]
```
Stack:
```
use_file: simple_expr DOT LBRACKET seq_expr RBRACKET LESSMINUS FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 575

```
fun_expr: simple_expr DOT LBRACE . _*
simple_expr: simple_expr DOT LBRACE . seq_expr RBRACE
```

### Sample 1

Sentence:
```
false . {
```
Stack:
```
use_file: simple_expr DOT LBRACE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 576

```
fun_expr: simple_expr DOT LBRACE seq_expr . _*
simple_expr: simple_expr DOT LBRACE seq_expr . RBRACE
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
seq_expr: fun_seq_expr .
fun_seq_expr: fun_expr SEMI . _*
```

## Pattern 577

```
fun_expr: simple_expr DOT LBRACE seq_expr RBRACE LESSMINUS . _*
```

### Sample 1

Sentence:
```
false . { X } <-
```
Stack:
```
use_file: simple_expr DOT LBRACE seq_expr RBRACE LESSMINUS
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 578

```
fun_expr: simple_expr DOT LBRACE seq_expr RBRACE LESSMINUS FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . _*
```

### Sample 1

Sentence:
```
false . { X } <- function
```
Stack:
```
use_file: simple_expr DOT LBRACE seq_expr RBRACE LESSMINUS FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 579

```
fun_expr: simple_expr DOT LBRACE seq_expr RBRACE LESSMINUS FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```

### Sample 1

Sentence:
```
false . { X } <- function [@ and ]
```
Stack:
```
use_file: simple_expr DOT LBRACE seq_expr RBRACE LESSMINUS FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 580

```
fun_expr: simple_expr DOT mod_longident . _*
mk_longident_mod_longident_LIDENT_: mod_longident . DOT LIDENT
mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT
simple_expr: simple_expr DOT mod_longident . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
mod_longident: mk_longident_mod_longident_UIDENT_ .
mk_longident_mod_longident_UIDENT_: mod_longident DOT UIDENT .
```

## Pattern 581

```
fun_expr: simple_expr DOT mod_longident DOTOP . _*
simple_expr: simple_expr DOT mod_longident DOTOP . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 582

```
fun_expr: simple_expr DOT mod_longident DOTOP LPAREN . _*
simple_expr: simple_expr DOT mod_longident DOTOP LPAREN . separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN
```

### Sample 1

Sentence:
```
false . X .+ (
```
Stack:
```
use_file: simple_expr DOT mod_longident DOTOP LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 583

```
fun_expr: simple_expr DOT mod_longident DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ . _*
simple_expr: simple_expr DOT mod_longident DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ . RPAREN
```

### Sample 1

Sentence:
```
false . X .+ ( X ;
```
Stack:
```
use_file: simple_expr DOT mod_longident DOTOP LPAREN fun_expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
separated_or_terminated_nonempty_list_SEMI_expr_: fun_expr SEMI . _*
```

## Pattern 584

```
fun_expr: simple_expr DOT mod_longident DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN LESSMINUS . _*
```

### Sample 1

Sentence:
```
false . X .+ ( X ) <-
```
Stack:
```
use_file: simple_expr DOT mod_longident DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN LESSMINUS
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 585

```
fun_expr: simple_expr DOT mod_longident DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN LESSMINUS FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . _*
```

### Sample 1

Sentence:
```
false . X .+ ( X ) <- function
```
Stack:
```
use_file: simple_expr DOT mod_longident DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN LESSMINUS FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 586

```
fun_expr: simple_expr DOT mod_longident DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN LESSMINUS FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```

### Sample 1

Sentence:
```
false . X .+ ( X ) <- function [@ and ]
```
Stack:
```
use_file: simple_expr DOT mod_longident DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN LESSMINUS FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 587

```
fun_expr: simple_expr DOT mod_longident DOTOP LBRACKET . _*
simple_expr: simple_expr DOT mod_longident DOTOP LBRACKET . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET
```

### Sample 1

Sentence:
```
false . X .+ [
```
Stack:
```
use_file: simple_expr DOT mod_longident DOTOP LBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 588

```
fun_expr: simple_expr DOT mod_longident DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ . _*
simple_expr: simple_expr DOT mod_longident DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACKET
```

### Sample 1

Sentence:
```
false . X .+ [ X ;
```
Stack:
```
use_file: simple_expr DOT mod_longident DOTOP LBRACKET fun_expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
separated_or_terminated_nonempty_list_SEMI_expr_: fun_expr SEMI . _*
```

## Pattern 589

```
fun_expr: simple_expr DOT mod_longident DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS . _*
```

### Sample 1

Sentence:
```
false . X .+ [ X ] <-
```
Stack:
```
use_file: simple_expr DOT mod_longident DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 590

```
fun_expr: simple_expr DOT mod_longident DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . _*
```

### Sample 1

Sentence:
```
false . X .+ [ X ] <- function
```
Stack:
```
use_file: simple_expr DOT mod_longident DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 591

```
fun_expr: simple_expr DOT mod_longident DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```

### Sample 1

Sentence:
```
false . X .+ [ X ] <- function [@ and ]
```
Stack:
```
use_file: simple_expr DOT mod_longident DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 592

```
fun_expr: simple_expr DOT mod_longident DOTOP LBRACE . _*
simple_expr: simple_expr DOT mod_longident DOTOP LBRACE . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE
```

### Sample 1

Sentence:
```
false . X .+ {
```
Stack:
```
use_file: simple_expr DOT mod_longident DOTOP LBRACE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 593

```
fun_expr: simple_expr DOT mod_longident DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ . _*
simple_expr: simple_expr DOT mod_longident DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACE
```

### Sample 1

Sentence:
```
false . X .+ { X ;
```
Stack:
```
use_file: simple_expr DOT mod_longident DOTOP LBRACE fun_expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
separated_or_terminated_nonempty_list_SEMI_expr_: fun_expr SEMI . _*
```

## Pattern 594

```
fun_expr: simple_expr DOT mod_longident DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS . _*
```

### Sample 1

Sentence:
```
false . X .+ { X } <-
```
Stack:
```
use_file: simple_expr DOT mod_longident DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 595

```
fun_expr: simple_expr DOT mod_longident DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . _*
```

### Sample 1

Sentence:
```
false . X .+ { X } <- function
```
Stack:
```
use_file: simple_expr DOT mod_longident DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 596

```
fun_expr: simple_expr DOT mod_longident DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```

### Sample 1

Sentence:
```
false . X .+ { X } <- function [@ and ]
```
Stack:
```
use_file: simple_expr DOT mod_longident DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 597

```
fun_expr: simple_expr DOT label_longident LESSMINUS . _*
```

### Sample 1

Sentence:
```
false . x <-
```
Stack:
```
use_file: simple_expr DOT label_longident LESSMINUS
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 598

```
fun_expr: simple_expr DOT label_longident LESSMINUS FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . _*
```

### Sample 1

Sentence:
```
false . x <- function
```
Stack:
```
use_file: simple_expr DOT label_longident LESSMINUS FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 599

```
fun_expr: simple_expr DOT label_longident LESSMINUS FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```

### Sample 1

Sentence:
```
false . x <- function [@ and ]
```
Stack:
```
use_file: simple_expr DOT label_longident LESSMINUS FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 600

```
fun_expr: FOR ext list_attribute_ pattern EQUAL seq_expr . direction_flag seq_expr DO seq_expr DONE
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
seq_expr: fun_seq_expr .
fun_seq_expr: fun_expr SEMI . _*
```

## Pattern 601

```
fun_expr: FOR ext list_attribute_ pattern EQUAL seq_expr direction_flag . seq_expr DO seq_expr DONE
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
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```

Also covered by these intermediate patterns:
```
direction_flag: DOWNTO .
```

## Pattern 602

```
fun_expr: FOR ext list_attribute_ pattern EQUAL seq_expr direction_flag seq_expr . DO seq_expr DONE
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
seq_expr: fun_seq_expr .
fun_seq_expr: fun_expr SEMI . _*
```

## Pattern 603

```
fun_expr: FOR ext list_attribute_ pattern EQUAL seq_expr direction_flag seq_expr DO . seq_expr DONE
```

### Sample 1

Sentence:
```
for false = X downto X do
```
Stack:
```
use_file: FOR ext list_attribute_ pattern EQUAL seq_expr direction_flag seq_expr DO
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 604

```
fun_expr: FOR ext list_attribute_ pattern EQUAL seq_expr direction_flag seq_expr DO seq_expr . DONE
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
seq_expr: fun_seq_expr .
fun_seq_expr: fun_expr SEMI . _*
```

## Pattern 605

```
labeled_tuple_pat_element_list_pattern_no_exn_: pattern_no_exn . _*
letop_binding_body: pattern_no_exn . EQUAL seq_expr
reversed_labeled_tuple_pattern_pattern_no_exn_: pattern_no_exn . COMMA DOTDOT
```

### Sample 1

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
&& & and and* assert ` ! || |] begin 'a' class : := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
pattern_no_exn: pattern_no_exn AS val_ident .
val_ident: LIDENT .
```

## Pattern 606

```
letop_binding_body: pattern_no_exn EQUAL . seq_expr
```

### Sample 1

Sentence:
```
let* false =
```
Stack:
```
use_file: LETOP pattern_no_exn EQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 607

```
fun_expr: LETOP letop_bindings . IN seq_expr
```

### Sample 1

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
&& & and as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
letop_bindings: letop_bindings ANDOP letop_binding_body .
letop_binding_body: val_ident . _*
simple_pattern: val_ident .
val_ident: LIDENT .
```

## Pattern 608

```
fun_expr: LETOP letop_bindings IN . seq_expr
```

### Sample 1

Sentence:
```
let* x in
```
Stack:
```
use_file: LETOP letop_bindings IN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 609

```
letop_bindings: letop_bindings ANDOP . letop_binding_body
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 610

```
simple_expr: mod_longident DOT LPAREN seq_expr . RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
seq_expr: fun_seq_expr .
fun_seq_expr: fun_expr SEMI . _*
```

## Pattern 611

```
simple_expr: mod_longident DOT LBRACKETBAR . _*
```

### Sample 1

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
&& & and and* as | || class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 612

```
simple_expr: mod_longident DOT LBRACKETBAR separated_or_terminated_nonempty_list_SEMI_expr_ . BARRBRACKET
```

### Sample 1

Sentence:
```
X . [| X ;
```
Stack:
```
use_file: mod_longident DOT LBRACKETBAR fun_expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
separated_or_terminated_nonempty_list_SEMI_expr_: fun_expr SEMI . _*
```

## Pattern 613

```
simple_expr: mod_longident DOT LBRACKET . _*
```

### Sample 1

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
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 614

```
simple_expr: mod_longident DOT LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACKET
```

### Sample 1

Sentence:
```
X . [ X ;
```
Stack:
```
use_file: mod_longident DOT LBRACKET fun_expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
separated_or_terminated_nonempty_list_SEMI_expr_: fun_expr SEMI . _*
```

## Pattern 615

```
simple_expr: mod_longident DOT LBRACELESS . separated_or_terminated_nonempty_list_SEMI_object_expr_field_ GREATERRBRACE
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 616

```
simple_expr: mod_longident DOT LBRACELESS separated_or_terminated_nonempty_list_SEMI_object_expr_field_ . GREATERRBRACE
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
separated_or_terminated_nonempty_list_SEMI_object_expr_field_: LIDENT option_preceded_EQUAL_expr__ SEMI . _*
```

## Pattern 617

```
simple_expr: mod_longident DOT LBRACE . record_expr_content RBRACE
```

### Sample 1

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
&& & and and* as assert | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy [@ [@@ [@@@ [> [< [%% < <- let let* match >. method - -. -> module mutable nonrec of open ?label: or % + +. += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type _ val virtual when while with
```


## Pattern 618

```
record_expr_content: simple_expr . WITH separated_or_terminated_nonempty_list_SEMI_record_expr_field_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

Also covered by these intermediate patterns:
```
simple_expr: OBJECT ext list_attribute_ class_self_pattern list_text_cstr_class_field__ END .
```

## Pattern 619

```
record_expr_content: simple_expr WITH . separated_or_terminated_nonempty_list_SEMI_record_expr_field_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 620

```
simple_expr: mod_longident DOT LBRACE record_expr_content . RBRACE
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
record_expr_content: separated_or_terminated_nonempty_list_SEMI_record_expr_field_ .
separated_or_terminated_nonempty_list_SEMI_record_expr_field_: label_longident option_type_constraint_ option_preceded_EQUAL_expr__ SEMI . _*
```

## Pattern 621

```
constr_longident: mod_longident DOT . LPAREN COLONCOLON RPAREN
mk_longident_mod_longident_LIDENT_: mod_longident DOT . LIDENT
mk_longident_mod_longident_UIDENT_: mod_longident DOT . UIDENT
mk_longident_mod_longident_val_ident_: mod_longident DOT . val_ident
simple_expr: mod_longident DOT . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy [@ [@@ [@@@ [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 622

```
reversed_labeled_tuple_body: FUNCTION . _*
reversed_labeled_tuple_body: LABEL simple_expr COMMA FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
```

### Sample 1

Sentence:
```
~label: 'a' , function
```
Stack:
```
use_file: LABEL simple_expr COMMA FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 623

```
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
reversed_labeled_tuple_body: LABEL simple_expr COMMA FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
```

### Sample 1

Sentence:
```
~label: 'a' , function [@ and ]
```
Stack:
```
use_file: LABEL simple_expr COMMA FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 624

```
reversed_labeled_tuple_body: FUNCTION . _*
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COMMA FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . _*
```

### Sample 1

Sentence:
```
function false -> . , function
```
Stack:
```
use_file: FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COMMA FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 625

```
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COMMA FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```

### Sample 1

Sentence:
```
function false -> . , function [@ and ]
```
Stack:
```
use_file: FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COMMA FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 626

```
match_case: pattern WHEN seq_expr . MINUSGREATER seq_expr
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
seq_expr: fun_seq_expr .
fun_seq_expr: fun_expr SEMI . _*
```

## Pattern 627

```
match_case: pattern WHEN seq_expr MINUSGREATER . seq_expr
```

### Sample 1

Sentence:
```
function false when X ->
```
Stack:
```
use_file: FUNCTION ext list_attribute_ pattern WHEN seq_expr MINUSGREATER
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 628

```
match_case: pattern MINUSGREATER . _*
```

### Sample 1

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
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 629

```
fun_expr: IF ext list_attribute_ seq_expr . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
seq_expr: fun_seq_expr .
fun_seq_expr: fun_expr SEMI . _*
```

## Pattern 630

```
fun_expr: IF ext list_attribute_ seq_expr THEN . _*
```

### Sample 1

Sentence:
```
if X then
```
Stack:
```
use_file: IF ext list_attribute_ seq_expr THEN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 631

```
fun_expr: IF ext list_attribute_ seq_expr THEN FUNCTION . _*
reversed_labeled_tuple_body: FUNCTION . _*
```

### Sample 1

Sentence:
```
if X then function
```
Stack:
```
use_file: IF ext list_attribute_ seq_expr THEN FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 632

```
fun_expr: IF ext list_attribute_ seq_expr THEN FUNCTION ext list_attribute_ . _*
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```

### Sample 1

Sentence:
```
if X then function [@ and ]
```
Stack:
```
use_file: IF ext list_attribute_ seq_expr THEN FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 633

```
fun_expr: IF ext list_attribute_ seq_expr THEN FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ ELSE . _*
```

### Sample 1

Sentence:
```
if X then function false -> X else
```
Stack:
```
use_file: IF ext list_attribute_ seq_expr THEN FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ ELSE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 634

```
fun_expr: IF ext list_attribute_ seq_expr THEN FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ ELSE FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . _*
```

### Sample 1

Sentence:
```
if X then function false -> X else function
```
Stack:
```
use_file: IF ext list_attribute_ seq_expr THEN FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ ELSE FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 635

```
fun_expr: IF ext list_attribute_ seq_expr THEN FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ ELSE FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```

### Sample 1

Sentence:
```
if X then function false -> X else function [@ and ]
```
Stack:
```
use_file: IF ext list_attribute_ seq_expr THEN FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ ELSE FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 636

```
fun_expr: IF ext list_attribute_ seq_expr THEN fun_expr ELSE . _*
```

### Sample 1

Sentence:
```
if X then X else
```
Stack:
```
use_file: IF ext list_attribute_ seq_expr THEN fun_expr ELSE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 637

```
fun_expr: IF ext list_attribute_ seq_expr THEN fun_expr ELSE FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . _*
```

### Sample 1

Sentence:
```
if X then X else function
```
Stack:
```
use_file: IF ext list_attribute_ seq_expr THEN fun_expr ELSE FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 638

```
fun_expr: IF ext list_attribute_ seq_expr THEN fun_expr ELSE FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```

### Sample 1

Sentence:
```
if X then X else function [@ and ]
```
Stack:
```
use_file: IF ext list_attribute_ seq_expr THEN fun_expr ELSE FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 639

```
simple_expr: simple_expr DOT LPAREN seq_expr . RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
seq_expr: fun_seq_expr .
fun_seq_expr: fun_expr SEMI . _*
```

## Pattern 640

```
simple_expr: simple_expr DOT LBRACKET . seq_expr RBRACKET
```

### Sample 1

Sentence:
```
{ 'a' . [
```
Stack:
```
use_file: LBRACE simple_expr DOT LBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 641

```
simple_expr: simple_expr DOT LBRACKET seq_expr . RBRACKET
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
seq_expr: fun_seq_expr .
fun_seq_expr: fun_expr SEMI . _*
```

## Pattern 642

```
simple_expr: simple_expr DOT LBRACE . seq_expr RBRACE
```

### Sample 1

Sentence:
```
{ 'a' . {
```
Stack:
```
use_file: LBRACE simple_expr DOT LBRACE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 643

```
simple_expr: simple_expr DOT LBRACE seq_expr . RBRACE
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
seq_expr: fun_seq_expr .
fun_seq_expr: fun_expr SEMI . _*
```

## Pattern 644

```
mk_longident_mod_longident_LIDENT_: mod_longident . DOT LIDENT
mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT
simple_expr: simple_expr DOT mod_longident . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
mod_longident: mk_longident_mod_longident_UIDENT_ .
mk_longident_mod_longident_UIDENT_: mod_longident DOT UIDENT .
```

## Pattern 645

```
simple_expr: simple_expr DOT mod_longident DOTOP . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 646

```
simple_expr: simple_expr DOT mod_longident DOTOP LPAREN . separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN
```

### Sample 1

Sentence:
```
{ 'a' . X .+ (
```
Stack:
```
use_file: LBRACE simple_expr DOT mod_longident DOTOP LPAREN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 647

```
simple_expr: simple_expr DOT mod_longident DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ . RPAREN
```

### Sample 1

Sentence:
```
{ 'a' . X .+ ( X ;
```
Stack:
```
use_file: LBRACE simple_expr DOT mod_longident DOTOP LPAREN fun_expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
separated_or_terminated_nonempty_list_SEMI_expr_: fun_expr SEMI . _*
```

## Pattern 648

```
simple_expr: simple_expr DOT mod_longident DOTOP LBRACKET . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET
```

### Sample 1

Sentence:
```
{ 'a' . X .+ [
```
Stack:
```
use_file: LBRACE simple_expr DOT mod_longident DOTOP LBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 649

```
simple_expr: simple_expr DOT mod_longident DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACKET
```

### Sample 1

Sentence:
```
{ 'a' . X .+ [ X ;
```
Stack:
```
use_file: LBRACE simple_expr DOT mod_longident DOTOP LBRACKET fun_expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
separated_or_terminated_nonempty_list_SEMI_expr_: fun_expr SEMI . _*
```

## Pattern 650

```
simple_expr: simple_expr DOT mod_longident DOTOP LBRACE . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE
```

### Sample 1

Sentence:
```
{ 'a' . X .+ {
```
Stack:
```
use_file: LBRACE simple_expr DOT mod_longident DOTOP LBRACE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 651

```
simple_expr: simple_expr DOT mod_longident DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACE
```

### Sample 1

Sentence:
```
{ 'a' . X .+ { X ;
```
Stack:
```
use_file: LBRACE simple_expr DOT mod_longident DOTOP LBRACE fun_expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
separated_or_terminated_nonempty_list_SEMI_expr_: fun_expr SEMI . _*
```

## Pattern 652

```
simple_expr: simple_expr DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ . RPAREN
```

### Sample 1

Sentence:
```
{ 'a' .+ ( X ;
```
Stack:
```
use_file: LBRACE simple_expr DOTOP LPAREN fun_expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
separated_or_terminated_nonempty_list_SEMI_expr_: fun_expr SEMI . _*
```

## Pattern 653

```
simple_expr: simple_expr DOTOP LBRACKET . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET
```

### Sample 1

Sentence:
```
{ 'a' .+ [
```
Stack:
```
use_file: LBRACE simple_expr DOTOP LBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 654

```
simple_expr: simple_expr DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACKET
```

### Sample 1

Sentence:
```
{ 'a' .+ [ X ;
```
Stack:
```
use_file: LBRACE simple_expr DOTOP LBRACKET fun_expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
separated_or_terminated_nonempty_list_SEMI_expr_: fun_expr SEMI . _*
```

## Pattern 655

```
simple_expr: simple_expr DOTOP LBRACE . separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE
```

### Sample 1

Sentence:
```
{ 'a' .+ {
```
Stack:
```
use_file: LBRACE simple_expr DOTOP LBRACE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 656

```
simple_expr: simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACE
```

### Sample 1

Sentence:
```
{ 'a' .+ { X ;
```
Stack:
```
use_file: LBRACE simple_expr DOTOP LBRACE fun_expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
separated_or_terminated_nonempty_list_SEMI_expr_: fun_expr SEMI . _*
```

## Pattern 657

```
simple_expr: BEGIN ext list_attribute_ seq_expr . END
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
seq_expr: fun_seq_expr .
fun_seq_expr: fun_expr SEMI . _*
```

## Pattern 658

```
simple_expr: LBRACE record_expr_content . RBRACE
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
record_expr_content: separated_or_terminated_nonempty_list_SEMI_record_expr_field_ .
separated_or_terminated_nonempty_list_SEMI_record_expr_field_: label_longident option_type_constraint_ option_preceded_EQUAL_expr__ SEMI . _*
```

## Pattern 659

```
option_preceded_EQUAL_expr__: EQUAL FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . _*
```

### Sample 1

Sentence:
```
{< x = function
```
Stack:
```
use_file: LBRACELESS LIDENT EQUAL FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 660

```
option_preceded_EQUAL_expr__: EQUAL FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```

### Sample 1

Sentence:
```
{< x = function [@ and ]
```
Stack:
```
use_file: LBRACELESS LIDENT EQUAL FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 661

```
simple_expr: LBRACELESS separated_or_terminated_nonempty_list_SEMI_object_expr_field_ . GREATERRBRACE
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
separated_or_terminated_nonempty_list_SEMI_object_expr_field_: LIDENT option_preceded_EQUAL_expr__ SEMI . _*
```

## Pattern 662

```
simple_expr: LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ . RBRACKET
```

### Sample 1

Sentence:
```
[ X ;
```
Stack:
```
use_file: LBRACKET fun_expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
separated_or_terminated_nonempty_list_SEMI_expr_: fun_expr SEMI . _*
```

## Pattern 663

```
floating_attribute: LBRACKETATATAT . attr_id attr_payload RBRACKET
```

### Sample 1

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
&& & and* ` ! | || |] 'a' : :: := :> , (*comment*) (**documentation *) . .. .+  
 = 1.0 > >} >] # ## != ^ +! land ** 1 ~label: { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let* ( >. .< .~ - -. -> ?label: % + +. += !+ ? ' {%%ext|s|} {%%%%ext|s|} } ] ) ; ;; * "s" ~ _
```


## Pattern 664

```
floating_attribute: LBRACKETATATAT attr_id . attr_payload RBRACKET
```

### Sample 1

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
&& & and and* as | || |] :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = functor > >} >] # ## in != ^ +! land ** inherit initializer [@ [@@ [> [< < <- >. method -> mutable nonrec of ?label: or % += private ' } rec ) ; sig * struct then to virtual when with
```

Also covered by these intermediate patterns:
```
attr_id: single_attr_id . _*
single_attr_id: AND .
```

## Pattern 665

```
structure_item: INCLUDE . ext list_attribute_ module_expr list_post_item_attribute_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ val virtual when while with
```


## Pattern 666

```
structure_item: INCLUDE ext list_attribute_ . module_expr list_post_item_attribute_
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 667

```
primitive_declaration: EXTERNAL . ext list_attribute_ val_ident COLON possibly_poly_core_type_ EQUAL nonempty_list_raw_string_ list_post_item_attribute_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 668

```
primitive_declaration: EXTERNAL ext list_attribute_ . val_ident COLON possibly_poly_core_type_ EQUAL nonempty_list_raw_string_ list_post_item_attribute_
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 669

```
primitive_declaration: EXTERNAL ext list_attribute_ val_ident . COLON possibly_poly_core_type_ EQUAL nonempty_list_raw_string_ list_post_item_attribute_
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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
val_ident: LIDENT .
```

## Pattern 670

```
primitive_declaration: EXTERNAL ext list_attribute_ val_ident COLON . possibly_poly_core_type_ EQUAL nonempty_list_raw_string_ list_post_item_attribute_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 671

```
primitive_declaration: EXTERNAL ext list_attribute_ val_ident COLON possibly_poly_core_type_ . EQUAL nonempty_list_raw_string_ list_post_item_attribute_
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
possibly_poly_core_type_: core_type .
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 672

```
primitive_declaration: EXTERNAL ext list_attribute_ val_ident COLON possibly_poly_core_type_ EQUAL . nonempty_list_raw_string_ list_post_item_attribute_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to true try type X _ val virtual when while with
```


## Pattern 673

```
sig_exception_declaration: EXCEPTION . ext list_attribute_ constr_ident generalized_constructor_arguments list_attribute_ list_post_item_attribute_
str_exception_declaration: EXCEPTION . ext list_attribute_ constr_ident EQUAL constr_longident list_attribute_ list_post_item_attribute_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```


## Pattern 674

```
sig_exception_declaration: EXCEPTION ext list_attribute_ . constr_ident generalized_constructor_arguments list_attribute_ list_post_item_attribute_
str_exception_declaration: EXCEPTION ext list_attribute_ . constr_ident EQUAL constr_longident list_attribute_ list_post_item_attribute_
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 675

```
constr_extra_nonprefix_ident: LPAREN . RPAREN
constr_ident: LPAREN . COLONCOLON RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 676

```
constr_ident: LPAREN COLONCOLON . RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 677

```
constr_extra_nonprefix_ident: LBRACKET . RBRACKET
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 678

```
generalized_constructor_arguments: OF . constructor_arguments
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 679

```
constructor_arguments: LBRACE . label_declarations RBRACE
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 680

```
label_declaration: mutable_flag . LIDENT COLON possibly_poly_core_type_no_attr_ list_attribute_
label_declaration_semi: mutable_flag . LIDENT COLON possibly_poly_core_type_no_attr_ list_attribute_ SEMI list_attribute_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
mutable_flag: MUTABLE .
```

## Pattern 681

```
label_declaration: mutable_flag LIDENT . COLON possibly_poly_core_type_no_attr_ list_attribute_
label_declaration_semi: mutable_flag LIDENT . COLON possibly_poly_core_type_no_attr_ list_attribute_ SEMI list_attribute_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 682

```
label_declaration: mutable_flag LIDENT COLON . possibly_poly_core_type_no_attr_ list_attribute_
label_declaration_semi: mutable_flag LIDENT COLON . possibly_poly_core_type_no_attr_ list_attribute_ SEMI list_attribute_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 683

```
possibly_poly_core_type_no_attr_: reversed_nonempty_llist_typevar_ . DOT alias_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
atomic_type: QUOTE ident .
reversed_nonempty_llist_typevar_: QUOTE ident .
ident: LIDENT .
```

## Pattern 684

```
possibly_poly_core_type_no_attr_: reversed_nonempty_llist_typevar_ DOT . alias_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 685

```
constructor_arguments: LBRACE label_declarations . RBRACE
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
label_declarations: label_declaration_semi . _*
label_declaration_semi: mutable_flag LIDENT COLON possibly_poly_core_type_no_attr_ list_attribute_ SEMI . list_attribute_
```

## Pattern 686

```
constructor_arguments: reversed_separated_nonempty_llist_STAR_atomic_type_ . STAR atomic_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
constructor_arguments: atomic_type .
reversed_separated_nonempty_llist_STAR_atomic_type_: atomic_type .
atomic_type: UNDERSCORE .
```

## Pattern 687

```
constructor_arguments: reversed_separated_nonempty_llist_STAR_atomic_type_ STAR . atomic_type
reversed_separated_nonempty_llist_STAR_atomic_type_: reversed_separated_nonempty_llist_STAR_atomic_type_ STAR . atomic_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 688

```
str_exception_declaration: EXCEPTION ext list_attribute_ constr_ident EQUAL . constr_longident list_attribute_ list_post_item_attribute_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```


## Pattern 689

```
constr_extra_nonprefix_ident: LPAREN . RPAREN
constr_longident: LPAREN . COLONCOLON RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 690

```
constr_longident: mod_longident DOT . LPAREN COLONCOLON RPAREN
mk_longident_mod_longident_UIDENT_: mod_longident DOT . UIDENT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 691

```
constr_longident: mod_longident DOT LPAREN . COLONCOLON RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 692

```
generalized_constructor_arguments: COLON . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 693

```
generalized_constructor_arguments: COLON reversed_nonempty_llist_typevar_ . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
atomic_type: QUOTE ident .
reversed_nonempty_llist_typevar_: QUOTE ident .
ident: LIDENT .
```

## Pattern 694

```
generalized_constructor_arguments: COLON reversed_nonempty_llist_typevar_ DOT . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 695

```
generalized_constructor_arguments: COLON reversed_nonempty_llist_typevar_ DOT constructor_arguments . MINUSGREATER atomic_type
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
constructor_arguments: LBRACE label_declarations RBRACE .
```

## Pattern 696

```
generalized_constructor_arguments: COLON reversed_nonempty_llist_typevar_ DOT constructor_arguments MINUSGREATER . atomic_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 697

```
generalized_constructor_arguments: COLON constructor_arguments . MINUSGREATER atomic_type
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
constructor_arguments: LBRACE label_declarations RBRACE .
```

## Pattern 698

```
generalized_constructor_arguments: COLON constructor_arguments MINUSGREATER . atomic_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 699

```
open_description: OPEN . _*
```

### Sample 1

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
&& & and and* as assert ` | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 700

```
open_description: OPEN BANG . ext list_attribute_ mod_ext_longident list_post_item_attribute_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 701

```
open_description: OPEN BANG ext list_attribute_ . mod_ext_longident list_post_item_attribute_
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 702

```
open_description: OPEN ext list_attribute_ . mod_ext_longident list_post_item_attribute_
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 703

```
module_subst: MODULE . ext list_attribute_ UIDENT COLONEQUAL mod_ext_longident list_post_item_attribute_
module_type_declaration: MODULE . TYPE ext list_attribute_ ident option_preceded_EQUAL_module_type__ list_post_item_attribute_
module_type_subst: MODULE . TYPE ext list_attribute_ ident COLONEQUAL module_type list_post_item_attribute_
signature_item: MODULE . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] ) ; ;; sig * "s" struct then ~ to true try val virtual when while with
```


## Pattern 704

```
module_type_declaration: MODULE TYPE . ext list_attribute_ ident option_preceded_EQUAL_module_type__ list_post_item_attribute_
module_type_subst: MODULE TYPE . ext list_attribute_ ident COLONEQUAL module_type list_post_item_attribute_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 705

```
module_type_declaration: MODULE TYPE ext list_attribute_ . ident option_preceded_EQUAL_module_type__ list_post_item_attribute_
module_type_subst: MODULE TYPE ext list_attribute_ . ident COLONEQUAL module_type list_post_item_attribute_
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 706

```
module_type_subst: MODULE TYPE ext list_attribute_ ident COLONEQUAL . module_type list_post_item_attribute_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 707

```
module_subst: MODULE ext list_attribute_ . UIDENT COLONEQUAL mod_ext_longident list_post_item_attribute_
signature_item: MODULE ext list_attribute_ . _*
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 708

```
module_subst: MODULE ext list_attribute_ UIDENT COLONEQUAL . mod_ext_longident list_post_item_attribute_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 709

```
signature_item: MODULE ext list_attribute_ REC . module_name COLON module_type list_post_item_attribute_ list_and_module_declaration_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 710

```
signature_item: MODULE ext list_attribute_ REC module_name . COLON module_type list_post_item_attribute_ list_and_module_declaration_
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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
module_name: UIDENT .
```

## Pattern 711

```
signature_item: MODULE ext list_attribute_ REC module_name COLON . module_type list_post_item_attribute_ list_and_module_declaration_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 712

```
list_and_module_declaration_: AND . list_attribute_ module_name COLON module_type list_post_item_attribute_ list_and_module_declaration_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 713

```
list_and_module_declaration_: AND list_attribute_ . module_name COLON module_type list_post_item_attribute_ list_and_module_declaration_
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 714

```
list_and_module_declaration_: AND list_attribute_ module_name . COLON module_type list_post_item_attribute_ list_and_module_declaration_
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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
module_name: UIDENT .
```

## Pattern 715

```
list_and_module_declaration_: AND list_attribute_ module_name COLON . module_type list_post_item_attribute_ list_and_module_declaration_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 716

```
signature_item: MODULE ext list_attribute_ module_name . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
module_name: UIDENT .
module_subst: MODULE ext list_attribute_ UIDENT . COLONEQUAL mod_ext_longident list_post_item_attribute_
```

## Pattern 717

```
signature_item: MODULE ext list_attribute_ module_name EQUAL . mod_longident list_post_item_attribute_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 718

```
module_declaration_body: COLON . module_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 719

```
module_declaration_body: functor_arg . module_declaration_body
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
functor_arg: LPAREN module_name COLON module_type RPAREN .
```

## Pattern 720

```
signature_item: INCLUDE . ext list_attribute_ module_type list_post_item_attribute_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 721

```
signature_item: INCLUDE ext list_attribute_ . module_type list_post_item_attribute_
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 722

```
sig_exception_declaration: EXCEPTION . ext list_attribute_ constr_ident generalized_constructor_arguments list_attribute_ list_post_item_attribute_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```


## Pattern 723

```
sig_exception_declaration: EXCEPTION ext list_attribute_ . constr_ident generalized_constructor_arguments list_attribute_ list_post_item_attribute_
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 724

```
class_type_declarations: CLASS . TYPE ext list_attribute_ virtual_flag formal_class_parameters LIDENT EQUAL class_signature list_post_item_attribute_ list_and_class_type_declaration_
signature_item: CLASS . ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON class_type list_post_item_attribute_ list_and_class_description_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try X _ val when while with
```


## Pattern 725

```
class_type_declarations: CLASS TYPE . ext list_attribute_ virtual_flag formal_class_parameters LIDENT EQUAL class_signature list_post_item_attribute_ list_and_class_type_declaration_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val when while with
```


## Pattern 726

```
formal_class_parameters: LBRACKET reversed_separated_nonempty_llist_COMMA_type_parameter_ . RBRACKET
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
reversed_separated_nonempty_llist_COMMA_type_parameter_: reversed_separated_nonempty_llist_COMMA_type_parameter_ COMMA type_parameter .
type_parameter: type_variance type_variable .
type_variable: UNDERSCORE .
```

## Pattern 727

```
class_type_declarations: CLASS TYPE ext list_attribute_ virtual_flag formal_class_parameters . LIDENT EQUAL class_signature list_post_item_attribute_ list_and_class_type_declaration_
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
formal_class_parameters: LBRACKET reversed_separated_nonempty_llist_COMMA_type_parameter_ RBRACKET .
```

## Pattern 728

```
class_type_declarations: CLASS TYPE ext list_attribute_ virtual_flag formal_class_parameters LIDENT . EQUAL class_signature list_post_item_attribute_ list_and_class_type_declaration_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 729

```
class_type_declarations: CLASS TYPE ext list_attribute_ virtual_flag formal_class_parameters LIDENT EQUAL . class_signature list_post_item_attribute_ list_and_class_type_declaration_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [%% < <- let* ( match >. .< .~ method - -. -> module mutable new nonrec of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 730

```
class_signature: OBJECT . list_attribute_ class_self_type list_text_csig_class_sig_field__ END
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) do (**documentation *) done . .. .+ downto effect else  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** initializer 1 ~label: lazy { {< [ [@@ [| [> [< [% < <- let let* x match >. .< .~ - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ virtual when while with
```


## Pattern 731

```
class_self_type: LPAREN . core_type RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 732

```
class_self_type: LPAREN core_type . RPAREN
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 733

```
class_signature: OBJECT list_attribute_ class_self_type . list_text_csig_class_sig_field__ END
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) do (**documentation *) done . .. .+ downto effect else  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** initializer 1 ~label: lazy { {< [ [@ [@@ [| [> [< [% < <- let let* x ( match >. .< .~ - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ virtual when while with
```

Also covered by these intermediate patterns:
```
class_self_type: LPAREN core_type RPAREN .
```

## Pattern 734

```
class_sig_field: VAL . list_attribute_ mutable_virtual_flags LIDENT COLON core_type list_post_item_attribute_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val when while with
```


## Pattern 735

```
class_sig_field: VAL list_attribute_ mutable_virtual_flags . LIDENT COLON core_type list_post_item_attribute_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
mutable_virtual_flags: MUTABLE VIRTUAL .
```

## Pattern 736

```
class_sig_field: VAL list_attribute_ mutable_virtual_flags LIDENT . COLON core_type list_post_item_attribute_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 737

```
class_sig_field: VAL list_attribute_ mutable_virtual_flags LIDENT COLON . core_type list_post_item_attribute_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 738

```
class_sig_field: METHOD . list_attribute_ private_virtual_flags LIDENT COLON possibly_poly_core_type_ list_post_item_attribute_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val when while with
```


## Pattern 739

```
class_sig_field: METHOD list_attribute_ private_virtual_flags . LIDENT COLON possibly_poly_core_type_ list_post_item_attribute_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
private_virtual_flags: PRIVATE VIRTUAL .
```

## Pattern 740

```
class_sig_field: METHOD list_attribute_ private_virtual_flags LIDENT . COLON possibly_poly_core_type_ list_post_item_attribute_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 741

```
class_sig_field: METHOD list_attribute_ private_virtual_flags LIDENT COLON . possibly_poly_core_type_ list_post_item_attribute_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 742

```
class_sig_field: INHERIT . list_attribute_ class_signature list_post_item_attribute_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@@ [@@@ [| [> [< [%% < <- let* ( match >. .< .~ method - -. -> module mutable new nonrec of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 743

```
class_sig_field: INHERIT list_attribute_ . class_signature list_post_item_attribute_
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [%% < <- let* ( match >. .< .~ method - -. -> module mutable new nonrec of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 744

```
class_signature: LET . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 745

```
class_signature: LET OPEN . _*
```

### Sample 1

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
&& & and and* as assert ` | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 746

```
class_signature: LET OPEN BANG . list_attribute_ mod_longident IN class_signature
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 747

```
class_signature: LET OPEN BANG list_attribute_ . mod_longident IN class_signature
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 748

```
class_signature: LET OPEN BANG list_attribute_ mod_longident . IN class_signature
mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
mod_longident: mk_longident_mod_longident_UIDENT_ .
mk_longident_mod_longident_UIDENT_: mod_longident DOT UIDENT .
```

## Pattern 749

```
class_signature: LET OPEN BANG list_attribute_ mod_longident IN . class_signature
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [%% < <- let* ( match >. .< .~ method - -. -> module mutable new nonrec of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 750

```
class_signature: LBRACKET . reversed_separated_nonempty_llist_COMMA_core_type_ RBRACKET clty_longident
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 751

```
class_signature: LBRACKET reversed_separated_nonempty_llist_COMMA_core_type_ . RBRACKET clty_longident
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
reversed_separated_nonempty_llist_COMMA_core_type_: core_type .
row_field: core_type .
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 752

```
class_signature: LBRACKET reversed_separated_nonempty_llist_COMMA_core_type_ RBRACKET . clty_longident
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 753

```
reversed_separated_nonempty_llist_COMMA_core_type_: reversed_separated_nonempty_llist_COMMA_core_type_ COMMA . core_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 754

```
class_signature: LET OPEN list_attribute_ . mod_longident IN class_signature
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 755

```
class_signature: LET OPEN list_attribute_ mod_longident . IN class_signature
mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
mod_longident: mk_longident_mod_longident_UIDENT_ .
mk_longident_mod_longident_UIDENT_: mod_longident DOT UIDENT .
```

## Pattern 756

```
class_signature: LET OPEN list_attribute_ mod_longident IN . class_signature
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [%% < <- let* ( match >. .< .~ method - -. -> module mutable new nonrec of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 757

```
class_sig_field: CONSTRAINT . list_attribute_ constrain_field list_post_item_attribute_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 758

```
class_sig_field: CONSTRAINT list_attribute_ . constrain_field list_post_item_attribute_
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 759

```
constrain_field: core_type . EQUAL core_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 760

```
constrain_field: core_type EQUAL . core_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 761

```
class_signature: OBJECT list_attribute_ class_self_type list_text_csig_class_sig_field__ . END
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
list_text_csig_class_sig_field__: class_sig_field . list_text_csig_class_sig_field__
class_sig_field: INHERIT list_attribute_ class_signature . list_post_item_attribute_
class_signature: OBJECT list_attribute_ class_self_type list_text_csig_class_sig_field__ END .
```

## Pattern 762

```
list_and_class_type_declaration_: AND . list_attribute_ virtual_flag formal_class_parameters LIDENT EQUAL class_signature list_post_item_attribute_ list_and_class_type_declaration_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val when while with
```


## Pattern 763

```
list_and_class_type_declaration_: AND list_attribute_ virtual_flag formal_class_parameters . LIDENT EQUAL class_signature list_post_item_attribute_ list_and_class_type_declaration_
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
formal_class_parameters: LBRACKET reversed_separated_nonempty_llist_COMMA_type_parameter_ RBRACKET .
```

## Pattern 764

```
list_and_class_type_declaration_: AND list_attribute_ virtual_flag formal_class_parameters LIDENT . EQUAL class_signature list_post_item_attribute_ list_and_class_type_declaration_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 765

```
list_and_class_type_declaration_: AND list_attribute_ virtual_flag formal_class_parameters LIDENT EQUAL . class_signature list_post_item_attribute_ list_and_class_type_declaration_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [%% < <- let* ( match >. .< .~ method - -. -> module mutable new nonrec of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 766

```
signature_item: CLASS ext list_attribute_ virtual_flag formal_class_parameters . LIDENT COLON class_type list_post_item_attribute_ list_and_class_description_
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
formal_class_parameters: LBRACKET reversed_separated_nonempty_llist_COMMA_type_parameter_ RBRACKET .
```

## Pattern 767

```
signature_item: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT . COLON class_type list_post_item_attribute_ list_and_class_description_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 768

```
signature_item: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON . class_type list_post_item_attribute_ list_and_class_description_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let* match >. .< .~ method - -. -> module mutable new nonrec of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 769

```
class_type: LIDENT COLON . tuple_type MINUSGREATER class_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 770

```
class_type: LIDENT COLON tuple_type . MINUSGREATER class_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
tuple_type: atomic_type . _*
atomic_type: UNDERSCORE .
```

## Pattern 771

```
class_type: LIDENT COLON tuple_type MINUSGREATER . class_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let* match >. .< .~ method - -. -> module mutable new nonrec of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 772

```
class_signature: LBRACKET . reversed_separated_nonempty_llist_COMMA_core_type_ RBRACKET clty_longident
delimited_type_supporting_local_open: LBRACKET . _*
```

### Sample 1

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
&& & and and* as assert ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 773

```
class_type: tuple_type . MINUSGREATER class_type
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
tuple_type: atomic_type . _*
atomic_type: UNDERSCORE .
```

## Pattern 774

```
class_type: tuple_type MINUSGREATER . class_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let* match >. .< .~ method - -. -> module mutable new nonrec of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 775

```
class_type: optlabel . tuple_type MINUSGREATER class_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

Also covered by these intermediate patterns:
```
optlabel: OPTLABEL .
```

## Pattern 776

```
class_type: optlabel tuple_type . MINUSGREATER class_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
tuple_type: atomic_type . _*
atomic_type: UNDERSCORE .
```

## Pattern 777

```
class_type: optlabel tuple_type MINUSGREATER . class_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let* match >. .< .~ method - -. -> module mutable new nonrec of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 778

```
list_and_class_description_: AND . list_attribute_ virtual_flag formal_class_parameters LIDENT COLON class_type list_post_item_attribute_ list_and_class_description_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val when while with
```


## Pattern 779

```
list_and_class_description_: AND list_attribute_ virtual_flag formal_class_parameters . LIDENT COLON class_type list_post_item_attribute_ list_and_class_description_
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
formal_class_parameters: LBRACKET reversed_separated_nonempty_llist_COMMA_type_parameter_ RBRACKET .
```

## Pattern 780

```
list_and_class_description_: AND list_attribute_ virtual_flag formal_class_parameters LIDENT . COLON class_type list_post_item_attribute_ list_and_class_description_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 781

```
list_and_class_description_: AND list_attribute_ virtual_flag formal_class_parameters LIDENT COLON . class_type list_post_item_attribute_ list_and_class_description_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let* match >. .< .~ method - -. -> module mutable new nonrec of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 782

```
list_generic_and_type_declaration_type_kind__: AND . list_attribute_ type_parameters LIDENT type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_ list_generic_and_type_declaration_type_kind__
```

### Sample 1

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
&& & and and* as assert ` | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X val virtual when while with
```


## Pattern 783

```
list_generic_and_type_declaration_type_kind__: AND list_attribute_ type_parameters . LIDENT type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_ list_generic_and_type_declaration_type_kind__
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
type_parameters: LPAREN reversed_separated_nonempty_llist_COMMA_type_parameter_ RPAREN .
```

## Pattern 784

```
list_generic_and_type_declaration_type_subst_kind__: AND . list_attribute_ type_parameters LIDENT COLONEQUAL nonempty_type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_ list_generic_and_type_declaration_type_subst_kind__
```

### Sample 1

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
&& & and and* as assert ` | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X val virtual when while with
```


## Pattern 785

```
list_generic_and_type_declaration_type_subst_kind__: AND list_attribute_ type_parameters . LIDENT COLONEQUAL nonempty_type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_ list_generic_and_type_declaration_type_subst_kind__
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
type_parameters: LPAREN reversed_separated_nonempty_llist_COMMA_type_parameter_ RPAREN .
```

## Pattern 786

```
list_generic_and_type_declaration_type_subst_kind__: AND list_attribute_ type_parameters LIDENT . COLONEQUAL nonempty_type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_ list_generic_and_type_declaration_type_subst_kind__
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 787

```
list_generic_and_type_declaration_type_subst_kind__: AND list_attribute_ type_parameters LIDENT COLONEQUAL . nonempty_type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_ list_generic_and_type_declaration_type_subst_kind__
```

### Sample 1

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
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .+ downto effect else end  
 = exception 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type val virtual when while with
```


## Pattern 788

```
constr_extra_nonprefix_ident: LBRACKET . RBRACKET
delimited_type_supporting_local_open: LBRACKET . _*
```

### Sample 1

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
&& & and and* as assert ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 789

```
nonempty_type_kind: LBRACE label_declarations . RBRACE
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
label_declarations: label_declaration_semi . _*
label_declaration_semi: mutable_flag LIDENT COLON possibly_poly_core_type_no_attr_ list_attribute_ SEMI . list_attribute_
```

## Pattern 790

```
nonempty_type_kind: EXTERNAL . STRING
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to true try type X _ val virtual when while with
```


## Pattern 791

```
generic_constructor_declaration_BAR_: BAR . constr_ident generalized_constructor_arguments list_attribute_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```


## Pattern 792

```
nonempty_type_kind: core_type EQUAL . _*
```

### Sample 1

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
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .+ downto effect else end  
 = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```


## Pattern 793

```
nonempty_type_kind: core_type EQUAL PRIVATE . _*
```

### Sample 1

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
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .+ downto effect else end  
 = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```


## Pattern 794

```
nonempty_type_kind: core_type EQUAL PRIVATE LBRACE label_declarations . RBRACE
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
label_declarations: label_declaration_semi . _*
label_declaration_semi: mutable_flag LIDENT COLON possibly_poly_core_type_no_attr_ list_attribute_ SEMI . list_attribute_
```

## Pattern 795

```
nonempty_type_kind: core_type EQUAL LBRACE label_declarations . RBRACE
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
label_declarations: label_declaration_semi . _*
label_declaration_semi: mutable_flag LIDENT COLON possibly_poly_core_type_no_attr_ list_attribute_ SEMI . list_attribute_
```

## Pattern 796

```
class_type_declarations: CLASS . TYPE ext list_attribute_ virtual_flag formal_class_parameters LIDENT EQUAL class_signature list_post_item_attribute_ list_and_class_type_declaration_
local_structure_item: CLASS . ext list_attribute_ virtual_flag formal_class_parameters LIDENT class_fun_binding list_post_item_attribute_ list_and_class_declaration_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try X _ val when while with
```


## Pattern 797

```
local_structure_item: CLASS ext list_attribute_ virtual_flag formal_class_parameters . LIDENT class_fun_binding list_post_item_attribute_ list_and_class_declaration_
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
formal_class_parameters: LBRACKET reversed_separated_nonempty_llist_COMMA_type_parameter_ RBRACKET .
```

## Pattern 798

```
local_structure_item: CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT . class_fun_binding list_post_item_attribute_ list_and_class_declaration_
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open or % +. += !+ private ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 799

```
constr_extra_nonprefix_ident: LPAREN . RPAREN
constr_longident: LPAREN . COLONCOLON RPAREN
simple_param_pattern: LPAREN . pattern COLON reversed_nonempty_llist_typevar_ DOT core_type RPAREN
simple_pattern_not_ident: LPAREN . _*
val_extra_ident: LPAREN . operator RPAREN
```

### Sample 1

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
and as assert | |] begin class : :> , (*comment*) constraint do (**documentation *) done . .. downto else end  
 external for fun function functor >} >] if in include inherit initializer {< [@ [@@ [@@@ [> [< [%% <- let match >. .< .~ method -> mutable new nonrec object of open ?label: private ? ' {%%%%ext|s|} } ] rec ; ;; sig struct then to try type val virtual when while with
```


## Pattern 800

```
class_fun_binding: EQUAL . class_expr
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [%% < <- let* match >. .< .~ method - -. -> module mutable new nonrec of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 801

```
class_simple_expr: OBJECT . list_attribute_ class_self_pattern list_text_cstr_class_field__ END
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) do (**documentation *) done . .. .+ downto effect else  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** 1 ~label: lazy { {< [ [@@ [| [> [< [% < <- let let* x match >. .< .~ - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ virtual when while with
```


## Pattern 802

```
class_simple_expr: OBJECT list_attribute_ class_self_pattern . list_text_cstr_class_field__ END
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) do (**documentation *) done . .. .+ downto effect else  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** 1 ~label: lazy { {< [ [@ [@@ [| [> [< [% < <- let let* x ( match >. .< .~ - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ virtual when while with
```

Also covered by these intermediate patterns:
```
class_self_pattern: LPAREN pattern COLON core_type RPAREN .
```

## Pattern 803

```
class_field: VAL . value list_post_item_attribute_
```

### Sample 1

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
&& & and and* as assert ` | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val when while with
```


## Pattern 804

```
value: BANG . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 805

```
value: BANG list_attribute_ mutable_flag . _*
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
mutable_flag: MUTABLE .
```

## Pattern 806

```
value: BANG list_attribute_ mutable_flag LIDENT . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 807

```
value: BANG list_attribute_ mutable_flag LIDENT EQUAL . seq_expr
```

### Sample 1

Sentence:
```
object val ! x =
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern VAL BANG list_attribute_ mutable_flag LIDENT EQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 808

```
value: BANG list_attribute_ mutable_flag LIDENT type_constraint . EQUAL seq_expr
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
type_constraint: COLON core_type COLONGREATER core_type .
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 809

```
value: BANG list_attribute_ mutable_flag LIDENT type_constraint EQUAL . seq_expr
```

### Sample 1

Sentence:
```
object val ! x : {%%ext|s|} =
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern VAL BANG list_attribute_ mutable_flag LIDENT type_constraint EQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 810

```
value: list_attribute_ virtual_with_mutable_flag . LIDENT COLON core_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
virtual_with_mutable_flag: MUTABLE VIRTUAL .
```

## Pattern 811

```
value: list_attribute_ virtual_with_mutable_flag LIDENT . COLON core_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 812

```
value: list_attribute_ virtual_with_mutable_flag LIDENT COLON . core_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 813

```
value: list_attribute_ mutable_flag . _*
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
mutable_flag: MUTABLE .
virtual_with_mutable_flag: MUTABLE . VIRTUAL
```

## Pattern 814

```
value: list_attribute_ mutable_flag LIDENT . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 815

```
value: list_attribute_ mutable_flag LIDENT EQUAL . seq_expr
```

### Sample 1

Sentence:
```
object val x =
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern VAL list_attribute_ mutable_flag LIDENT EQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 816

```
value: list_attribute_ mutable_flag LIDENT type_constraint . EQUAL seq_expr
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
type_constraint: COLON core_type COLONGREATER core_type .
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 817

```
value: list_attribute_ mutable_flag LIDENT type_constraint EQUAL . seq_expr
```

### Sample 1

Sentence:
```
object val x : {%%ext|s|} =
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern VAL list_attribute_ mutable_flag LIDENT type_constraint EQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 818

```
class_field: METHOD . method_ list_post_item_attribute_
```

### Sample 1

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
&& & and and* as assert ` | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val when while with
```


## Pattern 819

```
method_: BANG . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 820

```
method_: BANG list_attribute_ private_flag . _*
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
private_flag: PRIVATE .
```

## Pattern 821

```
method_: BANG list_attribute_ private_flag LIDENT . _*
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open or % +. += !+ private ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 822

```
method_: BANG list_attribute_ private_flag LIDENT COLON . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try val virtual when while with
```


## Pattern 823

```
method_: BANG list_attribute_ private_flag LIDENT COLON TYPE . nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL seq_expr
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 824

```
method_: BANG list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ . DOT core_type EQUAL seq_expr
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
nonempty_list_mkrhs_LIDENT__: LIDENT . _*
```

## Pattern 825

```
method_: BANG list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT . core_type EQUAL seq_expr
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 826

```
method_: BANG list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type . EQUAL seq_expr
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 827

```
method_: BANG list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL . seq_expr
```

### Sample 1

Sentence:
```
object method ! x : type x . {%%ext|s|} =
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern METHOD BANG list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 828

```
method_: BANG list_attribute_ private_flag LIDENT COLON possibly_poly_core_type_ . EQUAL seq_expr
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
possibly_poly_core_type_: core_type .
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 829

```
method_: BANG list_attribute_ private_flag LIDENT COLON possibly_poly_core_type_ EQUAL . seq_expr
```

### Sample 1

Sentence:
```
object method ! x : {%%ext|s|} =
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern METHOD BANG list_attribute_ private_flag LIDENT COLON possibly_poly_core_type_ EQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 830

```
method_: list_attribute_ virtual_with_private_flag . LIDENT COLON possibly_poly_core_type_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
virtual_with_private_flag: PRIVATE VIRTUAL .
```

## Pattern 831

```
method_: list_attribute_ virtual_with_private_flag LIDENT . COLON possibly_poly_core_type_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 832

```
method_: list_attribute_ virtual_with_private_flag LIDENT COLON . possibly_poly_core_type_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 833

```
method_: list_attribute_ private_flag . _*
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
private_flag: PRIVATE .
virtual_with_private_flag: PRIVATE . VIRTUAL
```

## Pattern 834

```
method_: list_attribute_ private_flag LIDENT . _*
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open or % +. += !+ private ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 835

```
method_: list_attribute_ private_flag LIDENT COLON . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try val virtual when while with
```


## Pattern 836

```
method_: list_attribute_ private_flag LIDENT COLON TYPE . nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL seq_expr
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 837

```
method_: list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ . DOT core_type EQUAL seq_expr
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
nonempty_list_mkrhs_LIDENT__: LIDENT . _*
```

## Pattern 838

```
method_: list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT . core_type EQUAL seq_expr
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 839

```
method_: list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type . EQUAL seq_expr
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 840

```
method_: list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL . seq_expr
```

### Sample 1

Sentence:
```
object method x : type x . {%%ext|s|} =
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern METHOD list_attribute_ private_flag LIDENT COLON TYPE nonempty_list_mkrhs_LIDENT__ DOT core_type EQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 841

```
method_: list_attribute_ private_flag LIDENT COLON possibly_poly_core_type_ . EQUAL seq_expr
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
possibly_poly_core_type_: core_type .
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 842

```
method_: list_attribute_ private_flag LIDENT COLON possibly_poly_core_type_ EQUAL . seq_expr
```

### Sample 1

Sentence:
```
object method x : {%%ext|s|} =
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern METHOD list_attribute_ private_flag LIDENT COLON possibly_poly_core_type_ EQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 843

```
class_field: INITIALIZER . list_attribute_ seq_expr list_post_item_attribute_
```

### Sample 1

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
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 844

```
class_field: INITIALIZER list_attribute_ . seq_expr list_post_item_attribute_
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
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 845

```
class_field: INHERIT . _*
```

### Sample 1

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
&& & and and* as assert ` | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@@ [@@@ [| [> [< [%% < <- let* match >. .< .~ method - -. -> module mutable new nonrec of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 846

```
class_field: INHERIT BANG . list_attribute_ class_expr option_preceded_AS_mkrhs_LIDENT___ list_post_item_attribute_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@@ [@@@ [| [> [< [%% < <- let* match >. .< .~ method - -. -> module mutable new nonrec of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 847

```
class_field: INHERIT BANG list_attribute_ . class_expr option_preceded_AS_mkrhs_LIDENT___ list_post_item_attribute_
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [%% < <- let* match >. .< .~ method - -. -> module mutable new nonrec of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 848

```
class_simple_expr: LPAREN . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [%% < <- let* match >. .< .~ method - -. -> module mutable new nonrec of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 849

```
class_expr: LET . _*
let_bindings_no_ext_: LET . _*
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 850

```
let_bindings_no_ext_: LET PERCENT . attr_id list_attribute_ rec_flag let_binding_body list_post_item_attribute_
```

### Sample 1

Sentence:
```
object inherit let %
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ LET PERCENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and* ` ! | || |] 'a' : :: := :> , (*comment*) (**documentation *) . .. .+  
 = 1.0 > >} >] # ## != ^ +! land ** 1 ~label: { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let* ( >. .< .~ - -. -> ?label: % + +. += !+ ? ' {%%ext|s|} {%%%%ext|s|} } ] ) ; ;; * "s" ~ _
```


## Pattern 851

```
let_bindings_no_ext_: LET PERCENT attr_id list_attribute_ rec_flag . let_binding_body list_post_item_attribute_
```

### Sample 1

Sentence:
```
object inherit let % and rec
```
Stack:
```
use_file: OBJECT ext list_attribute_ class_self_pattern INHERIT list_attribute_ LET PERCENT attr_id list_attribute_ REC
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
rec_flag: REC .
```

## Pattern 852

```
class_expr: LET OPEN . _*
```

### Sample 1

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
&& & and and* as assert ` | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 853

```
class_expr: LET OPEN BANG . list_attribute_ mod_longident IN class_expr
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 854

```
class_expr: LET OPEN BANG list_attribute_ . mod_longident IN class_expr
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 855

```
class_expr: LET OPEN BANG list_attribute_ mod_longident . IN class_expr
mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
mod_longident: mk_longident_mod_longident_UIDENT_ .
mk_longident_mod_longident_UIDENT_: mod_longident DOT UIDENT .
```

## Pattern 856

```
class_expr: LET OPEN BANG list_attribute_ mod_longident IN . class_expr
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [%% < <- let* match >. .< .~ method - -. -> module mutable new nonrec of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 857

```
class_simple_expr: LBRACKET . reversed_separated_nonempty_llist_COMMA_core_type_ RBRACKET class_longident
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 858

```
class_simple_expr: LBRACKET reversed_separated_nonempty_llist_COMMA_core_type_ . RBRACKET class_longident
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
reversed_separated_nonempty_llist_COMMA_core_type_: core_type .
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 859

```
class_simple_expr: LBRACKET reversed_separated_nonempty_llist_COMMA_core_type_ RBRACKET . class_longident
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 860

```
class_expr: FUN . list_attribute_ class_fun_def
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer lazy {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open or % +. += !+ private ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 861

```
class_expr: FUN list_attribute_ . class_fun_def
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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open or % +. += !+ private ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 862

```
class_fun_def: simple_param_pattern . _*
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. module mutable new nonrec object of open or % +. += !+ private ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
simple_param_pattern: QUESTION LIDENT .
```

## Pattern 863

```
class_fun_def: simple_param_pattern MINUSGREATER . class_expr
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [%% < <- let* match >. .< .~ method - -. -> module mutable new nonrec of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 864

```
class_expr: let_bindings_no_ext_ . IN class_expr
```

### Sample 1

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
&& & and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
let_bindings_no_ext_: LET list_attribute_ rec_flag let_binding_body list_post_item_attribute_ .
list_post_item_attribute_: post_item_attribute . list_post_item_attribute_
post_item_attribute: LBRACKETATAT attr_id attr_payload RBRACKET .
```

## Pattern 865

```
class_expr: let_bindings_no_ext_ IN . class_expr
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [%% < <- let* match >. .< .~ method - -. -> module mutable new nonrec of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 866

```
class_expr: LET OPEN list_attribute_ . mod_longident IN class_expr
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 867

```
class_expr: LET OPEN list_attribute_ mod_longident . IN class_expr
mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
mod_longident: mk_longident_mod_longident_UIDENT_ .
mk_longident_mod_longident_UIDENT_: mod_longident DOT UIDENT .
```

## Pattern 868

```
class_expr: LET OPEN list_attribute_ mod_longident IN . class_expr
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [%% < <- let* match >. .< .~ method - -. -> module mutable new nonrec of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 869

```
let_bindings_no_ext_: LET list_attribute_ rec_flag . let_binding_body list_post_item_attribute_
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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
rec_flag: REC .
```

## Pattern 870

```
class_simple_expr: LPAREN class_expr . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
class_expr: class_simple_expr . _*
class_simple_expr: OBJECT list_attribute_ class_self_pattern list_text_cstr_class_field__ END .
```

## Pattern 871

```
class_simple_expr: LPAREN class_expr COLON . class_type RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let* match >. .< .~ method - -. -> module mutable new nonrec of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 872

```
class_simple_expr: LPAREN class_expr COLON class_type . RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
class_type: class_signature .
class_signature: OBJECT list_attribute_ class_self_type list_text_csig_class_sig_field__ END .
```

## Pattern 873

```
option_preceded_AS_mkrhs_LIDENT___: AS . LIDENT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 874

```
class_field: INHERIT list_attribute_ . class_expr option_preceded_AS_mkrhs_LIDENT___ list_post_item_attribute_
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [%% < <- let* match >. .< .~ method - -. -> module mutable new nonrec of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 875

```
class_field: CONSTRAINT . list_attribute_ constrain_field list_post_item_attribute_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 876

```
class_field: CONSTRAINT list_attribute_ . constrain_field list_post_item_attribute_
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 877

```
class_simple_expr: OBJECT list_attribute_ class_self_pattern list_text_cstr_class_field__ . END
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
list_text_cstr_class_field__: class_field . list_text_cstr_class_field__
class_field: INHERIT BANG list_attribute_ class_expr option_preceded_AS_mkrhs_LIDENT___ . list_post_item_attribute_
option_preceded_AS_mkrhs_LIDENT___: AS LIDENT .
```

## Pattern 878

```
class_fun_binding: COLON . class_type EQUAL class_expr
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let* match >. .< .~ method - -. -> module mutable new nonrec of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 879

```
class_fun_binding: COLON class_type . EQUAL class_expr
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
class_type: class_signature .
class_signature: OBJECT list_attribute_ class_self_type list_text_csig_class_sig_field__ END .
```

## Pattern 880

```
class_fun_binding: COLON class_type EQUAL . class_expr
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [%% < <- let* match >. .< .~ method - -. -> module mutable new nonrec of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 881

```
class_fun_binding: simple_param_pattern . class_fun_binding
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open or % +. += !+ private ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
simple_param_pattern: QUESTION LIDENT .
```

## Pattern 882

```
list_and_class_declaration_: AND . list_attribute_ virtual_flag formal_class_parameters LIDENT class_fun_binding list_post_item_attribute_ list_and_class_declaration_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val when while with
```


## Pattern 883

```
list_and_class_declaration_: AND list_attribute_ virtual_flag formal_class_parameters . LIDENT class_fun_binding list_post_item_attribute_ list_and_class_declaration_
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
formal_class_parameters: LBRACKET reversed_separated_nonempty_llist_COMMA_type_parameter_ RBRACKET .
```

## Pattern 884

```
list_and_class_declaration_: AND list_attribute_ virtual_flag formal_class_parameters LIDENT . class_fun_binding list_post_item_attribute_ list_and_class_declaration_
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open or % +. += !+ private ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 885

```
let_bindings_ext_: LET . ext list_attribute_ rec_flag let_binding_body list_post_item_attribute_
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 886

```
let_bindings_ext_: LET ext list_attribute_ rec_flag . let_binding_body list_post_item_attribute_
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
rec_flag: REC .
```

## Pattern 887

```
floating_attribute: LBRACKETATATAT attr_id attr_payload . RBRACKET
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
attr_payload: payload .
payload: COLON . _*
```

## Pattern 888

```
item_extension: LBRACKETPERCENTPERCENT attr_id payload . RBRACKET
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
payload: COLON . _*
```

## Pattern 889

```
fun_expr: LET ext list_attribute_ local_structure_item . IN seq_expr
```

### Sample 1

Sentence:
```
let type x and x
```
Stack:
```
use_file: LET ext list_attribute_ generic_type_declaration_nonrec_flag_type_kind_ AND list_attribute_ type_parameters LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
local_structure_item: generic_type_declaration_nonrec_flag_type_kind_ list_generic_and_type_declaration_type_kind__ .
list_generic_and_type_declaration_type_kind__: AND list_attribute_ type_parameters LIDENT . type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_ list_generic_and_type_declaration_type_kind__
```

## Pattern 890

```
fun_expr: LET ext list_attribute_ local_structure_item IN . seq_expr
```

### Sample 1

Sentence:
```
let {%%%%ext|s|} in
```
Stack:
```
use_file: LET ext list_attribute_ local_structure_item IN
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 891

```
simple_param_pattern: QUESTION LPAREN label_let_pattern option_preceded_EQUAL_seq_expr__ . RPAREN
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
option_preceded_EQUAL_seq_expr__: EQUAL seq_expr .
seq_expr: fun_seq_expr .
fun_seq_expr: fun_expr SEMI . _*
```

## Pattern 892

```
fun_expr: LIDENT LESSMINUS FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION . _*
```

### Sample 1

Sentence:
```
x <- function
```
Stack:
```
use_file: LIDENT LESSMINUS FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 893

```
fun_expr: LIDENT LESSMINUS FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```

### Sample 1

Sentence:
```
x <- function [@ and ]
```
Stack:
```
use_file: LIDENT LESSMINUS FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 894

```
simple_expr: LBRACKETBAR separated_or_terminated_nonempty_list_SEMI_expr_ . BARRBRACKET
```

### Sample 1

Sentence:
```
[| X ;
```
Stack:
```
use_file: LBRACKETBAR fun_expr SEMI
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
separated_or_terminated_nonempty_list_SEMI_expr_: fun_expr SEMI . _*
```

## Pattern 895

```
simple_expr: LPAREN MODULE . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ val virtual when while with
```


## Pattern 896

```
simple_expr: LPAREN MODULE ext list_attribute_ . _*
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 897

```
simple_expr: LPAREN MODULE ext list_attribute_ module_expr . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
module_expr: STRUCT list_attribute_ structure END .
```

## Pattern 898

```
simple_expr: LPAREN MODULE ext list_attribute_ module_expr COLON . module_type RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 899

```
simple_expr: LPAREN MODULE ext list_attribute_ module_expr COLON module_type . RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

Also covered by these intermediate patterns:
```
module_type: SIG list_attribute_ signature END .
```

## Pattern 900

```
simple_expr: LPAREN seq_expr . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class :: := , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
seq_expr: fun_seq_expr .
fun_seq_expr: fun_expr SEMI . _*
```

## Pattern 901

```
simple_expr: LPAREN seq_expr type_constraint . RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
type_constraint: COLON core_type COLONGREATER core_type .
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 902

```
fun_expr: MATCH ext list_attribute_ seq_expr . WITH reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

Also covered by these intermediate patterns:
```
seq_expr: fun_seq_expr .
fun_seq_expr: fun_expr SEMI . _*
```

## Pattern 903

```
fun_expr: MATCH ext list_attribute_ seq_expr WITH . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
```

### Sample 1

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
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 904

```
simple_expr: METAOCAML_BRACKET_OPEN seq_expr . METAOCAML_BRACKET_CLOSE
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
seq_expr: fun_seq_expr .
fun_seq_expr: fun_expr SEMI . _*
```

## Pattern 905

```
expr_colon_package_type: FUNCTION . _*
reversed_labeled_tuple_body: FUNCTION . _*
```

### Sample 1

Sentence:
```
( val function
```
Stack:
```
parse_module_expr: LPAREN VAL list_attribute_ FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 906

```
expr_colon_package_type: FUNCTION ext list_attribute_ . _*
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
```

### Sample 1

Sentence:
```
( val function [@ and ]
```
Stack:
```
parse_module_expr: LPAREN VAL list_attribute_ FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 907

```
expr_colon_package_type: FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COLONGREATER . module_type
```

### Sample 1

Sentence:
```
( val function false -> X :>
```
Stack:
```
parse_module_expr: LPAREN VAL list_attribute_ FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COLONGREATER
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 908

```
expr_colon_package_type: FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COLON . _*
```

### Sample 1

Sentence:
```
( val function false -> X :
```
Stack:
```
parse_module_expr: LPAREN VAL list_attribute_ FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 909

```
expr_colon_package_type: FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COLON module_type COLONGREATER . module_type
```

### Sample 1

Sentence:
```
( val function false -> X : {%%ext|s|} :>
```
Stack:
```
parse_module_expr: LPAREN VAL list_attribute_ FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COLON module_type COLONGREATER
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 910

```
expr_colon_package_type: fun_expr COLONGREATER . module_type
```

### Sample 1

Sentence:
```
( val X :>
```
Stack:
```
parse_module_expr: LPAREN VAL list_attribute_ fun_expr COLONGREATER
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 911

```
expr_colon_package_type: fun_expr COLON . _*
```

### Sample 1

Sentence:
```
( val X :
```
Stack:
```
parse_module_expr: LPAREN VAL list_attribute_ fun_expr COLON
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 912

```
expr_colon_package_type: fun_expr COLON module_type COLONGREATER . module_type
```

### Sample 1

Sentence:
```
( val X : {%%ext|s|} :>
```
Stack:
```
parse_module_expr: LPAREN VAL list_attribute_ fun_expr COLON module_type COLONGREATER
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 913

```
paren_module_expr: LPAREN VAL list_attribute_ expr_colon_package_type . RPAREN
```

### Sample 1

Sentence:
```
( val function false -> X :> sig end
```
Stack:
```
parse_module_expr: LPAREN VAL list_attribute_ FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COLONGREATER SIG list_attribute_ signature END
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
expr_colon_package_type: FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COLONGREATER module_type .
module_type: SIG list_attribute_ signature END .
```

## Pattern 914

```
open_declaration: OPEN ext list_attribute_ . module_expr list_post_item_attribute_
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 915

```
module_expr: STRUCT list_attribute_ structure . END
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
structure: list_structure_element_ .
list_structure_element_: SEMISEMI . _*
```

## Pattern 916

```
simple_pattern_not_ident: LPAREN MODULE ext list_attribute_ module_name COLON module_type . RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

Also covered by these intermediate patterns:
```
module_type: SIG list_attribute_ signature END .
```

## Pattern 917

```
labeled_tuple_pat_element_list_pattern_: LABEL . _*
labeled_tuple_pat_element_list_pattern_: TILDE LIDENT COMMA LABEL . simple_pattern
reversed_labeled_tuple_pattern_pattern_: LABEL . simple_pattern COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer ~label: lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type val virtual when while with
```


## Pattern 918

```
labeled_tuple_pat_element_list_pattern_: LABEL . _*
labeled_tuple_pat_element_list_pattern_: TILDE LPAREN LIDENT COLON core_type RPAREN COMMA LABEL . simple_pattern
reversed_labeled_tuple_pattern_pattern_: LABEL . simple_pattern COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer ~label: lazy {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type val virtual when while with
```


## Pattern 919

```
labeled_tuple_pat_element_list_pattern_: TILDE LIDENT . _*
reversed_labeled_tuple_pattern_pattern_: TILDE LIDENT . COMMA DOTDOT
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 920

```
class_self_pattern: LPAREN pattern . _*
labeled_tuple_pat_element_list_pattern_: pattern . _*
reversed_labeled_tuple_pattern_pattern_: pattern . COMMA DOTDOT
```

### Sample 1

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
&& & and and* assert ` ! || |] begin 'a' class := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
pattern: pattern AS val_ident .
val_ident: LIDENT .
```

## Pattern 921

```
class_self_pattern: LPAREN pattern COLON . core_type RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 922

```
class_self_pattern: LPAREN pattern COLON core_type . RPAREN
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 923

```
simple_expr: OBJECT ext list_attribute_ class_self_pattern . list_text_cstr_class_field__ END
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) do (**documentation *) done . .. .+ downto effect else  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** 1 ~label: lazy { {< [ [@ [@@ [| [> [< [% < <- let let* x ( match >. .< .~ - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ virtual when while with
```

Also covered by these intermediate patterns:
```
class_self_pattern: LPAREN pattern COLON core_type RPAREN .
```

## Pattern 924

```
simple_expr: OBJECT ext list_attribute_ class_self_pattern list_text_cstr_class_field__ . END
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
list_text_cstr_class_field__: class_field . list_text_cstr_class_field__
class_field: INHERIT BANG list_attribute_ class_expr option_preceded_AS_mkrhs_LIDENT___ . list_post_item_attribute_
option_preceded_AS_mkrhs_LIDENT___: AS LIDENT .
```

## Pattern 925

```
reversed_labeled_tuple_body: LABEL . _*
reversed_labeled_tuple_body: TILDE LIDENT COMMA LABEL . simple_expr
```

### Sample 1

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
&& & and and* as assert | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy [@ [@@ [@@@ [> [< [%% < <- let let* match >. method - -. -> module mutable nonrec of open ?label: or % + +. += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type _ val virtual when while with
```


## Pattern 926

```
reversed_labeled_tuple_body: FUNCTION . _*
reversed_labeled_tuple_body: TILDE LIDENT COMMA FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
```

### Sample 1

Sentence:
```
~ x , function
```
Stack:
```
use_file: TILDE LIDENT COMMA FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 927

```
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
reversed_labeled_tuple_body: TILDE LIDENT COMMA FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
```

### Sample 1

Sentence:
```
~ x , function [@ and ]
```
Stack:
```
use_file: TILDE LIDENT COMMA FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 928

```
reversed_labeled_tuple_body: LABEL . _*
reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint RPAREN COMMA LABEL . simple_expr
```

### Sample 1

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
&& & and and* as assert | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer ~label: lazy [@ [@@ [@@@ [> [< [%% < <- let let* match >. method - -. -> module mutable nonrec of open ?label: or % + +. += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then ~ to try type _ val virtual when while with
```


## Pattern 929

```
reversed_labeled_tuple_body: FUNCTION . _*
reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint RPAREN COMMA FUNCTION . ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
```

### Sample 1

Sentence:
```
~ ( x : {%%ext|s|} ) , function
```
Stack:
```
use_file: TILDE LPAREN LIDENT type_constraint RPAREN COMMA FUNCTION
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 930

```
reversed_labeled_tuple_body: FUNCTION ext list_attribute_ . _*
reversed_labeled_tuple_body: TILDE LPAREN LIDENT type_constraint RPAREN COMMA FUNCTION ext list_attribute_ . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
```

### Sample 1

Sentence:
```
~ ( x : {%%ext|s|} ) , function [@ and ]
```
Stack:
```
use_file: TILDE LPAREN LIDENT type_constraint RPAREN COMMA FUNCTION ext LBRACKETAT attr_id attr_payload RBRACKET
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 931

```
reversed_labeled_tuple_body: TILDE LIDENT . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 932

```
fun_expr: TRY ext list_attribute_ seq_expr . WITH reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

Also covered by these intermediate patterns:
```
seq_expr: fun_seq_expr .
fun_seq_expr: fun_expr SEMI . _*
```

## Pattern 933

```
fun_expr: TRY ext list_attribute_ seq_expr WITH . reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
```

### Sample 1

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
&& & and and* as assert ! || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 934

```
post_item_attribute: LBRACKETATAT attr_id attr_payload . RBRACKET
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
attr_payload: payload .
payload: COLON . _*
```

## Pattern 935

```
local_structure_item: TYPE ext list_attribute_ NONREC type_parameters type_longident . PLUSEQ private_flag reversed_bar_llist_extension_constructor_ list_post_item_attribute_
```

### Sample 1

Sentence:
```
type nonrec x
```
Stack:
```
use_file: TYPE ext list_attribute_ NONREC type_parameters LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
type_longident: mk_longident_mod_ext_longident_LIDENT_ .
generic_type_declaration_nonrec_flag_type_kind_: TYPE ext list_attribute_ NONREC type_parameters LIDENT . type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_
mk_longident_mod_ext_longident_LIDENT_: LIDENT .
```

## Pattern 936

```
local_structure_item: TYPE ext list_attribute_ NONREC type_parameters type_longident PLUSEQ . private_flag reversed_bar_llist_extension_constructor_ list_post_item_attribute_
```

### Sample 1

Sentence:
```
type nonrec x +=
```
Stack:
```
use_file: TYPE ext list_attribute_ NONREC type_parameters type_longident PLUSEQ
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```


## Pattern 937

```
local_structure_item: TYPE ext list_attribute_ NONREC type_parameters type_longident PLUSEQ private_flag . reversed_bar_llist_extension_constructor_ list_post_item_attribute_
```

### Sample 1

Sentence:
```
type nonrec x += private
```
Stack:
```
use_file: TYPE ext list_attribute_ NONREC type_parameters type_longident PLUSEQ PRIVATE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```

Also covered by these intermediate patterns:
```
private_flag: PRIVATE .
```

## Pattern 938

```
extension_constructor_rebind_BAR_: BAR . constr_ident EQUAL constr_longident list_attribute_
generic_constructor_declaration_BAR_: BAR . constr_ident generalized_constructor_arguments list_attribute_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```


## Pattern 939

```
extension_constructor_rebind_BAR_: BAR constr_ident EQUAL . constr_longident list_attribute_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```


## Pattern 940

```
extension_constructor_rebind_epsilon_: constr_ident EQUAL . constr_longident list_attribute_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```


## Pattern 941

```
generic_type_declaration_nonrec_flag_type_kind_: TYPE ext list_attribute_ type_parameters . LIDENT type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_
local_structure_item: TYPE ext list_attribute_ type_parameters . type_longident PLUSEQ private_flag reversed_bar_llist_extension_constructor_ list_post_item_attribute_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

Also covered by these intermediate patterns:
```
type_parameters: LPAREN reversed_separated_nonempty_llist_COMMA_type_parameter_ RPAREN .
```

## Pattern 942

```
local_structure_item: TYPE ext list_attribute_ type_parameters type_longident . PLUSEQ private_flag reversed_bar_llist_extension_constructor_ list_post_item_attribute_
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
type_longident: mk_longident_mod_ext_longident_LIDENT_ .
mk_longident_mod_ext_longident_LIDENT_: mod_ext_longident DOT LIDENT .
```

## Pattern 943

```
local_structure_item: TYPE ext list_attribute_ type_parameters type_longident PLUSEQ . private_flag reversed_bar_llist_extension_constructor_ list_post_item_attribute_
```

### Sample 1

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
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```


## Pattern 944

```
local_structure_item: TYPE ext list_attribute_ type_parameters type_longident PLUSEQ private_flag . reversed_bar_llist_extension_constructor_ list_post_item_attribute_
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
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```

Also covered by these intermediate patterns:
```
private_flag: PRIVATE .
```

## Pattern 945

```
extension: LBRACKETPERCENT attr_id payload . RBRACKET
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
payload: COLON . _*
```

## Pattern 946

```
meth_list: LIDENT COLON . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 947

```
object_type: LESS meth_list . GREATER
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
meth_list: atomic_type SEMI . _*
```

## Pattern 948

```
function_type: LIDENT COLON LPAREN reversed_nonempty_llist_typevar_ . DOT core_type RPAREN MINUSGREATER function_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
atomic_type: QUOTE ident .
reversed_nonempty_llist_typevar_: QUOTE ident .
ident: LIDENT .
```

## Pattern 949

```
function_type: LIDENT COLON LPAREN reversed_nonempty_llist_typevar_ DOT . core_type RPAREN MINUSGREATER function_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 950

```
function_type: LIDENT COLON LPAREN reversed_nonempty_llist_typevar_ DOT core_type . RPAREN MINUSGREATER function_type
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 951

```
function_type: LIDENT COLON LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN . MINUSGREATER function_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 952

```
function_type: LIDENT COLON LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER . function_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 953

```
function_type: LIDENT COLON tuple_type . MINUSGREATER function_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
function_type: LIDENT COLON atomic_type . _*
tuple_type: atomic_type . _*
atomic_type: UNDERSCORE .
```

## Pattern 954

```
function_type: LIDENT COLON tuple_type MINUSGREATER . function_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 955

```
function_type: LIDENT COLON atomic_type STAR . _*
tuple_type: atomic_type STAR . reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 956

```
function_type: LIDENT COLON atomic_type STAR reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ MINUSGREATER . function_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 957

```
function_type: LPAREN reversed_nonempty_llist_typevar_ . DOT core_type RPAREN MINUSGREATER function_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
atomic_type: QUOTE ident .
reversed_nonempty_llist_typevar_: QUOTE ident .
ident: LIDENT .
```

## Pattern 958

```
function_type: LPAREN reversed_nonempty_llist_typevar_ DOT . core_type RPAREN MINUSGREATER function_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 959

```
function_type: LPAREN reversed_nonempty_llist_typevar_ DOT core_type . RPAREN MINUSGREATER function_type
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 960

```
function_type: LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN . MINUSGREATER function_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 961

```
function_type: LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER . function_type
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 962

```
nonempty_type_kind: PRIVATE LBRACE label_declarations . RBRACE
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
label_declarations: label_declaration_semi . _*
label_declaration_semi: mutable_flag LIDENT COLON possibly_poly_core_type_no_attr_ list_attribute_ SEMI . list_attribute_
```

## Pattern 963

```
generic_type_declaration_no_nonrec_flag_type_subst_kind_: TYPE ext list_attribute_ NONREC type_parameters LIDENT COLONEQUAL . nonempty_type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_
```

### Sample 1

Sentence:
```
type nonrec x :=
```
Stack:
```
interface: TYPE ext list_attribute_ NONREC type_parameters LIDENT COLONEQUAL
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .+ downto effect else end  
 = exception 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type val virtual when while with
```


## Pattern 964

```
signature_item: TYPE ext list_attribute_ NONREC type_parameters type_longident . PLUSEQ private_flag reversed_bar_llist_extension_constructor_declaration_ list_post_item_attribute_
```

### Sample 1

Sentence:
```
type nonrec x
```
Stack:
```
interface: TYPE ext list_attribute_ NONREC type_parameters LIDENT
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
type_longident: mk_longident_mod_ext_longident_LIDENT_ .
generic_type_declaration_no_nonrec_flag_type_subst_kind_: TYPE ext list_attribute_ NONREC type_parameters LIDENT . COLONEQUAL nonempty_type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_
generic_type_declaration_nonrec_flag_type_kind_: TYPE ext list_attribute_ NONREC type_parameters LIDENT . type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_
mk_longident_mod_ext_longident_LIDENT_: LIDENT .
```

## Pattern 965

```
signature_item: TYPE ext list_attribute_ NONREC type_parameters type_longident PLUSEQ . private_flag reversed_bar_llist_extension_constructor_declaration_ list_post_item_attribute_
```

### Sample 1

Sentence:
```
type nonrec x +=
```
Stack:
```
interface: TYPE ext list_attribute_ NONREC type_parameters type_longident PLUSEQ
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```


## Pattern 966

```
signature_item: TYPE ext list_attribute_ NONREC type_parameters type_longident PLUSEQ private_flag . reversed_bar_llist_extension_constructor_declaration_ list_post_item_attribute_
```

### Sample 1

Sentence:
```
type nonrec x += private
```
Stack:
```
interface: TYPE ext list_attribute_ NONREC type_parameters type_longident PLUSEQ PRIVATE
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```

Also covered by these intermediate patterns:
```
private_flag: PRIVATE .
```

## Pattern 967

```
generic_type_declaration_no_nonrec_flag_type_subst_kind_: TYPE ext list_attribute_ type_parameters . LIDENT COLONEQUAL nonempty_type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_
generic_type_declaration_nonrec_flag_type_kind_: TYPE ext list_attribute_ type_parameters . LIDENT type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_
signature_item: TYPE ext list_attribute_ type_parameters . type_longident PLUSEQ private_flag reversed_bar_llist_extension_constructor_declaration_ list_post_item_attribute_
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```

Also covered by these intermediate patterns:
```
type_parameters: LPAREN reversed_separated_nonempty_llist_COMMA_type_parameter_ RPAREN .
```

## Pattern 968

```
generic_type_declaration_no_nonrec_flag_type_subst_kind_: TYPE ext list_attribute_ type_parameters LIDENT COLONEQUAL . nonempty_type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_
```

### Sample 1

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
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .+ downto effect else end  
 = exception 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type val virtual when while with
```


## Pattern 969

```
signature_item: TYPE ext list_attribute_ type_parameters type_longident . PLUSEQ private_flag reversed_bar_llist_extension_constructor_declaration_ list_post_item_attribute_
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
type_longident: mk_longident_mod_ext_longident_LIDENT_ .
mk_longident_mod_ext_longident_LIDENT_: mod_ext_longident DOT LIDENT .
```

## Pattern 970

```
signature_item: TYPE ext list_attribute_ type_parameters type_longident PLUSEQ . private_flag reversed_bar_llist_extension_constructor_declaration_ list_post_item_attribute_
```

### Sample 1

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
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```


## Pattern 971

```
signature_item: TYPE ext list_attribute_ type_parameters type_longident PLUSEQ private_flag . reversed_bar_llist_extension_constructor_declaration_ list_post_item_attribute_
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
&& & and and* as assert ` ! || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```

Also covered by these intermediate patterns:
```
private_flag: PRIVATE .
```

## Pattern 972

```
module_type: SIG list_attribute_ signature . END
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
signature: list_signature_element_ .
list_signature_element_: SEMISEMI . list_signature_element_
```

## Pattern 973

```
delimited_type_supporting_local_open: LPAREN MODULE ext list_attribute_ module_type . RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

Also covered by these intermediate patterns:
```
module_type: SIG list_attribute_ signature END .
```

## Pattern 974

```
attribute: LBRACKETAT attr_id attr_payload . RBRACKET
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
attr_payload: payload .
payload: COLON . _*
```

## Pattern 975

```
fun_expr: WHILE ext list_attribute_ . seq_expr DO seq_expr DONE
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
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```

Also covered by these intermediate patterns:
```
list_attribute_: attribute . list_attribute_
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 976

```
fun_expr: WHILE ext list_attribute_ seq_expr . DO seq_expr DONE
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
seq_expr: fun_seq_expr .
fun_seq_expr: fun_expr SEMI . _*
```

## Pattern 977

```
fun_expr: WHILE ext list_attribute_ seq_expr DO . seq_expr DONE
```

### Sample 1

Sentence:
```
while X do
```
Stack:
```
use_file: WHILE ext list_attribute_ seq_expr DO
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 978

```
fun_expr: WHILE ext list_attribute_ seq_expr DO seq_expr . DONE
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
seq_expr: fun_seq_expr .
fun_seq_expr: fun_expr SEMI . _*
```

## Pattern 979

```
implementation: structure . EOF
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end 
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
structure: list_structure_element_ .
list_structure_element_: SEMISEMI . _*
```

## Pattern 980

```
interface': . interface
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


## Pattern 981

```
interface: signature . EOF
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end 
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
signature: list_signature_element_ .
list_signature_element_: SEMISEMI . list_signature_element_
```

## Pattern 982

```
parse_any_longident': . parse_any_longident
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```


## Pattern 983

```
constr_extra_nonprefix_ident: LPAREN . RPAREN
mk_longident_mod_ext_longident___anonymous_42_: LPAREN . COLONCOLON RPAREN
val_extra_ident: LPAREN . operator RPAREN
```

### Sample 1

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
and as assert ` | |] begin 'a' class : :> , (*comment*) constraint do (**documentation *) done . .. downto effect else end  
 exception external false 1.0 for fun function functor >} >] # if in include inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% <- let x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 984

```
mk_longident_mod_ext_longident___anonymous_42_: LPAREN COLONCOLON . RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 985

```
mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident . DOT UIDENT
mk_longident_mod_ext_longident___anonymous_42_: mod_ext_longident . _*
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
mod_ext_longident: mod_ext_longident LPAREN mod_ext_longident RPAREN .
```

## Pattern 986

```
mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident DOT . UIDENT
mk_longident_mod_ext_longident___anonymous_42_: mod_ext_longident DOT . _*
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 987

```
mk_longident_mod_ext_longident___anonymous_42_: mod_ext_longident DOT LPAREN . COLONCOLON RPAREN
val_extra_ident: LPAREN . operator RPAREN
```

### Sample 1

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
and as assert ` | |] begin 'a' class : :> , (*comment*) constraint do (**documentation *) done . .. downto effect else end  
 exception external false 1.0 for fun function functor >} >] # if in include inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% <- let x ( match >. .< .~ method -> module mutable new nonrec object of open ?label: private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 988

```
mk_longident_mod_ext_longident___anonymous_42_: mod_ext_longident DOT LPAREN COLONCOLON . RPAREN
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```


## Pattern 989

```
parse_any_longident: any_longident . EOF
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

Also covered by these intermediate patterns:
```
any_longident: constr_extra_nonprefix_ident .
constr_extra_nonprefix_ident: FALSE .
```

## Pattern 990

```
parse_constr_longident': . parse_constr_longident
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to try type _ val virtual when while with
```


## Pattern 991

```
parse_constr_longident: constr_longident . EOF
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end 
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
constr_longident: mod_longident DOT LPAREN COLONCOLON RPAREN .
```

## Pattern 992

```
parse_core_type': . parse_core_type
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [@ [@@ [@@@ [| [%% <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open or % + +. += !+ private {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type val virtual when while with
```


## Pattern 993

```
parse_core_type: core_type . EOF
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

Also covered by these intermediate patterns:
```
core_type: core_type attribute .
attribute: LBRACKETAT attr_id attr_payload RBRACKET .
```

## Pattern 994

```
parse_expression': . parse_expression
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
&& & and and* as | || |] class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external functor > >} >] # ## in include != ^ +! land ** inherit initializer [@ [@@ [@@@ [> [< [%% < <- >. method -> module mutable nonrec of open ?label: or % += private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to type val virtual when with
```


## Pattern 995

```
parse_expression: seq_expr . EOF
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end 
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
seq_expr: fun_seq_expr .
fun_seq_expr: fun_expr SEMI . _*
```

## Pattern 996

```
parse_mod_ext_longident': . parse_mod_ext_longident
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 997

```
mk_longident_mod_ext_longident_UIDENT_: mod_ext_longident . DOT UIDENT
parse_mod_ext_longident: mod_ext_longident . EOF
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end 
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
mod_ext_longident: mod_ext_longident LPAREN mod_ext_longident RPAREN .
```

## Pattern 998

```
parse_mod_longident': . parse_mod_longident
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 999

```
mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT
parse_mod_longident: mod_longident . EOF
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end 
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
mod_longident: mk_longident_mod_longident_UIDENT_ .
mk_longident_mod_longident_UIDENT_: mod_longident DOT UIDENT .
```

## Pattern 1000

```
parse_module_expr': . parse_module_expr
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * "s" then ~ to true try type _ val virtual when while with
```


## Pattern 1001

```
parse_module_expr: module_expr . EOF
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end 
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
module_expr: STRUCT list_attribute_ structure END .
```

## Pattern 1002

```
parse_module_type': . parse_module_type
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [%% < <- let let* match >. .< .~ method - -. -> mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 1003

```
parse_module_type: module_type . EOF
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end 
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while
```

Also covered by these intermediate patterns:
```
module_type: SIG list_attribute_ signature END .
```

## Pattern 1004

```
parse_mty_longident': . parse_mty_longident
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 1005

```
parse_mty_longident: mty_longident . EOF
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

Also covered by these intermediate patterns:
```
mty_longident: mk_longident_mod_ext_longident_ident_ .
mk_longident_mod_ext_longident_ident_: ident .
ident: LIDENT .
```

## Pattern 1006

```
parse_pattern': . parse_pattern
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
&& & and and* as assert ! | || |] begin class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto else end  
 = external for fun function functor > >} >] ## if in include != ^ +! land ** inherit initializer {< [@ [@@ [@@@ [> [< [%% < <- let let* match >. .< .~ method -. -> module mutable new nonrec object of open ?label: or % +. += !+ private ? ' {%%%%ext|s|} } ] rec ) ; ;; sig * struct then to try type val virtual when while with
```


## Pattern 1007

```
labeled_tuple_pat_element_list_pattern_: pattern . _*
parse_pattern: pattern . EOF
reversed_labeled_tuple_pattern_pattern_: pattern . COMMA DOTDOT
```

### Sample 1

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
&& & and and* assert ` ! || |] begin 'a' class : := :> (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end 
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
pattern: pattern AS val_ident .
val_ident: LIDENT .
```

## Pattern 1008

```
parse_val_longident': . parse_val_longident
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 1009

```
parse_val_longident: val_longident . EOF
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

Also covered by these intermediate patterns:
```
val_longident: mk_longident_mod_longident_val_ident_ .
mk_longident_mod_longident_val_ident_: val_ident .
val_ident: LIDENT .
```

## Pattern 1010

```
mk_longident_mod_longident_UIDENT_: mod_longident . DOT UIDENT
mk_longident_mod_longident_val_ident_: mod_longident . DOT val_ident
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
mod_longident: mk_longident_mod_longident_UIDENT_ .
mk_longident_mod_longident_UIDENT_: mod_longident DOT UIDENT .
```

## Pattern 1011

```
mk_longident_mod_longident_UIDENT_: mod_longident DOT . UIDENT
mk_longident_mod_longident_val_ident_: mod_longident DOT . val_ident
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 1012

```
toplevel_phrase': . toplevel_phrase
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
 = functor > >} >] ## in != ^ +! land ** inherit initializer [@ [@@ [> [< < <- >. method -> mutable nonrec of ?label: or % += private ? ' } ] rec ) ; sig * struct then to virtual when with
```


## Pattern 1013

```
toplevel_directive: HASH . _*
```

### Sample 1

Sentence:
```
#
```
Stack:
```
use_file: HASH
```
Rejected when looking ahead at any of the terminals in:
```
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type _ val virtual when while with
```


## Pattern 1014

```
toplevel_phrase: toplevel_directive . SEMISEMI
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
toplevel_directive: HASH ident FALSE .
```

## Pattern 1015

```
toplevel_phrase: seq_expr . list_post_item_attribute_ SEMISEMI
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
seq_expr: fun_seq_expr .
fun_seq_expr: fun_expr SEMI . _*
```

## Pattern 1016

```
toplevel_phrase: seq_expr list_post_item_attribute_ . SEMISEMI
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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
list_post_item_attribute_: post_item_attribute . list_post_item_attribute_
post_item_attribute: LBRACKETATAT attr_id attr_payload RBRACKET .
```

## Pattern 1017

```
toplevel_phrase: list_text_str_structure_item__ . SEMISEMI
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end  
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
list_text_str_structure_item__: structure_item . list_text_str_structure_item__
structure_item: INCLUDE ext list_attribute_ module_expr . list_post_item_attribute_
module_expr: STRUCT list_attribute_ structure END .
```

## Pattern 1018

```
use_file': . use_file
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
 = functor > >} >] ## in != ^ +! land ** inherit initializer [@ [@@ [> [< < <- >. method -> mutable nonrec of ?label: or % += private ? ' } ] rec ) ; sig * struct then to virtual when with
```


## Pattern 1019

```
use_file: seq_expr list_post_item_attribute_ list_use_file_element_ . EOF
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end 
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
list_use_file_element_: SEMISEMI . _*
```

## Pattern 1020

```
use_file: list_use_file_element_ . EOF
```

### Sample 1

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
&& & and and* as assert ` ! | || |] begin 'a' class : :: := :> , (*comment*) constraint do (**documentation *) done . .. .+ downto effect else end 
 = exception external false 1.0 for fun function functor > >} >] # ## if in include != ^ +! land ** inherit initializer 1 ~label: lazy { {< [ [@ [@@ [@@@ [| [> [< [% [%% < <- let let* x ( match >. .< .~ method - -. -> module mutable new nonrec object of open ?label: or % + +. += !+ private ? ' {%%ext|s|} {%%%%ext|s|} } ] rec ) ; ;; sig * "s" struct then ~ to true try type X _ val virtual when while with
```

Also covered by these intermediate patterns:
```
list_use_file_element_: SEMISEMI . _*
```

