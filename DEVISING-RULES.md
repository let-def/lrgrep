# Devising new error rules

The important files to specify the grammar and the error messages are:

- the parser `ocaml/parser_raw.mly`, and the lexer `ocaml/lexer_raw.mll`, which are taken from the OCaml frontend with builtin error handling removed
- `ocaml/parse_errors.mlyl` in which we define the error matching rules.

In this document, we will see how to modify `ocaml/parse_errors.mlyl` to deal with specific situations.

### Setting frontend

To use the new frontend remember to setup `OCAMLPARAM` with `demo/setup_shell.sh`:

```shell
$ echo $OCAMLPARAM
$ # New frontend is not setup!
$ sh demo/setup_shell.sh
export OCAMLPARAM='pp=/.../lrgrep/_build/default/ocaml/frontend.bc,_'
$ eval `sh demo/setup_shell.sh`
$ echo $OCAMLPARAM
pp=/.../lrgrep/_build/default/ocaml/frontend.bc,_
$ # New frontend is ready.
```

If you want to temporarily use the normal frontend, for instance to compare the two outputs, you can temporarily reset `OCAMLPARAM`:

```shell
$ ocamlc ... # If OCAMLPARAM is set, new frontend is used
$ OCAMLPARAM= ocamlc ... # Default frontend will be used for this command
```

### Development loop

An iteration cycle usually looks like:

1. Start from a simple input with a syntax error
2. Add or tweak a rule in `ocaml/parse_errors.mlyl`
3. Run `make` to rebuild the frontend
4. Run `ocamlc` on the input

Repeat as necessary.

Two tools are provided to help working with error messages:

- the interpreter, invoked with `dune exec ocaml/interpreter.exe <input-file.ml>` , which gives detailed information about the state of the parser at the point of failure
- coverage checks using `dune exec src/main.exe -- -coverage ocaml/parse_errors.mlyl -g ./_build/default/ocaml/parser_raw.cmly`, which will print pieces of input which are not handled by any rule

## Simple case, starting from an sample input

Consider the following example from StackOverflow: https://stackoverflow.com/questions/70691144/match-inside-match-ocaml-raises-syntax-error.

This code snippet has multiple problems, so let’s stick to the first part.
The type declarations in this part switches from a colon `:` to an equal sign `=` in the middle of a record type definition:

```ocaml
type 'a grid = 'a Array.t Array.t
type problem = { initial_grid : int option grid }
type available = { loc : int * int; possible : int list }
type state = { 
  problem : problem;
  current_grid : int option grid;
  available = available list
} 
```

Without any patterns `ocamlc` just yields a syntax error:

```ocaml
$ ocamlc demo/stackoverflow3.ml 
File "demo/stackoverflow3.ml", line 7, characters 12-13:
7 |   available = available list
                ^
Error: Syntax error
```

We now run the interpreter:

```
$ dune exec ocaml/interpreter.exe demo/stackoverflow3.ml
File "demo/stackoverflow3.ml", line 9, characters 76-77, parser stack (most recent first):
- line 9:66-75	LIDENT
		  [label_declaration_semi: mutable_flag LIDENT . COLON possibly_poly(core_type_no_attr) list(attribute) SEMI list(attribute)]
		  [label_declaration: mutable_flag LIDENT . COLON possibly_poly(core_type_no_attr) list(attribute)]
- line 9:65-65	mutable_flag
- line 9:33-65	label_declaration_semi
- line 9:14-33	label_declaration_semi
- line 9:13-14	LBRACE
- line 9:11-12	EQUAL
- ...
```

The interpreter prints a stack trace of the parser just before receiving the erroneous token. The most recent state is printed first. The output reveals that the parser is mid-way though either a `label_declaration_semi` or a `label_declaration` rule.
 In this case `.` indicates state of the parser in the line
 `[label_declaration: mutable_flag LIDENT . COLON possibly_poly(core_type_no_attr) list(attribute)]`

We can now capture the problematic situation with a pattern recognizing precisely this situation:

```
| [label_declaration: mutable_flag LIDENT . COLON]
   { ... }
```

`[...]` is a pattern matching [LR items](https://en.wikipedia.org/wiki/LR_parser#Items). In this case, we omitted `possibly_poly(core_type_no_attr) list(attribute)`  in the pattern. This is a short-hand notation for all items that have a matching prefix.

After matching this situation, we want to express that we are looking ahead at an equal sign in the token stream.
The lookahead `token` is passed as a parameter to the semantic action so we can simply pattern match on it.

It also means that our rule is not total: it should only apply when the lookahead token is `Parser_raw.EQUAL`. This is expressed by adding the `partial` keyword before the semantic action. When an action is `partial`, it should evaluate to an option. Returning `Some _` means that the rule applied, returning `None` forces lrgrep to resume matching with the next rules. With this in mind, we now formulate the following pattern and add it to `ocaml/parse_errors.mlyl`:

```
| [label_declaration: mutable_flag LIDENT . COLON]
  partial {
    match token with
    | Parser_raw.EQUAL -> 
      Some "Expecting ':' to declare the type of a record field, not '='"
    | _ -> None
  }
```

Next we recompile the ocaml frontend with `make` for the frontend to pick up the newly added error pattern.

Rerunning `ocamlc` now gives a nice error message:

```
$ ocamlc demo/stackoverflow3.ml 
File "demo/stackoverflow3.ml", line 9, characters 76-77:
9 | type state = { problem : problem; current_grid : int option grid; available = available list } 
                                                                                ^
Error: Expecting ':' to declare the type of a record field, not '='
```

## 

## A more complex example

Another example is the following also from StackOverflow: https://stackoverflow.com/questions/52323697/why-is-there-a-ocaml-syntax-error-on-if-statement
This example has an unfortunate semicolon `;` in the `then` branch
making the parser believe that the developer wanted to express a one-armed if:

```ocaml
let rec list_above thresh lst =
  if lst = [] then 
    printf("Herewego");
  else 
    begin 
  if (List.hd lst) >= thresh then 
    (((List.hd lst)::(list_above thresh List.tl lst)))
  else if (List.hd lst) < thresh then 
    ((list_above thresh List.tl lst));
end
;;
```

We again run the interpreter to dump the state of the parser when it fails:
```
$ dune exec ../ocaml/interpreter.exe stackoverflow2.ml
Entering directory '/home/jmi/software/lrgrep'
File "stackoverflow2.ml", line 4, characters 2-6, parser stack (most recent first):
- line 3:22-23	SEMI
  	  seq_expr ::= expr SEMI . PERCENT attr_id seq_expr
  	  seq_expr ::= expr SEMI . seq_expr
  	  seq_expr ::= expr SEMI .
- from 2:2 to 3:22	expr
  	↱ seq_expr
  	  strict_binding ::= EQUAL seq_expr .
- line 1:30-31	EQUAL
  	↱ strict_binding
  	  fun_binding ::= strict_binding .
  	↱ fun_binding
  	  strict_binding ::= labeled_simple_pattern fun_binding .
- line 1:26-29	labeled_simple_pattern
  ```

The output tells us that we are half-way through parsing a sequential
expression `seq_expr` and that `if`-`then` tokens have already been reduced by
the one-armed `if` rule in the OCaml grammar:
```
  | IF ext_attributes seq_expr THEN expr
    { Pexp_ifthenelse($3, $5, None), $2 }
```

Like in the above example, we can express a rule of the same shape to capture the situation:
```
  | expr as e; SEMI
  partial {
    match token with
    | Parser_raw.ELSE -> ...
    | _ -> None
  }
```

In this case it gets a bit more tricky as the expression has already
been reduced by the parser. To express the desired rule we therefore
need to inspect the result of the semantic action of the OCaml parser,
i.e., an OCaml AST. We therefore pattern-match on a one-armed `if` in
the shape of a `Pexp_ifthenelse` with an omitted (`None`)
`else`-branch and report a suitable error message:
```
| expr as e; SEMI
partial {
  match token with
  | Parser_raw.ELSE -> (
    match e with
    | None -> assert false
    | Some (Parser_raw.MenhirInterpreter.Element (state, expr, startp, _endp)) ->
      match Parser_raw.MenhirInterpreter.incoming_symbol state with
      | N N_expr -> (
      match expr.pexp_desc with
      | Pexp_ifthenelse(_, _, None) ->
        Some ("The semicolon line "
              ^ string_of_int startp.pos_lnum
              ^ ", character "
              ^ string_of_int (startp.pos_cnum - startp.pos_bol)
              ^ " terminates the if _ then _ expression. \
                 Remove it to add an else branch."
                 )
      | _ -> None
      )
    | _ -> None
    )
  | _ -> None
}
```

After adding the above to `ocaml/parse_errors.mlyl` we again recompile
the frontend by running `make`. We can now observe the improved error
message:

```
$ ocamlc demo/stackoverflow2.ml 
File "demo/stackoverflow2.ml", line 6, characters 2-6:
6 |   else
      ^^^^
Error: The semicolon line 4, character 2 terminates the if _ then _ expression. Remove it to add an else branch.
```
