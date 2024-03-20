### Notation

We describe the grammars using a variant of BNF notation, extended with "?" to denote an optional construction (e.g. `A?` is equivalent to `(A | )`, "*" to denote zero or more occurrences (`A*` is `(A A* | )`) and `+` to denote one or more occurrences (`A+` is `A (A+ | )`).

Literal text is delimited by quotes `" ... "` and lowercase words denote non-terminals (`main`, `rule`, etc). Symbols are implicitly separated by whitespaces.

# Calc: a minimal example

Here is a small error specification to add error messages to Menhir's Calc demo:

```ocaml
{
  let print_loc (pos : Lexing.position) =
    Printf.sprintf "line %d, column %d"
     pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
}

rule error_message = parse error (main)
| /expr: . INT { "Syntax error: expecting an integer" }
| lpos=LPAREN; [expr / ... . RPAREN]
  { "Syntax error: expecting a closing parenthesis (opened at " ^ print_loc $startloc(lpos) ^ ")" }
```

The full example can be found in [examples/calc](https://github.com/let-def/lrgrep/tree/parser-tweak/examples/calc) directory, with the specification living in [parse_errors.mlyl](https://github.com/let-def/lrgrep/blob/parser-tweak/examples/calc/parse_errors.mlyl) and the grammar in [parser.mly](https://github.com/let-def/lrgrep/blob/parser-tweak/examples/calc/parser.mly). The first part of the file, delimited by the `{`/`}` pair, is called the "header" and contains arbitrary OCaml definitions that can be referred to later. The header is optional, and followed by a list of rules. This file defines a single rule called `error_message`.

*Clauses.* The rule has two clauses. A clause is made of a pattern and an action. The first pattern is ` /expr: . INT` and the action is `"Syntax error: expecting an integer"`. The pattern matches when we are ready to recognize an integer. In LR parlance, it is when the item `expr: . INT` is active. `expr: INT` is a rule of the grammar (see [parser.mly line 20](https://github.com/let-def/lrgrep/blob/parser-tweak/examples/calc/parser.mly#L20)), and the `.` identifies a specific position in this rule. `/` is the "filtering" operator that restricts matches according to active items. 
The second pattern is slightly more involved. It is meant to identify situations in which a closing parenthesis is expected. The relevant grammatical rule is `expr: LPAREN expr RPAREN`, so why not simply using `/expr : LPAREN expr . RPAREN`, like in the first pattern? The problem has to do with _reductions_. When reaching a situation where a closing parenthesis is expected, the parser is not directly in a state where the item `expr : LPAREN expr . RPAREN` is valid, because it has not reduced the `expr`. For instance with input `(1 + 2`, just before failing the parser's stack is `LPAREN expr PLUS INT`, which doesn't match our pattern. To give more control to the author of error messages, LRgrep provides the `[ ... ]` "reduction" operator to let one decides whether to reduce or not before matching a sub-pattern. Since we want to match after reducing, the right pattern is `[expr / expr: LPAREN expr . RPAREN]`: reduce to an `expr`, and then filter to those where `expr: LPAREN expr . RPAREN` is active. `[expr / ... . RPAREN]` is just a shorthand to avoid repetition: `...` acts as a "glob", expanding to any sequence of symbol that would result in a valid item. Finally, we prefix the pattern with `lpos=LPAREN;`. `LPAREN` restricts the match to situations where an opening parenthesis precedes the expression. `lpos=` is a capture, which allows us to refer to the position of this parenthesis in the message using `$startloc(lpos)`.

*Preferring precise patterns.* In both cases, we can observe that the pattern and the message mirror each other: a good pattern should, as much as possible, translate in formal terms the content of the message. For instance, using `LPAREN; [expr]` for the second pattern would have the same effect, even though the closing parenthesis is not mentioned. In the grammar, an `RPAREN` always appear after `LPAREN expr`: it is implied anyway. However this is not future proof. If the grammar is later extended with an hypothetical rule like `definition: LPAREN expr COLON type RPAREN`, the clause would still apply, silently leading to an incorrect error message. In this case, only a `COLON` allow to continue the analysis! Therefore, it is a good practice for the pattern to be at least as restrictive as the error message.

*Scope of a rule*. The text `parse error (main)` indicates that the rule is meant to handle errors that occurred when parsing from the `main` entrypoint of Calc's grammar. `error` is optional; if it is omitted the generator will be applicable to any parsing configuration, not just the erroring ones. The entrypoint specification admits other variants as well: `(main)` specifies a single entrypoint, a comma-separated list denotes `(main, aux, ...)` multiple entrypoints, and completely omitting it is equivalent to listing all entrypoints. Therefore using `parse`, `parse (main)` or `parse error` would also be valid. But `parse error (main)` is more precise: it allows the code generator to output a smaller program and the coverage analysis to give a more relevant information. 

*Textual precedence.* If a situation matches mutiple clauses, the first clause, in text order, has priority. (In this example, the two clauses are mutually exclusive, but it is not the case in general.)

*Coverage check.* To guarantee that all error cases are covered we can run a coverage analysis: `lrgrep -coverage parser.cmly parse_errors.mlyl`. The coverage analysis outputs a list of uncovered sentences. In this case, it should output an empty list. For this simple grammar, only two rules are sufficient for covering all cases with reasonable explanations. (An unreasonable clause would be `| { "Syntax error" }`: it provides full coverage and a useless message.)

FIXME: The coverage check does not handle the first transitions correctly, it will incorrectly report full coverage or will not provide counter examples.

# Syntax

An `.mlyl` file is made of a sequence of rules, as well as optional heading and trailing block of OCaml code, wrapped between `{` ... `}`.

```
mlyl: 
  header
  rule* 
  trailer

header: action?
trailer: action?

action: "{" OCaml code ... "}"
```

The heading and trailing codes, if present, should be syntactically valid OCaml structures.

**Rules.** A rule starts with the keyword "rule", followed by a list of identifiers: the name of the rule, mandatory, followed by zero or more parameters. Identifiers follow OCaml lexical conventions. The rule might apply to any parser (`= parse`), or to only parse errors (`= parse error`), and can be restricted to only certain entrypoints by specifying them between parentheses. After that comes the list of clauses.

```
rule: "rule" ident ident* "=" "parse" "error"? entrypoints? clause*

entrypoints: "(" symbol ( "," symbol )* ")"

ident: ["a"-"z" "A"-"Z" "_"]  ["a"-"z" "A"-"Z" "0"-"9" "_" "'"]

symbol: ident | ident "(" (symbol ("," symbol)* )? ")"
```

Grammatical symbols follow Menhir's conventions:

- A terminal starts with an upper-case, like `INT`.
- A (ground) non-terminal starts with a lower-case, like `expr`.
- An instance of an higher-order non-terminal uses function application notation. They start with a non-terminal ident followed by a comma-separated list of symbols between parentheses. E.g. `list(INT)`. 

**Clauses**. A clause starts with a "|" followed by a pattern and an optional lookahead constraint, restricting the situations in which it applies, and ends with an action. Three kind of actions are possible:

- *total action:* common case is a piece of OCaml code wrapped between "{"/"}" and that is executed when the clause matches. Matching ends, returning the value the OCaml code evaluated to.
- *partial action:* sometimes, semantic information are needed to decide whether a clause applies or not. To handle this case, LRgrep provides the `%partial` keyword. If an action is preceded by `%partial`, the OCaml code should evaluate to an `option` value: if it is `None`, matching continues with next clauses. If it is `Some ` value, matching ends, as in the total case.
- *unreachable action:* to ensure that a case is never reached, it is possible to use an unreachable action noted `{ . }`. In this case, LRGrep will check that no input can match this clause, because all cases covered by the pattern were already covered by clauses with higher priority.

TODO: unreachable action are not implemented yet.

```
clause: "|" pattern lookahead? ( "|" pattern lookahead? )*
        clause_action

clause_action: "%partial" action | "{" "." "}"
```

**Patterns.** The pattern language is a dialect of regular expressions. Therefore we can find the usual regular operators: sequence, disjunction, repetition (Kleene-star), atom (matching a single symbol) and the empty match, or $\epsilon$.

The LRgrep specific operators are filters and reductions. A filter restricts current matches to the ones in which a designated item is active, without consuming any input. A reduction operator recognizes situations that would match after a sequence of zero or more reductions. Furthermore, LRgrep supports captures for the base case of matching a symbol and the reduction can be bound to a name.

The Kleene-star and the reduction operators also have longest variant to disambiguate captures. When there are multiple ways of matching the same input, the different matches can lead to different values being captured. `[...]` and `*` favor the shortest matches while `[[...]]` and `**` favor the longest ones.

```
(* Base patterns *)
pattern:
  | "/" filter                -- item filtering
  | (ident "=")? capturable   -- optionnally extracting a value

filter: ( symbol ":" )?  ( "." | "..." | symbol )*

(* Patterns from which one can extract a value *)
capturable:
  | symbol                    -- one symbol
  | "_"                       -- any symbol
  | "[" pattern "]"           -- reduction pattern

(* Composition patterns *)
pattern:
  |                           -- empty pattern, always matching
  | pattern "*"               -- pattern repetition (Kleene star)
  | pattern "?"               -- optional pattern
  | pattern ";" pattern       -- pattern sequence
  | pattern "|" pattern       -- pattern disjunction
  | "(" pattern ")"           -- parentheses for disambiguation

(* Longest variants *)
capturable: "[[" pattern "]]" -- longest reduction
pattern: pattern "**"         -- longest repetition
```

**Lookahead constraints.** A clause can be restricted to match only for certain lookahead tokens. By adding "@" followed by a comma-separated list of symbols after the pattern of a clause, the pattern will match only when the lookahead token that caused a parser to fail belongs to the list. The list specifies the list of terminals allowed as lookahead. Elements of the list are either a terminal or the special syntax `first(nt)`, where `nt` is a nonterminal, which expands to the list of terminals in $\text{FIRST}(nt)$, the terminals that can appear as the first element of sentences generated by $nt$: $\{ a \in T | \exists u, a.u \in L(nt) \}$. In the _calc_ example, `first(expr) = INT, LPAREN`.

```
lookahead: "@" (lookahead_symbol ("," lookahead_symbol)* )?

lookahead_symbol: terminal | "first" "(" nonterminal ")"
```

# Integration with Menhir

LRgrep works by postprocessing the automaton produced by Menhir at compile time, and analyzing the state of the parser after a syntax error at runtime.

For this to be possible, a few things are necessary:

- Menhir has to save the automaton for LRgrep to process, and it has to generate an introspectable parser; this is done with the `--inspection --table --cmly` flags
- a custom interpretation loop has to be used to get access to the parser at the point of failure

With this, LRgrep compiles an error specification to functions that dissect a failing parser, functions that are called from the custom interpreter loop.

### The calc example

We can take a look at the calc example to see the changes.

In the [dune](examples/calc/dune) file:

- the parser is compiled with the extra flags:

  ```scheme
  (menhir
    (modules parser)
    (flags :standard --inspection --table --cmly))
  ```

- a custom rule is used to invoke LRgrep:

  ```scheme
  (rule
    (targets parse_errors.ml)
    (deps    parser.cmly parse_errors.mlyl)
    (action
       (run lrgrep parse_errors.mlyl -g parser.cmly
            -o parse_errors.ml)))
  ```

  `parse_errors.mlyl` is the error specification and `parser.cmly` the parsing automaton (produced by menhir `--cmly`); both are consumed by LRgrep to produce `parse_errors.ml`, which is the OCaml module implementing the error specification

- `lrgrep.runtime` is added as a dependency, it is required by LRgrep generated code (and `menhirLib` is required by Menhir's table-based parsers):

  ```
  (executable
   (name main)
   (libraries menhirLib lrgrep.runtime))
  ```

  The other change is the custom loop in `main.ml`:

  ```ocaml
  let rec parse
      (lexbuf : Lexing.lexbuf)
      (last_token : Parser.token * Lexing.position * Lexing.position)
      (last_env : _ Interpreter.env)
      (checkpoint : 'a Interpreter.checkpoint)
    : ('a, string) result
    =
    match checkpoint with
    | InputNeeded _ -> consume lexbuf checkpoint
    | Shifting (_, _, _) | AboutToReduce (_, _) as cp ->
      parse lexbuf last_token last_env (Interpreter.resume cp)
    | Accepted x -> Ok x
    | Rejected -> assert false
    | HandlingError _ ->
      handle_error last_token last_env

  and handle_error last_token last_env =
    match Parse_errors.error_message last_env Lexing.dummy_pos last_token with
    | None -> Result.Error "Syntax error (no handler)"
    | Some err -> Result.Error err

  and consume lexbuf = function
    | Interpreter.InputNeeded env as checkpoint ->
      begin match Lexer.token lexbuf with
      | raw_token ->
        let token = (raw_token, lexbuf.lex_start_p, lexbuf.lex_curr_p) in
        parse lexbuf token env (Interpreter.offer checkpoint token)
      | exception Lexer.Error msg ->
        Result.Error msg
      end
    | _ -> assert false
  ```

  The structure of the loop is common for Menhir parser's, with two main changes:

  - the `HandlingError _` case and `handle_error` function bypass the usual error handling strategy (inherited from good ol' Yacc !); instead, they pass the control to LRgrep handler `Parse_errors.error_message`
  - the `last_token` and `last_env` are preserved. This is necessary for LRgrep to operate correctly: Menhir does not always detect an error after shifting but might delay the report after a few reductions. At this point it is too late for LRgrep to work properly.

  The rest of the loop is concerned by feeding the parser with new tokens and advancing the analysis (shifting and reducing) between two tokens, until reaching a success or a failure.

  TODO: this loop is generic and could be provided by LRgrep runtime library; this involves functor, not exciting >_<

# Working out an error specification

There are three main tools provided to help understanding what does wrong with a grammar: a dynamic one, the interpreter and two static ones, sentence enumeration and coverage checking.

The interpreter parses an input and provides an annotated stack dump when it fails.

CAVE AT: the interpreter is only provided for the OCaml grammar at the moment, see [ocaml/interpreter.ml](ocaml/interpreter.ml), coverage checking is a rough proof-of-concept (FIXME!)

The enumeration works by listing sentences that stresses the failure paths of the parser. The coverage compares a grammar and an error specification to find uncovered cases and unreachable messages.


## The annotating interpreter

The interpreter is useful to craft an error pattern for a known situation.

Let's start with a really simple example:

```ocaml
let x = when 5
```

The OCaml parser tells us that it is a syntax error:

```sh
$ echo 'let x = when 5' | ocaml -stdin
File "(stdin)", line 1, characters 8-12:
1 | let x = when 5
            ^^^^
Error: Syntax error
```

LRGrep interpreter provides an annotated stack dump:

```sh
$ echo 'let x = when 5' | dune exec ocaml/interpreter.exe -
File "<stdin>", line 1, characters 8-12, parser stack (most recent first):
		  [strict_binding: EQUAL . seq_expr]
- line 1:6-7	EQUAL
- line 1:4-5	val_ident
- line 1:3-3	rec_flag
- line 1:3-3	list(attribute)
- line 1:3-3	ext
- line 1:0-3	LET
- entrypoint	implementation
```

In this case, the second line, which contains the item set of the parser state before failure, is probably the most interesting one: `[strict_binding: EQUAL . seq_expr]`. It tells us that we are expecting an expression next (the dot is just before `seq_expr`).

The other lines details the path that led us to this situation from the start of an OCaml implementation (*.ml) file, though this is likely irrelevant for this problem.

A reasonable error clause could be:

```
| / ... . seq_expr ... { "Expecting an expression at this point." }
```

Or, rather, the more general:

```
| [_* / ... . seq_expr ...] { "Expecting an expression at this point." }
```

which also covers all stacks which reduce to a stack expecting an expression.

See the experience report on the design of error patterns for the [OCaml parser](https://github.com/let-def/phd-lrgrep/blob/main/notes/case-studies/ocaml-spec.md) for more information.

## Enumeration

The enumeration works by listing sentences that exhaustively cover all the ways a parser can _locally_ fail.

Of course, there is generally an infinite number of ways a parser can fail. By locally, we mean variants of the same failure are considered equivalent:

- the path that led to a failure is ignored, e.g. `let x = 5 when` and `let x = 5 let y = 6 when` are considered equivalent
- two failures that go through the reductions are considered equivalent, even if the number of reduction differ; for instance `x * 2 if` and `x * 2 * 3 if` are considered equivalent

With these two identifications, only a finite number of situations have to be considered and they can be covered using only clauses of the form `[_* /A: b . c /D : e . f]`, that is, set of items modulo reductions.

It is helpful to look at the sentences produced by enumeration to get an idea of the possible failures of a grammar.

For instance, the calc grammar can be enumerated using `make enumerate`, or by manually executing `dune exec lrgrep -- -enumerate lr0 parse_errors.mlyl -g ../../_build/default/examples/calc/parser.cmly`. The `lr0` flag after specifies the precision of the enumeration; `lr0` is the correct value for 99% of cases (TODO: make it the default and add another flag for advanced use cases).

The first lines of output look like:

```
main INT MINUS @ error TIMES RPAREN PLUS EOL DIV # [ /expr: expr MINUS . expr ]
main INT DIV @ error TIMES RPAREN PLUS EOL DIV # [ /expr: expr DIV . expr ]
main INT PLUS @ error TIMES RPAREN PLUS EOL DIV # [ /expr: expr PLUS . expr ]
main LPAREN INT RPAREN @ error LPAREN INT # [ /expr: LPAREN expr RPAREN . ]
```

Each line represent a different case and has the form `<entrypoint> <symbols...> @ <lookaheads...> # <state>*`.

The entrypoint is `main` in these examples. The symbols are the prefix of the sentence that caused the failure, and the lookaheads are each terminal that can cause a failure if appended to the prefix. For instance, for the first line the prefix is `INT MINUS` and the different ways to make it fail are `INT MINUS error`, `INT MINUS TIMES`, `INT MINUS RPAREN`, `INT MINUS PLUS`, `INT MINUS EOL` and `INT MINUS DIV` (`error` is a special terminal introduced by menhir to deal with Yacc-style errors; you can safely ignore it).

After that comes the list of states that causes the failure. A state is represented as the set of its kernel items wrapped between `[ ... ]`. The first line fails with a single state (because there are no reductions involved, see below for an example involving reductions,) which has a single item `/expr: expr MINUS . expr`.

A very specific message can be written:

`| /expr: expr MINUS . expr { "Expecting an expression after '-'."}`

Multiple states appear in a line when reductions are involved. For instance look at these two lines (with states formatted for better display):

```
main LPAREN LPAREN INT RPAREN @ EOL
  [ /expr: LPAREN expr RPAREN . ]
  [ /expr: expr . DIV expr
    /expr: expr . TIMES expr
    /expr: expr . MINUS expr
    /expr: expr . PLUS expr
    /expr: LPAREN expr . RPAREN ]
main LPAREN INT RPAREN @ RPAREN
  [ /expr: LPAREN expr RPAREN . ]
  [ /main: expr . EOL
    /expr: expr . DIV expr
    /expr: expr . TIMES expr
    /expr: expr . MINUS expr
    /expr: expr . PLUS expr ]
```

After a closing parenthesis, we are immediately in state `[ /expr: LPAREN expr RPAREN . ]`, but after reducing `expr: LPAREN expr RPAREN` different states are possible, as illustrated in these examples.

See the experience report on the design of error patterns for the [Mini-Elm](https://github.com/let-def/phd-lrgrep/blob/main/notes/case-studies/elm-spec.md) parser for more information. The Elm error specification is automatically generated by fuzzing the reference parser using enumeration.

## Coverage checks

The coverage checker can be enabled by passing `-coverage` to LRGrep.

FIXME: It is only a proof-of-concept that was tested on OCaml grammar and is not guaranteed to work on other grammars. Namely, a few cases important are not handled, which lead to either infinite loops or false negatives!!
