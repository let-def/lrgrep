# Documentation for DSL

I am working on a DSL that needs documentation, examples and test:

LRGrep is a tool designed to produce high-quality and maintenable syntax error messages for an LR parser.
This document described the version 0.1 of the DSL and the associated tools.

# A DSL for specifying syntax errors

LRGrep takes an "error specification" as input, stored in a file with extension `.mlyl`. It complements a `.mly` grammar and specifies how syntax errors in an LR parser should be handled.

An error specification consists of one or more named **rules**. Each rule is made of a list of **branches**, each containing a **pattern** and an **action**.

The **patterns** are a dialect of regular expressions suitable for matching and extracting information from the stack of an LR parser (the partial derivation built by the parser before encountering a syntax error, REF). The branches are tested in order, and the action associated with the first pattern that matches is executed. Thus, one can think of rules as a function defined by pattern matching on the stack of an LR parser instead of algebraic data types — this is what they are compiled to once processed by the LRGrep compiler (REF).

### Pattern Syntax

Patterns are composed of:
- **Symbols**: Represent grammar symbols (e.g., `ident`, `symbol(arg)`).
- **Filters**: Restrict matches based on grammatical constructions (e.g., `/symbol:`).
- **Captures**: Extract locations and semantic values of grammar symbols (e.g., `ident=`, `[_]`).
- **Repetitions**: Match zero or more occurrences (e.g., `symbol*`, `symbol**`).
- **Alternatives**: Match one of several patterns (e.g., `pattern1|pattern2`).

## Examples

### Example 1: Basic Rule

```ocaml
rule error_message =
    | ident; EQUAL { "Expected assignment" }
;
```

### Example 2: Rule with Filters and Captures

```ocaml
rule my_rule = parse error
| "ident" "=" "ident" { "Expected assignment" }
| "ident" "(" ")" { "Expected function call" }
| {.}
;
```

## DSL reference

Here is the grammar specified using [BNF](https://en.wikipedia.org/wiki/Backus–Naur_form) syntax.

### Error specifications

```
<error-spec> ::= <ocaml-defs>? <rule>+ <ocaml-defs>?
```

An error specification consists of optional heading OCaml definitions, one or more rules, and optional trailing OCaml definitions.

Compiling this specification produces an OCaml module starting with the heading definitions (if any), followed, for each rule, by a function named after the rule that implements it, and finally by the trailing definitions.

### OCaml code

```
<ocaml-defs> ::= "{" ... OCaml definitions ... "}"

<ocaml-expr> ::= "{" ... OCaml expression ... "}"
```

Global definitions and semantic actions are arbitrary pieces of OCaml code wrapped between squiggly braces `{` and `}`.

The definitions should be well-formed module items (see [_module items and definition_](https://ocaml.org/manual/5.3/modules.html#module-items)) while the semantic actions should be expressions (see [_Expressions_](https://ocaml.org/manual/5.3/expr.html)).

### Rules

```
<rule> ::= 
  "rule" <ident> <ident>* "=" 
    "parse" "error"? [ "(" <symbol> (, <symbol>)* ")" ]
  <clause>*
```

A rule is given by three pieces of information:
1. The name and the optional parameters of the rule.
   For instance `rule error_message filename = ...` introduces a rule named `error_message` that receives a single parameter named `filename`.
2. The scope in which this rule is to be applied:
   - `parse` alone does not constraint the scope, meaning that this rule is intended to apply to any instance of the parser.
   - `parse error` signals that the rule is intended to be applied only on parser that encountered a syntax error; this should be the most common case.
   - `parse (some_start_symbol, another_start_symbol)` signals that the rule matches only specific entry points of the grammar, and, therefore, is intended to be applied on parsers recognizing any of these symbols; by default all start symbols are recognized.
   - `parse error (some_symbol)` combines the two and recognizes only errors that occurred when recognizing specific start symbols.
   By restricting the scope of a rule, LRGrep is able to better analyze the specification, which, in turns, allows to generate more optimized code and to report more accurate warnings.
   Using `parse error` is important to be notified of syntax errors not covered by a rule or, conversely, of unreachable error messages; those that cannot be produced by any input.
3. A list of clauses, each recognizing a specific grammatical situation.
   The clauses are tried in order, such that the first clause has the priority over all the other ones, followed by the second, etc.
   This encourages putting the most specific clauses that recognize precise situations first, and gradually fallback to generic error messages (eventually ending at the most general message `| { "syntax error" }`).

A rule is translated to an OCaml function of the same name that evaluates to an option:
- `Some foo` if one of the clause was found to match
- `None` if no clause matched.

### Identifiers and grammatical symbols

Identifiers need to be manipulated carefully in LRGrep because they are used to connect Menhir definitions to OCaml ones, and, as such, some should be valid in Menhir and some should be valid in OCaml.
```
<ident> ::= ["a"-"z"] ["A"-"Z" "a"-"z" "0"-"9" "_" "'"]*
```

Identifiers follow OCaml lexical conventations and start with a lower-case letter followed by letters, digits, underscores, or apostrophes. Note that LRGrep's keywords, `rule`, `parse` and `error`, can be used like normal identifiers.

```
<menhir-ident> ::= ["a"-"z" "A"-"Z"] ["A"-"Z" "a"-"z" "0"-"9" "_" "'"]*

<symbol> ::= <menhir-ident>
           | <menhir-ident> "(" separated_list(",", <symbol>) ")"
```

Menhir identifiers are less constrained and can start with any letter.

The important concept is that of _grammatical symbol_, which is either a menhir identifier (`CHAR`, `expr`, etc) or an instance of a higher-order symbol, reading like function applications (`list(CHAR)`, `complex(example,with(nested),application)`, etc). See [Menhir Manual, Section 5.2 Parameterizing rules](https://gallium.inria.fr/~fpottier/menhir/manual.pdf) for more information.

```
<terminal> ::= <symbol>

<non-terminal> ::= <symbol>
```

In Menhir, the convention is to use upper-case identifiers to refer to terminals and lower-case ones to refer to non-terminals. This is not enforced, though in this grammar we will use `<terminal>` and `<non-terminal>` in contexts where only a terminal or only a non-terminal is expected, respectively.

### Clauses

```
<clause> ::= ("|" <pattern>)+ <action>
```

A clause consists of one or more patterns separated by `|` and an action.

```
<action> ::= <ocaml-expr>
           | "%partial" <ocaml-expr>
           | "{" "." "}"
```

In the simplest case, an action is just a piece of OCaml code that is evaluated when one of the pattern matches, with the rule parameters and variables captured in the patterns bound. This is said to be total action.

A action can also be made partial by prefixing it with `%partial`. A partial action should evaluate to an option. If it is `Some foo`, the clause is considered successful and behaves as if it was a total action returning `foo`. If it is `None`, matching is resumed with the remaining clauses.

From a typing point of view, all total clauses should evaluate to the same type, say `a` --- error messages do not have to be strings, they can be an arbitrary type chosen by the user. Partial clauses should have type `a option`.

The last case `{.}` signals an "unreachable clause": all cases covered by this clause should have be handled by a more specific clause of higher priority. This is detected at compile-time and is intended to help authoring high quality error messages and to prevent regressions (FIXME: the feature is disabled at the moment).

### Patterns

```
<pattern> ::= <expr> <lookahead-specs>?
```

A pattern is a regular expression together with an optional constraint on the lookahead symbols which allow it to match.
A pattern matches if the regular expression matches a _suffix_ of the stack and, if a lookahead constraint has been given, the symbol that caused the parser to fail belongs to it.

```
<lookahead-specs> ::= "@" <lookahead-spec> ("," <lookahead-spec>)*

<lookahead-spec> ::= <terminal>
                   | "first" "(" <non-terminal> ")"
```

The lookahead specifications denote a list of terminals, introduced by a "@" followed by a comma separated list of single lookahead specification:
- either directly a `<terminal>`,
- or `first(sym)`, which evaluates to the list of terminals that appear first in the language recognized by the non-terminal `sym`.

### Regular expressions

```
<expr> ::= <expr> ";" <expr>  // Sequence of two expressions.
         | <expr> "|" <expr>  // Choice between two expressions.
         | "(" <expr> ")"     // Grouping of an expression.
         | <expr> "*"         // Zero or more occurrences of an expression.
         | <expr> "?"         // Zero or one occurrence of an expression.
         | ϵ                  // An empty expression.
```

The usual operators of regular expressions are accepted:
- The disjunction, or alternative, is written `|`, as usual. `a|b` recognizes either `a` or `b`
- The concatenation, or sequence, is written `;`. For instance `a;b` recognizes `a` followed by `b` (the implicit syntax used by traditional regular expressions, `ab`, would be ambiguous in the context of LRGrep)
- The parentheses `( ... )` are used for disambiguation and are not given a special meaning.
- A star suffix, or Kleene star, recognizes an arbitrary number of repetition of an expression; `a*` recognizes the empty word, a single `a`, `aa`, `aaa`, etc.
- A question mark suffix makes an expression optional. `a?` recognizes `a` or the empty word.
- `ϵ` recognizes the empty word; in the surface syntax, it can be expressed either by writing nothing: `a?` is a short-hand for `(a | )`.

```
<expr> ::= <capture>? <symbol>  // Optional capture followed by a symbol.
         | <capture>? _         // Optional capture followed by a symbol.

capture ::= <ident> "="
```

The atoms of a regular expression are not characters but grammar symbols.
An atom can be preceded by an optional capture, written `x =`. When an element is captured, its semantic value is bound to variable `x` in the action. It is also possible to access the source location associated to the element using `$startpos(x)`, `$endpos(x)`, and `$positions(x)`, for the starting location, ending location, and a pair of both, respectively, represented using `Lexing.position`.

The special syntax `_` represents any symbol (like the `.` of standard regular expressions) and comes useful when the actual symbol does not matter.

### Extended expressions

The expressiveness of the DSL is significantly enhanced by a few specific operators that are tailored for working with LR grammars.

#### Filters

The first of these operators is the **filter**:

```
<expr> ::= "/" (<non-terminal> ":")? <filter-symbol>*

<filter-symbol> ::= "."      // The dot marks a position of interest in a rule
                 | <symbol>  // A symbol of the rule
                 | "_"       // Any symbol
                 | "_" "*"   // Any sequence of zero or more symbols
```

A filter acts as a look-ahead constraint: it does not consume any input but restricts the situations to those matching the constraint.

The most basic form of a filter is `/` followed by a grammar item (an item is a rule with a `.` added between the symbols to mark a specific position).

For instance, the rule `expr: MINUS expr` introduces three items `expr: . MINUS expr`, `expr: MINUS . expr` and `expr: MINUS expr.`
The expression `/expr: MINUS . expr` matches a situation in which a `MINUS` has been recognized and an `expr` is expected for the rule to succeed. 

Because this construction is very common and a grammar often of has many variations of similar rules, filters offer many short-hands for recognizing more than one item at once:
- First, the non-terminal no the left-hand side (before the `:`) can be ommitted to match any rule satisfying the right-hand side; for instance if there is also a rule `range: MINUS . expr`, `/ MINUS . expr` matches both.
- `_` acts as a placeholder for any symbol
- `_*` acts as a placeholder for any sequence of symbols; thus the very general filter `/ _* . expr _*` matches any situation where an expression is expected, no matter what comes before or after
- Finally, there can be more than one `.`; for instance the loose filter `/ IF . _* THEN . _* ELSE . _*` matches  failures that happen after any of the keyword of an `if-then-else` and can be a good way to remind users of the correct syntax for using them.

_Sugar_: when using a filter immediately after an expression, the `;` can be omitted. E.g `pos=MINUS; /expr: MINUS . expr` can be written `pos=MINUS /expr: MINUS . expr`, shortened to `pos=MINUS /expr: _ . expr` and generalized to `pos=MINUS / _ . expr`.

#### Reductions 

The second construction specific to LRGrep is the **reduction** operator: 

```
<expr> ::= <capture>? "[" <expr> "]"
```

An LR(1) parser does not reduce a construction immediately after it is completed but instead waits for the next symbol to ensure that it is not a possible continuation of this construction.

To illustrate this, consider a grammar containing the rules:

```
tuple: 
| X
| X COMMA tuple
```

After reading only `X COMMA X COMMA X`, the parser has not yet produced a single `tuple`, even though this prefix is a syntactically correct tuple. If the next symbol is a `COMMA`, then the tuple continues and it would have been incorrect to reduce it already; only if the next symbol differs from `COMMA` is the parser allowed to reduce in sequence:
- `X COMMA X COMMA [X]` -> `X COMMA X COMMA tuple`,
- `X COMMA [X COMMA tuple]` -> `X COMMA tuple`,
- `[X COMMA tuple]` -> `tuple`.

Therefore if we want to recognize a tuple in this situation, we would have to explicitly write a regular expression to recognize all these possibilities, something along the line of `(X; COMMA)*; X`. With a more realistic grammar, this would quickly get impossible to write or maintain. The reduce operator solves this problem: `[tuple]` recognizes any sequence that can be reduced to a tuple.

Almost all expressions are allowed between the brackets, so `[expr | pattern]` recognizes sequences that can be reduced to either an expression or a pattern. The two limitations are that reductions cannot be nested (`[ [tuple] ]` is not supported, though this would not make much sense), and captures are not allowed inside reductions (because the semantic values they would refer to have not been produced since the reductions did not really happen).

FIXME: In the future it should be possible to relax this restriction and at least allow capturing the locations of the symbols that would have been consumed by the reduction. But would this be useful?!

Finally, a reduction can be captured, e.g `v = [tuple]`, but this capture does not give access to the semantic value, only to the locations `$startpos(v)`, `$endpos(v)` and `$positions(v)`.

#### Shortest-and-longest variants

When an expression capturing values can match in multiple ways, it can be important to specify which match one is interested in as this will determine the values and locations bound to the variables.

The default policy is to keep the shortest match, but this can be changed by using variants of the iterative operators:

```
<expr> ::= <expr> "**"                  // Longest match of zero or more occurrences of an expression.
         | <capture>? "[[" <expr> "]]"  // Longest matching sequence of reductions
```

Going back to the previous example with `X COMMA X COMMA X`, matching `v = [tuple]` without any additional constraint,
binds v to the location of the rightmost `X` as this is sufficient to get a well-formed tuple, while `v = [[tuple]]` binds v to a location spanning from the leftmost `X` to the rightmost one.

## Authoring specifications 

FIXME

### Observations on writing expressions

While the project is still very young and has not been applied to many grammars, a few observations emerged during early tests:

- A good pattern seems to mirror the error message explaining the problem; thus, a pattern should be readable as a "formalization" of the associated error message.
- It appears that the Kleene-star is rarely useful; it is generally more efficient, readable and maintenable to match an iterative construction using reduction operators.
- The disjunction operator serves mostly to factor common constructions as they appear. That is, clauses are first written to match individual cases resorting only to symbols, filters and reductions, composed together using sequences. When an error message appears relevant to multiple situations, the different patterns can be merged using disjunctions to isolate the specific parts. 

### The connection between grammars and error specifications

TODO. And between derivations, stacks and patterns. Or "how to interpret an LR stack".

## Migrating specifications

This section collects the breaking changes made to LRgrep implementation.
The goal is to have the format be stable when reaching 1.0, but as the implementation is very young we expect a few breaking changes to happen in the meantime.

### From v0.0 to v0.1

- The `partial` keyword is now written `%partial` to avoid ambiguity with identifiers/grammar symbols.
- `$startloc`/`$endloc` changed to `$startpos`/`$endpos` to match Menhir syntax.

## Changelog

### v0.1 (UNRELEASED)

- important performance improvements (>4x measure on complex grammars)
- critical bug fixes (in code generation, runtime interpreter, and static analyses)
- improvement to the optimizer (leading to smaller automata)
- `partial` keyword is now written `%partial`
- `$startloc`/`$endloc` changed to `$startpos`/`$endpos` to match Menhir syntax
- `$positions(x)` introduced as a short-hand for `($startpos(x),$endpos(x))`

### v0.0

- version released along my PhD defense.
- contains a proof-of-concept version of all core features, but it is not ready for production.
