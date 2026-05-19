# Getting started with LRgrep

LRgrep addresses the problem of producing clear, actionable syntax error messages for Menhir-generated LR parsers. Traditional LR parsers can report the location of a failure, but their low-level stack representations make it difficult to craft high-quality explanations.

LRgrep solves this by providing a declarative domain-specific language (DSL) that lets you describe error situations directly in terms of your grammar's symbols and rules. Instead of hardcoding error rules or manually inspecting parser states, you write a specification that maps parser stack configurations to tailored error messages. The toolchain compiles this specification into an efficient recognizer, performs exhaustive static checks, and provides interactive diagnostics to help you iteratively refine your error handling.

---

## 2. LRgrep's compiler and DSL

At the heart of LRgrep is a compiler that transforms a high-level error specification into a runtime pattern matcher. An LRgrep specification is compiled into a recognizer that operates on the parser's stack at failure points.

As a guiding example, we will use this simple arithmetic grammar:
```
%left PLUS MINUS
%left TIMES DIV
%nonassoc UMINUS

main: e = expr EOL { e }

expr: i = INT                     { i }
    | LPAREN e = expr RPAREN      { e }
    | e1 = expr PLUS e2 = expr    { e1 + e2 }
    | e1 = expr MINUS e2 = expr   { e1 - e2 }
    | e1 = expr TIMES e2 = expr   { e1 * e2 }
    | e1 = expr DIV e2 = expr     { e1 / e2 }
    | MINUS e = expr %prec UMINUS { - e }
```

### The specification language
An LRgrep specification (typically stored in a `.lrgrep` file) consists of one or more **rules**. Each rule is a sequence of **clauses**, where a clause pairs a **pattern** with an **action** (a fragment of OCaml code). Patterns are tested in order against the suffix of the parser's stack; the first match triggers its associated action, which constructs the error message.

Patterns are regular expressions built from grammar symbols and enhanced with two domain-specific constructs:

- **Filters** (`/ item`): Impose constraints on what the parser expects next without consuming stack elements. An *item* (or LR(0) item) is a production with a dot `.` inserted at a specific position, representing a partially recognized rule. For example, `expr: LPAREN . expr RPAREN` means an opening parenthesis has been read and an expression is expected. Filters support globbing: `_` matches any single symbol, `_*` matches any sequence, and the left-hand side (`expr:` in this case) can be ommitted. Thus, `/ _* . expr _*` matches any situation where an expression is a legal continuation, regardless of surrounding context.
- **Reductions** (`[ pattern ]`): Match stack segments that can be reduced to a given non-terminal, regardless of whether the parser has performed those reductions yet. This bridges the gap between the raw stack and the grammatical structure the user intends. For example, given the input `(1 + 2`, the parser's stack ends with `LPAREN expr PLUS INT`. Without reduction, the immediate context focuses only on the trailing `INT`. The pattern `[expr]` matches both `INT` and `expr PLUS INT` because both can be reduced to an expression. Composed with a filter as in `[expr / LPAREN expr . RPAREN]`, this pattern specifically targets expressions that follow an opening parenthesis and expect a closing one, enabling precise, context-aware messages.

### Compilation
Given a compiled Menhir grammar (`parser.cmly`) and a specification (`errors.lrgrep`), the compiler generates an OCaml module:
```bash
lrgrep compile -g parser.cmly -s errors.lrgrep -o errors.ml
```
The resulting module exports functions (one per rule) that accept the parser's environment and the offending token, returning the semantic value of the clause that matchedx (or `None` if there was no match). The compiler also validates the specification against the grammar, to ensure that generation is sound.

> **Example: Arithmetic Calculator**
>
> ```ocaml
> rule error_message = parse error (main)
> | / . INT
>   { "Expecting an integer" }
> | lpos=LPAREN; [expr / LPAREN expr . RPAREN]
>   { "Expecting a closing parenthesis (opened at " ^ print_loc $startpos(lpos) ^ ")" }
> ```
> This specification covers common arithmetic mistakes by combining stack reductions (`[expr]`), contextual filters (`/ LPAREN expr . RPAREN`) to produce targeted suggestions. It also shows that semantic values and locations can be extracted from the stack to report the location of the opening parenthesis.

---

## 3. Runtime integration

Integrating LRgrep into an existing Menhir-based parser requires minimal boilerplate. The tool relies on Menhir's incremental parsing API, which exposes the parser's environment at the moment a syntax error is detected.

Below is a minimal snippet demonstrating how to hook the compiled LRgrep module into a parser's failure handler using Menhir's `loop_handle_undo`:

```ocaml
module MI = Parser.MenhirInterpreter

type position = Lexing.position
type triple = Parser.token * position * position

(* Assume [Errors] is the module generated by LRgrep *)
let print_syntax_error (env : _ MI.env) (triple : triple) =
  match Errors.error_message env triple with
  | Some msg ->
    eprintf "Syntax error.\n%s\n%!" msg
  | None ->
    eprintf "Syntax error (uncovered situation).\n%!"

let parse (input : unit -> Parser.token * position * position) =
  (* Remembering the last token *)
  let last_triple = ref None in
  let input' () =
    let result = input () in
    last_triple := Some result;
    result
  in
  let succeed value = Some value in
  (* Failure handler *)
  let fail (checkpoint : _ MI.checkpoint) (triple : triple) =
    match checkpoint with
    | MI.InputNeeded env ->
      print_syntax_error env triple;
      None
    | _ ->
      (* The first checkpoint of the handler is guaranteed to be in
         `InputNeeded` state *)
      assert false
  in
  MI.loop_handle_undo succeed fail input' Parser.Incremental.main
```

When the parser encounters an illegal token, `loop_handle_undo` yields the last checkpoint of the form `InputNeeded env` (the state of the parser immediately before seeing the token). The environment `env` represents the parser's stack and state just before the failure. By passing `env` and the offending token to the compiled LRgrep function, you obtain an explanation tailored to this exact syntactic context.

---

## 4. Static analyses: enumeration and coverage

Writing error specifications is an iterative process. LRgrep provides static analyses that guide you from an empty specification to a complete, maintainable one.

### Enumeration: starting a new specification
When beginning with a new grammar, it is often unclear which error situations need to be handled. The `cover` command (with the `-e` flag) enumerates uncovered error states and suggests concrete sample sentences that trigger them:
```bash
$ lrgrep enumerate -g parser.cmly -e
...
## Pattern 3

| [_* /expr: LPAREN expr . RPAREN]

### Sample 0

Sentence:
  LPAREN INT
Stack:
  main: LPAREN INT
Rejected when looking ahead at any of the terminals in:
  EOL INT LPAREN
...
```
This output serves as a design guide and a baseline test suite. Each suggested sentence reveals a distinct stack configuration, helping you identify recurring patterns and draft initial clauses.

### Coverage: refining and maintaining specifications
As you add patterns, the LRgrep compiler automatically performs **coverage** and **redundancy** checks:
- **Coverage** ensures that every reachable error configuration is matched by at least one clause. If a situation is missed, the compiler reports the exact stack pattern needed to cover it and provides a sample sentence to exercise it.
- **Redundancy** warns when a clause matches no new configurations because earlier clauses already handle them. This keeps the specification minimal and prevents conflicting or overlapping messages.

These checks enable a feedback-driven workflow: write a few high-level patterns, compile, address uncovered cases, and repeat until the specification is both exhaustive and irredundant. This naturally encourages starting with broad patterns and gradually specializing them as edge cases  emerge.

They also help to maintain a specification up to date when the grammar evolves.

By default, only a warning is emitted for incomplete coverage:
```
$ lrgrep compile -g parser.cmly -s errors.lrgrep
lrgrep: warning: rule error_message has only partial coverage (use --cover-report <file> to get more information)
```

`--cover-report report.md` produces a markdown report in the same format as the enumeration above.

---

## 5. The interpreter

While enumeration and coverage work *top-down* (starting from the grammar to guarantee exhaustive coverage with coarse-grained patterns), the interpreter works *bottom-up* (starting from a concrete invalid sentence to refine or debug a specific message).

The `interpret` command provides a lightweight debugger for your error specification. Given a grammar and an invalid input sentence, it simulates parsing, displays the failure point, reconstructs the parser's stack, and indicates which LRgrep patterns match it:

```bash
lrgrep -- interpret -g _build/default/examples/calc/parser.cmly
$ LPAREN INT PLUS
Parser stack (most recent first):
		[_* / expr: expr PLUS . expr]
- line 1:11-15	PLUS
- line 1:7-10	expr
- line 1:0-6	LPAREN
-              	main
Rejected lookahead symbols:
  DIV, EOL, PLUS, RPAREN, TIMES
$
```

Suggest that handling `[_* / expr: expr PLUS . expr]` would match our sample `LPAREN INT PLUS` sentence.

The interpreter is useful for customizing and refining error messages. It lets you:
- Observe how the parser interprets ambiguous or malformed input.
- Verify that patterns capture the intended stack suffixes and that filters/reductions behave as expected.
- Iterate on message wording by correlating stack structure with user-facing explanations.
- Debug specification regressions when the grammar evolves.

By combining the interpreter with static coverage checks, you can confidently evolve your specification as the grammar changes, ensuring that error messages remain accurate, informative, and maintainable over time.

---

*This guide provides a high-level introduction to the LRgrep toolchain. For detailed syntax rules, advanced pattern constructs, and migration notes, refer to the [LRgrep Reference](REFERENCE.md). For a complete worked example, see the [lrgrep-example repository](https://github.com/fpottier/lrgrep-example/).*
