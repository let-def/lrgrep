# Using LRgrep

LRgrep performs tasks by executing commands on a compiled grammar and an optional error specification. All commands require a grammar file (`-g`/`--grammar`), and some accept an error specification (`-s`/`--spec`).

```bash
  -g <parser.cmly>   Grammar file (mandatory for all commands)
  -s <spec.lrgrep>   Error specification (optional, used by some commands)
```

Commands can be combined to run multiple analyses in a single invocation, amortizing the cost of loading and analyzing the grammar.

---

## `compile`

**Purpose**: Generate an OCaml module implementing an error specification.
**Required**: `-g`, `-s`.
**Options**:
- `-o <output.ml>`: Customize the output filename (default: `<spec>.ml`).
- `--cover-report <file>`: Generate a Markdown report detailing coverage gaps and suggestions.

Compiles the specification into a runtime pattern matcher. By default, incomplete coverage emits a warning during compilation. Use `--cover-report` to produce a structured report that can be used to incrementally complete the specification. Use `--cover-all` to return a non-zero exit code when coverage is not complete

```bash
lrgrep compile -g parser.cmly -s errors.lrgrep -o errors.ml --cover-report report.md
```

---

## `enumerate`

**Purpose**: Explore uncovered syntax errors by suggesting patterns or sample sentences.
**Required**: `-g`.
**Options**:
- `-e`: Suggest uncovered input sentences alongside stack patterns.
- `--format {plain,markdown}`: Output format (default: `markdown`).

Outputs a structured report of uncovered error configurations. Each entry includes a suggested LRgrep pattern and a concrete sample sentence that triggers it, serving as a design guide and baseline test suite for new specifications.

```bash
lrgrep enumerate -g parser.cmly -e
```

---

## `interpret`

**Purpose**: Diagnose invalid sentences by interactively analyzing parsing failures.
**Required**: `-g`.
**Options**:
- `-s`: Highlight which clauses in the specification match the error.

Launches an interactive REPL where you can paste invalid input sentences. For each input, it simulates parsing, displays the failure point, reconstructs the parser's stack, and shows which LRgrep patterns would match. This is ideal for bottom-up debugging and message refinement.

```bash
$ lrgrep interpret -g parser.cmly
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

---

*For advanced commands like error-resilient parsing (`recover`) or completion generation (`complete`), refer to the full CLI reference. The core workflow for authoring and maintaining error specifications revolves around `compile`, `enumerate`, and `interpret`.*
