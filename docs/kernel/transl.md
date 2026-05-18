# Transl

Translation from user pattern syntax to regular expressions.

## Purpose

Converts the high-level pattern syntax written by users into `Regexp.Expr.t` regular expressions. Handles symbol resolution, filter compilation with globbing support, and reduction pattern compilation via the target trie.

## Architecture

### Indices module — precomputed mappings

```ocaml
type 'g t = {
  all_symbols: 'g symbol indexset;
  by_incoming_symbol: ('g symbol, 'g lr1 indexset) vector;
  prod_by_lhs: ('g nonterminal, 'g production indexset) vector;
  by_items: ('g item, 'g lr1 indexset) vector;
}
```

- **`by_incoming_symbol`**: For each symbol, which LR(1) states have an incoming transition on it. Used to compile atom patterns.
- **`prod_by_lhs`**: For each nonterminal, which productions have it as LHS. Used for filter compilation.
- **`by_items`**: For each LR(0) item, which LR(1) states recognize it. Used to resolve filter dot positions to state sets.

Computes the reflexive closure of nullable left-recursive nonterminals for correct item indexing.

### Globbing module — filter pattern matching

Parses filter patterns like `[foo _* bar]` into a normalized structure supporting substring matching with wildcards:

```ocaml
type 'g glob_exact = {
  dots: IntSet.t;                     (* dot positions in the pattern *)
  syms: 'g symbol indexset array;     (* symbol sets at each position *)
  length: int;
  skip: 'g glob_skip option;          (* nested skip for multi-component patterns *)
}
```

Key functions:
- `match_skip`: Tests if a production RHS matches a glob pattern
- `extract`: Given a matching RHS, returns the set of dot positions where the pattern matches

Uses sophisticated backtracking (`upmatch`/`downmatch`) for efficient substring search.

### Filter compilation

```ocaml
val transl_filter : 'g grammar -> Indices.t -> Syntax.position
  -> lhs:Syntax.symbol option -> rhs:Syntax.filter_component list
  -> 'g lr1 indexset
```

1. Resolve LHS to productions via `prod_by_lhs`
2. Parse RHS into a glob filter
3. For each production, extract matching dot positions
4. Union the LR(1) states that recognize those items via `by_items`

### Reduction expression compilation

```ocaml
let compile_reduce_expr (type g) (g : g grammar) rg trie re
  : 'g Redgraph.target indexset * 'g lr1 indexset
```

Walks the target trie, deriving the regex against it, collecting:
- `targets`: goto transitions where the reduction can complete
- `immediate`: LR(1) states where the reduction can happen immediately (ε-reductions)

### Main translation function

```ocaml
val transl : 'g grammar -> 'g Redgraph.graph -> Indices.t -> 'g Redgraph.target_trie
  -> capture:(Syntax.capture_kind -> string -> Capture.set)
  -> Syntax.regular_expression
  -> Capture.set * 'g Expr.t
```

Recursively translates syntax tree nodes:

| Syntax node | Translation |
|-------------|-------------|
| `Atom (capture, symbol, mark)` | `Set` (states with incoming `symbol`) + `Usage` marker |
| `Alternative` | `Alt` of translated alternatives |
| `Repetition` | `Star` with policy |
| `Reduce` | `Reduce` node compiled via `compile_reduce_expr`, wrapped with `Alt` for immediate matches |
| `Concat` | `Seq` of translated components |
| `Filter` | `Filter` of states from `transl_filter` |

Returns the set of all captures and the compiled expression.

## Key design decisions

- **Two modes**: `for_reduction` flag prevents captures and nested reductions inside reduction patterns.
- **Globbing**: Enables powerful stack-content matching without requiring users to enumerate all productions.
- **Immediate reductions**: When a reduction can happen immediately (ε-reduction), the translation adds an `Alt` with a `Filter` or empty `Seq` to handle the zero-length case.

## Dependencies

- `Utils`, `Misc`, `Syntax`, `Fix.Indexing`, `Regexp`, `Info`

## Depended on by

- `Spec` (used in `import_rule` to compile patterns)
