# Info

Grammar information and index management. The foundational module that every other kernel module depends on.

## Purpose

Takes Menhir's raw grammar representation and builds a comprehensive, indexed representation of all grammar elements: terminals, non-terminals, productions, LR(0)/LR(1) states, items, transitions, and reductions. All access is O(1) via typed index vectors.

## Architecture

### The `grammar` type

The central opaque type `'g grammar` carries all derived information. The `'g` type parameter provides type-level cardinality safety: indices for one grammar cannot be mixed with another.

### The `Load_grammar` functor

Entry point. Takes a Menhir `GRAMMAR` module and produces a `grammar` value with all index structures computed.

```ocaml
module Load_grammar(G : MenhirSdk.Cmly_api.GRAMMAR) : sig
  type g
  val grammar : g grammar
end
```

### Submodules

Each grammar element category has its own submodule following the `INDEXED` signature:

| Submodule | Element | Key functions |
|-----------|---------|---------------|
| `Terminal` | `'g terminal` | `to_string`, `alias`, `semantic_value`, `is_error`, `find`, `intersect`, `lookaheads_to_string` |
| `Nonterminal` | `'g nonterminal` | `to_string`, `nullable`, `first`, `kind`, `find`, `find_mangled` |
| `Symbol` | `'g symbol` (= terminal ⊎ nonterminal) | `desc`, `is_terminal`, `is_nonterminal`, `inj_t`, `inj_n`, `to_string`, `find` |
| `Production` | `'g production` | `lhs`, `rhs`, `length`, `kind` |
| `Item` | `'g item` (= production + dot position) | `make`, `last`, `desc`, `prev`, `position`, `production`, `is_reducible`, `to_string` |
| `Lr0` | `'g lr0` | `incoming`, `items`, `is_entrypoint` |
| `Lr1` | `'g lr1` | `all`, `accepting`, `wait`, `to_lr0`, `incoming`, `items`, `shift_on`, `reduce_on`, `reject`, `predecessors`, `is_entrypoint`, `entrypoints`, `entrypoint_table`, `default_reduction`, `intersect`, `to_string` |
| `Transition` | `'g transition` (= goto ⊎ shift) | `goto`, `shift`, `any`, `of_goto`, `of_shift`, `split`, `source`, `target`, `symbol`, `goto_symbol`, `shift_symbol`, `successors`, `predecessors`, `find_goto`, `find_goto_target`, `accepting`, `find`, `to_string` |
| `Reduction` | `'g reduction` (= lr1, production, lookaheads) | `state`, `production`, `lookaheads`, `from_lr1` |

### Key design decisions

- **Type-level cardinalities**: Every index type is parameterized by `'g`. The `Fix.Indexing` module provides `index`, `indexset`, `indexmap`, and `vector` types that enforce compile-time safety.
- **Vector-based storage**: All data is stored in vectors for O(1) random access. The `vector` type from `Fix.Indexing` is used throughout.
- **Lazy predecessors**: `Lr1.predecessors` returns a `lazy_stream` to avoid materializing all predecessor sets upfront.
- **Fuzzy find**: The `find` functions across Terminal, Nonterminal, and Symbol accept `?approx:int` and return disambiguation suggestions on failure.

## Key concepts

### Wait states

LR(1) states where the parser must look at more input. These are initial states and targets of shift transitions, excluding accepting states.

### Accepting states

LR(1) states reached after recognizing a complete entrypoint. The only valid action is to reduce.

### Items

An LR(0) item is a production with a dot position: `A → α . β`. Items are indexed globally, not per-state, for efficient set operations.

### Transition types

- **Goto transitions**: triggered by non-terminals (after a reduction)
- **Shift transitions**: triggered by terminals (consuming input)
- **Accepting transitions**: goto transitions from initial to accepting states

## Dependencies

- `Utils`, `Misc`, `Fix.Indexing` (infrastructure)
- `MenhirSdk.Cmly_api.GRAMMAR` (source grammar)

## Depended on by

Every other kernel module.
