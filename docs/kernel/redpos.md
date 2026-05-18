# Redpos

Compact representation of reduction positions.

## Purpose

Encodes positions within reductions as compact integers. A position is a pair `(n, A)` meaning "pop n elements from the stack, then follow the goto transition labeled A". This compact encoding is used throughout the reduction graph and coverage analysis for efficient storage and lookup.

## Architecture

### The `table` type

```ocaml
type 'g table = {
  desc: ('g t, 'g desc) vector;  (* index -> (nonterminal, offset) *)
  zero: ('g nonterminal, 'g t index) vector;  (* nonterminal -> position 0 *)
}
```

Positions for each nonterminal are allocated contiguously in the `desc` vector. The `zero` vector provides O(1) lookup of position 0 for each nonterminal.

### Construction

```ocaml
val make : 'g grammar -> 'g table
```

Iterates all productions, computes the maximum RHS length per nonterminal, then allocates contiguous slots. Total cardinality = 1 (for a sentinel) + sum of max production lengths across all nonterminals.

### Operations

```ocaml
val inj    : 'g table -> 'g nonterminal index -> int -> 'g t index  (* (nt, offset) -> index; asserts on out-of-range *)
val prj    : 'g table -> 'g t index -> 'g nonterminal index * int   (* index -> (nt, offset) *)
val previous : 'g table -> 'g t index -> 'g nonterminal index Either 'g t index
val previous' : 'g table -> 'g t index Opt.n -> 'g nonterminal index Either 'g t index Opt.n
val is_zero : 'g table -> 'g t index -> bool
```

### `previous` semantics

Returns:
- `Left nt` if the position is 0 for nonterminal `nt` (start of a reduction, need to follow goto `nt`)
- `Right pos'` if the position is > 0 (middle of a reduction, `pos'` is the previous position)

This distinguishes "pop and follow goto" from "pop and continue reducing" in the reduction graph traversal.

## Key design decisions

- **Contiguous allocation**: Positions for each nonterminal are consecutive, enabling simple index arithmetic.
- **Predecessor-based navigation**: `previous` uses `Index.pred` (O(1)) rather than recomputing from `(nt, offset-1)`.
- **Optional support**: `previous'` handles `Opt.n` indices for use in graphs where positions may be absent.

## Dependencies

- `Utils`, `Misc`, `Fix.Indexing`, `Info`

## Depended on by

- `Redgraph` (for tracking reduction progress)
- `Coverage` (for AND-OR graph nodes)
- `Sentence_generation` (for cell expansion)
