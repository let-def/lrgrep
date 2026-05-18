# Redgraph

Reduction graph construction and analysis.

## Purpose

Maps LR(1) stack suffixes to the viable reductions applicable to each configuration. The reduction graph is the engine that enables pattern matching on parser reductions — it answers the question "given a sequence of LR states on the stack, what reductions can fire and with what lookaheads?"

## Architecture

### Two-phase construction

**Phase 1: ε-reduction closure** (per LR(1) state, local analysis)

For each LR(1) state, computes the tree of possible ε-reductions (reductions that don't consume input). This forms a `stack_tree` ending with "pending" non-ε reductions that need stack states to proceed.

```ocaml
type 'g stack_tree = {
  next: ('g lr1 index list * 'g terminal indexset * 'g stack_tree) list;
  reductions: ('g nonterminal, 'g terminal indexset) indexmap list;
}

type 'g reduction_closure = {
  accepting: 'g terminal indexset;
  failing: 'g terminal indexset;
  stacks: 'g stack_tree;
  all_stacks: ('g lr1 index list * 'g terminal indexset) list;
  all_reductions: ('g nonterminal, 'g terminal indexset) indexmap list;
}
```

```ocaml
val close_lr1_reductions : 'g grammar -> ('g, 'g lr1) reduction_closures
```

**Phase 2: target indexing**

Builds a reverse index from reduction targets to graph nodes. The user specifies "I want to reduce an expression" — the target trie maps that pattern to the set of goto transitions where it can occur.

```ocaml
type 'g target_trie = private {
  mutable sub: ('g lr1, 'g target_trie) indexmap;
  mutable immediates: 'g lr1 indexset;
  mutable targets: ('g lr1, 'g target index) indexmap;
}

val index_targets : 'g grammar -> ('g, 'g lr1) reduction_closures
  -> 'g target_trie * ('g goto_transition, 'g targets) vector
```

**Phase 3: graph construction**

Builds the minimized reduction graph. Paths through the graph enumerate all stack suffixes consumable by reductions. Right recursion `A → α A` becomes a cycle.

```ocaml
val make : 'g grammar -> ('g, 'g lr1) reduction_closures
  -> ('g goto_transition, 'g targets) vector -> 'g graph
```

### Navigation

```ocaml
val initial : 'g graph -> 'g lr1 index -> 'g transition list
val follow  : 'g graph -> 'g step index -> 'g action

type 'g transition = {
  reached: 'g target indexset;
  reachable: 'g target indexset;
  step: 'g step index;
}

type 'g action =
  | Advance of 'g step index    (* pop, continue reducing *)
  | Switch of ('g lr1, 'g transition list) indexmap  (* goto, branch on top-of-stack state *)
```

### Lookahead filtering

```ocaml
val filter_reductions : 'g grammar -> 'g terminal indexset
  -> ('n, 'g terminal indexset) indexmap list
  -> ('n, 'g terminal indexset) indexmap list
```

Restricts reduction lookahead sets when the domain is narrowed (e.g., after conflict resolution).

## Key concepts

### Graph cells

Each cell represents a `(state, depth, lookahead)` configuration — a non-deterministic position where `state` must be `depth` positions deep in the stack, with lookahead in the given set.

### Advance vs Switch

- **Advance**: Move to the next step in a reduction sequence. Simulates a "pop" — doesn't depend on actual stack contents.
- **Switch**: Transition to different goto targets. Simulates a "goto" — depends on the LR(1) state at the top of the stack.

### Minimization

Valmari's algorithm minimizes the graph while preserving reachability structure needed for cost computation.

## Dependencies

- `Fix.Indexing`, `Utils`, `Misc`, `Info`

## Depended on by

- `Regexp` (for `Reducing` continuations)
- `Transl` (for `compile_reduce_expr`)
- `Spec` (passed to `import_rule`)
- `Coverage` (for reduction closures)
- `Sentence_generation` (for cell expansion)
