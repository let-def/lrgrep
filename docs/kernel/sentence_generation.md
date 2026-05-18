# Sentence_generation

Generating concrete parse sentences from LR state sequences.

## Purpose

Converts abstract sequences of LR(1) states or transitions into concrete terminal sequences (sentences) that would produce those parse stacks. Used to generate counterexamples for coverage analysis and debug information.

## Architecture

### Three-step pipeline

```
LR states → Transitions → Cells → Terminals
```

### Step 1: States to transitions

```ocaml
val to_transitions : 'g grammar -> 'g lr1 index list -> 'g lr1 index * 'g transition index list
```

Converts a sequence of LR states into the transitions connecting them. For consecutive states `(x, y)`, finds the unique transition from `x` to `y` (either a goto or a shift).

### Step 2: Transitions to cells

```ocaml
val to_cells : 'g grammar -> (module (g, cell) Reachability.t_cell) -> 'g transition index list -> cell list
```

Maps transitions to reduction graph cells using dynamic programming. For each transition, considers all `(pre_class, post_class)` pairs and finds the minimum-cost path through the cost tree.

Algorithm:
1. Process transitions right-to-left
2. For each transition node, iterate all post_classes
3. For each post_class, find the best candidate from the suffix (minimal cost where candidate's class ⊆ current class)
4. For each pre_class, find the best `(pre, post)` pair minimizing `cost(cell) + cost(suffix)`
5. Keep only minimal-cost candidates

### Step 3: Cells to terminals

```ocaml
val expand_cells : 'g grammar -> (module (g, cell) Reachability.t_cell) -> cell list -> 'g terminal index list
```

Recursively expands cells back to terminal symbols. Handles two node types from the cost tree:

**Leaf node (`L tr`)**: A transition
- **Shift**: return the shift symbol (terminal)
- **Goto**: check if a nullable reduction applies (if `c_post ⊆ nullable_lookaheads` and `c_pre ∩ c_post ≠ ∅`, produce nothing). Otherwise, find the minimum-cost non-nullable reduction equation and recursively solve the sub-node.

**Inner node (`R (l, r)`)**: A concatenation
- Decompose into left and right sub-problems
- Iterate over all `(i_post_l, i_pre_r)` pairs via the coercion matrix
- Find the pair where `cost(left) + cost(right) = current_cost`
- Recursively solve right, then left (to produce terminals in correct order)
- Use `Break` exception to short-circuit once a minimal solution is found

### Public API

```ocaml
val sentence_of_transitions : 'g grammar -> (module g Reachability.t) -> 'g transition index list -> 'g terminal index list
val sentence_of_stack : 'g grammar -> (module g Reachability.t) -> 'g lr1 index list -> 'g terminal index list
```

## Key concepts

### Minimum-cost paths

The algorithm always seeks minimum-cost paths through the cost DAG. "Cost" is the number of input symbols needed to reach a configuration (computed by `Reachability.Analysis.cost`).

### Lookahead class constraints

At each step, the pre_class and post_class constrain which lookahead symbols can precede and follow the current position. The algorithm only explores paths consistent with these constraints.

### Nullable reductions

When a goto transition can be traversed via a nullable (ε) reduction, the algorithm prefers that path (produces zero terminals). This is checked first before exploring non-nullable alternatives.

### The Break exception

Used to short-circuit the exploration of inner nodes. Once a minimal-cost decomposition is found, there's no need to explore remaining candidates.

## Dependencies

- `Utils`, `Fix.Indexing`, `Info`, `Reachability`

## Depended on by

- `Coverage` (for counterexample generation)
