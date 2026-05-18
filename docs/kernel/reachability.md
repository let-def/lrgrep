# Reachability

Reachability analysis for LR automata after conflict resolution.

## Purpose

Computes which `(LR(1) state, lookahead)` configurations are actually reachable after conflicts have been resolved (some transitions removed). Also computes minimal parsing costs — the minimum number of input symbols needed to reach each configuration. This powers counterexample generation and LRC refinement.

## Architecture

### Functor interface

The module is a functor that produces a module conforming to signature `S`:

```ocaml
module type S = sig
  type g
  type reduction = { production; lookahead; steps; state }
  val unreduce : g goto_transition index -> reduction list
  module Classes : sig ... end
  module Coercion : sig ... end
  module Tree : sig ... end
  module Cell : sig ... end
  module Analysis : sig
    val cost : Cell.n index -> int
    val finite : Cell.n index -> bool
  end
end

val make : 'g grammar -> 'g t
and type 'g t = (module S with type g = 'g)
```

### Reduction inversion

```ocaml
type reduction = {
  production: g production index;
  lookahead: g terminal indexset;
  steps: g transition index list;
  state: g lr1 index;
}

val unreduce : g goto_transition index -> reduction list
```

For each goto transition, lists all reductions that result in following it. Reverses the effect of a reduction: given `s --A--> t`, finds all `(state, production, lookahead)` triples such that reducing `production` at `state` pops the RHS and follows goto `A` to reach `t`.

### Terminal classes

```ocaml
module Classes : sig
  val for_edge : g goto_transition index -> g terminal indexset array
  val for_lr1 : g lr1 index -> g terminal indexset array
  val pre_transition : g transition index -> g terminal indexset array
  val post_transition : g transition index -> g terminal indexset array
end
```

Partitions terminals by behavioral equivalence across transitions. Terminals in the same class have identical reachability properties. Computed via refinement-based fixpoint iteration over SCCs (Tarjan's algorithm).

### Coercion matrices

```ocaml
module Coercion : sig
  type pre = Pre_identity | Pre_singleton of int
  val pre : 'a indexset array -> 'a indexset array -> pre option
  type forward = int array array
  type backward = int array
  type infix = { forward : forward; backward : backward; }
  val infix : ?lookahead:'a indexset -> 'a indexset array -> 'a indexset array -> infix
end
```

Maps between different terminal partitionings. When one partition refines another, coercion matrices enable efficient translation between their compact representations.

### Cost tree (DAG)

```ocaml
module Tree : sig
  include CARDINAL
  val leaf : g transition index -> n index
  val split : n index -> (g transition index, n index * n index) either
  type equations = { nullable_lookaheads; nullable; non_nullable }
  val goto_equations : g goto_transition index -> equations
  val pre_classes : n index -> g terminal indexset array
  val post_classes : n index -> g terminal indexset array
end
```

Hash-consed DAG where:
- **Leaves**: individual transitions
- **Inner nodes**: matrix products `(left, right)` representing concatenated cost computations

The `goto_equations` function returns the cost equations for a goto transition, distinguishing nullable (ε) from non-nullable reductions.

### Compact cells

```ocaml
module Cell : sig
  include CARDINAL
  type row = int
  type column = int
  val encode : Tree.n index -> pre:row -> post:column -> n index
  val decode : n index -> Tree.n index * row * column
  type goto
  val goto_encode : g goto_transition index -> pre:row -> post:column -> goto index
  val goto_decode : goto index -> g goto_transition index * row * column
  val iter_goto : g goto_transition index -> (goto index -> unit) -> unit
end
```

Bit-packs `(tree_node, pre_class_index, post_class_index)` into a single integer for efficient storage in large cost matrices.

### Analysis (dataflow solver)

```ocaml
module Analysis : sig
  val cost : Cell.n index -> int     (* minimal symbols to reach this cell *)
  val finite : Cell.n index -> bool  (* is the cost finite? *)
end
```

Two analyses:
1. **Shortest path**: computes minimal costs via fixpoint iteration
2. **Finite language**: determines which cells are reachable at all

Uses `Reversedependencies` for efficient incremental updates during dataflow.

## Key concepts

### SCC-based processing

The grammar's goto transitions form a graph. Tarjan's SCC algorithm identifies strongly connected components. Processing in reverse topological order ensures that when an SCC is solved, all its dependencies are already resolved.

### Cost equations

For a goto transition `s --A--> t`, the cost to traverse it is:
- 0 if a nullable reduction applies (ε-reduction)
- Otherwise, the minimum over all non-nullable reductions of: `cost(reduction body) + cost(continuation)`

## Dependencies

- `Fix.Indexing`, `Utils`, `Misc`, `Info`

## Depended on by

- `LRC` (for lookahead classes)
- `Sentence_generation` (for cost-based cell expansion)
- `Coverage` (indirectly, via LRC)
