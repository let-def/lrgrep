# LRC

LR with Classes — refinement of LR(1) states by reachable lookahead classes.

## Purpose

Refines LR(1) states by partitioning them based on which lookahead classes can actually reach them. After conflict resolution, some sequences of LR states on the stack may be unreachable even though individual transitions are. LRC catches these unreachable sequences by tracking `(LR1 state, lookahead class)` pairs.

## Architecture

### LRC states

```ocaml
type 'g n  (* LRC state index *)

type ('g, 'n) t = {
  lr1_of: ('n, 'g lr1 index) vector;           (* LRC -> underlying LR1 *)
  lrcs_of: ('g lr1, 'n indexset) vector;        (* LR1 -> set of LRC states *)
  all_wait: 'n indexset;                         (* wait states *)
  all_leaf: 'n indexset;                         (* leaf states *)
  all_successors: ('n, 'n indexset) vector;      (* transition relation *)
  reachable_from: ('n, 'n indexset) vector;      (* reachability relation *)
}
```

Each LRC state corresponds to an `(LR1 state, lookahead class)` pair that is actually reachable. One LR1 state may split into multiple LRC states if different lookahead classes reach it via different paths.

### Construction

```ocaml
val make : 'g grammar -> 'g Reachability.t -> ('g, 'g n) t
```

Builds the LRC structure from the grammar and reachability analysis. Iterates over all LR1 states and their lookahead classes (from `Reachability.Classes.for_lr1`), creating LRC states for reachable combinations.

### Minimal LRC

```ocaml
type 'g mlrc
val make_minimal : 'g grammar -> 'g Reachability.t -> ('g, 'g mlrc) t
```

Quotients equivalent LRC states using Valmari's DFA minimization algorithm. States that have identical transition behavior (same successors for each lookahead class) are merged, significantly reducing memory usage.

### Entrypoint reachability

```ocaml
type 'n entrypoints = {
  reachable: 'n indexset;
  wait: 'n indexset;
  entrypoints: 'n indexset;
  successors: ('n, 'n indexset) vector;
  predecessors: ('n, 'n indexset) vector;
  some_prefix: 'n index -> int * 'n index list;
}

val from_entrypoints : 'g grammar -> ('g, 'n) t -> 'n indexset -> 'n entrypoints
```

Computes the subset of LRC states reachable from specific entrypoints. The `some_prefix` function returns a minimal-length path from an entrypoint to any given state (returned in reverse order, excluding the target).

### Debugging

```ocaml
val check_deterministic : 'g grammar -> 'g Reachability.t -> unit
val check_equivalence : ('g, 'g mlrc) t -> ('g, 'g mlrc) t -> bool
```

## Key concepts

### Why LRC matters

Consider a pruned automaton where `s0 --a--> s1` and `s1 --b--> s2` are both individually reachable, but no single lookahead can traverse both. LRC splits `s1` into `s1_a` (reachable via `a` from `s0`) and `s1_other` (reachable other ways), revealing that `s1_a --b--> s2` is actually unreachable.

### Prefix computation

`some_prefix` computes minimal-length paths for counterexample generation. Current limitation: minimizes number of symbols, not terminals. A short symbol path may expand to a long terminal string.

### Practical impact

The LRC refinement rarely diverges from LR(1) in practice. Most of the time, minimized LRC states coincide with LR(1) states. It mainly matters for "severely pruned" automata with aggressive conflict resolution.

## Dependencies

- `Fix.Indexing`, `Utils`, `Misc`, `Info`, `Reachability`

## Depended on by

- `Automata` (for stack definitions)
- `Coverage` (for AND-OR graph construction)
