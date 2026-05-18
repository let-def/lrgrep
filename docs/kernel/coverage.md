# Coverage

Coverability analysis of the LR automaton and matching machine.

## Purpose

Determines whether the user's error matching machine covers all possible failing configurations of the LR automaton. Produces counterexamples for uncovered failures — concrete input sequences that would trigger a parse error but are not handled by any user clause.

## Architecture

### Andor module — non-deterministic AND-OR graph

```ocaml
type ('g, 'lrc, 'n) node = {
  lrc: 'lrc index;                    (* current LRC state *)
  rpos: 'g Redpos.t Opt.n index;      (* reduction position (None = between reductions) *)
  active: 'g terminal indexset;       (* lookahead symbols still being processed *)
  mutable successors: ('g terminal indexset * 'n index) array;
}

type ('g, 'lrc) graph = Graph : ('g, 'lrc, 'n) _graph -> ('g, 'lrc) graph

val make : 'g grammar -> ('g lr1, 'g Redgraph.reduction_closure) vector
  -> ('g, 'lrc) Automata.stacks -> 'g Redpos.table -> ('g, 'lrc) graph
```

Builds a non-deterministic graph where:
- **OR nodes** (`rpos = None` or `rpos` at start of reduction): multiple possible reductions can fire
- **AND nodes** (`rpos` in middle of reduction): deterministic consumption of LR states (popping the stack)

Each node tracks a single reduction with precise LR state and lookahead set. Built via fixpoint iteration over the reduction closures.

### Deter module — deterministic automaton

```ocaml
type ('g, 'n, 'm) node = {
  index: 'm index;
  ker: 'n indexset;                    (* kernel of Andor nodes *)
  mutable top: 'g lr1 indexset;
  mutable accept: 'g terminal indexset;
  mutable successors: ('g terminal indexset * ('g, 'n, 'm) node) array;
}

type ('g, 'lrc, 'n) graph = Graph : ('g, 'lrc, 'n, 'm) _graph -> ('g, 'lrc, 'n) graph
```

Determinizes the AND-OR graph by merging all OR nodes into a single state and grouping AND nodes that branch on the same LR state. Edges to OR nodes become ε-transitions; edges to AND nodes are labeled by LR(1) states.

Key design choice: lookaheads are NOT part of the DFA kernel. Different Andor nodes in the same kernel may accept, reject, or defer different lookaheads. Including lookaheads in the kernel would cause combinatorial explosion without actionable benefit.

### Enum module — unaccepted lookahead enumeration

Augments the deterministic graph by tracking which lookahead symbols have NOT yet been accepted at each node. Sink nodes (no successors) with non-empty unaccepted sets describe all possible uncovered failures. Witness paths to these sinks, reconstructed via predecessor tracking, serve as evidence for counterexample generation.

Works with GLR automata: since all applicable reductions are tracked simultaneously, a lookahead is "unaccepted" only if none of the possible reductions accept it.

### Cover module — synchronized product

Computes the coverage of the user's error matching machine by constructing a synchronized product of the machine (from `Automata`) with the enumeration graph. States where the enumeration graph has unaccepted lookaheads but the machine has no matching transition are coverage gaps.

### Extract module — counterexample extraction

Extracts paths from uncovered sink nodes back toward initial states by propagating rejectable lookaheads backward through the enumeration graph. Produces maximal and global prefix lists that callers can feed to `Sentence_generation` for concrete terminal sequences.

### Report module — user-facing output

Formats enumeration and coverage results for display to users.

## Key design decisions

### Three-level abstraction

| Level | What it tracks | What it ignores |
|-------|---------------|-----------------|
| Andor | Single reduction, precise state, precise lookaheads | Other reductions |
| Deter | All reductions at once, precise states | Lookahead sets |
| Enum | All reductions, precise states, unaccepted lookaheads | Accepted lookaheads |

This layered approach balances efficiency, precision, and actionability. Tracking everything at once would be prohibitively expensive; relevant information can be recovered at each level.

### Path-dependent lookaheads

Since lookaheads are not in the Deter kernel, the Enum module's lookahead tracking is path-dependent. What matters is that there exists at least one path to a node where a given lookahead is unaccepted.

## Dependencies

- `Utils`, `Misc`, `Fix.Indexing`, `Info`, `Redgraph`, `Redpos`, `Automata`, `LRC`, `Reachability`

## Depended on by

- Top-level driver (for coverage reporting)
- `Sentence_generation` (consumes paths for counterexample generation)
