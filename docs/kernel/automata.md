# Automata

DFA construction and analysis for LR error pattern matching.

## Purpose

Converts user error patterns (compiled as regular expressions) into a deterministic finite automaton that can be efficiently executed at runtime. The DFA matches against sequences of LR(1) states on the parser stack, respecting clause priorities and managing captured variables.

## Architecture

### NFA module — from regex to NFA

```ocaml
type ('g, 'r) t = {
  uid: int;
  k: 'g K.t;                                (* continuation *)
  transitions: ('g Label.t * ('g, 'r) t lazy_t) list;
  branch: ('g, 'r) branch index;
  mutable mark: unit ref;
}

val make : 'g grammar -> 'g Redgraph.graph -> ('g, 'r) branch index -> 'g K.t -> ('g, 'r) t
val from_branches : 'g grammar -> 'g Redgraph.graph -> ('g, 'r) branches -> ('g, 'r) NFA.t vector
```

Builds the NFA by recursively deriving continuations. Uses `K.derive` to compute transitions, then partitions them by label equivalence (via `IndexRefine.annotated_partition`) to merge transitions with the same filter/captures/usage.

Transitions are lazy — NFA states are only materialized when explored during determinization.

### DFA module — power set construction

```ocaml
type ('g, 'r, 'dfa, 'n) state = {
  index: 'dfa index;
  branches: ('n, ('g, 'r) branch index) vector;
  accepting: 'n Boolvector.t;
  mutable transitions : ('g, 'r, 'dfa, 'n) transition list;
}

and ('g, 'r, 'dfa, 'src) transition = Transition : {
    label: 'g lr1 indexset;
    target: ('g, 'r, 'dfa, 'tgt) state;
    mapping: ('src, 'tgt) mapping;
  } -> ('g, 'r, 'dfa, 'src) transition

type ('g, 'r, 'dfa) t = {
  initial: 'dfa index;
  states: ('dfa, ('g, 'r, 'dfa) packed) vector;
  domain: ('dfa, 'g lr1 indexset) vector;
  kernels: ('dfa, ('g, 'r) NFA.t array) vector;
}

val determinize : 'g grammar -> ('g, 'r) branches -> ('g, 's) stacks -> 's index -> ('g, 'r) _t
```

Modified power set construction with three key differences from standard subset construction:

1. **Ordered kernels**: NFA states in each kernel are ordered by priority. This is a "power sequence" construction, not a power set — order matters for priority resolution.

2. **Reachable-stack pruning**: Only paths corresponding to reachable LR stacks are determinized. Transitions to unreachable configurations are omitted (automata implication).

3. **Priority-based implicit pruning**: Branches that can never fire due to lower priority are pruned during construction, avoiding combinatorial state explosion.

Hash-consing ensures canonical representation of equivalent DFA states.

### Dataflow module — liveness and register allocation

Performs fixpoint analysis on the DFA to compute:
- **Liveness**: which captured variables are still needed at each state
- **Definedness**: which variables have been defined at each state
- **Register allocation**: maps live variables to register slots

Uses a refinement of Valmari's algorithm for DFA minimization after register allocation, producing a more compact machine.

### Machine module — bytecode representation

```ocaml
type ('g, 'r, _, _) t = {
  states: int cardinal;
  initial: 'dfa index option;
  accepting: ('dfa index, (clause * priority * register array) list) vector;
  unhandled: ('dfa index, bool) vector;
  outgoing: ('dfa index, transition indexset) vector;
  target: (transition index, 'dfa index) vector;
  label: (transition index, label) vector;
  register_count: int;
  partial_captures: Capture.set;
}

and label = {
  filter: 'g lr1 indexset;
  moves: (Capture.t, register index) indexmap;
  captures: (Capture.t, register index) list;
  clear: Capture.set;
  priority: priority;
}
```

The final representation is a sparse transition table with a register transfer language:
- **moves**: transfer register values across transitions
- **captures**: store new captured values into registers
- **clear**: clear registers that go out of scope
- **priority**: dynamic priority for resolving clause precedence

## Key concepts

### Priority chain

Instead of statically duplicating states for each priority ordering, the Machine uses a dynamic priority chain. Each accepting state stores a list of `(clause, priority, registers)` tuples. At runtime, the first matching clause wins.

### Register transfer language

Captured values flow through the automaton via registers. The dataflow analysis computes live ranges and allocates registers lazily. The naive greedy allocation produces less efficient but more minimizable code.

### Stack abstraction

The `stacks` type parameterizes the DFA construction with the actual stack topology:

```ocaml
type ('g, 'n) stacks = {
  domain: 'n cardinal;
  tops: 'n indexset;
  prev: 'n index -> 'n indexset;
  label: 'n index -> 'g lr1 index;
}
```

This allows the same DFA construction to work over plain LR(1) states or refined LRC states.

## Dependencies

- `Utils`, `Misc`, `Fix.Indexing`, `Lrgrep_support`, `Info`, `Spec`, `Regexp`

## Depended on by

- `Codegen` (consumes `Machine.t` for code generation)
- `Coverage` (uses `stacks` type and DFA for analysis)
