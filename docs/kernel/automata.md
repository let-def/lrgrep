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
(** Returns a closure: given a continuation, produces the corresponding NFA state.
    States are memoized by hash-consing on the continuation. *)

val from_branches : 'g grammar -> 'g Redgraph.graph -> ('g, 'r) branches -> ('g, 'r) NFA.t vector
(** Builds initial NFA states for all branches. *)
```

Builds the NFA by recursively deriving continuations. Uses `K.derive` to compute transitions, then partitions them by label equivalence (via `IndexRefine.annotated_partition`) to merge transitions with the same filter, captures, and usage.

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

Performs multi-pass fixpoint analysis on the DFA:

1. **Reachability**: which branches are reachable from accepting states
2. **Usage marking**: marks transitions reachable from accepting states
3. **Dead-code analysis**: warns on unreachable clauses and shadowing clauses
4. **Priority splits**: which kernel positions can distinguish clause precedence
5. **Priority chain construction**: builds [Order_chain] for dynamic ordering of continuations
6. **Accepted-before**: which branches have been accepted on paths to each state
7. **Liveness**: which captured variables are still needed at each state
8. **Definedness**: which variables have been defined at each state
9. **Variable classes**: partitions captures for register allocation
10. **Register allocation**: maps live variables to register slots (naive greedy by class)

Register allocation is done lazily based on live ranges. The naive greedy allocation assigns registers according to variable classes, leading to less efficient but more minimizable ("factorizable") code.

### Machine module — bytecode representation

```ocaml
type ('g, 'r, 'st, 'tr) t = {
  initial: 'st index option;
  source: ('tr, 'st index) vector;
  target: ('tr, 'st index) vector;
  label: ('tr, ('g, 'r) label) vector;
  unhandled: ('st, 'g lr1 indexset) vector;
  outgoing: ('st, 'tr indexset) vector;
  accepting: ('st, (('g, 'r) branch index * priority * Register.t Capture.map) list) vector;
  branches: ('st, (('g, 'r) branch index * bool * Register.t Capture.map) list) vector;
  register_count: int;
  partial_captures: Capture.set;
}

and ('g, 'r) label = {
  filter: 'g lr1 indexset;
  moves: Register.t Register.map;
  captures: (Capture.t * Register.t) list;
  clear: Register.set;
  priority: (('g, 'r) branch index * priority * priority) list;
}
```

The final representation is a sparse transition table with a register transfer language:
- **moves**: register-to-register transfers across transitions
- **captures**: store new captured values into registers
- **clear**: clear registers for captures that go out of scope or are undefined
- **priority**: dynamic priority remappings for clause precedence. Each element `(c, p1, p2)` means a match of clause `c` at priority `p1` in the source corresponds to priority `p2` in the target.

Minimization uses a refinement of Valmari's algorithm with custom decomposition by accepted actions and register transfer operations.

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
  (** Total number of stack positions. *)
  tops: 'n indexset;
  (** Set of stack top positions — viable positions where the stack can end. *)
  prev: 'n index -> 'n indexset;
  (** Predecessor positions in the LR automaton for a given stack position. *)
  label: 'n index -> 'g lr1 index;
  (** The LR(1) state associated with a stack position. *)
}
```

This allows the same DFA construction to work over plain LR(1) states or refined LRC states.

## Dependencies

- `Utils`, `Misc`, `Fix.Indexing`, `Lrgrep_support`, `Info`, `Spec`, `Regexp`

## Depended on by

- `Codegen` (consumes `Machine.t` for code generation)
- `Coverage` (uses `stacks` type and DFA for analysis)
