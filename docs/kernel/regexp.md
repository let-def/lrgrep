# Regexp

Regular expression definitions and derivation operations for LRGrep.

## Purpose

Defines the regular expression language used to describe error patterns over LR(1) state sequences. Provides Brzozowski-style derivative computation (`K.derive`) that drives NFA construction in the Automata module.

## Architecture

### Expr module — expression terms

```ocaml
type uid = private int

type 'g t = private {
  uid : uid;
  desc : 'g desc;
  position : Syntax.position;
}

and 'g desc =
  | Set of 'g lr1 indexset * Capture.set
  | Alt of 'g t list
  | Seq of 'g t list
  | Star of 'g t * Syntax.quantifier_kind
  | Filter of 'g lr1 indexset
  | Reduce of Capture.set * 'g Reductions.t
  | Usage of Usage.set
```

- **`Set`**: Match any LR(1) state in the set, optionally capturing it.
- **`Alt`**: Disjunction. `Alt []` = empty language.
- **`Seq`**: Concatenation. `Seq []` = {ε}.
- **`Star`**: Kleene star with quantifier policy (`*`, `+`, `?`, `{m,n}`).
- **`Filter`**: State guard — restrict matching to LR(1) states in the given set.
- **`Reduce`**: Reduction operator — match a viable reduction pattern.
- **`Usage`**: Dead-code tracking marker.

UIDs are assigned at parse time. By properties of Antimirov's derivatives, no new terms appear during derivation.

### Reductions module — reduction patterns

```ocaml
type 'g t = {
  pattern: 'g Redgraph.target indexset;  (* which reduction targets to match *)
  capture: Capture.set;                   (* captured variables *)
  policy: Syntax.quantifier_kind;         (* shortest vs longest match *)
}
```

### Label module — transition labels

```ocaml
type 'g t = {
  filter: 'g lr1 indexset;    (* which LR states this transition accepts *)
  captures: Capture.set;      (* variables captured on this transition *)
  usage: Usage.set;           (* usage tracking *)
}
```

### K module — continuations

Continuations represent the "rest to match" during derivative computation:

```ocaml
type 'g t =
  | Accept                    (* recognition complete *)
  | Done                      (* end of expression, derives to Accept *)
  | More of 'g Expr.t * 'g t  (* continue with sub-expression *)
  | Reducing of {             (* mid-reduction *)
      reduction: 'g Reductions.t;
      steps: 'g Redgraph.step indexset;
      next: 'g t;
    }
```

### The derive function

```ocaml
val K.derive : 'g grammar -> 'g Redgraph.graph -> 'g lr1 indexset -> 'g K.t -> ('g Label.t * 'g K.t) list
```

Given a continuation `k` and a set of LR(1) states, computes all possible transitions as `(label, next_continuation)` pairs. This is the core operation that drives automaton construction.

The `Reducing` case is the most complex: it tracks which reduction graph steps are currently active, what targets are being sought, and which continuation to use upon successful reduction.

### Shortest vs Longest policy

Controlled by the order in which the `Accept` continuation and the continuing
`Reducing` step are emitted during derivative computation:
- **Shortest**: `Accept` is emitted before continuing reductions (prefer smaller match)
- **Longest**: `Accept` is emitted after continuing reductions (prefer larger match)

The same ordering principle applies to `Star` derivatives: shortest emits the
"stop repeating" continuation first, longest emits it last.

## Key design decisions

- **UID-based comparison**: Expressions are compared by unique ID via `compare`, enabling ordering for deduplication in the NFA. UIDs are assigned at parse time and remain stable during derivation.
- **Antimirov derivatives**: Used for parallel derivatives (multiple continuations from one expression), adapted for LR states rather than character alphabets.
- **Usage tracking**: The `Usage` constructor marks which source constructs are exercised, enabling dead-code warnings.

## Dependencies

- `Fix.Indexing`, `Utils`, `Misc`, `Info`
- `Redgraph` (for `Reducing` continuations)

## Depended on by

- `Transl` (produces `Expr.t` values)
- `Spec` (stores compiled expressions per branch)
- `Automata` (consumes expressions to build NFAs)
