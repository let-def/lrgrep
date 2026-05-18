# Spec

Specification structures for user error rules.

## Purpose

Represents the user's error specification after parsing and resolution. Organizes clauses (groups of patterns sharing an action) and branches (individual pattern-action pairs), with priority chains, lookahead constraints, and compiled regular expressions.

## Architecture

### Clause and Branch types

```ocaml
type ('g, 'r) clause   (* a group of patterns sharing an action *)
type ('g, 'r) branch   (* a single pattern within a clause *)
```

The `'g` parameter is the grammar, `'r` is the rule-level index.

### Clause metadata

```ocaml
type clause_def = {
  new_group: bool;              (* starts a new priority group? *)
  shortest: bool;               (* in a %shortest group? *)
  syntax: Syntax.clause;        (* original syntax node *)
}

type ('g, 'r) clauses = {
  definitions : (('g, 'r) clause, clause_def) vector;
  captures : (('g, 'r) clause, (Capture.n, Syntax.capture_kind * string) indexmap) vector;
}
```

### Branch data

```ocaml
type ('g, 'r) branches = {
  clause: (('g, 'r) branch, ('g, 'r) clause index) vector;
  pattern: (('g, 'r) branch, Syntax.pattern) vector;
  expr: (('g, 'r) branch, 'g Expr.t) vector;
  of_clause : (('g, 'r) clause, ('g, 'r) branch indexset) vector;
  lookaheads : (('g, 'r) branch, 'g terminal indexset option) vector;
  br_captures : (('g, 'r) branch, Capture.n indexset) vector;
  is_total: ('g, 'r) branch Boolvector.t;
  is_partial: ('g, 'r) branch Boolvector.t;
  priority: (('g, 'r) branch, ('g, 'r) branch opt index) vector;
}
```

### Rule wrapper

```ocaml
type 'g _rule = Rule : ('g, 'r) clauses * ('g, 'r) branches -> 'g _rule
```

### Compilation

```ocaml
val import_rule : 'g grammar -> 'g Redgraph.graph -> 'g Transl.Indices.t
  -> 'g Redgraph.target_trie -> Syntax.rule -> 'g _rule
```

Transforms parsed syntax into internal structures:
1. Iterates clauses, classifying them as total/partial
2. For each clause, iterates branches
3. Compiles each branch pattern via `Transl.transl`
4. Computes priority relationships between branches

### Priority system

The `priority` vector maps each branch to the branch that should take precedence when both match:
- Normal clauses: text order (first listed = highest priority)
- `%shortest` groups: all clauses share priority; the shortest match wins

### Total vs Partial

- **Total** (`is_total`): Accepts any lookahead — the action is unconditional
- **Partial** (`is_partial`): Only accepts specific lookaheads (stored in `lookaheads` field)
- Partial clauses enable conditional error handling based on the next token

## Key design decisions

- **Dual representation**: Each branch stores both the original `Syntax.pattern` (for diagnostics) and the compiled `Expr.t` (for automaton construction).
- **Priority chain**: The `priority` vector encodes a linked list of fallback branches, avoiding combinatorial state explosion in the automaton.
- **Clause-grouping**: The `of_clause` mapping enables codegen to group branches that share the same action.

## Dependencies

- `Utils`, `Misc`, `Fix.Indexing`, `Regexp`, `Info`, `Transl`, `Redgraph`, `Syntax`

## Depended on by

- `Automata` (consumes `branches.expr`, `branches.priority`)
- `Codegen` (consumes `clauses`, `branches` for code generation)
