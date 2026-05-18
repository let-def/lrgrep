# Codegen

Code generation for the LR matching machine.

## Purpose

Generates OCaml source code from the compiled automaton (`Automata.Machine.t`) and specification (`Spec.clauses`, `Spec.branches`). Produces a sparse transition table, bytecode program, and wrapper functions that integrate with Menhir's interpreter infrastructure.

## Architecture

### Output structure

Generated code has three parts:

1. **Bytecode and transition table** (`output_table`)
2. **Semantic action dispatcher** (`output_execute_function`)
3. **Wrapper function** (`output_wrapper`)

### Bytecode and table

```ocaml
let output_table (type g r) out rule (machine : (g, r, _, _) Automata.Machine.t)
    (program, table, remap)
```

Calls `Lrgrep_support.compact` to compress the sparse transition table, then outputs:
- `lrgrep_program_<name>`: a `Lrgrep_runtime.program` record with register count, initial state, table, and bytecode string
- The table is packed via `Lrgrep_support_packer.encode` for compact representation

### State compaction

```ocaml
let get_state_for_compaction index = {
  Lrgrep_support.
  accept = List.map add_match machine.accepting.:(index);
  halting = machine.unhandled.:(index);
  transitions = IndexSet.fold add_transition machine.outgoing.:(index) [];
}
```

For each machine state, extracts:
- Accepting configurations: `(clause, priority, registers)` tuples
- Halting flag: whether unhandled input causes failure
- Transitions: `(filter, actions)` pairs with moves, stores, clears, and targets

### Semantic action dispatcher

```ocaml
let output_execute_function out
```

Generates `lrgrep_execute_<name>` — a pattern-matching function over `(clause, token)` pairs. For each clause:
1. Matches on branch index and lookahead constraint
2. Binds captured variables from registers
3. Executes the user's semantic action
4. Returns the result (wrapped in `Some` for total, bare for partial)

### Capture binding

```ocaml
let bind_capture out ~offset index (def, name, refs)
```

Handles three capture types:

| Type | Generated variable | Source |
|------|-------------------|--------|
| `Value` | `name`, `_startpos_name_`, `_endpos_name_`, `_positions_name_` | Register value, with type recovery |
| `Start_loc` | `_startpos_name_` | Start position from register |
| `End_loc` | `_endpos_name_` | End position from register |

Type recovery: traces which LR(1) states contributed to a capture, looks up their incoming symbols, and unifies their semantic value types.

### Lookahead constraints

For partial branches with lookahead restrictions, generates pattern matching on the token:

```ocaml
| branch_idx, TerminalName _ ->  (* action *)
| (branch_idx), _ -> None        (* fallback for constrained branches *)
```

### Wrapper function

```ocaml
let output_wrapper out {Syntax.name; args; _}
```

Generates the public API function that:
1. Runs the bytecode interpreter (`lrgrep_run`)
2. For each resulting clause, tries the execute function
3. Returns the first successful match

### Module functor support

For parameterized grammars, wraps generated code in a `Make` functor that takes the lexer module as a parameter.

## Key design decisions

- **Two-phase generation**: First compacts the state machine and emits bytecode, then generates semantic actions with proper variable binding.
- **Lazy variable introduction**: Capture variables are introduced in the generated code regardless of whether the action uses them (FIXME noted in code).
- **Position tracking**: The `refs` triple `(rstart, rend, rpos)` tracks which position keywords (`$startpos`, `$endpos`, `$positions`) are used in the action, enabling conditional code generation.

## Dependencies

- `Fix.Indexing`, `Utils`, `Misc`, `Info`, `Spec`, `Automata`, `Syntax`, `Code_printer`, `Lrgrep_support`, `Lrgrep_support_packer`

## Depended on by

- Top-level driver (invoked to produce final output)
