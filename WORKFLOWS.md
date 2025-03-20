# Using LRgrep

LRgrep performs tasks by executing commands on a grammar and specification. Flags can be combined to run multiple analyses in a single invocation, amortizing the cost of loading the grammar.

All commands require a grammar (`-g`/`--grammar`), and some accept an error specification (`-s`/`--spec`):

```
  -g/--grammar <parser.cmly>   Grammar file (mandatory for all commands)
  -s/--spec <spec.mlyl>        Error specification (optional, used by some commands)
```

> Global flags:
> 
> ```
>   -g/--grammar <parser.cmly>  Parser/grammar to work with
>   -s/--spec <spec.mlyl>  Error specification to process
> ```
> 
> LRgrep can achieve many different tasks related to grammars and error specifications. All of them need at least a grammar as input, so it is mandatory to pass the `-g <parser.cmly>`.
> Some of them also depend on an error specification, specified with the `-s <spec.mlyl>` flag.
> 
> The other arguments determine transformations and analysis that are applied to the grammar and the (optional) specification. They are better understood but looking at the different workflows in which LRgrep can be used.
> 
> The different flags can be applied simultaneously to a single invocation. This is done to amortize the cost of the grammar and specification analysis over multiple tasks.

---

## `compile`

**Purpose**: Generate an OCaml module implementing an error specification.  
**Required**: `-g`, `-s`.  
**Options**:  
- `-o <output.ml>`: Customize the output filename (default: `<spec>.ml`).  

Produces a parser module from the specification file.

> ## Compile
> 
> The main purpose of LRgrep is to generate a program realizing an error specification. This is done by the `compile` command which generate an OCaml module `<spec>.ml` from the mandatory `-s <spec.mlyl>`. `-o <output.ml>` can be used to customize the name of the generated file.

---

## `interpret`

**Purpose**: Diagnose invalid sentences by analyzing parsing failures.  
**Required**: `-g`.  
**Options**:  
- `-i <sentence>`: Specify input sentences directly.  
- `-i -`: Read input from stdin.  
- `-s`: Highlight clauses in the specification that match the error.  

Outputs a backtrace of the parser's state during failure, with annotations from the grammar (and specification if provided).

---

## `cover`

**Purpose**: Explore coverage of syntax errors by suggesting patterns or sample sentences.  
**Required**: `-g`.  
**Options**:  
- `-o <output>`: File to store the coverage report (default to the standard output, written `-`).  
- `-p`: Suggest uncovered grammar patterns.  
- `-e`: Suggest uncovered input sentences.  
- `-s`: Restrict suggestions to parts *not* covered by the specification.  
- `--format {plain,json}`: Output format (default: `plain`).  
- `--dead`: Focus on clauses that should be unrecheable.  

**Behavior**: Outputs uncovered elements based on specified criteria.

---

## `recover`

**Purpose**: Generate an error-resilient parser.  
**Required**: `-g`.  
**Options**:  
- `-o <output.ml>`: Customize the output filename (default: `<parser>_recover.ml`).  

Produces a parser that recovers from syntax errors. Ignores specifications (`-s`).

---

## `complete`

**Purpose**: Generate a parser for enumerating syntactic completions.  
**Required**: `-g`.  
**Options**:  
- `-o <output.ml>`: Customize the output filename (default: `<parser>_complete.ml`).  

Creates an OCaml module that suggests valid completions for partial inputs. Ignores specifications (`-s`).
