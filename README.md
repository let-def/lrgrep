# Syntax error analyser 

This repository provides different tools to work on the error messages of a menhir-generated parser.

The main tool is `lrgrep`.

It takes:

- a compiled Menhir grammar (a .cmly file, produced by passing `--cmly` flag to Menhir) 
- a list of rules (usually a .mlyl file).

If the list of rule is well-formed, it produces an OCaml module that can match the rules against the state of a parser at runtime.

By carefully crafting the rules, one can provide fine-grained message to explain syntax errors.



The repository is is structured as follow:

- the main tool, lrgrep, can be found in [src/main.ml]()
- [runtime]() implements the `lrgrep.runtime` library used by generated analysers
- in [ocaml](), we try to apply this methodology to OCaml grammar:
  - [parser_raw.mly](ocaml/parser_raw.mly) and [lexer_raw.mll](ocaml/lexer_raw.mll) define an OCaml 4.13 compatible grammar with syntax error reporting removed
  - [parse_errors.mlyl](ocaml/parse_errors.mlyl) define the error rules for this grammar
  -  the [frontend](ocaml/frontend.ml) binary is an alternative parser that can be used with ocamlc/ocamlopt 4.13 (using the `-pp <path-to-frontend.exe>` flag)
  - the [interpreter](ocaml/interpreter.ml) binary is a tool that prints detailed information on the parsing process, useful to craft good error rules 
- [lib]() implements various algorithms used by other tools



## Working on OCaml grammar

For now, the main focus is on the [ocaml]() sub-directory, and [ocaml/parse_errors.mlyl]() specifically.
My current workflow is as follow:

- starts from an example, an OCaml code with a syntax error for which the message is quite bad
- by reading the [grammar](ocaml/parser_raw.mly) and the output of the interpreter, get an idea of what the parsing situation looks like around the error point
- craft an error rule, and debug it using by passing `-pp frontend` to  `ocamlc` 

### Setting up the tools

All the work is done using OCaml 4.13. Make sure you are using the right switch:

```bash
$ ocamlc -version
4.13.1
```

Clone the repository and install dependencies:

```bash
$ git clone https://github.com/let-def/lrgrep.git
$ cd lrgrep
$ opam install menhir fix
```

At this point, `make` should succeed (contact me if not) and produce the three binaries: `lrgrep.exe`, `frontend.bc` and `interpreter.exe`.

It is usually better to test with the bytecode frontend as it leads to shorter iteration cycles.

#### Quick test

Try the new frontend with some simple examples:

```
$ ocamlc -c -pp _build/default/ocaml/frontend.bc test_ok.ml
```
This first example compiled successfully.

```
$ ocamlc -c -pp _build/default/ocaml/frontend.bc test_ko.ml
ocamlc -pp _build/default/ocaml/frontend.bc test_ko.ml
File "test_ko.ml", line 4, characters 0-3:
4 | let z = 7
    ^^^
Error: Spurious semi-colon at 2:9

File "test_ko.ml", line 1:
Error: Error while running external preprocessor
Command line: _build/default/ocaml/frontend.bc 'test_ko.ml' > /tmp/ocamlppbbc3f9
```

In this one however, there is a syntax error. Luckily, this case is covered by a rule: while the error happens on line 4, it is likely caused by the semi-colon at the end of line 2.

#### Using the frontend for compiling ocaml files

By using the `OCAMLPARAM` environment variable, we can instruct all execution of ocaml compilers in the current shell to use our frontend.

```shell
$ ./setup_shell.sh
export 'OCAMLPARAM=pp=$PWD/lrgrep/_build/default/ocaml/frontend.bc,_'
# setup_shell commands produces a suitable OCAMLPARAM value
$ eval `./setup_shell.sh`
$ ocamlc test_ko.ml
...
Error: Spurious semi-colon at 2:9
...
# In the updated environment, the new frontend is picked up automatically
```

Now you are ready to iterate on [ocaml/parse_errors.mlyl]() to produce new rules.

**Note: `unset OCAMLPARAM` to switch back to the normal frontend**

## Devising a new rule

