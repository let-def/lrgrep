# Syntax error analyser 

This repository provides different tools to work on the error messages of a menhir-generated parser.

The main tool is `lrgrep`. It takes:

- a compiled Menhir grammar (a .cmly file, produced by passing `--cmly` flag to Menhir) 
- a list of rules (usually a .mlyl file).

If the list of rule is well-formed, it produces an OCaml module that can match the rules against the state of a parser at runtime.

By carefully crafting the rules, one can provide fine-grained message to explain syntax errors.



The repository is is structured as follow:

- the main tool, lrgrep, can be found in [src/main.ml]()
- [support]() implements the compact table representation shared by the generator and the generated analysers via the `lrgrep.runtime` library
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
$ ocamlc -c -pp _build/default/ocaml/frontend.bc test_ko_01.ml
ocamlc -pp _build/default/ocaml/frontend.bc test_ko_01.ml
File "test_ko_01.ml", line 4, characters 0-3:
4 | let z = 7
    ^^^
Error: Spurious semi-colon at 2:9

File "test_ko_01.ml", line 1:
Error: Error while running external preprocessor
Command line: _build/default/ocaml/frontend.bc 'test_ko_01.ml' > /tmp/ocamlppbbc3f9
```

In this one however, there is a syntax error. Luckily, this case is covered by a rule: while the error happens on line 4, it is likely caused by the semi-colon at the end of line 2.

#### Using the frontend for compiling ocaml files

By using the `OCAMLPARAM` environment variable, we can instruct all execution of ocaml compilers in the current shell to use our frontend.

```shell
$ ./setup_shell.sh
export 'OCAMLPARAM=pp=$PWD/lrgrep/_build/default/ocaml/frontend.bc,_'
# setup_shell commands produces a suitable OCAMLPARAM value
$ eval `./setup_shell.sh`
$ ocamlc test_ko_01.ml
...
Error: Spurious semi-colon at 2:9
...
# In the updated environment, the new frontend is picked up automatically
```

Now you are ready to iterate on [ocaml/parse_errors.mlyl]() to produce new rules.

**Note: `unset OCAMLPARAM` to switch back to the normal frontend**

## Devising a new rule

**TODO**

# Getting started with LRGrep codebase

I am trying to document the code. Each of the [src](src), [lib](lib), [ocaml](ocaml), and [support](support) directories contain a README.md that briefly explains the purpose of this directory.

External dependencies that are worth knowing:

- [MenhirSdk](https://opam.ocaml.org/packages/menhirSdk/) is a part of the [Menhir](http://gitlab.inria.fr/fpottier/menhir) parser generator that allows external tool to post-process compiled grammars
- [Cmon](https://opam.ocaml.org/packages/cmon/) is a pretty-printer for recursive values
- [Fix](https://gitlab.inria.fr/fpottier/fix) is a library for computing fixed points; it also provides a convenient representation of finite sets
- [LRijkstra](https://gitlab.inria.fr/fpottier/menhir/-/blob/master/src/LRijkstraFast.mli) is taken from Menhir and implements the algorithm described in ["Faster Reachability Analysis for LR(1) Parsers"](https://dl.acm.org/doi/10.1145/3486608.3486903), though we apply it for a slightly different purpose than the one described in the articles

## Things to do

I have noted some urgent and less urgent things to improve in [FUTURE.md](FUTURE.md).
