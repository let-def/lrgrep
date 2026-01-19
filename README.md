# Syntax error analyser 

This repository provides different tools to work on the error messages of a menhir-generated parser.

The main tool is `lrgrep`. It takes:

- a compiled Menhir grammar (a .cmly file, produced by passing `--cmly` flag to Menhir) 
- a list of rules (usually a .lrgrep file).

If the list of rule is well-formed, it produces an OCaml module that can match
the rules against the state of a parser at runtime.

By carefully crafting the rules, one can provide fine-grained message to explain syntax errors.

The repository is is structured as follow:

- the main tool, lrgrep, can be found in [src/main.ml]()
- [support]() implements the compact table representation shared by the
  generator and the generated analysers via the `lrgrep.runtime` library
- [lib]() implements various algorithms used by other tools

# Getting started with LRGrep codebase

I am trying to document the code. Each of the [src](src), [lib](lib), and [support](support) directories contain a README.md that briefly explains the purpose of this directory.

External dependencies that are worth knowing:

- [MenhirSdk](https://opam.ocaml.org/packages/menhirSdk/) is a part of the [Menhir](http://gitlab.inria.fr/fpottier/menhir) parser generator that allows external tool to post-process compiled grammars
- [Cmon](https://opam.ocaml.org/packages/cmon/) is a pretty-printer for recursive values
- [Fix](https://gitlab.inria.fr/fpottier/fix) is a library for computing fixed points; it also provides a convenient representation of finite sets
- [LRijkstra](https://gitlab.inria.fr/fpottier/menhir/-/blob/master/src/LRijkstraFast.mli) is taken from Menhir and implements the algorithm described in ["Faster Reachability Analysis for LR(1) Parsers"](https://dl.acm.org/doi/10.1145/3486608.3486903), though we apply it for a slightly different purpose than the one described in the articles
