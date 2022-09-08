# LRgrep command

This is the implementation of the main `lrgrep` command.

The entrypoint is module [Main](main.ml). Functionalities are implemented in sub-directories:

- [front](front/) implements the lexer, the parser and defines the AST
- [mid](mid/) implements various grammatical analyses, transformations to an intermediate language, simulation of reductions and regular expression derivatives
- [back](back/) implements generation and compaction of automaton, table and of OCaml code, and coverages check
