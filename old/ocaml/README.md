This directory implements custom parser errors for OCaml.

Two binaries are provided:

- [Frontend](frontend.ml) implements an alternative frontend for OCaml binaries (pass using `-pp`), that will parse the AST just like OCaml, but will use LRgrep to produce messages in case of errors.

- [Interpreter](interpreter.ml) parses prefix of OCaml code and dumps the state of the stack either at the end or at the first error, hinting at possible reductions. This is useful to design an error pattern from an sample input.

The [Parser](#parser) library is an alternative implementation of OCaml parser used by both by Frontend and Interpreter.

[Embed](embed.ml) is a simple helper to embed binary files in an OCaml source. It is used to embed Menhir CMLY data into the interpreter binary.

## Parser

[Lexer_raw](lexer_raw.mll) and [Parser_raw](parser_raw.mly) implements an OCamllex and Menhir-based parser for OCaml.

[Parser_def](parser_def.ml) contains some internal definitions of the parser.

[Parse_errors](parse_errors.mlyl) implements custom error messages using LRgrep.

TODO: find a way to reuse analysis from [src/mid/]() in Frontend and Interpreter.
