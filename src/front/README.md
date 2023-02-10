This is the frontend of LRgrep.

- [Syntax](syntax.ml) defines the AST, its the most interesting file.
- Parsing is implemented by [Lexer](lexer.mll) and [Parser](parser.mly), using OCamllex and Menhir.

Some code and lexical conventions are taken from OCamllex

**TODO (not urgent):** action lexing is implemented using global states and side effects. That deserves some cleaning.