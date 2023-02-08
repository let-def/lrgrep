## FIXME

- Looking at examples from Stackoverflow:

  1. https://stackoverflow.com/questions/71847555/ocaml-syntax-error-but-i-dont-know-where-can-somoene-help-me
      Already handled! LATER: Could improve message (link to the manual ?)
  2. https://stackoverflow.com/questions/52323697/why-is-there-a-ocaml-syntax-error-on-if-statement
      Interesting, there is a semi-colon after the `if ... then ...` which is terminating this expression for the OCaml parser, therefore it fails on the `else`.

  3. https://stackoverflow.com/questions/67361797/ocaml-syntax-error-in-let-expression-following-module-definition

  4. https://stackoverflow.com/questions/69595112/the-problem-with-writing-a-function-in-ocaml

  5. https://stackoverflow.com/questions/70691144/match-inside-match-ocaml-raises-syntax-error

- Document error coverage.
- LATER: Document other error rules and common patterns/tricks
- LATER: typing of captured constructs in the generated code (avoid wrapping  with options and manually matching Menhir’s GADT when it is safe)
- LATER: in the coverage output, factor common reductions
