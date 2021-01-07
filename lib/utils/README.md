This directory provide definitions that are not specific to Lrlex and should be
easy to reuse elsewhere.

- [cmon.ml]: "Caml Object Notation", a pretty-printer for values that mimics
  OCaml syntax and has explicit representation of sharing (using `let`-binders)
- [fastdom.ml]: implementation of "A Simple, Fast Dominance Algorithm", used by 
  `Cmon`
