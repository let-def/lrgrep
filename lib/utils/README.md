This directory provide definitions that are not specific to Lrlex and should be
easy to reuse elsewhere.

- [cmon.mli](): "Caml Object Notation", a pretty-printer for values that mimics
  OCaml syntax and has explicit representation of sharing (using `let`-binders)
- [fastdom.mli](): an implementation of "A Simple, Fast Dominance Algorithm",
  used by `Cmon` for pretty-printing shared constructions
- [bitSet.mli](): a representation of set of bits taken from Menhir
  (CompressedBitSet) and augmented to implement the `Refine.DECOMPOSABLE`
interface
- [refine.mli](): an algorithm for computing the refinement of a list of sets:
  the smallest list of non-overlapping subsets with the same union
- [strong.mli](): type system tricks to work with finite sets
- [scc.mli](): compute the strongly connected components of a graph represented
  using `Strong`'s finite sets
- [mulet.mli](): a generic toolkit for producing automaton from regular
  expression using derivation
- [valmari.mli](): a DFA minimization algorithm implemented using `Strong`
  finite sets
