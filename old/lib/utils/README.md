This directory provide definitions that are not specific to LRgrep and should be
easy to reuse elsewhere.

**Finite Sets.** First, it defines a few data structures and algorithms for dealing with finite sets and their elements, when represented by `Fix.Indexing`:

- [SetSig](setSig.ml) is an alternative to Set.S from the standard library to define polymorphic sets with parameterized elements.
- [IndexSet](indexSet.ml) implements sets polymorphic with respect to `Fix.Indexing` elements, backed by a compact representation of bit sets defined in [IntSet](intSet.ml) and taken from Menhir (CompressedBitSet)

- [Refine](refine.mli) is a generic algorithm that computes the refinement of a list of sets: the smallest list of non-overlapping subsets with the same union
- [IndexRefine](indexRefine.ml) applies Refine to IndexSet
- [IndexBuffer](indexBuffer.mli) is a dynamically resizable variant of `Fix.Indexing.Vector`, to use when the cardinal of the finite set is not yet known
- [IndexMap](indexMap.mli) is a polymorphic instance of `Map`, just like `IndexSet` is a polymorphic instance of `IntSet`

**Misc.** Other miscellaneous routines are defined in [Misc](misc.ml). Small functions that do not have a proper module to go to.
