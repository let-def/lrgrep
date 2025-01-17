This is a (temporary) fork of [Fix](https://gitlab.inria.fr/fpottier/fix) library.

The only addition is the [`Indexing`](indexing.mli)`.Index.Unsafe` module, which allow to locally reveal that a strongly-typed index is just an integer to implement some data structures more efficiently. Modular abstraction is temporarily broken: it is the responsibility of the caller to make sure that only correct indices will be generated.

This directory is licensed under GPLv2.
