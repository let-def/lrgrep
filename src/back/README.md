# Back-end

The [Dfa](dfa.ml) module starts from an instance of [`Mid.Sigs`](../mid/sigs.ml)`.REGEXP` and implements determinisation, table and code generation.

**TODO:** this is too much and kind of spaghetti, split in sub-modules

The [Coverage](coverage.ml) module implements coverage checking by intersecting the Dfa with an Nfa. Two candidates are provided:

- `Lrc.NFA` checks that all valid stacks are covered (starting from a parser that is waiting for input, so either in the initial state, or that has consumed some tokens but need more to finish the analysis)
- `Lrce.NFA` checks that all stacks that can reject a token, or, reduce to a stack that will reject a token, are covered (also starting from a parser waiting for input, for the generated analyser it means it should be fed with the last valid parser state before seeing the error)