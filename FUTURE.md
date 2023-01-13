

# Future work

---

Some thoughts on things that can be improved.

[TOC]

## Cleanup

[ocaml/frontend.ml]() and [ocaml/interpreter.ml]() contains some copy of analyses done by [src/mid]() and maybe [src/back]().

This should be refactored such that the analysis are implemented once and reused elsewhere.
The difficulty is that the analysis depends on a Menhir Grammar which MenhirSDK only allows to read from a file. This file is not guaranteed to be available from these binaries, so we embed it, but then there are no direct ways to read it. Should we generate a tmp file? That's ugly but maybe it is the easiest solution.

Or maybe I should ask for MenhirSdk interface to be a bit more flexible.

## Features

- [ ] Allow **matching on lookahead** tokens too
  This can be worked-around using partial matching with an extra argument, but we lose the benefits of coverage checks
- [ ] Report **unused/unreachable patterns**

- [ ] **Factorize coverage** reports
  Right now (02/09/2022), one example is reported for each state that does not handle some possible transitions. 
  1. This by itself is quite arbitrary, it might be worth thinking of something better
  2. Even if we keep this approach, reports should avoid reliance on the automaton and focus on grammatical concepts. Error paths with different states but similar symbols can be displayed together, and rather than printing a raw path, it might be preferable to display the sequence of reduction that led to the problematic state
  3. The item set of the last goto transition of this sequence of reduction might be a nice way       to explain an error very shortly, e.g. "paths reaching `LPAREN expr . RPAREN` are not covered".
- [ ] Maybe implement **interactive exploration** (TUI) of error paths.
- [ ] An interactive UI could also help **refining an exhaustive error coverage**. Interactive or not, this use case is important.
- [x] Finer-grained **typing of captures**. Right now, captures are always represented by an optional stack element.
  - We can statically tell if the capture is total or not, if total we can get rid of the option.
  - A stack element is very generic, we can often recover the type of the associated semantic value.
- [ ] Captures sometime only care about locations... A dedicated syntax would be more convenient.
- [ ] **Interaction between reduction and capture**. We could capture an approximate location if a capture occur within a reduction. A bit far fetched, but it could even be possible to compute the actual reduction to get the semantic value.

## Correction

- [ ] The interpreter does not keep track of lookahead, this can cause incorrect reductions to appear in hinted reductions (also, those hints should include the lookahead tokens)
- [ ] Coverage check stops after the first error for a given branch - but a subset of the lookahead might permit reaching other errors
- [x] The current implementation of **captures** is incorrect. In presence of non-determinism, if there are multiple occurrences of the capture, it will always keep the most recent one, though the matching branch might refer to a previous occurrence. (fixed in PR#2)

## Performance

- [ ] More **non-determinism**, for Automaton generation and for reduction simulation
  - Keeping an NFA longer won't increase the performance of the generator itself, but will help with coverage analysis.
  - [x] For reduction simulation, one of the main trick to increase performance. (Already mostly NFA-based)
  - [ ] Maybe we don't need DFA at all, runtime matching performance might be sufficient with an NFA? We can experiment if DFA size becomes problematic.
  - [ ] Keeping an NFA will help implementing the **correct capture semantics**. (Fixed in PR#2, however I maintain that NFA will be more helpful!)
- [ ] The automaton we produce is not optimized at the moment. Some dead branches could be cleaned up, and a minimization algorithm will help (an extension of Valmari to "partial transition functions", or some heuristics applied either on the NFA or the DFA)

### Datastructures

Play a bit with various datastructures for representing finite sets.
Right now we only use Menhir's `CompressedBitSet`. Idea to test:
- [ ] `CoBitSet` (WIP) might improve performance when we deal with a lot of
  complementation
- [ ] a mix between `CompressedBitSet` and Patricia trie might help with large
  sparse sets

## Examples / Applications

It is important to find convincing applications of LRgrep beyond OCaml syntax.

I struggled to find parser's with actually good error messages (rather than using "having good error messages" as a strawman's argument againt LR 🙃), but Elm's parser stand out. Real work went into making the error messages good.

I think that an LR parser for Elm syntax with comparable error messages quality might be a good argument in favor of LRgrep. It would require a lot of work and might not be enough, but still good. For reference:

- [Elm's parser](https://github.com/elm/compiler/tree/master/compiler/src/Parse)
- [Elm's syntax errors](https://github.com/elm/compiler/blob/master/compiler/src/Reporting/Error/Syntax.hs)

Also for "brace-based" languages, it might be possible to have a finer-grained characterization of errors than existing tools permit (with possibly the exception of heavily instrumented manually written parsers). For instance, it should be possible to switch between expression/statement/block-level analysis for each pattern.
