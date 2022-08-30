# Middle-end

The middle-end implements the core algorithms of LRgrep.

The entire library is parameterized over the contents of a Menhir CMLY file  (that contains  marshalled representation of the grammar and of the automaton). Ideally, it could just be a big function taking a `MenhirSdk.Cmly_format.GRAMMAR` as argument, however it is not (yet) possible to make a functor that spans multiple implementation file in OCaml.

Therefore the analysis is splits into finer-grained components, the interfaces of which are defined in the [Sigs](sigs.ml) module. Documentation for all signatures can be found here.

The [Info](info.ml) module processes the Menhir automaton to present a more convenient API and pre- compute some information used by further passes.

The [Regexp](regexp.ml) defines the intermediate representation of regular expressions and implements Antimirov's derivative. It does not compute reductions directly, but normalize regular expressions into parts that should be matched directly and parts that should be reduced.

The [Redgraph](redgraph.ml) module computes information necessary to implement reduction efficiently.

The [Reduction](reduction.ml) module connects Regexp and Redgraph to implement derivation modulo reductoin. 