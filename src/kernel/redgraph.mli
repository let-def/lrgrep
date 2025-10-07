open Fix.Indexing
open Utils
open Misc
open Info

type 'g reduction_closure = {
  failing: 'g terminal indexset;
  reductions: ('g nonterminal, 'g terminal indexset) indexmap list;
  stacks: ('g lr1 index list * 'g terminal indexset) list;
}

val close_lr1_reductions
  :  'g grammar
  -> ('g lr1, 'g reduction_closure) vector

val close_goto_reductions
  :  'g grammar
  -> ('g lr1, 'g reduction_closure) vector
  -> ('g goto_transition, 'g reduction_closure) vector

type 'g target

type 'g trie = private {
  mutable sub: ('g lr1, 'g trie) indexmap;
  mutable immediates: 'g lr1 indexset;
  mutable targets: ('g lr1, 'g target index) indexmap;
}

val index_targets
  :  'g grammar
  -> ('g lr1, 'g reduction_closure) vector
  -> 'g trie * ('g goto_transition, ('g target index * 'g terminal indexset) list) vector

type ('g, 's) node
type ('g, 's) step

type 'g node_desc = {
  lr1: 'g lr1 index;
  lookaheads: 'g terminal indexset;
}

type ('g, 's) step_desc = {
  next: ('g, 's) step index;
  reachable: 'g target indexset;
  goto: ('g lr1, 'g target indexset * ('g, 's) node indexset) indexmap;
}

type ('g, 's) graph = {
  nodes: (('g,'s) node, 'g node_desc * ('g,'s) step index) vector;
  initials: ('g lr1, ('g,'s) step index) vector;
  steps: (('g,'s) step, ('g, 's) step_desc) vector;
}

val make
  :  'g grammar
  -> ('g lr1, 'g reduction_closure) vector
  -> ('g goto_transition, ('g target index * 'g terminal indexset) list) vector
  -> ('g,'g lr1) graph

val dump_closure
  :  ?failing:bool
  -> 'g grammar
  -> ('n index -> string)
  -> ('n, 'g reduction_closure) vector
  -> unit

val dump_dot
  :  out_channel
  -> 'g grammar
  (* -> ('g goto_transition, 'g reduction_closure) vector *)
  -> ('g, 's) graph
  -> unit
