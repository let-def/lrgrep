open Fix.Indexing
open Utils
open Misc
open Info

type 'g n

type ('g, 'n) t = {
  lr1_of: ('n, 'g lr1 index) vector;
  lrcs_of: ('g lr1, 'n indexset) vector;
  all_wait: 'n indexset;
  all_leaf: 'n indexset;
  all_successors: ('n, 'n indexset) vector;
  reachable_from: ('n, 'n indexset) vector;
}

val make : 'g grammar -> 'g Reachability.t -> ('g, 'g n) t
val to_string : 'g grammar -> ('g, 'g n) t -> 'g n index -> string
val set_to_string : 'g grammar -> ('g, 'g n) t -> 'g n indexset -> string

type 'n entrypoints = {
  reachable: 'n indexset;
  wait: 'n indexset;
  entrypoints: 'n indexset;
  successors: ('n, 'n indexset) vector;
  predecessors: ('n, 'n indexset) vector;
  some_prefix: 'n index -> 'n index list;
}

val from_entrypoints : 'g grammar -> ('g, 'n) t -> 'n indexset -> 'n entrypoints

type 'g dlrc
val make_deterministic : 'g grammar -> 'g Reachability.t -> ('g, 'g dlrc) t

type 'g mlrc
val make_minimal : 'g grammar -> 'g Reachability.t -> ('g, 'g mlrc) t

val check_equivalence : 'g grammar -> ('g, 'n1) t -> ('g, 'n2) t -> 'n1 indexset -> 'n2 indexset -> unit
