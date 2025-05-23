open Fix.Indexing
open Utils
open Misc
open Info

type 'g n

type 'g t = {
  lr1_of: ('g n, 'g lr1 index) vector;
  lrcs_of: ('g lr1, 'g n indexset) vector;
  first_lrc_of: ('g lr1, 'g n index) vector;
  all_wait: 'g n indexset;
  all_successors: ('g n, 'g n indexset) vector;
  reachable_from: ('g n, 'g n indexset) vector;
}

val make : 'g info -> 'g Reachability.t -> 'g t
val lookahead : 'g Reachability.t -> 'g t -> 'g n index -> 'g terminal indexset
val class_index : 'g t -> 'g n index -> int
val to_string : 'g info -> 'g t -> 'g n index -> string
val set_to_string : 'g info -> 'g t -> 'g n indexset -> string

type 'g entrypoints = {
  reachable: 'g n indexset;
  wait: 'g n indexset;
  entrypoints: 'g n indexset;
  successors: ('g n, 'g n indexset) vector;
  predecessors: ('g n, 'g n indexset) vector;
  some_prefix: 'g n index -> 'g n index list;
}

val from_entrypoints : 'g info -> 'g t -> 'g n indexset -> 'g entrypoints
