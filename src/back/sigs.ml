open Fix.Indexing
open Utils.Misc

module type DFA = sig
  module Regexp : Mid.Sigs.REGEXP
  open Regexp
  open Info

  type state_index = int
  type state

  type thread
  val thread : int -> thread index

  val threads : state -> int

  type transition
  val label : transition -> Lr1.set
  val source : transition -> state_index
  val target : transition -> state_index
  val reverse_mapping : transition -> target_thread:thread index -> (thread, RE.var indexset) indexmap
  val all_vars : transition -> RE.var indexset

  val index : state -> state_index
  val forward : state -> transition list
  val backward : state -> transition list
  val accepted : state -> (KRE.clause index * thread index) list

  type dfa = state array

  (** Produce a DFA from an initial expression, as an array.
      The initial state is the first element of the array. *)
  val derive_dfa : KRESet.t -> dfa

  type liveness = (thread, RE.var indexset) indexmap array

  (** Compile a DFA to a compact table, suitable for use with Lrgrep_support
      and runtime libraries. *)
  val gen_table : dfa -> liveness -> Lrgrep_support.compact_dfa

  (** FIXME: Cleanup *)
  val eval : dfa -> state_index -> stack:Lr1.t list -> unit
end
