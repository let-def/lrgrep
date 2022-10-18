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

  val index : state -> state_index
  val forward : state -> transition list
  val backward : state -> transition list
  val accepted : state -> (KRE.clause index * thread indexset) list

  type dfa = state array

  (** Produce a DFA from an initial expression, as an array.
      The initial state is the first element of the array. *)
  val derive_dfa : KRESet.t -> dfa

  (** Compile a DFA to a compact table, suitable for use with Lrgrep_support
      and runtime libraries. *)
  val gen_table : dfa -> Lrgrep_support.compact_dfa

  (** FIXME: Cleanup *)
  val eval : dfa -> state_index -> stack:Lr1.t list -> unit
end
