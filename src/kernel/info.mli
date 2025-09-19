(* MIT License
 *
 * Copyright (c) 2025 Frédéric Bour
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *)

(** This module defines data structures and operations for handling grammar
  information in a structured way. It includes representations for terminals,
  non-terminals, productions, and LR states, along with their transitions and
  reductions. The module is designed to work with Menhir's grammar
  representation and extends it with additional functionality for
  convenience. *)

open Utils
open Misc
open Fix.Indexing

module type GRAMMAR = MenhirSdk.Cmly_api.GRAMMAR

type 'g grammar

module Lift(G : MenhirSdk.Cmly_api.GRAMMAR) : sig
  type g
  val grammar : g grammar
end

type 'g terminal
type 'g nonterminal
type 'g symbol = ('g terminal, 'g nonterminal) Sum.n
type 'g production
type 'g item
type 'g lr0
type 'g lr1

(* Finite sets to index to different kinds of transitions.
   For instance, [goto_transition] represents the finite set of goto transition:
   - the value [Transition.goto grammar : goto_transition cardinal] is the
     cardinal of this set
   - any value of type [goto_transition index] is a member of this set
     (representing a goto transition)
*)
type 'g goto_transition
type 'g shift_transition
type 'g transition = ('g goto_transition, 'g shift_transition) Sum.n

type 'g reduction

module type INDEXED = sig
  type 'g n

  val cardinal : 'g grammar -> 'g n cardinal
  val of_int : 'g grammar -> int -> 'g n index
end

module Terminal : sig
  include INDEXED with type 'g n = 'g terminal

  val to_string : 'g grammar -> 'g n index -> string
  val all : 'g grammar -> 'g n indexset
  val regular : 'g grammar -> 'g n indexset

  (** [semantic_value term] is [Some typ] if terminal [term] has a semantic
      value of type [typ], or [None] for unparameterized terminals. *)
  val semantic_value : 'g grammar -> 'g n index -> string option

  (** Wrapper around [IndexSet.inter] speeding-up intersection with [all] *)
  val intersect : 'g grammar -> 'g n indexset -> 'g n indexset -> 'g n indexset

  (** Is it the special `error` symbol *)
  val is_error : 'g grammar -> 'g n index -> bool

  val lookaheads_to_string : 'g grammar -> 'g n indexset -> string
end

module Nonterminal : sig
  include INDEXED with type 'g n = 'g nonterminal
  val to_string : 'g grammar -> 'g n index -> string
  val all : 'g grammar -> 'g n indexset
  val kind : 'g grammar -> 'g n index -> [`REGULAR | `START]
  val semantic_value : 'g grammar -> 'g n index -> string option
  val nullable : 'g grammar -> 'g n index -> bool
  val first : 'g grammar -> 'g n index -> 'g terminal indexset
end

module Symbol : sig
  include INDEXED with type 'g n = 'g symbol

  type 'g desc =
    | T of 'g terminal index
    | N of 'g nonterminal index

  val desc : 'g grammar -> 'g n index -> 'g desc

  val is_terminal : 'g grammar -> 'g n index -> bool
  val is_nonterminal : 'g grammar -> 'g n index -> bool

  val name : 'g grammar -> ?mangled:bool -> 'g n index -> string
  val semantic_value : 'g grammar -> 'g n index -> string option

  val all : 'g grammar -> 'g n indexset

  val inj_t : 'g grammar -> 'g terminal index -> 'g symbol index
  val inj_n : 'g grammar -> 'g nonterminal index -> 'g symbol index
end

module Production : sig
  include INDEXED with type 'g n = 'g production
  val lhs : 'g grammar -> 'g n index -> 'g nonterminal index
  val rhs : 'g grammar -> 'g n index -> 'g symbol index array
  val length : 'g grammar -> 'g n index -> int
  val kind : 'g grammar -> 'g n index -> [ `REGULAR | `START ]
  val all : 'g grammar -> 'g n indexset
end

(* Explicit representation of LR(0) items *)
module Item : sig
  include INDEXED with type 'g n = 'g item
  val make : 'g grammar -> 'g production index -> int -> 'g n index
  val last : 'g grammar -> 'g production index -> 'g n index
  val prev : 'g grammar -> 'g n index -> 'g n index option
  val desc : 'g grammar -> 'g n index -> 'g production index * int
  val position : 'g grammar -> 'g n index -> int
  val production : 'g grammar -> 'g n index -> 'g production index
  val is_reducible : 'g grammar -> 'g n index -> bool
  val to_string : 'g grammar -> 'g n index -> string
end

module Lr0 : sig
  include INDEXED with type 'g n = 'g lr0

  (* See [Lr1.incoming]. *)
  val incoming : 'g grammar -> 'g n index -> 'g symbol index option

  (* See [Lr1.items]. *)
  val items : 'g grammar -> 'g n index -> 'g item indexset

  (* If the state is an initial state, returns the pseudo (start)
     production that recognizes this entrypoint. *)
  val is_entrypoint : 'g grammar -> 'g n index -> 'g production index option
end

module Lr1 : sig
  include INDEXED with type 'g n = 'g lr1
  val all : 'g grammar -> 'g n indexset
  val accepting : 'g grammar -> 'g n indexset

  (* A ``wait'' state is an LR(1) state in which the parser needs to look at
     more input before knowing how to proceed.
     Wait states are the initial states and the targets of SHIFT transitions
     (states with a terminal as incoming symbol), except the accepting ones
     (after reading EOF, the only valid action is to reduce). *)
  val wait : 'g grammar -> 'g n indexset

  (* Get the LR(0) "core" state *)
  val to_lr0 : 'g grammar -> 'g n index -> 'g lr0 index

  (* The symbol annotating the incoming transitions of a state.
     There is none for initial states, and at most one for others. *)
  val incoming : 'g grammar -> 'g n index -> 'g symbol index option

  (* Get the items in the kernel of a state (before closure). *)
  val items : 'g grammar -> 'g n index -> 'g item indexset

  (* Printing functions, for debug purposes.
     Not nice for the end-user (FIXME). *)

  val to_string : 'g grammar -> 'g n index -> string
  val list_to_string : 'g grammar -> 'g n index list -> string
  val set_to_string : 'g grammar -> 'g n indexset -> string

  val symbol_to_string : 'g grammar -> 'g n index -> string

  (** [shift_on t] is the set of lookaheads that state [t] can shift *)
  val shift_on : 'g grammar -> 'g n index -> 'g terminal indexset

  (** [reduce_on t] is the set of lookaheads that trigger a reduction in state
      [t] *)
  val reduce_on : 'g grammar -> 'g n index -> 'g terminal indexset

  (** [reject t] is set of lookaheads that cause the automaton to fail when in
      state [t] *)
  val reject : 'g grammar -> 'g n index -> 'g terminal indexset

  (** [predecessors t] is the set of LR(1) states that have transition going
      to [t]. *)
  val predecessors : 'g grammar -> 'g n index -> 'g n indexset lazy_stream

  (** Wrapper around [IndexSet.inter] speeding-up intersection with [all] *)
  val intersect : 'g grammar -> 'g n indexset -> 'g n indexset -> 'g n indexset

  val is_entrypoint : 'g grammar -> 'g n index -> 'g production index option
  val entrypoint_table : 'g grammar -> (string, 'g n index) Hashtbl.t
  val entrypoints : 'g grammar -> 'g n indexset

  val default_reduction : 'g grammar -> 'g n index -> 'g production index option
end

module Transition : sig
  (* The set of goto transitions *)
  val goto : 'g grammar -> 'g goto_transition cardinal
  (* The set of all transitions = goto U shift *)
  val any : 'g grammar -> 'g transition cardinal
  (* The set of shift transitions *)
  val shift : 'g grammar -> 'g shift_transition cardinal

  (* Building the isomorphism between any and goto U shift *)

  (* Inject goto into any *)
  val of_goto : 'g grammar -> 'g goto_transition index -> 'g transition index

  (* Inject shift into any *)
  val of_shift : 'g grammar -> 'g shift_transition index -> 'g transition index

  (* Project a transition into a goto or a shift transition *)
  val split
    :  'g grammar
    -> 'g transition index
    -> ('g goto_transition index, 'g shift_transition index) either

  (* [find_goto s nt] finds the goto transition originating from [s] and
     labelled by [nt], or raise [Not_found].  *)
  val find_goto : 'g grammar -> 'g lr1 index -> 'g nonterminal index -> 'g goto_transition index
  val find_goto_target : 'g grammar -> 'g lr1 index -> 'g nonterminal index -> 'g lr1 index

  (* Get the source state of a transition *)
  val source : 'g grammar -> 'g transition index -> 'g lr1 index

  (* Get the target state of a transition *)
  val target : 'g grammar -> 'g transition index -> 'g lr1 index

  (* Symbol that labels a transition *)
  val symbol : 'g grammar -> 'g transition index -> 'g symbol index

  (* Symbol that labels a goto transition *)
  val goto_symbol : 'g grammar -> 'g goto_transition index -> 'g nonterminal index

  (* Symbol that labels a shift transition *)
  val shift_symbol : 'g grammar -> 'g shift_transition index -> 'g terminal index

  (* [successors s] returns all the transitions [tr] such that
     [source tr = s] *)
  val successors : 'g grammar -> 'g lr1 index -> 'g transition indexset

  (* [predecessors s] returns all the transitions [tr] such that
     [target tr = s] *)
  val predecessors : 'g grammar -> 'g lr1 index -> 'g transition indexset

  (* Accepting transitions are goto transitions from an initial state to an
     accepting state, recognizing one of the grammar entrypoint. *)
  val accepting : 'g grammar -> 'g goto_transition indexset

  val to_string : 'g grammar -> 'g transition index -> string
end

module Reduction : sig
  include INDEXED with type 'g n = 'g reduction

  (* A reduction is a triple [(lr1, prod, lookaheads)], meaning that:
     in state [lr1], when looking ahead at a terminal in [lookaheads], the
     action is to reduce [prod]. *)

  val state: 'g grammar -> 'g n index -> 'g lr1 index
  val production: 'g grammar -> 'g n index -> 'g production index
  val lookaheads: 'g grammar -> 'g n index -> 'g terminal indexset

  (* All reductions applicable to an lr1 state. *)
  val from_lr1: 'g grammar -> 'g lr1 index -> 'g n indexset
end

val raw : _ grammar -> (module MenhirSdk.Cmly_api.GRAMMAR)
