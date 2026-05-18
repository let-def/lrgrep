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

(** Grammar information interface

    This module exports data structures and operations for handling grammar
    information. It provides indexed representations of grammars with type-level
    cardinalities for type-safe access.

    Main components:

    - The [Load_grammar] functor takes Menhir's grammar representation and
      computes all index structures for the grammar.

    - Types for grammar elements with their indices:
      - [terminal], [nonterminal], [symbol], [production], [item]
      - [lr0], [lr1]: LR(0) and LR(1) states
      - [goto_transition], [shift_transition], [transition]: Transitions
      - [reduction]: Reductions

    - Module interfaces for each grammar element:
      - [Terminal], [Nonterminal], [Symbol], [Production], [Item]
      - [Lr0], [Lr1], [Transition], [Reduction]

    Each module provides:
      - [cardinal]: The size of the index set
      - [of_int]: Construct an index from an integer
      - Type-specific functions for accessing properties
*)

open Utils
open Misc
open Fix.Indexing

module type GRAMMAR = MenhirSdk.Cmly_api.GRAMMAR

type 'g grammar

module Load_grammar(G : MenhirSdk.Cmly_api.GRAMMAR) : sig
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
  val alias : 'g grammar -> 'g n index -> string option

  (** [all g] is the set of all terminals in grammar [g] *)
  val all : 'g grammar -> 'g n indexset

  (** [regular g] is the set of regular terminals in grammar [g],
      excluding EOF, ERROR, and pseudo-terminals *)
  val regular : 'g grammar -> 'g n indexset

  (** [semantic_value term] is [Some typ] if terminal [term] has a semantic
      value of type [typ], or [None] for unparameterized terminals. *)
  val semantic_value : 'g grammar -> 'g n index -> string option

  (** Wrapper around [IndexSet.inter] speeding-up intersection with [all] *)
  val intersect : 'g grammar -> 'g n indexset -> 'g n indexset -> 'g n indexset

  (** Is it the special `error` symbol *)
  val is_error : 'g grammar -> 'g n index -> bool

  (** Converts a set of lookahead terminals to a human-readable string.
      Sets larger than 10 elements are abbreviated. *)
  val lookaheads_to_string : 'g grammar -> 'g n indexset -> string

  (** Finds a terminal by name. Returns disambiguation suggestions when
      the name is not found and [approx > 0]. *)
  val find
    :  'g grammar -> ?approx:int -> string
    -> ('g n index, (int * string * 'g n index) list) result
end

module Nonterminal : sig
  include INDEXED with type 'g n = 'g nonterminal

  (** Converts a nonterminal index to its name string *)
  val to_string : 'g grammar -> 'g n index -> string

  (** [all g] is the set of all non-terminals in grammar [g] *)
  val all : 'g grammar -> 'g n indexset

  (** Returns [`REGULAR] for ordinary non-terminals and [`START] for
      entrypoint (start) non-terminals *)
  val kind : 'g grammar -> 'g n index -> [`REGULAR | `START]

  (** [semantic_value nt] is [Some typ] if nonterminal [nt] has a semantic
      value of type [typ], or [None] for unparameterized non-terminals. *)
  val semantic_value : 'g grammar -> 'g n index -> string option

  (** [nullable nt] is [true] if nonterminal [nt] can derive the empty string *)
  val nullable : 'g grammar -> 'g n index -> bool

  (** [first nt] is the set of terminals that can begin a string derived
      from nonterminal [nt] *)
  val first : 'g grammar -> 'g n index -> 'g terminal indexset

  (** Finds a nonterminal by name. Checks both regular and mangled names.
      Returns disambiguation suggestions when the name is not found and
      [approx > 0]. *)
  val find
    :  'g grammar -> ?approx:int -> string
    -> ('g n index, [ `Mangled of 'g n index
                    | `Dym of (int * string * 'g n index) list ]) result
end

module Symbol : sig
  include INDEXED with type 'g n = 'g symbol

  (** Discriminated union of terminal and nonterminal indices *)
  type 'g desc =
    | T of 'g terminal index
    | N of 'g nonterminal index

  (** Returns the symbol as a discriminated union: [T] for terminals,
      [N] for non-terminals *)
  val desc : 'g grammar -> 'g n index -> 'g desc

  (** Returns [true] if the symbol is a terminal *)
  val is_terminal : 'g grammar -> 'g n index -> bool

  (** Returns [true] if the symbol is a non-terminal *)
  val is_nonterminal : 'g grammar -> 'g n index -> bool

  (** Converts a symbol index to its name string. With [mangled:true],
      returns the mangled name used internally by Menhir. *)
  val to_string : 'g grammar -> ?mangled:bool -> 'g n index -> string

  (** [semantic_value s] returns the semantic value type of symbol [s].
      Returns [Some "unit"] for unparameterized terminals. *)
  val semantic_value : 'g grammar -> 'g n index -> string option

  (** [all g] is the set of all symbols (terminals and non-terminals) in grammar [g] *)
  val all : 'g grammar -> 'g n indexset

  (** Inject a terminal index into the symbol index space *)
  val inj_t : 'g grammar -> 'g terminal index -> 'g symbol index

  (** Inject a nonterminal index into the symbol index space *)
  val inj_n : 'g grammar -> 'g nonterminal index -> 'g symbol index

  (** Finds a symbol (terminal or nonterminal) by name. Checks both regular
      and mangled names. Returns disambiguation suggestions when the name
      is not found and [approx > 0]. *)
  val find
    :  'g grammar
    -> ?approx:int
    -> string
    -> ('g n index, [ `Mangled of 'g nonterminal index
                    | `Dym of (int * string * 'g n index) list]) result
end

module Production : sig
  include INDEXED with type 'g n = 'g production

  (** [lhs p] returns the left-hand side nonterminal of production [p] *)
  val lhs : 'g grammar -> 'g n index -> 'g nonterminal index

  (** [rhs p] returns the right-hand side symbols of production [p] *)
  val rhs : 'g grammar -> 'g n index -> 'g symbol index array

  (** [length p] returns the number of symbols on the right-hand side of production [p] *)
  val length : 'g grammar -> 'g n index -> int

  (** Returns [`REGULAR] for ordinary productions and [`START] for pseudo
      (start) productions generated for entrypoints *)
  val kind : 'g grammar -> 'g n index -> [ `REGULAR | `START ]

  (** [all g] is the set of all productions in grammar [g] *)
  val all : 'g grammar -> 'g n indexset
end

(** Explicit representation of LR(0) items.
    An item is a production with a dot position: [A -> α . β].
    Items are indexed globally across all productions for efficient set operations.
    The dot position ranges from 0 (before all RHS symbols) to [length] (after all). *)
module Item : sig
  include INDEXED with type 'g n = 'g item

  (** [make g prod pos] creates an item for production [prod] with the dot
      at position [pos]. Raises [Invalid_argument] if [pos] is out of bounds. *)
  val make : 'g grammar -> 'g production index -> int -> 'g n index

  (** [last g prod] creates an item with the dot at the end of production [prod],
      i.e., the item where the production is fully recognized. *)
  val last : 'g grammar -> 'g production index -> 'g n index

  (** [prev g item] returns the previous item in the same production (dot moved
      one position left), or [None] if the dot is already at position 0. *)
  val prev : 'g grammar -> 'g n index -> 'g n index option

  (** [desc g item] returns the (production, position) pair for the item *)
  val desc : 'g grammar -> 'g n index -> 'g production index * int

  (** [position g item] returns the dot position within the item's production *)
  val position : 'g grammar -> 'g n index -> int

  (** [production g item] returns the production that this item belongs to *)
  val production : 'g grammar -> 'g n index -> 'g production index

  (** [is_reducible g item] returns [true] if the dot is at the end of the
      production, meaning the item is ready to be reduced. *)
  val is_reducible : 'g grammar -> 'g n index -> bool

  (** Converts an item to a human-readable string in standard notation,
      e.g., "A:B . c d" *)
  val to_string : 'g grammar -> 'g n index -> string
end

(** LR(0) state information.
    LR(0) states represent the "core" of LR(1) states, ignoring lookahead information. *)
module Lr0 : sig
  include INDEXED with type 'g n = 'g lr0

  (** [incoming g s] returns the symbol that labels the transition into state [s].
      Returns [None] for initial states (which have no incoming transition).
      For non-initial states, returns [Some sym] where [sym] is the symbol
      that was parsed to reach this state. *)
  val incoming : 'g grammar -> 'g n index -> 'g symbol index option

  (** [items g s] returns the set of LR(0) items in state [s].
      These are the core items before closure is applied. *)
  val items : 'g grammar -> 'g n index -> 'g item indexset

  (** [is_entrypoint g s] returns [Some prod] if [s] is an initial state
      corresponding to an entrypoint, where [prod] is the pseudo (start)
      production for that entrypoint. Returns [None] for non-initial states. *)
  val is_entrypoint : 'g grammar -> 'g n index -> 'g production index option
end

(** LR(1) state information.
    LR(1) states extend LR(0) cores with lookahead information,
    determining when reductions should be performed. *)
module Lr1 : sig
  include INDEXED with type 'g n = 'g lr1

  (** [all g] is the set of all LR(1) states in grammar [g] *)
  val all : 'g grammar -> 'g n indexset

  (** [accepting g] is the set of LR(1) accepting states in grammar [g].
      These states are reached after fully recognizing an entrypoint. *)
  val accepting : 'g grammar -> 'g n indexset

  (** [wait g] is the set of "wait" states in grammar [g].
      A wait state is an LR(1) state where the parser must look at more
      input before deciding how to proceed. Wait states include initial
      states and targets of shift transitions, excluding accepting states. *)
  val wait : 'g grammar -> 'g n indexset

  (** [to_lr0 g s] returns the LR(0) "core" state corresponding to LR(1) state [s].
      Multiple LR(1) states may share the same LR(0) core but differ in lookaheads. *)
  val to_lr0 : 'g grammar -> 'g n index -> 'g lr0 index

  (** [incoming g s] returns the symbol that labels the transition into state [s].
      Returns [None] for initial states. For non-initial states, returns [Some sym]
      where [sym] is the symbol that was parsed to reach this state. *)
  val incoming : 'g grammar -> 'g n index -> 'g symbol index option

  (** [items g s] returns the set of LR(0) items in the kernel of state [s],
      i.e., the items before closure is applied. *)
  val items : 'g grammar -> 'g n index -> 'g item indexset

  (** [is_entrypoint g s] returns [Some prod] if [s] is an entrypoint state,
      where [prod] is the pseudo (start) production. Returns [None] otherwise. *)
  val is_entrypoint : 'g grammar -> 'g n index -> 'g production index option

  (** [entrypoints g] is the set of LR(1) states that correspond to grammar entrypoints *)
  val entrypoints : 'g grammar -> 'g n indexset

  (** Hash table mapping entrypoint names to their corresponding LR(1) states *)
  val entrypoint_table : 'g grammar -> (string, 'g n index) Hashtbl.t

  (** Debug printing functions. Formats are not stable across versions. *)

  (** Converts an LR(1) state to a debug string *)
  val to_string : 'g grammar -> 'g n index -> string

  (** Converts a list of LR(1) states to a debug string *)
  val list_to_string : 'g grammar -> 'g n index list -> string

  (** Converts a set of LR(1) states to a debug string *)
  val set_to_string : 'g grammar -> 'g n indexset -> string

  (** Converts the incoming symbol of a state to a debug string *)
  val symbol_to_string : 'g grammar -> 'g n index -> string

  (** [shift_on g s] is the set of terminals that state [s] can shift on,
      i.e., terminals for which there is a shift transition from [s]. *)
  val shift_on : 'g grammar -> 'g n index -> 'g terminal indexset

  (** [reduce_on g s] is the set of terminals that trigger a reduction in state [s],
      i.e., lookahead terminals for which a reduce action is taken. *)
  val reduce_on : 'g grammar -> 'g n index -> 'g terminal indexset

  (** [reject g s] is the set of terminals that cause a syntax error in state [s],
      i.e., terminals for which there is no shift, reduce, or default action. *)
  val reject : 'g grammar -> 'g n index -> 'g terminal indexset

  (** [predecessors g s] is the lazy stream of LR(1) states that have a transition
      going to state [s]. Lazy evaluation avoids materializing all predecessors upfront. *)
  val predecessors : 'g grammar -> 'g n index -> 'g n indexset lazy_stream

  (** Wrapper around [IndexSet.inter] speeding-up intersection with [all] *)
  val intersect : 'g grammar -> 'g n indexset -> 'g n indexset -> 'g n indexset

  (** [default_reduction g s] returns the default reduction for state [s], if any.
      Some states have a single applicable reduction that can be taken without
      checking the lookahead terminal. *)
  val default_reduction : 'g grammar -> 'g n index -> 'g production index option
end

module Transition : sig
  (** [goto g] is the cardinality of the set of goto transitions in grammar [g] *)
  val goto : 'g grammar -> 'g goto_transition cardinal

  (** [any g] is the cardinality of the set of all transitions (goto + shift) in grammar [g] *)
  val any : 'g grammar -> 'g transition cardinal

  (** [shift g] is the cardinality of the set of shift transitions in grammar [g] *)
  val shift : 'g grammar -> 'g shift_transition cardinal

  (** Inject a goto transition index into the combined transition index space *)
  val of_goto : 'g grammar -> 'g goto_transition index -> 'g transition index

  (** Inject a shift transition index into the combined transition index space *)
  val of_shift : 'g grammar -> 'g shift_transition index -> 'g transition index

  (** Project a transition index into either a goto or shift transition index *)
  val split
    :  'g grammar
    -> 'g transition index
    -> ('g goto_transition index, 'g shift_transition index) either

  (** [find_goto s nt] finds the goto transition from state [s] labelled by
      nonterminal [nt]. Raises [Invalid_argument] if no such transition exists. *)
  val find_goto : 'g grammar -> 'g lr1 index -> 'g nonterminal index -> 'g goto_transition index

  (** [find_goto_target s nt] returns the target state of the goto transition
      from [s] labelled by [nt]. Raises [Invalid_argument] if no such transition exists. *)
  val find_goto_target : 'g grammar -> 'g lr1 index -> 'g nonterminal index -> 'g lr1 index

  (** [source tr] returns the source (origin) state of transition [tr] *)
  val source : 'g grammar -> 'g transition index -> 'g lr1 index

  (** [target tr] returns the target (destination) state of transition [tr] *)
  val target : 'g grammar -> 'g transition index -> 'g lr1 index

  (** [symbol tr] returns the grammar symbol that labels transition [tr] *)
  val symbol : 'g grammar -> 'g transition index -> 'g symbol index

  (** [goto_symbol tr] returns the nonterminal that labels goto transition [tr] *)
  val goto_symbol : 'g grammar -> 'g goto_transition index -> 'g nonterminal index

  (** [shift_symbol tr] returns the terminal that labels shift transition [tr] *)
  val shift_symbol : 'g grammar -> 'g shift_transition index -> 'g terminal index

  (** [successors g s] returns the set of outgoing transitions from state [s] *)
  val successors : 'g grammar -> 'g lr1 index -> 'g transition indexset

  (** [predecessors g s] returns the set of incoming transitions to state [s] *)
  val predecessors : 'g grammar -> 'g lr1 index -> 'g transition indexset

  (** [accepting g] is the set of accepting transitions in grammar [g].
      These are goto transitions from an initial state to an accepting state,
      recognizing completion of a grammar entrypoint. *)
  val accepting : 'g grammar -> 'g goto_transition indexset

  (** Converts a transition to a debug string of the form "source -> target" *)
  val to_string : 'g grammar -> 'g transition index -> string

  (** [find g src tgt] finds the transition from [src] to [tgt], if one exists.
      Returns [None] if there is no direct transition between the two states. *)
  val find : 'g grammar -> 'g lr1 index -> 'g lr1 index -> 'g transition index option
end

(** Reduction information.
    A reduction represents a (state, production, lookahead set) triple,
    meaning that in the given state, when the lookahead terminal is in
    the set, the parser should reduce by the given production. *)
module Reduction : sig
  include INDEXED with type 'g n = 'g reduction

  (** [state r] returns the LR(1) state where reduction [r] applies *)
  val state: 'g grammar -> 'g n index -> 'g lr1 index

  (** [production r] returns the production that reduction [r] reduces by *)
  val production: 'g grammar -> 'g n index -> 'g production index

  (** [lookaheads r] returns the set of lookahead terminals for reduction [r].
      When the next input terminal is in this set, the reduction is triggered. *)
  val lookaheads: 'g grammar -> 'g n index -> 'g terminal indexset

  (** [from_lr1 g s] returns the set of all reductions applicable in LR(1) state [s] *)
  val from_lr1: 'g grammar -> 'g lr1 index -> 'g n indexset
end

val raw : _ grammar -> (module MenhirSdk.Cmly_api.GRAMMAR)
