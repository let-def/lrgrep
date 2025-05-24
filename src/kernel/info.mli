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

(** [INDEXED] represents a finite set, essentially extending
    [Fix.Indexing.CARDINAL] with convenient definitions.

    The grammar defines many fixed sets (for terminals, non-terminals,
    productions, LR states, ...). For convenience, we represent each of those
    sets using [Fix.Indexing]. *)
module type INDEXED = sig
  (** This module defines a finite set *)
  include CARDINAL

  (** An element of the set *)
  type t = n index

  (** A subset of elements *)
  type set = n indexset

  (** A partial map from elements to values of type ['a] *)
  type 'a map = (n, 'a) indexmap
end

(** [GRAMMAR_INDEXED] extends [INDEXED] with bijection with
    Menhir's representation. *)
module type GRAMMAR_INDEXED = sig
  include INDEXED

  (** The type of Menhir's representation for elements of this finite set *)
  type raw

  (** Import an element from the grammar representation *)
  val of_g : raw -> t

  (** Export an element to the grammar representation *)
  val to_g : t -> raw
end

type 'g terminal
type 'g nonterminal
type 'g symbol = ('g terminal, 'g nonterminal) Sum.n
type 'g production
type 'g item
type 'g lr0
type 'g lr1

type 'g goto_transition
type 'g shift_transition
type 'g transition = ('g goto_transition, 'g shift_transition) Sum.n

type 'g reduction

module type S = sig
  module Grammar : GRAMMAR
  type g

  module Terminal : sig
    include GRAMMAR_INDEXED with type raw = Grammar.terminal and type n = g terminal
    val to_string : t -> string
    val all : set
    val regular : set

    (** [semantic_value term] is [Some typ] if terminal [term] has a semantic
        value of type [typ], or [None] for unparameterized terminals. *)
    val semantic_value : t -> string option

    (** Wrapper around [IndexSet.inter] speeding-up intersection with [all] *)
    val intersect : set -> set -> set
  end

  module Nonterminal : sig
    include GRAMMAR_INDEXED with type raw = Grammar.nonterminal and type n = g nonterminal
    val to_string : t -> string
    val all : set
    val kind : t -> [`REGULAR | `START]
    val semantic_value : t -> string option
    val nullable : t -> bool
  end

  module Symbol : sig
    include Sum.S with type l := g terminal
                   and type r := g nonterminal
                   and type n = (g terminal, g nonterminal) Sum.n

    type t = n index
    type set = n indexset

    type desc =
      | T of Terminal.t
      | N of Nonterminal.t

    val desc : t -> desc

    val is_terminal : n index -> bool
    val is_nonterminal : n index -> bool

    val of_g : Grammar.symbol -> t
    val to_g : t -> Grammar.symbol

    val name : ?mangled:bool -> t -> string
    val semantic_value : t -> string option

    val all : set
  end

  module Production : sig
    include GRAMMAR_INDEXED with type raw = Grammar.production and type n = g production
    val lhs : t -> Nonterminal.t
    val rhs : t -> Symbol.t array
    val length : t -> int
    val kind : t -> [ `REGULAR | `START ]
    val all : set
  end

  (* Explicit representation of LR(0) items *)
  module Item : sig
    include INDEXED with type n = g item
    val make : Production.t -> int -> t
    val prev : t -> t option
    val desc : t -> Production.t * int
    val position : t -> int
    val production : t -> Production.t
    val is_reducible : t -> bool
    val to_string : t -> string
  end

  module Lr0 : sig
    include GRAMMAR_INDEXED with type raw = Grammar.lr0 and type n = g lr0

    (* See [Lr1.incoming]. *)
    val incoming : t -> Symbol.t option

    (* See [Lr1.items]. *)
    val items : t -> Item.set

    (* If the state is an initial state, returns the pseudo (start)
       production that it recognizes this entrypoint. *)
    val is_entrypoint : t -> Production.t option
  end

  module Lr1 : sig
    include GRAMMAR_INDEXED with type raw = Grammar.lr1 and type n = g lr1
    val all : set
    val accepting : set

    (* A ``wait'' state is an LR(1) state in which the parser needs to look at
       more input before knowing how to proceed.
       Wait states are the initial states and the targets of SHIFT transitions
       (states with a terminal as incoming symbol), except the accepting ones
       (after reading EOF, the only valid action is to reduce). *)
    val wait : set

    (* Get the LR(0) "core" state *)
    val to_lr0 : t -> Lr0.t

    (* The symbol annotating the incoming transitions of a state.
       There is none for initial states, and at most one for others. *)
    val incoming : t -> Symbol.t option

    (* Get the items in the kernel of a state (before closure). *)
    val items : t -> Item.set

    (* Printing functions, for debug purposes.
       Not nice for the end-user (FIXME). *)

    val to_string : t -> string
    val list_to_string : t list -> string
    val set_to_string : set -> string

    val symbol_to_string : t -> string

    (** [shift_on t] is the set of lookaheads that state [t] can shift *)
    val shift_on : t -> Terminal.set

    (** [reduce_on t] is the set of lookaheads that trigger a reduction in state
        [t] *)
    val reduce_on : t -> Terminal.set

    (** [reject t] is set of lookaheads that cause the automaton to fail when in
        state [t] *)
    val reject : t -> Terminal.set

    (** [predecessors t] is the set of LR(1) states that have transition going
        to [t]. *)
    val predecessors : t -> set

    (** Wrapper around [IndexSet.inter] speeding-up intersection with [all] *)
    val intersect : set -> set -> set

    val is_entrypoint : t -> Production.t option
    val entrypoints : (string, t) Hashtbl.t
    val all_entrypoints : set
  end

  module Transition : sig
    (* Abstract types used as index to represent the different sets of
       transitions.
       For instance, [goto] represents the finite set of goto transition:
       - the value [goto : goto cardinal] is the cardinal of this set
       - any value of type [goto index] is a member of this set
         (representing a goto transition)
    *)
    type any = g transition
    type goto = g goto_transition
    type shift = g shift_transition

    (* The set of goto transitions *)
    val goto : goto cardinal
    (* The set of all transitions = goto U shift *)
    val any : any cardinal
    (* The set of shift transitions *)
    val shift : shift cardinal

    (* Building the isomorphism between any and goto U shift *)

    (* Inject goto into any *)
    val of_goto : goto index -> any index

    (* Inject shift into any *)
    val of_shift : shift index -> any index

    (* Project a transition into a goto or a shift transition *)
    val split : any index -> (goto index, shift index) either

    (* [find_goto s nt] finds the goto transition originating from [s] and
       labelled by [nt], or raise [Not_found].  *)
    val find_goto : Lr1.t -> Nonterminal.t -> goto index
    val find_goto_target : Lr1.t -> Nonterminal.t -> Lr1.t

    (* Get the source state of a transition *)
    val source : any index -> Lr1.t

    (* Get the target state of a transition *)
    val target : any index -> Lr1.t

    (* Symbol that labels a transition *)
    val symbol : any index -> Symbol.t

    (* Symbol that labels a goto transition *)
    val goto_symbol : goto index -> Nonterminal.t

    (* Symbol that labels a shift transition *)
    val shift_symbol : shift index -> Terminal.t

    (* [successors s] returns all the transitions [tr] such that
       [source tr = s] *)
    val successors : Lr1.t -> any index list

    (* [predecessors s] returns all the transitions [tr] such that
       [target tr = s] *)
    val predecessors : Lr1.t -> any index list

    (* Accepting transitions are goto transitions from an initial state to an
       accepting state, recognizing one of the grammar entrypoint. *)
    val accepting : goto indexset
  end

  module Reduction : sig
    include INDEXED with type n = g reduction

    (* A reduction is a triple [(lr1, prod, lookaheads)], meaning that:
       in state [lr1], when looking ahead at a terminal in [lookaheads], the
       action is to reduce [prod]. *)

    val state: t -> Lr1.t
    val production: t -> Production.t
    val lookaheads: t -> Terminal.set

    (* All reductions applicable to an lr1 state. *)
    val from_lr1: Lr1.t -> set
  end
end

module Make(Grammar : GRAMMAR) : S with module Grammar = Grammar

type 'g info = (module S with type g = 'g)
