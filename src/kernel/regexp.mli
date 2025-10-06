(* The MIT License (MIT)

   Copyright (c) 2025 Frédéric Bour

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
*)

(** This module implements the regular expressions used by LRGrep.
    It provides functions for creating, comparing, and deriving regular
    expressions and continuations, which appear during the derivation process.
    It is parameterized by the `Info` and `Redgraph` modules, which are used to
    provide information about the LR automaton and its (viable) reductions,
    respectively. *)
open Fix.Indexing
open Utils
open Misc
open Info

(** The Capture module defines types and functions for representing variables
    captured in regular expressions.
    It uses an index type to uniquely identify a capture in an expression. *)
module Capture : sig
  type n
  type t = n index
  type set = n indexset
  type 'a map = (n, 'a) indexmap

  (* The gensym is instantiated separately for each expression *)
  val gensym : unit -> unit -> n index
end

(** The RE module type defines the signature for regular expressions, including
    types for reductions, unique IDs to identify sub-terms, and the regular
    expression terms themselves.

    It also includes functions for creating, comparing, and converting regular
    expressions to a Cmon document. *)
module Reductions : sig
  type 'g t = {
    pattern: ('g, 'g lr1) Redgraph.node indexset;
    capture: Capture.set;
    usage: Usage.set;
    policy: Syntax.quantifier_kind;
  }

  val compare : 'g t -> 'g t -> int

  val cmon : _ t -> Cmon.t
end

module Expr : sig
  (** Integers that serves has unique id to identify sub-terms.
      Thanks to properties of Antimirov's derivatives, no new term is
      introduced during derivation. All terms are produced during initial
      parsing. *)
  type uid = private int

  type 'g t = private {
    uid : uid;
    desc : 'g desc;
    position : Syntax.position;
  }

  (** The different constructors of regular expressions*)
  and 'g desc =
    | Set of 'g lr1 indexset * Capture.set * Usage.set
    (** Recognise a set of states, and optionally bind the matching state to
        a variable. *)
    | Alt of 'g t list
    (** [Alt ts] is the disjunction of sub-terms [ts] (length >= 2).
        [Alt []] represents the empty language. *)
    | Seq of 'g t list
    (** [Seq ts] is the concatenation of sub-terms [ts] (length >= 2).
        [Seq []] represents the {ε}. *)
    | Star of 'g t * Syntax.quantifier_kind
    (** [Star t] is represents the Kleene star of [t] *)
    | Filter of 'g lr1 indexset
    | Reduce of Capture.set * 'g Reductions.t
    (** The reduction operator *)

  (** A regular expression term with its unique ID, its description and its
      position. *)
  val empty : _ t

  (** Introduce a new term, allocating a unique ID *)
  val make : Syntax.position -> 'g desc -> 'g t

  (** Compare two terms *)
  val compare : 'g t -> 'g t -> int

  (** Print a term to a [Cmon] document. [var] arguments allow to customize
      printing of variables. *)
  val cmon : ?lr1:('g lr1 index -> Cmon.t) -> 'g t -> Cmon.t
end

module Label : sig
  type 'g t = {
    filter: 'g lr1 indexset;
    captures: Capture.set;
    usage: Usage.set;
  }
  val compare : 'lr1 t -> 'lr1 t -> int
end

module K : sig
  type ('g, 's) t =
    | Accept
    | Done
    | More of 'g Expr.t * ('g, 's) t
    | Reducing of {
        reduction: 'g Reductions.t;
        steps: ('g, 's) Redgraph.step indexset;
        next: ('g, 's) t;
      }

  val compare : ('g, 's) t -> ('g, 's) t -> int

  val cmon : ?lr1:('g lr1 index -> Cmon.t) -> ('g, 's) t -> Cmon.t

  val derive : 'g grammar -> ('g, 'g lr1) Redgraph.graph -> 'g lr1 indexset -> ('g, 'g lr1) t -> ('g Label.t * ('g, 'g lr1) t) list
end
