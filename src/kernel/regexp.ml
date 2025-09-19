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
end = struct
  include Positive
  type t = n index
  type set = n indexset
  type 'a map = (n, 'a) indexmap
  let gensym () =
    let r = ref (-1) in
    fun () -> incr r; Index.of_int n !r
end

(** The RE module type defines the signature for regular expressions, including
    types for reductions, unique IDs to identify sub-terms, and the regular
    expression terms themselves.

    It also includes functions for creating, comparing, and converting regular
    expressions to a Cmon document. *)
module Reductions = struct
  type 'g t = {
    pattern: 'g goto_transition indexset;
    capture: Capture.set;
    usage: Usage.set;
    policy: Syntax.quantifier_kind;
  }

  let compare r1 r2 =
    if r1 == r2 then 0 else
      let c = IndexSet.compare r1.pattern r2.pattern in
      if c <> 0 then c else
        IndexSet.compare r1.capture r2.capture

  let cmon {capture=_; pattern; usage=_; policy} =
    Cmon.record [
      (*"capture", cmon_indexset capture;*)
      "pattern", cmon_set_cardinal (*cmon_indexset*) pattern;
      (*"usage", Usage.cmon_set usage;*)
      "policy", Syntax.cmon_quantifier_kind policy;
    ]
end

module Expr = struct
  (** Integers that serves has unique id to identify sub-terms.
      Thanks to properties of Antimirov's derivatives, no new term is
      introduced during derivation. All terms are produced during initial
      parsing. *)
  type uid = int

  let uid =
    let k = ref 0 in
    fun () -> incr k; !k

  type 'g t = {
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
  let empty = {uid = 0; desc = Alt[]; position = Lexing.dummy_pos}

  (** Introduce a new term, allocating a unique ID *)
  let make position desc =
    {uid = uid (); desc; position}

  (** Compare two terms *)
  let compare t1 t2 =
    Int.compare t1.uid t2.uid

  let cmon ?(lr1=cmon_index) t =
    let rec aux t =
      match t.desc with
      | Set (lr1s, _var, _usage) ->
        Cmon.construct "Set" [
          cmon_indexset ~index:lr1 lr1s;
          (*cmon_indexset var;
            Usage.cmon_set usage;*)
        ]
      | Alt ts -> Cmon.constructor "Alt" (Cmon.list_map aux ts)
      | Seq ts -> Cmon.constructor "Seq" (Cmon.list_map aux ts)
      | Star (t, qk) -> Cmon.construct "Star" [aux t; Syntax.cmon_quantifier_kind qk]
      | Filter lr1s ->
        Cmon.constructor "Filter" (cmon_indexset ~index:lr1 lr1s)
      | Reduce (_var, r) ->
        Cmon.construct "Reduce" [(*cmon_indexset var;*) Reductions.cmon r]
    in
    aux t
end

module Label = struct
  type 'g t = {
    filter: 'g lr1 indexset;
    captures: Capture.set;
    usage: Usage.set;
  }

  let compare l1 l2 =
    if l1 == l2 then 0 else
      let c = IndexSet.compare l1.filter l2.filter in
      if c <> 0 then c else
        IndexSet.compare l1.captures l2.captures

  let filter label filter =
    let filter = IndexSet.inter label.filter filter in
    if IndexSet.is_empty filter then
      None
    else
      Some {label with filter}

  let union l1 l2 = {
    filter = IndexSet.union l1.filter l2.filter;
    captures = IndexSet.union l1.captures l2.captures;
    usage = Usage.join l1.usage l2.usage;
  }

  let capture label vars usage =
    if IndexSet.is_empty vars && Usage.is_empty usage then
      label
    else
      {label with captures = IndexSet.union label.captures vars;
                  usage = Usage.join label.usage usage}
end

module K = struct

  type ('g, 's) t =
    | Accept
    | Done
    | More of 'g Expr.t * ('g, 's) t
    | Reducing of {
        reduction: 'g Reductions.t;
        step: ('g, 's) Redgraph.step index;
        next: ('g, 's) t;
      }

  let cmon ?lr1 k =
    let rec aux = function
    | Accept -> Cmon.constant "Accept"
    | Done -> Cmon.constant "Done"
    | More (e, t) ->
      Cmon.construct "More" [Expr.cmon ?lr1 e; aux t]
    | Reducing {reduction=_; step; next} ->
      Cmon.crecord "Reducing" [
        "reduction", Cmon.constant "...";
        "step", cmon_index step;
        "next", aux next;
      ]
    in
    aux k

  let rec compare t1 t2 =
    if t1 == t2 then 0 else
      match t1, t2 with
      | Accept, Accept -> 0
      | Done, Done -> 0
      | More (e1, t1'), More (e2, t2') ->
        let c = Expr.compare e1 e2 in
        if c <> 0 then c else
          compare t1' t2'
      | Reducing r1, Reducing r2 ->
        let c = Reductions.compare r1.reduction r2.reduction in
        if c <> 0 then c else
          let c = Index.compare r1.step r2.step in
          if c <> 0 then c else
            compare r1.next r2.next
      | Accept, (More _ | Reducing _ | Done) -> -1
      | Done, (More _ | Reducing _) -> -1
      | (More _ | Reducing _ | Done), Accept -> +1
      | (More _ | Reducing _), Done -> +1
      | More _, Reducing _ -> -1
      | Reducing _, More _ -> +1

  let intersecting s1 s2 =
    not (IndexSet.disjoint s1 s2)

  let derive (type g s) (g : g grammar) (rg: (g, s) Redgraph.graph) filter k =
    let continue r label next = match !r with
      | (label', next') :: r' when next' == next ->
        r := (Label.union label' label, next) :: r'
      | r' ->
        r := (label, next) :: r'
    in
    let is_live (reduction : _ Reductions.t) step =
      (step : _ index :> int) > 0
      &&
      let reachable = rg.steps.:(step).reachable in
      IndexSet.exists
        (fun gt -> intersecting reachable rg.targets.:(gt))
        reduction.pattern
    in
    let ks = ref [] in
    let push_reduction_step label reduction next step =
      if is_live reduction step then
        continue ks
          (Label.capture label reduction.capture Usage.empty)
          (Reducing {reduction; step; next})
    in
    let rec process_k label = function
      | Accept ->
        ()

      | Done ->
        continue ks label Accept

      | More (re, next) as self ->
        process_re label self next re.desc

      | Reducing {reduction; step; next} ->
        process_reduction_step label reduction next step

    and process_reduction_step label reduction k step =
      let {Redgraph. goto; next; _} = rg.steps.:(step) in
      Printf.printf "reduction step %d, next %d\n" (step :> int) (next :> int);
      push_reduction_step label reduction k next;
      IndexSet.iter begin fun node ->
        let ndesc, nstep = rg.nodes.:(node) in
        if IndexSet.mem ndesc.lr1 label.filter then (
          Printf.printf "node on state %s, next %d\n" (Lr1.to_string g ndesc.lr1) (nstep :> int);
          let label = {label with filter = IndexSet.singleton ndesc.lr1} in
          if intersecting ndesc.gotos reduction.pattern then
            process_k (Label.capture label IndexSet.empty reduction.usage) k;
          push_reduction_step label reduction k nstep;
        )
      end goto

    and process_re label self next = function
      | Set (s, var, usage) ->
        begin match Label.filter label s with
          | None -> ()
          | Some label ->
            continue ks (Label.capture label var usage) next
        end

      | Alt es ->
        List.iter (fun e -> process_k label (More (e, next))) es

      | Star (r, Shortest) ->
        process_k label next;
        process_k label (More (r, self))

      | Star (r, Longest) ->
        process_k label (More (r, self));
        process_k label next

      | Seq es ->
        process_k label (List.fold_right (fun e k -> More (e, k)) es next)

      | Filter filter ->
        begin match Label.filter label filter with
          | None -> ()
          | Some label' -> process_k label' next
        end

      | Reduce (cap, reduction) ->
        let label =
          Label.capture label
            (IndexSet.union cap reduction.capture)
            Usage.empty
        in
        IndexSet.iter begin fun lr1 ->
          let label = {label with filter = IndexSet.singleton lr1} in
          IndexSet.iter
            (push_reduction_step label reduction next)
            rg.initials.:(lr1)
        end label.filter
    in
    let label = {Label. filter; captures = IndexSet.empty; usage = Usage.empty} in
    process_k label k;
    List.rev !ks
end
