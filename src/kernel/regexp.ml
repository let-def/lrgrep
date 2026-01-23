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
    pattern: 'g Redgraph.target indexset;
    capture: Capture.set;
    policy: Syntax.quantifier_kind;
  }

  let compare r1 r2 =
    if r1 == r2 then 0 else
      let c = IndexSet.compare r1.pattern r2.pattern in
      if c <> 0 then c else
        let c = IndexSet.compare r1.capture r2.capture in
        c

  let cmon {capture=_; pattern; policy} =
    Cmon.record [
      (*"capture", cmon_indexset capture;*)
      "pattern_domain", cmon_set_cardinal (*cmon_indexset*) pattern;
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
    | Set of 'g lr1 indexset * Capture.set
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
    | Usage of Usage.set

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
      | Set (lr1s, _var) ->
        Cmon.construct "Set" [cmon_indexset ~index:lr1 lr1s]
      | Alt ts -> Cmon.constructor "Alt" (Cmon.list_map aux ts)
      | Seq ts -> Cmon.constructor "Seq" (Cmon.list_map aux ts)
      | Star (t, qk) -> Cmon.construct "Star" [aux t; Syntax.cmon_quantifier_kind qk]
      | Filter lr1s ->
        Cmon.constructor "Filter" (cmon_indexset ~index:lr1 lr1s)
      | Reduce (_var, r) ->
        Cmon.construct "Reduce" [(*cmon_indexset var;*) Reductions.cmon r]
      | Usage _ ->
        Cmon.constant "Usage"
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

  type 'g t =
    | Accept
    | Done
    | More of 'g Expr.t * 'g t
    | Reducing of {
        reduction: 'g Reductions.t;
        steps: 'g Redgraph.step indexset;
        next: 'g t;
      }

  let cmon ?lr1 ?step k =
    let rec aux = function
    | Accept -> Cmon.constant "Accept"
    | Done -> Cmon.constant "Done"
    | More (e, t) ->
      Cmon.construct "More" [Expr.cmon ?lr1 e; aux t]
    | Reducing {reduction=_; steps; next} ->
      Cmon.crecord "Reducing" [
        "reduction", Cmon.constant "...";
        "steps", cmon_indexset ?index:step steps;
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
          let c = IndexSet.compare r1.steps r2.steps in
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

  let derive (type g) (_g : g grammar) (rg: g Redgraph.graph) filter k =
    let continue r label next = match !r with
      | (label', next') :: r' when next' == next ->
        r := (Label.union label' label, next) :: r'
      | r' ->
        r := (label, next) :: r'
    in
    let ks = ref [] in
    let rec process_reduction_step matching next_steps filter (reduction : _ Reductions.t) step =
      match Redgraph.follow rg step with
      | Advance step' ->
        next_steps := IndexMap.update step' (union_update filter) !next_steps
      | Switch map ->
        let matching' = ref IndexSet.empty in
        IndexMap.rev_iter begin fun (lr1, trs) ->
          if IndexSet.mem lr1 filter then (
            let has_match = ref false in
            List.iter begin fun (tr : _ Redgraph.transition) ->
              if not !has_match then
                has_match := intersecting tr.reached reduction.pattern;
              if intersecting tr.reachable reduction.pattern then begin
                (*let reach = IndexSet.inter reachable reduction.pattern in
                  Printf.eprintf "continuing to step %d on %s because targets %s are reachable\n"
                  (step : _ index :> int) (Lr1.to_string g lr1)
                  (string_of_indexset reach)
                  ;*)
                process_reduction_step matching next_steps (IndexSet.singleton lr1) reduction tr.step
              end
            end trs;
            if !has_match then
              matching' := IndexSet.add lr1 !matching';
          )
        end map;
        matching := IndexSet.union !matching' !matching
    in
    let rec process_k label = function
      | Accept ->
        ()

      | Done ->
        continue ks label Accept

      | More (re, next) as self ->
        process_re label self next re.desc

      | Reducing {reduction; steps; next} ->
        let filter0 = label.filter in
        let matching = ref IndexSet.empty in
        let next_steps = ref IndexMap.empty in
        let f = process_reduction_step matching next_steps label.filter reduction in
        IndexSet.iter f steps;
        let push_matching () =
          if IndexSet.is_not_empty !matching then (
            let label = {label with filter = !matching} in
            process_k label next
          )
        in
        let push_steps () =
          let label = Label.capture label reduction.capture Usage.empty in
          let next_steps =
            !next_steps
            |> IndexMap.bindings
            |> List.map (fun (a, b) -> (b, a))
            |> IndexRefine.annotated_partition
          in
          List.iter (fun (filter, steps) ->
              assert (IndexSet.subset filter filter0);
              let steps = IndexSet.of_list steps in
              continue ks {label with filter}
                (Reducing {reduction; steps; next});
            ) next_steps;
        in
        begin match reduction.policy with
          | Shortest ->
            push_matching ();
            push_steps ()
          | Longest ->
            push_steps ();
            push_matching ()
        end

    and process_re label self next = function
      | Set (s, var) ->
        begin match Label.filter label s with
          | None -> ()
          | Some label ->
            continue ks (Label.capture label var Usage.empty) next
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
        let next_steps = ref [] in
        IndexSet.iter begin fun lr1 ->
          let steps =
            List.fold_right begin fun (tr : _ Redgraph.transition) steps ->
              if intersecting tr.reachable reduction.pattern
              then IndexSet.add tr.step steps
              else steps
            end (Redgraph.initial rg lr1) IndexSet.empty
          in
          if IndexSet.is_not_empty steps then
            push next_steps (steps, lr1);
        end label.filter;
        let next_steps = IndexRefine.annotated_partition !next_steps in
        List.iter (fun (steps, filter) ->
            let filter = IndexSet.of_list filter in
            continue ks {label with filter} (Reducing {reduction; steps; next})
          ) next_steps;

        | Usage set ->
          let label = Label.capture label IndexSet.empty set in
          process_k label next
    in
    let label = {Label. filter; captures = IndexSet.empty; usage = Usage.empty} in
    process_k label k;
    List.rev !ks
end
