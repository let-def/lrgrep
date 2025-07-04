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
    pattern: 'g Viable_reductions.viable indexset;
    capture: Capture.set;
    usage: Usage.set;
    policy: Syntax.quantifier_kind;
  }

  let compare r1 r2 =
    if r1 == r2 then 0 else
      let c = IndexSet.compare r1.pattern r2.pattern in
      if c <> 0 then c else
        IndexSet.compare r1.capture r2.capture

  let cmon {capture; pattern; usage; policy} =
    Cmon.record [
      "capture", cmon_indexset capture;
      "pattern", cmon_set_cardinal (*cmon_indexset*) pattern;
      "usage", Usage.cmon_set usage;
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

  let cmon t =
    let rec aux t =
      match t.desc with
      | Set (lr1s, var, usage) ->
        Cmon.construct "Set" [
          cmon_set_cardinal lr1s;
          cmon_indexset var;
          Usage.cmon_set usage;
        ]
      | Alt ts -> Cmon.constructor "Alt" (Cmon.list_map aux ts)
      | Seq ts -> Cmon.constructor "Seq" (Cmon.list_map aux ts)
      | Star (t, qk) -> Cmon.construct "Star" [aux t; Syntax.cmon_quantifier_kind qk]
      | Filter lr1s ->
        Cmon.constructor "Filter" (cmon_set_cardinal lr1s)
      | Reduce (var, r) ->
        Cmon.construct "Reduce" [cmon_indexset var; Reductions.cmon r]
    in
    aux t
end

module Label = struct
  type 'g t = {
    filter: 'g lr1 indexset;
    captures: Capture.set;
    usage: Usage.set;
  }

  (*let is_immediate {filter; captures; usage=_} =
    IndexSet.equal filter Lr1.all &&
    IndexSet.is_empty captures *)

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

open Viable_reductions

module K = struct

  type 'g t =
    | Accept
    | Done
    | More of 'g Expr.t * 'g t
    | Reducing of {
        reduction: 'g Reductions.t;
        transitions: 'g Viable_reductions.outer_transitions;
        next: 'g t;
      }

  let rec list_compare f xxs yys =
    if xxs == yys then 0 else
      match xxs, yys with
      | [], _  -> -1
      | _ , [] -> +1
      | (x :: xs), (y :: ys) ->
        let c = f x y in
        if c <> 0 then c else
          list_compare f xs ys

  let compare_outer_candidate c1 c2 =
    let c = compare_index c1.target c2.target in
    if c <> 0 then c else
      let c = IndexSet.compare c1.source c2.source in
      if c <> 0 then c else
        IndexSet.compare c1.lookahead c2.lookahead

  let compare_reduction_step r1 r2 =
    let c = IndexSet.compare r1.reachable r2.reachable in
    if c <> 0 then c else
      list_compare compare_outer_candidate r1.goto_transitions r1.goto_transitions

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
          let c = list_compare compare_reduction_step r1.transitions r2.transitions in
          if c <> 0 then c else
            compare r1.next r2.next
      | Accept, (More _ | Reducing _ | Done) -> -1
      | Done, (More _ | Reducing _) -> -1
      | (More _ | Reducing _ | Done), Accept -> +1
      | (More _ | Reducing _), Done -> +1
      | More _, Reducing _ -> -1
      | Reducing _, More _ -> +1

  let cmon_goto_transition ?lookahead:lookahead' ~source:cmon_source
      {target; lookahead; source; reduction=_}
    =
    Cmon.record [
      "target"    , cmon_index target;
      "lookahead" , (
        match lookahead' with
        | None -> cmon_set_cardinal lookahead
        | Some lookahead' ->
          Cmon.constant (
            Printf.sprintf "{%d elements} (%d matching current reduction)"
              (IndexSet.cardinal lookahead)
              (IndexSet.cardinal (IndexSet.inter lookahead lookahead'))
          )
      );
      "source"    , cmon_source source;
    ]

  let cmon_outer_goto_transition ?(lr1=string_of_index) ?lookahead c =
    cmon_goto_transition ?lookahead c
      ~source:(fun source ->
          if IndexSet.cardinal source >= 10 then cmon_set_cardinal source else
            cmon_indexset source
              ~index:(fun ilr1 -> Cmon.constant (lr1 ilr1));
        )

  let cmon_transitions ~goto_transition trs =
    Cmon.list_map begin fun {reachable; goto_transitions} ->
      Cmon.record [
        "reachable", cmon_set_cardinal reachable;
        "goto_transitions", Cmon.list_map goto_transition goto_transitions;
      ]
    end trs

  let rec cmon ?lr1 = function
    | Accept ->
      Cmon.constant "Accept"
    | Done ->
      Cmon.constant "Done"
    | More (re, next) ->
      Cmon.construct "More" [Expr.cmon re; cmon next]
    | Reducing {reduction; transitions; next} ->
      let goto_transition = cmon_outer_goto_transition ?lr1 in
      Cmon.crecord "Reducing" [
        "reduction"   , Reductions.cmon reduction;
        "transitions" , cmon_transitions ~goto_transition transitions;
        "next"        , cmon next;
      ]

  let intersecting s1 s2 =
    not (IndexSet.disjoint s1 s2)

  let live_redstep (red : _ Reductions.t) (step : _ Viable_reductions.reduction_step) =
    intersecting red.pattern step.reachable

  let live_redstate viable (red : _ Reductions.t) (state : _ index) =
    intersecting red.pattern viable.reachable_from.:(state)

  let rec reduce_target viable ~on_outer r target =
    (live_redstate viable r target &&
     reduce_inner_transitions viable ~on_outer r
       viable.transitions.:(target))
    || IndexSet.mem target r.pattern

  and reduce_inner_transitions viable ~on_outer r {Viable_reductions. inner; outer} =
    let matched = ref false in
    let visit_candidate (c : ('g, unit) Viable_reductions.goto_transition) =
      if reduce_target viable ~on_outer r c.target then
        matched := true
    in
    let rec loop = function
      | step :: xs when live_redstep r step ->
        List.iter visit_candidate step.goto_transitions;
        loop xs
      | _ -> ()
    in
    loop inner;
    if outer <> [] then
      on_outer outer;
    !matched

  let derive viable filter k =
    let continue r label next = match !r with
      | (label', next') :: r' when next' == next ->
        r := (Label.union label' label, next) :: r'
      | r' ->
        r := (label, next) :: r'
    in
    let reduce_outer matching ks next label reduction transitions =
      let rec visit_transitions label reduction = function
        | step :: transitions when live_redstep reduction step ->
          List.iter (visit_candidate label) step.goto_transitions;
          begin match transitions with
            | step' :: _ when live_redstep reduction step' ->
              let reducing = Reducing {reduction; transitions; next} in
              push ks (label, reducing)
            | _ -> ()
          end
        | _ -> ()
      and visit_candidate label (candidate : _ Viable_reductions.goto_transition) =
        match Label.filter label candidate.source with
        | Some label
          when reduce_target viable reduction candidate.target
              ~on_outer:(visit_transitions label reduction) ->
          matching := IndexSet.union label.filter !matching
        | _ -> ()
      in
      visit_transitions
        (Label.capture label reduction.capture Usage.empty)
        reduction transitions
    in
    let ks = ref [] in
    let rec process_k label = function
      | Accept ->
        ()

      | Done ->
        continue ks label Accept

      | More (re, next) as self ->
        process_re label self next re.desc

      | Reducing {reduction; transitions; next} ->
        let l' = ref IndexSet.empty in
        let ks' : ('a Label.t * 'a t) list ref = ref [] in
        reduce_outer l' ks' next label reduction transitions;
        match Label.filter label !l', reduction.policy with
        | None, _ ->
          ks := !ks' @ !ks
        | Some label, Longest ->
          ks := !ks' @ !ks;
          process_k (Label.capture label IndexSet.empty reduction.usage) next
        | Some label, Shortest ->
          process_k (Label.capture label IndexSet.empty reduction.usage) next;
          ks := !ks' @ !ks

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
        let ks' = ref [] in
        let matching = ref IndexSet.empty in
        IndexSet.iter (fun lr1 ->
            reduce_outer matching ks'
              next
              {label with filter = IndexSet.singleton lr1}
              reduction
              viable.initial.:(lr1)
          ) label.filter;
        let label =
          Label.filter
            (Label.capture label IndexSet.empty reduction.usage)
            !matching
        in
        begin match reduction.policy with
          | Shortest ->
            Option.iter (fun label -> process_k label next) label;
            ks := !ks' @ !ks;
          | Longest ->
            ks := !ks' @ !ks;
            Option.iter (fun label -> process_k label next) label;
        end
    in
    let label = {Label. filter; captures = IndexSet.empty; usage = Usage.empty} in
    process_k label k;
    List.rev !ks
end
