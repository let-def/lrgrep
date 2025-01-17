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
module type RE = sig
  module Info : Info.S
  open Info
  type redstate

  (** Integers that serves has unique id to identify sub-terms.
      Thanks to properties of Antimirov's derivatives, no new term is
      introduced during derivation. All terms are produced during initial
      parsing. *)
  type uid = private int

  type reduction = {
    pattern: redstate indexset;
    capture: Capture.set;
    usage: Usage.set;
    policy: Syntax.quantifier_kind;
  }

  val compare_reduction : reduction -> reduction -> int

  val cmon_reduction : reduction -> Cmon.t

  (** A regular expression term with its unique ID, its description and its
      position. *)
  type t = {
    uid : uid;
    desc : desc;
    position : Syntax.position;
  }

  (** The different constructors of regular expressions*)
  and desc =
    | Set of Lr1.set * Capture.set * Usage.set
    (** Recognise a set of states, and optionally bind the matching state to
        a variable. *)
    | Alt of t list
    (** [Alt ts] is the disjunction of sub-terms [ts] (length >= 2).
        [Alt []] represents the empty language. *)
    | Seq of t list
    (** [Seq ts] is the concatenation of sub-terms [ts] (length >= 2).
        [Seq []] represents the {ε}. *)
    | Star of t * Syntax.quantifier_kind
    (** [Star t] is represents the Kleene star of [t] *)
    | Filter of Lr1.set
    | Reduce of Capture.set * reduction
    (** The reduction operator *)

  (** Introduce a new term, allocating a unique ID *)
  val make : Syntax.position -> desc -> t

  (** Compare two terms *)
  val compare : t -> t -> int

  (** Print a term to a [Cmon] document. [var] arguments allow to customize
      printing of variables. *)
  val cmon : t -> Cmon.t
end

(** The S module type defines the signature of the internal regex language,
    which includes [Redgraph] for representing reductions, and [RE] for the
    surface language, and [K] module for representing intermediate continuations
    during derivation.
*)
module type S = sig
  module Info : Info.S
  open Info

  module Redgraph : Viable_reductions.S with module Info := Info

  module RE : RE
    with module Info := Info
     and type redstate := Redgraph.n

  module K : sig
    type label = {
      filter: Lr1.set;
      captures: Capture.set;
      usage: Usage.set;
    }

    val is_immediate_label : label -> bool

    val compare_label : label -> label -> int

    type t =
      | Done
      | More of RE.t * t
      | Reducing of {
          reduction: RE.reduction;
          transitions: Redgraph.outer_transitions;
          next: t;
        }
    val compare : t -> t -> int
    val cmon : t -> Cmon.t

    (* TODO
       There is no way to detect immediate match (empty (sub-)regex leading
       immediately to Done).
       The case is handled by looking for [(empty_label, Done)] in the result
       list, where [empty_label] is a label capturing and filtering nothing
       (vars and ordering are empty, filter = Lr1.all).

       However, this does not allow to distinguish between the regular
       expressions ϵ and _ which will both lead to "Done" while matching no
       transitions.
    *)
    val derive : Lr1.set -> t -> (label * t option) list
  end
end

module Make
    (Info : Info.S)
    (Redgraph : Viable_reductions.S with module Info := Info)
  : S with module Info = Info =
struct
  module Info = Info
  module Redgraph = Redgraph
  open Info


  (* [RE]: Syntax for regular expression extended with reduction operator *)
  module RE : RE with module Info := Info
                  and type redstate := Redgraph.n =
  struct
    type uid = int

    let uid =
      let k = ref 0 in
      fun () -> incr k; !k

    type reduction = {
      pattern: Redgraph.n indexset;
      capture: Capture.set;
      usage: Usage.set;
      policy: Syntax.quantifier_kind;
    }

    let compare_reduction r1 r2 =
      if r1 == r2 then 0 else
        let c = IndexSet.compare r1.pattern r2.pattern in
        if c <> 0 then c else
          IndexSet.compare r1.capture r2.capture

    type t = {
      uid : uid;
      desc : desc;
      position : Syntax.position;
    }
    and desc =
      | Set of Lr1.set * Capture.set * Usage.set
      | Alt of t list
      | Seq of t list
      | Star of t * Syntax.quantifier_kind
      | Filter of Lr1.set
      | Reduce of Capture.set * reduction

    let make position desc = {uid = uid (); desc; position}

    let compare t1 t2 =
      Int.compare t1.uid t2.uid

    let cmon_usage_set _ = Cmon.constant "<Usage.set>"

    let cmon_reduction {capture; pattern; usage; policy} =
      Cmon.record [
        "capture", cmon_indexset capture;
        "pattern", cmon_set_cardinal (*cmon_indexset*) pattern;
        "usage", cmon_usage_set usage;
        "policy", Syntax.cmon_quantifier_kind policy;
      ]

    let cmon t =
      let rec aux t =
        match t.desc with
        | Set (lr1s, var, usage) ->
          Cmon.construct "Set" [
            cmon_set_cardinal lr1s;
            cmon_indexset var;
            cmon_usage_set usage;
          ]
        | Alt ts -> Cmon.constructor "Alt" (Cmon.list_map aux ts)
        | Seq ts -> Cmon.constructor "Seq" (Cmon.list_map aux ts)
        | Star (t, qk) -> Cmon.construct "Star" [aux t; Syntax.cmon_quantifier_kind qk]
        | Filter lr1s ->
          Cmon.constructor "Filter" (cmon_set_cardinal lr1s)
        | Reduce (var, r) ->
          Cmon.construct "Reduce" [cmon_indexset var; cmon_reduction r]
      in
      aux t
  end

  module K = struct
    type label = {
      filter: Lr1.set;
      captures: Capture.set;
      usage: Usage.set;
    }

    let is_immediate_label {filter; captures; usage=_} =
      IndexSet.equal filter Lr1.all &&
      IndexSet.is_empty captures

    let compare_label l1 l2 =
      if l1 == l2 then 0 else
        let c = IndexSet.compare l1.filter l2.filter in
        if c <> 0 then c else
          IndexSet.compare l1.captures l2.captures

    type t =
      | Done
      | More of RE.t * t
      | Reducing of {
          reduction: RE.reduction;
          transitions: Redgraph.outer_transitions;
          next: t;
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
      let c = compare_index c1.Redgraph.target c2.Redgraph.target in
      if c <> 0 then c else
        let c = IndexSet.compare c1.filter c2.filter in
        if c <> 0 then c else
          IndexSet.compare c1.lookahead c2.lookahead

    let compare_reduction_step r1 r2 =
      let c = IndexSet.compare r1.Redgraph.reachable r2.Redgraph.reachable in
      if c <> 0 then c else
        list_compare compare_outer_candidate r1.candidates r2.candidates

    let rec compare t1 t2 =
      if t1 == t2 then 0 else
        match t1, t2 with
        | Done, Done -> 0
        | More (e1, t1'), More (e2, t2') ->
          let c = RE.compare e1 e2 in
          if c <> 0 then c else
            compare t1' t2'
        | Reducing r1, Reducing r2 ->
          let c = RE.compare_reduction r1.reduction r2.reduction in
          if c <> 0 then c else
            let c = list_compare compare_reduction_step r1.transitions r2.transitions in
            if c <> 0 then c else
              compare r1.next r2.next
        | Done, (More _ | Reducing _) -> -1
        | (More _ | Reducing _), Done -> +1
        | More _, Reducing _ -> -1
        | Reducing _, More _ -> +1

    let cmon_candidate ?lookahead:lookahead' ~filter:cmon_filter
      {Redgraph. target; lookahead; filter; reduction=_}
      =
      Cmon.record [
        "target"  , cmon_index target;
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
        "filter"     , cmon_filter filter;
      ]

    let cmon_outer_candidate ?lookahead c =
      cmon_candidate ?lookahead c
        ~filter:(fun filter ->
            if IndexSet.cardinal filter >= 10 then cmon_set_cardinal filter else
              cmon_indexset filter
                ~index:(fun lr1 -> Cmon.constant (Lr1.to_string lr1));
          )

    let cmon_transitions ~candidate trs =
      trs |> Cmon.list_map @@ fun {Redgraph. reachable; candidates} ->
      Cmon.record [
        "reachable", cmon_set_cardinal reachable;
        "candidates", Cmon.list_map candidate candidates;
      ]

    (*let cmon_all_transitions {Redgraph. inner; outer} =
      Cmon.record [
        "inner", cmon_transitions ~candidate:(cmon_candidate ~filter:(fun () -> Cmon.unit)) inner;
        "outer", cmon_transitions ~candidate:cmon_outer_candidate outer;
      ]*)

    let rec cmon = function
      | Done ->
        Cmon.constant "Done"
      | More (re, next) ->
        Cmon.construct "More" [RE.cmon re; cmon next]
      | Reducing {reduction; transitions; next} ->
        Cmon.crecord "Reducing" [
          "reduction"   , RE.cmon_reduction reduction;
          "transitions" , cmon_transitions ~candidate:cmon_outer_candidate transitions;
          "next"        , cmon next;
        ]

    let not_empty s1 =
      not (IndexSet.is_empty s1)

    let intersecting s1 s2 =
      not (IndexSet.disjoint s1 s2)

    let label_filter label filter =
      let filter = Lr1.intersect label.filter filter in
      if not_empty filter then
        Some {label with filter}
      else
        None

    let label_union l1 l2 = {
      filter = IndexSet.union l1.filter l2.filter;
      captures = IndexSet.union l1.captures l2.captures;
      usage = Usage.join l1.usage l2.usage;
    }

    let label_capture label vars usage =
      if not_empty vars || not (Usage.is_empty usage) then
        {label with captures = IndexSet.union label.captures vars;
                    usage = Usage.join label.usage usage}
      else
        label

    let live_redstep (red : RE.reduction) (step : _ Redgraph.reduction_step) =
      intersecting red.pattern step.reachable

    let live_redstate (red : RE.reduction) (state : Redgraph.n index) =
      intersecting red.pattern (Redgraph.reachable state)

    let rec reduce_target ~on_outer r target =
      (live_redstate r target &&
       reduce_inner_transitions ~on_outer r
         (Redgraph.get_transitions target))
      || IndexSet.mem target r.pattern

    and reduce_inner_transitions ~on_outer r {Redgraph. inner; outer} =
      let matched = ref false in
      let visit_candidate (c : unit Redgraph.goto_candidate) =
        if reduce_target ~on_outer r c.target then
          matched := true
      in
      let rec loop = function
        | step :: xs when live_redstep r step ->
          List.iter visit_candidate step.candidates;
          loop xs
        | _ -> ()
      in
      loop inner;
      if outer <> [] then
        on_outer outer;
      !matched

    let derive filter k =
      let accept r label = match !r with
        | (label', None) :: r' ->
          r := (label_union label' label, None) :: r'
        | r' ->
          r := (label, None) :: r'
      in
      let continue r label next = match !r with
        | (label', (Some next' as k)) :: r'  when next' == next ->
          r := (label_union label' label, k) :: r'
        | r' ->
          r := (label, Some next) :: r'
      in
      (* FIXME:
         "can_succeed_next" is a workaround for a limitation of dataflow analysis.
         It is used to place capture the end location of a reduction if is
         likely to succeed at the next transition.
         It is correct to always capture the location, however the dataflow
         analysis does a bad job (!) of eliminating useless captures, which
         lead to a bigger than necessary automaton. can_succeed_next is used as
         a heuristic to not place unnecessary captures.

         The proper solution is to improve the dataflow analysis, which will
         lead to smaller automata in general and allows us to get rid of this
         heuristic.
       *)
      let can_succeed_next (red : RE.reduction) = function
        | (step : Lr1.set Redgraph.reduction_step) :: _ ->
          List.exists (fun cand -> IndexSet.mem cand.Redgraph.target red.pattern)
            step.candidates
        | [] -> false
      in
      let reduce_outer matching ks next label reduction transitions =
        let rec visit_transitions label reduction = function
          | step :: transitions when live_redstep reduction step ->
            List.iter (visit_candidate label) step.candidates;
            begin match transitions with
              | step' :: _ when live_redstep reduction step' ->
                let reducing = Reducing {reduction; transitions; next} in
                push ks (label, Some reducing)
              | _ -> ()
            end
          | _ -> ()
        and visit_candidate label (candidate : Lr1.set Redgraph.goto_candidate) =
          match label_filter label candidate.filter with
          | Some label
            when reduce_target reduction candidate.target
                ~on_outer:(visit_transitions label reduction) ->
            matching := IndexSet.union label.filter !matching
          | _ -> ()
        in
        visit_transitions
          (if can_succeed_next reduction transitions
           then label_capture label reduction.capture Usage.empty
           else label)
          reduction transitions
      in
      let ks = ref [] in
      let rec process_k label = function
        | Done ->
          accept ks label

        | More (re, next) as self ->
          process_re label self next re.desc

        | Reducing {reduction; transitions; next} ->
          let l' = ref IndexSet.empty in
          let ks' = ref [] in
          reduce_outer l' ks' next label reduction transitions;
          match label_filter label !l', reduction.policy with
          | None, _ ->
            ks := !ks' @ !ks
          | Some label, Longest ->
            ks := !ks' @ !ks;
            process_k (label_capture label IndexSet.empty reduction.usage) next
          | Some label, Shortest ->
            process_k (label_capture label IndexSet.empty reduction.usage) next;
            ks := !ks' @ !ks

      and process_re label self next = function
        | Set (s, var, usage) ->
          begin match label_filter label s with
            | None -> ()
            | Some label ->
              continue ks (label_capture label var usage) next
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
          begin match label_filter label filter with
            | None -> ()
            | Some label' -> process_k label' next
          end

        | Reduce (cap, reduction) ->
          let label =
            label_capture label
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
                (Vector.get Redgraph.initial lr1)
            ) label.filter;
          let label =
            label_filter
              (label_capture label IndexSet.empty reduction.usage)
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
      let label = {filter; captures = IndexSet.empty; usage = Usage.empty} in
      process_k label k;
      List.rev !ks
  end
end
