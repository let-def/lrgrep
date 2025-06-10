(*
 * MIT License
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

(** This module is responsible for building a representation of the reachable
    reductions of an LR automaton, by refining the viable reductions with the
    reachability analysis.
    It includes functionality to construct state machines (NFA and DFA) suitable
    for static analyses, and use them to perform coverage checks, and to
    generate sentences that lead to specific states or failures.  *)

open Utils
open Misc
open Fix.Indexing
open Info

(* Flag to control the inclusion of expensive assertions for debugging *)
let build_with_expensive_assertions = false

(* Flag to control the safety checks *)
let build_safe = true

(* Graphviz style for visualizing graphs *)
let style = "\
    bgcolor=black;\
    color=white;\
    fontcolor=white;\
    node [color=white fontcolor=white shape=rectangle];\
    edge [color=white fontcolor=white];\
    compound=true;\
  "

type 'g source = ('g Viable_reductions.viable, 'g lr1) Sum.n

type 'g config = {
  source: 'g source index;
  lrcs: 'g Lrc.n indexset;
}

(* Signature of reachable reductions modules *)
module type S = sig
  type g

  include CARDINAL

  type target = n index * g reduction index

  type transitions = target list list

  type desc = {
    config: g config;
    transitions: transitions;
  }

  val initial : (g Lrc.n, n index) indexmap
  val states : (n, desc) vector

  val reject : n index -> g terminal indexset
  val rejectable : n index -> g terminal indexset * (n index * g terminal indexset) list

  val successors : (n, n indexset) vector
  val predecessors : (n, n indexset) vector

  val reductions : n index -> n index -> g reduction indexset

  val iter_targets : transitions -> (target -> unit) -> unit
  val rev_iter_targets : transitions -> (target -> unit) -> unit
  val fold_targets : ('a -> target -> 'a) -> 'a -> transitions -> 'a
  val rev_fold_targets : (target -> 'a -> 'a) -> transitions -> 'a -> 'a
end

(* Functor to create an instance of reachable reductions *)
module Make
    (X: sig
       type g
       val g : g grammar
       val viable : g Viable_reductions.t
       val lrc_graph : g Lrc.t
       val entrypoints : g Lrc.entrypoints
     end)
    () : S with type g = X.g
=
struct
  open X
  type g = X.g

  include IndexBuffer.Gen.Make()

  module Source = struct
    let viable = Vector.length viable.reachable_from
    let inj_l = Sum.inj_l
    let inj_r x = Sum.inj_r viable x
    let prj x = Sum.prj viable x
  end

  type target = n index * g reduction index

  type transitions = target list list

  type desc = {
    config: g config;
    transitions: transitions;
  }

  let states = get_generator ()

  (* Hashtable to store nodes and their configurations *)
  let nodes = Hashtbl.create 7

  (* Recursive function to visit a configuration and build the state machine *)
  let rec visit_config lrcs source =
    let config = {source; lrcs} in
    match Hashtbl.find_opt nodes config with
    | Some state -> state
    | None ->
      let reservation = IndexBuffer.Gen.reserve states in
      let index = IndexBuffer.Gen.index reservation in
      Hashtbl.add nodes config index;
      let source_transitions =
        match Source.prj source with
        | L v -> viable.transitions.:(v)
        | R lr1 -> {Viable_reductions.inner=[]; outer=viable.initial.:(lr1)}
      in
      let transitions = visit_transitions lrcs source_transitions in
      IndexBuffer.Gen.commit states reservation {config; transitions};
      index

  (* Visit transitions from a given configuration *)
  and visit_transitions lrcs {Viable_reductions. inner; outer} =
    let inner =
      List.fold_left (fun acc {Viable_reductions. goto_transitions; _} ->
          List.fold_left
            (fun acc {Viable_reductions. target; lookahead=_; source=(); reduction} ->
               let state = visit_config lrcs (Source.inj_l target) in
               (state, reduction) :: acc)
            acc goto_transitions
        ) [] inner
    in
    match visit_outer lrcs outer with
    | [] -> [inner]
    | x :: xs -> (inner @ x) :: xs

  (* Visit outer transitions with lookahead restrictions *)
  and visit_outer lrcs = function
    | [] -> []
    | {Viable_reductions. goto_transitions; _} :: rest ->
      let visit_goto_transition
          {Viable_reductions. target; lookahead=_; source=lr1s; reduction} =
        let compatible_lrc lr1 = IndexSet.inter lrcs lrc_graph.lrcs_of.:(lr1) in
        let lrcs = indexset_bind lr1s compatible_lrc in
        if IndexSet.is_empty lrcs
        then None
        else Some (visit_config lrcs (Source.inj_l target), reduction)
      in
      let candidates = List.filter_map visit_goto_transition goto_transitions in
      let rest =
        match rest with
        | [] -> []
        | rest -> visit_outer (indexset_bind lrcs (Vector.get entrypoints.predecessors)) rest
      in
      candidates :: rest

  (* Initial configurations for the state machine *)
  let initial =
    let process lrc =
      let lr1 = lrc_graph.lr1_of.:(lrc) in
      assert (not (IndexSet.mem lr1 (Lr1.accepting g)));
      visit_config
        (IndexSet.singleton lrc)
        (Source.inj_r lr1)
    in
    IndexMap.inflate process entrypoints.wait

  (* Freeze the state generator to finalize the state machine *)
  let states = IndexBuffer.Gen.freeze states

  (* Print timing information and the number of nodes in the state machine *)
  let () = stopwatch 2 "reachable reductions with %d nodes" (cardinal n)

  (* Iterate over all transition targets *)
  let iter_targets tr f =
    List.iter (List.iter f) tr

  (* Iterate over all transition targets in reverse order *)
  let rev_iter_targets tr f =
    list_rev_iter (list_rev_iter f) tr

  (* Fold over all targets in transitions *)
  let fold_targets f acc tr =
    List.fold_left (List.fold_left f) acc tr

  (* Fold over all targets in transitions in reverse order *)
  let rev_fold_targets f tr acc =
    List.fold_right (List.fold_right f) tr acc

  (* Successors of a state *)
  let successors =
    Vector.map (fun desc ->
        rev_fold_targets
          (fun (st, _) acc -> IndexSet.add st acc)
          desc.transitions IndexSet.empty
      ) states

  (* Predecessors of a state *)
  let predecessors =
    relation_reverse n successors

  (* The set of terminals that are immediately rejected by a state *)
  let reject st =
    let {config; _} = Vector.get states st in
    match Source.prj config.source with
    | L v ->
      let config = viable.config.:(v) in
      IndexSet.inter (Lr1.reject g config.top) config.lookahead
    | R lr1 -> IndexSet.inter (Lr1.reject g lr1) (Terminal.regular g)

  (* Compute the set of terminals that can be rejected by successors of a state *)
  let rejectable =
    let table = Vector.init n (fun st -> (reject st, IndexMap.empty)) in
    let propagate source (sreject, _) _target (treject, trejects as prev) =
      let treject' = IndexSet.union sreject treject in
      if treject' == treject then
        prev
      else
        let delta = IndexSet.diff sreject treject in
        let update = function
          | None -> Some delta
          | Some ts -> Some (IndexSet.union ts delta)
        in
        (treject', IndexMap.update source update trejects)
    in
    fix_relation ~propagate predecessors table;
    let project (ts, edges) = (ts, IndexMap.bindings edges) in
    Vector.get (Vector.map project table)

  (* List the reductions that can lead from state [src] to state [tgt] *)
  let reductions src tgt =
    fold_targets (fun acc (tgt', red) ->
        if equal_index tgt tgt'
        then IndexSet.add red acc
        else acc
      ) IndexSet.empty (Vector.get states src).transitions
end

(* Functor to create a covering tree for the Reachable_reductions module *)
module Covering_tree
    (Reductions: sig
       type g
       val g : g grammar
       val viable : g Viable_reductions.t end)
    (Reach : S with type g = Reductions.g)
    ()
=
struct
  open Reach
  open Reductions

  (* Enumerate the stack suffixes produced by following one or more goto
     transitions as part of a sequence of reductions.*)
  module Goto_star = struct
    include IndexBuffer.Gen.Make()

    (* Function to get the LR0 stack from a source *)
    let lr0_stack_of_source source =
      match Sum.prj (Vector.length viable.transitions) source with
      | L v -> List.rev_map (Lr1.to_lr0 g) (Viable_reductions.get_stack viable v)
      | R lr1 -> [Lr1.to_lr0 g lr1]

    let def = get_generator ()

    (* Function to get the index of a LR0 stack *)
    let index_of =
      let table = Hashtbl.create 7 in
      let index_of lr0s =
        match Hashtbl.find_opt table lr0s with
        | Some i -> i
        | None ->
          let r = IndexBuffer.Gen.reserve def in
          let i = IndexBuffer.Gen.index r in
          Hashtbl.add table lr0s i;
          IndexBuffer.Gen.commit def r lr0s;
          i
      in
      let project desc = index_of (lr0_stack_of_source desc.config.source) in
      Vector.get (Vector.map project states)

    let def = IndexBuffer.Gen.freeze def

    (* Print the number of configurations to cover *)
    let () =
      Printf.eprintf "Goto*: found %d configurations to cover\n"
        (cardinal (Vector.length def))
  end

  (* Type representing a node in the covering tree *)
  type node = {
    state: n index;
    rejected: g terminal indexset;
    mutable handled: g terminal indexset;
    mutable children: node list;
  }

  (* Vector to store the set of rejected terminals before a state *)
  let reject_before = Vector.make n IndexSet.empty

  (* Function to make an arborescence from a set of roots *)
  let make_arborescence roots =
    let marked = Boolvector.make n false in
    let nodes = ref 0 in
    let queue = ref [] in
    (* Function to visit a state and add it to the arborescence *)
    let visit rejected state acc =
      let rejected = IndexSet.union (reject state) rejected in
      let continue, rejected' =
        if Boolvector.test marked state then (
          let rejected' = Vector.get reject_before state in
          let rejected'' = IndexSet.union rejected rejected' in
          (rejected' != rejected'', rejected'')
        ) else (
          Boolvector.set marked state;
          (true, rejected)
        )
      in
      if continue then (
        incr nodes;
        Vector.set reject_before state rejected';
        let node = {state; rejected; handled = IndexSet.empty; children = []} in
        push queue node;
        node :: acc
      ) else
        acc
    in
    let roots = IndexSet.fold (visit IndexSet.empty) roots [] in
    (* Function to propagate the arborescence to children *)
    let propagate node =
      let next = Vector.get successors node.state in
      node.children <- IndexSet.fold (visit node.rejected) next node.children
    in
    fixpoint ~propagate queue;
    Printf.eprintf "Arborescence has %d nodes\n" !nodes;
    roots

  (* Function to complete the arborescence with goals *)
  let complete_arborescence goals roots =
    let sentences = ref 0 in
    (* Function to get a child node for a state *)
    let get_child node state =
      let pred node' = equal_index node'.state state in
      match List.find_opt pred node.children with
      | Some node' -> node'
      | None ->
        let rejected = IndexSet.union (reject state) node.rejected in
        let node' = {state; rejected; handled = IndexSet.empty; children = []} in
        node.children <- node' :: node.children;
        node'
    in
    (* Function to visit a state after processing its children *)
    let rec visit_after node todo =
      let goal = Goto_star.index_of node.state in
      Vector.set goals goal
        (IndexSet.diff (Vector.get goals goal) todo);
      let todo = List.fold_left (fun todo (successor, terminals) ->
          let todo' = IndexSet.inter todo terminals in
          if IndexSet.is_empty todo' then todo else (
            visit_after (get_child node successor) todo';
            IndexSet.diff todo todo'
          )
        ) todo (snd (rejectable node.state))
      in
      if not (IndexSet.is_empty todo) then (
        assert (IndexSet.subset todo (reject node.state));
        node.handled <- todo;
        incr sentences;
      )
    in
    let rec visit_before node =
      let visited =
        List.fold_left
          (fun acc node' -> IndexSet.union (visit_before node') acc)
          IndexSet.empty node.children
      in
      let goal = Goto_star.index_of node.state in
      let rejectable, _ = rejectable node.state in
      let remaining = Vector.get goals goal in
      let remaining = IndexSet.diff remaining visited in
      Vector.set goals goal remaining;
      let todo = IndexSet.inter rejectable remaining in
      visit_after node todo;
      IndexSet.union visited todo
    in
    List.iter (fun node -> ignore (visit_before node)) roots;
    Printf.eprintf "Enumeration has %d sentences\n" !sentences

  let goals =
    let table = Vector.make Goto_star.n IndexSet.empty in
    Index.iter (Vector.length states) (fun st ->
        let ra = reject st in
        let rb, _ = rejectable st in
        let rc = Vector.get reject_before st in
        let r = IndexSet.union ra (IndexSet.union rb rc) in
        table.@(Goto_star.index_of st) <- IndexSet.union r
      );
    table

  let roots =
    let add _ = IndexSet.add in
    let initial = IndexMap.fold add Reach.initial IndexSet.empty in
    let roots = make_arborescence initial in
    complete_arborescence goals roots;
    roots

  let enum_sentences f =
    let rec visit path (node : node) =
      let path = node.state :: path in
      if not (IndexSet.is_empty node.handled) then
        f path node.rejected;
      List.iter (visit path) node.children
    in
    List.iter (visit []) roots
end

module type FAILURE_NFA = sig
  type g
  include CARDINAL
  val initial : (n, g terminal indexset) indexmap
  (*val reject : n index -> terminal indexset*)
  val rejectable : n index -> g terminal indexset
  val top : n index -> g lr1 index option
  val goto : n index -> g lr1 index list
  val is_bottom : n index -> bool
  val incoming : n index -> g lr1 indexset
  val incoming_lrc : n index -> g Lrc.n indexset
  val transitions : n index -> (n, g terminal indexset) indexmap
  val to_string : n index -> string
end

module FailureNFA
    (Reductions: sig
       type g
       val g : g grammar
       val viable : g Viable_reductions.t
       val lrc_graph : g Lrc.t
       val entrypoints : g Lrc.entrypoints
     end)
    (Reach : S with type g = Reductions.g)
    ()
  : FAILURE_NFA with type g = Reductions.g
=
struct
  type g = Reductions.g
  open Reductions

  let () =
    if build_with_expensive_assertions then (
      Vector.iteri (fun lrc lr1 ->
          let lr1s' = Lr1.predecessors g lr1 in
          IndexSet.iter (fun lrc' ->
              assert (IndexSet.mem lrc_graph.lr1_of.:(lrc') lr1s')
            ) entrypoints.predecessors.:(lrc)
        ) lrc_graph.lr1_of;
      let validate {Reach. config; transitions} =
        let rec validate lrcs = function
          | [] -> ()
          | x :: xs ->
            List.iter (fun (reach', _) ->
                let lrcs' = (Vector.get Reach.states reach').config.lrcs in
                if not (IndexSet.subset lrcs' lrcs) then (
                  Printf.eprintf "%s not included in %s\n"
                    (Lrc.set_to_string g lrc_graph lrcs')
                    (Lrc.set_to_string g lrc_graph lrcs);
                  assert false
                )
              ) x;
            validate (indexset_bind lrcs (Vector.get entrypoints.predecessors)) xs
        in
        validate config.lrcs transitions
      in
      Vector.iter validate Reach.states
    )

  module States = struct
    module Suffix = IndexBuffer.Gen.Make()
    module Reach_or_suffix = Sum.Make(Reach)(Suffix)
    include Sum.Make(struct
        type n = g Lrc.n
        let n = Vector.length lrc_graph.reachable_from
      end)(Reach_or_suffix)

    let prefix i = inj_l i
    let suffix i = inj_r (Reach_or_suffix.inj_r i)
    let reach i = inj_r (Reach_or_suffix.inj_l i)

    type t =
      | Prefix of g Lrc.n index
      | Suffix of Suffix.n index
      | Reach  of Reach.n index

    let prj i = match prj i with
      | L i -> Prefix i
      | R i ->
        match Reach_or_suffix.prj i with
        | L i -> Reach i
        | R i -> Suffix i
  end

  type n = States.n
  let n = States.n

  let map_union m1 m2 =
    let merge _ a0 a1 = Some (IndexSet.union a0 a1) in
    let m3 = IndexMap.union merge m1 m2 in
    if IndexMap.equal IndexSet.equal m2 m3
    then m2
    else m3

  let epsilon_closure =
    let epsilon_predecessors = Vector.make Reach.n IndexSet.empty in
    Vector.iteri (fun src {Reach.transitions; _} ->
        match transitions with
        | [] -> ()
        | epsilon :: _ ->
          List.iter
            (fun (tgt, _) -> epsilon_predecessors.@(tgt) <- IndexSet.add src)
            epsilon
      ) Reach.states;
    let table =
      Vector.init Reach.n
        (fun i -> IndexMap.singleton (States.reach i) (Reach.reject i))
    in
    fix_relation epsilon_predecessors table
      ~propagate:(fun _ map _ map' -> map_union map map');
    Vector.get table

  let suffix_transitions =
    States.Suffix.get_generator ()

  let lrc_initials =
    IndexSet.filter
      (fun lrc -> IndexSet.is_empty entrypoints.predecessors.:(lrc))
      entrypoints.wait

  let bottom =
    States.suffix
      (IndexBuffer.Gen.add suffix_transitions (IndexMap.empty, IndexSet.empty))

  let reach_transitions =
    let preds lrcs = indexset_bind lrcs (Vector.get entrypoints.predecessors) in
    let succs lrcs = indexset_bind lrcs (Vector.get entrypoints.successors) in
    let rec import lrcs = function
      | [] -> (IndexMap.empty, IndexSet.empty)
      | targets :: next ->
        let targets, lrcs1 =
          let add (set, lrcs) (reach, _) =
            let set = map_union (epsilon_closure reach) set in
            let desc =Vector.get Reach.states reach in
            let lrcs = IndexSet.union desc.config.lrcs lrcs in
            (set, lrcs)
          in
          List.fold_left add (IndexMap.empty, IndexSet.empty) targets
        in
        (*FIXME: Find out why it is not always a subset*)
        if not (IndexSet.subset lrcs1 lrcs) then (
          Printf.eprintf "expected: %s in %s\n"
            (Lrc.set_to_string g lrc_graph lrcs1)
            (Lrc.set_to_string g lrc_graph lrcs);
          assert false
        );
        match next with
        | [] -> (targets, lrcs1)
        | next ->
          let next, lrcs2 = import (preds lrcs) next in
          let lrcs2 = IndexSet.inter (succs lrcs2) lrcs in
          let suffix = IndexBuffer.Gen.add suffix_transitions (next, lrcs2) in
          (IndexMap.add (States.suffix suffix) IndexSet.empty targets,
           IndexSet.union lrcs1 lrcs2)
    in
    Vector.map (fun (d : Reach.desc) ->
        let regular =
          match d.transitions with
          | [] | [_] -> IndexMap.empty
          | _ :: rest -> fst (import (preds d.config.lrcs) rest)
        in
        if IndexSet.subset d.config.lrcs lrc_initials then (
          assert (IndexMap.is_empty regular);
          IndexMap.singleton bottom IndexSet.empty
        ) else
          regular
      ) Reach.states

  let suffix_transitions =
    IndexBuffer.Gen.freeze suffix_transitions

  let initial =
    let add _ st acc = map_union (epsilon_closure st) acc in
    IndexMap.fold add Reach.initial IndexMap.empty

  let rejectable =
    let table = Vector.make States.Suffix.n IndexSet.empty in
    let get i =
      match States.prj i with
      | States.Prefix _ -> IndexSet.empty
      | States.Reach  i -> fst (Reach.rejectable i)
      | States.Suffix i -> Vector.get table i
    in
    let populate target _ set = IndexSet.union (get target) set in
    let def targets = IndexMap.fold populate targets IndexSet.empty in
    let initialize suf (targets, _) =
      Vector.set table suf (def targets)
    in
    (* A suffix state comes always before its predecessors, and rejectables are
       propagated from a state to its predecessors, so:
       - a forward pass should initialize them correctly,
       - there is no need to look for a fixed point. *)
    Vector.iteri initialize suffix_transitions;
    if build_with_expensive_assertions then (
      let validate st (targets, _) =
        assert (IndexSet.equal (Vector.get table st) (def targets))
      in
      Vector.iteri validate suffix_transitions
    );
    get

  let top i =
    match States.prj i with
    | States.Prefix _ -> None
    | States.Suffix _ -> None
    | States.Reach r ->
      let desc = Vector.get Reach.states r in
      let source = desc.config.source in
      match Sum.prj (Vector.length viable.transitions) source with
      | L v -> Some viable.config.:(v).top
      | R lr1 -> Some lr1

  let goto i =
    match States.prj i with
    | States.Prefix _ -> []
    | States.Suffix _ -> []
    | States.Reach r ->
      let desc = Vector.get Reach.states r in
      let source = desc.config.source in
      match Sum.prj (Vector.length viable.transitions) source with
      | L v -> Viable_reductions.get_stack viable v
      | R _ -> []

  let incoming_lrc i =
    match States.prj i with
    | States.Prefix lrc -> IndexSet.singleton lrc
    | States.Reach i -> (Vector.get Reach.states i).config.lrcs
    | States.Suffix i -> snd (Vector.get suffix_transitions i)

  let reach_incoming =
    let incoming desc = IndexSet.map (Vector.get lrc_graph.lr1_of) desc.Reach.config.lrcs in
    Vector.map incoming Reach.states

  let suffix_incoming =
    let incoming (_, lrcs) = IndexSet.map (Vector.get lrc_graph.lr1_of) lrcs in
    Vector.map incoming suffix_transitions

  let incoming i =
    match States.prj i with
    | States.Prefix lrc -> IndexSet.singleton lrc_graph.lr1_of.:(lrc)
    | States.Reach i -> Vector.get reach_incoming i
    | States.Suffix i -> Vector.get suffix_incoming i

  let transitions i =
    match States.prj i with
    | States.Prefix lrc ->
      IndexMap.inflate (fun _ -> IndexSet.empty)
        (IndexSet.map States.prefix entrypoints.predecessors.:(lrc))
    | States.Reach i -> Vector.get reach_transitions i
    | States.Suffix i -> fst (Vector.get suffix_transitions i)

  let is_bottom i = i = bottom

  let to_string i =
    if is_bottom i then "bottom" else
      match States.prj i with
      | States.Prefix i -> Printf.sprintf "prefix(%d)" (Index.to_int i)
      | States.Reach  i ->
        let desc = Vector.get Reach.states i in
        Printf.sprintf "reach(%d, %s)"
          (Index.to_int i)
          (Lrc.set_to_string g lrc_graph desc.config.lrcs)
      | States.Suffix i -> Printf.sprintf "suffix(%d)" (Index.to_int i)

  let () =
    let oc = open_out_bin "nfa.dot" in
    let p fmt = Printf.kfprintf (fun oc -> output_char oc '\n') oc fmt in
    p "digraph G {";
    p "  %s" style;
    Index.iter n (fun i ->
        p "  st%d[label=%S];\n" (i :> int) (to_string i);
        IndexMap.iter (fun j _ ->
            p "  st%d -> st%d;\n" (i :> int) (Index.to_int j);
          ) (transitions i)
      );
    p "}";
    close_out oc
end

module Coverage_check
    (G : sig type g val g : g grammar end)
    (NFA : FAILURE_NFA with type g = G.g)
    (DFA : sig
       open Info
       type n
       val n : n cardinal
       val initial : n index
       val successors : n index -> (G.g lr1 indexset * n index) list
       val accept : n index -> G.g terminal indexset
     end)
    ()
=
struct
  open G

  module FDFA = struct
    let table = Hashtbl.create 7

    let todo = ref []

    let explore_transitions trs =
      let sets =
        List.fold_left (fun acc tr ->
            IndexMap.fold
              (fun state reject acc ->
                 (NFA.incoming state, (reject, state)) :: acc)
              tr acc
          )
          [] trs
      in
      List.map (fun (lbl, clss) ->
          let reject, states =
            List.fold_left (fun (rejects, states) (reject, state) ->
              (IndexSet.union reject rejects, IndexSet.add state states))
              (IndexSet.empty, IndexSet.empty)
              clss
          in
          push todo states;
          (lbl, reject, states)
        )
        (IndexRefine.annotated_partition sets)

    let propagate states =
      if not (Hashtbl.mem table states) then (
        Hashtbl.add table states (
          explore_transitions (
            IndexSet.fold (fun state acc -> NFA.transitions state :: acc)
              states []
          )
        )
      )

    let initial = explore_transitions [NFA.initial]

    let () =
      fixpoint ~propagate todo

    let () =
      stopwatch 3 "Determinized Failure NFA with %d states" (Hashtbl.length table)
  end

  let pts ts = string_of_indexset ~index:(Terminal.to_string g) ts

  let () =
    let oc = open_out_bin "dfa.dot" in
    let p fmt = Printf.kfprintf (fun oc -> output_char oc '\n') oc fmt in
    p "digraph G {";
    p "  %s" style;
    Index.iter DFA.n (fun src ->
        let accept = pts (DFA.accept src) in
        p "  st%d [label=%S]" (Index.to_int src)
          (if src = DFA.initial then "initial " ^ accept else accept)
        ;
        List.iter (fun (lbl, tgt) ->
            p "  st%d -> st%d [label=%S]"
              (Index.to_int src) (Index.to_int tgt)
              (Lr1.set_to_string g lbl)
        ) (DFA.successors src);
      );
    p "}";
    close_out_noerr oc

  module Image = struct
    type t = {
      handled: g terminal indexset;
      rejected: g terminal indexset;
    }

    let empty = {rejected=IndexSet.empty; handled=IndexSet.empty}

    (* Return the image propagated when following a transition:
       - reaching a DFA state accepting [accept]
       - rejecting [reject]
       - with continuations possibly rejecting [rejectable]

       Returns [None] if nothing has to be propagated (all rejected or
       rejectable lookaheads are handled), or [Some img] with the propagated
       image if some lookahead still need to be handled.
    *)
    let normalize ~accept ~reject ~rejectable image =
      let handled = IndexSet.union image.handled accept in
      let rejected = IndexSet.diff (IndexSet.union reject image.rejected) handled in
      let handled = IndexSet.inter handled rejectable in
      if IndexSet.is_empty rejected && IndexSet.equal handled rejectable then
        None
      else
        Some {rejected; handled}

    (* This function increase an image with a propagated delta.
     * Returns [None] if the image is unchanged, or [Some (image', delta')] with
     * the new image and the change.
    *)
    let increase ~delta image =
      let {rejected; handled} = delta in
      let rejected = IndexSet.union rejected image.rejected in
      let handled = IndexSet.diff (IndexSet.inter handled image.handled) rejected in
      if rejected == image.rejected && handled == image.handled then
        None
      else
        let image' = {rejected; handled} in
        let rejected = IndexSet.diff rejected image.rejected in
        Some (image', {rejected; handled})

    (* If two different delta are propagated to the same target,
       it is possible to refine the deltas and update the target once.
       E.g. [increase ~delta:d1 (increase ~delta:d2 target)] is equivalent to
            [increase ~delta:(refine d1 d2) target]
    *)
    let refine i1 i2 =
      {handled = IndexSet.inter i1.handled i2.handled;
       rejected = IndexSet.union i1.rejected i2.rejected}
  end

  module State = Prod.Make(DFA)(NFA)

  type desc = {
    state: State.n index;
    mutable successors: (g lr1 indexset * g terminal indexset * State.n index) list;
    mutable predecessors: (g lr1 indexset * g terminal indexset * State.n index) list;

    uncovered: g lr1 indexset;
    mutable image: Image.t;
    mutable failing: g terminal indexset;
    mutable failing_successors: (g lr1 indexset * (g terminal indexset * desc)) list;

    mutable mark: bool;
    mutable todo: Image.t;
  }

  let coverage = IndexTable.create 7

  let get_state index =
    match IndexTable.find_opt coverage index with
    | Some desc -> desc
    | None ->
      let dfa, nfa = State.prj index in
      let (successors, uncovered) =
        List.fold_left (fun (successors, uncovered) (lbl, dfa') ->
            let uncovered' = IndexSet.diff uncovered lbl in
            let successors' =
              if uncovered != uncovered' then (
                let lbl' = IndexSet.inter uncovered lbl in
                IndexMap.fold
                  (fun nfa' reject acc ->
                     let index' = State.inj dfa' nfa' in
                     (lbl', reject, index') :: acc)
                  (NFA.transitions nfa) successors
              ) else
                successors
            in
            (successors', uncovered')
          ) ([], NFA.incoming nfa) (DFA.successors dfa)
      in
      let desc = {
        state = index;
        successors; uncovered;
        predecessors = [];
        image = Image.empty;
        failing = IndexSet.empty;
        failing_successors = [];
        mark = false;
        todo = Image.empty;
      } in
      IndexTable.add coverage index desc;
      desc

  let worklist = ref []

  let add_delta index = function
    | None -> ()
    | Some delta ->
      let desc = get_state index in
      match Image.increase ~delta desc.image with
      | None -> ()
      | Some (image', delta') ->
        desc.image <- image';
        let delta' =
          if desc.mark then
            Image.refine delta' desc.todo
          else (
            push worklist desc;
            desc.mark <- true;
            delta'
          )
        in
        desc.todo <- delta'

  let initial =
    let initialize nfa reject acc =
      let index = State.inj DFA.initial nfa in
      add_delta index (
        Image.normalize
          ~accept:(DFA.accept DFA.initial)
          ~rejectable:(NFA.rejectable nfa)
          ~reject Image.empty
      );
      index :: acc
    in
    IndexMap.fold initialize NFA.initial []

  let () =
    let propagate desc =
      let img = desc.todo in
      desc.mark <- false;
      desc.todo <- Image.empty;
      List.iter (fun (_, reject, index') ->
          let dfa, nfa = State.prj index' in
          add_delta index' (
            Image.normalize
              ~accept:(DFA.accept dfa)
              ~rejectable:(NFA.rejectable nfa)
              ~reject img
          )
        ) desc.successors
    in
    fixpoint ~propagate worklist

  (* Normalize successors & compute predecessors *)
  let () =
    IndexTable.iter (fun index desc ->
        let check_successor (_lbl, reject, index') =
          let dfa', nfa' = State.prj index' in
          Option.is_some (
            Image.normalize
              ~accept:(DFA.accept dfa')
              ~rejectable:(NFA.rejectable nfa')
              ~reject desc.image
          )
        in
        List.iter (fun (lbl, reject, index' as succ) ->
            if check_successor succ then
              let desc' = get_state index' in
              desc'.predecessors <- (lbl, reject, index) :: desc'.predecessors
          ) desc.successors
      ) coverage

  let is_bottom desc =
    let _, nfa = State.prj desc.state in
    NFA.is_bottom nfa

  let is_immediate_failure desc =
    not (IndexSet.is_empty desc.uncovered) || is_bottom desc

  (* Propagate failing lookaheads backward *)
  let () =
    let counter = ref 0 in
    let worklist = ref [] in
    let schedule index desc =
      if not desc.mark then (
        desc.mark <- true;
        push worklist index;
      )
    in
    let update desc (lbl, reject, index') =
      let _, nfa' = State.prj index' in
      let desc' = IndexTable.find coverage index' in
      let failing =
        let f1 = IndexSet.inter desc.failing desc'.image.rejected in
        let f2 = IndexSet.inter desc.failing (NFA.rejectable nfa') in
        IndexSet.union f1 (IndexSet.diff f2 desc'.image.handled)
      in
      if not (IndexSet.subset failing desc'.failing) then (
        desc'.failing <- IndexSet.union failing desc'.failing;
        desc'.failing_successors <- (lbl, (reject, desc)) :: desc'.failing_successors;
        schedule index' desc'
      )
    in
    let propagate index =
      let desc = get_state index in
      assert desc.mark;
      desc.mark <- false;
      List.iter (update desc) desc.predecessors
    in
    IndexTable.iter (fun index desc ->
        if is_immediate_failure desc then (
          let _, nfa = State.prj index in
          desc.failing <- IndexSet.union desc.image.rejected (NFA.rejectable nfa);
          assert (not (IndexSet.is_empty desc.failing));
          schedule index desc
        )
      ) coverage;
    fixpoint ~counter ~propagate worklist;
    let initial_failing =
      IndexMap.fold (fun nfa _ acc ->
          match IndexTable.find_opt coverage (State.inj DFA.initial nfa) with
          | None -> acc
          | Some desc -> IndexSet.union desc.failing acc
        ) NFA.initial IndexSet.empty
    in
    Printf.eprintf "Backpropagated failing lookaheads in %d steps.\n" !counter;
    Printf.eprintf "Unhandled lookaheads: %s.\n"
      (string_concat_map ", " (Terminal.to_string g)
         (IndexSet.elements initial_failing))

  (* Generate sentences and determinize on the fly *)

  type step = {
    label: g lr1 indexset;
    goto: g lr1 index list list;
  }

  let union_all l =
    List.fold_left IndexSet.union IndexSet.empty l

  let enum_uncovered ~f =
    let rec visit path states =
      let transitions =
        List.fold_left (fun acc (reject, desc) ->
            List.fold_left (fun acc (lbl, (reject', desc')) ->
                let _, nfa = State.prj desc.state in
                let goto = match NFA.goto nfa with
                  | [] -> []
                  | other -> [other]
                in
                (lbl, (goto, (IndexSet.union reject reject', desc'))) :: acc
              ) acc desc.failing_successors
          ) [] states
      in
      List.iter (fun (label, states') ->
          let goto, states' = List.split states' in
          let goto = List.concat goto in
          visit ({label; goto} :: path) states')
        (IndexRefine.annotated_partition transitions);
      let uncovered =
          List.filter_map (fun (reject, desc) ->
              if IndexSet.is_empty desc.uncovered then
                None
              else
                let _, nfa = State.prj desc.state in
                let goto = match NFA.goto nfa with
                  | [] -> []
                  | other -> [other]
                in
                Some (desc.uncovered, (goto, IndexSet.union reject (NFA.rejectable nfa)))
            ) states
      in
      List.iter
        (fun (label, rejects) ->
           let goto, rejects = List.split rejects in
           let goto = List.concat goto in
           f ({label; goto} :: path) (union_all rejects))
        (IndexRefine.annotated_partition uncovered);
      begin match List.filter (fun (_, desc) -> is_bottom desc) states with
        | [] -> ()
        | immediate ->
          let failing =
            List.fold_left (fun acc (reject, desc) ->
                let _, nfa = State.prj desc.state in
                IndexSet.union acc
                  (IndexSet.union reject (NFA.rejectable nfa))
              ) IndexSet.empty immediate
          in
          f path failing;
      end
    in
    let initials =
      IndexMap.fold (fun nfa reject acc ->
          let index = State.inj DFA.initial nfa in
          match IndexTable.find_opt coverage index with
          | None -> acc
          | Some desc ->
            if IndexSet.is_empty desc.failing
            then acc
            else (reject, desc) :: acc
        ) NFA.initial []
    in
    visit [] initials

  let _ =
    let count = ref 0 in
    enum_uncovered ~f:(fun path failing ->
        incr count;
        Printf.eprintf "Sentence %d, failing when looking ahead at %s:\n"
          !count (string_of_indexset ~index:(Terminal.to_string g) failing);
        let print_step i step =
          let plr1 lr1 = Lr1.symbol_to_string g lr1 in
          let lr1 = IndexSet.choose step.label in
          Printf.eprintf "- %s\n" (plr1 lr1);
          match step.goto with
          | [] ->
            if i = 0 then
              Printf.eprintf "  [ %s]\n"
              (string_concat_map " " (Item.to_string g)
                 (IndexSet.elements (Lr1.items g lr1)))
          | goto ->
            List.iter (fun goto ->
                let lr1' = List.hd goto in
                let suff = string_concat_map ";" plr1 (List.rev goto) in
                let pad = String.make (String.length suff) ' ' in
                Printf.eprintf "  [%s " suff;
                let need_sep = ref false in
                IndexSet.iter (fun item ->
                    if !need_sep then Printf.eprintf "\n    %s" pad;
                    need_sep := true;
                    Printf.eprintf "%s" (Item.to_string g item);
                  ) (Lr1.items g lr1');
                Printf.eprintf "]\n";
              ) goto
        in
        List.iteri print_step (List.rev path)
      )
end
