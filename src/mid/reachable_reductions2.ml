open Utils
open Misc
open Fix.Indexing

let build_with_expensive_assertions = false

module type S = sig
  module Info : Info.S
  module Viable: Viable_reductions.S with module Info := Info
  module Lrc : Lrc.S with module Info := Info
  open Info

  include CARDINAL
  module Source : SUM with type l := Viable.n and type r := Lr1.n

  type config = {
    source: Source.n index;
    lrcs: Lrc.set;
  }

  type target = n index * Reduction.t

  type transitions = target list list

  type desc = {
    config: config;
    transitions: transitions;
  }

  val initial : n index Lrc.map
  val states : (n, desc) vector

  val reject : n index -> Terminal.set
  val rejectable : n index -> Terminal.set * (n index * Terminal.set) list

  val successors : (n, n indexset) vector
  val predecessors : (n, n indexset) vector

  val reductions : n index -> n index -> Reduction.set

  val iter_targets : transitions -> (target -> unit) -> unit
  val rev_iter_targets : transitions -> (target -> unit) -> unit
  val fold_targets : ('a -> target -> 'a) -> 'a -> transitions -> 'a
  val rev_fold_targets : (target -> 'a -> 'a) -> transitions -> 'a -> 'a
end

module Make
    (Info : Info.S)
    (Viable: Viable_reductions.S with module Info := Info)
    (Lrc: Lrc.S with module Info := Info)
    () : S with module Info := Info
            and module Viable := Viable
            and module Lrc := Lrc
=
struct
  open Info
  let time = Stopwatch.enter Stopwatch.main "Reachable reductions"

  include IndexBuffer.Gen.Make()

  module Source = Sum(Viable)(Lr1)

  type config = {
    source: Source.n index;
    lrcs : Lrc.set;
  }

  type target = n index * Reduction.t

  type transitions = target list list

  type desc = {
    config: config;
    transitions: transitions;
  }

  let states = get_generator ()

  let nodes = Hashtbl.create 7

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
        | L viable -> Viable.get_transitions viable
        | R lr1 -> {Viable.inner=[]; outer=Vector.get Viable.initial lr1}
      in
      let transitions = visit_transitions lrcs source_transitions in
      IndexBuffer.Gen.commit states reservation {config; transitions};
      index

  and visit_transitions lrcs {Viable.inner; outer} =
    let inner =
      List.fold_left (fun acc {Viable.candidates; _} ->
          List.fold_left
            (fun acc {Viable.target; lookahead=_; filter=(); reduction} ->
               let state = visit_config lrcs (Source.inj_l target) in
               (state, reduction) :: acc)
            acc candidates
        ) [] inner
    in
    inner :: visit_outer lrcs outer

  and visit_outer lrcs = function
    | [] -> []
    | {Viable.candidates; _} :: rest ->
      let visit_candidate {Viable.target; lookahead=_; filter=lr1s; reduction} =
        let compatible_lrc lr1 = IndexSet.inter lrcs (Lrc.lrcs_of_lr1 lr1) in
        let lrcs = indexset_bind lr1s compatible_lrc in
        if IndexSet.is_empty lrcs
        then None
        else Some (visit_config lrcs (Source.inj_l target), reduction)
      in
      let candidates = List.filter_map visit_candidate candidates in
      let rest =
        if rest = [] then
          []
        else
          visit_outer (indexset_bind lrcs Lrc.predecessors) rest
      in
      candidates :: rest

  let initial =
    let process lrc =
      let lr1 = Lrc.lr1_of_lrc lrc in
      assert (not (IndexSet.mem lr1 Lr1.accepting));
      visit_config
        (IndexSet.singleton lrc)
        (Source.inj_r lr1)
    in
    IndexMap.inflate process Lrc.idle

  let states = IndexBuffer.Gen.freeze states

  let () = Stopwatch.step time "Nodes: %d" (cardinal n)

  let iter_targets tr f =
    List.iter (List.iter f) tr

  let rev_iter_targets tr f =
    list_rev_iter (list_rev_iter f) tr

  let fold_targets f acc tr =
    List.fold_left (List.fold_left f) acc tr

  let rev_fold_targets f tr acc =
    List.fold_right (List.fold_right f) tr acc

  let successors =
    Vector.map (fun desc ->
        rev_fold_targets
          (fun (st, _) acc -> IndexSet.add st acc)
          desc.transitions IndexSet.empty
      ) states

  let predecessors =
    relation_reverse successors

  let reject st =
    let {config; _} = Vector.get states st in
    let lr1 = match Source.prj config.source with
      | L viable -> (Viable.get_config viable).top
      | R lr1 -> lr1
    in
    Lr1.reject lr1

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

  let reductions src tgt =
    fold_targets (fun acc (tgt', red) ->
        if equal_index tgt tgt'
        then IndexSet.add red acc
        else acc
      ) IndexSet.empty (Vector.get states src).transitions

  let () = Stopwatch.leave time
end

module Covering_tree
    (Info : Info.S)
    (Viable: Viable_reductions.S with module Info := Info)
    (Lrc: Lrc.S with module Info := Info)
    (Reach : S with module Info := Info
                and module Viable := Viable
                and module Lrc := Lrc)
    ()
=
struct
  open Info
  open Reach

  module Goto_star = struct
    include IndexBuffer.Gen.Make()

    let lr0_stack_of_source source =
      match Source.prj source with
      | L v -> List.rev_map Lr1.to_lr0 (Viable.get_stack v)
      | R lr1 -> [Lr1.to_lr0 lr1]

    let def = get_generator ()

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

    let () =
      Printf.eprintf "Goto*: found %d configurations to cover\n"
        (cardinal (Vector.length def))
  end

  type node = {
    state: n index;
    rejected: Terminal.set;
    mutable handled: Terminal.set;
    mutable children: node list;
  }

  let reject_before = Vector.make n IndexSet.empty

  let make_arborescence roots =
    let marked = Boolvector.make n false in
    let nodes = ref 0 in
    let queue = ref [] in
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
    let propagate node =
      let next = Vector.get successors node.state in
      node.children <- IndexSet.fold (visit node.rejected) next node.children
    in
    fixpoint ~propagate queue;
    Printf.eprintf "Arborescence has %d nodes\n" !nodes;
    roots

  let complete_arborescence goals roots =
    let sentences = ref 0 in
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
        vector_set_union table (Goto_star.index_of st) r
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
  type reach
  type lr1
  type lrc
  type terminal
  include CARDINAL
  val initial : (n, terminal indexset) indexmap
  (*val reject : n index -> terminal indexset*)
  val rejectable : n index -> terminal indexset
  val reach_incoming : (reach, lr1 indexset) vector
  val incoming : n index -> lr1 indexset
  val incoming_lrc : n index -> lrc indexset
  val transitions : n index -> (n, terminal indexset) indexmap
  val kind : n index -> string
end

module FailureNFA
    (Info : Info.S)
    (Viable: Viable_reductions.S with module Info := Info)
    (Lrc: Lrc.S with module Info := Info)
    (Reach : S with module Info := Info
                and module Viable := Viable
                and module Lrc := Lrc)
    ()
  : FAILURE_NFA with type terminal := Info.Terminal.n
                 and type lr1 := Info.Lr1.n
                 and type lrc := Lrc.n
                 and type reach = Reach.n
=
struct

  let time = Stopwatch.enter Stopwatch.main "FailureNFA2"

  type reach = Reach.n

  module States = struct
    module Suffix = IndexBuffer.Gen.Make()
    module Reach_or_suffix = Sum(Reach)(Suffix)
    include Sum(Lrc)(Reach_or_suffix)

    let prefix i = inj_l i
    let suffix i = inj_r (Reach_or_suffix.inj_r i)
    let reach i = inj_r (Reach_or_suffix.inj_l i)

    type t =
      | Prefix of Lrc.n index
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
            (fun (tgt, _) -> vector_set_add epsilon_predecessors tgt src)
            epsilon
      ) Reach.states;
    let closure =
      Vector.init Reach.n
        (fun i -> IndexMap.singleton (States.reach i) (Reach.reject i))
    in
    fix_relation epsilon_predecessors closure
      ~propagate:(fun _ map _ map' -> map_union map map');
    Vector.get closure

  let suffix_transitions =
    States.Suffix.get_generator ()

  let lrc_initials =
    IndexSet.filter
      (fun lrc -> IndexSet.is_empty (Lrc.predecessors lrc))
      Lrc.idle

  let sink =
    let r = IndexBuffer.Gen.reserve suffix_transitions in
    let i = IndexBuffer.Gen.index r in
    let sink = IndexMap.singleton (States.suffix i) IndexSet.empty in
    IndexBuffer.Gen.commit suffix_transitions r
      (sink, lrc_initials);
    States.suffix i

  let reach_transitions =
    let preds lrcs = indexset_bind lrcs Lrc.predecessors in
    let succs lrcs = indexset_bind lrcs Lrc.successors in
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
        (*assert (IndexSet.subset lrcs1 lrcs);*)
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
        if IndexSet.disjoint lrc_initials d.config.lrcs then
          regular
        else
          IndexMap.add sink IndexSet.empty regular
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

  let incoming_lrc i =
    match States.prj i with
    | States.Prefix lrc -> IndexSet.singleton lrc
    | States.Reach i -> (Vector.get Reach.states i).config.lrcs
    | States.Suffix i -> snd (Vector.get suffix_transitions i)

  let reach_incoming =
    let incoming desc = IndexSet.map Lrc.lr1_of_lrc desc.Reach.config.lrcs in
    Vector.map incoming Reach.states

  let suffix_incoming =
    let incoming (_, lrcs) = IndexSet.map Lrc.lr1_of_lrc lrcs in
    Vector.map incoming suffix_transitions

  let incoming i =
    match States.prj i with
    | States.Prefix lrc -> IndexSet.singleton (Lrc.lr1_of_lrc lrc)
    | States.Reach i -> Vector.get reach_incoming i
    | States.Suffix i -> Vector.get suffix_incoming i

  let transitions i =
    match States.prj i with
    | States.Prefix lrc ->
      IndexMap.inflate (fun _ -> IndexSet.empty)
        (IndexSet.map States.prefix (Lrc.predecessors lrc))
    | States.Reach i -> Vector.get reach_transitions i
    | States.Suffix i -> fst (Vector.get suffix_transitions i)

  let kind i =
    if i = sink then "sink" else
      match States.prj i with
      | States.Prefix i -> Printf.sprintf "prefix(%d)" (Index.to_int i)
      | States.Reach i  -> Printf.sprintf "reach(%d)"  (Index.to_int i)
      | States.Suffix i ->
        Printf.sprintf "suffix(%d)" (Index.to_int i)

  let () = Stopwatch.leave time
end

module Coverage_check
    (Info : Info.S)
    (Lrc : Lrc.S with module Info := Info)
    (NFA : FAILURE_NFA with type terminal := Info.Terminal.n
                        and type lr1 := Info.Lr1.n
                        and type lrc := Lrc.n)
    (DFA : sig
       open Info
       type n
       val n : n cardinal
       val initial : n index
       val successors : n index -> (Lr1.set * n index) list
       val accept : n index -> Terminal.set
     end)
    ()
=
struct
  open Info
  let time = Stopwatch.enter Stopwatch.main "Coverage check"

  let style = "\
    bgcolor=black;\
    color=white;\
    fontcolor=white;\
    node [color=white fontcolor=white shape=rectangle];\
    edge [color=white fontcolor=white];\
    compound=true;\
  "

  let pts ts = string_of_indexset ~index:Terminal.to_string ts

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
              (Lr1.set_to_string lbl)
        ) (DFA.successors src);
      );
    p "}";
    close_out_noerr oc

  module Domain = struct
    type image = {
      handled: Terminal.set;
      rejected: Terminal.set;
    }

    type t = (NFA.n, image) indexmap

    let empty = IndexMap.empty

    let empty_image = {rejected=IndexSet.empty; handled=IndexSet.empty}

    let increase dfa nfa image rejected domain =
      (* This function increase the image at the intersection of an nfa state
         and a dfa state.
         Everything in dfa.accept and image.handled is handled.

         Everything rejected by the transition is added to the image,
         but everything handled is removed from the image.

         Finally, only the terminals still rejectable are kept in handled.
      *)
      let handled = IndexSet.union image.handled (DFA.accept dfa) in
      let rejected = IndexSet.diff (IndexSet.union rejected image.rejected) handled in
      let rejectable = NFA.rejectable nfa in
      let handled = IndexSet.inter handled rejectable in
      if IndexSet.is_empty rejected && IndexSet.equal handled rejectable then
        (domain, None)
      else
        match IndexMap.find_opt nfa domain with
        | None ->
          let addition = {rejected; handled} in
          (IndexMap.add nfa addition domain, Some addition)
        | Some image ->
          (* rejected grows, handled shrinks *)
          let rejected = IndexSet.union rejected image.rejected in
          let handled = IndexSet.diff (IndexSet.inter handled image.handled) rejected in
          if rejected == image.rejected && handled == image.handled then
            (domain, None)
          else
            let rejected = IndexSet.diff rejected image.rejected in
            (IndexMap.add nfa image domain, Some {rejected; handled})

    let refine i1 i2 =
      {handled = IndexSet.inter i1.handled i2.handled;
       rejected = IndexSet.union i1.rejected i2.rejected}
  end

  let coverage = Vector.make DFA.n Domain.empty

  let delta = ref []

  let safe = true

  let add_delta dfa nfa = function
    | None -> ()
    | Some img ->
      if safe then
        delta := (dfa, IndexMap.singleton nfa img) :: !delta
      else
        match !delta with
        | (dfa', dom) :: rest when dfa == dfa' ->
          let update = function
            | None -> Some img
            | Some img' -> Some (Domain.refine img img')
          in
          let dom = IndexMap.update nfa update dom in
          delta := (dfa, dom) :: rest
        | rest ->
          delta := (dfa, IndexMap.singleton nfa img) :: rest

  let () =
    let grow nfa rejected domain =
      fst (Domain.increase DFA.initial nfa
             Domain.empty_image rejected domain)
    in
    let domain = IndexMap.fold grow NFA.initial Domain.empty in
    push delta (DFA.initial, domain);
    Vector.set coverage DFA.initial domain

  let propagations = ref 0

  let propagate (dfa0, delta) =
    incr propagations;
    List.iter (fun (lr1s, dfa) ->
        let dom = Vector.get coverage dfa in
        let dom' =
          IndexMap.fold (fun nfa0 img dom ->
              if IndexSet.disjoint lr1s (NFA.incoming nfa0) then dom else (
                IndexMap.fold (fun nfa rejected dom ->
                    let dom, delta =
                      Domain.increase dfa nfa img rejected dom
                    in
                    add_delta dfa nfa delta;
                    dom
                  ) (NFA.transitions nfa0) dom
              )
            ) delta dom
        in
        if dom != dom' then
          Vector.set coverage dfa dom';
      ) (DFA.successors dfa0)

  let () = fixpoint ~propagate delta

  let () =
    let reached = ref 0 in
    let intersections = ref 0 in
    let uncovered = ref 0 in
    let unreachable = ref 0 in
    Vector.iteri (fun dfa dom ->
        let size = IndexMap.cardinal dom in
        if size <> 0 then (
          incr reached;
          intersections := !intersections + size;
          List.iter (fun (lr1s, _) ->
              if not (
                  IndexMap.exists
                    (fun nfa _ -> not (IndexSet.disjoint lr1s (NFA.incoming nfa)))
                    dom
                ) then incr unreachable
            )
            (DFA.successors dfa);
        );
        IndexMap.iter (fun nfa _ ->
            let rem =
              List.fold_left (fun lr1s (label, _) -> IndexSet.diff lr1s label)
                (NFA.incoming nfa)
                (DFA.successors dfa)
            in
            if not (IndexSet.is_empty rem) then
              incr uncovered
          ) dom;
      ) coverage;
    Stopwatch.step time
      "Propagated coverage (%d steps, %d reached states, %d intersections, \
       %d uncovered transitions, %d unreachable transition)\n"
      !propagations !reached !intersections !uncovered !unreachable

  let () = Stopwatch.leave time

  let () =
    let oc = open_out_bin "dfa-coverage.dot" in
    let p fmt = Printf.kfprintf (fun oc -> output_char oc '\n') oc fmt in
    p "digraph G {";
    p "  %s" style;
    p "  rankdir=LR;";
    p "  compound=true;";
    p "  uncovered [shape=doubleoctagon];";
    Vector.iteri (fun src cover ->
        let isrc = Index.to_int src in
        let accept = string_of_indexset ~index:Terminal.to_string (DFA.accept src) in
        let accept = if accept = "[]" then "" else accept in
        p "  st%d [label=%S];" isrc (if src = DFA.initial then "initial\n" ^ accept else accept);
        let covered = ref IndexSet.empty in
        let pcover nfa (dom : Domain.image) =
          if true then
            Printf.sprintf "%s:\n%s/%s"
              (NFA.kind nfa)
              (pts dom.rejected)
              (pts (IndexSet.diff
                      (NFA.rejectable nfa)
                      (IndexSet.union dom.rejected dom.handled)))
          else
            Printf.sprintf "%s:\nhandled:%s\nrejected:%s\nrejectable:%s"
              (NFA.kind nfa)
              (pts dom.handled)
              (pts dom.rejected)
              (pts (NFA.rejectable nfa))
        in
        List.iter (fun (lbl, tgt) ->
            covered := IndexSet.union lbl !covered;
            let cover =
              IndexMap.fold (fun nfa dom acc ->
                  if IndexSet.disjoint (NFA.incoming nfa) lbl
                  then acc
                  else pcover nfa dom :: acc
                ) cover []
            in
            let cover = if cover = [] then [] else ""::cover in
            let itgt = Index.to_int tgt in
            p "  st%d -> st%d [label=%S]"
              isrc itgt (String.concat "\n" (Lr1.set_to_string lbl :: cover))
          ) (DFA.successors src);
        IndexMap.iter (fun nfa dom ->
            if not (IndexSet.subset (NFA.incoming nfa) !covered) then (
              p " st%d -> uncovered [label=%S];" isrc
                (Lr1.set_to_string (NFA.incoming nfa) ^"\n"^ pcover nfa dom)
            )
          ) cover
      ) coverage;
    p "}";
    close_out_noerr oc

end
