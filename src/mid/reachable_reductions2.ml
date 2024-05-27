open Utils
open Misc
open Fix.Indexing

let build_with_expensive_assertions = false
let build_safe = true

let style = "\
    bgcolor=black;\
    color=white;\
    fontcolor=white;\
    node [color=white fontcolor=white shape=rectangle];\
    edge [color=white fontcolor=white];\
    compound=true;\
  "


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
    match visit_outer lrcs outer with
    | [] -> [inner]
    | x :: xs -> (inner @ x) :: xs

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
    IndexSet.inter (Lr1.reject lr1) Terminal.regular

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
  val is_bottom : n index -> bool
  val incoming : n index -> lr1 indexset
  val incoming_lrc : n index -> lrc indexset
  val transitions : n index -> (n, terminal indexset) indexmap
  val to_string : n index -> string
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

  let () =
    if build_with_expensive_assertions then (
      Index.iter Lrc.n (fun lrc ->
          let lr1 = Lrc.lr1_of_lrc lrc in
          let lr1s' = Info.Lr1.predecessors lr1 in
          IndexSet.iter (fun lrc' ->
              assert (IndexSet.mem (Lrc.lr1_of_lrc lrc') lr1s')
            ) (Lrc.predecessors lrc)
        );
      let validate {Reach. config; transitions} =
        let rec validate lrcs = function
          | [] -> ()
          | x :: xs ->
            List.iter (fun (reach', _) ->
                let lrcs' = (Vector.get Reach.states reach').config.lrcs in
                if not (IndexSet.subset lrcs' lrcs) then (
                  Printf.eprintf "%s not included in %s\n"
                    (Lrc.set_to_string lrcs')
                    (Lrc.set_to_string lrcs);
                  assert false
                )
              ) x;
            validate (indexset_bind lrcs Lrc.predecessors) xs
        in
        validate config.lrcs transitions
      in
      Vector.iter validate Reach.states
    )

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
      (fun lrc -> IndexSet.is_empty (Lrc.predecessors lrc))
      Lrc.idle

  let bottom =
    States.suffix
      (IndexBuffer.Gen.add suffix_transitions (IndexMap.empty, IndexSet.empty))

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
        (*FIXME: Find out why it is not always a subset*)
        if not (IndexSet.subset lrcs1 lrcs) then (
          Printf.eprintf "expected: %s in %s\n"
            (Lrc.set_to_string lrcs1)
            (Lrc.set_to_string lrcs);
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

  let is_bottom i = i = bottom

  let to_string i =
    if is_bottom i then "bottom" else
      match States.prj i with
      | States.Prefix i -> Printf.sprintf "prefix(%d)" (Index.to_int i)
      | States.Reach  i ->
        let desc = Vector.get Reach.states i in
        Printf.sprintf "reach(%d, %s)"
          (Index.to_int i)
          (Lrc.set_to_string desc.config.lrcs)
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
      Stopwatch.step time
        "Determinized Failure NFA, %d states\n"
        (Hashtbl.length table)
  end

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

  module Image = struct
    type t = {
      handled: Terminal.set;
      rejected: Terminal.set;
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

  module State = Prod(DFA)(NFA)

  type desc = {
    state: State.n index;
    mutable successors: (Lr1.set * Terminal.set * State.n index) list;
    mutable predecessors: (Lr1.set * Terminal.set * State.n index) list;

    uncovered: Lr1.set;
    mutable image: Image.t;
    mutable failing: Terminal.set;
    mutable failing_successors: (Lr1.set * desc) list;

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

  let () = Stopwatch.leave time

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

  let is_immediate_failure desc =
    not (IndexSet.is_empty desc.uncovered) ||
    let _, nfa = State.prj desc.state in
    NFA.is_bottom nfa

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
    let update desc (lbl, _reject, index') =
      let _, nfa' = State.prj index' in
      let desc' = IndexTable.find coverage index' in
      let failing =
        let f1 = IndexSet.inter desc.failing desc'.image.rejected in
        let f2 = IndexSet.inter desc.failing (NFA.rejectable nfa') in
        IndexSet.union f1 (IndexSet.diff f2 desc'.image.handled)
      in
      let delta = IndexSet.diff failing desc'.failing in
      if not (IndexSet.is_empty delta) then (
        desc'.failing <- IndexSet.union failing desc'.failing;
        desc'.failing_successors <- (lbl, desc) :: desc'.failing_successors;
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
    Printf.eprintf "Unhandled lookaheads: %s.\n" (pts initial_failing)

  (* Output failing sub-graph *)
  (*let () =
    let oc = open_out_bin "failing.dot" in
    let p fmt = Printf.kfprintf (fun oc -> output_char oc '\n') oc fmt in
    p "digraph G {";
    p "  %s" style;
    let count = ref 0 in
    IndexTable.iter (fun index desc ->
        if not (IndexSet.is_empty desc.failing) then (
          incr count;
          let dfa, _nfa = State.prj index in
          p "  st%d[label=%S];\n" (index :> int)
            (pts (DFA.accept dfa) (*^ "\n" ^ NFA.to_string nfa*));
          List.iter (fun (failing, lbl, _ts, index') ->
              p "  st%d -> st%d [label=%S];\n"
                (index :> int) (index' : _ index :> int)
                (pts failing ^"\n"^ Lr1.set_to_string lbl)
            ) desc.failing_successors
        )
      ) coverage;
    Printf.eprintf "Coverage graph has %d states, failing sub-graph has %d states.\n"
      (IndexTable.length coverage)
      !count;
    p "}";
    close_out oc*)

  (* Generate sentences and determinize on the fly *)
  let enum_uncovered ~f =
    let initials =
      IndexMap.fold (fun nfa _ acc ->
          let index = State.inj DFA.initial nfa in
          match IndexTable.find_opt coverage index with
          | None -> acc
          | Some desc ->
            if IndexSet.is_empty desc.failing
            then acc
            else desc :: acc
        ) NFA.initial []
    in
    let rec visit path states =
      let failing =
        List.fold_left
          (fun acc desc -> IndexSet.union desc.failing acc)
          IndexSet.empty states
      in
      let transitions =
        List.concat_map (fun desc -> desc.failing_successors) states
        |> IndexRefine.annotated_partition
      in
      let covered =
        List.fold_left
          (fun covered (lbl, states') ->
             IndexSet.union (visit (lbl :: path) states') covered)
          IndexSet.empty transitions
      in
      let uncovered =
        IndexSet.diff failing covered
      in
      if not (IndexSet.is_empty uncovered) then
        f path uncovered failing;
      failing
    in
    visit [] initials

  let _ =
    enum_uncovered ~f:(fun path _uncovered _failing ->
        Printf.eprintf "%s\n"
          (Lr1.list_to_string (List.map IndexSet.choose path))
      )
end
