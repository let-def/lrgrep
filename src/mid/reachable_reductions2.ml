open Utils
open Misc
open Fix.Indexing

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

  let () = Stopwatch.leave time

  (*module Brute = struct
    type thread = {
      node: n index;
      rejected: Terminal.set;
      visited: n indexset;
    }

    let steps = ref 0

    let queue = ref []

    let start_thread node =
      {node; rejected = reject node; visited = IndexSet.singleton node}

    let visit_node thread node =
      if not (IndexSet.mem node thread.visited) then (
        incr steps;
        let rejected = IndexSet.union (reject node) thread.rejected in
        let visited =
          if IndexSet.equal rejected thread.rejected then
            IndexSet.add node thread.visited
          else
            IndexSet.singleton node
        in
        push queue {node; rejected; visited}
      )

    let () = IndexMap.iter (fun _ node -> push queue (start_thread node)) initial

    let propagate thread =
      IndexSet.iter
        (visit_node thread)
        (Vector.get successors thread.node)

    let rec fixpoint i = match !queue with
      | [] -> ()
      | todo' ->
        Printf.eprintf "iteration %d: %d threads\n%!" i (List.length todo');
        queue := [];
        List.iter propagate todo';
        fixpoint (i + 1)

    let () = fixpoint 0

    let () = Printf.eprintf "Bruteforced in %d steps \n" !steps
  end*)

  (*let () =
    Vector.iter (fun desc ->
        match Source.prj desc.config.source with
        | R _ -> ()
        | L viable ->
          let config = Viable.get_config viable in
          let lr1 = List.fold_left (fun _ x -> x) config.top config.rest in
          IndexSet.iter (fun lrc ->
              assert (IndexSet.mem (Lrc.lr1_of_lrc lrc) (Lr1.predecessors lr1));
              if Lrc.lr1_of_lrc lrc <> lr1 then
                Printf.eprintf "Expecting to have state %s at the top of the stack but got %s\n"
                  (Lr1.to_string lr1)
                  (Lr1.to_string (Lrc.lr1_of_lrc lrc))
            )
            desc.config.lrcs
      ) states
    *)

  module Arborescence = struct
    let marked = Boolvector.make n false
    let reject_before = Vector.make n IndexSet.empty

    let nodes = ref 0

    type node = {
      state: n index;
      rejected: Terminal.set;
      mutable handle: Terminal.set;
      mutable children: node list;
    }

    let queue = ref []

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
        let node = {state; rejected; handle = IndexSet.empty; children = []} in
        push queue node;
        node :: acc
      ) else
        acc

    let roots = IndexMap.fold (fun _ -> visit IndexSet.empty) initial []

    let propagate node =
      let next = Vector.get successors node.state in
      node.children <- IndexSet.fold (visit node.rejected) next node.children

    let () = fixpoint ~propagate queue

    let () = Printf.eprintf "Arborescence has %d nodes\n" !nodes
  end

  module Goal = struct
    include IndexBuffer.Gen.Make()

    let lr0_stack_of_source source =
      match Source.prj source with
      | L v -> List.rev_map Lr1.to_lr0 (Viable.get_stack v)
      | R lr1 -> [Lr1.to_lr0 lr1]

    let stack_def = get_generator ()

    let stack_of_state =
      let table = Hashtbl.create 7 in
      let index_of lr0s =
        match Hashtbl.find_opt table lr0s with
        | Some i -> i
        | None ->
          let r = IndexBuffer.Gen.reserve stack_def in
          let i = IndexBuffer.Gen.index r in
          Hashtbl.add table lr0s i;
          IndexBuffer.Gen.commit stack_def r lr0s;
          i
      in
      let project desc = index_of (lr0_stack_of_source desc.config.source) in
      Vector.get (Vector.map project states)

    let _stack_def = IndexBuffer.Gen.freeze stack_def

    let () = Printf.eprintf "Found %d configurations to cover\n" (cardinal n)

    let goal = Vector.make n IndexSet.empty

    let () = Index.iter (Vector.length states) (fun st ->
        let ra = reject st in
        let rb, _ = rejectable st in
        let rc = Vector.get Arborescence.reject_before st in
        let r = IndexSet.union ra (IndexSet.union rb rc) in
        vector_set_union goal (stack_of_state st) r
      )
  end

  module Enum = struct
    let sentences = ref 0

    let get_child (node : Arborescence.node) state =
      let pred (node' : Arborescence.node) = equal_index node'.state state in
      match List.find_opt pred node.children with
      | Some node' -> node'
      | None ->
        let rejected = IndexSet.union (reject state) node.rejected in
        let node' =
          {Arborescence. state; rejected; handle = IndexSet.empty; children = []}
        in
        incr Arborescence.nodes;
        node.children <- node' :: node.children;
        node'

    let rec visit_after (node : Arborescence.node) todo =
      let stack = Goal.stack_of_state node.state in
      Vector.set Goal.goal stack
        (IndexSet.diff (Vector.get Goal.goal stack) todo);
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
        node.handle <- todo;
        incr sentences;
      )

    let rec visit_before node =
      let visited =
        List.fold_left
          (fun acc node' -> IndexSet.union (visit_before node') acc)
          IndexSet.empty node.Arborescence.children
      in
      let stack = Goal.stack_of_state node.state in
      let rejectable, _ = rejectable node.state in
      let remaining = Vector.get Goal.goal stack in
      let remaining = IndexSet.diff remaining visited in
      Vector.set Goal.goal stack remaining;
      let todo = IndexSet.inter rejectable remaining in
      visit_after node todo;
      IndexSet.union visited todo

    let () =
      let process node = ignore (visit_before node) in
      List.iter process Arborescence.roots

    let () = Printf.eprintf "Enumeration has %d sentences\n" !sentences
  end

  (*module Enum = struct
    let () =
      (* Now traverse arborescence ... *)
      let rec visit path node =
        let path = node :: path in
        let visited =
          List.fold_left
            (fun acc node' -> IndexSet.union (visit path node') acc)
            IndexSet.empty node.Arborescence.children
        in
        let stack = stack_of_state node.state in
        if not (IndexSet.is_empty visited) then
          assert (IndexSet.subset node.rejected visited);
        let remaining = IndexSet.diff (Vector.get goal stack) visited in
        if not (IndexSet.disjoint
      in
      List.iter
        (fun node -> ignore (visit [] node))
        Arborescence.initial
  end*)
end
