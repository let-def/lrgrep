open Utils
open Misc
open Fix.Indexing

module type Failure_NFA = sig
  type lr1
  type terminal
  type n
  val n : n cardinal

  val initials : n indexset
  val label : n index -> lr1 indexset
  val next : n index -> n indexset
  val fail : n index -> terminal indexset option
  (* [Some empty]    : the current state doesn't fail (but a prefix may have
                       already failed and there exists a suffix that will fail)
     [Some nonempty] : the current state fails for any of these lookaheads
     [None]          : the current suffix already failed, try again with a
                       shorter suffix
  *)
end

module Make (I : Info.S)(Lrc : Lrc.S with module Info := I)() =
struct
  open I

  let time = Stopwatch.enter Stopwatch.main "Lrc.Make"

  let () = Stopwatch.step time "Computed LRijkstra"

  module Redgraph = struct
    let time = Stopwatch.enter time "Computing Redgraph"

    type transition =
      | Inner of node
      | Outer of {
          pop: int;
          reach: Transition.goto indexset;
        }

    and reduction = {
      production: Production.t;
      lookahead: Terminal.set;
      transition: transition;
    }

    and node = {
      (*index: n index;*)
      lr1: Lr1.t;
      reductions: reduction list;
      mutable accept: Terminal.set;
      mutable partition: Terminal.n IndexSet.Set.t;
      mutable imm_fail : Terminal.set;
      mutable may_fail : Terminal.set;
    }

    type stack =
      | Goto of Lr1.t * stack
      | Bottom of int * Lr1.set

    let count = ref 0

    let initial, goto_transitions =
      let pop = function
        | Goto (_, stack) -> stack
        | Bottom (n, lr1s) -> Bottom (n + 1, Lr1.set_predecessors lr1s)
      in
      let pop_to (n, stack) prod =
        let rec pop_n stack = function
          | 0 -> stack
          | n -> pop_n (pop stack) (n - 1)
        in
        let m = Production.length prod in
        (m, pop_n stack (m - n))
      in
      let rec reductions_at stack lr1 =
        let reductions = simulate_reductions (0, Goto (lr1, stack)) (Lr1.reductions lr1) in
        (*let reservation = IndexBuffer.Gen.reserve nodes in*)
        incr count;
        let result = {
          lr1;
          (*index = IndexBuffer.Gen.index reservation;*)
          accept = IndexSet.empty;
          reductions;
          partition = IndexSet.Set.empty;
          imm_fail = Lr1.reject lr1; (*IndexSet.empty;*)
          may_fail = IndexSet.empty;
        } in
        (*IndexBuffer.Gen.commit nodes reservation result;*)
        result

      and simulate_reductions (n, stack) = function
        | [] -> []
        | (production, lookahead) :: rest ->
          let (n, stack) = pop_to (n, stack) production in
          let result = simulate_reductions (n, stack) rest in
          let transition =
            let lhs = Production.lhs production in
            match stack with
            | Bottom (pop, lr1s) ->
              Outer {pop; reach = IndexSet.map (fun lr1 -> Transition.find_goto lr1 lhs) lr1s}
            | Goto (lr1, _) ->
              Inner (reductions_at stack (Transition.find_goto_target lr1 lhs))
          in
          {production; lookahead; transition} :: result
      in
      let initial =
        IndexMap.inflate
          (fun lr1 -> reductions_at (Bottom (1, Lr1.predecessors lr1)) lr1)
          Lr1.idle
      and goto_transitions =
        Vector.init Transition.goto @@ fun tr ->
        let any = Transition.of_goto tr in
        let src = Transition.source any in
        let tgt = Transition.target any in
        reductions_at (Goto (src, Bottom (1, Lr1.predecessors src))) tgt
      in
      (initial, goto_transitions)

    let () = Stopwatch.step time "Generated %d nodes" !count

    let () =
      let todo = Vector.make Transition.goto IndexSet.Set.empty in
      let todo_dom = ref IndexSet.empty in
      let propagate_goto la tr =
        let node = Vector.get goto_transitions tr in
        if not (IndexSet.Set.mem la node.partition) then (
          let todo' = Vector.get todo tr in
          if IndexSet.Set.is_empty todo' then
            todo_dom := IndexSet.add tr !todo_dom;
          Vector.set todo tr (IndexSet.Set.add la todo');
        )
      in
      let rec propagate_node la node =
        if not (IndexSet.Set.mem la node.partition) then (
          node.partition <- IndexSet.Set.add la node.partition;
          List.iter (propagate_reduction la) node.reductions;
        )
      and propagate_reduction la red =
        let la = Terminal.intersect la red.lookahead in
        if not (IndexSet.is_empty la) then
          match red.transition with
          | Inner node -> propagate_node la node
          | Outer t -> IndexSet.iter (propagate_goto la) t.reach
      in
      IndexMap.iter
        (fun _ node -> propagate_node Terminal.all node)
        initial;
      while not (IndexSet.is_empty !todo_dom) do
        let todo_dom' = !todo_dom in
        todo_dom := IndexSet.empty;
        IndexSet.iter (fun tr ->
            let todo' = Vector.get todo tr in
            Vector.set todo tr IndexSet.Set.empty;
            IndexSet.Set.iter
              (fun la -> propagate_node la (Vector.get goto_transitions tr)) todo'
          ) todo_dom'
      done

    let () = Stopwatch.step time "Propagated lookaheads"

    let () =
      let rec normalize_node node =
        let partition =
          IndexRefine.partition
            (IndexSet.Set.elements node.partition)
        in
        node.partition <- IndexSet.Set.of_list partition;
        (*node.imm_fail <-
          IndexSet.(diff (diff Terminal.all total) node.accept);*)
        node.reductions |> List.iter @@ fun red ->
        match red.transition with
        | Inner node -> normalize_node node
        | Outer _ -> ()
      in
      IndexMap.iter (fun _ node -> normalize_node node) initial;
      Vector.iter normalize_node goto_transitions

    let () = Stopwatch.step time "Partitioned lookaheads"

    let failures =
      let rec failures node =
        if IndexSet.is_empty node.may_fail then (
          node.may_fail <- Terminal.all;
          update_failures node;
        );
        node.may_fail
      and update_failures node =
        node.may_fail <-
          IndexSet.diff
            (List.fold_left next_failures IndexSet.empty node.reductions)
            node.accept;

      and next_failures acc red =
        IndexSet.union acc @@
        Terminal.intersect red.lookahead @@
        match red.transition with
        | Inner node -> failures node
        | Outer {reach; pop=_} ->
          IndexSet.fold
            (fun goto acc -> IndexSet.union acc (failures (Vector.get goto_transitions goto)))
            reach IndexSet.empty
      in
      (*TODO: look for a fixed point
        For OCaml grammar:
        first pass removes 9327 partitions
        with 4-passes, we can remove up to 9363 partitions
      *)
      failures

    let () =
      let filtered = ref 0 in
      let filter_node node =
        let failures = failures node in
        node.partition <- IndexSet.Set.filter_map (fun set ->
            let set = IndexSet.inter set failures in
            if IndexSet.is_empty set then
              (incr filtered; None)
            else
              Some set
          ) node.partition;
        node.accept <- IndexSet.union
            (Lr1.shift_on node.lr1)
            (IndexSet.diff (Lr1.reduce_on node.lr1) node.may_fail)
      in
      let rec filter_tree node =
        filter_node node;
        List.iter
          (fun red -> match red.transition with
             | Inner node -> filter_tree node
             | Outer _ -> ())
          node.reductions
      in
      IndexMap.iter (fun _ node -> filter_tree node) initial;
      Vector.iter filter_tree goto_transitions;
      Stopwatch.step time "Filtered %d non-failing partitions" !filtered

    (* Compute direct access to outer transitions *)

    type outer_transition = {
      steps: reduction list;
      pop: int;
      reach: Transition.goto indexset;
      lookahead: Terminal.set;
    }

    let compute_outer_transitions =
      let rec visit_node path la acc node =
        List.fold_left (visit_reduction path la) acc node.reductions
      and visit_reduction path la acc (r : reduction) =
        let la = Terminal.intersect la r.lookahead in
        assert (not (IndexSet.is_empty la));
        match r.transition with
        | Inner node -> visit_node (r :: path) la acc node
        | Outer {pop; reach}  ->
          { steps = r :: path; pop; reach; lookahead = la} :: acc
      in
      fun node ->
        let trs = visit_node [] Terminal.all [] node in
        let trs = List.sort (fun t1 t2 -> Int.compare t1.pop t2.pop) trs in
        (node, trs)

    let initial_outer =
      IndexMap.map compute_outer_transitions initial

    let goto_outer =
      Vector.map compute_outer_transitions goto_transitions

    let () = Stopwatch.leave time
  end

  module Redgraph_la : sig
    type n
    val n : n cardinal
    val initials : n indexset
    val next : n index -> n indexset
    val label : n index -> Lr1.t option
    val fail : n index -> Terminal.set
  end = struct

    include IndexBuffer.Gen.Make()

    type t = {
      index: n index;
      label: Lr1.t option;
      fail: Terminal.set;
      mutable next: n indexset;
    }

    let nodes = get_generator ()

    let imported_parts = Vector.make Transition.goto IndexSet.Map.empty

    let rec import_goto goto la =
      let node = Vector.get Redgraph.goto_transitions goto in
      let nodes =
        IndexSet.Set.fold (fun la' acc ->
            assert (IndexSet.disjoint la' la || IndexSet.subset la' la);
            if not (IndexSet.quick_subset la' la) then acc else
              IndexSet.add (import_part goto la') acc
          ) node.partition IndexSet.empty
      in
      nodes

    and import_part (goto : Transition.goto index) la =
      let parts = Vector.get imported_parts goto in
      match IndexSet.Map.find_opt la parts with
      | Some index -> index
      | None ->
        let node, transitions = Vector.get Redgraph.goto_outer goto in
        let reservation = IndexBuffer.Gen.reserve nodes in
        let source = Transition.source (Transition.of_goto goto) in
        let index = IndexBuffer.Gen.index reservation in
        let node = {
          index;
          label = Some source;
          next = IndexSet.empty;
          fail = Terminal.intersect la node.imm_fail;
        } in
        IndexBuffer.Gen.commit nodes reservation node;
        Vector.set imported_parts goto (IndexSet.Map.add la index parts);
        node.next <- import_transitions la (Lr1.predecessors source) 1 transitions;
        index

    and import_transitions la lr1s n = function
      | [] -> IndexSet.empty
      | (tr : Redgraph.outer_transition) :: trs when n = tr.pop ->
        let rest = import_transitions la lr1s n trs in
        if IndexSet.disjoint la tr.lookahead
        then rest
        else IndexSet.union rest (indexset_bind tr.reach (fun g ->
            let lr1 = Transition.source (Transition.of_goto g) in
            if not (IndexSet.mem lr1 lr1s) then (
              Printf.eprintf "expected %s to be a member of %s\n%s\n"
                (Lr1.to_string lr1)
                (Lr1.set_to_string lr1s)
                (Format.asprintf "%a"
                   Grammar.Print.itemset
                   (List.concat_map
                      (fun lr1 ->
                         List.map (fun (p, pos) -> (Production.to_g p, pos)) (Lr1.items lr1))
                      (IndexSet.elements lr1s)))
              ;
              assert false
            );
            import_goto g la
          ))
      | transitions ->
        let targets = import_transitions la (Lr1.set_predecessors lr1s) (n + 1) transitions in
        if IndexSet.is_empty targets then
          IndexSet.empty
        else
          let reservation = IndexBuffer.Gen.reserve nodes in
          let index = IndexBuffer.Gen.index reservation in
          let node = {index; label = None; next = targets; fail = IndexSet.empty} in
          IndexBuffer.Gen.commit nodes reservation node;
          IndexSet.singleton index

    let import_initial lr1 ((node : Redgraph.node), trs) =
      let reservation = IndexBuffer.Gen.reserve nodes in
      let next = import_transitions Terminal.all (Lr1.predecessors lr1) 1 trs in
      let index = IndexBuffer.Gen.index reservation in
      let node = {index; label = Some lr1; next; fail = node.imm_fail} in
      IndexBuffer.Gen.commit nodes reservation node;
      index

    let initials =
      IndexMap.fold
        (fun lr1 def acc -> import_initial lr1 def :: acc)
        Redgraph.initial_outer []
      |> List.rev
      |> IndexSet.of_list

    let nodes = IndexBuffer.Gen.freeze nodes

    let fail n = (Vector.get nodes n).fail
    let next n = (Vector.get nodes n).next
    let label n = (Vector.get nodes n).label

    let () =
      Stopwatch.step time "Lookahead-specialized redgraph with %d nodes" (cardinal n)
  end

  module Redgraph_lrc_la : sig
    type n
    val n : n cardinal
    val initials : n indexset
    val next : n index -> n indexset
    val label : n index -> Lr1.set
    val fail : n index -> Terminal.set
    val lrcs : n index -> Lrc.set
  end = struct
    type 'n t = {
      index: 'n index;
      label: Lr1.set;
      fail: Terminal.set;
      mutable next: 'n indexset;
      lrcs: Lrc.set;
    }

    module Full = IndexBuffer.Gen.Make()
    let nodes = Full.get_generator ()

    let table = Hashtbl.create 7

    let normalize lrcs la =
      match Redgraph_la.label la with
      | None -> lrcs
      | Some lr1 -> IndexSet.inter lrcs (Lrc.lrcs_of_lr1 lr1)

    let rec import lrcs la =
      let lrcs = normalize lrcs la in
      if IndexSet.is_empty lrcs then
        None
      else
        let key = (la, lrcs) in
        match Hashtbl.find_opt table key with
        | Some result -> Some result
        | None ->
          let label = match Redgraph_la.label la with
            | None -> IndexSet.map Lrc.lr1_of_lrc lrcs
            | Some lr1 -> IndexSet.singleton lr1
          in
          let reservation = IndexBuffer.Gen.reserve nodes in
          let index = IndexBuffer.Gen.index reservation in
          Hashtbl.add table key index;
          let result = {
            index; label; lrcs;
            next = IndexSet.empty;
            fail = Redgraph_la.fail la;
          } in
          IndexBuffer.Gen.commit nodes reservation result;
          let lrcs = indexset_bind lrcs Lrc.predecessors in
          let las = Redgraph_la.next la in
          let next = IndexSet.filter_map (import lrcs) las in
          result.next <- next;
          Some index

    let full_initials =
      IndexSet.filter_map (fun la ->
          let lrcs = Lrc.lrcs_of_lr1 (Option.get (Redgraph_la.label la)) in
          import lrcs la
        ) Redgraph_la.initials

    let nodes = IndexBuffer.Gen.freeze nodes

    let () = Stopwatch.step time "la and lrc intersection, with %d nodes" (cardinal Full.n)

    module Live = Gensym()

    let live_map =
      let reverse = Vector.make Full.n IndexSet.empty in
      Vector.iter (fun t ->
          IndexSet.iter (fun t' -> vector_set_add reverse t' t.index) t.next;
        ) nodes;
      let map = Vector.make Full.n None in
      let rec reach t =
        if Option.is_none (Vector.get map t) then (
          Vector.set map t (Some (Live.fresh ()));
          IndexSet.iter reach (Vector.get reverse t)
        )
      in
      Vector.iter (fun t ->
          if not (IndexSet.is_empty t.fail) then
            reach t.index
        ) nodes;
      map

    let live_index n =
      Vector.get live_map n

    let () =
      Stopwatch.step time "%d live nodes (%d dead-ends)"
        (cardinal Live.n) (cardinal Full.n - cardinal Live.n)

    include Live

    let fail = Vector.make n IndexSet.empty
    let next = Vector.make n IndexSet.empty
    let label = Vector.make n IndexSet.empty
    let lrcs = Vector.make n IndexSet.empty

    let () = Vector.iteri (fun full live ->
        match live with
        | None -> ()
        | Some live ->
          let node = Vector.get nodes full in
          Vector.set fail  live node.fail;
          Vector.set next  live (IndexSet.filter_map live_index node.next);
          Vector.set label live node.label;
          Vector.set lrcs  live node.lrcs;
      ) live_map

    let initials = IndexSet.filter_map live_index full_initials
    let fail  = Vector.get fail
    let next  = Vector.get next
    let label = Vector.get label
    let lrcs  = Vector.get lrcs
  end

  module type Failure_NFA =
    Failure_NFA with type lr1 := Lr1.n
                 and type terminal := Terminal.n

  module Lrc_NFA : Failure_NFA with type n = Lrc.n = struct
    include Lrc
    let initials = indexset_bind Lr1.idle Lrc.lrcs_of_lr1
    let label i = IndexSet.singleton (lr1_of_lrc i)
    let next = predecessors
    let fail _ = None
  end

  module Lrce : Failure_NFA = struct
    include Sum(Lrc)(Redgraph_lrc_la)

    let initials = IndexSet.map inj_r Redgraph_lrc_la.initials

    let rednext =
      Vector.init Redgraph_lrc_la.n begin fun i ->
        let base = IndexSet.map inj_r (Redgraph_lrc_la.next i) in
        if IndexSet.is_empty (Redgraph_lrc_la.fail i) then
          base
        else
          IndexSet.union base
            (IndexSet.map inj_l
               (indexset_bind (Redgraph_lrc_la.lrcs i) Lrc_NFA.next))
      end

    let next n =
      match prj n with
      | L lrc -> IndexSet.unsafe_to_indexset (Lrc.predecessors lrc)
      | R red -> Vector.get rednext red

    let label n =
      match prj n with
      | L lrc -> Lrc_NFA.label lrc
      | R red -> Redgraph_lrc_la.label red

    let fail n =
      match prj n with
      | L _ -> None
      | R red -> Some (Redgraph_lrc_la.fail red)
  end

  let () = Stopwatch.leave time
end

module Make2
    (I : Info.S)
    (Viable: Viable_reductions.S with module Info := I)
    (Lrc: Lrc.S with module Info := I)
    () :
sig
  open I
  type state
  val state : state cardinal

  type config = { source : Viable.state index; lrcs : Lrc.set; }
  type transitions = {
    inner : state indexset;
    outer : state indexset list;
  }

  val initial : transitions Lrc.map
  val states : (state, config * transitions) vector

  val successors : (state, state indexset) vector
  val predecessors : (state, state indexset) vector

  val iter_targets : transitions -> (state index -> unit) -> unit
  val rev_iter_targets : transitions -> (state index -> unit) -> unit
  val fold_targets : (state index -> 'a -> 'a) -> transitions -> 'a -> 'a
  val rev_fold_targets : ('a -> state index -> 'a) -> 'a -> transitions -> 'a

  val immediate_fails : state index -> Terminal.set
  val indirect_fails : state index -> Terminal.set
end =
struct
  open I
  let time = Stopwatch.enter Stopwatch.main "Reachable reductions (2)"

  (* Check that lookaheads are decreasing in redgraph

    let () =
      let open Viable in
      let iter_cands f step =
        List.iter (fun {target; lookahead; _} -> f target lookahead) step.candidates
      in
      let visit_state f st =
        let {inner; outer} = Viable.get_transitions st in
        List.iter (iter_cands f) inner;
        List.iter (iter_cands f) outer
      in
      Index.iter Viable.state (visit_state (fun target la ->
          visit_state
            (fun _ la' -> assert (IndexSet.subset la' la))
            target
        ))
  *)

  module State = IndexBuffer.Gen.Make()
  type state = State.n
  let state = State.n

  type config = {
    source: Viable.state index;
    lrcs: Lrc.set;
  }

  type transitions = {
    inner: state indexset;
    outer: state indexset list;
  }

  let states = State.get_generator ()

  let nodes = Hashtbl.create 7

  let rec visit_transitions lrcs {Viable.inner; outer} =
    let inner =
      List.fold_left (fun acc {Viable.candidates; _} ->
          List.fold_left
            (fun acc {Viable.target; lookahead=_; filter=()} ->
               IndexSet.add (visit_config {source=target; lrcs}) acc)
            acc candidates
        ) IndexSet.empty inner
    in
    let outer = match outer with
      | first :: rest -> visit_outer lrcs first rest
      | [] -> []
    in
    {inner; outer}

  and visit_config config =
    match Hashtbl.find_opt nodes config with
    | Some state -> state
    | None ->
      let reservation = IndexBuffer.Gen.reserve states in
      let index = IndexBuffer.Gen.index reservation in
      Hashtbl.add nodes config index;
      let transitions = Viable.get_transitions config.source in
      IndexBuffer.Gen.commit states reservation
        (config, visit_transitions config.lrcs transitions);
      index

  and visit_outer lrcs {Viable.candidates; _} rest =
    let visit_candidate {Viable.target; lookahead=_; filter=lr1s} =
      let compatible_lrc lr1 = IndexSet.inter lrcs (Lrc.lrcs_of_lr1 lr1) in
      let lrcs = indexset_bind lr1s compatible_lrc in
      if IndexSet.is_empty lrcs
      then None
      else Some (visit_config {lrcs; source=target})
    in
    let candidates =
      IndexSet.of_list (List.filter_map visit_candidate candidates)
    in
    match rest with
    | [] -> [candidates]
    | next :: rest ->
      candidates :: visit_outer (indexset_bind lrcs Lrc.predecessors) next rest

  let initial =
    let process lrc =
      visit_transitions
        (Lrc.predecessors lrc)
        (Vector.get Viable.initial (Lrc.lr1_of_lrc lrc))
    in
    IndexMap.inflate process Lrc.idle

  let states = IndexBuffer.Gen.freeze states

  let () = Stopwatch.step time "Nodes: %d" (cardinal state)

  let iter_targets {inner; outer} f =
    IndexSet.iter f inner;
    List.iter (IndexSet.iter f) outer

  let rev_iter_targets {inner; outer} f =
    IndexSet.rev_iter f inner;
    List.iter (IndexSet.rev_iter f) (List.rev outer)

  let fold_targets f {inner; outer} acc =
    let acc = IndexSet.fold f inner acc in
    List.fold_left
      (fun acc step -> IndexSet.fold f step acc)
      acc outer

  let rev_fold_targets f acc {inner; outer} =
    let acc = IndexSet.fold_right f acc inner in
    List.fold_right
      (fun set acc -> IndexSet.fold_right f acc set)
      outer acc

  let successors =
    Vector.map (fun (_config, transitions) ->
        rev_fold_targets
          (fun acc st -> IndexSet.add st acc)
          IndexSet.empty transitions
      ) states

  let predecessors =
    Misc.relation_reverse successors

  (* Full failures computation *)

  let immediate_fails st =
    let config, _ = Vector.get states st in
    let viable = Viable.get_config config.source in
    Lr1.reject viable.top

  let indirect_fails =
    Vector.init state immediate_fails

  let () =
    let propagate _src fail1 _tgt fail2 =
      IndexSet.union fail1 fail2
    in
    Misc.fixpoint predecessors indirect_fails ~propagate

  (* Active lookahead restricted failures -- meaningless
    let immediate_fails = Vector.map (fun (config, _) ->
        let viable = Viable.get_config config.source in
        Terminal.intersect viable.lookahead (Lr1.reject viable.top)
      ) states

    let indirect_fails = Vector.copy immediate_fails

    let update_indirect_fails index =
      let _, transitions = Vector.get states index in
      fold_targets (fun target acc ->
          IndexSet.union acc (Vector.get indirect_fails target)
        ) transitions (Vector.get indirect_fails index)

    let () =
      let todo = ref [] in
      let update index =
        let fail0 = Vector.get indirect_fails index in
        let fail = IndexSet.union (update_indirect_fails index) fail0 in
        if not (IndexSet.equal fail0 fail) then (
          push todo index;
          Vector.set indirect_fails index fail
        )
      in
      Index.iter state update;
      let rec loop () =
        match !todo with
        | [] -> ()
        | todo' ->
          todo := [];
          List.iter update todo';
          loop ()
      in
      loop ()

  *)

  let indirect_fails = Vector.get indirect_fails

  let () =
    let immediate = ref 0 in
    let propagated = ref 0 in
    Index.iter state (fun st ->
        let cfail = IndexSet.cardinal (immediate_fails st) in
        let cfail' = IndexSet.cardinal (indirect_fails st) in
        immediate := !immediate + cfail;
        propagated := !propagated + cfail' - cfail;
      );
    Stopwatch.step time
      "Guaranteed failures (%d immediates, %d propagated)"
      !immediate !propagated

  (* Check decreasing lookaheads invariant
  let () =
    let get_lookahead config =
      (Viable.get_config config.source).lookahead
    in
    Vector.iter (fun (config, transitions) ->
        let la = get_lookahead config in
        iter_targets transitions (fun target ->
            let la' = get_lookahead (fst (Vector.get states target)) in
            assert (IndexSet.subset la' la);
      )
    ) states*)

  let () = Stopwatch.leave time

end
