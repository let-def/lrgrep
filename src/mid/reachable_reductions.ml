open Utils
open Misc
open Fix.Indexing
open Monotonous

module type S = sig
  module Info : Info.S
  module Viable: Viable_reductions.S with module Info := Info
  module Lrc : Lrc.S with module Info := Info
  open Info

  include CARDINAL

  module Source : SUM with type l := Viable.n and type r := Lrc.n

  type config = {
    source: Source.n index;
    lrcs : Lrc.set;
    accepted: Terminal.set;
    rejected: Terminal.set;
  }

  type target = n index * Reduction.t

  type transitions = {
    inner: target list;
    outer: target list list;
  }

  type desc = {
    config: config;
    transitions: transitions;
  }

  val initial : n index Lrc.map
  val states : (n, desc) vector

  val successors : (n, n indexset) vector
  val predecessors : (n, n indexset) vector

  val accepted : n index -> Terminal.set
  val rejected : n index -> Terminal.set
  val potential_reject : n index -> Terminal.set

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
  let time = Stopwatch.enter Stopwatch.main "Reachable reductions (3)"

  module Source = Sum(Viable)(Lrc)

  include IndexBuffer.Gen.Make()

  type config = {
    source: Source.n index;
    lrcs : Lrc.set;
    accepted: Terminal.set;
    rejected: Terminal.set;
  }

  type target = n index * Reduction.t

  type transitions = {
    inner: target list;
    outer: target list list;
  }

  type desc = {
    config: config;
    transitions: transitions;
  }

  let states = get_generator ()

  let nodes = Hashtbl.create 7

  let make_config ~accepted ~rejected lrcs source =
    let lr1 =
      match Source.prj source with
      | L viable -> (Viable.get_config viable).top
      | R lrc -> Lrc.lr1_of_lrc lrc
    in
    let a =
      IndexSet.union accepted
        (IndexSet.diff (Lr1.shift_on lr1) rejected)
    and r =
      IndexSet.union rejected
        (IndexSet.diff (Lr1.reject lr1) accepted)
    in
    {accepted=a; rejected=r; source; lrcs}

  let rec visit_config ~accepted ~rejected lrcs source =
    let config = make_config ~accepted ~rejected lrcs source in
    match Hashtbl.find_opt nodes config with
    | Some state -> state
    | None ->
      let reservation = IndexBuffer.Gen.reserve states in
      let index = IndexBuffer.Gen.index reservation in
      Hashtbl.add nodes config index;
      let {accepted; rejected; source; lrcs} = config in
      let transitions =
        match Source.prj source with
        | L viable ->
          visit_transitions ~accepted ~rejected lrcs
            (Viable.get_transitions viable)
        | R lrc ->
          match Vector.get Viable.initial (Lrc.lr1_of_lrc lrc) with
          | [] -> {inner=[]; outer=[]}
          | outer0 :: outer ->
            {inner=[]; outer=visit_outer ~accepted ~rejected lrcs outer0 outer}
      in
      IndexBuffer.Gen.commit states reservation {config; transitions};
      index

  and visit_transitions ~accepted ~rejected lrcs {Viable.inner; outer} =
    (*let  = Viable.get_transitions config.viable in*)
    let inner =
      List.fold_left (fun acc {Viable.candidates; _} ->
        List.fold_left
          (fun acc {Viable.target; lookahead=_; filter=(); reduction} ->
            let state = visit_config ~accepted ~rejected lrcs (Source.inj_l target) in
            (state, reduction) :: acc)
          acc candidates
      ) [] inner
    in
    let outer = match outer with
      | first :: rest -> visit_outer ~accepted ~rejected lrcs first rest
      | [] -> []
    in
    {inner; outer}

  and visit_outer ~accepted ~rejected lrcs {Viable.candidates; _} rest =
    let visit_candidate {Viable.target; lookahead=_; filter=lr1s; reduction} =
      let compatible_lrc lr1 = IndexSet.inter lrcs (Lrc.lrcs_of_lr1 lr1) in
      let lrcs = indexset_bind lr1s compatible_lrc in
      if IndexSet.is_empty lrcs
      then None
      else Some (visit_config ~accepted ~rejected lrcs (Source.inj_l target), reduction)
    in
    let candidates = List.filter_map visit_candidate candidates in
    match rest with
    | [] -> [candidates]
    | next :: rest ->
      candidates ::
      visit_outer ~accepted ~rejected (indexset_bind lrcs Lrc.predecessors) next rest

  let initial =
    let process lrc =
      let lr1 = Lrc.lr1_of_lrc lrc in
      let accepted = Lr1.shift_on lr1 in
      let rejected = Lr1.reject lr1 in
      visit_config ~accepted ~rejected
        (IndexSet.singleton lrc)
        (Source.inj_r lrc)
    in
    IndexMap.inflate process Lrc.idle

  let states = IndexBuffer.Gen.freeze states

  let () = Stopwatch.step time "Nodes: %d" (cardinal n)

  let iter_targets {inner; outer} f =
    List.iter f inner;
    List.iter (List.iter f) outer

  let rev_iter_targets {inner; outer} f =
    list_rev_iter f inner;
    list_rev_iter (list_rev_iter f) outer

  let fold_targets f acc {inner; outer} =
    let acc = List.fold_left f acc inner in
    let acc = List.fold_left (List.fold_left f) acc outer in
    acc

  let rev_fold_targets f {inner; outer} acc =
    let acc = List.fold_right f inner acc in
    let acc = List.fold_right (List.fold_right f) outer acc in
    acc

  let successors =
    Vector.map (fun desc ->
        rev_fold_targets
          (fun (st, _) acc -> IndexSet.add st acc)
          desc.transitions IndexSet.empty
      ) states

  let predecessors =
    Misc.relation_reverse successors

  let accepted st = (Vector.get states st).config.accepted
  let rejected st = (Vector.get states st).config.rejected

  let potential_reject =
    let table = Vector.init n rejected in
    let widen _src rej1 _tgt rej2 =
      (*assert (IndexSet.disjoint rej1 (accepted tgt));*)
      IndexSet.union rej1 rej2
    in
    Misc.fixpoint predecessors table ~propagate:widen;
    Vector.get table

  let () = Stopwatch.leave time

end

module FailureNFA
    (Info : Info.S)
    (Viable: Viable_reductions.S with module Info := Info)
    (Lrc: Lrc.S with module Info := Info)
    (Reach : S with module Info := Info
                and module Viable := Viable
                and module Lrc := Lrc)
    ()
=
struct

  let time = Stopwatch.enter Stopwatch.main "FailureNFA"

  let has_extra_rejections st =
    not (IndexSet.equal (Reach.potential_reject st) (Reach.rejected st))

  (* Potential optimization:

  let has_strong_extra_rejections st =
    (Reach.potential_reject st) != (Reach.rejected st)

  let () =
    Index.iter Reach.n (fun st ->
      assert (has_extra_rejections st = has_strong_extra_rejections st))
  *)

  let extra_transitions st =
    let open Reach in
    let filter xs = List.filter (fun (st, _) -> has_extra_rejections st) xs in
    let rec filter_trim = function
      | [] -> []
      | x :: xs ->
        match filter x, filter_trim xs with
        | [], [] -> []
        | x', xs' -> x' :: xs'
    in
    let {transitions; _} = Vector.get states st in
    let inner = filter transitions.inner in
    let outer = filter_trim transitions.outer in
    {inner; outer}

  let lrcs_of st = (Vector.get Reach.states st).config.lrcs

  open Info

  type tree =
    Node of Lrc.set * Terminal.set * Reach.n indexset * tree list

  let rec merge_tree l1 l2 = match l1, l2 with
    | [], xs | xs, [] -> xs
    | (Node (s1, j1, r1, t1) as n1) :: l1',
      (Node (s2, j2, r2, t2) as n2) :: l2' ->
      let c = IndexSet.compare s1 s2 in
      if c < 0 then
        n1 :: merge_tree l1' l2
      else if c > 0 then
        n2 :: merge_tree l1 l2'
      else
        Node (s1, IndexSet.union j1 j2, IndexSet.union r1 r2, merge_tree t1 t2) :: merge_tree l1' l2'

  let order_node (Node (s1, _, _, _)) (Node (s2, _, _, _)) =
    IndexSet.compare s1 s2

  let rec merge_trees ls =
    let ls = List.sort order_node ls in
    let rec visit_many s j r t = function
      | Node (s', j', r', t') :: rest when IndexSet.equal s s' ->
        visit_many s (IndexSet.union j j') (IndexSet.union r r') (t' @ t) rest
      | rest ->
        Node (s, j, r, merge_trees t) :: visit_one rest
    and visit_one = function
      | Node (s, j, r, t) :: Node (s', j', r', t') :: rest when IndexSet.equal s s' ->
        visit_many s (IndexSet.union j j') (IndexSet.union r r') (t' @ t) rest
      | x :: xs -> x :: visit_one xs
      | [] -> []
    in
    visit_one ls

  let rec expand_outer rejected lrcs = function
    | [] -> []
    | hd :: tl ->
      let tl = match tl with
        | [] -> []
        | tl -> expand_outer rejected (Misc.indexset_bind lrcs Lrc.predecessors) tl
      in
      let hd =
        IndexSet.fold (fun st acc ->
            let lrcs = IndexSet.inter lrcs (lrcs_of st) in
            if IndexSet.is_empty lrcs
            then acc
            else Node (lrcs, rejected, IndexSet.singleton st, []) :: acc
          ) hd []
        |> List.sort order_node
      in
      let tl = List.filter_map (fun (Node (lrcs', _, _, _) as tree) ->
          let lrcs = IndexSet.inter lrcs
              (Misc.indexset_bind lrcs' Lrc.successors)
          in
          if IndexSet.is_empty lrcs
          then None
          else Some (Node (lrcs, rejected, IndexSet.empty, [tree]))
        ) tl
        |> List.sort order_node
      in
      merge_tree hd tl

  include IndexBuffer.Gen.Make()

  type config = {
    state: Reach.n index;
    lrcs: Lrc.set;
  }

  type desc =
    | State of {
        config: config;
        rejected: Info.Terminal.set;
        epsilon: n indexset;
      }
    | Intermediate of {
        lrcs: Lrc.set;
        rejected: Info.Terminal.set;
        potential: Info.Terminal.set;
        next: n indexset;
      }

  let states = get_generator ()

  let table = Hashtbl.create 7

  let intermediate_table = Hashtbl.create 7

  let rec merge_outer acc = function
    | [] -> acc
    | x :: xs ->
      let hd, tl = match acc with
        | [] -> IndexSet.empty, []
        | hd :: tl -> hd, tl
      in
      let add_tr hd (st, _) = IndexSet.add st hd in
      let hd = List.fold_left add_tr hd x in
      let tl = merge_outer tl xs in
      hd :: tl

  let inner_closure st =
    let rejected = ref IndexSet.empty in
    let acc = ref [] in
    let rec loop st =
      rejected := IndexSet.union (Reach.rejected st) !rejected;
      let tr = extra_transitions st in
      acc := merge_outer !acc tr.outer;
      List.iter loop_tr tr.inner
    and loop_tr (st, _) = loop st
    in
    loop st;
    !rejected, !acc

  let visit_intermediate lrcs rejected potential next =
    let desc = Intermediate {lrcs; rejected; potential; next} in
    match Hashtbl.find_opt intermediate_table desc with
    | Some index -> index
    | None ->
      let index = IndexBuffer.Gen.add states desc in
      Hashtbl.add intermediate_table desc index;
      index

  let prepare_state st lrcs =
    let rejected, outer = inner_closure st in
    let outer = expand_outer rejected lrcs outer in
    (rejected, outer)

  let rec visit_config config =
    let lrcs = IndexSet.inter (lrcs_of config.state) config.lrcs in
    let config = {config with lrcs} in
    match Hashtbl.find_opt table config with
    | Some transitions -> transitions
    | None ->
      let reservation = IndexBuffer.Gen.reserve states in
      let index = IndexBuffer.Gen.index reservation in
      Hashtbl.add table config index;
      let rejected, trees = prepare_state config.state config.lrcs in
      let _, epsilon = memoize_branch trees in
      IndexBuffer.Gen.commit states reservation
        (State {config; rejected; epsilon});
      index

  and memoize_tree acc (Node (lrcs, rejected, reachs, next)) =
    let (potential, reached) as acc =
      IndexSet.fold
        (fun state (potential, states) ->
           (IndexSet.union (Reach.potential_reject state) potential,
            IndexSet.add (visit_config {state; lrcs}) states)
        )
        reachs acc
    in
    let potential', next =
      List.fold_left memoize_tree (IndexSet.empty, IndexSet.empty) next
    in
    if IndexSet.is_empty next then acc else
      (IndexSet.union potential' potential,
       IndexSet.add (visit_intermediate lrcs rejected potential' next) reached)

  and memoize_branch trees =
    merge_trees trees
    |> List.fold_left memoize_tree (IndexSet.empty, IndexSet.empty)

  let initial =
    IndexMap.map
      (fun state -> visit_config {lrcs = lrcs_of state; state})
      Reach.initial

  let states = IndexBuffer.Gen.freeze states

  let () = Stopwatch.step time "Nodes: %d" (cardinal n)

  let outer_transitions =
    let rec raw_transitions st acc =
      match Vector.get states st with
      | Intermediate {lrcs; next; _} ->
        (IndexSet.map Lrc.lr1_of_lrc lrcs, next) :: acc
      | State {epsilon; _} ->
        IndexSet.fold raw_transitions epsilon acc
    in
    tabulate_finset n
      (fun st -> raw_transitions st [])

  let potential st =
    match Vector.get states st with
    | Intermediate t -> t.potential
    | State t -> Reach.potential_reject t.config.state

  let rejected st =
    match Vector.get states st with
    | Intermediate t -> t.rejected
    | State t -> t.rejected

  let lrcs_of st =
    match Vector.get states st with
    | Intermediate t -> t.lrcs
    | State t -> t.config.lrcs

  let () = Stopwatch.step time "Outer transitions"

  let () = Stopwatch.leave time

  let count ?(negate=false) pred vector =
    let k = ref 0 in
    let expected = not negate in
    Vector.iter (fun dfa -> if pred dfa = expected then incr k) vector;
    !k

  module Check(DFA : sig
    type n
    val n : n cardinal
    val initial : n index
    val successors : n index -> (Lr1.set * n index) list
    val accept : n index -> Terminal.set
  end) = struct

    let time = Stopwatch.enter Stopwatch.main "Check"

    let predecessors =
      let table = Vector.make DFA.n [] in
      Index.iter DFA.n (fun src ->
          List.iter
            (fun (lr1, tgt) -> Vector.set_cons table tgt (lr1, src))
            (DFA.successors src)
        );
      Vector.get table

    let reverse ~propagate fixpoint =
      let table = Vector.make DFA.n [] in
      let update src tgt dom img =
        Vector.set_cons table tgt (src,dom,img)
      in
      Vector.iteri (fun src f -> propagate (update src) src f) fixpoint;
      Vector.get table

    let rec fixpoint ~propagate todo = match !todo with
      | [] -> ()
      | todo' ->
        todo := [];
        List.iter (fun (dfa, nfa) -> propagate dfa nfa) todo';
        fixpoint ~propagate todo

    module Reducible_forward = struct
      let table = Vector.make DFA.n Decreasing.maximum

      let follow update dfa handled (lr1s, nfa) =
        List.fold_left begin fun lr1s (label, dfa') ->
          let lr1s' = IndexSet.diff lr1s label in
          if lr1s != lr1s' then
            update dfa' nfa handled;
          lr1s'
        end lr1s (DFA.successors dfa)

      let propagate update dfa f =
        List.iter begin fun (dom, handled) ->
          let handled = IndexSet.union handled (DFA.accept dfa) in
          if handled != Terminal.all then
            let process_dom x =
              if not (IndexSet.subset (potential x) handled) then
                let process piece = ignore (follow update dfa handled piece) in
                List.iter process (outer_transitions x)
            in
            IndexSet.iter process_dom dom;
        end (Decreasing.to_list f)

      let () =
        let todo = ref [] in
        let update dfa dom handled =
          let f = Vector.get table dfa in
          let f, df = Decreasing.decrease f (Decreasing.piece dom handled) in
          if not (Decreasing.is_maximum df) then (
            Vector.set table dfa f;
            push todo (dfa, df);
          )
        in
        IndexMap.iter begin fun lrc st ->
          let dom = IndexSet.singleton (Lrc.lr1_of_lrc lrc) in
          let img = IndexSet.singleton st in
          ignore (follow update DFA.initial IndexSet.empty (dom, img))
        end initial;
        fixpoint ~propagate:(propagate update) todo

      let () =
        Stopwatch.step time "Reducible fix point: reached %d states"
          (count ~negate:true Decreasing.is_maximum table)
    end

    module Reducible_backward = struct

      let transitions = Vector.make DFA.n []

      let () =
        let reverse src (f : (n, Terminal.n) Decreasing.t) =
          let register src n dst ns' ts =
              Vector.set_cons transitions dst
                (ns', src, n, ts)
          in
          List.iter (fun (ns, ts) ->
              IndexSet.iter (fun n ->
                  List.iter (fun (lr1s, ns') ->
                      List.iter (fun (lr1s', dst) ->
                          if not (IndexSet.disjoint lr1s lr1s') then
                            register src n dst ns' ts
                        ) (DFA.successors src)
                    ) (outer_transitions n)
                ) ns
            ) (Decreasing.to_list f)
        in
        Vector.iteri reverse Reducible_forward.table

      let table = Vector.mapi (fun dfa f ->
        let image =
          List.fold_left
            (fun acc (lr1s, _) -> IndexSet.union acc lr1s)
            IndexSet.empty (DFA.successors dfa)
        in
        Decreasing.map (fun (ns, img) ->
          let pred n =
            List.exists
              (fun (lr1s, _) -> not (IndexSet.subset lr1s image))
              (outer_transitions n)
          in
          (IndexSet.filter pred ns, img)
        ) f
      ) Reducible_forward.table

      let todo = ref []

      let update dfa dom handled =
        let f = Vector.get table dfa in
        let f, df = Decreasing.decrease f (Decreasing.piece dom handled) in
        if not (Decreasing.is_maximum df) then (
          Vector.set table dfa f;
          push todo (dfa, df);
        )

      let propagate dfa f =
        List.iter (fun (dom, img) ->
            List.iter (fun (dom', src, n, img') ->
                if not (IndexSet.disjoint dom dom') then
                  update src (IndexSet.singleton n) (IndexSet.union img img')
              ) (Vector.get transitions dfa)
          ) (Decreasing.to_list f)

      let () =
        Vector.iteri propagate table;
        fixpoint ~propagate todo

      let () =
        Stopwatch.step time "Reducible backward fixpoint: reached %d states"
          (count ~negate:true Decreasing.is_maximum table)
    end

    module Prefix_forward = struct
      let table = Vector.map (fun f ->
          Decreasing.piecewise (
            List.fold_left (fun acc (dom, img) ->
                IndexSet.fold (fun dom acc ->
                    match IndexSet.diff (rejected dom) img with
                    | r when IndexSet.is_empty r -> acc
                    | r -> (lrcs_of dom, IndexSet.diff Terminal.all r) :: acc
                  ) dom acc
              ) [] (Decreasing.to_list f)
          )
        ) Reducible_forward.table

      let () =
        let count = count Decreasing.is_maximum table in
        Stopwatch.step time "Prepared forward prefix (%d empty, %d non empty)"
        count (cardinal DFA.n - count)

      let follow update dfa (lrcs, img) =
        let img = IndexSet.union img (DFA.accept dfa) in
        if img != Terminal.all then
          List.fold_left (fun lrcs (label, dfa') ->
              let lrcs' =
                IndexSet.inter lrcs (indexset_bind label Lrc.lrcs_of_lr1)
              in
              if IndexSet.is_empty lrcs' then lrcs else (
                update dfa' lrcs' img;
                IndexSet.diff lrcs lrcs'
              )
            ) lrcs (DFA.successors dfa)
        else
          IndexSet.empty

      let propagate update dfa nfa =
        if DFA.accept dfa != Terminal.all then
          List.iter
            (fun piece -> ignore (follow update dfa piece))
            (Decreasing.to_list nfa)

      let () =
        let todo = ref [] in
        let update dfa dom' handled =
          let dom = indexset_bind dom' Lrc.predecessors in
          let f = Vector.get table dfa in
          let f, df = Decreasing.decrease f (Decreasing.piece dom handled) in
          if not (Decreasing.is_maximum df) then (
            Vector.set table dfa f;
            push todo (dfa, df);
          )
        in
        let propagate = propagate update in
        Vector.iteri propagate table;
        fixpoint ~propagate todo;
        Stopwatch.step time "Prefix forward fixpoint (reached %d states)"
          (count ~negate:true Decreasing.is_maximum table)

    end

    module Prefix_backward = struct
      let transitions =
        reverse ~propagate:Prefix_forward.propagate
          Prefix_forward.table

      let table = Vector.mapi (fun dfa f ->
          Decreasing.map (fun (_, handled as piece) ->
              (Prefix_forward.follow (fun _ _ _ -> ()) dfa piece, handled)
            ) f
        ) Prefix_forward.table

      let () =
        Stopwatch.step time "Prepared backward prefix (%d uncovered states)"
          (count ~negate:true Decreasing.is_maximum table)

      let todo = ref []

      let backward_propagate update tgt f =
        let pieces =
          List.map
            (fun (dom, img) -> (indexset_bind dom Lrc.successors, img))
            (Decreasing.to_list f)
        in
        List.iter (fun (src, lrcs, handled) ->
            update src (
              Decreasing.piecewise (
                List.filter_map (fun (lrcs', handled') ->
                    let lrcs' = IndexSet.inter lrcs lrcs' in
                    if IndexSet.is_empty lrcs' then None else
                      let handled' = IndexSet.union handled handled' in
                      if IndexSet.equal handled' Terminal.all
                      then None
                      else Some (lrcs', handled')
                  ) pieces
              )
            )
          ) (transitions tgt)

      let () =
        let update dfa g =
          let f = Vector.get table dfa in
          let f, df = Decreasing.decrease f g in
          if not (Decreasing.is_maximum df) then (
            Vector.set table dfa f;
            push todo (dfa, df);
          )
        in
        let propagate = backward_propagate update in
        Vector.iteri propagate table;
        fixpoint ~propagate todo

      let () =
        Stopwatch.step time "Prefix backward fixpoint (%d reached)"
          (count ~negate:true Decreasing.is_maximum table)

      type tree = {
        depth: int;
        mutable unhandled: (Lrc.set * Terminal.set) list;
        mutable next: (Lrc.set * tree) list;
      }

      let nodes = ref 0
      let max_depth = ref 0

      let visited = Vector.make DFA.n Decreasing.maximum

      let rec visit src node g =
        let f = Vector.get visited src in
        let f, df = Decreasing.decrease f g in
        if not (Decreasing.is_maximum df) then (
          Vector.set visited src f;
          List.iter (fun (dom, img) ->
              let dom = ref dom in
              Prefix_forward.propagate (fun tgt dom' img' ->
                  let dom' = IndexSet.inter !dom dom' in
                  if not (IndexSet.is_empty dom') then (
                    dom := IndexSet.diff !dom dom';
                    let node' = { unhandled = []; next = []; depth = node.depth + 1 } in
                    incr nodes;
                    max_depth := Int.max !max_depth node'.depth;
                    node.next <- (dom', node') :: node.next;
                    let dom' = indexset_bind dom' Lrc.predecessors in
                    visit tgt node' (Decreasing.piece dom' (IndexSet.union img img'))
                  )
                ) src (Vector.get table src);
              if not (IndexSet.is_empty !dom) then
                node.unhandled <- (!dom, img) :: node.unhandled
            ) (Decreasing.to_list f)
        )

      let root =
        let node = {unhandled = []; next = []; depth = 0} in
        visit DFA.initial node (Vector.get table DFA.initial);
        node

      let () =
        Stopwatch.step time "Prefix coverage tree has %d nodes (max depth: %d)" !nodes !max_depth
    end

    let () = Stopwatch.leave time

  end
end
