open Utils
open Misc
open Fix.Indexing
open Monotonous

module Increasing = Increasing_ref

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

let cons_opt x xs =
  let xs = match xs with
    | None -> []
    | Some xs -> xs
  in
  x :: xs

let some_cons_opt x xs =
  Some (cons_opt x xs)

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
    relation_reverse successors

  let accepted st = (Vector.get states st).config.accepted
  let rejected st = (Vector.get states st).config.rejected

  let potential_reject =
    let table = Vector.init n rejected in
    let widen _src rej1 _tgt rej2 =
      (*assert (IndexSet.disjoint rej1 (accepted tgt));*)
      IndexSet.union rej1 rej2
    in
    fixpoint predecessors table ~propagate:widen;
    Vector.get table

  let () = Stopwatch.leave time

end

module type FAILURE_NFA = sig
  type terminal
  type lr1
  type lrc
  type reach

  include CARDINAL

  type config = { state : reach index; lrcs : lrc indexset; }
  type desc =
      State of {
        config : config;
        rejected : terminal indexset;
        epsilon : n indexset;
      }
    | Intermediate of {
        lrcs : lrc indexset;
        rejected : terminal indexset;
        potential : terminal indexset;
        next : n indexset;
      }

  val initial : (lrc, n index) indexmap
  val states : (n, desc) vector
  val outer_transitions :
    n index -> (lr1 indexset * n indexset) list
  val potential : n index -> terminal indexset
  val rejected : n index -> terminal indexset
  val delta : n index -> terminal indexset
  val lrcs_of : n index -> lrc indexset
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

  type reach = Reach.n

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
        | tl -> expand_outer rejected (indexset_bind lrcs Lrc.predecessors) tl
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
              (indexset_bind lrcs' Lrc.successors)
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

  let delta = tabulate_finset n
      (fun st -> IndexSet.diff (potential st) (rejected st))

  let lrcs_of st =
    match Vector.get states st with
    | Intermediate t -> t.lrcs
    | State t -> t.config.lrcs

  let () =
    Stopwatch.step time "Outer transitions";
    Stopwatch.leave time

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

  let time = Stopwatch.enter Stopwatch.main "Coverage-check"

  let count ?(negate=false) pred vector =
    let k = ref 0 in
    let expected = not negate in
    Vector.iter (fun dfa -> if pred dfa = expected then incr k) vector;
    !k

  let predecessors =
    let table = Vector.make DFA.n [] in
    Index.iter DFA.n (fun src ->
        List.iter
          (fun (lr1, tgt) -> Vector.set_cons table tgt (lr1, src))
          (DFA.successors src)
      );
    Vector.get table

  let successor =
    let index src =
      List.fold_left (fun acc (lr1s, tgt) ->
          IndexSet.fold
            (fun lr1 acc -> IndexMap.add lr1 tgt acc)
            lr1s acc
        ) IndexMap.empty (DFA.successors src)
    in
    let table = Vector.init DFA.n index in
    fun dfa lr1 ->
      IndexMap.find_opt lr1 (Vector.get table dfa)

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
      List.iter propagate todo';
      fixpoint ~propagate todo

  let rec diffs d = function
    | [] -> d
    | x :: xs -> diffs (IndexSet.diff d x) xs

  module Reducible_forward = struct
    let () = Stopwatch.step time "Forward propagating reductions"

    let table = Vector.make DFA.n Increasing.minimum

    let todo = ref []

    let update dfa g =
      let f = Vector.get table dfa in
      let f, df = Increasing.increase ~ignore:(DFA.accept dfa) f g in
      if not (Increasing.is_minimum df) then (
        Vector.set table dfa f;
        push todo (dfa, df);
      )

    let initial_fun =
      Increasing.piecewise (
        IndexMap.fold
          (fun _ st acc -> (IndexSet.singleton st, NFA.potential st) :: acc)
          NFA.initial []
      )

    let () = update DFA.initial initial_fun

    let follow dfa unhandled (lr1s, nfa') =
      let image nfa = IndexSet.inter unhandled (NFA.potential nfa) in
      let g = Increasing.from nfa' image in
      if not (Increasing.is_minimum g) then
        ignore (
          List.fold_left (fun lr1s (label, dfa') ->
              let lr1s' = IndexSet.diff lr1s label in
              if lr1s != lr1s' then
                update dfa' g;
              lr1s'
            ) lr1s (DFA.successors dfa)
        )

    let propagate (dfa, f) =
      Increasing.iter f (fun nfa unhandled ->
          List.iter
            (follow dfa (IndexSet.inter unhandled (NFA.delta nfa)))
            (NFA.outer_transitions nfa)
        )

    let () = fixpoint ~propagate todo

    let () =
      Vector.iteri (fun dfa f ->
          Increasing.iter f (fun nfa img ->
              assert (IndexSet.disjoint img (DFA.accept dfa));
              assert (IndexSet.subset img (NFA.potential nfa))
            )
        ) table;
      Stopwatch.step time "Reducible fixpoint: reached %d states"
        (count ~negate:true Increasing.is_minimum table)

    let iter_transitions dfa inc ~f =
      let follow unhandled (lr1s, nfa') =
        let image nfa = IndexSet.inter unhandled (NFA.potential nfa) in
        let g = Increasing.from nfa' image in
        if not (Increasing.is_minimum g) then
          IndexSet.iter (fun lr1 -> f lr1 g (successor dfa lr1))
            lr1s
      in
      Increasing.iter inc (fun nfa unhandled ->
          List.iter
            (follow (IndexSet.inter unhandled (NFA.delta nfa)))
            (NFA.outer_transitions nfa)
        )
  end

  module Prefix_forward = struct
    let expand f =
      Increasing.piecewise (
        Increasing.fold f (fun nfa unhandled acc ->
            let unhandled = IndexSet.inter unhandled (NFA.rejected nfa) in
            if IndexSet.is_empty unhandled
            then acc
            else (NFA.lrcs_of nfa, unhandled) :: acc
          ) []
      )

    let table = Vector.map expand Reducible_forward.table

    let todo = ref []

    let update dfa g =
      let f = Vector.get table dfa in
      let f, df = Increasing.increase ~ignore:(DFA.accept dfa) f g in
      if not (Increasing.is_minimum df) then (
        Vector.set table dfa f;
        push todo (dfa, df);
      )

    let follow src lrc unhandled =
      match successor src (Lrc.lr1_of_lrc lrc) with
      | None -> ()
      | Some tgt ->
        update tgt (Increasing.piece (Lrc.predecessors lrc) unhandled)

    let propagate (dfa, f) =
      Increasing.iter f (follow dfa)

    let iter_transitions dfa inc ~f =
      let follow src lrc unhandled =
        let lr1 = Lrc.lr1_of_lrc lrc in
        f lr1
          (Increasing.piece (Lrc.predecessors lrc) unhandled)
          (successor src lr1)
      in
      Increasing.iter inc (follow dfa)

    let () =
      Vector.iteri (fun i x -> propagate (i, x)) table;
      fixpoint ~propagate todo

    let () =
      Vector.iteri (fun dfa f ->
          Increasing.iter f
            (fun _ img -> assert (IndexSet.disjoint img (DFA.accept dfa)))
        ) table;
      Stopwatch.step time "Prefix forward fixpoint: reached %d states"
        (count ~negate:true Increasing.is_minimum table)
  end

  module Prefix_backward = struct
    let transitions = Vector.make DFA.n IndexMap.empty

    let table =
      Vector.mapi (fun src f ->
          let lr1s =
            List.fold_left (fun acc (lr1s, tgt) ->
                IndexSet.fold
                  (fun lr1 acc -> IndexMap.add lr1 tgt acc)
                  lr1s acc
              )
              IndexMap.empty (DFA.successors src)
          in
          Increasing.filter f
            (fun lrc unhandled ->
               match IndexMap.find_opt (Lrc.lr1_of_lrc lrc) lr1s with
               | None -> true
               | Some tgt ->
                 let map = Vector.get transitions tgt in
                 let lrcs = Lrc.predecessors lrc in
                 let map =
                   IndexSet.fold (fun lrc' map ->
                       IndexMap.update lrc'
                         (some_cons_opt (src, lrc, unhandled)) map
                     ) lrcs map
                 in
                 Vector.set transitions tgt map;
                 false
            )
        ) Prefix_forward.table

    let () =
      Stopwatch.step time "Prefix backward initialized with %d states"
        (count ~negate:true Increasing.is_minimum table)

    let todo = ref []

    let update dfa g =
      let f = Vector.get table dfa in
      let f, df = Increasing.increase f g in
      if not (Increasing.is_minimum df) then (
        Vector.set table dfa f;
        push todo (dfa, df);
      )

    let follow dfa lrc unhandled =
      let tr = Vector.get transitions dfa in
      match IndexMap.find_opt lrc tr with
      | None -> assert (dfa = DFA.initial)
      | Some trs ->
        List.iter (fun (src, lrc', unhandled') ->
            let unhandled = IndexSet.inter unhandled unhandled' in
            if not (IndexSet.is_empty unhandled) then (
              update src (Increasing.piece (IndexSet.singleton lrc') unhandled)
            )
          ) trs

    let propagate (dfa, f) =
      Increasing.iter f (follow dfa)

    let () =
      Vector.iteri (fun i x -> propagate (i, x)) table;
      fixpoint ~propagate todo

    let () =
      Stopwatch.step time "Prefix backward fixpoint: reached %d states"
        (count ~negate:true Increasing.is_minimum table)
  end

  module Reducible_backward = struct
    let transitions = Vector.make DFA.n []

    let uncovered =
      let follow dfa nfa unhandled lr1s nfa' =
        let has_image nfa' = not (IndexSet.disjoint unhandled (NFA.delta nfa')) in
        let nfa' = IndexSet.filter has_image nfa' in
        if IndexSet.is_empty nfa' then
          IndexSet.empty
        else
          List.fold_left (fun lr1s (label, dfa') ->
              let lr1s' = IndexSet.diff lr1s label in
              if lr1s != lr1s' then
                Vector.set_cons transitions dfa'
                  (nfa', unhandled, IndexSet.inter lr1s label, dfa, nfa);
              lr1s'
            ) lr1s (DFA.successors dfa)
      in
      let propagate dfa f =
        Increasing.fold f (fun nfa unhandled acc ->
            let process acc (lr1s, nfa') =
              let rem = follow dfa nfa unhandled lr1s nfa' in
              if IndexSet.is_empty rem
              then acc
              else (nfa, unhandled, rem, nfa') :: acc
            in
            List.fold_left process acc (NFA.outer_transitions nfa)
          ) []
      in
      Vector.mapi propagate Reducible_forward.table

    let table = Vector.mapi (fun dfa pieces ->
        let red_uncovered =
          let prepare (nfa, img, _, _) = (IndexSet.singleton nfa, img) in
          Increasing.piecewise (List.map prepare pieces)
        in
        let prefix_uncovered = Vector.get Prefix_backward.table dfa in
        Increasing.fold (Vector.get Reducible_forward.table dfa)
          (fun nfa unhandled acc ->
             let unhandled = IndexSet.inter unhandled (NFA.rejected nfa) in
             if IndexSet.is_empty unhandled then acc else
               let unhandled' =
                 IndexSet.fold
                   (fun lrc acc ->
                      IndexSet.union (Increasing.image prefix_uncovered lrc) acc)
                   (NFA.lrcs_of nfa) IndexSet.empty
               in
               let unhandled = IndexSet.inter unhandled unhandled' in
               Increasing.add acc nfa unhandled
          ) red_uncovered
      ) uncovered

    let () =
      Stopwatch.step time "Reducible backward: %d directly uncovered states, %d including prefix"
        (count ((<>) []) uncovered)
        (count ~negate:true Increasing.is_minimum table)

    let todo = ref []

    let update dfa g =
      let f = Vector.get table dfa in
      let f, df = Increasing.increase f g in
      if not (Increasing.is_minimum df) then (
        Vector.set table dfa f;
        push todo (dfa, df);
      )

    let propagate (dfa_tgt, f) =
      Increasing.iter f (fun nfa_tgt unhandled ->
          List.iter (fun (nfa_tgts', unhandled', _, dfa_src, nfa_src) ->
              if IndexSet.mem nfa_tgt nfa_tgts' then
                let unhandled = IndexSet.inter unhandled unhandled' in
                if not (IndexSet.is_empty unhandled) then
                  update dfa_src (Increasing.piece (IndexSet.singleton nfa_src) unhandled);
            )
            (Vector.get transitions dfa_tgt)
        )

    let () =
      Vector.iteri (fun i x -> propagate (i,x)) table;
      fixpoint ~propagate todo

    let () =
      Stopwatch.step time "Reducible backward fixpoint: %d states reached"
        (count ~negate:true Increasing.is_minimum table)
  end

  module Sym_or_lr1 = Sum(Symbol)(Lr1)

  module FG = struct
    type t = (NFA.n, Terminal.n) Increasing.t * (Lrc.n, Terminal.n) Increasing.t

    let compare (a1,b1) (a2,b2) =
      let c = Increasing.compare a1 a2 in
      if c <> 0 then c else
        Increasing.compare b1 b2

    let normalize f g =
      (f, Increasing.union g (Prefix_forward.expand f))

    let is_minimum (f,g) =
      Increasing.is_minimum f && Increasing.is_minimum g

    let union (f,g) (f',g')=
      (Increasing.union f f', Increasing.union g g')
  end


  module UncoveredNFA = struct
    include IndexBuffer.Gen.Make()

    type desc = {
      dfa: DFA.n index;
      fg: FG.t;
      mutable transitions: (Sym_or_lr1.n, n indexset) indexmap;
      mutable uncovered: (Sym_or_lr1.n, FG.t) indexmap;
    }

    let states = get_generator ()

    let initial_fun =
      let f1 = Reducible_forward.initial_fun in
      let f2 = Vector.get Reducible_backward.table DFA.initial in
      FG.normalize (Increasing.intersect f1 f2) Increasing.minimum

    let sym_of lr1 = match Lr1.incoming lr1 with
      | Some sym -> Sym_or_lr1.inj_l sym
      | None -> Sym_or_lr1.inj_r lr1

    module KeyMap = Map.Make(FG)

    let visited = Vector.make DFA.n KeyMap.empty

    let todo = ref []

    let visit dfa (f,g) =
      let f' = Vector.get Reducible_backward.table dfa in
      let g' = Vector.get Prefix_backward.table dfa in
      let f = Increasing.intersect f f' in
      let g = Increasing.intersect g g' in
      let fg = FG.normalize f g in
      if FG.is_minimum (f,g) then None else
        let visited' = Vector.get visited dfa in
        match KeyMap.find_opt fg visited' with
        | Some _ as result -> result
        | None ->
          let reservation = IndexBuffer.Gen.reserve states in
          let index = IndexBuffer.Gen.index reservation in
          let desc = {
            dfa; fg; transitions = IndexMap.empty;
            uncovered = IndexMap.empty;
          } in
          Vector.set visited dfa (KeyMap.add fg index visited');
          push todo desc;
          IndexBuffer.Gen.commit states reservation desc;
          Some index

    let propagate ({dfa; fg; _} as desc) =
      let transitions = ref IndexMap.empty in
      let update_inc key fg map =
        if FG.is_minimum fg then map else
          IndexMap.update key (function
              | None -> Some fg
              | Some fg' -> Some (FG.union fg fg')
            ) map
      in
      let update_transition lr1 fg dfa' =
        let sym = sym_of lr1 in
        match dfa' with
        | None ->
          desc.uncovered <- update_inc sym fg desc.uncovered
        | Some tgt ->
          transitions := IndexMap.update sym (function
              | None -> Some (IndexMap.singleton tgt fg)
              | Some map' -> Some (update_inc tgt fg map')
            ) !transitions
      in
      Reducible_forward.iter_transitions dfa (fst fg)
        ~f:(fun lr1 f' dfa' -> update_transition lr1 (f',Increasing.minimum) dfa');
      Prefix_forward.iter_transitions dfa (snd fg)
        ~f:(fun lr1 g' dfa' -> update_transition lr1 (Increasing.minimum,g') dfa');
      let visit_target dfa' fg acc =
        match visit dfa' fg with
        | None -> acc
        | Some index -> IndexSet.add index acc
      in
      let visit_tr map' = IndexMap.fold visit_target map' IndexSet.empty in
      let transitions = IndexMap.map visit_tr !transitions in
      desc.transitions <- transitions

    let initial =
      Option.fold
        ~none:IndexSet.empty
        ~some:IndexSet.singleton
        (visit DFA.initial initial_fun)

    let () = fixpoint ~propagate todo

    let states = IndexBuffer.Gen.freeze states

    let () = Stopwatch.step time "Uncovered NFA has %d states" (cardinal n)
  end

  module UncoveredDFA = struct
    include IndexBuffer.Gen.Make()

    type desc = {
      nfas: UncoveredNFA.n indexset;
      transitions: (Sym_or_lr1.n, n index) indexmap;
      uncovered: (Sym_or_lr1.n, FG.t) indexmap;
    }

    let states = get_generator ()

    let table : (UncoveredNFA.n indexset, _) Hashtbl.t = Hashtbl.create 7

    let rec visit nfas =
      match Hashtbl.find_opt table nfas with
      | Some index -> index
      | None ->
        let reservation = IndexBuffer.Gen.reserve states in
        let index = IndexBuffer.Gen.index reservation in
        Hashtbl.add table nfas index;
        let visit_nfa nfa (uncovered, transitions) =
          let ndesc = Vector.get UncoveredNFA.states nfa in
          (IndexMap.union (fun _ m1 m2 -> Some (FG.union m1 m2))
             uncovered ndesc.uncovered,
           IndexMap.union (fun _ m1 m2 -> Some (IndexSet.union m1 m2))
             ndesc.transitions transitions)
        in
        let uncovered, transitions =
          IndexSet.fold visit_nfa nfas
            (IndexMap.empty, IndexMap.empty)
        in
        let transitions = IndexMap.map visit transitions in
        let desc = {nfas; transitions; uncovered} in
        IndexBuffer.Gen.commit states reservation desc;
        index

    let initial = visit UncoveredNFA.initial

    let states = IndexBuffer.Gen.freeze states

    let () =
      let count_f = ref 0 in
      let count_g = ref 0 in
      Vector.iter (fun desc ->
          IndexMap.iter (fun _ (f,g) ->
              if not (Increasing.is_minimum f) then incr count_f;
              if not (Increasing.is_minimum g) then incr count_g;
            ) desc.uncovered
        ) states;
      Stopwatch.step time "Uncovered DFA has %d states (%d uncovered lrc, %d uncovered nfa)"
        (cardinal n)
        !count_g
        !count_f

    type terminal = Terminal.n
    type transition = Sym_or_lr1.n index

    let initials f = f initial
    let successors i f =
      let desc = Vector.get states i in
      IndexMap.iter f desc.transitions

    let add_image (f, g) acc =
      acc
      |> Increasing.fold f (fun _ img acc -> IndexSet.union img acc)
      |> Increasing.fold g (fun _ img acc -> IndexSet.union img acc)

    let rejected = tabulate_finset n (fun i ->
        let desc = Vector.get states i in
        IndexMap.fold (fun _ -> add_image) desc.uncovered IndexSet.empty
      )

    let potential_reject = tabulate_finset n (fun i ->
        let desc = Vector.get states i in
        IndexSet.fold
          (fun nfa acc -> add_image (Vector.get UncoveredNFA.states nfa).fg acc)
          desc.nfas IndexSet.empty
      )
  end

  let enum_sentence f =
    let visited = Boolvector.make UncoveredDFA.n false in
    let todo = ref [[], UncoveredDFA.initial] in
    let propagate (suffix, index) =
      if not (Boolvector.test visited index) then (
        Boolvector.set visited index;
        let desc = Vector.get UncoveredDFA.states index in
        IndexMap.iter (fun sym fg ->
            f (sym :: suffix) (UncoveredDFA.add_image fg IndexSet.empty)
          ) desc.uncovered;
        IndexMap.iter (fun sym state' ->
            push todo (sym :: suffix, state')
          ) desc.transitions
      )
    in
    fixpoint ~propagate todo

  let () = enum_sentence (fun suffix unhandled ->
      let suffix = List.filter_map (fun sym ->
          match Sym_or_lr1.prj sym with
          | L sym -> Some sym
          | R _ -> None
        ) suffix
      in
      Printf.printf "%s %s\n"
        (string_concat_map " " Symbol.name suffix)
        (string_of_indexset ~index:Terminal.to_string unhandled)
    )
  (*module Coverage = Coverage_tree.Make(UncoveredDFA)

  let () =
    let count = ref 0 in
    Coverage.enum_sentences ~cover:UncoveredDFA.n ~index:(fun x -> x)
      (fun _ _ ->
         if !count = 1000 then
           Printf.printf "%d sentences\n" !count;
         incr count;
      );
    Stopwatch.step time "Covered using %d sentences" !count*)

  (*module Determinize = struct
    module Sym_or_lr1 = Sum(Symbol)(Lr1)

    include IndexBuffer.Gen.Make()

    type fg = (NFA.n, Terminal.n) Increasing.t * (Lrc.n, Terminal.n) Increasing.t

    type desc = {
      dfa: DFA.n index;
      fg: fg;
      mutable transitions: (Sym_or_lr1.n, n index) indexmap;
      mutable uncovered: (Sym_or_lr1.n, fg) indexmap;
    }

    let states = get_generator ()

    let normalize f g =
      (f, Increasing.union g (Prefix_forward.expand f))

    let initial_fun =
      let f1 = Reducible_forward.initial_fun in
      let f2 = Vector.get Reducible_backward.table DFA.initial in
      normalize (Increasing.intersect f1 f2) Increasing.minimum

    let sym_of lr1 = match Lr1.incoming lr1 with
      | Some sym -> Sym_or_lr1.inj_l sym
      | None -> Sym_or_lr1.inj_r lr1

    module KeyMap = Map.Make(struct
        type t = (NFA.n, Terminal.n) Increasing.t *
                 (Lrc.n, Terminal.n) Increasing.t
        let compare (a1,b1) (a2,b2) =
          let c = Increasing.compare a1 a2 in
          if c <> 0 then c else
            Increasing.compare b1 b2
      end)

    let visited = Vector.make DFA.n KeyMap.empty

    let todo = ref []

    let is_minimum (f,g) =
      Increasing.is_minimum f && Increasing.is_minimum g

    let union (f,g) (f',g')=
      (Increasing.union f f', Increasing.union g g')

    let visit dfa (f,g) =
      let f' = Vector.get Reducible_backward.table dfa in
      let g' = Vector.get Prefix_backward.table dfa in
      let f = Increasing.intersect f f' in
      let g = Increasing.intersect g g' in
      let fg = normalize f g in
      if is_minimum (f,g) then None else
        let visited' = Vector.get visited dfa in
        match KeyMap.find_opt fg visited' with
        | Some _ as result -> result
        | None ->
          let reservation = IndexBuffer.Gen.reserve states in
          let index = IndexBuffer.Gen.index reservation in
          let desc = {
            dfa; fg; transitions = IndexMap.empty;
            uncovered = IndexMap.empty;
          } in
          Vector.set visited dfa (KeyMap.add fg index visited');
          push todo desc;
          IndexBuffer.Gen.commit states reservation desc;
          Some index

    let process ({dfa; fg; _} as desc) =
      let uncovered = ref IndexMap.empty in
      let transitions = ref IndexMap.empty in
      let update_inc key fg map =
        if is_minimum fg then map else
          IndexMap.update key (function
              | None -> Some fg
              | Some fg' -> Some (union fg fg')
            ) map
      in
      let update_transition lr1 fg dfa' =
        let sym = sym_of lr1 in
        match dfa' with
        | None ->
          uncovered := update_inc sym fg !uncovered
        | Some tgt ->
          transitions := IndexMap.update sym (function
              | None -> Some (IndexMap.singleton tgt fg)
              | Some map' -> Some (update_inc tgt fg map')
            ) !transitions
      in
      Reducible_forward.iter_transitions dfa (fst fg)
        ~f:(fun lr1 f' dfa' -> update_transition lr1 (f',Increasing.minimum) dfa');
      Prefix_forward.iter_transitions dfa (snd fg)
        ~f:(fun lr1 g' dfa' -> update_transition lr1 (Increasing.minimum,g') dfa');
      let transitions =
        IndexMap.map (fun sym map' ->
            let node' = get_child node sym in
            IndexMap.iter (fun dfa' fg ->
                push todo (node', dfa', fg)
              ) map'
          ) !transitions
      in
      index

    let root = new_node ()

    let () =
      propagate (root, DFA.initial, initial_fun);
      fixpoint ~propagate todo

    let rec count_branch node =
      let count =
        IndexMap.fold (fun _ node' acc ->
            acc + count_branch node'
          ) node.child 0
      in
      if count = 0 && not (IndexMap.is_empty node.reached) then
        1
      else
        count

    let iter_branches node fn =
      let rec loop prefix node =
        let ok =
          IndexMap.fold (fun sym node' acc ->
              ( match Sym_or_lr1.prj sym with
                | L sym -> loop (sym :: prefix) node'
                | R _lr1 ->
                  match IndexMap.choose_opt node'.reached with
                  | None -> false
                  | Some (_dfa, (f, g)) ->
                    fn f g prefix;
                    true
              ) || acc
            ) node.child false
        in
        if not ok then
          match IndexMap.choose_opt node.reached with
          | None -> false
          | Some (_dfa, (f, g)) ->
            fn f g prefix;
            true
        else
          ok
      in
      ignore (loop [] node)

    let lrc_prefix =
      let table = Vector.make Lrc.n [] in
      let todo = ref [] in
      let expand prefix state =
        match Vector.get table state with
        | [] ->
          Vector.set table state prefix;
          let prefix = state :: prefix in
          let successors = Lrc.successors state in
          if not (IndexSet.is_empty successors) then
            push todo (successors, prefix)
        | _ -> ()
      in
      Index.iter Info.Lr1.n (fun lr1 ->
          if Option.is_none (Info.Lr1.incoming lr1) then
            expand [] (Lrc.first_lrc_of_lr1 lr1)
        );
      let propagate (successors, prefix) =
        IndexSet.iter (expand prefix) successors
      in
      fixpoint ~propagate todo;
      Vector.get table

    let print_sym sym =
      print_char ' ';
      print_string (Symbol.name sym)

    let handle_lrc suffix lrc unhandled =
      let prefix = List.rev_map Lrc.lr1_of_lrc (lrc :: lrc_prefix lrc) in
      let entry = List.hd prefix in
      let prefix = List.tl prefix in
      let prefix = List.filter_map Lr1.incoming prefix in
      print_string (Lr1.to_string entry);
      List.iter print_sym prefix;
      List.iter print_sym suffix;
      print_char ' ';
      print_endline (string_of_indexset ~index:Terminal.to_string unhandled)


    let handle_nfa suffix nfa unhandled =
      print_string "HANDLING NFA";
      List.iter print_sym suffix;
      print_newline ();
      let unhandled = ref unhandled in
      let threads = ref [nfa, suffix] in
      let visit suffix nfa =
        let handled = IndexSet.inter !unhandled (NFA.rejected nfa) in
        if not (IndexSet.is_empty handled) then (
          unhandled := IndexSet.diff !unhandled handled;
          let lrc = IndexSet.choose (NFA.lrcs_of nfa) in
          handle_lrc suffix lrc handled
        );
        if not (IndexSet.disjoint !unhandled (NFA.potential nfa)) then
          push threads (nfa, suffix)
      in
      let expand (nfa, suffix) =
        List.iter
          (fun (lr1s, nfa') ->
             let suffix =
               Option.get (Lr1.incoming (IndexSet.choose lr1s)) :: suffix
             in
             IndexSet.iter (visit suffix) nfa'
          )
          (NFA.outer_transitions nfa)
      in
      let rec loop () =
        match !threads with
        | _ when IndexSet.is_empty !unhandled -> ()
        | [] -> assert false
        | threads' ->
          threads := [];
          List.iter expand threads';
          loop ()
      in
      loop ()

    let () =
      Vector.iter (fun i -> assert (Increasing.is_minimum i)) todo_red;
      Vector.iter (fun i -> assert (Increasing.is_minimum i)) todo_pre;
      Stopwatch.step time "Covering tree has %d nodes, %d branches" !count (count_branch root);
      iter_branches root begin fun f g suffix ->
        let todo = ref (
            IndexSet.empty
            |> Increasing.fold g (fun _ -> IndexSet.union)
            |> Increasing.fold f (fun _ -> IndexSet.union)
          ) in
        let register_case lrc unhandled acc =
          (IndexSet.cardinal unhandled, lrc, unhandled) :: acc
        in
        let order_case (c1, _, _) (c2, _, _) = Int.compare c2 c1 in
        let handle_case fn (_, case, unhandled) =
          let unhandled = IndexSet.inter !todo unhandled in
          if not (IndexSet.is_empty unhandled) then (
            todo := IndexSet.diff !todo unhandled;
            fn case unhandled
          )
        in
        let handle_cases fn f =
          Increasing.fold f register_case []
          |> List.sort order_case
          |> List.iter (handle_case fn)
        in
        handle_cases (handle_lrc suffix) g;
        if not (IndexSet.is_empty !todo) then
          handle_cases (handle_nfa suffix) f;
      end
  end*)

  let () = Stopwatch.leave time

end
