open Utils
open Misc
open Fix.Indexing

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
