open Utils
open Misc
open Fix.Indexing
open Info

type ('g, 'st, 'lrc, 'en) state = {
  mac: 'st index;
  pos: ('lrc, 'en) Sum.n index;
  mutable live: 'g terminal indexset;
  mutable predecessors: (('st, ('lrc, 'en) Sum.n) Prod.n Opt.n index * 'g terminal indexset) list;
  mutable unhandled: ('lrc indexset, ('g, 'en) Denumeration.edge list) either;
}

let coverage (type g r st tr lrc en)
    (g : g grammar)
    (branches : (g, r) Spec.branches)
    (machine : (g, r, st, tr) Automata.Machine.t)
    (stacks : (g, lrc) Automata.stacks)
    (enum : (g, lrc, en) Denumeration._graph)
  =
  let state_count = Vector.length machine.outgoing in
  let reachable = Vector.make state_count IndexMap.empty in
  let todo = ref [] in
  (* Reached configuration `(mac,pos)` (mac a machine state, enu an enum state),
     while lookaheads `accepted` have already been accepted. *)
  let reached predecessor mac pos live =
    let live =
      List.fold_left begin fun la (br, _, _) ->
        if Boolvector.test branches.is_partial br then la else
          (* FIXME: check for unreachable clauses *)
          match branches.lookaheads.:(br) with
          | None -> IndexSet.empty
          | Some la -> IndexSet.diff live la
      end live machine.accepting.:(mac)
    in
    if IndexSet.is_not_empty live then
      let map = reachable.:(mac) in
      match IndexMap.find_opt pos map with
      | Some st ->
        let live' = IndexSet.union live st.live in
        if live' != st.live then (
          st.live <- live';
          st.predecessors <- (predecessor, live) :: st.predecessors;
          push todo st
        )
      | None ->
        let st = {mac; pos; live; predecessors = [predecessor, live]; unhandled = R []} in
        reachable.:(mac) <- IndexMap.add pos st map;
        push todo st
  in
  let lrc_at pos =
    match Sum.prj stacks.domain pos with
    | L lrc -> lrc
    | R enu -> enum.states.:(enu).lrc
  in
  let process_transition filter predecessor live tr candidates =
    let mac = machine.target.:(tr) in
    let label = machine.label.:(tr).filter in
    filter begin fun pos ->
      if IndexSet.mem (stacks.label (lrc_at pos)) label then (
        reached predecessor mac pos live;
        false
      ) else
        true
    end candidates
  in
  let unhandled_initials = match machine.initial with
    | None -> []
    | Some initial ->
      let filter f l =
        List.filter (fun x -> f (Sum.inj_r stacks.domain x)) l
      in
      IndexSet.fold
        (process_transition filter Opt.none (Terminal.regular g))
        machine.outgoing.:(initial)
        (IndexMap.fold (fun _ en acc -> en :: acc) enum.initials [])
  in
  let propagate_lrcs cst lrc =
    let predecessor = Opt.some (Prod.inj state_count cst.mac cst.pos) in
    let trs = machine.outgoing.:(cst.mac) in
    let live = cst.live in
    let filter f s = IndexSet.filter (fun x -> f (Sum.inj_l x)) s in
    let process edge acc =
      process_transition filter predecessor live edge acc
    in
    let lrcs = stacks.prev lrc in
    if IndexSet.is_empty lrcs then
      cst.unhandled <- L IndexSet.empty
    else
      let lrcs = IndexSet.fold process trs lrcs in
      if IndexSet.is_not_empty lrcs then
        cst.unhandled <- L lrcs
  in
  let propagate cst =
    match Sum.prj stacks.domain cst.pos with
    | L lrc -> propagate_lrcs cst lrc
    | R enu ->
      let enu = enum.states.:(enu) in
      match enu.successors with
      | [] -> propagate_lrcs cst enu.lrc
      | edges ->
        let filter_edge f edge = f (Sum.inj_r stacks.domain edge.Denumeration.target) in
        let filter_edges f edges = List.filter (filter_edge f) edges in
        let predecessor = Opt.some (Prod.inj state_count cst.mac cst.pos) in
        let trs = machine.outgoing.:(cst.mac) in
        let live = cst.live in
        let process edge acc =
          process_transition filter_edges predecessor live edge acc
        in
        cst.unhandled <- R (IndexSet.fold process trs edges)
  in
  let counter = ref 0 in
  fixpoint ~counter ~propagate todo;
  let unhandled_tail = ref 0 in
  let unhandled_midl = ref 0 in
  Vector.iter begin IndexMap.iter begin fun _ node ->
      match node.unhandled with
      | L _ -> incr unhandled_tail
      | R (_ :: _) -> incr unhandled_midl
      | R _ -> ()
    end end reachable;
  stopwatch 1 "dcoverage in %d steps; %d unhandled initials; %d&%d unhandled transitions (middle&tail)"
    !counter
    (List.length unhandled_initials)
    !unhandled_midl
    !unhandled_tail
