open Utils
open Misc
open Fix.Indexing
open Info

type ('g, 'st, 'lrc, 'en) state = {
  mac: 'st index;
  pos: ('lrc, 'en) Sum.n index;
  mutable accepted: 'g terminal indexset;
}

let coverage (type g r st tr lrc en)
    (*(g : g grammar)*)
    (branches : (g, r) Spec.branches)
    (machine : (g, r, st, tr) Automata.Machine.t)
    (stacks : (g, lrc) Automata.stacks)
    (enum : (g, lrc, en) Denumeration._graph)
  =
  let reachable = Vector.make (Vector.length machine.outgoing) IndexMap.empty in
  let todo = ref [] in
  (* Reached configuration `(mac,pos)` (mac a machine state, enu an enum state),
     while lookaheads `accepted` have already been accepted. *)
  let reached mac pos accepted =
    match
      List.fold_left begin fun la (br, _, _) ->
        if Boolvector.test branches.is_partial br
        then la
        else
          (* FIXME: check for unreachable clauses *)
          match branches.lookaheads.:(br) with
          | None -> raise Exit
          | Some la' -> IndexSet.union la' la
      end accepted machine.accepting.:(mac)
    with
    | exception Exit -> ()
    | accepted ->
      let map = reachable.:(mac) in
      match IndexMap.find_opt pos map with
      | Some st ->
        let accepted0 = st.accepted in
        let accepted' = IndexSet.inter accepted st.accepted in
        if accepted' != accepted0 then (
          st.accepted <- accepted';
          push todo st
        )
      | None ->
        let st = {mac; pos; accepted} in
        reachable.:(mac) <- IndexMap.add pos st map;
        push todo st
  in
  let lrc_at pos =
    match Sum.prj stacks.domain pos with
    | L lrc -> lrc
    | R enu -> enum.states.:(enu).lrc
  in
  let process_transition filter accepted tr candidates =
    let mac = machine.target.:(tr) in
    let label = machine.label.:(tr).filter in
    filter begin fun pos ->
      if IndexSet.mem (stacks.label (lrc_at pos)) label then (
        reached mac pos accepted;
        false
      ) else
        true
    end candidates
  in
  let _unhandled_initials =
    let trs =
      Option.fold machine.initial
        ~none:IndexSet.empty
        ~some:(Vector.get machine.outgoing)
    in
    IndexSet.fold (process_transition List.filter IndexSet.empty) trs
      (IndexMap.fold (fun _ en acc -> Sum.inj_r stacks.domain en :: acc) enum.initials [])
  in
  let propagate_lrcs cst lrc =
    let trs = machine.outgoing.:(cst.mac) in
    let accepted = cst.accepted in
    let process edge acc = process_transition IndexSet.filter accepted edge acc in
    let unhandled = IndexSet.fold process trs (IndexSet.lift_sum (stacks.prev lrc)) in
    ignore unhandled
  in
  let propagate cst =
    match Sum.prj stacks.domain cst.pos with
    | L lrc -> propagate_lrcs cst lrc
    | R enu ->
      let enu = enum.states.:(enu) in
      match enu.successors with
      | [] -> propagate_lrcs cst enu.lrc
      | edges ->
        let trs = machine.outgoing.:(cst.mac) in
        let filter_edge f l =
          let f' edge = f (Sum.inj_r stacks.domain edge.Denumeration.target) in
          List.filter f' l
        in
        let process edge acc = process_transition filter_edge cst.accepted edge acc in
        let unhandled = IndexSet.fold process trs edges in
        ignore unhandled
  in
  let counter = ref 0 in
  fixpoint ~counter ~propagate todo;
  stopwatch 1 "dcoverage in %d steps" !counter
