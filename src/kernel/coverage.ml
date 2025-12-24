open Utils
open Misc
open Fix.Indexing
open Info

(* Compact representation of a position in a rule *)
module Position = Unsafe_cardinal()
type 'g position = 'g Position.t

type 'g position_desc = 'g nonterminal index * int

type 'g positions = {
  desc: ('g position, 'g position_desc) vector;
  zero: ('g nonterminal, 'g position index) vector;
}

let make_positions (type g) (g : g grammar) : g positions =
  let length = Vector.make (Nonterminal.cardinal g) 0 in
  Index.iter (Production.cardinal g) (fun prod ->
      length.@(Production.lhs g prod) <- Int.max (Production.length g prod)
    );
  let open Position.Const(struct
      type t = g
      let cardinal =
        Vector.fold_left (+) (1 + Vector.length_as_int length) length
    end)
  in
  let desc = Vector.make' n (fun () -> Index.of_int (Nonterminal.cardinal g) 0, 0) in
  let enum = Index.enumerate n in
  let zero = Vector.mapi (fun nt count ->
      let zero = enum () in
      desc.:(zero) <- (nt, 0);
      for i = 1 to count do
        desc.:(enum ()) <- (nt, i);
      done;
      zero
    ) length
  in
  {desc; zero}

let inject_position (type g) (p : g positions) nt pos =
  assert (pos >= 0);
  let p0 = p.zero.:(nt) in
  let pn = Index.of_int (Vector.length p.desc) ((p0 :> int) + pos) in
  let (nt', _) = p.desc.:(pn)  in
  assert (Index.equal nt nt');
  pn

let project_position (type g) (p : g positions) pos =
  p.desc.:(pos)

let previous_position (type g) (p : g positions) pos =
  match p.desc.:(pos) with
  | (nt, 0) -> Either.Left nt
  | _ -> Either.Right (Index.of_int (Vector.length p.desc) ((pos :> int) - 1))

let pack_position positions i j =
  Prod.inj (Vector.length positions.desc) i j

let pack_inject positions lrc nt pos =
  pack_position positions (inject_position positions nt pos) lrc

let unpack_position positions i =
  Prod.prj (Vector.length positions.desc) i

let get_map v i j =
  let map = v.:(i) in
  match IndexMap.find_opt j map with
  | Some r -> r
  | None ->
    let r = ref IndexSet.empty in
    v.:(i) <- IndexMap.add j r map;
    r

let (@:=) r f =
  r := f !r

type ('g, 'lrc) failure_node = {
  lrc: 'lrc index;
  nt: 'g nonterminal index;
  mutable fallible: 'g terminal indexset;
  mutable fail_fwd: (('g, 'lrc) failure_node * 'g terminal indexset) list;
  mutable fwd: (('g, 'lrc) failure_node * 'g terminal indexset) list;
  mutable bkd: (('g, 'lrc) failure_node * 'g terminal indexset) list;
}

let failures (type g lrc)
    (g : g grammar)
    (stacks : (g, lrc) Automata.stacks)
    (rcs : (g lr1, g Redgraph.reduction_closure) vector)
  =
  let table = Vector.make stacks.domain IndexMap.empty in
  let todo = ref [] in
  let rec explore lrc nt =
    let map = table.:(lrc) in
    match IndexMap.find_opt nt map with
    | Some node -> node
    | None ->
      let tgt = Transition.find_goto_target g (stacks.label lrc) nt in
      let rc = rcs.:(tgt) in
      let node = {lrc; nt; fallible = rc.failing; fwd = []; bkd = []; fail_fwd = []} in
      push todo node;
      table.:(lrc) <- IndexMap.add nt node map;
      let _, fwd = List.fold_left (fun (lrcs, fwd) nts ->
          let lrcs = IndexSet.bind lrcs stacks.prev in
          let fwd = IndexMap.fold (fun nt la fwd ->
              IndexSet.fold (fun lrc fwd ->
                  let node' = explore lrc nt in
                  node'.bkd <- (node, la) :: node'.bkd;
                  (node', la) :: fwd
                ) lrcs fwd
            ) nts fwd
          in
          (lrcs, fwd)
        ) (IndexSet.singleton lrc, []) rc.reductions
      in
      node.fwd <- fwd;
      node
  in
  let propagate node =
    List.iter (fun (node', la) ->
        let fallible = IndexSet.union (IndexSet.inter node.fallible la) node'.fallible in
        if fallible != node'.fallible then (
          let delta = IndexSet.diff node'.fallible fallible in
          node'.fallible <- fallible;
          node'.fail_fwd <- (node, delta) :: node'.fail_fwd;
          push todo node'
        )
      ) node.bkd
  in
  let rec explore_all lrcs nt = function
    | 0 ->
      let nodes = IndexSet.fold (fun lrc acc -> explore lrc nt :: acc) lrcs [] in
      fixpoint ~propagate todo;
      nodes
    | n ->
      explore_all (IndexSet.bind lrcs stacks.prev) nt (n - 1)
  in
  explore_all

let coverage (type g r st tr lrc)
    (g : g grammar)
    (branches : (g, r) Spec.branches)
    (machine : (g, r, st, tr) Automata.Machine.t)
    (stacks : (g, lrc) Automata.stacks)
    (rcs : (g lr1, g Redgraph.reduction_closure) vector)
    (positions : g positions)
    initial
  =
  let state_count = Vector.length machine.outgoing in
  let reached = Vector.make state_count IndexMap.empty in
  let transitions = Vector.make state_count [] in
  let pending = ref [] in
  let todo = Vector.make state_count IndexMap.empty in
  let schedule st0 lp0 st lp la =
    let reached = get_map reached st lp in
    let la = IndexSet.diff la !reached in
    if IndexSet.is_not_empty la then (
      reached @:= IndexSet.union la;
      let todo = get_map todo st lp in
      if IndexSet.is_empty !todo then
        push pending st;
      todo @:= IndexSet.union la;
      transitions.@(st) <- List.cons (st0, lp0, st, lp, la);
    )
  in
  let unhandled_inner = ref 0 in
  let unhandled_stack = ref 0 in
  let unhandled_transitions xs =
    unhandled_inner := !unhandled_inner + List.length xs
  in
  let propagate_position st lp la =
    let la =
      List.fold_left (fun la (br, _, _) ->
          if Boolvector.test branches.is_partial br then
            la
          else
            (* FIXME: check for unreachable clauses *)
            match branches.lookaheads.:(br) with
            | None -> IndexSet.empty
            | Some la' -> IndexSet.diff la la'
        ) la machine.accepting.:(st)
    in
    if IndexSet.is_not_empty la then
      let pos, lrc = unpack_position positions lp in
      match previous_position positions pos with
      | Left nt ->
        let src = stacks.label lrc in
        let tgt = Transition.find_goto_target g src nt in
        List.iteri begin fun pos' nts ->
          IndexMap.iter begin fun nt' la' ->
            let la = IndexSet.inter la la' in
            if IndexSet.is_not_empty la then
              schedule st lp st (pack_inject positions lrc nt' pos') la
          end nts
        end rcs.:(tgt).reductions;
        let la = IndexSet.inter la rcs.:(tgt).failing in
        if IndexSet.is_not_empty la then
          () (*TODO: unhandled la*)
          (*schedule st lp st (pack_position positions positions.free lrc) la*)
      | Right pos' ->
        let lrcs = IndexSet.split_by_run stacks.label (stacks.prev lrc) in
        if List.is_empty lrcs then
          assert false
          (* Initial state: all lookaheads should have been handled by now *)
          (* assert (pos' = positions.free);
             incr unhandled_stack) *)
        else
          let trs = machine.outgoing.:(st) in
          let process tr lrcs =
            let st' = machine.target.:(tr) in
            let filter = machine.label.:(tr).filter in
            List.filter begin fun (lr1, lrcs) ->
              if IndexSet.mem lr1 filter then (
                IndexSet.iter
                  (fun lrc' -> schedule st lp st' (pack_position positions pos' lrc') la)
                  lrcs;
                false
              ) else
                true
            end lrcs
          in
          unhandled_transitions (IndexSet.fold process trs lrcs)
  in
  let propagate st =
    let map = todo.:(st) in
    todo.:(st) <- IndexMap.empty;
    IndexMap.iter (fun lp set -> propagate_position st lp !set) map
  in
  let lrcs = IndexSet.split_by_run stacks.label stacks.tops in
  let trs = machine.outgoing.:(initial) in
  let process tr lrcs =
    let st = machine.target.:(tr) in
    let filter = machine.label.:(tr).filter in
    List.filter begin fun (lr1, lrcs) ->
      if IndexSet.mem lr1 filter then begin
        List.iteri begin fun pos nts ->
          IndexMap.iter begin fun nt la ->
            let pos = inject_position positions nt (pos + 1) in
            IndexSet.iter begin fun lrc ->
              let lp = pack_position positions pos lrc in
              schedule st lp st lp la
            end lrcs
          end nts
        end rcs.:(lr1).reductions;
        false
      end else
        true
    end lrcs
  in
  let counter = ref 0 in
  let unhandled = IndexSet.fold process trs lrcs in
  fixpoint ~counter ~propagate pending;
  stopwatch 2 "computed coverage (%d transitions, %d iterations, \
               %d unhandled initial, %d unhandled inner, %d unhandled stacks)"
    (Vector.fold_left (fun acc trs -> acc + List.length trs) 0 transitions)
    !counter
    (List.length unhandled)
    !unhandled_inner
    !unhandled_stack
  ;
  ()
