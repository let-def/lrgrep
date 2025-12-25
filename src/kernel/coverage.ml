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

(* Computation of free failures:

   Given a set of stacks and an initial reduction, find each lookahead that end
   up being rejected by at least one stack of the set.
*)

(* A failure node associates to a goto transition the set of lookaheads that can
   be rejected (directly or not). *)
type ('g, 'lrc) failure_node = {

  (* Characterizing the goto transition *)

  (* The lrc index represent the set of stacks ending in this state. *)
  lrc: 'lrc index;
  (* The nonterminal labelling the goto transition to follow from these stacks. *)
  nt: 'g nonterminal index;

  (* Results of the analysis *)

  (* The lookahead symbols that are rejected by at least one of the stack that
     end in [lrc] after following the transition labelled [nt], written [lrc+nt]. *)
  mutable fallible: 'g terminal indexset;

  (* forward transitions (a transition [nd, la] is in [fwd] if [nd] is reachable by
     reducing at least one stack from [lrc+nt], while looking ahead a symbol in [la]. *)
  mutable fwd: (('g, 'lrc) failure_node * 'g terminal indexset) list;

  (* backward transition, the converse of the [fwd] relation *)
  mutable bkd: (('g, 'lrc) failure_node * 'g terminal indexset) list;

  (* [fail_fwd] records the shortest path to certain failures.
     E.g. to find how to make a certain terminal [t ∈ nd.fallible] fails, look
     for a transition [nd', la] in [nd.fail_fwd] such that [t ∈ la]:
     - if there is one, repeat the search in [nd']
     - if there is none, [t] is rejected directly by [nd]
  *)
  mutable fail_fwd: (('g, 'lrc) failure_node * 'g terminal indexset) list;

}

(* Staged and cached lazy computation for construction the graph of failure nodes:
   1. [let finder = free_failures grammar stacks rcs]
      lazily constructs the graph
   2. [finder lrcs nt depth] is the list of failure nodes reachable by following
      a goto transition labelled [nt] [depth] states deep in the stacks
      described by [lrcs].
*)
let free_failures (type g lrc)
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

(* Compute coverage of a machine (an automaton realizing an error
   specification).
*)

type ('g, 'lrc) lrc_position = ('g position, 'lrc) Prod.n index

type ('g, 'st, 'lrc) coverage_transition = {
  source: 'st index;
  source_position: ('g, 'lrc) lrc_position;
  target_position: ('g, 'lrc) lrc_position;
  lookahead: 'g terminal indexset;
}

type ('g, 'st, 'lrc) machine_coverage = {
  transitions: ('st, ('g, 'st, 'lrc) coverage_transition list) vector;
  unhandled_initial: 'lrc indexset;
  unhandled_lookaheads: ('st, (('g, 'lrc) lrc_position * 'g terminal indexset) list) vector;
  unhandled_predecessors: ('st, (('g, 'lrc) lrc_position * 'lrc indexset * 'g terminal indexset) list) vector;
}

let coverage (type g r st tr lrc)
    (g : g grammar)
    (branches : (g, r) Spec.branches)
    (machine : (g, r, st, tr) Automata.Machine.t)
    (stacks : (g, lrc) Automata.stacks)
    (rcs : (g lr1, g Redgraph.reduction_closure) vector)
    (positions : g positions)
    initial
  : (g, st, lrc) machine_coverage
  =
  let state_count = Vector.length machine.outgoing in
  let reached = Vector.make state_count IndexMap.empty in
  let transitions = Vector.make state_count [] in
  let unhandled_lookaheads = Vector.make state_count [] in
  let unhandled_predecessors = Vector.make state_count [] in
  let pending = ref [] in
  let todo = Vector.make state_count IndexMap.empty in
  let schedule source source_position target target_position la =
    let reached = get_map reached target target_position in
    let lookahead = IndexSet.diff la !reached in
    if IndexSet.is_not_empty lookahead then (
      reached @:= IndexSet.union lookahead;
      let todo = get_map todo target target_position in
      if IndexSet.is_empty !todo then
        push pending target;
      todo @:= IndexSet.union lookahead;
      transitions.@(target) <-
        List.cons {source; source_position; target_position; lookahead}
    )
  in
  let collect_unhandled_lrc unhandled =
    List.fold_right
      (fun (_lr1,lrcs) set -> IndexSet.union lrcs set)
      unhandled IndexSet.empty
  in
  let propagate_position st lp la =
    let la =
      List.fold_left begin fun la (br, _, _) ->
        if Boolvector.test branches.is_partial br then
          la
        else
          (* FIXME: check for unreachable clauses *)
          match branches.lookaheads.:(br) with
          | None -> IndexSet.empty
          | Some la' -> IndexSet.diff la la'
      end la machine.accepting.:(st)
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
          unhandled_lookaheads.@(st) <- List.cons (lp, la)
      | Right pos' ->
        let lrcs = stacks.prev lrc in
        if IndexSet.is_empty lrcs then
          (* Only initial state has no predecessors.  But all lookaheads should
             have been handled before reaching this configuration. *)
          assert false;
        (* Group by lr1 core *)
        let lrcs = IndexSet.split_by_run stacks.label lrcs in
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
        let unhandled = IndexSet.fold process trs lrcs in
        if not (List.is_empty unhandled) then
          unhandled_predecessors.@(st) <-
            List.cons (lp, collect_unhandled_lrc unhandled, la)
  in
  let propagate st =
    let map = todo.:(st) in
    todo.:(st) <- IndexMap.empty;
    IndexMap.iter (fun lp set -> propagate_position st lp !set) map
  in
  let lrcs = IndexSet.split_by_run stacks.label stacks.tops in
  let trs = machine.outgoing.:(initial) in
  let process_initial tr lrcs =
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
  let unhandled_initial =
    collect_unhandled_lrc (IndexSet.fold process_initial trs lrcs)
  in
  let total_list_elements v = Vector.fold_left (fun acc xs -> acc + List.length xs) 0 v in
  fixpoint ~counter ~propagate pending;
  stopwatch 2 "computed coverage (%d transitions, %d iterations, %d uncovered initial states, \
               %d states with uncovered lookaheads, %d states with uncovered predecessors)"
    (total_list_elements transitions)
    !counter
    (IndexSet.cardinal unhandled_initial)
    (total_list_elements unhandled_lookaheads)
    (total_list_elements unhandled_predecessors)
  ;
  {transitions; unhandled_initial;
   unhandled_lookaheads; unhandled_predecessors}
