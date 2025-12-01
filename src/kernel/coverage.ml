open Utils
open Misc
open Fix.Indexing
open Info

(* Compact representation of a position in a rule *)
module Position = Unsafe_cardinal()
type 'g position = 'g Position.t

type 'g positions = {
  desc: ('g position, 'g nonterminal index * int) vector;
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
        Vector.fold_left (+) (Vector.length_as_int length) length
    end)
  in
  let desc = Vector.make' n
      (fun () -> Index.of_int (Nonterminal.cardinal g) 0, 0)
  in
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
  assert (Index.equal nt (fst p.desc.:(pn)));
  pn

let project_position (type g) (p : g positions) pos =
  p.desc.:(pos)

let previous_position (type g) (p : g positions) pos =
  let nt, pos' = p.desc.:(pos) in
  if pos' = 0 then
    Either.Left nt
  else
    Either.Right (Index.of_int (Vector.length p.desc) ((pos :> int) - 1))

let get_set map i =
  match IndexMap.find_opt i map with
  | None -> IndexSet.empty
  | Some set -> set

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
  let pending = ref [] in
  let todo = Vector.make state_count IndexMap.empty in
  let schedule st lrc pos la =
    let la =
      match IndexMap.find_opt lrc reached.:(st) with
      | None -> la
      | Some positions ->
        match IndexMap.find_opt pos positions with
        | None -> la
        | Some la' -> IndexSet.diff la la'
    in
    if not (IndexSet.is_empty la) then (
      let map = todo.:(st) in
      if IndexMap.is_empty map then (
        push pending st;
        todo.:(st) <- IndexMap.singleton lrc (IndexMap.singleton pos la);
      ) else
        todo.:(st) <- IndexMap.update lrc (function
            | None -> Some (IndexMap.singleton pos la)
            | Some poss -> Some (IndexMap.update pos (union_update la) poss)
          ) map;
    )
  in
  let propagate_position st lrc pos la =
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
    if not (IndexSet.is_empty la) then
      match previous_position positions pos with
      | Either.Right pos' ->
        let lrcs = IndexSet.split_by_run stacks.label (stacks.prev lrc) in
        let trs = machine.outgoing.:(st) in
        let process tr lrcs =
          let target = machine.target.:(tr) in
          let filter = machine.label.:(tr).filter in
          List.filter begin fun (lr1, lrcs) ->
            if IndexSet.mem lr1 filter then (
              IndexSet.iter
                (fun lrc -> schedule target lrc pos' la)
                lrcs;
              false
            ) else
              true
          end lrcs
        in
        ignore (IndexSet.fold process trs lrcs)
      | Either.Left nt ->
        let src = stacks.label lrc in
        let tgt = Transition.find_goto_target g src nt in
        List.iteri begin fun pos nts ->
          IndexMap.iter begin fun nt la' ->
            let la = IndexSet.inter la la' in
            if not (IndexSet.is_empty la) then
              schedule st lrc (inject_position positions nt pos) la
          end nts
        end rcs.:(tgt).reductions
  in
  let propagate st =
    let map = todo.:(st) in
    todo.:(st) <- IndexMap.empty;
    reached.@(st) <-
      IndexMap.union
        (fun _ ma mb -> Some (IndexMap.union (fun _ sa sb -> Some (IndexSet.union sa sb)) ma mb))
        map;
    IndexMap.iter (fun lrc -> IndexMap.iter (propagate_position st lrc)) map
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
            IndexSet.iter (fun lrc -> schedule st lrc pos la) lrcs
          end nts
        end rcs.:(lr1).reductions;
        (*rcs.:(lr1).reductions
          IndexSet.iter
          (fun lrc -> schedule target lrc pos' la)
          lrcs;*)
        false
      end else
        true
    end lrcs
  in
  ignore (IndexSet.fold process trs lrcs);
  fixpoint ~propagate pending;
  stopwatch 2 "computed coverage";
  ()
