open Utils
open Misc
open Fix.Indexing
open Info

(*
  Compact representation of a position in a reduction
  (a pair `(n, A)` interpreted as `pop n elements before
  following the goto transition labelled `A`).
*)
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
  Opt.some pn

let project_position (type g) (p : g positions) pos =
  p.desc.:(pos)

let previous_position (type g) (p : g positions) pos =
  match Opt.prj pos with
  | None -> Either.Right Opt.none
  | Some pos' ->
    match p.desc.:(pos') with
    | (nt, 0) -> Either.Left nt
    | _ -> Either.Right (Option.get (Index.pred pos))

type 'g pending_reductions = ('g nonterminal, 'g terminal indexset) indexmap list

type ('g, 'lrc) state = {
  lrc: 'lrc index;
  failed: 'g terminal indexset;
  pending: 'g pending_reductions;
  mutable successors: ('g lr0 indexset * ('g, 'lrc) state) list;
  mutable predecessors: ('g lr0 indexset * ('g, 'lrc) state) list;
}

let rec filter_reductions la = function
  | [] -> []
  | nts :: rest ->
    let nts =
      IndexMap.filter_map begin fun _nt la' ->
        let la = IndexSet.inter la la' in
        if IndexSet.is_empty la
        then None
        else Some la
      end nts
    in
    let rest = filter_reductions la rest in
    if IndexMap.is_empty nts && List.is_empty rest
    then []
    else nts :: rest

let rec merge_reductions rs1 rs2 =
  match rs1, rs2 with
  | [], rs | rs, [] -> rs
  | r1 :: rs1, r2 :: rs2 ->
    let merge_lookaheads _ s1 s2 = Some (IndexSet.union s1 s2) in
    let r = IndexMap.union merge_lookaheads r1 r2 in
    r :: merge_reductions rs1 rs2

let enumerate (type g lrc)
    (g : g grammar)
    (rcs : (g lr1, g Redgraph.reduction_closure) vector)
    (stacks : (g, lrc) Automata.stacks)
  =
  let module Map = Map.Make(struct
      type t = g terminal indexset * g pending_reductions
      let compare (s1,l1) (s2,l2) =
        let c = IndexSet.compare s1 s2 in
        if c <> 0 then c else
          List.compare (fun m1 m2 -> IndexMap.compare IndexSet.compare m1 m2)
            l1 l2
    end)
  in
  let table = Vector.make stacks.domain Map.empty in
  let todo = ref [] in
  let states = ref 0 in
  let visit lrc key =
    let map = table.:(lrc) in
    match Map.find_opt key map with
    | Some result -> result
    | None ->
      let failed, pending = key in
      let state = {
        lrc; failed; pending;
        successors = [];
        predecessors = [];
      }
      in
      push todo state;
      incr states;
      table.:(lrc) <- Map.add key state map;
      state
  in
  let populate state =
    assert (List.is_empty state.successors);
    match state.pending with
    | [] -> ()
    | nts :: pending ->
      stacks.prev state.lrc |> IndexSet.iter @@ fun lrc ->
      let lr1 = stacks.label lrc in
      let reached = ref IndexSet.empty in
      let rec explore nts acc =
        IndexMap.fold begin fun nt lookaheads (failed, pending) ->
          let target = Transition.find_goto_target g lr1 nt in
          let reductions = rcs.:(target) in
          let failed =
            IndexSet.fused_inter_union ~acc:failed
              lookaheads reductions.failing
          in
          match filter_reductions lookaheads reductions.all_reductions with
          | [] ->
            reached := IndexSet.add (Lr1.to_lr0 g target) !reached;
            (failed, pending)
          | nts :: pending' ->
            if IndexMap.is_empty nts then
              reached := IndexSet.add (Lr1.to_lr0 g target) !reached;
            explore nts (failed, merge_reductions pending' pending)
        end nts acc
      in
      let key = explore nts (state.failed, pending) in
      let state' = visit lrc key in
      let reached = !reached in
      state.successors <- (reached, state') :: state.successors;
      state'.predecessors <- (reached, state) :: state'.predecessors
  in
  let _initials =
    IndexSet.rev_map_elements stacks.tops begin fun lrc ->
      let rc = rcs.:(stacks.label lrc) in
      visit lrc (rc.failing, rc.all_reductions)
    end
  in
  let counter = ref 0 in
  fixpoint ~counter ~propagate:populate todo;
  let reachable = ref IndexSet.empty in
  let initial = ref IndexSet.empty in
  Vector.iter begin fun map ->
    Map.iter begin fun _ state ->
      match state.successors, state.predecessors with
      | [], [] ->
        initial := IndexSet.add (Lr1.to_lr0 g (stacks.label state.lrc)) !initial
      | [], predecessors ->
        List.iter begin fun (lr0s, _) ->
          (*assert (IndexSet.is_not_empty lr0s);*)
          reachable := IndexSet.union lr0s !reachable
        end predecessors
      | (_ :: _), _ -> ()
    end map
  end table;
  Printf.eprintf "deterministic enumeration: %d cycles, reached %d states, %d reduction patterns, %d initial patterns\n"
    !counter !states (IndexSet.cardinal !reachable) (IndexSet.cardinal !initial);
  IndexSet.iter begin fun lr0 ->
    let items = Coverage.string_of_items_for_filter g lr0 in
    Printf.eprintf "| /%s\n  { ... }\n" (String.concat "\n  /" items)
  end !initial;
  IndexSet.iter begin fun lr0 ->
    let items = Coverage.string_of_items_for_filter g lr0 in
    Printf.eprintf "| [_* /%s]\n  { ... }\n" (String.concat "\n      /" items)
  end !reachable;
