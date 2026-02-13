open Utils
open Misc
open Fix.Indexing
open Info

type 'g pending_reductions = ('g nonterminal, 'g terminal indexset) indexmap list

type ('g, 'n) edge = {
  source: 'n index;
  target: 'n index;
  reached: 'g lr0 indexset;
}

type ('g, 'lrc, 'n) state = {
  lrc: 'lrc index;
  failed: 'g terminal indexset;
  pending: 'g pending_reductions;
  mutable successors: ('g, 'n) edge list;
  mutable predecessors: ('g, 'n) edge list;
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

let filter_next_stacks la next =
  List.filter_map begin fun (stack, la', tree) ->
    let la = IndexSet.inter la la' in
    if IndexSet.is_empty la
    then None
    else Some (stack, la, tree)
  end next

let rec merge_reductions rs1 rs2 =
  match rs1, rs2 with
  | [], rs | rs, [] -> rs
  | r1 :: rs1, r2 :: rs2 ->
    let merge_lookaheads _ s1 s2 = Some (IndexSet.union s1 s2) in
    let r = IndexMap.union merge_lookaheads r1 r2 in
    r :: merge_reductions rs1 rs2

type ('g, 'lrc, 'n) _graph = {
  initials: ('lrc, 'n index) indexmap;
  states: ('n, ('g, 'lrc, 'n) state) vector;
}

type ('g, 'lrc) graph = Graph : ('g, 'lrc, 'n) _graph -> ('g, 'lrc) graph

let enumerate (type g lrc)
    (g : g grammar)
    (rcs : (g lr1, g Redgraph.reduction_closure) vector)
    (stacks : (g, lrc) Automata.stacks)
  =
  let open IndexBuffer in
  let module States = Gen.Make() in
  let states = States.get_generator () in
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
  let visit lrc key =
    let map = table.:(lrc) in
    match Map.find_opt key map with
    | Some result -> result
    | None ->
      let failed, pending = key in
      let index = Gen.add states {
        lrc; failed; pending;
        successors = [];
        predecessors = [];
      } in
      push todo index;
      table.:(lrc) <- Map.add key index map;
      index
  in
  let populate source =
    let state = Gen.get states source in
    assert (List.is_empty state.successors);
    match state.pending with
    | [] -> ()
    | nts :: pending ->
      stacks.prev state.lrc |> IndexSet.iter @@ fun lrc ->
      let lr1 = stacks.label lrc in
      let maximal = ref IndexSet.empty in
      let failed = ref state.failed in
      let pending = ref [pending] in
      let rec explore nts =
        IndexMap.iter begin fun nt lookaheads ->
          let target = Transition.find_goto_target g lr1 nt in
          let reductions = rcs.:(target) in
          failed := IndexSet.fused_inter_union ~acc:!failed
              lookaheads reductions.failing;
          let rec visit_stacks candidate (stacks : g Redgraph.stack_tree) lookaheads =
            let nexts = filter_next_stacks lookaheads stacks.next in
            List.iter (fun (stack, la, next) ->
                visit_stacks (List.hd stack) next la) nexts;
            begin match filter_reductions lookaheads stacks.reductions with
              | [] ->
                if List.is_empty nexts then
                  maximal := IndexSet.add (Lr1.to_lr0 g candidate) !maximal
              | nts :: pending' ->
                explore nts;
                push pending pending'
            end
          in
          visit_stacks target reductions.stacks lookaheads
        end nts
      in
      explore nts;
      let pending = List.fold_left merge_reductions [] !pending in
      let target = visit lrc (!failed, pending) in
      let state' = Gen.get states target in
      let reached = if List.is_empty pending then !maximal else IndexSet.empty in
      let edge = {source; target; reached} in
      state.successors <- edge :: state.successors;
      state'.predecessors <- edge :: state'.predecessors
  in
  let initials =
    IndexMap.inflate begin fun lrc ->
      let rc = rcs.:(stacks.label lrc) in
      visit lrc (rc.failing, rc.all_reductions)
    end stacks.tops
  in
  let counter = ref 0 in
  fixpoint ~counter ~propagate:populate todo;
  let states = Gen.freeze states in
  let reachable = ref IndexSet.empty in
  let initial = ref IndexSet.empty in
  Vector.iter begin fun state ->
    match state.successors, state.predecessors with
    | [], [] ->
      let rec visit_stacks candidate (stacks : g Redgraph.stack_tree) lookaheads =
        match filter_next_stacks lookaheads stacks.next with
        | [] -> initial := IndexSet.add (Lr1.to_lr0 g candidate) !initial
        | nexts ->
          List.iter (fun (stack, la, next) ->
              visit_stacks (List.hd stack) next la) nexts
      in
      let lr1 = stacks.label state.lrc in
      visit_stacks lr1 rcs.:(lr1).stacks (Terminal.regular g)
    | [], predecessors ->
      List.iter begin fun edge ->
        (*assert (IndexSet.is_not_empty lr0s);*)
        reachable := IndexSet.union edge.reached !reachable
      end predecessors
    | (_ :: _), _ -> ()
  end states;
  Printf.eprintf "deterministic enumeration: %d cycles, reached %d states, \
                  %d reduction patterns, %d initial patterns, \
                  %d initials without reductions\n"
    !counter (Vector.length_as_int states)
    (IndexSet.cardinal !reachable) (IndexSet.cardinal !initial)
    (IndexMap.cardinal (IndexMap.filter (fun _ ix -> List.is_empty states.:(ix).successors) initials))
  ;
  initial := IndexSet.diff !initial !reachable;
  IndexSet.iter begin fun lr0 ->
    let items = Coverage.string_of_items_for_filter g lr0 in
    Printf.eprintf "| /%s\n  { ... }\n" (String.concat "\n  /" items)
  end !initial;
  IndexSet.iter begin fun lr0 ->
    let items = Coverage.string_of_items_for_filter g lr0 in
    Printf.eprintf "| [_* /%s]\n  { ... }\n" (String.concat "\n      /" items)
  end !reachable;
  Graph {initials; states}
