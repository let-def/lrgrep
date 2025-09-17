(* MIT License

   Copyright (c) 2025 Frédéric Bour

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
 *)

(** This module is responsible for computing viable reductions in a LR(1) parser
    generator. It generates a graph of states, where each state represents a
    configuration of the parser, including the top of the stack, the rest of the
    stack, and the current lookahead set. The module also computes transitions
    between these states based on possible reductions and goto actions.
*)

open Fix.Indexing
open Utils
open Misc
open Info

(* Step 1: pre-compute closure of reductions of ϵ-rules *)

type ('g, 'a) with_lookahead = 'a * 'g terminal indexset

(* Group items being reduced by their depth (reductions with one producer, two producers, etc). *)
let group_reductions g = function
  | [] -> []
  | items ->
    let rec group depth acc = function
      | [] -> [acc]
      | (it, la) :: rest when depth = Item.position g it ->
        let lhs = Production.lhs g (Item.production g it) in
        group depth (IndexMap.update lhs (union_update la) acc) rest
      | otherwise ->
        acc :: group (depth + 1) IndexMap.empty otherwise
    in
    let compare_items (it1, _) (it2, _) =
      Int.compare (Item.position g it1) (Item.position g it2)
    in
    group 0 IndexMap.empty (List.sort compare_items items)

type 'g reduce_closure = {
  failing: 'g terminal indexset;
  reductions: ('g nonterminal, 'g terminal indexset) indexmap list;
  stacks: ('g, 'g lr1 index list) with_lookahead list;
}

let add_failing g r reject la =
  r := IndexSet.union (Terminal.intersect g reject la) !r

let reduce_closures (type g) (g : g grammar) : (g lr1, g reduce_closure) vector =
  Vector.init (Lr1.cardinal g) @@ fun lr1 ->
  let failing = ref IndexSet.empty in
  let rec pop lookahead acc (item : g item index) = function
    | [] -> let items, stacks = acc in ((item, lookahead) :: items, stacks)
    | hd :: tl as stack ->
      match Item.prev g item with
      | Some item' -> pop lookahead acc item' tl
      | None ->
        let lhs = Production.lhs g (Item.production g item) in
        let stack = Transition.find_goto_target g hd lhs :: stack in
        let items, stacks = acc in
        let acc = (items, (stack, lookahead) :: stacks) in
        reduce lookahead acc stack
  and reduce lookahead acc stack =
    let lr1 = List.hd stack in
    add_failing g failing (Lr1.reject g lr1) lookahead;
    IndexSet.fold begin fun red acc ->
      let lookahead = Terminal.intersect g (Reduction.lookaheads g red) lookahead in
      if IndexSet.is_empty lookahead
      then acc
      else pop lookahead acc (Item.last g (Reduction.production g red)) stack
    end (Reduction.from_lr1 g lr1) acc
  in
  let items, stacks = reduce (Terminal.all g) ([],[]) [lr1] in
  let reductions = group_reductions g items in
  let failing = !failing in
  {failing; reductions; stacks}

let rec filter_reductions g la = function
  | [] -> []
  | r :: rs as rrs ->
    let filtered = ref false in
    let r' =
      IndexMap.filter_map (fun _ la' ->
          let la'' = Terminal.intersect g la la' in
          if la' != la'' then filtered := true;
          if IndexSet.is_empty la'' then None else Some la''
        ) r
    in
    let rs' = filter_reductions g la rs in
    if rs == rs' && not !filtered
    then rrs
    else r' :: rs'

let rec filter_stacks g la acc = function
  | [] -> acc
  | (x, la') :: xs ->
    let la' = Terminal.intersect g la la' in
    let acc =
      if IndexSet.is_empty la'
      then acc
      else (x, la') :: acc
    in
    filter_stacks g la' acc xs

let rec merge_reduction_step map acc = function
  | [] -> (map, acc)
  | [] :: _ -> assert false
  | (r :: rs) :: rrs ->
    let acc = if List.is_empty rs then acc else rs :: acc in
    let augment _ a b = Some (IndexSet.union a b) in
    let map = IndexMap.union augment r map in
    merge_reduction_step map acc rrs

let rec merge_reductions = function
  | [] -> []
  | rrs ->
    let r, rrs' = merge_reduction_step IndexMap.empty [] rrs in
    r :: merge_reductions rrs'

let goto_reduce_closures (type g) (g : g grammar) rcs
  : (g goto_transition, g reduce_closure) vector
  =
  let sentinel = {failing = IndexSet.empty; reductions = []; stacks = []} in
  let table = Vector.make (Transition.goto g) sentinel in
  Index.rev_iter (Transition.goto g) begin fun gt ->
    let tr = Transition.of_goto g gt in
    let src = Transition.source g tr in
    let tgt = Transition.target g tr in
    let stacks = ref [] in
    let reductions = ref [] in
    let failing = ref IndexSet.empty in
    let rec visit_target tgt la =
      let rc = rcs.:(tgt) in
      add_failing g failing rc.failing la;
      stacks := filter_stacks g la !stacks rc.stacks;
      match filter_reductions g la rc.reductions with
      | [] -> ()
      | r :: rs ->
        if not (List.is_empty rs) then
          push reductions rs;
        IndexMap.iter begin fun nt la' ->
          let la' = Terminal.intersect g la la' in
          if not (IndexSet.is_empty la') then
            visit_goto (Transition.find_goto g src nt) la'
        end r
    and visit_goto gt' la =
      if Index.compare gt' gt <= 0 then
        visit_target (Transition.target g (Transition.of_goto g gt')) la
      else
        let rc = table.:(gt') in
        add_failing g failing rc.failing la;
        stacks := filter_stacks g la !stacks rc.stacks;
        let rs = filter_reductions g la rc.reductions in
        if not (List.is_empty rs) then
          push reductions rs
    in
    visit_target tgt (Terminal.all g);
    let failing = !failing in
    let stacks = !stacks in
    let reductions = merge_reductions !reductions in
    table.:(gt) <- {failing; reductions; stacks}
  end;
  table

module Inner = Unsafe_cardinal()
type 'g inner = 'g Inner.t

type ('g, 'n) states = ('g lr1, 'g inner) Sum.n

type 'g state =
  | Lr1 of 'g lr1 index
  | Goto of 'g lr1 indexset * 'g lr1 index * 'g terminal indexset

type ('g, 'node, 'step) step = {
  next: 'step index;
  reachable: 'node indexset;
  goto: 'node indexset;
}

(* TODO determinize: the deterministic viable reductions might be much smaller?! *)

let viable2 (type g) (g : g grammar) rc grc =
  let module Nodes = IndexBuffer.Gen.Make() in
  let nodes = Nodes.get_generator () in
  let table = Vector.make (Transition.goto g) IndexSet.Map.empty in
  let get_memoize gt la ~f =
    let map = table.:(gt) in
    match IndexSet.Map.find_opt la map with
    | Some index -> index
    | None ->
      let r = IndexBuffer.Gen.reserve nodes in
      table.:(gt) <- IndexSet.Map.add la (IndexBuffer.Gen.index r) map;
      IndexBuffer.Gen.commit nodes r (f ());
      IndexBuffer.Gen.index r
  in
  let rec visit_reductions la lr1s = function
    | [] -> []
    | nts :: rest ->
      let lr1s = IndexSet.bind lr1s (Lr1.predecessors g) in
      let curr =
        let follow_nt nt la' acc =
          let la' = Terminal.intersect g la la' in
          if IndexSet.is_empty la' then
            acc
          else
            let follow src = visit_goto (Transition.find_goto g src nt) la' in
            IndexSet.map follow lr1s :: acc
        in
        IndexMap.fold follow_nt nts []
      in
      let next = visit_reductions la lr1s rest in
      match curr, next with
      | [], [] -> []
      | _ -> List.fold_right IndexSet.union curr IndexSet.empty :: next
  and visit_goto gt la =
    get_memoize gt la ~f:begin fun () ->
      let transitions =
        visit_reductions la
          (IndexSet.singleton (Transition.source g (Transition.of_goto g gt)))
          grc.:(gt).reductions
      in
      (gt, la, transitions)
    end
  in
  let initials = Vector.init (Lr1.cardinal g) (fun lr1 ->
      visit_reductions (Terminal.all g)
        (IndexSet.singleton lr1)
        rc.:(lr1).reductions
    )
  in
  let nodes = IndexBuffer.Gen.freeze nodes in
  stopwatch 2 "viable2: %d nodes\n" (Vector.length_as_int nodes);
  (* Compute the set reachable states (closure of successors). *)
  let reachable_from =
    Vector.map
      (fun (_, _, transitions) ->
         List.fold_right IndexSet.union transitions IndexSet.empty
      ) nodes
  in
  stopwatch 2 "prepared big-step successors";
  Tarjan.close_relation reachable_from;
  stopwatch 2 "closed the big-step successors";
  (* Implement small steps with sharing *)
  let open IndexBuffer in
  let module Steps = Gen.Make() in
  let steps = Steps.get_generator () in
  let zero = Gen.add steps IndexSet.Map.empty in
  let index = Dyn.make {
      next = zero;
      goto = IndexSet.empty;
      reachable = IndexSet.empty;
    }
  in
  let get goto next =
    let map = Gen.get steps next in
    match IndexSet.Map.find_opt goto map with
    | Some step -> step
    | None ->
      let step = Gen.add steps IndexSet.Map.empty in
      Gen.set steps next (IndexSet.Map.add goto step map);
      (* Compute reachability for all steps of a reduction *)
      let reachable = IndexSet.bind goto (Vector.get reachable_from) in
      let reachable = IndexSet.union reachable (Dyn.get index next).reachable in
      Dyn.set index step {next; goto; reachable};
      step
  in
  let pack_transitions trs = List.fold_right get trs zero in
  let add_reachables (gt, la, trs) = (gt, la, pack_transitions trs) in
  let _transitions = Vector.map add_reachables nodes in
  let _initials = Vector.map pack_transitions initials in
  stopwatch 2 "closed the small-step successors (%d elements)"
    (Vector.length_as_int (Gen.freeze steps))

(* Step 2: explore viable reductions *)

module Viable = Unsafe_cardinal()
type 'g viable = 'g Viable.t

(* A goto transition, which includes the target state,
   the set of lookahead symbols that permitted to follow it,
   the source state (different for inner/epsilon-reductions, and outer ones),
   and the reduction to be performed. *)
type ('g, 'a) goto_transition = {
  target: 'g viable index;
  lookahead: 'g terminal indexset;
  source: 'a;
  reduction: 'g reduction index;
}

(* A step in the reduction process, which includes the set of reachable states and a list
   of goto candidates. *)
type ('g, 'a) reduction_step = {
  reachable: 'g viable indexset;
  goto_transitions: ('g, 'a) goto_transition list;
}

(* Transitions within the same state (inner) and transitions to other states (outer). *)
type 'g inner_transitions = ('g, unit) reduction_step list
type 'g outer_transitions = ('g, 'g lr1 indexset) reduction_step list

type 'g transitions = {
  inner: 'g inner_transitions;
  outer: 'g outer_transitions;
}

(* A configuration of a reduction simulation state,
   including the top of the stack, the rest of the stack,
   and the lookahead symbols that permitted to reach it. *)
type 'g config = {
  top: 'g lr1 index;
  rest: 'g lr1 index list;
  lookahead: 'g terminal indexset;
}

type 'g t = {
  initial: ('g lr1, 'g outer_transitions) vector;
  config: ('g viable, 'g config) vector;
  reachable_from: ('g viable, 'g viable indexset) vector;
  transitions: ('g viable, 'g transitions) vector;
}

let make (type g) (g : g grammar) (_rc : (g lr1, g reduce_closure) vector) : g t =
  stopwatch 2 "constructing viable reduction graph";
  let open Info in
  let module States = IndexBuffer.Gen.Make() in
  let open Viable.Eq(struct type t = g include States end) in
  let Refl : (g viable, States.n) eq = eq in
  (* Get the generator for state indices. *)
  let states = States.get_generator () in
  (* A hashtable to store configurations and their corresponding state indices. *)
  let nodes = Hashtbl.create 7 in
  let _reductions = group_reductions g in
  (* Create states by visiting configurations and their outgoing transitions. *)
  let rec visit_config config =
    match Hashtbl.find_opt nodes config with
    | Some state -> state
    | None ->
      let reservation = IndexBuffer.Gen.reserve states in
      let index = IndexBuffer.Gen.index reservation in
      Hashtbl.add nodes config index;
      IndexBuffer.Gen.commit states reservation (config, visit_transitions config);
      index
  (* Visit all outgoing transitions of a given configuration. *)
  and visit_transitions config =
    visit_inner config (failwith "TODO") (*reductions.:(config.top)*)
  (* Follow inner transitions for a given configuration and list of reductions. *)
  and visit_inner config = function
    | [] -> ([], [])
    | gotos :: next ->
      let (inner, outer) = match config.rest with
        | top :: rest ->
          visit_inner {config with top; rest} next
        | [] ->
          ([], visit_outers config.lookahead (IndexSet.singleton config.top) next)
      in
      let process_goto red =
        let prod = Reduction.production g red in
        let lookahead = Terminal.intersect g (Reduction.lookaheads g red) config.lookahead in
        if IndexSet.is_empty lookahead then
          None
        else
          let top = Transition.find_goto_target g config.top (Production.lhs g prod) in
          let rest = config.top :: config.rest in
          let target = visit_config {top; rest; lookahead} in
          Some {target; lookahead; source=(); reduction=red}
      in
      (List.filter_map process_goto gotos :: inner, outer)
  (* Follow outer transitions for a given lookahead set,
     set of LR(1) states, and list of reductions. *)
  and visit_outers lookahead lr1_states = function
    | [] -> []
    | gotos :: next ->
      let lr1_states = IndexSet.bind lr1_states (Lr1.predecessors g) in
      visit_outer lookahead lr1_states gotos next
  (* Helper function for visiting a single outer transition. *)
  and visit_outer lookahead lr1_states gotos next =
    let next = visit_outers lookahead lr1_states next in
    let process_goto acc red =
      let lookahead =
        Terminal.intersect g lookahead (Reduction.lookaheads g red)
      in
      if IndexSet.is_empty lookahead then acc
      else
        let lhs = Production.lhs g (Reduction.production g red) in
        let process_target source acc =
          (source, Transition.find_goto_target g source lhs) :: acc
        in
        IndexSet.fold process_target lr1_states []
        |> List.sort (fun (s1,t1) (s2,t2) ->
             let c = Index.compare t1 t2 in
             if c <> 0 then c else Index.compare s1 s2)
        |> merge_group
           ~equal:Index.equal
           ~group:(fun top sources ->
             let config = {top; rest = []; lookahead} in
             let source = IndexSet.of_list sources in
             let target = visit_config config in
             {source; target; lookahead; reduction=red}
           )
    in
    let gotos = List.fold_left process_goto [] gotos in
    gotos :: next
  in
  (* Compute the initial set of transitions for each LR(1) state. *)
  let initial = Vector.init (Lr1.cardinal g) (fun lr1 ->
      match failwith "TODO" (*reductions.:(lr1)*) with
      | [] -> []
      | gotos :: next ->
        visit_outer (Terminal.regular g) (IndexSet.singleton lr1) gotos next
    )
  in
  let states = IndexBuffer.Gen.freeze states in
  stopwatch 2 "constructed viable reduction graph with %d nodes" (cardinal States.n);
  (* Compute the set reachable states (closure of successors). *)
  let reachable_from =
    let add_target acc step = IndexSet.add step.target acc in
    let add_targets acc l =
      List.fold_left (List.fold_left add_target) acc l
    in
    Vector.mapi
      (fun self (_, (inner, outer)) ->
         add_targets (add_targets (IndexSet.singleton self) inner) outer)
      states
  in
  Tarjan.close_relation reachable_from;
  stopwatch 2 "closed the big-step successors";
  (* Compute reachability for all steps of a reduction *)
  let rec process_steps = function
    | [] -> []
    | step :: steps ->
      let steps = process_steps steps in
      let acc = match steps with
        | [] -> IndexSet.empty
        | x :: _ -> x.reachable
      in
      let reachable =
        let add_reach acc c = IndexSet.union acc reachable_from.:(c.target) in
        List.fold_left add_reach acc step
      in
      {reachable; goto_transitions=step} :: steps
  in
  let make_reduction_step (_, (inner, outer)) =
    {inner = process_steps inner; outer = process_steps outer}
  in
  let initial = Vector.map process_steps initial in
  let config = Vector.map fst states in
  let transitions = Vector.map make_reduction_step states in
  stopwatch 2 "closed the small-step successors";
  {initial; reachable_from; config; transitions}

let get_stack vr state =
  let {top; rest; _} = vr.config.:(state) in
  List.rev (top :: rest)

(* [string_of_stack st] is a string representing the suffix of the stacks
   recognized by when reaching state [st]. *)
let string_of_stack g vr state =
  string_concat_map " " (Lr1.to_string g) (get_stack vr state)
