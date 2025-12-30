open Utils
open Misc
open Fix.Indexing
open Info

(* Computation of free failures:

   Given a set of stacks and an initial reduction, find each lookahead that end
   up being rejected by at least one stack of the set.
*)

(* A failure node associates to a goto transition the set of lookaheads that can
   be rejected (directly or not). *)
type ('g, 'lrc) kernel = {
  (* The lrc index represent the set of stacks ending in this state. *)
  lrc: 'lrc index;

  (* The nonterminal labelling the goto transition to follow from these stacks,
     or none if the if the stacks should be considered directly. *)
  nto: 'g nonterminal opt index;

  (* Lookahead *)
  lookahead: 'g terminal indexset;
}

type ('lrc, 'n) edge = {
  path: 'lrc index list;
  source: 'n index;
  target: 'n index;
}

type ('g, 'lrc, 'n) _graph = {
  ker : ('n, ('g, 'lrc) kernel) vector;
  fwd : ('n, ('lrc, 'n) edge list) vector;
  bkd : ('n, ('lrc, 'n) edge list) vector;
  entries : 'n index list;
}

type ('g, 'lrc) graph = Graph : ('g, 'lrc, 'n) _graph -> ('g, 'lrc) graph

(* Staged and cached lazy computation for construction the graph of failure nodes:
   1. [let finder = free_failures grammar stacks rcs]
      lazily constructs the graph
   2. [finder lrcs nt depth] is the list of failure nodes reachable by following
      a goto transition labelled [nt] [depth] states deep in the stacks
      described by [lrcs].
*)

let rec fold_expand expand env f acc = function
  | [] -> acc
  | [x] -> f env acc x
  | x :: xs ->
    let acc = f env acc x in
    let env = expand env in
    fold_expand expand env f acc xs

let make_graph (type g lrc)
    (grammar : g grammar)
    (stacks : (g, lrc) Automata.stacks)
    (rcs : (g lr1, g Redgraph.reduction_closure) vector)
    (entries : (g, lrc) kernel list)
  =
  let open IndexBuffer in
  let module Nodes = Gen.Make() in
  let nodes = Nodes.get_generator () in
  let fwd = Dyn.make [] in
  let table = Hashtbl.create 500 in
  let rec synthesize node ker =
    let paths, tgt =
      let lr1 = stacks.label ker.lrc in
      match Opt.prj ker.nto with
      | None ->
        IndexSet.fold
          (fun lrc acc -> (lrc, [lrc]) :: acc)
          (stacks.prev ker.lrc) [],
        lr1
      | Some nt ->
        ([ker.lrc, []], Transition.find_goto_target grammar lr1 nt)
    in
    let rc = rcs.:(tgt) in
    let explore_paths paths acc nts =
      IndexMap.fold begin fun nt lookahead' acc ->
        let lookahead = IndexSet.inter ker.lookahead lookahead' in
        if IndexSet.is_not_empty lookahead then
          List.fold_left begin fun acc (lrc', path) ->
            let target = get_node {lrc = lrc'; nto = Opt.some nt; lookahead} in
            {source=node; target; path} :: acc
          end acc paths
        else
          acc
      end nts acc
    in
    let expand_paths paths =
      List.fold_left begin fun acc (lrc0, path) ->
        IndexSet.fold
          (fun lrc acc -> (lrc, lrc :: path) :: acc)
          (stacks.prev lrc0) acc
      end [] paths
    in
    Dyn.set fwd node (fold_expand expand_paths paths explore_paths [] rc.reductions)

  and get_node ker =
    assert (IndexSet.is_not_empty ker.lookahead);
    match Hashtbl.find_opt table ker with
    | Some node -> node
    | None ->
      let node = Gen.add nodes ker in
      Hashtbl.add table ker node;
      synthesize node ker;
      node
  in
  let entry_nodes = List.map (Gen.add nodes) entries in
  List.iter2 synthesize entry_nodes entries;
  let ker = Gen.freeze nodes in
  let fwd = Dyn.contents fwd Nodes.n in
  let bkd = Vector.make Nodes.n [] in
  Vector.iter (List.iter (fun edge -> bkd.@(edge.target) <- List.cons edge)) fwd;
  Graph {ker; fwd; bkd; entries = entry_nodes}

let get_lr1_state grammar (stacks : _ Automata.stacks) ker =
  let lr1 = stacks.label ker.lrc in
  match Opt.prj ker.nto with
  | None -> lr1
  | Some nt -> Transition.find_goto_target grammar lr1 nt

let get_lr0_state grammar (stacks : _ Automata.stacks) ker =
  Lr1.to_lr0 grammar (get_lr1_state grammar stacks ker)

(* Analysis of reachable lookaheads *)

type ('g, 'n) fallible_lookaheads = {
  fwd_fallible: ('n, 'g terminal indexset) vector;
  bkd_fallible: ('n, 'g terminal indexset) vector;
}

let get_failing grammar stacks rcs ker =
  rcs.:(get_lr1_state grammar stacks ker).Redgraph.failing

let fallible_lookaheads (type n) grammar stacks rcs (gr : (_, _, n) _graph) =
  let n = Vector.length gr.ker in
  let fwd = Vector.map (get_failing grammar stacks rcs) gr.ker in
  let bkd = Vector.copy fwd in
  let succ f nd = List.iter (fun edge -> f edge.target) gr.fwd.:(nd) in
  let scc = Tarjan.indexed_scc n ~succ in
  Tarjan.close_forward scc ~succ fwd;
  let pred f nd = List.iter (fun edge -> f edge.source) gr.bkd.:(nd) in
  Tarjan.close_backward scc ~pred fwd;
  {fwd_fallible = fwd; bkd_fallible = bkd}

let cover_with_maximal_patterns grammar stacks rcs gr =
  let results = ref [] in
  let todo = ref (List.map (fun node -> node, [], IndexSet.empty) gr.entries) in
  let marked = Boolvector.make (Vector.length gr.ker) false in
  let visited = Vector.make (Vector.length gr.ker) IndexSet.empty in
  let propagate (node, path, failing as acc) =
    let failing = IndexSet.union (get_failing grammar stacks rcs gr.ker.:(node)) failing in
    if not (Boolvector.test marked node) || not (IndexSet.equal visited.:(node) failing) then (
      Boolvector.set marked node;
      visited.@(node) <- IndexSet.union failing;
      match gr.fwd.:(node) with
      | [] -> push results acc
      | edges ->
        List.iter begin fun edge ->
          push todo (edge.target, edge :: path, failing)
        end edges
    )
  in
  fixpoint ~propagate todo;
  !results

let cover_remaining grammar stacks gr sentences {fwd_fallible; bkd_fallible} =
  let uncovered = Vector.make (Lr0.cardinal grammar) IndexSet.empty in
  (* Gather lookaheads to cover *)
  Vector.iteri begin fun i ker ->
    uncovered.@(get_lr0_state grammar stacks ker) <-
      IndexSet.union (IndexSet.union fwd_fallible.:(i) bkd_fallible.:(i))
  end gr.ker;
  (* Remove those already covered by sentences *)
  List.iter begin fun (node, path, failing) ->
    let remove node =
      let lr0 = get_lr0_state grammar stacks gr.ker.:(node) in
      uncovered.:(lr0) <- IndexSet.diff uncovered.:(lr0) failing
    in
    remove node;
    List.iter (fun edge -> remove edge.source) path
  end sentences;
  (* Cover remaining cases; use a DFS *)

(* Strategy for enumeration

   - Construct the graph using [get_node] to inject each entry point.
   - Use [cover_entries_with_maximal_patterns] to produce suffixes starting from
     all entrypoints simulateneously and covering all lookaheads.

   This is sufficient for maximal patterns, but if we want exhaustive coverage,
   we need a second pass:

   - We take the entry nodes and the suffixes produced so far
   - We visit all the LR(0) reachable from entry nodes to gather
     all the lookaheads with which they can be reached
   - We visit all suffixes to remove the lookaheads that are already covered
   - We do a BFS to collect suffixes reaching LR(0) states we still have to
     cover.
   - We do a BFS to collect prefixes reaching LR(0) states we still have to
     cover.
   - Then for each LR(0) state we still have to cover, pick enough prefixes and
     suffixes to cover everything, update remaining things to cover.


   For this we will construct do a BFS, reified as a tree, in which in each
   branch we commit to covering the yet uncovered lookaheads.
   When outputting a sentence, we drop the non-productive (not covering anything
   new) prefix, then we update all other branches of the BFS to drop the already
   committed lookaheads.  Woooo...

   Then for printing, we group sentences by their final lr0 state, which
   represent the right pattern.
*)

(* Group sentences by patterns

   let order (lr0, _) (lr0', _) =
    match Index.compare lr0 lr0' with
    | 0 ->
      (* Same lr0 *)
      0
    | c0 ->
      (* Not the same:
   - Order first by decreasing number of items
   - If same number of items, fall back to an (arbitrary) total order
           induced by LR(0) state number *)
      match Int.compare
              (IndexSet.cardinal (Lr0.items gr.grammar lr0))
              (IndexSet.cardinal (Lr0.items gr.grammar lr0'))
      with
      | 0 -> c0
      | c -> c
     in
     group_by !by_lr0 ~compare:order
     ~group:(fun (lr0, node) rest -> (lr0, node :: List.map snd rest))

  List.iter begin fun (lr0, nodes) ->
    let sentences = extract_suffixes nodes in
    let lhs =
      match Lr0.incoming gr.grammar lr0 with
      | Some sym when Symbol.is_nonterminal gr.grammar sym ->
        "[_ "
      | Some _ | None ->
        "["
    in
    let pad = String.make (String.length lhs) ' ' in
    let lines =
      let items = IndexSet.elements (Lr0.items gr.grammar lr0) in
      List.mapi (fun i item ->
          let filter = Item.to_string gr.grammar item in
          if i = 0 then
            lhs ^ "/" ^ filter
          else
            pad ^ "/" ^ filter
        ) items
    in
    let lines = String.concat "\n" lines ^ "]" in
    print_endline lines;
    List.iter begin fun (node, edges, handled, failing) ->
      let suffix = List.fold_left
          (fun path edge -> edge.target.ker.lrc :: List.rev_append edge.path path)
          [node.ker.lrc] edges
      in
      let base = List.hd suffix in
      let complete = List.rev_append (prefix base) suffix in
      let sentence = List.map gr.stacks.label complete in
      let sentence = List.map (Lr1.to_string gr.grammar) sentence in
      print_endline (String.concat " " sentence);
      print_endline ("  for unique lookaheads: " ^ Terminal.lookaheads_to_string gr.grammar handled);
      let failing = IndexSet.diff failing handled in
      if IndexSet.is_not_empty failing then
        print_endline ("  for redundant lookaheads: " ^ Terminal.lookaheads_to_string gr.grammar failing);
    end sentences;
  end by_lr0
*)
