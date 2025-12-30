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

type ('g, 'lrc) node = {
  ker: ('g, 'lrc) kernel;
  failing: 'g terminal indexset;

  (* forward transitions (a transition [nd, la] is in [fwd] if [nd] is reachable by
     reducing at least one stack from [lrc+nt], while looking ahead a symbol in [la]. *)
  mutable fwd: ('g, 'lrc) edge list;

  (* backward transition, the converse of the [fwd] relation *)
  mutable bkd: ('g, 'lrc) edge list;

  (* Flow analyses *)

  (* Markers to remember node state in graph traversals *)
  mutable scheduled: bool;
  mutable visited: 'g terminal indexset;

  (* Pass 1: forward analyses. *)

  (* Lookaheads that are rejected in at least one stack suffix reducing to this
     node. These lookaheads are reachable and failing in at least one (possibly
     indirect) predecessor. *)
  mutable suffix_fallible: 'g terminal indexset;

  (* Pass 2: backward analysis. *)

  (* Lookaheads for which there exists a prefix, reached by following a forward
     edge, causing a stack to be rejected. These are reachable from this
     node.  *)
  mutable prefix_faillible: 'g terminal indexset;
}

and ('g, 'lrc) edge = {
  path: 'lrc index list;
  source: ('g, 'lrc) node;
  target: ('g, 'lrc) node;
}

type ('g, 'lrc) graph = {
  grammar : 'g grammar;
  stacks : ('g, 'lrc) Automata.stacks;
  rcs : ('g lr1, 'g Redgraph.reduction_closure) vector;
  table: (('g, 'lrc) kernel, ('g, 'lrc) node) Hashtbl.t;
  mutable frozen : bool;
  mutable todo: ('g, 'lrc) node list;
}

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
  =
  let table = Hashtbl.create 500 in
  {table; grammar; stacks; rcs; frozen = false; todo = []}

let rec get_node gr lrc nto suffix_fallible lookahead =
  assert (not gr.frozen);
  assert (IndexSet.is_not_empty lookahead);
  let ker = {lrc; nto; lookahead} in
  match Hashtbl.find_opt gr.table ker with
  | Some node ->
    let fallible = IndexSet.union suffix_fallible node.suffix_fallible in
    if fallible != node.suffix_fallible then (
      node.suffix_fallible <- fallible;
      if not node.scheduled then (
        gr.todo <- node :: gr.todo;
        node.scheduled <- true;
      );
    );
    node
  | None ->
    let paths, tgt =
      let lr1 = gr.stacks.label lrc in
      match Opt.prj nto with
      | None ->
        IndexSet.fold
          (fun lrc acc -> (lrc, [lrc]) :: acc)
          (gr.stacks.prev lrc) [],
        lr1
      | Some nt ->
        ([lrc, []], Transition.find_goto_target gr.grammar lr1 nt)
    in
    let rc = gr.rcs.:(tgt) in
    let failing = IndexSet.inter rc.failing lookahead in
    let suffix_fallible = IndexSet.union suffix_fallible failing in
    let node = {
      ker; failing;
      fwd = []; bkd = [];
      scheduled = false;
      visited = IndexSet.empty;
      suffix_fallible;
      prefix_faillible = rc.failing;
    } in
    Hashtbl.add gr.table ker node;
    let explore_paths paths acc nts =
      IndexMap.fold begin fun nt lookahead' acc ->
        let e_lookahead = IndexSet.inter lookahead lookahead' in
        if IndexSet.is_not_empty e_lookahead then
          List.fold_left begin fun acc (lrc', path) ->
            let target = get_node gr lrc' (Opt.some nt) suffix_fallible e_lookahead in
            let edge = {source=node; target; path} in
            target.bkd <- edge :: target.bkd;
            edge :: acc
          end acc paths
        else
          acc
      end nts acc
    in
    let expand_paths paths =
      List.fold_left (fun acc (lrc0, path) ->
          IndexSet.fold (fun lrc acc -> (lrc, lrc :: path) :: acc)
            (gr.stacks.prev lrc0) acc
        ) [] paths
    in
    node.fwd <- fold_expand expand_paths paths explore_paths [] rc.reductions;
    node

let get_node gr lrc nto lookahead =
  get_node gr lrc nto IndexSet.empty lookahead

let iter_nodes gr f =
  Hashtbl.iter (fun _ v -> f v) gr.table

let fold_nodes gr f acc =
  Hashtbl.fold (fun _ v acc -> f v acc) gr.table acc

(* Analyses *)

let get_lr0_state gr node =
  let lr1 = gr.stacks.label node.ker.lrc in
  Lr1.to_lr0 gr.grammar @@
  match Opt.prj node.ker.nto with
  | None -> lr1
  | Some nt -> Transition.find_goto_target gr.grammar lr1 nt

let solve gr =
  assert (not gr.frozen);
  gr.frozen <- true;
  let todo = ref gr.todo in
  gr.todo <- [];
  let schedule nd =
    if not nd.scheduled then (
      nd.scheduled <- true;
      push todo nd
    )
  in
  (* Forward pass: lookaheads fallible in suffix *)
  let propagate_suffix_fallible nd =
    nd.scheduled <- false;
    List.iter begin fun edge ->
      let fallible = IndexSet.union nd.suffix_fallible edge.target.suffix_fallible in
      if edge.target.suffix_fallible != fallible then (
        edge.target.suffix_fallible <- fallible;
        schedule edge.target;
      )
    end nd.fwd
  in
  fixpoint ~propagate:propagate_suffix_fallible todo;
  (* Backward pass: lookaheads fallible in prefix *)
  let propagate_prefix_fallible nd =
    nd.scheduled <- false;
    List.iter begin fun edge ->
      let fallible =
        IndexSet.union nd.prefix_faillible edge.source.prefix_faillible
      in
      if fallible != edge.source.prefix_faillible then (
        edge.source.prefix_faillible <- fallible;
        schedule edge.source;
      )
    end nd.bkd
  in
  iter_nodes gr propagate_prefix_fallible;
  fixpoint ~propagate:propagate_prefix_fallible todo

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
*)

(*
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

let rec clear_marks node =
  if node.scheduled then (
    node.scheduled <- false;
    node.visited <- IndexSet.empty;
    List.iter (fun edge -> clear_marks edge.target) node.fwd
  )

let cover_entries_with_maximal_patterns nodes =
  let results = ref [] in
  let todo = ref (List.map (fun node -> node, [], IndexSet.empty) nodes) in
  let propagate (node, path, failing as acc) =
    let failing = IndexSet.union node.failing failing in
    if not node.scheduled || not (IndexSet.equal node.visited failing) then (
      node.scheduled <- true;
      node.visited <- IndexSet.union node.visited failing;
      match node.fwd with
      | [] -> push results acc
      | edges ->
        List.iter begin fun edge ->
          push todo (edge.target, edge :: path, failing)
        end edges
    )
  in
  fixpoint ~propagate todo;
  List.iter clear_marks nodes;
  !results

let cover_remaining gr nodes sentences =
  let uncovered = Vector.make (Lr0.cardinal gr.grammar) IndexSet.empty in
  (* Gather lookaheads to cover *)
  let rec gather node =
    if not node.scheduled then (
      node.scheduled <- true;
      uncovered.@(get_lr0_state gr node) <-
        IndexSet.union (IndexSet.union node.prefix_faillible node.suffix_fallible);
      List.iter (fun edge -> gather edge.target) node.fwd
    )
  in
  List.iter gather nodes;
  List.iter clear_marks nodes;
  (* Remove those already covered by sentences *)
  List.iter begin fun (node, path, failing) ->
    let remove node =
      let lr0 = get_lr0_state gr node in
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

type ('g, 'lrc) bfs_node =
  | Root of ('g, 'lrc) node
  | Edge of {
      edge: ('g, 'lrc) edge;
      parent: ('g, 'lrc) bfs_node;
      mutable mark: int;
      mutable committed: 'g terminal indexset;
    }
