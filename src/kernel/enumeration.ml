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

let kernel lrc ?goto lookahead =
  let nto = Opt.(Option.fold ~none ~some goto) in
  {lrc; nto; lookahead}

type ('lrc, 'n) edge = {
  path: 'lrc index list;
  source: 'n index;
  target: 'n index;
}

type ('g, 'lrc, 'e, 'n) _graph = {
  entries : 'e cardinal;
  ker : (('e, 'n) Sum.n, ('g, 'lrc) kernel) vector;
  fwd : (('e, 'n) Sum.n, ('lrc, ('e, 'n) Sum.n) edge list) vector;
  bkd : (('e, 'n) Sum.n, ('lrc, ('e, 'n) Sum.n) edge list) vector;
}

type ('g, 'lrc, 'entry) graph =
    Graph : ('g, 'lrc, 'entry, 'n) _graph -> ('g, 'lrc, 'entry) graph

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

let make_graph (type g lrc entry)
    (grammar : g grammar)
    (rcs : (g lr1, g Redgraph.reduction_closure) vector)
    (stacks : (g, lrc) Automata.stacks)
    (entries : (entry, (g, lrc) kernel) vector)
  =
  let open IndexBuffer in
  let module Nodes = Gensym() in
  let nodes = Dyn.make (Vector.as_array entries).(0) in
  let fwd = Dyn.make [] in
  let table = Hashtbl.create 500 in
  let entry_count = Vector.length entries in
  let rec synthesize (node : (entry, Nodes.n) Sum.n index) ker =
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
      let node = Sum.inj_r entry_count (Nodes.fresh ()) in
      Dyn.set nodes node ker;
      Hashtbl.add table ker node;
      synthesize node ker;
      node
  in
  Vector.iteri begin fun i ker ->
    let i = Sum.inj_l i in
    Dyn.set nodes i ker;
    synthesize i ker
  end entries;
  let total = Sum.cardinal entry_count Nodes.n in
  let ker = Dyn.contents nodes total in
  let fwd = Dyn.contents fwd total in
  let bkd = Vector.make total [] in
  Vector.iter (List.iter (fun edge -> bkd.@(edge.target) <- List.cons edge)) fwd;
  Graph {entries=entry_count; ker; fwd; bkd}

let get_lr1_state grammar (stacks : _ Automata.stacks) ker =
  let lr1 = stacks.label ker.lrc in
  match Opt.prj ker.nto with
  | None -> lr1
  | Some nt -> Transition.find_goto_target grammar lr1 nt

let get_lr0_state grammar (stacks : _ Automata.stacks) ker =
  Lr1.to_lr0 grammar (get_lr1_state grammar stacks ker)

(* Analysis of reachable lookaheads *)

let get_failing grammar stacks rcs ker =
  rcs.:(get_lr1_state grammar stacks ker).Redgraph.failing

type ('g, 'lrc, 'e, 'n) failing_sentence = {
  first: 'n index;
  edges: ('lrc, 'n) edge list;
  failing: 'g terminal indexset;
  entry: 'e index;
}

let make_failing_sentence gr (first, edges, failing) =
  let entry = List.fold_left (fun _ edge -> edge.source) first edges in
  match Sum.prj gr.entries entry with
  | L entry -> {first; edges; failing; entry}
  | R _ -> assert false

let cover_with_maximal_patterns grammar rcs stacks gr =
  let results = ref [] in
  let todo = ref (
      Index.init_seq gr.entries (fun node -> Sum.inj_l node, [], IndexSet.empty)
      |> List.of_seq
    )
  in
  let marked = Boolvector.make (Vector.length gr.ker) false in
  let visited = Vector.make (Vector.length gr.ker) IndexSet.empty in
  let propagate (node, path, failing) =
    let failing = IndexSet.union (get_failing grammar stacks rcs gr.ker.:(node)) failing in
    if not (Boolvector.test marked node) || not (IndexSet.equal visited.:(node) failing) then (
      Boolvector.set marked node;
      visited.@(node) <- IndexSet.union failing;
      match gr.fwd.:(node) with
      | [] ->
        if IndexSet.is_not_empty failing then
          push results (make_failing_sentence gr (node, path, failing))
      | edges ->
        List.iter begin fun edge ->
          push todo (edge.target, edge :: path, failing)
        end edges
    )
  in
  fixpoint ~propagate todo;
  !results

let cover_all (type n) grammar rcs stacks ?(already_covered=[]) ?(manually_covered=ignore) (gr : (_, _, _, n) _graph) =
  let n = Vector.length gr.ker in
  let fallible0 = Vector.make (Lr0.cardinal grammar) IndexSet.empty in
  let cover lr0 set = fallible0.@(lr0) <- IndexSet.union set in
  List.iter begin fun {first; edges; failing; _} ->
    let mark node = cover (get_lr0_state grammar stacks gr.ker.:(node)) failing in
    mark first;
    List.iter (fun edge -> mark edge.source) edges
  end already_covered;
  manually_covered cover;
  let visited = Boolvector.make n false in
  let fallible = Vector.make n IndexSet.empty in
  let prefixes = Vector.make n [] in
  let suffixes = Vector.make n [] in
  let shortest_prefix = Vector.make n [] in
  let shortest_suffix = Vector.make n [] in
  let todo = ref [] in
  let propagate (dir, node, path, failing) =
    let ker = gr.ker.:(node) in
    let failing = IndexSet.union failing (get_failing grammar stacks rcs ker) in
    begin match dir with
      | `Prefix ->
        if list_is_empty shortest_prefix.:(node) then
          shortest_prefix.:(node) <- path
      | `Suffix ->
        if list_is_empty shortest_suffix.:(node) then
          shortest_suffix.:(node) <- path
    end;
    let fallible' = IndexSet.union failing fallible.:(node) in
    if not (Boolvector.test visited node) || fallible' != fallible.:(node) then (
      Boolvector.set visited node;
      fallible.:(node) <- fallible';
      let lr0 = get_lr0_state grammar stacks ker in
      let fallible0' = IndexSet.diff failing fallible0.:(lr0) in
      (* Save path if it is the first to cover some lookahead *)
      if IndexSet.is_not_empty fallible0' then (
        fallible0.@(lr0) <- IndexSet.union fallible0';
        let sentences = match dir with
          | `Prefix -> prefixes
          | `Suffix -> suffixes
        in
        sentences.@(node) <- List.cons (path, failing);
      );
      (* Extend path with successors *)
      let succ f = match dir with
        | `Prefix -> List.iter (fun edge -> f edge edge.source) gr.bkd.:(node)
        | `Suffix -> List.iter (fun edge -> f edge edge.target) gr.fwd.:(node)
      in
      succ (fun edge node' -> push todo (dir, node', edge :: path, failing))
    );
  in
  Index.iter n begin fun node ->
    if list_is_empty gr.bkd.:(node) then
      propagate (`Suffix, node, [], IndexSet.empty)
    else if list_is_empty gr.fwd.:(node) then
      propagate (`Prefix, node, [], IndexSet.empty)
  end;
  fixpoint ~propagate todo;
  Index.init_seq n begin fun node () ->
    let output (prefix, pfail) (suffix, sfail) =
      let failing = IndexSet.union pfail sfail in
      let sentence = List.rev_append prefix suffix in
      let first = match sentence with
        | [] -> node
        | x :: _ -> x.target
      in
      (first, sentence, failing)
    in
    let sprefix = shortest_prefix.:(node) in
    let ssuffix = shortest_suffix.:(node) in
    let output_prefixes prefixes =
      List.to_seq prefixes
      |> Seq.map (fun prefix' -> output prefix' (ssuffix, IndexSet.empty))
    in
    let output_suffixes suffixes =
      List.to_seq suffixes
      |> Seq.map (fun suffix' -> output (sprefix, IndexSet.empty) suffix')
    in
    let output_prefixes_suffixes prefixes suffixes () =
      Seq.Cons (output_prefixes prefixes,
                fun () -> Seq.Cons (output_suffixes suffixes, Seq.empty))
    in
    match List.rev prefixes.:(node), suffixes.:(node) with
    | prefix0 :: prefixes, suffix0 :: suffixes ->
      Seq.Cons (seq_singleton (output prefix0 suffix0),
                output_prefixes_suffixes prefixes suffixes)
    | prefixes, suffixes ->
      output_prefixes_suffixes prefixes suffixes ()
  end
  |> Seq.concat
  |> Seq.concat
  |> Seq.filter (fun (node, edges, failing) ->
      let productive node =
        let lr0 = get_lr0_state grammar stacks gr.ker.:(node) in
        let fallible = fallible0.:(lr0) in
        let fallible' = IndexSet.diff fallible failing in
        if not (IndexSet.equal fallible fallible') then (
          fallible0.:(lr0) <- fallible';
          true
        ) else
          false
      in
      productive node || List.exists (fun edge -> productive edge.source) edges
    )
  |> Seq.map (make_failing_sentence gr)
  |> seq_memoize

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
