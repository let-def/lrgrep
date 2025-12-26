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
type ('g, 'lrc) node = {

  (* Characterizing the stacks analyzed at node *)

  (* The lrc index represent the set of stacks ending in this state. *)
  lrc: 'lrc index;

  (* The nonterminal labelling the goto transition to follow from these stacks,
     or none if the if the stacks should be considered directly. *)
  nto: 'g nonterminal opt index;

  (* Lookaheads failing immediately *)
  failing: 'g terminal indexset;

  (* forward transitions (a transition [nd, la] is in [fwd] if [nd] is reachable by
     reducing at least one stack from [lrc+nt], while looking ahead a symbol in [la]. *)
  mutable fwd: ('g, 'lrc) edge list;

  (* backward transition, the converse of the [fwd] relation *)
  mutable bkd: ('g, 'lrc) edge list;

  (* Flow analyses *)

  (* If [entry] is non-empty, then this node is an entrypoint,
     entered while looking ahead at any symbol is [entry].

     The goal is then to find stacks ending in this node that fails
     when looking at symbols in [entry]. *)
  mutable entry: 'g terminal indexset;

  (* Marker to remember if node is already scheduled for processing *)
  mutable scheduled: bool;

  (* Pass 1: forward analyses. *)

  (* Lookaheads for which there exists at least one stack suffix reducing to this node *)
  mutable reachable: 'g terminal indexset;

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
  lookahead: 'g terminal indexset;
}

type ('g, 'lrc) graph = {
  grammar : 'g grammar;
  stacks : ('g, 'lrc) Automata.stacks;
  rcs : ('g lr1, 'g Redgraph.reduction_closure) vector;
  table: ('lrc, ('g nonterminal opt, ('g, 'lrc) node) indexmap) vector;
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
  let table = Vector.make stacks.domain IndexMap.empty in
  {table; grammar; stacks; rcs; frozen = false; todo = []}

let rec get_node gr lrc nto =
  let map = gr.table.:(lrc) in
  match IndexMap.find_opt nto map with
  | Some node -> node
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
    let node = {
      lrc; nto; failing = rc.failing;
      fwd = []; bkd = [];
      scheduled = false;
      entry = IndexSet.empty;
      reachable = IndexSet.empty;
      suffix_fallible = IndexSet.empty;
      prefix_faillible = IndexSet.empty;
    } in
    gr.table.:(lrc) <- IndexMap.add nto node map;
    let explore_paths paths acc nts =
      IndexMap.fold begin fun nt lookahead acc ->
        List.fold_left begin fun acc (lrc', path) ->
          let target = get_node gr lrc' (Opt.some nt) in
          let edge = {source=node; target; path; lookahead} in
          target.bkd <- edge :: target.bkd;
          edge :: acc
        end acc paths
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

let iter_nodes gr f =
  Vector.iter (fun map -> IndexMap.iter (fun _ v -> f v) map) gr.table

(* Analyses *)

let mark_entry gr node reachable =
  assert (not gr.frozen);
  if IndexSet.is_not_empty reachable then (
    node.entry <- IndexSet.union reachable node.entry;
    node.reachable <- node.entry;
    if not node.scheduled then (
      gr.todo <- node :: gr.todo;
      node.scheduled <- true;
    )
  )

let solve gr =
  assert (not gr.frozen);
  gr.frozen <- true;
  (* Forward pass 1: lookahead reachability *)
  let todo = ref gr.todo in
  gr.todo <- [];
  let schedule nd =
    if not nd.scheduled then (
      nd.scheduled <- true;
      push todo nd;
    )
  in
  let propagate_reachable nd =
    nd.scheduled <- false;
    List.iter (fun edge ->
        let reachable =
          IndexSet.fused_inter_union
            nd.reachable edge.lookahead
            ~acc:edge.target.reachable
        in
        if edge.target.reachable != reachable then (
          edge.target.reachable <- reachable;
          schedule edge.target;
        )
      ) nd.fwd
  in
  fixpoint ~propagate:propagate_reachable todo;
  (* Forward pass 2: lookaheads fallible in suffix *)
  iter_nodes gr begin fun nd ->
    nd.suffix_fallible <- IndexSet.inter nd.failing nd.reachable;
    if IndexSet.is_not_empty nd.suffix_fallible then
      schedule nd
  end;
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
  iter_nodes gr begin fun nd ->
    nd.prefix_faillible <- IndexSet.inter nd.failing nd.reachable;
    if IndexSet.is_not_empty nd.prefix_faillible then
      schedule nd
  end;
  let propagate_prefix_fallible nd =
    nd.scheduled <- false;
    List.iter (fun edge ->
        let fallible = IndexSet.inter nd.prefix_faillible edge.lookahead in
        let fallible =
          IndexSet.fused_inter_union
            fallible edge.source.reachable
            ~acc:edge.source.prefix_faillible
        in
        if fallible != edge.source.prefix_faillible then (
          edge.source.prefix_faillible <- fallible;
          schedule edge.source;
        )
      ) nd.bkd
  in
  fixpoint ~propagate:propagate_prefix_fallible todo

let extract_suffixes nodes =
  let uncovered = ref IndexSet.empty in
  let prepare nd =
    uncovered := IndexSet.union nd.suffix_fallible !uncovered;
    (nd, [], IndexSet.empty)
  in
  let todo = ref (List.map prepare nodes) in
  let suffixes = ref [] in
  let propagate (nd, path, failing) =
    let failing = IndexSet.inter (IndexSet.union nd.failing failing) nd.reachable in
    let u =
      let u = !uncovered in
      let handled = IndexSet.inter (IndexSet.inter nd.entry u) failing in
      if IndexSet.is_not_empty handled then (
        push suffixes (nd, path, handled, failing);
        uncovered := IndexSet.diff u handled;
        !uncovered
      ) else u
    in
    if not (IndexSet.disjoint nd.suffix_fallible u) || not (IndexSet.disjoint failing u) then
      List.iter begin fun edge ->
          push todo (edge.source, edge :: path, failing)
      end nd.bkd;
  in
  fixpoint ~propagate todo;
  !suffixes

let maximal_patterns gr prefix =
  let get_top_state node =
    let lr1 = gr.stacks.label node.lrc in
    match Opt.prj node.nto with
    | None -> lr1
    | Some nt -> Transition.find_goto_target gr.grammar lr1 nt
  in
  let by_lr0 = ref [] in
  iter_nodes gr begin fun node ->
    if List.is_empty node.fwd then begin
      let lr0 = Lr1.to_lr0 gr.grammar (get_top_state node) in
      push by_lr0 (lr0, node);
    end
  end;
  let by_lr0 =
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
    let group (lr0, node) rest = (lr0, node :: List.map snd rest) in
    group_by ~compare:order ~group !by_lr0
  in
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
          (fun path edge -> edge.target.lrc :: List.rev_append edge.path path)
          [node.lrc] edges
      in
      let base = List.hd suffix in
      let complete = List.rev_append (prefix base) suffix in
      let sentence = List.map gr.stacks.label complete in
      let sentence = List.map (Lr1.to_string gr.grammar) sentence in
      print_endline (String.concat " " sentence);
      print_endline ("  for unique lookaheads: " ^ Terminal.lookaheads_to_string gr.grammar handled);
      let failing = IndexSet.diff failing handled in
      if IndexSet.is_not_empty failing then
        print_endline ("  for unique lookaheads: " ^ Terminal.lookaheads_to_string gr.grammar failing);
    end sentences;
  end by_lr0
