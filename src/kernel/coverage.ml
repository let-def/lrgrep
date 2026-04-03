(* MIT License
 *
 * Copyright (c) 2026 Frédéric Bour
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *)

(** This module implements coverability analysis of the LR automaton and a
    corresponding matching machine to detect uncovered failing configurations.

    Design overview:

    - Andor module: builds an AND-OR graph where OR-nodes represent
      non-deterministic configurations (multiple possible reductions) and
      AND-nodes represent deterministic consumption of LR state (popping and
      branching from the top of the LR stack).

    - Deter module: constructs a deterministic automaton from the AND-OR graph
      by merging all OR-nodes and grouping AND-nodes branching on the same LR
      state. In other words, an edge reaching an OR-node is an ϵ-transition and
      an edge reaching an AND-node is labelled by an LR(1) state.

    - Enum module: Augment the deterministic graph by computing the lookahead
      symbols not yet accepted when reaching a node. The unaccepted symbols of
      the sinks (nodes without successor) describe all the possible failures,
      which we can illustrate with counter-examples by following the
      predecessors back to the initial node.

    - Cover module: Determine the coverage of an error matching machine by
      computing a synchronized product of the machine with the enum graph.

    - Extract module: extracts counter-examples for fallible reductions from the
      enumeration and coverage graphs.

    - Report module: formats and presents enumeration and coverage results to
      users.

    Implementation details:

    To compute coverage, one has to track many information at each step:
    possible stack prefixes (current LR states), on-going reductions, unaccepted
    lookahead symbols.  Doing so all at once is expensive (the state space is
    huge), and unnecessary, as relevant information can be recovered later.

    The current implementation is the result of dozens of experimentations to
    find a balance between efficiency, precision and ease of use.

    The Andor module is non-deterministic in reductions: a node tracks a single
    reduction, together with the precise LR state and set of lookahead symbols.

    The Deter module determinizes Andor but ignores the set of lookahead symbols.
    (That is, the lookahead symbols of the different Andor nodes in the kernel
    of a Deter node are unrelated; one might accept a symbol, another reject it,
    and a third one just asks for more reduction to decide what to do with
    lookahead).
    Determinizing with respect to lookahead would cause a combinatorial
    explosion without providing more actionable information.

    The Enum module refines Deter with the unaccepted lookaheads symbols of
    each node. Since lookaheads are not part of Deter kernel, this is a
    path-dependent, so the shortest paths witnessing that a given lookahead is
    unaccepted are remembered.
    What matters is that there is at least one way to reach a given node for a
    given lookahead.
    Since all reductions applicable to a given configuration are tracked
    simultaneously, this approach also work with GLR automata: we know that
    lookahead has not been accepted by any of the possible reductions
*)

open Utils
open Misc
open Fix.Indexing
open Info

let string_of_items_for_filter g lr0 =
  let decompose item =
    let prod, pos = Item.desc g item in
    let rhs = Production.rhs g prod in
    (Production.lhs g prod,
     Array.sub rhs 0 pos,
     Array.sub rhs pos (Array.length rhs - pos))
  in
  let lines = ref [] in
  let append (lhs, pre, post) =
    match pre with
    (* Optimization 1: skip items of the form symbol: symbol . ... *)
    | [|first|] when Index.equal (Symbol.inj_n g lhs) first -> ()
    | _ ->
      (* Optimization 2: group items of the form
         sym: α . x . β₁, sym: α . x.β₂, ...
         as sym: α . x _* *)
      match !lines with
      | (lhs', pre', post') :: rest
        when Index.equal lhs lhs' && array_equal Index.equal pre pre' ->
        begin match post', post with
          | `Suffix [||], _ | _, [||] ->
            push lines (lhs, pre, `Suffix post)
          | `Suffix post', post when Index.equal post'.(0) post.(0) ->
            lines := (lhs', pre', `Wild post.(0)) :: rest
          | `Wild post0, post when Index.equal post0 post.(0) ->
            ()
          | _ ->
            push lines (lhs, pre, `Suffix post)
        end
      | _ -> push lines (lhs, pre, `Suffix post)
  in
  IndexSet.rev_map_elements (Lr0.items g lr0) decompose
  |> List.sort compare
  |> List.iter append;
  let print_item (lhs, pre, post) =
    let syms syms = Array.to_list (Array.map (Symbol.to_string g) syms) in
    String.concat " " @@
    (Nonterminal.to_string g lhs ^ ":")
    :: syms pre
    @ "." :: match post with
    | `Suffix post -> syms post
    | `Wild sym -> [Symbol.to_string g sym; "_*"]
  in
  List.rev_map print_item !lines

let print_pattern g lr0 =
  let first, other, suffix =
    match Lr0.incoming g lr0 with
    | Some sym when Symbol.is_nonterminal g sym ->
      "| [_* /",
      "      /",
      "]"
    | Some _ | None ->
      "| /",
      "  /",
      ""
  in
  let rec prepare pad = function
    | [] -> assert false
    | [x] -> [pad ^ x ^ suffix]
    | x :: xs -> (pad ^ x) :: prepare other xs
  in
  prepare first (string_of_items_for_filter g lr0)

module Andor = struct
  type ('g, 'lrc, 'n) node = {
    lrc: 'lrc index;
    rpos: 'g Redpos.t Opt.n index;
    active: 'g terminal indexset;
    mutable successors: ('g terminal indexset * 'n index) array;
  }

  type ('g, 'lrc, 'n) _graph = {
    initials: ('lrc, 'g terminal indexset * 'n index) indexmap;
    nodes: ('n, ('g, 'lrc, 'n) node) vector;
  }

  type ('g, 'lrc) graph = Graph : ('g, 'lrc, 'n) _graph -> ('g, 'lrc) graph

  let is_or_node rtable node = match Opt.prj node.rpos with
    | None -> true
    | Some rpos -> Redpos.is_zero rtable rpos

  let top_state g (stacks : _ Automata.stacks) rtable node =
    match Opt.prj node.rpos with
    | None -> stacks.label node.lrc
    | Some rpos ->
      match Redpos.previous rtable rpos with
      | Either.Right _ -> assert false
      | Either.Left nt ->
        Transition.find_goto_target g (stacks.label node.lrc) nt

  let make (type g lrc)
      (g : g grammar)
      (rcs : (g lr1, g Redgraph.reduction_closure) vector)
      (stacks : (g, lrc) Automata.stacks)
      (rtable : g Redpos.table)
    =
    let open IndexBuffer in
    let module Map = Map.Make(struct
      type t = g Redpos.t Opt.n index * g terminal indexset
      let compare (i1,s1) (i2,s2) =
        let c = Index.compare i1 i2 in
        if c <> 0 then c else
          IndexSet.compare s1 s2
    end) in
    let module Nodes = Gen.Make() in
    let nodes = Nodes.get_generator () in
    let table = Vector.make stacks.domain Map.empty in
    let todo = ref [] in
    let get_state lrc rpos active =
      let map = table.:(lrc) in
      let accept =
        match Opt.prj rpos with
        | None -> rcs.:(stacks.label lrc).accepting
        | Some rpos ->
          match Redpos.previous rtable rpos with
          | Either.Right _ -> IndexSet.empty
          | Either.Left nt ->
            rcs.:(Transition.find_goto_target g (stacks.label lrc) nt).accepting
      in
      let accept = IndexSet.inter active accept in
      let active = IndexSet.diff active accept in
      let key = (rpos, active) in
      match Map.find_opt key map with
      | Some ix -> (accept, ix)
      | None ->
        let ix = Gen.add nodes {lrc; rpos; active; successors = [||]} in
        table.:(lrc) <- Map.add key ix map;
        push todo ix;
        (accept, ix)
    in
    let initials =
      let regular = Terminal.regular g in
      IndexMap.inflate (fun lrc -> get_state lrc Opt.none regular) stacks.tops
    in
    begin
      let propagations = ref 0 in
      let propagate source =
        incr propagations;
        let snode = Gen.get nodes source in
        assert (Array.length snode.successors = 0);
        let lrc = snode.lrc in
        match Opt.prj snode.rpos with
        | None ->
          let edges = ref [] in
          List.iteri begin fun i nts ->
            edges := IndexMap.fold begin fun nt label acc ->
                let rpos = Opt.some (Redpos.inj rtable nt (i + 1)) in
                get_state lrc rpos label :: acc
              end nts !edges
          end rcs.:(stacks.label lrc).all_reductions;
          snode.successors <- Array.of_list !edges
        | Some rpos ->
          begin match Redpos.previous rtable rpos with
            | Either.Right rpos' ->
              snode.successors <-
                IndexSet.map_to_array (stacks.prev lrc)
                  (fun lrc -> get_state lrc (Opt.some rpos') snode.active)
            | Either.Left nt ->
              let lr1 = stacks.label lrc in
              let goto = Transition.find_goto_target g lr1 nt in
              let edges = ref [] in
              List.iteri begin fun i nts ->
                edges := IndexMap.fold begin fun nt label acc ->
                    let label = IndexSet.inter snode.active label in
                    if IndexSet.is_not_empty label then
                      get_state lrc (Opt.some (Redpos.inj rtable nt i)) label :: acc
                    else
                      acc
                  end nts !edges
              end rcs.:(goto).all_reductions;
              snode.successors <- Array.of_list !edges
          end
      in
      let counter = ref 0 in
      fixpoint ~counter ~propagate todo;
      stopwatch 1 "Andor construction: %d iterations, %d propagations, %d nodes"
        !counter !propagations (cardinal Nodes.n);
    end;
    let nodes = Gen.freeze nodes in
    Graph {initials; nodes}
end

module Deter = struct
  type ('g, 'n, 'm) node = {
    index: 'm index;
    ker: 'n indexset;
    mutable top: 'g lr1 indexset;
    mutable accept: 'g terminal indexset;
    mutable successors: ('g terminal indexset * ('g, 'n, 'm) node) array;
  }

  type ('g, 'lrc, 'n, 'm) _graph = {
    initials: ('lrc, 'g terminal indexset * 'm index) indexmap;
    nodes: ('m, ('g, 'n, 'm) node) vector;
  }

  type ('g, 'lrc, 'n) graph = Graph : ('g, 'lrc, 'n, 'm) _graph -> ('g, 'lrc, 'n) graph

  let get_lrc agr node =
    agr.Andor.nodes.:(IndexSet.choose node.ker).lrc

  let make (type g lrc n)
      (g : g grammar)
      (_rcs : (g lr1, g Redgraph.reduction_closure) vector)
      (stacks : (g, lrc) Automata.stacks)
      (rtable : g Redpos.table)
      (gr : (g, lrc, n) Andor._graph)
    : (g, lrc, n) graph
    =
    let open IndexBuffer in
    let module Nodes = Gen.Make() in
    let nodes = Nodes.get_generator () in
    let table = Vector.make stacks.domain IndexSet.Map.empty in
    let todo = ref [] in
    let get_state ker =
      let n = IndexSet.choose ker in
      let lrc = gr.nodes.:(n).lrc in
      assert (IndexSet.for_all (fun n' -> Index.equal lrc gr.nodes.:(n').lrc) ker);
      let map = table.:(lrc) in
      match IndexSet.Map.find_opt ker map with
      | Some ix -> ix
      | None ->
        let r = Gen.reserve nodes in
        let index = Gen.index r in
        let node = {index; ker; successors = [||]; top = IndexSet.empty; accept = IndexSet.empty} in
        Gen.commit nodes r node;
        table.:(lrc) <- IndexSet.Map.add ker node map;
        push todo node;
        node
    in
    let initials =
      IndexMap.map (fun (accept, ix) -> (accept, (get_state (IndexSet.singleton ix)).index)) gr.initials
    in
    let propagate node =
      let top = ref IndexSet.empty in
      let accept0 = ref IndexSet.empty in
      let rec accumulate (accept, ker) acc =
        let node = gr.nodes.:(ker) in
        if Andor.is_or_node rtable node then (
          top := IndexSet.add (Andor.top_state g stacks rtable node) !top;
          accept0 := IndexSet.union accept !accept0;
          Array.fold_right accumulate node.successors acc
        ) else match acc with
          | None ->
            let accepts =
              Array.map (fun (accept', _) -> IndexSet.union accept accept') node.successors
            in
            let succs = Array.map (fun (_, succ) -> IndexSet.singleton succ) node.successors in
            Some (accepts, succs)
          | Some (accepts, succs) ->
            Array.iteri (fun i (accept', succ) ->
                accepts.(i) <- IndexSet.union (IndexSet.union accept accept') accepts.(i);
                succs.(i) <- IndexSet.add succ succs.(i)
              ) node.successors;
            acc
      in
      let accumulation =
        IndexSet.fold_right
          (fun acc x -> accumulate (IndexSet.empty, x) acc)
          None node.ker
      in
      node.top <- !top;
      node.accept <- !accept0;
      match accumulation with
      | None -> ()
      | Some (accepts, succs) ->
        node.successors <- Array.map2 (fun a b -> (a, get_state b)) accepts succs
    in
    let counter = ref 0 in
    fixpoint ~counter ~propagate todo;
    stopwatch 1 "Deter construction: %d iterations, %d initials, %d nodes"
      !counter (IndexMap.cardinal initials) (cardinal Nodes.n);
    let nodes = Gen.freeze nodes in
    Graph {initials; nodes}

  let get_lr0
      (type g lrc n m)
      (g : g grammar)
      (rcs : (g lr1, g Redgraph.reduction_closure) vector)
      (stacks : (g, lrc) Automata.stacks)
      (rtable : g Redpos.table)
      (agr : (g, lrc, n) Andor._graph)
      (dgr : (g, lrc, n, m) _graph)
      ix
    =
    let states = ref IndexSet.empty in
    let rec visit_stacks stack tree =
      match tree.Redgraph.next with
      | [] -> states := IndexSet.add (Lr1.to_lr0 g (List.hd stack)) !states
      | xs -> List.iter (fun (stack, _, tree) -> visit_stacks stack tree) xs
    in
    let rec visit_leaves (_, ker) =
      let node = agr.nodes.:(ker) in
      let succ = node.successors in
      if Array.exists (fun (_, ker) -> Andor.is_or_node rtable agr.nodes.:(ker)) succ then
        Array.iter visit_leaves succ
      else if Andor.is_or_node rtable node then
        let lr1 = Andor.top_state g stacks rtable node in
        visit_stacks [lr1] rcs.:(lr1).stacks
    in
    IndexSet.iter begin fun ker ->
      if Andor.is_or_node rtable agr.nodes.:(ker) then
        visit_leaves (IndexSet.empty, ker)
    end dgr.nodes.:(ix).ker;
    !states
end

let dyn_array () =
  let r = ref (Array.make 16 []) in
  let set index cell =
    if Array.length !r <= index then (
      let length = ref (Array.length !r) in
      while !length <= index do
        length := !length * 2;
      done;
      let maximals' = Array.make !length [] in
      Array.blit !r 0 maximals' 0 (Array.length !r);
      r := maximals'
    );
    (!r).(index) <- cell :: (!r).(index)
  in
  (r, set)

module Enum = struct
  type ('n,'term) _graph = {
    domain: 'n cardinal;
    predecessors: 'n index -> ('n index * int * 'term indexset) list;
    unaccepted: 'n index -> 'term indexset;
  }

  let prepare (type g lrc n m)
      (g : g grammar)
      (agr : (g, lrc, n) Andor._graph)
      (dgr : (g, lrc, n, m) Deter._graph)
      (some_prefix : lrc index -> int * lrc index list)
    : (m, g terminal) _graph *
      (m index list * g terminal indexset) list array
    =
    let unaccepted = Vector.make_associate dgr.nodes IndexSet.empty in
    let predecessors = Vector.make_associate dgr.nodes [] in
    let counter = ref 0 in
    let todo = ref [] in
    let delta = Vector.make_associate dgr.nodes IndexSet.empty in
    let update source target set =
      let set = IndexSet.diff set dgr.nodes.:(target).accept in
      let set = IndexSet.diff set unaccepted.:(target) in
      if IndexSet.is_not_empty set then (
        unaccepted.@(target) <- IndexSet.union set;
        if IndexSet.is_empty delta.:(target) then
          push todo target;
        delta.@(target) <- IndexSet.union set;
        match source with
        | None -> ()
        | Some source ->
          predecessors.@(target) <- List.cons (source, !counter, set)
      )
    in
    let propagate ix =
      let todo = delta.:(ix) in
      delta.:(ix) <- IndexSet.empty;
      let source = Some ix in
      Array.iter begin fun (accept, target) ->
        let todo = IndexSet.diff todo accept in
        if IndexSet.is_not_empty todo then
          update source target.Deter.index todo
      end dgr.nodes.:(ix).successors
    in
    let regular = Terminal.regular g in
    IndexMap.iter begin fun _ (accept, target) ->
      update None target (IndexSet.diff regular accept)
    end dgr.initials;
    fixpoint ~counter ~propagate todo;
    let pfx_min = ref max_int and pfx_max = ref 0 in
    Vector.iter begin fun node ->
      if Array.length node.Deter.successors = 0 then (
        let length, _ = some_prefix (Deter.get_lrc agr node) in
        pfx_min := Int.min !pfx_min length;
        pfx_max := Int.max !pfx_max length;
      )
    end dgr.nodes;
    let maximals = Array.make (if !pfx_min = max_int then 0 else !pfx_max - !pfx_min + 1) [] in
    Vector.iteri begin fun ix node ->
      if Array.length node.Deter.successors = 0 then (
        let length, _ = some_prefix (Deter.get_lrc agr node) in
        let index = length - !pfx_min in
        let unaccepted = unaccepted.:(ix) in
        maximals.(index) <- ([ix], unaccepted) :: maximals.(index)
      )
    end dgr.nodes;
    let predecessors = Vector.get predecessors in
    let unaccepted = Vector.get unaccepted in
    ({domain = Vector.length dgr.nodes; predecessors; unaccepted}, maximals)
end

module Cover = struct
  type ('g, 'lrc, 'enu) graph = Graph : {
      enum: ('n, 'g terminal) Enum._graph;
      position: 'n index -> ('lrc, 'enu) Sum.n index;
      unaccepted: 'n index -> 'g terminal indexset;
      sinks: 'n index list;
    } -> ('g, 'lrc, 'enu) graph

  let coverage (type g r st tr lrc ao en)
      (g : g grammar)
      (branches : (g, r) Spec.branches)
      (machine : (g, r, st, tr) Automata.Machine.t)
      (stacks : (g, lrc) Automata.stacks)
      (agr : (g, lrc, ao) Andor._graph)
      (dgr : (g, lrc, ao, en) Deter._graph)
    =
    let open IndexBuffer in
    let module Ker = Gen.Make() in
    let ker = Ker.get_generator () in
    let unaccepted = Dyn.make IndexSet.empty in
    let delta = Dyn.make IndexSet.empty in
    let predecessors = Dyn.make [] in
    let successors = Dyn.make [||] in
    let table = Vector.make (Sum.cardinal stacks.domain (Vector.length dgr.nodes)) IndexMap.empty in
    let maco = Opt.cardinal (Vector.length machine.outgoing) in
    let get mac pos =
      let map = table.:(pos) in
      match IndexMap.find_opt mac map with
      | Some index -> index
      | None ->
        let index = Gen.add ker (Prod.inj maco mac pos) in
        table.:(pos) <- IndexMap.add mac index map;
        index
    in
    let find_target trs lrc =
      let lr1 = stacks.label lrc in
      let check_tr tr =
        if IndexSet.mem lr1 machine.label.:(tr).filter
        then Some tr
        else None
      in
      match IndexSet.find_map check_tr trs with
      | None -> Opt.none
      | Some tr -> Opt.some machine.target.:(tr)
    in
    let uncovered = ref [] in
    let initialize_lrc node trs lrc =
      let lrcs = stacks.prev lrc in
      if IndexSet.is_empty trs || IndexSet.is_empty lrcs then
        push uncovered node
      else
        Dyn.set successors node @@
        IndexSet.map_to_array lrcs
          (fun lrc -> IndexSet.empty, get (find_target trs lrc) (Sum.inj_l lrc))
    in
    let initialize node =
      let mac, pos = Prod.prj maco (Gen.get ker node) in
      let trs = match Opt.prj mac with
        | None -> IndexSet.empty
        | Some mac -> machine.outgoing.:(mac)
      in
      match Sum.prj stacks.domain pos with
      | L lrc -> initialize_lrc node trs lrc
      | R enu ->
        let enu = dgr.nodes.:(enu) in
        match enu.successors with
        | [||] ->
          initialize_lrc node trs (Deter.get_lrc agr enu)
        | succ ->
          Dyn.set successors node @@
          Array.map begin fun (accept, enu') ->
            let mac = find_target trs (Deter.get_lrc agr enu') in
            let pos = Sum.inj_r stacks.domain enu'.index in
            (accept, get mac pos)
          end succ
    in
    let todo = ref [] in
    let update target set =
      let mac, pos = Prod.prj maco (Gen.get ker target) in
      let set =
        match Sum.prj stacks.domain pos with
        | L _ -> set
        | R en -> IndexSet.diff set dgr.nodes.:(en).accept
      in
      let set =
        match Opt.prj mac with
        | None -> set
        | Some mac ->
          List.fold_left begin fun la (br, _, _) ->
            if Boolvector.test branches.is_partial br then la else
              (* FIXME: check for unreachable clauses *)
              match branches.lookaheads.:(br) with
              | None -> IndexSet.empty
              | Some la' -> IndexSet.diff la la'
          end set machine.accepting.:(mac)
      in
      let set = IndexSet.diff set (Dyn.get unaccepted target) in
      let delta0 = Dyn.get delta target in
      let set = IndexSet.diff set delta0 in
      if IndexSet.is_not_empty set then (
        if IndexSet.is_empty delta0 then
          push todo target;
        Dyn.set delta target (IndexSet.union set delta0)
      );
      set
    in
    let counter = ref 0 in
    let propagations = ref 0 in
    let propagate node =
      incr propagations;
      if IndexSet.is_empty (Dyn.get unaccepted node) then
        initialize node;
      let todo = Dyn.get delta node in
      Dyn.set delta node IndexSet.empty;
      Dyn.set unaccepted node (IndexSet.union todo (Dyn.get unaccepted node));
      Array.iter begin fun (accept, target) ->
        let todo = IndexSet.diff todo accept in
        if IndexSet.is_not_empty todo then
          let delta = update target todo in
          if IndexSet.is_not_empty delta then
            Dyn.set predecessors target
              ((node, !counter, delta) :: Dyn.get predecessors target)
      end (Dyn.get successors node)
    in
    let regular = Terminal.regular g in
    let trs =
      Option.fold ~none:IndexSet.empty
        ~some:(Vector.get machine.outgoing) machine.initial
    in
    IndexMap.iter begin fun lrc (accept, enu) ->
      let st = get (find_target trs lrc) (Sum.inj_r stacks.domain enu) in
      ignore (update st (IndexSet.diff regular accept))
    end dgr.initials;
    fixpoint ~counter ~propagate todo;
    stopwatch 1 "Cover (%d iterations, %d propagations, %d uncovered states)"
      !counter !propagations (List.length !uncovered);
    let enum = {
      Enum.domain = Ker.n;
      predecessors = Dyn.get predecessors;
      unaccepted = Dyn.get unaccepted;
    } in
    let position ix =
      let _, pos = Prod.prj maco (Gen.get ker ix) in
      pos
    in
    Graph {enum; position; sinks = !uncovered; unaccepted = Dyn.get unaccepted}
end

module Extract = struct
  let compute_maximal_prefixes
      (type n goal term)
      ~(graph : (n, term) Enum._graph)
      ~(iter_sinks : (n index -> int -> unit) -> unit)
      ~(reached : n index -> goal indexset)
    =
    let sink_min = ref max_int and sink_max = ref 0 in
    iter_sinks (fun _ cost ->
        sink_min := Int.min cost !sink_min;
        sink_max := Int.max cost !sink_max;
      );
    let sink_min = !sink_min in
    let sinks = Array.make (if sink_min = max_int then 0 else !sink_max - sink_min + 1) [] in
    iter_sinks begin fun sink cost ->
      let index = cost - sink_min in
      sinks.(index) <- ([sink], graph.unaccepted sink) :: sinks.(index)
    end;
    (* We have the sinks, propagate to get the maximal nodes *)
    let rejectable = Vector.make graph.domain IndexSet.empty in
    let counter = ref 0 in
    let todo = ref [] in
    let maximals, add_maximal = dyn_array () in
    let first_counter = ref (-1) in
    let propagate (path, reachable) =
      let node = List.hd path in
      let rejectable0 = rejectable.:(node) in
      let rejectable' = IndexSet.union reachable rejectable0 in
      if rejectable' != rejectable0 then (
        (* There are some lookaheads not yet known to be rejectable *)
        rejectable.:(node) <- rejectable';
        if IndexSet.is_empty (reached node) then (
          (* It is an intermediate node *)
          List.iter begin fun (node', _, _) ->
            let reachable = IndexSet.inter reachable (graph.unaccepted node') in
            if IndexSet.is_not_empty reachable then
              push todo (node' :: path, reachable)
          end (graph.predecessors node)
        ) else (
          (* It is a maximal node *)
          if !first_counter = -1 then
            first_counter := !counter;
          add_maximal (!counter - !first_counter) (path, reachable)
        )
      )
    in
    Array.iter begin fun sinks ->
      let todo' = !todo in
      todo := [];
      List.iter propagate sinks;
      List.iter propagate todo';
      incr counter;
    end sinks;
    fixpoint ~counter ~propagate todo;
    !maximals

  let compute_global_prefixes
      (type n goal term)
      ~(graph : (n, term) Enum._graph)
      ~(maximals : (n index list * term indexset) list array)
      ~(reached : n index -> goal indexset)
    : (n index list * term indexset) list array
    =
    let counter = ref 0 in
    let todo = ref [] in
    let globals, add_global = dyn_array () in
    let first_counter = ref (-1) in
    let rejectable = Vector.make graph.domain IndexSet.empty in
    let propagate (path, reachable) =
      let node = List.hd path in
      let rejectable0 = rejectable.:(node) in
      let rejectable' = IndexSet.union reachable rejectable0 in
      if rejectable' != rejectable0 then (
        (* There are some lookaheads not yet known to be rejectable *)
        rejectable.:(node) <- rejectable';
        if IndexSet.is_not_empty (reached node) then (
          if !first_counter = -1 then
            first_counter := !counter;
          add_global (!counter - !first_counter) (path, reachable);
        );
        (* It is an intermediate node *)
        List.iter begin fun (node', _, _) ->
          let reachable = IndexSet.inter reachable (graph.unaccepted node') in
          if IndexSet.is_not_empty reachable then
            push todo (node' :: path, reachable)
        end (graph.predecessors node)
      )
    in
    Array.iter begin fun maximals ->
      let todo' = !todo in
      todo := [];
      List.iter propagate maximals;
      List.iter propagate todo';
      incr counter;
    end maximals;
    fixpoint ~counter ~propagate todo;
    !globals
end

module Report = struct

  let cleanup_sentences (goal : 'term indexset) lr0 (sentences : ('tactic * int * 'term indexset) list) =
    let covered = ref IndexSet.empty in
    sentences
    |> List.sort (fun (_t1,c1,_l1) (_t2,c2,_l2) -> Int.compare c1 c2)
    |> list_rev_filter_map (fun (t,c,l) ->
        let l = IndexSet.inter goal (IndexSet.diff l !covered) in
        covered := IndexSet.union l !covered;
        if IndexSet.is_empty l
        then None
        else Some (t,c,l,lr0)
      )

  let emit_local
    (type lr0 term tactic)
    (goals : (lr0, term indexset) vector)
    (sentences : lr0 index -> (tactic * int * term indexset) list)
    : (lr0 index * (tactic * term indexset) Seq.t) Seq.t
    =
    Vector.to_seqi goals |> Seq.filter_map begin fun (lr0, goal) ->
      if IndexSet.is_empty goal then None else
        match cleanup_sentences goal lr0 (sentences lr0) with
        | [] -> None
        | sentences ->
          let sentences =
            List.to_seq sentences |> Seq.filter_map begin fun (t,_c,l,_) ->
              let l = IndexSet.inter goals.:(lr0) l in
              if IndexSet.is_empty l
              then None
              else Some (t, l)
            end
          in
          Some (lr0, sentences)
    end

  let emit_global
      (type lr0 term tactic)
      (goals : (lr0, term indexset) vector)
      (sentences : lr0 index -> (tactic * int * term indexset) list)
      : (lr0 index * tactic * term indexset) Seq.t
    =
    let module H = Heap.Int in
    let add_sentences heap = function
      | [] -> heap
      | (tactic, c, la, lr0) :: sentences ->
        H.insert c (tactic, la, lr0, sentences) heap
    in
    let all_sentences =
      Vector.fold_lefti begin fun heap lr0 goal ->
        if IndexSet.is_empty goal then heap else
          add_sentences heap (cleanup_sentences goal lr0 (sentences lr0))
      end H.empty goals
    in
    let rec loop heap () =
      match H.pop heap with
      | None -> Seq.Nil
      | Some (_cost, (tactic, la, lr0, rest), heap) ->
        let heap = add_sentences heap rest in
        let la = IndexSet.inter goals.:(lr0) la in
        if IndexSet.is_empty la
        then loop heap ()
        else Seq.Cons ((lr0, tactic, la), loop heap)
    in
    loop all_sentences

  let emit_all (type n goal term)
      ~goals:(goal_domain : goal cardinal)
      ~(graph : (n, term) Enum._graph)
      ~(maximals : (n index list * term indexset) list array)
      ~(globals : (n index list * term indexset) list array)
      ~(reached : n index -> goal indexset)
    : (goal index * (n index list * term indexset) Seq.t) Seq.t *
      (goal index * (n index list * term indexset) Seq.t) Seq.t
    =
    let goals = Vector.make goal_domain IndexSet.empty in
    let populate offset arr =
      let sentences = Vector.make goal_domain [] in
      Array.iteri begin fun i candidates ->
        List.iter begin fun (path, la) ->
          IndexSet.iter begin fun goal ->
            goals.@(goal) <- IndexSet.union la;
            sentences.@(goal) <- List.cons (path, offset + i, la)
          end (reached (List.hd path))
        end candidates
      end arr;
      sentences
    in
    let max_sentences = populate 0 maximals in
    let glob_sentences = populate (Array.length maximals) globals in
    let rec commit_until prefix la = function
      | [] -> ()
      | ixs when ixs == prefix -> ()
      | ix :: ixs ->
        IndexSet.iter
          (fun g -> goals.:(g) <- IndexSet.diff goals.:(g) la)
          (reached ix);
        commit_until prefix la ixs
    in
    let expand_and_commit (prefix0, la) =
      commit_until [] la prefix0;
      let rec loop_successor prefix la acc (node, goal) =
        let la = IndexSet.inter (graph.unaccepted node) la in
        loop_successors (node :: prefix) la acc goal
      and loop_successors prefix la acc goal =
          match graph.predecessors (List.hd prefix) with
        | [] ->
          commit_until prefix0 la prefix;
          (prefix, la) :: acc
        | predecessors ->
          let match_goal (next, _cost, goal') =
            let goal = IndexSet.inter goal' goal in
            if IndexSet.is_empty goal then None else
              Some (next, goal)
          in
          match List.filter_map match_goal predecessors with
          | [] -> assert false
          | [next] -> loop_successor prefix la acc next
          | nexts -> List.fold_left (loop_successor prefix la) acc nexts
      in
      List.to_seq (loop_successors prefix0 la [] la)
    in
    let is_nonempty_seq seq =
      match seq () with
      | Seq.Nil -> None
      | Seq.Cons (x, xs) ->
        Some (fun () -> Seq.Cons (x, xs))
    in
    let locals =
      emit_local goals (Vector.get max_sentences)
      |> Seq.filter_map begin fun (lr0, sentences) ->
        match is_nonempty_seq (Seq.concat_map expand_and_commit sentences) with
        | None -> None
        | Some sentences ->
          Some (lr0, sentences)
      end
    in
    let globals =
      emit_global goals (Vector.get glob_sentences)
      |> Seq.filter_map begin fun (lr0, path, la) ->
        match is_nonempty_seq (expand_and_commit (path, la)) with
        | None -> None
        | Some sentences ->
          Some (lr0, sentences)
      end
    in
    (locals, globals)
end
