(* MIT License
 *
 * Copyright (c) 2026 Frédéric Bour
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 *
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

(** Generating parse sentences from transitions

    This module provides functionality to generate concrete parse examples
    (sentences) from sequences of LR states or LR transitions. It's used
    for generating counterexamples and debug information.

    Core algorithm:

    - The algorithm works backwards from the desired parsing outcome to find
      a valid sequence of transitions that would produce that outcome.

    - [to_transitions] converts a sequence of LR states to a sequence of
      transitions that connect those states.

    - [to_cells] maps transitions to reduction graph cells, using dynamic
      programming to find the minimum-cost path through the reduction graph.

    - [expand_cells] recursively expands cells back to the original terminal
      symbols that would trigger the reductions.

    Key data structures:

    - Cells: Represent positions in the reduction graph, encoded as a compact
      triple (node, pre_class, post_class) for efficient storage and lookup.
      (Pre_class and post_class constrain the lookahead symbols that can precede
      and follow)

    - The algorithm uses dynamic programming to find minimum-cost paths
       through the reduction graph.

    Implementation details:

    - [to_cells] uses a sophisticated dynamic programming approach where at
      each transition, it considers:
      - All post_classes of the current node
      - For each, all pre_classes that can reach it with finite cost
      Then it keeps only the minimal cost paths

    - [expand_cells] handles two cases:
      - [L tr]: A transition node - either shift (return the terminal) or
        goto (recursively solve the subproblem with minimum cost)
      - [R (l, r)]: An inner node - decompose into left and right subproblems,
        finding solutions that minimize total cost

    - The [Break] exception is used to short-circuit when a minimal-cost
      solution is found during the exploration of all possible decompositions.

    - Nullable reductions need some special care. If a nullable reduction is
      possible and the lookahead classes allow it, the algorithm takes that path
      instead of the non-nullable one.
*)

open Utils
open Fix.Indexing
open Info

(** Find the transition from LR state [x] to LR state [y].
    Returns a goto transition if [y] is reached via a non-production,
    or a shift transition if [y] is reached via a terminal.
    Raises [Not_found] indirectly via [assert false] if no incoming
    transition exists for [y]. *)
let find_transition (type g) (g : g grammar) x y =
  match Lr1.incoming g y with
  | None -> assert false
  | Some sym ->
    match Symbol.desc g sym with
    | N n -> Transition.of_goto g (Transition.find_goto g x n)
    | T _ ->
      IndexSet.choose
        (IndexSet.inter
           (Transition.successors g x)
           (Transition.predecessors g y))

(** Convert a sequence of LR states to the initial state and the list of
    transitions connecting consecutive states.
    Raises [Invalid_argument] if the input list is empty. *)
let to_transitions g = function
  | [] -> invalid_arg "Sentence_generation.to_transitions: empty list"
  | initial :: rest ->
    let follow x y = (y, find_transition g x y) in
    let _, trs = List.fold_left_map follow initial rest in
    (initial, trs)

(** Map a list of transitions to reduction graph cells, finding the
    minimum-cost path through the cost DAG using dynamic programming.

    Processes transitions right-to-left. For each transition, iterates all
    post_classes and pre_classes, selecting the (pre, post) pair that
    minimizes the total cost: [cost(cell) + cost(suffix)].

    Returns the list of cells along the minimum-cost path. *)
let to_cells (type g cell) (g : g grammar) ((module R) : (g, cell) Reachability.t_cell) trs =
  let rec aux = function
    | [] -> [Terminal.all g, 0, []]
    | x :: xs ->
      let candidates = aux xs in
      let node = R.Tree.leaf x in
      let post_candidates =
        Array.to_seqi (R.Tree.post_classes node)
        |> Seq.filter_map (fun (post, classe) ->
            let cost, tail =
              List.fold_left begin fun (bcost, _ as best) (cclasse, ccost, tail) ->
                if ccost < bcost && IndexSet.quick_subset cclasse classe
                then (ccost, tail)
                else best
              end (max_int, []) candidates
            in
            if cost < max_int
            then Some (post, cost, tail)
            else None
          )
        |> List.of_seq
      in
      let encode = R.Cell.encode node in
      let pre_candidates =
        Array.to_seqi (R.Tree.pre_classes node)
        |> Seq.filter_map (fun (pre, classe) ->
            let cost, tail =
              List.fold_left begin fun (bcost, _ as best) (post, cost, tail) ->
                let cell = encode ~pre ~post in
                let cost' = R.Analysis.cost cell in
                if cost' < max_int && cost' + cost < bcost
                then (cost' + cost, cell :: tail)
                else best
              end (max_int, []) post_candidates
            in
            if cost < max_int then
              Some (classe, cost, tail)
            else
              None
          )
        |> List.of_seq
      in
      pre_candidates
  in
  snd (
    List.fold_left
      (fun (bcost, _ as best) (_la, cost, tail') ->
         if cost < bcost
         then (cost, tail')
         else best)
      (max_int, []) (aux trs)
  )

(** Recursively expand reduction graph cells back to terminal symbols.

    Handles two node types from the cost tree:
    - [L tr]: A leaf transition — shifts return the terminal symbol;
      gotos check for nullable reductions first, then recurse into the
      minimum-cost non-nullable reduction equation.
    - [R (l, r)]: An inner node — decomposes into left and right
      sub-problems via the coercion matrix, recursing into both children
      whose combined cost equals the current cost.

    Uses a [Break] exception to short-circuit once a minimal-cost
    decomposition is found. *)
let expand_cells (type g cell) (g : g grammar) ((module R) : (g, cell) Reachability.t_cell) cells =
  let open R in
  let exception Break of g terminal index list in
  let rec aux cell acc =
    let node, i_pre, i_post = Cell.decode cell in
    match Tree.split node with
    | L tr ->
      (* The node corresponds to a transition *)
      begin match Transition.split g tr with
        | R shift ->
          (* It is a shift transition, just shift the symbol *)
          Transition.shift_symbol g shift :: acc
        | L goto ->
          (* It is a goto transition *)
          let eqn = Tree.goto_equations goto in
          let c_pre = (Tree.pre_classes node).(i_pre) in
          let c_post = (Tree.post_classes node).(i_post) in
          if not (IndexSet.is_empty eqn.nullable_lookaheads) &&
             IndexSet.quick_subset c_post eqn.nullable_lookaheads &&
             not (IndexSet.disjoint c_pre c_post) then
            (* If a nullable reduction is possible, don't do anything *)
            acc
          else
            (* Otherwise look at all equations that define the cost of the
               goto transition and recursively visit one of minimal cost *)
            let current_cost = Analysis.cost cell in
            match
              List.find_map begin fun (red, node') ->
                if IndexSet.disjoint c_post red.lookahead then
                  (* The post lookahead class does not permit reducing this
                       production *)
                  None
                else
                  match Tree.pre_classes node' with
                  | [|c_pre'|] when IndexSet.disjoint c_pre' c_pre ->
                    (* The pre lookahead class does not allow entering this
                         branch. *)
                    None
                  | pre' ->
                    (* Visit all lookahead classes, pre and post, and find
                         the mapping between the parent node and this
                         sub-node *)
                    let pred_pre _ c_pre' = IndexSet.quick_subset c_pre' c_pre in
                    let pred_post _ c_post' = IndexSet.quick_subset c_post c_post' in
                    match
                      Misc.array_findi pred_pre 0 pre',
                      Misc.array_findi pred_post 0 (Tree.post_classes node')
                    with
                    | exception Not_found -> None
                    | i_pre', i_post' ->
                      let cell = Cell.encode node' ~pre:i_pre' ~post:i_post' in
                      if Analysis.cost cell = current_cost then
                        (* We found a candidate of minimal cost *)
                        Some cell
                      else
                        None
              end eqn.non_nullable
            with
            | None ->
              Printf.eprintf "abort, cost = %d\n%!" current_cost;
              assert false
            | Some cell' ->
              (* Solve the sub-node *)
              aux cell' acc
      end
    | R (l, r) ->
      (* It is an inner node.
         We decompose the problem into left-hand and right-hand
         sub-problems, finding sub-solutions of minimal cost *)
      let current_cost = Analysis.cost cell in
      let coercion =
        Coercion.infix (Tree.post_classes l) (Tree.pre_classes r)
      in
      let l_index = Cell.encode l in
      let r_index = Cell.encode r in
      begin try
          Array.iteri (fun i_post_l all_pre_r ->
              let l_cost = Analysis.cost (l_index ~pre:i_pre ~post:i_post_l) in
              Array.iter (fun i_pre_r ->
                  let r_cost = Analysis.cost (r_index ~pre:i_pre_r ~post:i_post) in
                  if l_cost + r_cost = current_cost then (
                    let acc = aux (r_index ~pre:i_pre_r ~post:i_post) acc in
                    let acc = aux (l_index ~pre:i_pre ~post:i_post_l) acc in
                    raise (Break acc)
                  )
                ) all_pre_r
            ) coercion.Coercion.forward;
          assert false
        with Break acc -> acc
      end
  in
  List.fold_right aux cells []

(** Generate a terminal sentence from a list of transitions.
    Combines [to_cells] and [expand_cells] in a single pipeline. *)
let sentence_of_transitions (type g) (g : g grammar) ((module R) : g Reachability.t) trs =
  expand_cells g (module R) (to_cells g (module R) trs)

(** Generate a terminal sentence from a list of LR states (a parse stack).
    Combines [to_transitions], [to_cells], and [expand_cells] in a single
    pipeline. The input list must be non-empty (see [to_transitions]). *)
let sentence_of_stack (type g) (g : g grammar) ((module R) : g Reachability.t) lr1s =
  let _initial, transitions = to_transitions g lr1s in
  let cells = to_cells g (module R) transitions in
  expand_cells g (module R) cells
