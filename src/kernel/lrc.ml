(* MIT License
 *
 * Copyright (c) [Year] [Your Name]
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

(** This module constructs a graph refining the LR automaton to reason about
  reachable configurations---the pairs of an LR state and a lookahead token, the
  transitions that allow to go from one to another, in order to determine which
  ones are reachable from initial states. It includes functionality to compute
  reachable states, wait states, entry points, predecessors, successors, and
  prefixes for states in the LR automaton.
  LRC means "LR with classes", where a class is the partition of lookahead
  symbols with identical behaviors, as determined by the reachability
  analysis. *)

open Utils
open Misc
open Fix.Indexing
open Info

module N = Unsafe_cardinal()
type 'g n = 'g N.t

type 'g t = {
  lr1_of: ('g n, 'g lr1 index) vector;
  lrcs_of: ('g lr1, 'g n indexset) vector;
  all_wait: 'g n indexset;
  all_successors: ('g n, 'g n indexset) vector;
  reachable_from: ('g n, 'g n indexset) vector;
}

(* Compute the difference between two indices *)
let index_delta (type n) (i : n index) (j : n index) =
  (i :> int) - (j :> int)

(* Compute the index of the class of an LRC state *)
let class_index t lrc =
  index_delta lrc (Option.get (IndexSet.minimum t.lrcs_of.:(t.lr1_of.:(lrc))))

let count t = Vector.length t.lr1_of

(* Functor to create an LRC module from an Info module and Reachability module *)
let make (type g) (g : g grammar) ((module Reachability) : g Reachability.t) =
  (* Compute the total number of LRC states *)
  let n =
    let count lr1 = Array.length (Reachability.Classes.for_lr1 lr1) in
    let sum = ref 0 in
    Index.iter (Lr1.cardinal g) (fun lr1 -> sum := !sum + count lr1);
    !sum
  in

  (* Lift `n` to type-level *)
  let open N.Const(struct type t = g let cardinal = n end) in

  (* Shift an index by a given offset *)
  let index_shift (i : n index) offset =
    Index.of_int n ((i :> int) + offset)
  in

  (* Map from LRC states to their corresponding LR1 states *)
  let lr1_of = Vector.make' n (fun () -> Index.of_int (Lr1.cardinal g) 0) in

  (* Map from LR1 states to their corresponding set of LRC states *)
  let lrcs_of =
    let count = ref 0 in
    let init_lr1 lr1 =
      let classes = Reachability.Classes.for_lr1 lr1 in
      assert (Array.length classes > 0);
      let first = Index.of_int n !count in
      count := !count + Array.length classes;
      let all = ref IndexSet.empty in
      for i = Array.length classes - 1 downto 0 do
        let lrc = index_shift first i in
        all := IndexSet.add lrc !all;
        Vector.set lr1_of lrc lr1
      done;
      !all
    in
    Vector.init (Lr1.cardinal g) init_lr1
  in

  (* Map from LR1 states to their first LRC state *)
  let first_lrc_of =
    Vector.map (fun x -> Option.get (IndexSet.minimum x)) lrcs_of
  in

  (* Set of wait LRC states *)
  let all_wait = IndexSet.map (Vector.get first_lrc_of) (Lr1.wait g) in

  (* Step timing after computing the LRC set *)
  stopwatch 2 "Computed LRC set (%d elements)" (cardinal n);

  (* Compute transitions for each LRC state *)
  let predecessors = Vector.make n IndexSet.empty in
  let process lr1 =
    let tgt_first = first_lrc_of.:(lr1) in
    match Option.map (Symbol.desc g) (Lr1.incoming g lr1) with
    | None -> ()
    | Some (T t) ->
      predecessors.:(tgt_first) <-
        IndexSet.map (fun tr ->
            let src = Transition.source g tr in
            let classes = Reachability.Classes.for_lr1 src in
            let class_index _ classe = IndexSet.mem t classe in
            index_shift first_lrc_of.:(src)
              (Misc.array_findi class_index 0 classes)
          ) (Transition.predecessors g lr1)
    | Some _ ->
      let process_transition tr =
        let node = Reachability.Tree.leaf tr in
        let encode = Reachability.Cell.encode node in
        let pre_classes = Reachability.Classes.pre_transition tr in
        let post_classes = Reachability.Classes.post_transition tr in
        let coercion =
          Reachability.Coercion.infix post_classes
            (Reachability.Classes.for_lr1 lr1)
        in
        let src_first = first_lrc_of.:(Transition.source g tr) in
        let pre_classes = Array.length pre_classes in
        let post_classes = Array.length post_classes in
        for post = 0 to post_classes - 1 do
          let reachable = ref IndexSet.empty in
          for pre = 0 to pre_classes - 1 do
            if Reachability.Analysis.cost (encode ~pre ~post) < max_int then
              reachable := IndexSet.add (index_shift src_first pre) !reachable
          done;
          let reachable = !reachable in
          Array.iter (fun index ->
              predecessors.@(index_shift tgt_first index) <- IndexSet.union reachable)
            coercion.forward.(post)
        done
      in
      IndexSet.rev_iter process_transition (Transition.predecessors g lr1)
  in
  Index.rev_iter (Lr1.cardinal g) process;
  stopwatch 2 "Computed LRC predecessors";
  let all_successors = Misc.relation_reverse n predecessors in
  stopwatch 2 "Computed LRC successors";
  let reachable_from = Vector.copy all_successors in
  Tarjan.close_relation reachable_from;
  stopwatch 2 "Closed LRC successors";
  {lr1_of; lrcs_of; all_wait; all_successors; reachable_from}

let intset_bind s f =
  IntSet.fold_right (fun acc x -> IntSet.union (f x) acc) IntSet.empty s

(* Determinize *)
let determinize (type g) (g : g grammar) ((module Reachability) : g Reachability.t) =
  let open IndexBuffer in
  let module State = Gen.Make() in
  let module Transitions = Gen.Make() in
  let states = State.get_generator () in
  let transitions = Transitions.get_generator () in
  let table = Hashtbl.create 7 in
  let todo = ref [] in
  let visit lr1 classes =
    let key = (lr1, classes) in
    match Hashtbl.find_opt table key with
    | Some index -> index
    | None ->
      let index = Gen.add states key in
      Hashtbl.add table key index;
      push todo index;
      index
  in
  let propagate index =
    let (lr1, classes) = Gen.get states index in
    let post_classes = Reachability.Classes.for_lr1 lr1 in
    IndexSet.iter (fun tr ->
        let pre_classes = Reachability.Classes.pre_transition tr in
        let mid_classes = Reachability.Classes.post_transition tr in
        let node = Reachability.Tree.leaf tr in
        let encode = Reachability.Cell.encode node in
        let classes' =
          intset_bind classes (fun post_index ->
              let ca = post_classes.(post_index) in
              match
                Utils.Misc.array_findi
                  (fun _ cb -> IndexSet.quick_subset ca cb) 0 mid_classes
              with
              | exception Not_found -> IntSet.empty
              | post ->
                IntSet.init_subset 0 (Array.length pre_classes - 1)
                  (fun pre -> Reachability.Analysis.cost (encode ~pre ~post) < max_int)
            )
        in
        ignore (Gen.add transitions (index, visit (Transition.source g tr) classes'))
      ) (Transition.predecessors g lr1)
  in
  let wait = IndexSet.map (fun lr1 -> visit lr1 (IntSet.singleton 0)) (Lr1.wait g) in
  fixpoint ~propagate todo;
  stopwatch 2 "Determinized Lrc wait: %d states\n" (Hashtbl.length table);
  let module Min = Valmari.Minimize(struct
                       type t = g lr1 index
                       let compare =  Index.compare
                     end)(struct
                       let source tr = fst (Gen.get transitions tr)
                       let target tr = snd (Gen.get transitions tr)
                       let label tr = fst (Gen.get states (source tr))

                       let initials f = IndexSet.iter f wait

                       let refinements _ = ()

                       let finals f =
                         IndexSet.iter (fun lr1 ->
                             match Hashtbl.find_opt table (lr1, IntSet.singleton 0) with
                             | None -> ()
                             | Some index -> f index
                           ) (Lr1.entrypoints g)
                       type states = State.n
                       let states = State.n
                       type transitions = Transitions.n
                       let transitions = Transitions.n
                     end)
  in
  stopwatch 2 "Minimized deterministic Lrc: %d states\n" (cardinal Min.states);
  ignore wait
  (*IndexSet.iter (fun lr1 ->
      Array.iteri
        (fun i _ -> visit lr1 (IntSet.singleton i))
        (Reachability.Classes.for_lr1 lr1)
    ) (Lr1.wait g);
  fixpoint ~propagate todo;
  stopwatch 2 "Determinized Lrc all: %d states\n" (Hashtbl.length table)*)

(* Convert an LRC state to a string representation *)
let to_string g t lrc =
  Printf.sprintf "%s/%d"
    (Lr1.to_string g t.lr1_of.:(lrc))
    (class_index t lrc)

(* Convert a set of LRC states to a string representation *)
let set_to_string info t lrcs =
  string_of_indexset ~index:(to_string info t) lrcs

type 'g entrypoints = {
  reachable: 'g n indexset;
  wait: 'g n indexset;
  entrypoints: 'g n indexset;
  successors: ('g n, 'g n indexset) vector;
  predecessors: ('g n, 'g n indexset) vector;
  some_prefix: 'g n index -> 'g n index list;
}

(* Find states reachable from specific states by closing over raw reachability
   information *)
let from_entrypoints (type g) (g: g grammar) lrc_graph entrypoints : g entrypoints =
  (* Set of reachable states *)
  let reachable = ref IndexSet.empty in
  (* Compute transitive successors for each state and populate reachable set *)
  let successors =
    let table = Vector.make (count lrc_graph) IndexSet.empty in
    let todo = ref entrypoints in
    let populate i =
      reachable := IndexSet.add i !reachable;
      if IndexSet.is_empty table.:(i) then (
        let succ = lrc_graph.all_successors.:(i) in
        todo := IndexSet.union succ !todo;
        Vector.set table i succ;
      )
    in
    while not (IndexSet.is_empty !todo) do
      let todo' = !todo in
      todo := IndexSet.empty;
      IndexSet.rev_iter populate todo';
    done;
    table
  in

  (* Compute predecessors for each state using successors information *)
  let predecessors = Misc.relation_reverse (count lrc_graph) successors in

  (* Final reachable states *)
  let reachable = !reachable in

  (* Wait states that are reachable *)
  let wait = IndexSet.inter lrc_graph.all_wait reachable in

  (* Compute a prefix to reach each state *)
  let some_prefix =
    let table = lazy (
      let table = Vector.make (count lrc_graph) [] in
      let todo = ref [] in
      let expand prefix state =
        match Vector.get table state with
        | [] ->
          Vector.set table state prefix;
          let prefix = state :: prefix in
          let succ = successors.:(state) in
          if not (IndexSet.is_empty succ) then
            push todo (succ, prefix)
        | _ -> ()
      in
      Vector.iteri (fun lr1 lrcs ->
          if Option.is_none (Lr1.incoming g lr1) then
            expand [] (Option.get (IndexSet.minimum lrcs)))
        lrc_graph.lrcs_of;
      let propagate (succ, prefix) =
        IndexSet.iter (expand prefix) succ
      in
      fixpoint ~propagate todo;
      table
    )
    in
    (fun st -> Vector.get (Lazy.force table) st)
  in
  {reachable; wait; entrypoints; successors; predecessors; some_prefix}
