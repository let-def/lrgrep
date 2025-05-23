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
  first_lrc_of: ('g lr1, 'g n index) vector;
  all_wait: 'g n indexset;
  all_successors: ('g n, 'g n indexset) vector;
  reachable_from: ('g n, 'g n indexset) vector;
}

(* Compute the difference between two indices *)
let index_delta (type n) (i : n index) (j : n index) =
  (i :> int) - (j :> int)

(* Compute the index of the class of an LRC state *)
let class_index t lrc =
  index_delta lrc t.first_lrc_of.:(t.lr1_of.:(lrc))

(* Compute the lookahead terminals for an LRC state *)
let lookahead (type g) ((module Reachability) : g Reachability.t) t lrc =
  let lr1 = t.lr1_of.:(lrc) in
  (Reachability.Classes.for_lr1 lr1).(index_delta lrc t.first_lrc_of.:(lr1))

(*val to_string : 'g info -> 'g t -> 'g n index -> string
  val set_to_string : 'g info -> 'g t -> 'g n indexset -> string*)

let count t = Vector.length t.lr1_of

(* Functor to create an LRC module from an Info module and Reachability module *)
let make (type g) (info : g info) ((module Reachability) : g Reachability.t) =
  let open (val info) in

  (* Compute the total number of LRC states *)
  let n =
    let count lr1 = Array.length (Reachability.Classes.for_lr1 lr1) in
    let sum = ref 0 in
    Index.iter Lr1.n (fun lr1 -> sum := !sum + count lr1);
    !sum
  in

  (* Lift `n` to type-level *)
  let open N.Const(struct type t = g let cardinal = n end) in

  (* Shift an index by a given offset *)
  let index_shift (i : n index) offset =
    Index.of_int n ((i :> int) + offset)
  in

  (* Map from LRC states to their corresponding LR1 states *)
  let lr1_of = Vector.make' n (fun () -> Index.of_int Lr1.n 0) in

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
    Vector.init Lr1.n init_lr1
  in

  (* Map from LR1 states to their first LRC state *)
  let first_lrc_of =
    Vector.map (fun x -> Option.get (IndexSet.minimum x)) lrcs_of
  in

  (* Set of wait LRC states *)
  let all_wait = IndexSet.map (Vector.get first_lrc_of) Lr1.wait in

  (* Step timing after computing the LRC set *)
  stopwatch 2 "Computed LRC set";

  (* Compute transitions for each LRC state *)
  let predecessors = Vector.make n IndexSet.empty in
  let process lr1 =
    let tgt_first = first_lrc_of.:(lr1) in
    match Option.map Symbol.desc (Lr1.incoming lr1) with
    | None -> ()
    | Some (T t) ->
      Vector.set predecessors tgt_first @@
      List.fold_left (fun acc tr ->
          let src = Transition.source tr in
          let class_index =
            Misc.array_findi
              (fun _ classe -> IndexSet.mem t classe) 0 (Reachability.Classes.for_lr1 src)
          in
          let src_index = index_shift first_lrc_of.:(src) class_index in
          IndexSet.add src_index acc
        ) IndexSet.empty (Transition.predecessors lr1)
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
        let src_first = first_lrc_of.:(Transition.source tr) in
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
      List.iter process_transition (Transition.predecessors lr1)
  in
  Index.iter Lr1.n process;
  let all_successors = Misc.relation_reverse n predecessors in
  stopwatch 2 "Computed LRC successors";
  let reachable_from = Misc.relation_closure ~reverse:predecessors all_successors in
  stopwatch 2 "Closed LRC successors";
  {lr1_of; lrcs_of; first_lrc_of; all_wait; all_successors; reachable_from}

(* Convert an LRC state to a string representation *)
let to_string (type g) ((module Info) : g info) t lrc =
  Printf.sprintf "%s/%d"
    (Info.Lr1.to_string t.lr1_of.:(lrc))
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
let from_entrypoints (type g) (info: g info) lrc_graph entrypoints : g entrypoints =
  let open (val info) in
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
      Vector.iteri (fun lr1 lrc0 ->
          if Option.is_none (Lr1.incoming lr1) then
            expand [] lrc0)
        lrc_graph.first_lrc_of;
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
