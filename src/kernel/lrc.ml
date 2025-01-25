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

(* Signature for a refinement of the set of LR states *)
module type RAW0 = sig
  include Info.INDEXED
  module Info : Info.S
  open Info

  (** Wait states lifted to LRC *)
  val all_wait : set

  val lr1_of_lrc : t -> Lr1.t
  val lrcs_of_lr1 : Lr1.t -> set
  val first_lrc_of_lr1 : Lr1.t -> t
  val all_successors : t -> set
end

(* Extended signature for class-based refinements *)
module type RAW = sig
  include RAW0
  val lookahead : t -> Info.Terminal.set
  val class_index : t -> int
  val to_string : t -> string
  val set_to_string : set -> string
end

(* Signature for a fully-featured LRC module with reachability information *)
module type S = sig
  include RAW
  val reachable : set
  val wait : set
  val entrypoints : set
  val predecessors : t -> set
  val successors : t -> set

  (* [some_prefix st] is a list of states reaching an entrypoint, starting from
     [st], or the empty list if state is unreachable. *)
  val some_prefix : t -> t list
end

(* Find states reachable from specific states by closing over raw reachability
   information *)
module From_entrypoints(Info : Info.S)
    (Lrc : RAW with module Info := Info)
    (For : sig val entrypoints : Lrc.set end)
  : S with module Info := Info and type n = Lrc.n =
struct
  include Lrc
  include For

  type n = Lrc.n
  let n = Lrc.n

  (* Set of reachable states *)
  let reachable = ref IndexSet.empty

  (* Compute transitive successors for each state and populate reachable set *)
  let successors =
    let table = Vector.make Lrc.n IndexSet.empty in
    let todo = ref entrypoints in
    let populate i =
      reachable := IndexSet.add i !reachable;
      if IndexSet.is_empty (Vector.get table i) then (
        let succ = Lrc.all_successors i in
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

  (* Compute predecessors for each state using successors information *)
  let predecessors = Misc.relation_reverse n successors

  (* Accessors for successors and predecessors *)
  let successors = Vector.get successors
  let predecessors = Vector.get predecessors

  (* Final reachable states *)
  let reachable = !reachable

  (* Wait states that are reachable *)
  let wait = IndexSet.inter Lrc.all_wait reachable

  (* Compute a prefix to reach each state *)
  let some_prefix =
    let table = lazy (
      let table = Vector.make Lrc.n [] in
      let todo = ref [] in
      let expand prefix state =
        match Vector.get table state with
        | [] ->
          Vector.set table state prefix;
          let prefix = state :: prefix in
          let succ = successors state in
          if not (IndexSet.is_empty succ) then
            push todo (succ, prefix)
        | _ -> ()
      in
      Index.iter Info.Lr1.n (fun lr1 ->
          if Option.is_none (Info.Lr1.incoming lr1) then
            expand [] (Lrc.first_lrc_of_lr1 lr1)
        );
      let propagate (succ, prefix) =
        IndexSet.iter (expand prefix) succ
      in
      fixpoint ~propagate todo;
      table
    )
    in
    (fun st -> Vector.get (Lazy.force table) st)
end

(* Functor to create an LRC module from an Info module and Reachability module *)
module Make
    (I : Info.S)
    (Reachability : Reachability.S with module Info := I)
: RAW with module Info := I
=
struct
  open I

  (* Start timing for LRC computation *)
  let time = Stopwatch.enter Stopwatch.main "Lrc"

  (* Compute the total number of LRC states *)
  let n =
    let count lr1 = Array.length (Reachability.Classes.for_lr1 lr1) in
    let sum = ref 0 in
    Index.iter Lr1.n (fun lr1 -> sum := !sum + count lr1);
    !sum

  (* Lift `n` to type-level *)
  include Const(struct let cardinal = n end)

  type t = n index
  type set = n indexset
  type 'a map = (n, 'a) indexmap

  (* Shift an index by a given offset *)
  let index_shift (i : n index) offset =
    Index.of_int n ((i :> int) + offset)

  (* Compute the difference between two indices *)
  let index_delta (type n) (i : n index) (j : n index) =
    (i :> int) - (j :> int)

  (* Map from LRC states to their corresponding LR1 states *)
  let lr1_of_lrc = Vector.make' n (fun () -> Index.of_int Lr1.n 0)

  (* Map from LR1 states to their corresponding set of LRC states *)
  let lrcs_of_lr1 =
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
        Vector.set lr1_of_lrc lrc lr1
      done;
      !all
    in
    Vector.init Lr1.n init_lr1

  (* Map from LR1 states to their first LRC state *)
  let first_lrc_of_lr1 = Vector.map (fun x -> Option.get (IndexSet.minimum x)) lrcs_of_lr1

  (* Accessors for the mappings between LRC and LR1 states *)
  let lr1_of_lrc       = Vector.get lr1_of_lrc
  let lrcs_of_lr1      = Vector.get lrcs_of_lr1
  let first_lrc_of_lr1 = Vector.get first_lrc_of_lr1

  (* Set of wait LRC states *)
  let all_wait = IndexSet.map first_lrc_of_lr1 Lr1.wait

  (* Compute the class index for an LRC state *)
  let class_index lrc =
    let lr1 = lr1_of_lrc lrc in
    let lrc0 = first_lrc_of_lr1 lr1 in
    index_delta lrc lrc0

  (* Compute the lookahead terminals for an LRC state *)
  let lookahead lrc =
    let lr1 = lr1_of_lrc lrc in
    let lrc0 = first_lrc_of_lr1 lr1 in
    let lookaheads = Reachability.Classes.for_lr1 lr1 in
    lookaheads.(index_delta lrc lrc0)

  (* Step timing after computing the LRC set *)
  let () = Stopwatch.step time "Computed LRC set"

  (* Compute successors for each LRC state *)
  let all_successors =
    (* TODO: This computes the predecessors and then reverses the relation.
     * We could compute the successors directly. *)
    let table = Vector.make n IndexSet.empty in
    let process lr1 =
      let tgt_first = first_lrc_of_lr1 lr1 in
      match Option.map Symbol.desc (Lr1.incoming lr1) with
      | None -> ()
      | Some (T t) ->
        Vector.set table tgt_first @@
        List.fold_left (fun acc tr ->
          let src = Transition.source tr in
          let class_index =
            Misc.array_findi
              (fun _ classe -> IndexSet.mem t classe) 0 (Reachability.Classes.for_lr1 src)
          in
          let src_index = index_shift (first_lrc_of_lr1 src) class_index in
          IndexSet.add src_index acc
        ) IndexSet.empty (Transition.predecessors lr1)
      | Some _ ->
        let process_transition tr =
          let node = Reachability.Tree.leaf tr in
          let cells = Vector.get Reachability.Cells.table node in
          let pre_classes = Reachability.Classes.pre_transition tr in
          let post_classes = Reachability.Classes.post_transition tr in
          let coercion =
            Reachability.Coercion.infix post_classes
              (Reachability.Classes.for_lr1 lr1)
          in
          let src_first = first_lrc_of_lr1 (Transition.source tr) in
          let pre_classes = Array.length pre_classes in
          let post_classes = Array.length post_classes in
          for post = 0 to post_classes - 1 do
            let reachable = ref IndexSet.empty in
            for pre = 0 to pre_classes - 1 do
              let index = Reachability.Cells.table_index ~post_classes ~pre ~post in
              if cells.(index) < max_int then
                reachable := IndexSet.add (index_shift src_first pre) !reachable
            done;
            let reachable = !reachable in
            Array.iter
              (fun index ->
                vector_set_union table
                  (index_shift tgt_first index) reachable)
              coercion.forward.(post)
          done
        in
        List.iter process_transition (Transition.predecessors lr1)
    in
    Index.iter Lr1.n process;
    Vector.get (Misc.relation_reverse n table)

  (* Convert an LRC state to a string representation *)
  let to_string lrc =
    Printf.sprintf "%s/%d"
      (Lr1.to_string (lr1_of_lrc lrc))
      (class_index lrc)

  (* Convert a set of LRC states to a string representation *)
  let set_to_string lrcs =
    string_of_indexset ~index:to_string lrcs

  (* End timing after computing all necessary information *)
  let () = Stopwatch.leave time
end

(* Minimize the number of states in a refinement *)
module Minimize
    (Info : Info.S)
    (Lrc : RAW0 with module Info := Info)
  : RAW0 with module Info := Info =
struct
  open Info
  (* Start timing for LRC minimization *)
  let time = Stopwatch.enter Stopwatch.main "Minimizing Lrc"

  (* Define a DFA (Deterministic Finite Automaton) module for minimization *)
  module DFA = struct
    type states = Lrc.n
    let states = Lrc.n

    (* Define transitions as a vector of (source, target) pairs *)
    module Transitions = Vector.Of_array(struct
      type a = Lrc.n index * Lrc.n index
      let array =
        let count = ref 0 in
        Index.iter Lrc.n
          (fun lrc -> count := !count + IndexSet.cardinal (Lrc.all_successors lrc));
        let array = Array.make !count (Index.of_int Lrc.n 0, Index.of_int Lrc.n 0) in
        let index = ref 0 in
        Index.iter Lrc.n
          (fun src ->
            IndexSet.iter (fun tgt ->
              array.(!index) <- (src, tgt);
              incr index;
            ) (Lrc.all_successors src)
          );
        array
    end)

    type transitions = Transitions.n
    let transitions = Vector.length Transitions.vector

    (* Label transitions with their corresponding LR1 states *)
    let label tr =
      let src, _ = Vector.get Transitions.vector tr in
      Lrc.lr1_of_lrc src

    (* Source state for a transition *)
    let source tr = fst (Vector.get Transitions.vector tr)

    (* Target state for a transition *)
    let target tr = snd (Vector.get Transitions.vector tr)

    (* Initial states (wait states in Lrc) *)
    let initials f =
      IndexSet.iter f Lrc.all_wait

    (* Final states (states with no incoming transitions in LR1) *)
    let finals f =
      Index.iter Lr1.n
        (fun lr1 ->
          match Lr1.incoming lr1 with
          | None -> IndexSet.iter f (Lrc.lrcs_of_lr1 lr1)
          | Some _ -> ())

    (* Refinements based on incoming transitions *)
    let refinements (f : (add:(states index -> unit) -> unit) -> unit) =
      Index.iter Lr1.n
        (fun lr1 ->
          match Lr1.incoming lr1 with
          | None ->
            f (fun ~add -> IndexSet.iter add (Lrc.lrcs_of_lr1 lr1))
          | Some _ -> ()
        )
  end

  (* Minimize the DFA using Valmari's algorithm *)
  module MDFA = Valmari.Minimize(struct
    type t = Lr1.t
    let compare = compare_index
  end)(DFA)

  type n = MDFA.states
  let n = MDFA.states

  type t = n index
  type set = n indexset
  type 'a map = (n, 'a) indexmap

  (* Set of wait states in the minimized DFA *)
  let all_wait =
    let set = ref IndexSet.empty in
    Array.iter (fun t -> set := IndexSet.add t !set) MDFA.initials;
    !set

  (* Map from minimized states to their corresponding LR1 states *)
  let lr1_of_lrc =
    tabulate_finset n
      (fun t -> Lrc.lr1_of_lrc (MDFA.represent_state t))

  (* Map from LR1 states to their corresponding set of minimized states *)
  let lrcs_of_lr1 =
    let table = Vector.make Lr1.n IndexSet.empty in
    Index.iter n
      (fun lrc -> vector_set_add table (lr1_of_lrc lrc) lrc);
    Vector.get table

  (* Map from LR1 states to their first minimized state *)
  let first_lrc_of_lr1 lr1 =
    Option.get (IndexSet.minimum (lrcs_of_lr1 lr1))

  (* Compute successors for each minimized state *)
  let all_successors =
    let table = Vector.make n IndexSet.empty in
    Index.iter MDFA.transitions
      (fun tr -> vector_set_add table (MDFA.target tr) (MDFA.source tr));
    Vector.get table

  (* Step timing after minimizing LRC states *)
  let () =
    Stopwatch.step time "Minimized Lrc from %d states to %d"
      (cardinal Lrc.n) (cardinal MDFA.states);
    (* End timing *)
    Stopwatch.leave time
end
