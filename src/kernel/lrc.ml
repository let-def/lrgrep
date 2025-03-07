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

(* Extended signature for class-based refinements *)
module type S = sig
  include Info.INDEXED
  module Info : Info.S
  open Info

  val lr1_of_lrc : t -> Lr1.t
  val lrcs_of_lr1 : Lr1.t -> set
  val first_lrc_of_lr1 : Lr1.t -> t
  val lookahead : t -> Info.Terminal.set
  val class_index : t -> int
  val to_string : t -> string
  val set_to_string : set -> string

  (** Wait states lifted to LRC *)
  val all_wait : set
  val all_successors : t -> set

  val reachable_from : t -> set
end

(* Signature for a fully-featured LRC module with reachability information *)
module type ENTRYPOINTS = sig
  module Lrc : S
  val reachable : Lrc.set
  val wait : Lrc.set
  val entrypoints : Lrc.set
  val predecessors : Lrc.t -> Lrc.set
  val successors : Lrc.t -> Lrc.set

  (* [some_prefix st] is a list of states reaching an entrypoint, starting from
     [st], or the empty list if state is unreachable. *)
  val some_prefix : Lrc.t -> Lrc.t list
end

(* Functor to create an LRC module from an Info module and Reachability module *)
module Make
    (I : Info.S)
    (Reachability : Reachability.S with module Info := I)
: S with module Info := I
=
struct
  open I

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
  let () = stopwatch 2 "Computed LRC set"

  (* Compute successors for each LRC state *)
  let all_successors, reachable_from =
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
          let encode = Reachability.Cell.encode node in
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
              if Reachability.Analysis.cost (encode ~pre ~post) < max_int then
                reachable := IndexSet.add (index_shift src_first pre) !reachable
            done;
            let reachable = !reachable in
            Array.iter (fun index ->
                table.@(index_shift tgt_first index) <- IndexSet.union reachable)
              coercion.forward.(post)
          done
        in
        List.iter process_transition (Transition.predecessors lr1)
    in
    Index.iter Lr1.n process;
    let successors = Misc.relation_reverse n table in
    stopwatch 2 "Computed LRC successors";
    let reachable = Misc.relation_closure ~reverse:table successors in
    let count = ref 0 in
    let hash = ref 0 in
    Vector.iteri (fun i s ->
        let s = IndexSet.cardinal s in
        count := !count + s;
        hash := !hash + Index.to_int i * s)
      reachable;
    Printf.eprintf "closure: cardinal: %d, hash: %d\n" !count !hash;
    stopwatch 2 "Closed LRC successors";
    (Vector.get successors, Vector.get reachable)

  (* Convert an LRC state to a string representation *)
  let to_string lrc =
    Printf.sprintf "%s/%d"
      (Lr1.to_string (lr1_of_lrc lrc))
      (class_index lrc)

  (* Convert a set of LRC states to a string representation *)
  let set_to_string lrcs =
    string_of_indexset ~index:to_string lrcs
end

(* Find states reachable from specific states by closing over raw reachability
   information *)
module From_entrypoints(Info : Info.S)
    (Lrc : S with module Info := Info)
    (For : sig val entrypoints : Lrc.set end)
  : ENTRYPOINTS
    with module Lrc.Info := Info
    with module Lrc := Lrc =
struct
  include For

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
  let predecessors = Misc.relation_reverse Lrc.n successors

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

(* Minimize the number of states in a refinement *)
(*module Minimize
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
      (fun lrc -> table.@(lr1_of_lrc lrc) <- IndexSet.add lrc);
    Vector.get table

  (* Map from LR1 states to their first minimized state *)
  let first_lrc_of_lr1 lr1 =
    Option.get (IndexSet.minimum (lrcs_of_lr1 lr1))

  (* Compute successors for each minimized state *)
  let all_successors =
    let table = Vector.make n IndexSet.empty in
    Index.iter MDFA.transitions
      (fun tr -> table.@(MDFA.target tr) <- IndexSet.add (MDFA.source tr));
    Vector.get table

  (* Step timing after minimizing LRC states *)
  let () =
    Stopwatch.step time "Minimized Lrc from %d states to %d"
      (cardinal Lrc.n) (cardinal MDFA.states);
    (* End timing *)
    Stopwatch.leave time
  end *)
