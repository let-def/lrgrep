(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* This module provides an implementation of Tarjan's algorithm for
   finding the strongly connected components of a graph.

   The algorithm runs when the functor is applied. Its complexity is
   $O(V+E)$, where $V$ is the number of vertices in the graph $G$, and
   $E$ is the number of edges. *)

module Run (G : sig

  type node

  (* We assume each node has a unique index. Indices must range from
     $0$ to $n-1$, where $n$ is the number of nodes in the graph. *)

  val n: int
  val index: node -> int

  (* Iterating over a node's immediate successors. *)

  val successors: (node -> unit) -> node -> unit

  (* Iterating over all nodes. *)

  val iter: (node -> unit) -> unit

end) = struct

  (* Define the internal data structure associated with each node. *)

  type data = {

      (* Each node carries a flag which tells whether it appears
         within the SCC stack (which is defined below). *)

      mutable stacked: bool;

      (* Each node carries a number. Numbers represent the order in
         which nodes were discovered. *)

      mutable number: int;

      (* Each node [x] records the lowest number associated to a node
         already detected within [x]'s SCC. *)

      mutable low: int;

      (* Each node carries a pointer to a representative element of
         its SCC. This field is used by the algorithm to store its
         results. *)

      mutable representative: G.node;

      (* Each representative node carries a list of the nodes in
         its SCC. This field is used by the algorithm to store its
         results. *)

      mutable scc: G.node list

    }

  (* Define a mapping from external nodes to internal ones. Here, we
     simply use each node's index as an entry into a global array. *)

  let table =

    (* Create the array. We initially fill it with [None], of type
       [data option], because we have no meaningful initial value of
       type [data] at hand. *)

    let table = Array.make G.n None in

    (* Initialize the array. *)

    G.iter (fun x ->
      table.(G.index x) <- Some {
        stacked = false;
        number = 0;
        low = 0;
        representative = x;
        scc = []
      }
    );

    (* Define a function which gives easy access to the array. It maps
       each node to its associated piece of internal data. *)

    function x ->
      match table.(G.index x) with
      | Some dx ->
          dx
      | None ->
          assert false (* Indices do not cover the range $0\ldots n$, as expected. *)

  (* Create an empty stack, used to record all nodes which belong to
     the current SCC. *)

  let scc_stack = Stack.create()

  (* Initialize a function which allocates numbers for (internal)
     nodes. A new number is assigned to each node the first time it is
     visited. Numbers returned by this function start at 1 and
     increase. Initially, all nodes have number 0, so they are
     considered unvisited. *)

  let mark =
    let counter = ref 0 in
    fun dx ->
      incr counter;
      dx.number <- !counter;
      dx.low <- !counter

  (* This reference will hold a list of all representative nodes.
     The components that have been identified last appear at the
     head of the list. *)

  let representatives =
    ref []

  (* Look at all nodes of the graph, one after the other. Any
     unvisited nodes become roots of the search forest. *)

  let () = G.iter (fun root ->
    let droot = table root in

    if droot.number = 0 then begin

      (* This node hasn't been visited yet. Start a depth-first walk
         from it. *)

      mark droot;
      droot.stacked <- true;
      Stack.push droot scc_stack;

      let rec walk x =
        let dx = table x in

        G.successors (fun y ->
          let dy = table y in

          if dy.number = 0 then begin

            (* $y$ hasn't been visited yet, so $(x,y)$ is a regular
               edge, part of the search forest. *)

            mark dy;
            dy.stacked <- true;
            Stack.push dy scc_stack;

            (* Continue walking, depth-first. *)

            walk y;
            if dy.low < dx.low then
              dx.low <- dy.low

          end
          else if (dy.low < dx.low) && dy.stacked then begin

            (* The first condition above indicates that $y$ has been
               visited before $x$, so $(x, y)$ is a backwards or
               transverse edge. The second condition indicates that
               $y$ is inside the same SCC as $x$; indeed, if it
               belongs to another SCC, then the latter has already
               been identified and moved out of [scc_stack]. *)

            if dy.number < dx.low then
              dx.low <- dy.number

          end

        ) x;

        (* We are done visiting $x$'s neighbors. *)

        if dx.low = dx.number then begin

          (* $x$ is the entry point of a SCC. The whole SCC is now
             available; move it out of the stack. We pop elements out
             of the SCC stack until $x$ itself is found. *)

          let rec loop () =
            let element = Stack.pop scc_stack in
            element.stacked <- false;
            dx.scc <- element.representative :: dx.scc;
            element.representative <- x;
            if element != dx then
              loop() in

          loop();
          representatives := x :: !representatives

        end in

      walk root

    end
  )

  (* There only remains to make our results accessible to the outside. *)

  let representative x =
    (table x).representative

  let scc x =
    (table x).scc

  let representatives =
    Array.of_list !representatives

  (* The array [representatives] contains a representative for each component.
     The components that have been identified last appear first in this array.
     A component is identified only after its successors have been identified;
     therefore, this array is naturally in topological order. *)

  let yield action x =
      let data = table x in
      assert (data.representative == x); (* a sanity check *)
      assert (data.scc <> []);           (* a sanity check *)
      action x data.scc

  let iter action =
    Array.iter (yield action) representatives

  let rev_topological_iter action =
    for i = Array.length representatives - 1 downto 0 do
      yield action representatives.(i)
    done

  let map action =
    Array.map (yield action) representatives |> Array.to_list

  let rev_map action =
    let accu = ref [] in
    rev_topological_iter (fun repr labels ->
      accu := action repr labels :: !accu
    );
    !accu

end

open Fix.Indexing

module IndexedSCC (G : sig
  include CARDINAL
  val successors : (n index -> unit) -> n index -> unit
end) = struct
  module SCC = Run (struct
    type node = G.n index
    let n = cardinal G.n
    let index n = (n : _ index :> int)
    let successors = G.successors
    let iter f = Index.iter G.n f
  end)

  module Repr = Vector.Of_array(struct
      type a = G.n index
      let array = SCC.representatives
    end)

  type n = Repr.n
  let n = Vector.length Repr.vector

  let representatives = Repr.vector

  let nodes = Vector.map
      (fun node -> Utils.IndexSet.of_list (SCC.scc node))
      representatives

  let component = Vector.make' G.n (fun () -> Index.of_int n 0)

  let () =
    Vector.iteri (fun scc nodes' ->
        Utils.IndexSet.iter
          (fun node -> Vector.set component node scc)
          nodes'
      ) nodes
end

open Utils
open Misc

let indexset_bind : 'a indexset -> ('a index -> 'b indexset) -> 'b indexset =
  fun s f ->
  IndexSet.fold_right (fun acc lr1 -> IndexSet.union (f lr1) acc) IndexSet.empty s

let close_relation (type n) (rel : (n, _) vector) =
  let module Scc = IndexedSCC(struct
                     type nonrec n = n
                     let n = Vector.length rel
                     let successors f i =
                       Utils.IndexSet.iter f (Vector.get rel i)
                   end)
  in
  Vector.rev_iteri (fun _scc nodes ->
    let f i = indexset_bind rel.:(i) (Vector.get rel) in
    let set = indexset_bind nodes f in
    IndexSet.iter (fun i -> rel.:(i) <- set) nodes
  ) Scc.nodes
