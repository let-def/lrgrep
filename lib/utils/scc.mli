open Strong.Finite

(** Compute the strongly connected components of a graph whose nodes are
    represented as elements of a [Strong.Finite.set]. *)
module Make (G : sig

    type states
    (** [states Finite.set] is the set of the nodes of the graph *)

    val states : states set
    (** Represent the nodes of the graph *)

    val successors : states elt -> (states elt -> 'a -> 'a) -> 'a -> 'a
    (** Fold a function over all successors of a node of the graph *)
  end) :
sig
  type sccs
  (** [sccs Finite.set] is the set of strongly connected components *)

  val scc : (G.states, sccs elt) Array.t
  (** A map that associates each node of the graph to its component. *)

  val sccs : (sccs, G.states elt list) Array.t
  (** A map that associates each component with the nodes that belong to it. *)
end
