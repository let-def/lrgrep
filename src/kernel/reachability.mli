open Fix.Indexing
open Utils.Misc
open Info

module type S = sig
  type g

  type reduction = {
    (* The production that is being reduced *)
    production: g production index;

    (* The set of lookahead terminals that allow this reduction to happen *)
    lookahead: g terminal indexset;

    (* The shape of the stack, all the transitions that are replaced by the
       goto transition when the reduction is performed *)
    steps: g transition index list;

    (* The lr1 state at the top of the stack before reducing.
       That is [state] can reduce [production] when the lookahead terminal
       is in [lookahead]. *)
    state: g lr1 index;
  }

  (* [unreduce tr] lists all the reductions that ends up following [tr]. *)
  val unreduce : g goto_transition index -> reduction list

  module Classes : sig
    (* Returns the classes of terminals for a given goto transition *)
    val for_edge : g goto_transition index -> g terminal indexset array

    (* Returns the classes of terminals for a given LR(1) state *)
    val for_lr1 : g lr1 index -> g terminal indexset array

    (* Returns the classes of terminals before taking a transition *)
    val pre_transition : g transition index -> g terminal indexset array

    (* Returns the classes of terminals after taking a transition *)
    val post_transition : g transition index -> g terminal indexset array
  end

  module Coercion : sig
    type pre = Pre_identity | Pre_singleton of int

    (* Compute the pre coercion from a partition of the form
         P = first(cost(s, A))
       to a partition of the form
         Q = first(ccost(s, A → ϵ•α)))
    *)
    val pre : 'a indexset array -> 'a indexset array -> pre option

    type forward = int array array
    type backward = int array
    type infix = { forward : forward; backward : backward; }

    (* Compute the infix coercion from two partitions P Q such that Q <= P *)
    val infix : ?lookahead:'a indexset -> 'a indexset array -> 'a indexset array -> infix
  end

  module Tree : sig
    include CARDINAL

    (* Returns the leaf node corresponding to a given transition *)
    val leaf : g transition index -> n index

    (* Splits a node into its left and right children if it is an inner node *)
    val split : n index -> (g transition index, n index * n index) either

    (* Returns the nullable terminals and non-nullable equations for a given goto transition *)
    type equations = {
      nullable_lookaheads: g terminal indexset;
      nullable: reduction list;
      non_nullable: (reduction * n index) list;
    }
    val goto_equations : g goto_transition index -> equations

    (* Returns the pre-classes for a given node *)
    val pre_classes : n index -> g terminal indexset array

    (* Returns the post-classes for a given node *)
    val post_classes : n index -> g terminal indexset array
  end

  (* Identify each cell of compact cost matrices.
     A [Cell.n index] can be thought of as a triple made of a tree node and two indices
     (row, col) of the compact cost matrix associated to the node. *)
  module Cell : sig
    include CARDINAL

    (* A value of type row represents the index of a row of a matrix.
       A row of node [n] belongs to the interval
         0 .. Array.length (Tree.pre_classes n) - 1
    *)
    type row = int

    (* A value of type column represents the index of a column of a matrix.
       A column of node [n] belongs to the interval
         0 .. Array.length (Tree.post_classes n) - 1
    *)
    type column = int

    (* Get the cell corresponding to a node, a row, and a column *)
    val encode : Tree.n index -> pre:row -> post:column -> n index

    (* Get the node, row, and column corresponding to a cell *)
    val decode : n index -> Tree.n index * row * column

    type goto
    val goto : goto cardinal
    val is_goto : n index -> goto index option
    val of_goto : goto index -> n index
    val goto_encode : g goto_transition index -> pre:row -> post:column -> goto index
    val goto_decode : goto index -> g goto_transition index * row * column
    val iter_goto : g goto_transition index -> (goto index -> unit) -> unit
  end

  module Analysis : sig
    val cost : Cell.n index -> int
    val finite : Cell.n index -> bool
  end
end

module Make (Info : Info.S)() : S with type g = Info.g
