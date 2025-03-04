open Fix.Indexing
open Utils.Misc


module type S = sig
  module Info : Info.S
  open Info

  module Classes : sig
    (* Returns the classes of terminals for a given goto transition *)
    val for_edge : Transition.goto index -> Terminal.set array

    (* Returns the classes of terminals for a given LR(1) state *)
    val for_lr1 : Lr1.t -> Terminal.set array

    (* Returns the classes of terminals before taking a transition *)
    val pre_transition : Transition.any index -> Terminal.set array

    (* Returns the classes of terminals after taking a transition *)
    val post_transition : Transition.any index -> Terminal.set array
  end

  module Tree : sig
    include CARDINAL

    (* Returns the leaf node corresponding to a given transition *)
    val leaf : Transition.any index -> n index

    (* Splits a node into its left and right children if it is an inner node *)
    val split : n index -> (Transition.any index, n index * n index) either

    (* Returns the nullable terminals and non-nullable equations for a given goto transition *)
    val goto_equations : Transition.goto index -> Terminal.set * (n index * Terminal.set) list

    (* Returns the pre-classes for a given node *)
    val pre_classes : n index -> Terminal.set array

    (* Returns the post-classes for a given node *)
    val post_classes : n index -> Terminal.set array
  end

  module Cells : sig
    (* A value of type t identifies a single cell. It is an integer that
       encodes the node index and the offset of the cell inside the matrix of
       that node.
    *)
    type t = private int

    (* A value of type offset represents an index of a compact cost matrix.
       An offset of node [n] belongs to the interval
         0 .. Array.length (Tree.pre_classes n) *
              Array.length (Tree.post_classes n) - 1
    *)
    type offset = int

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

    (* The table that stores all compact cost matrices *)
    val table : (Tree.n, int array) vector

    (* Total number of cells *)
    val count : int

    (* Cost of a cell *)
    val cost : t -> int

    (* Compute an index in a table *)
    val table_index : post_classes:int -> pre:int -> post:int -> int

    (* Compute a linear offset from a node, a row, and a column *)
    val offset : Tree.n index -> row -> column -> offset

    (* Get the cell corresponding to a node and offset *)
    val encode_offset : Tree.n index -> offset -> t

    (* Get the node and offset corresponding to a cell *)
    val decode_offset : t -> Tree.n index * offset

    (* Get the cell corresponding to a node, a row, and a column *)
    val encode : Tree.n index -> row -> column -> t

    (* Get the node, row, and column corresponding to a cell *)
    val decode : t -> Tree.n index * row * column
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

  module Finite : sig
    val get : Cells.t -> bool
  end
end

module Make (Info : Info.S)() : S with module Info := Info
