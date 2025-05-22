open Fix.Indexing
open Utils
open Misc
open Info

type 'g reduction_path = {
  (* The production that is being reduced *)
  production: 'g production index;

  (* The set of lookahead terminals that allow this reduction to happen *)
  lookahead: 'g terminal indexset;

  (* The shape of the stack, all the transitions that are replaced by the
     goto transition when the reduction is performed *)
  steps: 'g transition index list;

  (* The lr1 state at the top of the stack before reducing.
     That is [state] can reduce [production] when the lookahead terminal
     is in [lookahead]. *)
  state: 'g lr1 index;
}

val unreduce : 'g info -> ('g goto_transition, 'g reduction_path list) vector

module Coercion : sig
  type pre = Pre_identity | Pre_singleton of int

  (* Compute the pre coercion from a partition of the form
       P = first(cost(s, A))
     to a partition of the form
       Q = first(ccost(s, A → ϵ•α)))
  *)
  val pre
    :  'g terminal indexset array
    -> 'g terminal indexset array
    -> pre option

  type forward = int array array
  type backward = int array
  type infix = { forward : forward; backward : backward; }

  (* Compute the infix coercion from two partitions P Q such that Q <= P *)
  val infix
    :  ?lookahead:'g terminal indexset
    -> 'g terminal indexset array
    -> 'g terminal indexset array
    -> infix
end

module Classes : sig
  (** A partition of terminals is represented as an array of terminal sets *)
  type 'g partition = 'g terminal indexset array

  (** Store all partitions computed for a grammar ['g] *)
  type 'g t = {
    (* Partitions after an LR(1) state.
       It is trivial (all terminals in a single class) for LR(1) states whose
       incoming symbol is a terminal. *)
    of_lr1 : ('g lr1, 'g partition) vector;
    of_goto : ('g goto_transition, 'g partition) vector;
    (* Pre-compute useful partitions to reuse them in subsequent computations *)
    singletons : ('g terminal, 'g partition) vector;
    all_terminals : 'g partition;
  }

  (** Compute the classes for grammar ['g] *)
  val make : 'g info -> ('g goto_transition, 'g reduction_path list) vector -> 'g t

  (** [pre_transition g c tr] returns the partition computed in [c]
      before following the transition [tr] of grammar [g] *)
  val pre_transition : 'g info -> 'g t -> 'g transition index -> 'g partition

  (** [post_transition g c tr] returns the partition computed in [c]
      after following the transition [tr] of grammar [g] *)
  val post_transition :
    'g info -> 'g t -> 'g transition index -> 'g partition
end

(* Specify reachability as a system of recursive equations *)
module Problem : sig
  type 'g var
  type 'g term = ('g transition, 'g var) Sum.n

  type 'g equations = {
    nullable_lookaheads: 'g terminal indexset;
    nullable: 'g reduction_path list;
    non_nullable: ('g reduction_path * 'g term index) list;
  }

  type 'g t = {
    equations: ('g goto_transition, 'g equations) vector;
    variables: 'g var cardinal;
    pre_classes: ('g var, 'g Classes.partition) vector;
    post_classes: ('g var, 'g Classes.partition) vector;
  }

  val make : 'g info -> 'g Classes.t -> 'g t

  val pre_classes : 'g Classes.t -> 'g t -> 'g term index -> 'g Classes.partition

  val post_classes : 'g Classes.t -> 'g t -> 'g term index -> 'g Classes.partition
end

(* Solve the reachability problem by assigning a cost to each cell *)
module Solution : sig
  type 'g cell

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
  val encode : 'g Problem.t -> 'g Problem.term index -> pre:row -> post:column -> 'g cell index

  (* Get the node, row, and column corresponding to a cell *)
  val decode : 'g Problem.t -> 'g cell index -> 'g Problem.term index * row * column

  type 'g goto_cell

  type 'g t = {
    cells: 'g cell cardinal;
    goto_cells: 'g goto_cell cardinal;
    cost: ('g cell, int) vector;
    finite: 'g cell Boolvector.t;
  }

  val is_goto : 'g t -> 'g cell index -> 'g goto_cell index option
  val of_goto : 'g t -> 'g goto_cell index -> 'g cell index
  val goto_encode : 'g t -> 'g goto_transition index -> pre:row -> post:column -> 'g goto_cell index
  val goto_decode : 'g t -> 'g goto_cell index -> 'g goto_transition index * row * column
  val iter_goto : 'g t -> 'g goto_transition index -> ('g goto_cell index -> unit) -> unit

  val solve : 'g Problem.t -> 'g t
end

(* Identify each cell of compact cost matrices.
     A [Cell.n index] can be thought of as a triple made of a tree node and two indices
     (row, col) of the compact cost matrix associated to the node. *)
  module Cell : sig
    include CARDINAL


    type goto
    val goto : goto cardinal
    val is_goto : n index -> goto index option
    val of_goto : goto index -> n index
    val goto_encode : Transition.goto index -> pre:row -> post:column -> goto index
    val goto_decode : goto index -> Transition.goto index * row * column
    val iter_goto : Transition.goto index -> (goto index -> unit) -> unit
  end

  module Analysis : sig
    val cost : Cell.n index -> int
    val finite : Cell.n index -> bool
  end
end

module Make (Info : Info.S)() : S with module Info := Info
