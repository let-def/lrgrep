open Fix.Indexing
open Utils

(* This module works with an LR automaton and finds ways to synthesize the symbols necessary to take a goto transition (that is, given an arbitrary parser configuration, how to produce a list of action that is guaranteed to produce a non-terminal, provided that this is a correct continuation of this configuration).
   This is used by [Recovery] to generate an Abstract Syntax Tree (AST) even for invalid inputs.

   The module is implemented as a functor taking an LR automaton and metadata to guide the synthesis, and computing the cheapest sequence of actions to take a given transition or completing a partially recognized rule.
   Cheapest because there are, in general, infinitely many solutions to these problems. A large part of this module deals with cost: each action is given a cost (by default [1.0] per symbol but this can be arbitrarily customized), and the sequence with the lowest is selected.
*)
module type S = sig
  module Info : Mid.Info.S
  open Info

  (* Synthesis operates by answering two kind of questions:
     - how to take a Goto transition (a transition labelled by a non-terminal; this amounts to synthesizing the non-terminal
     - how to complete a partially recognized rule (represented as a pair of a rule and an integer representing a position in this rule); this amounts to synthesizing all symbols at the right of this position
     These questions are represented by the [variable] type. *)
  type variable =
    | Goto of Transition.goto index
    | Tail of Lr1.t * Item.t (* In [Tail (st, item)], item must be long to [Lr1.effective_items st] *)

  (* Converts a variable to a string for debugging and logging purposes *)
  val variable_to_string : variable -> string

  (* Finds the productions that can be used to take a goto transition.
     In other words, if a parser is in state [Transition.(source (of_goto gt))],
     synthesizing the symbols of any production in [goto_candidates gt] and reducing this production
     causes the parser to follow the transition [gt]. *)
  val goto_candidates : Transition.goto index -> Production.set

  (* To solve a tail problem, one needs to look at the item:
     - if it is positioned at the end of the production, this production should be reduced
     - otherwise it is necessary to follow a transition to another state, advancing the item by one symbol *)
  type tail_solution =
    | Tail_reduce of float (* [Tail_reduce c]: the production should be reduced, costing [c] *)
    | Tail_follow of float * Transition.any index * Item.t
      (* [Tail_follow (c, tr, it)]: the transition [tr] should be followed.
         Directly following the transition would cost [c].
         If [c] is infinite, it is necessary to decompose the problem:
         - if [tr] is labelled by a terminal, this tail is stuck
         - if [tr] is labelled by a non-terminal, it is a goto transition and this should be solved first *)

  (* Determines the best solution for a tail item, returning the penalty for this item and the action to take *)
  val tail_candidates : Lr1.t -> Item.t -> float * tail_solution

  (* Calculates the cost of the solution of a variable *)
  val cost_of  : variable -> float

  (* Type representing an action to take in the LR automaton for synthesizing symbols. *)
  type action =
    | Reduce of Production.t (* Reduce a production *)
    | Shift  of Symbol.t     (* Follow the transition labelled by a symbol *)
    | Var    of variable     (* Apply the solution to this sub-problem *)

  (* Determines the actions with the lowest cost solving a given variable and returns their cost alongside.
     If the cost is infinite, the actions are incomplete. Synthesis is stuck and needs more placeholder values to proceed. *)
  val action : variable -> float * action list

  (* An obstacle prevent a variable from being synthesized:
     - it is a symbol if this symbol requires a semantic value and none has
       been provided, or if the symbol has been given an infinite cost
     - it is a production if this production has been given an infinite cost
     - it is an item if this item has been given an infinite penalty:w
   *)
  module Obstacle : sig
    include CARDINAL
    type t = n index

    type desc =
      | Symbol of Symbol.t
      | Production of Production.t
      | Item of Item.t

    val inj : desc -> t
    val prj : t -> desc
  end

  (* Represent a set of sets of obstacles *)
  module ObstacleSet : Set.S with type elt = Obstacle.n IndexSet.t

  (* Finds the obstacles preventing the synthesis of a given variable.

     If the variable can be solved ([cost_of var < infinity]), this set is
     empty. Otherwise, the cost is infinite because some symbols necessary to
     synthesize the variable cannot be synthesized (they are missing semantic
     values of attributed an infinite cost), or because some items or
     productions were given an infinite penalty.

     This function returns a set of sets: solving all obstacles of one of the
     sets is sufficient to make the variable synthesizable. *)
  val obstacles : variable -> ObstacleSet.t
end

(* [Make(Info)(A)] solve synthesis for the automaton described by [Info], using
   the costs and semantic values described in [A]. *)
module Make
  (Info : Mid.Info.S)
  (A : Recover_attrib.S with module Info := Info)
  : S with module Info := Info
