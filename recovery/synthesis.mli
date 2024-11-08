open Fix.Indexing

module type S = sig
  module Info : Mid.Info.S
  open Info

  type variable =
    | Goto of Transition.goto index
    | Tail of Lr1.t * Production.t * int

  val variable_to_string : variable -> string

  type action =
    | Abort
    | Reduce of Production.t
    | Shift  of Symbol.t
    | Var    of variable

  val cost_of  : variable -> float
  val solution : variable -> float * action list

  module SymbolsSet : Set.S with type elt = Symbol.set
  val minimal_placeholders : variable -> SymbolsSet.t
end

module Make
  (Info : Mid.Info.S)
  (A : Recover_attrib.S with module Info := Info)
  : S with module Info := Info
