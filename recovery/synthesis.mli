open Fix.Indexing

module type S = sig
  module Info : Mid.Info.S
  open Info

  type variable =
    | Goto of Transition.goto index
    | Tail of Lr1.t * Production.t * int

  val variable_to_string : variable -> string

  type 'a paction =
    | Abort
    | Reduce of Production.t
    | Shift  of Symbol.t
    | Var    of 'a

  val paction_to_string : ('a -> string) -> 'a paction -> string

  type action = variable paction

  val action_to_string : action -> string

  val cost_of  : variable -> float
  val cost_of_action  : action -> float
  val cost_of_actions : action list -> float
  val solution : variable -> action list
  val report   : Format.formatter -> unit

  module SymbolsSet : Set.S with type elt = Symbol.set
  val minimal_placeholders : variable -> SymbolsSet.t
end

module Make
  (Info : Mid.Info.S)
  (A : Recover_attrib.S with module Info := Info)
  : S with module Info := Info
