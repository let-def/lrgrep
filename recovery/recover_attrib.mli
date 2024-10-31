module type S = sig
  module Info : Mid.Info.S
  open Info

  val cost_of_prod    : Production.t -> float
  val penalty_of_item : Production.t * int -> float
  val cost_of_symbol  : Symbol.t -> float

  val default_prelude     : Format.formatter -> unit
  val default_terminal    : Terminal.t -> string option
  val default_nonterminal : Nonterminal.t -> string option
end

module Make (Info : Mid.Info.S) : S with module Info := Info
