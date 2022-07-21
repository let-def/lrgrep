open Grammar

module type DERIVABLE = sig
  type t
  val derive : t -> t dfa_transition list
  val merge : t list -> t
  val compare : t -> t -> int
  val cmon : t -> Cmon.t
end

module Cache (D : DERIVABLE) : sig
  include DERIVABLE
  val lift : D.t -> t
  val unlift : t -> D.t
end

module Make (D : DERIVABLE) :
sig
  type t
  type transitions = D.t dfa_transition list * t dfa_transition list

  type compilation_cache
  val make_compilation_cache : unit -> compilation_cache

  type compilation
  val compile : compilation_cache -> D.t -> compilation
  val cmon : compilation -> Cmon.t

  val initial : compilation -> transitions
  val derive : t -> transitions
  val compare : t -> t -> int
end
