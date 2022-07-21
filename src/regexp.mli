module RE : sig
  type uid = private int

  type var = int * int

  type t = { uid : uid; desc : desc; position : Syntax.position; }

  and desc =
    | Set of Grammar.Lr1.set * var option
    | Alt of t list
    | Seq of t list
    | Star of t
    | Reduce

  val make : Syntax.position -> desc -> t
  val compare : t -> t -> int
  val cmon : ?var:(var -> Cmon.t) -> t -> Cmon.t
end

module KRE : sig
  type t =
    | Done of { clause : int }
    | More of RE.t * t

  val cmon : t -> Cmon.t
  val more : RE.t -> t -> t
  val compare : t -> t -> int
end

module KRESet : sig
  include Set.S with type elt = KRE.t

  val prederive :
    visited:t ref ->
    reached:int list ref ->
    direct:(Grammar.Lr1.set * RE.var list * KRE.t) list ref ->
    reduce:KRE.t list ref -> elt -> unit

  val derive_reduce : t -> t Grammar.dfa_transition list
  val cmon : t -> Cmon.t
end
