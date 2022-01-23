open Utils

module Intersect
    (DFA: Middle.Intf.MINIMIZED_DFA)
    (Property: sig
       type t
       val empty : t
       val is_empty : t -> bool

       val initial : t
       val reachable : t -> DFA.sigma
       val derive : t -> DFA.sigma -> t
       val leq_join : t -> t -> t
     end) :
sig
  val properties : (DFA.states, Property.t) Strong.Finite.Array.t
end =
struct
  let properties =
    Strong.Finite.Array.make
      DFA.states
      Property.empty

  let () =
    Array.iter (fun st ->
        Strong.Finite.Array.set
          properties st Property.initial
      ) DFA.initial
end
