type clause = int
type var = int
type register = clause * var
type arities = int array

module Registers : sig
  type 'a bank
  val make_bank : arities -> 'a bank
  val store : 'a bank -> register -> 'a -> unit

  type 'a t
  val get : 'a bank -> clause -> 'a option array
end = struct
  type 'a bank = 'a option array array

  let make_bank clauses =
    Array.map (fun arity -> Array.make arity None) clauses

  let store bank (clause, var) value =
    bank.(clause).(var) <- Some value

  type 'a t = 'a array
  let get bank clause =
    Array.copy bank.(clause)
end
