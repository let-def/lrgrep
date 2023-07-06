module type S = SetSig.S0
include S with type element = int
val allocate : t ref -> int
