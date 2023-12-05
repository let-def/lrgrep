open Fix.Indexing

include SetSig.S1 with type 'a t = private IntSet.t
                   and type 'a element = 'a index

val unsafe_to_indexset : 'a t -> 'b t
val unsafe_of_intset : IntSet.t -> 'a t

module CoerceSum(X : CARDINAL)(Y : CARDINAL) : sig
  val coerce : X.n t -> Sum(X)(Y).n t
end

val init_from_set : 'a cardinal -> ('a index -> bool) -> 'a t

module Set : SetSig.StdSetS1 with type 'a t = private IntSetSet.t
                              and type 'a elt = 'a t

module Map : SetSig.StdMapS1 with type ('n, 'a) t = private 'a IntSetMap.t
                              and type 'n key = 'n t
