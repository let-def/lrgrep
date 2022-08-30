open Fix.Indexing

include SetSig.S1 with type 'a t = private IntSet.t
                   and type 'a element = 'a index

module CoerceSum(X : CARDINAL)(Y : CARDINAL) : sig
  val coerce : X.n t -> Sum(X)(Y).n t
end
