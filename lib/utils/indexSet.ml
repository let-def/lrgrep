open Fix.Indexing

module F (X : Index.Unsafe.T) = struct
  module type S = sig
    include SetSig.S1 with type 'a t = IntSet.t
                       and type 'a element = 'a X.t

    val unsafe_to_indexset : 'a t -> 'b t
  end
end

module IntSet : F(Index.Unsafe.Int).S =
struct
  type _element = int
  type _t = IntSet.t

  type 'a element = _element
  type 'a t = _t

  include (IntSet : SetSig.S0 with type element := _element and type t := _t)

  let unsafe_to_indexset x = x
end

include Index.Unsafe.Coerce(F)(IntSet)

module CoerceSum(X : CARDINAL)(Y : CARDINAL) = struct
  let coerce : X.n t -> Sum(X)(Y).n t = unsafe_to_indexset
end
