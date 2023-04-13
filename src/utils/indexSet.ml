open Fix.Indexing

module F (X : Index.Unsafe.T) = struct
  module type S = sig
    include SetSig.S1 with type 'a t = IntSet.t
                       and type 'a element = 'a X.t

    val unsafe_to_indexset : 'a t -> 'b t
  end
end

module FIntSet : F(Index.Unsafe.Int).S =
struct
  type _element = int
  type _t = IntSet.t

  type 'a element = _element
  type 'a t = _t

  include (IntSet : SetSig.S0 with type element := _element and type t := _t)

  let unsafe_to_indexset x = x
end

module IndexSet = Index.Unsafe.Coerce(F)(FIntSet)

include IndexSet

module CoerceSum(X : CARDINAL)(Y : CARDINAL) = struct
  let coerce : X.n t -> Sum(X)(Y).n t = unsafe_to_indexset
end

let init_from_set c f =
  match Fix.Indexing.cardinal c with
  | 0 -> empty
  | n -> init_subset (Index.of_int c 0) (Index.of_int c (n - 1)) f


module FF (X : Index.Unsafe.T) = struct
  module type S = sig
    include SetSig.StdSetS1 with type 'a t = IntSetSet.t
                             and type 'a elt = 'a X.t IndexSet.t
  end
end

module FIntSetSet : FF(Index.Unsafe.Int).S =
struct
  type _elt = IntSet.t
  type _t = IntSetSet.t

  type 'a elt = _elt
  type 'a t = _t

  include (IntSetSet: Set.S with type elt := _elt and type t := _t)
end

module Set = Index.Unsafe.Coerce(FF)(FIntSetSet)
