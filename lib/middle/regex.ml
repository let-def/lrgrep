open Utils

module Make
    (Sigma : Intf.SIGMA)
    (Reduction : Intf.REDUCTION with module Lr1 = Sigma.Lr1)
  : Intf.REGEX
    with module Sigma = Sigma
     and module Reduction = Reduction
  =
struct
  module Sigma = Sigma
  module Reduction = Reduction
  module Lr1 = Sigma.Lr1
  module Grammar = Lr1.Grammar

  module Action = struct
    open BitSet

    type t = {
      accept: IntSet.t;
      (*store: IntSet.t;*)
    }

    let empty = {
      accept = IntSet.empty;
      (*store = IntSet.empty;*)
    }

    let compare t1 t2 =
      let c = IntSet.compare t1.accept t2.accept in
      c
      (*match c with
      | 0 -> IntSet.compare t1.store t2.store
      | n -> n*)

    let append t1 t2 = {
      accept = IntSet.union t1.accept t2.accept;
      (*store = IntSet.union t1.store t2.store;*)
    }

    let accept i = { accept = IntSet.singleton i (*; store = IntSet.empty*) }
  end

  module rec Regular : Mulet.S
    with type sigma = Sigma.t
     and type label = Action.t
     and type abstract = Reduction_operator.t
    = Mulet.Make(Sigma)(Action)(Reduction_operator)

  and Reduction_operator : sig
    include Mulet.DERIVABLE
      with type sigma := Sigma.t
       and type label := Action.t

    val make : Regular.Expr.t -> t
  end
  =
  struct
    type derivations = {
      uid : int;
      table : Regular.Expr.t Reduction.Derivation.derivations;
      non_empty : Reduction.Derivation.Set.t;
      nullable : Reduction.Derivation.Set.t;
    }

    type t = { state: Reduction.Concrete.Set.t; derivations : derivations }

    let initial =
      Grammar.Lr1.fold (fun lr1 acc ->
          Reduction.Concrete.Set.add (Reduction.Concrete.from_lr1 lr1) acc
        ) Reduction.Concrete.Set.empty

    let uid = ref 0

    let make expr =
      let singleton lr1 = Sigma.Pos (Lr1.Set.singleton lr1) in
      let table = Reduction.Derivation.derive
          ~step:begin fun nt expr ->
            let sigma = Lr1.by_incoming_symbol (Grammar.N nt) in
            let _, expr = Regular.Expr.left_delta expr (Sigma.Pos sigma) in
            expr
          end
          ~finish:begin fun lr1 expr ->
            let _, expr = Regular.Expr.left_delta expr (singleton lr1) in
            expr
          end
          expr
      in
      let non_empty =
        let not_empty expr = not (Regular.Expr.is_empty expr) in
        Reduction.Derivation.filter not_empty table
      in
      let nullable =
        Reduction.Derivation.filter Regular.Expr.nullable table
      in
      incr uid;
      {
        state = initial;
        derivations = {table; non_empty; nullable; uid = !uid};
      }

    let compare t1 t2 =
      match Reduction.Concrete.Set.compare t1.state t2.state with
      | 0 -> Int.compare t1.derivations.uid t2.derivations.uid
      | n -> n

    let is_empty _ = false

    let nullable t =
      Reduction.Concrete.Set.exists (fun st ->
          not (Reduction.Derivation.Set.disjoint
                 t.derivations.nullable
                 (Reduction.Derivation.reached st))
        ) t.state

    let fold_derived_exprs t acc f =
      Reduction.Concrete.Set.fold (fun st acc ->
          Reduction.Derivation.Set.fold (fun deriv acc ->
              f acc (Reduction.Derivation.get t.derivations.table deriv)
            ) (Reduction.Derivation.reached st) acc
        ) t.state acc

    let get_label t =
      fold_derived_exprs t Action.empty @@ fun action expr ->
      Action.append action (Regular.Expr.get_label expr)

    let left_classes t f acc =
      Reduction.Concrete.Set.fold (fun st acc ->
          List.fold_left begin fun acc (set, _) ->
            f (Sigma.Pos set) acc
          end acc (Reduction.Concrete.transitions st)
        ) t.state acc

    let left_delta t sigma =
      let state =
        Reduction.Concrete.Set.fold (fun st acc ->
            List.fold_left
              (fun acc (x, tgt) ->
                 if Sigma.intersect (Sigma.Pos x) sigma
                 then Reduction.Concrete.Set.add tgt acc
                 else acc)
              acc
              (Reduction.Concrete.transitions st)
          ) t.state
          Reduction.Concrete.Set.empty
      in
      if Reduction.Concrete.Set.exists (fun st ->
          not (Reduction.Derivation.Set.disjoint
                 t.derivations.non_empty
                 (Reduction.Derivation.reachable st))
        ) state
      then
        let t = {t with state} in
        let re = Regular.Expr.abstract t in
        let re = fold_derived_exprs t re Regular.Expr.(|.) in
        Action.empty, re
      else Action.empty, Regular.Expr.empty
  end

  type reduction_operator = Reduction_operator.t

  include Regular

  let simulate_reductions expr =
    Expr.abstract (Reduction_operator.make expr)
end
