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

    val make : Regular.Expr.t -> Regular.Expr.t
  end
  =
  struct
    type derivations = {
      uid : int;
      table : Regular.Expr.t Reduction.Derivation.derivations;
      non_empty : Reduction.Derivation.Set.t;
    }

    type t = { state: Reduction.Concrete.Set.t; derivations : derivations }

    let uid = ref 0

    let is_interesting derivations st = not (
        Reduction.Derivation.Set.disjoint derivations.non_empty
          (Reduction.Derivation.reachable st)
      )

    let fold_derived_exprs t acc f =
      Reduction.Concrete.Set.fold (fun st acc ->
          Reduction.Derivation.Set.fold (fun deriv acc ->
              f acc (Reduction.Derivation.get t.derivations.table deriv)
            ) (Reduction.Derivation.reached st) acc
        ) t.state acc

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
      incr uid;
      let derivations = {table; non_empty; uid = !uid} in
      let state =
        Grammar.Lr1.fold (fun lr1 acc ->
            let state = Reduction.Concrete.from_lr1 lr1 in
            if is_interesting derivations state
            then Reduction.Concrete.Set.add state acc
            else acc
          ) Reduction.Concrete.Set.empty
      in
      Regular.Expr.(expr |. abstract { state; derivations })

    let compare t1 t2 =
      match Int.compare t1.derivations.uid t2.derivations.uid with
      | 0 -> Reduction.Concrete.Set.compare t1.state t2.state
      | n -> n

    let is_empty _ = false

    let nullable _ = false

    let get_label _ = Action.empty

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
      if Reduction.Concrete.Set.exists (is_interesting t.derivations) state
      then
        let t = {t with state} in
        let re = Regular.Expr.abstract t in
        let re = fold_derived_exprs t re Regular.Expr.(|.) in
        Action.empty, re
      else Action.empty, Regular.Expr.empty
  end

  type reduction_operator = Reduction_operator.t

  include Regular

  let simulate_reductions = Reduction_operator.make
end
