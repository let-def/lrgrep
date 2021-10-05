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

    val cmon : Regular.Expr.t -> Cmon.t
  end
  =
  struct
    type derivations = {
      uid : int;
      table : Regular.Expr.t Reduction.Derivation.derivations;
      non_empty : Reduction.Derivation.Set.t;
    }

    type t =
      | Initial of {
          states: Lr1.Set.t;
          derivations : derivations;
        }
      | Concrete of {
          state : Reduction.Concrete.Set.t;
          derivations : derivations;
        }

    let cmon_lr1_set set =
      Cmon.constant ("#" ^ string_of_int (Lr1.Set.cardinal set))

    let cmon_concrete_set set =
      Cmon.list
        (List.map Cmon.int
           (Reduction.Concrete.Set.elements set :> int list))

    let cmon_derivations set = Cmon.record ["uid", Cmon.int set.uid]

    let cmon re = Mulet.cmon_re
        ~set:(fun set ->
            let tag, set =  match set with
              | Sigma.Pos lr1s -> "Sigma.Pos", lr1s
              | Sigma.Neg lr1s -> "Sigma.Neg", lr1s
            in
            Cmon.constructor tag (cmon_lr1_set set)
          )
        ~label:(fun {Action. accept} ->
            let acc =
              Cmon.list (List.map Cmon.int (BitSet.IntSet.elements accept))
            in
            Cmon.record ["accept", acc]
          )
        ~abstract:(function
            | Initial t -> Cmon.crecord "Initial" [
                "states", cmon_lr1_set t.states;
                "derivations", cmon_derivations t.derivations;
              ]
            | Concrete t -> Cmon.crecord "Concrete" [
                "state", cmon_concrete_set t.state;
                "derivations", cmon_derivations t.derivations;
                "reachable",  (
                  let derivations =
                    Reduction.Derivation.derive
                      ~step:(fun lr1 nts ->
                          match Grammar.(Lr0.incoming (Lr1.lr0 lr1)) with
                          | None -> assert false
                          | Some sym -> Grammar.symbol_name sym :: nts
                        )
                      ~finish:(fun _ nts -> Cmon.list (List.rev_map Cmon.string nts))
                      []
                  in
                  Cmon.list (
                    Reduction.Concrete.Set.fold (fun st acc ->
                        Reduction.Derivation.Set.fold (fun d acc ->
                            Reduction.Derivation.get derivations d :: acc
                          ) (Reduction.Derivation.reached st) acc
                      ) t.state []
                  )
                )
              ]
          )
        re

    let uid = ref 0

    let is_interesting derivations st = not (
        Reduction.Derivation.Set.disjoint derivations.non_empty
          (Reduction.Derivation.reachable st)
      )

    let fold_derived_exprs t acc f =
      match t with
      | Initial _ -> acc
      | Concrete {state; derivations} ->
        Reduction.Concrete.Set.fold (fun st acc ->
            Reduction.Derivation.Set.fold (fun deriv acc ->
                f acc (Reduction.Derivation.get derivations.table deriv)
              ) (Reduction.Derivation.reached st) acc
          ) state acc

    let make expr =
      let delta lr1 expr =
        snd (Regular.Expr.left_delta expr
               (Sigma.Pos (Lr1.Set.singleton lr1)))
      in
      let expr = Regular.Expr.(expr ^. star (set Sigma.full)) in
      let table =
        Reduction.Derivation.derive ~step:delta ~finish:delta expr
      in
      let non_empty =
        let not_empty expr = not (Regular.Expr.is_empty expr) in
        Reduction.Derivation.filter not_empty table
      in
      incr uid;
      let derivations = {table; non_empty; uid = !uid} in
      let states =
        Grammar.Lr1.fold (fun lr1 acc ->
            if is_interesting derivations (Reduction.Concrete.from_lr1 lr1)
            then Lr1.Set.add lr1 acc
            else acc
          ) Lr1.Set.empty
      in
      Regular.Expr.(expr |. abstract (Initial { states; derivations }))

    let compare t1 t2 =
      let n1, n2 =
        match t1, t2 with
        | Initial  _  , Concrete _  -> (-1, 0)
        | Concrete _  , Initial  _  -> (+1, 0)
        | Initial  t1 , Initial  t2 ->
          Lr1.Set.compare t1.states t2.states,
          Int.compare t1.derivations.uid t2.derivations.uid
        | Concrete t1 , Concrete t2 ->
          Reduction.Concrete.Set.compare t1.state t2.state,
          Int.compare t1.derivations.uid t2.derivations.uid
      in
      if n1 = 0 then n2 else n1


    let is_empty _ = false

    let nullable _ = false

    let get_label _ = Action.empty

    let left_classes t f acc =
      match t with
      | Initial t ->
        Lr1.Set.fold (fun lr1 acc ->
            f (Sigma.Pos (Lr1.Set.singleton lr1)) acc
          ) t.states acc
      | Concrete t ->
        Reduction.Concrete.Set.fold (fun st acc ->
            List.fold_left begin fun acc (set, _) ->
              f (Sigma.Pos set) acc
            end acc (Reduction.Concrete.transitions st)
          ) t.state acc

    let left_delta t sigma =
      let t' = match t with
        | Concrete {state; derivations} ->
          let state =
            Reduction.Concrete.Set.fold (fun st acc ->
                List.fold_left
                  (fun acc (x, tgt) ->
                     if Sigma.intersect (Sigma.Pos x) sigma
                     then Reduction.Concrete.Set.add tgt acc
                     else acc)
                  acc
                  (Reduction.Concrete.transitions st)
              ) state
              Reduction.Concrete.Set.empty
          in
          if Reduction.Concrete.Set.exists (is_interesting derivations) state
          then Some (Concrete {state; derivations})
          else None
        | Initial {states; derivations} ->
          let state = Lr1.Set.fold (fun lr1 acc ->
              if Sigma.mem lr1 sigma then
                Reduction.Concrete.Set.add
                  (Reduction.Concrete.from_lr1 lr1) acc
              else acc
            ) states Reduction.Concrete.Set.empty
          in
          Some (Concrete {state; derivations})
      in
      let re = match t' with
        | None -> Regular.Expr.empty
        | Some t' ->
          fold_derived_exprs t' (Regular.Expr.abstract t') Regular.Expr.(|.)
      in
      (Action.empty, re)
  end

  type reduction_operator = Reduction_operator.t

  module SigmaActionMap = Map.Make(struct
      type t = Sigma.t * Action.t
      let compare (s1,a1) (s2,a2) =
        match Sigma.compare s1 s2 with
        | 0 -> Action.compare a1 a2
        | n -> n
    end)

  include Regular

  let simulate_reductions = Reduction_operator.make

  let cmon = Reduction_operator.cmon

  let group_by_outgoing_class dfa =
    Map.fold (fun expr transitions acc ->
        let classe =
          List.fold_left (fun classe (sigma, _, _ : transition) ->
              Sigma.union classe sigma
            ) Sigma.empty transitions,
          Expr.get_label expr
        in
        match SigmaActionMap.find_opt classe acc with
        | Some states ->
          states := (expr, transitions) :: !states;
          acc
        | None ->
          let states = ref [expr, transitions] in
          SigmaActionMap.add classe states acc
      ) dfa SigmaActionMap.empty
  |> SigmaActionMap.map (!)

  let group_transitions ~numbering exprs =
    let number expr : _ Strong.Finite.elt = Map.find expr numbering in
    let prepare_transition source (sigma, _label, target) =
      match number target with
      | exception Not_found -> None
      | target -> Some (Sigma.to_lr1set sigma, (source, target))
    in
    let prepare_state (source, transitions) =
      let source = number source in
      List.filter_map (prepare_transition source) transitions
    in
    List.flatten (List.map prepare_state exprs)

  let refine_transitions trans =
    let partition = Lr1.annotated_partition trans in
    let prepare (sigma, transitions) =
      Array.map
        (fun (source, target) -> (source, sigma, target))
        (Array.of_list transitions)
    in
    Array.concat (List.map prepare partition)

  let group_and_refine_transitions ~numbering exprs =
    refine_transitions (group_transitions ~numbering exprs)

  let is_final expr =
    let action = Expr.get_label expr in
    not (BitSet.IntSet.is_empty action.Action.accept)

  let minimize ~dfa ~initials =
    let (module States) = Strong.Natural.nth (Map.cardinal dfa) in
    let numbering =
      let count = ref 0 in
      Map.map (fun _ ->
          let id = !count in
          incr count;
          Strong.Finite.Elt.of_int States.n id
        ) dfa
    in
    let out_classes = group_by_outgoing_class dfa in
    let transition_map =
      SigmaActionMap.map (group_and_refine_transitions ~numbering) out_classes
    in
    let transitions =
      transition_map
      |> SigmaActionMap.bindings
      |> List.map snd
      |> Array.concat
    in
    let (module Transitions) =
      Strong.Finite.Array.module_of_array transitions in
    let module In = struct
      type states = States.n
      let states = States.n

      type transitions = Transitions.n
      let transitions = Transitions.n

      let initials =
        let initials = List.map (fun x -> Map.find x numbering) initials in
        fun f -> List.iter (fun x -> f x) initials

      let finals =
        let finals = Map.filter (fun expr _id -> is_final expr) numbering in
        fun f -> Map.iter (fun _expr id -> f id) finals

      let source tr =
        let source, _label, _target =
          Strong.Finite.Array.get Transitions.table tr
        in
        source

      let label tr =
        let _source, label, _target =
          Strong.Finite.Array.get Transitions.table tr
        in
        label

      let target tr =
        let _source, _label, target =
          Strong.Finite.Array.get Transitions.table tr
        in
        target

      let refinements ~refine =
        SigmaActionMap.iter (fun (_,action) transitions ->
            if not (BitSet.IntSet.is_empty action.Action.accept) then
              let iter f = Array.iter (fun (src,_,_) -> f src) transitions in
              refine ~iter
          ) transition_map
    end in
    let module V = Valmari.Minimize(Lr1.Set)(In) in
    (module struct
      include V
      type regex = Expr.t
      type sigma = Lr1.Set.t

      let initial = match V.initials with
        | [||] -> None
        | [|x|] -> Some x
        | _ -> assert false

      let transport_state expr =
        V.transport_state (Map.find expr numbering)
    end : Intf.MINIMIZED_DFA with type regex = Expr.t
                              and type sigma = Lr1.Set.t)
end
