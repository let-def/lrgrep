open Fix.Indexing
open Utils
open Misc

module type S = sig
  module Info : Mid.Info.S
  open Info

  type variable =
    | Goto of Transition.goto index
    | Tail of Lr1.t * Production.t * int

  val variable_to_string : variable -> string

  type 'a paction =
    | Abort
    | Reduce of Production.t
    | Shift  of Symbol.t
    | Var    of 'a

  val paction_to_string : ('a -> string) -> 'a paction -> string

  type action = variable paction

  val action_to_string : action -> string

  val cost_of  : variable -> float
  val cost_of_action  : action -> float
  val cost_of_actions : action list -> float
  val solution : variable -> action list
  val report   : Format.formatter -> unit
end

module Make
    (Info : Mid.Info.S)
    (A : Recover_attrib.S with module Info := Info)
  : S with module Info := Info =
struct
  open Info

  type variable =
    | Goto of Transition.goto index
    | Tail of Lr1.t * Production.t * int

  let decompose_goto gt =
    let tr = Transition.of_goto gt in
    (Transition.source tr,
     Transition.goto_symbol gt,
     Transition.target tr)

  let variable_to_string = function
    | Goto gt ->
      let st, n, st' = decompose_goto gt in
      Printf.sprintf "Goto (#%d-%s->#%d)"
        (Index.to_int st) (Nonterminal.to_string n) (Index.to_int st')
    | Tail (st, prod, pos) ->
      Printf.sprintf "Tail (#%d, p%d, %d)"
        (Index.to_int st) (Index.to_int prod) pos

  type 'a paction =
    | Abort
    | Reduce of Production.t
    | Shift  of Symbol.t
    | Var    of 'a

  let paction_to_string variable_to_string = function
    | Abort -> "Abort"
    | Reduce prod -> "Reduce p" ^ string_of_int (Index.to_int prod)
    | Shift  sym -> "Shift " ^ (Symbol.name ~mangled:true sym)
    | Var v -> "Var (" ^ variable_to_string v ^ ")"

  type action = variable paction

  let action_to_string = paction_to_string variable_to_string

  let check_cost r =
    assert (r >= 0.); r

  let cost_of_prod p    = check_cost (A.cost_of_prod p)
  let cost_of_symbol s  = check_cost (A.cost_of_symbol s)
  let penalty_of_item i = check_cost (A.penalty_of_item i)

  let app var v = v var

  let var var = match var with
    | Goto _ -> app var
    | Tail (_,prod,pos) ->
      if pos < Production.length prod then
        app var
      else
        Fun.const (cost_of_prod prod)

  let cost_of = function
    | Goto gt ->
      let src, nt, tgt = decompose_goto gt in
      let acc =
        List.filter_map (fun (prod, pos) ->
            if pos = 1 && Production.lhs prod = nt
            then Some (var (Tail (src, prod, 0)))
            else None
          ) (Lr1.items tgt)
      in
      let cost =
        let visit_reduction red acc =
          let prod = Reduction.production red in
          if Production.length prod = 0 && Production.lhs prod = nt then
            Float.min (cost_of_prod prod) acc
          else acc
        in
        IndexSet.fold visit_reduction (Reduction.from_lr1 src) infinity
      in
      if cost < infinity || acc <> [] then
        (fun v -> List.fold_left (fun cost f -> Float.min cost (f v)) cost acc)
      else Fun.const infinity

    | Tail (st, prod, pos) ->
      let penalty = penalty_of_item (prod, pos) in
      if penalty = infinity then
        Fun.const infinity
      else
      if pos >= Array.length (Production.rhs prod) then
        Fun.const (cost_of_prod prod)
      else
        let head =
          let sym = (Production.rhs prod).(pos) in
          let cost = cost_of_symbol sym in
          if cost < infinity then Fun.const cost
          else match Symbol.desc sym with
            | Symbol.T _ -> Fun.const infinity
            | Symbol.N nt -> var (Goto (Transition.find_goto st nt))
        in
        let tail =
          let sym = (Production.rhs prod).(pos) in
          match
            List.find
              (fun tr -> Index.compare sym (Transition.symbol tr) = 0)
              (Transition.successors st)
          with
          | tr -> var (Tail (Transition.target tr, prod, pos + 1))
          | exception Not_found ->
            Printf.eprintf "no transition: #%d (%d,%d)\n" (Index.to_int st) (Index.to_int prod) pos;
            Fun.const infinity
        in
        (fun v -> head v +. tail v)

  let cost_of =
    let module Solver = Fix.Make (struct
        type key = variable
        type 'a t = (key, 'a) Hashtbl.t
        let create () = Hashtbl.create 7
        let find k tbl = Hashtbl.find tbl k
        let add k v tbl = Hashtbl.add tbl k v
        let iter f tbl = Hashtbl.iter f tbl
        let clear = Hashtbl.clear
      end) (struct
        type property = float
        let bottom = infinity
        let equal : float -> float -> bool = (=)
        let is_maximal f = f = 0.0
      end)
    in
    Solver.lfp cost_of

  let cost_of_action = function
    | Abort    -> infinity
    | Reduce p -> cost_of_prod p
    | Shift s  -> cost_of_symbol s
    | Var v    -> cost_of v

  let select var1 var2 =
    if cost_of_action var1 <= cost_of_action var2
    then var1
    else var2

  let cost_of_actions actions =
    List.fold_left (fun cost act -> cost +. cost_of_action act) 0.0 actions

  let solution = function
    | Goto gt ->
      let src, nt, tgt = decompose_goto gt in
      let acc =
        List.fold_left (fun acc (prod, pos) ->
            if pos = 1 && Production.lhs prod = nt then
              select (Var (Tail (src, prod, 0))) acc
            else acc
          ) Abort (Lr1.items tgt)
      in
      let visit_reduction red acc =
        let prod = Reduction.production red in
        if Production.length prod = 0 && Production.lhs prod = nt
        then select (Reduce prod) acc
        else acc
      in
      let acc = IndexSet.fold visit_reduction (Reduction.from_lr1 src) acc in
      [acc]

    | Tail (_st, prod, pos) when pos = Array.length (Production.rhs prod) ->
      [Reduce prod]

    | Tail (st, prod, pos) ->
      let penalty = penalty_of_item (prod, pos) in
      if penalty = infinity then
        [Abort]
      else
        let head =
          let sym = (Production.rhs prod).(pos) in
          let cost = cost_of_symbol sym in
          if cost < infinity then
            Shift sym
          else match Symbol.desc sym with
            | T _ -> Abort
            | N nt -> Var (Goto (Transition.find_goto st nt))
        in
        let tail =
          let sym = (Production.rhs prod).(pos) in
          match
            List.find
              (fun tr -> Index.compare sym (Transition.symbol tr) = 0)
              (Transition.successors st)
          with
          | tr -> Var (Tail (Transition.target tr, prod, pos + 1))
          | exception Not_found ->
            Printf.eprintf "no transition: #%d (%d,%d)\n" (Index.to_int st) (Index.to_int prod) pos;
            Abort
        in
        [head; tail]

  let report ppf =
    let open Format in
    let solutions = ref [] in
    let group_assoc l =
      group_by l
        ~compare:(compare_fst compare)
        ~group:(fun (k, v) vs -> (k, v :: List.map snd vs))
    in
    Index.iter Lr1.n (fun st ->
        match List.fold_left (fun (item, cost) (prod, pos) ->
            let cost' = cost_of (Tail (st, prod, pos)) in
            let actions = solution (Tail (st, prod, pos)) in
            assert (cost' = cost_of_actions actions);
            if cost' < cost then (Some (prod, pos), cost') else (item, cost)
          ) (None, infinity) (Lr1.items st)
        with
        | None, _ ->
          fprintf ppf "no synthesis from %d\n" (Index.to_int st);
        | Some item, cost ->
          push solutions (item, (cost, st))
      );
    List.iter (fun ((prod, pos), states) ->
        fprintf ppf "# Item (%d,%d)\n" (Index.to_int prod) pos;
        let rhs = Production.rhs prod in
        fprintf ppf "%s:" (Nonterminal.to_string (Production.lhs prod));
        for i = 0 to pos - 1 do
          fprintf ppf " %s" (Symbol.name rhs.(i));
        done;
        fprintf ppf " .";
        for i = pos to Array.length rhs - 1 do
          fprintf ppf " %s" (Symbol.name rhs.(i));
        done;
        List.iter (fun (cost, states) ->
            fprintf ppf "at cost %f from states %s\n\n"
              cost (string_concat_map ", " string_of_index states)
          )
          (group_assoc states)
      ) (group_assoc !solutions)
end
