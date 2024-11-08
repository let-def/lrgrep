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

  module SymbolsSet : Set.S with type elt = Symbol.set
  val minimal_placeholders : variable -> SymbolsSet.t
end

module Make
    (Info : Mid.Info.S)
    (A : Recover_attrib.S with module Info := Info)
  : S with module Info := Info =
struct
  open Info

  let check_cost r =
    assert (r >= 0.); r

  let cost_of_prod p    = check_cost (A.cost_of_prod p)
  let cost_of_symbol s  = check_cost (A.cost_of_symbol s)
  let penalty_of_item i = check_cost (A.penalty_of_item i)

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

  type goto_candidates = {
    (* List the different actions that result in following a goto transition *)

    (* Non-nullable productions that would amount to follow this transition *)
    non_nullable: Production.set;
    nullable: Production.t option;
  }

  let goto_candidates gt =
    let src, nt, _ = decompose_goto gt in
    let non_nullable =
      IndexSet.of_list (
        List.concat_map (fun tr ->
            List.filter_map (fun (prod, pos) ->
                if pos = 1 && Production.lhs prod = nt
                then Some prod
                else None
              ) (Lr1.items (Transition.target tr))
          ) (Transition.successors src)
      )
    in
    let nullable =
      let visit_reduction red acc =
        let prod = Reduction.production red in
        if Production.length prod = 0 && Production.lhs prod = nt then
          match acc with
          | None -> Some prod
          | Some prod' when cost_of_prod prod < cost_of_prod prod' -> Some prod
          | _ -> acc
        else acc
      in
      IndexSet.fold visit_reduction (Reduction.from_lr1 src) None
    in
    { nullable; non_nullable }

  type tail_solution =
    | Tail_reduce of float
    | Tail_symbol of float * Symbol.t * Lr1.t
    | Tail_goto of float * Transition.goto index * Lr1.t
    | Tail_stuck

  let tail_candidates st prod pos =
    let penalty = penalty_of_item (prod, pos) in
    if penalty = infinity then
      Tail_stuck
    else if pos = Array.length (Production.rhs prod) then
      let cost = penalty +. cost_of_prod prod in
      if cost = infinity then
        Tail_stuck
      else
      Tail_reduce cost
    else
      let sym = (Production.rhs prod).(pos) in
      match
        List.find
          (fun tr -> Index.compare sym (Transition.symbol tr) = 0)
          (Transition.successors st)
      with
      (* A missing transition has been removed by conflict resolution *)
      | exception Not_found ->
        if false then (
          let lhs = Production.lhs prod in
          let rhs = Production.rhs prod in
          Printf.eprintf "no transition in #%d for %s:"
            (Index.to_int st) (Nonterminal.to_string lhs);
          for i = 0 to pos - 1 do
            prerr_char ' ';
            prerr_string (Symbol.name rhs.(i));
          done;
          prerr_string " .";
          for i = pos to Array.length rhs - 1 do
            prerr_char ' ';
            prerr_string (Symbol.name rhs.(i));
          done;
          prerr_char '\n';
        );
        Tail_stuck
      | tr ->
        let cost = cost_of_symbol sym in
        if cost < infinity then
          Tail_symbol (penalty +. cost, sym, Transition.target tr)
        else match Transition.split tr with
          | L gt -> Tail_goto (penalty, gt, Transition.target tr)
          | R _ -> Tail_stuck

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

  let cost_of = function
    | Goto gt ->
      let {nullable; non_nullable} = goto_candidates gt in
      let base_cost = match nullable with
        | Some prod -> cost_of_prod prod
        | None -> infinity
      in
      if base_cost = 0.0 then
        Fun.const 0.0
      else
        fun solution ->
          IndexSet.fold (fun prod cost ->
              let src = Transition.(source (of_goto gt)) in
              let var = Tail (src, prod, 0) in
              Float.min cost (solution var)
            ) non_nullable base_cost
    | Tail (st, prod, pos) ->
      match tail_candidates st prod pos with
      | Tail_stuck -> Fun.const infinity
      | Tail_reduce penalty -> Fun.const penalty
      | Tail_symbol (penalty, _, st') ->
        fun var -> (penalty +. var (Tail (st', prod, pos + 1)))
      | Tail_goto (penalty, gt, st') ->
        fun var -> (penalty +. var (Goto gt) +. var (Tail (st', prod, pos + 1)))

  module Table = struct
    type key = variable
    type 'a t = (key, 'a) Hashtbl.t
    let create () = Hashtbl.create 7
    let find k tbl = Hashtbl.find tbl k
    let add k v tbl = Hashtbl.add tbl k v
    let iter f tbl = Hashtbl.iter f tbl
    let clear = Hashtbl.clear
  end

  let cost_of =
    let module Solver = Fix.Make(Table)(struct
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
      let src, nt, _ = decompose_goto gt in
      let acc =
        List.fold_left (fun acc tr ->
            List.fold_left (fun acc (prod, pos) ->
                if pos = 1 && Production.lhs prod = nt then
                  select (Var (Tail (src, prod, 0))) acc
                else acc
              ) acc (Lr1.items (Transition.target tr))
          ) Abort (Transition.successors src)
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

  module SymbolsSet = Set.Make(struct
    type t = Symbol.set
    let compare = IndexSet.compare
  end)

  let epsilon = SymbolsSet.singleton IndexSet.empty

  let add t ts = SymbolsSet.map (IndexSet.add t) ts

  let join t1 t2 =
    if SymbolsSet.is_empty t1 || SymbolsSet.is_empty t2 then
      SymbolsSet.empty
    else if SymbolsSet.equal t1 epsilon then
      t2
    else if SymbolsSet.equal t2 epsilon then
      t1
    else
      SymbolsSet.fold (fun s1 acc ->
          SymbolsSet.fold (fun s2 acc ->
              SymbolsSet.add (IndexSet.union s1 s2) acc
            ) t2 acc
        ) t1 SymbolsSet.empty

  let minimal_placeholders =
    let module Solver = Fix.Make(Table)(struct
        type property = SymbolsSet.t
        let bottom = SymbolsSet.empty
        let equal = SymbolsSet.equal
        let is_maximal _ = false
      end)
    in
    Solver.lfp (fun var ->
        if cost_of var < infinity then
          fun _ -> epsilon
        else
          match var with
          | Goto gt ->
            (fun vals ->
               let src, nt, _ = decompose_goto gt in
               let null_reduction red =
                 let prod = Reduction.production red in
                 Production.length prod = 0 &&
                 Production.lhs prod = nt
               in
               if IndexSet.exists null_reduction (Reduction.from_lr1 src) then
                 epsilon
               else
                 let acc =
                   List.fold_left (fun acc tr ->
                       List.fold_left (fun acc (prod, pos) ->
                           if SymbolsSet.mem IndexSet.empty acc then
                             epsilon
                           else if pos = 1 && Production.lhs prod = nt then
                             SymbolsSet.union acc (vals (Tail (src, prod, 0)))
                           else acc
                         ) acc (Lr1.items (Transition.target tr))
                     ) SymbolsSet.empty (Transition.successors src)
                 in
                 if SymbolsSet.mem IndexSet.empty acc
                 then epsilon
                 else acc
            )
          | Tail (lr1, prod, pos) ->
            if A.penalty_of_item (prod, pos) = infinity then
              (fun _ -> SymbolsSet.empty)
            else if pos = Production.length prod then
              if A.cost_of_prod prod = infinity then
                (fun _ -> SymbolsSet.empty)
              else
                (fun _ -> epsilon)
            else (
              let sym = (Production.rhs prod).(pos) in
              match
                List.find_opt
                  (fun tr -> Transition.symbol tr = sym)
                  (Transition.successors lr1)
              with
              | None -> (fun _ -> SymbolsSet.empty)
              | Some tr ->
                let lr1' = Transition.target tr in
                match Transition.split tr with
                | L gt ->
                  (fun vals ->
                   let hd = vals (Goto gt) in
                   let tl = vals (Tail (lr1', prod, pos + 1)) in
                   if SymbolsSet.is_empty hd then
                     SymbolsSet.empty
                   else if SymbolsSet.equal hd epsilon then
                     tl
                   else
                     SymbolsSet.union
                       (add (Symbol.inj_r (Transition.goto_symbol gt)) tl)
                       (join hd tl))
                | R sh ->
                  if A.cost_of_symbol sym = infinity then
                    let t = Transition.shift_symbol sh in
                    (fun vals -> add (Symbol.inj_l t) (vals (Tail (lr1', prod, pos + 1))))
                  else
                    (fun vals -> vals (Tail (lr1', prod, pos + 1)))
            )
      )
end
