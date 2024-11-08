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

  type action =
    | Abort
    | Reduce of Production.t
    | Shift  of Symbol.t
    | Var    of variable

  val cost_of  : variable -> float
  val solution : variable -> float * action list

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

  type 'a interpretation = {
    stuck: 'a;
    const: float -> 'a;
    shift: float -> Symbol.t -> 'a;
    reduce: float -> Production.t -> 'a;
    sequence: 'a -> 'a -> 'a;
    choice: 'a -> 'a -> 'a;
    var: variable -> 'a -> 'a;
  }

  let eval sem = function
    | Goto gt ->
      let {nullable; non_nullable} = goto_candidates gt in
      (* Try some basic syntactic simplifications to ease
         implementation of interpretation and speed-up solver *)
      begin match nullable with
      | Some prod ->
        let cost = cost_of_prod prod in
        let base = sem.reduce cost prod in
        if cost = 0.0 then
          Fun.const base
        else
          fun fix ->
            IndexSet.fold (fun prod x ->
                let src = Transition.(source (of_goto gt)) in
                let var = Tail (src, prod, 0) in
                sem.choice x (sem.var var (fix var))
              ) non_nullable base
      | None ->
        fun fix ->
          IndexSet.fold (fun prod x ->
              let src = Transition.(source (of_goto gt)) in
              let var = Tail (src, prod, 0) in
              sem.choice x (sem.var var (fix var))
            ) non_nullable sem.stuck
      end
    | Tail (st, prod, pos) ->
      begin match tail_candidates st prod pos with
        | Tail_stuck ->
          Fun.const sem.stuck
        | Tail_reduce penalty ->
          Fun.const (sem.reduce penalty prod)
        | Tail_symbol (penalty, sym, st') ->
          let x = sem.shift penalty sym in
          fun fix ->
            let var = Tail (st', prod, pos + 1) in
            sem.sequence x (sem.var var (fix var))
        | Tail_goto (penalty, gt, st') ->
          let x = sem.const penalty in
          fun fix ->
            let y = Goto gt in
            let z = Tail (st', prod, pos + 1) in
            sem.sequence x (sem.sequence (sem.var y (fix y)) (sem.var z (fix z)))
      end

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
    Solver.lfp (eval {
      stuck    = infinity;
      const    = (fun x -> x);
      shift    = (fun x _ -> x);
      reduce   = (fun x _ -> x);
      sequence = (+.);
      choice   = Float.min;
      var      = (fun _ x -> x);
    })

  type action =
    | Abort
    | Reduce of Production.t
    | Shift of Symbol.t
    | Var of variable

  let solution =
    let sem = eval {
        stuck    = (infinity, [Abort]);
        const    = (fun x -> x, if x = infinity then [Abort] else []);
        shift    = (fun x sym -> (x, [Shift sym]));
        reduce   = (fun x prod -> (x, [Reduce prod]));
        sequence = (fun (x1, s1) (x2, s2) -> (x1 +. x2, s1 @ s2));
        choice   = (fun (x1, _ as t1) (x2, _ as t2) -> if x1 <= x2 then t1 else t2);
        var      = (fun _ x -> x);
    } in
    fun var -> sem var (fun var' -> (cost_of var', [Var var']))

  module SymbolsSet = Set.Make(struct
    type t = Symbol.set
    let compare = IndexSet.compare
  end)

  let epsilon = SymbolsSet.singleton IndexSet.empty

  let add t ts =
    if IndexSet.is_empty t
    then ts
    else SymbolsSet.map (IndexSet.union t) ts

  let join t1 t2 =
    if SymbolsSet.is_empty t1 || SymbolsSet.is_empty t2 then
      SymbolsSet.empty
    else
      let s1 = SymbolsSet.choose t1 in
      if SymbolsSet.equal t1 (SymbolsSet.singleton s1) then
        add s1 t2
      else
        let s2 = SymbolsSet.choose t2 in
        if SymbolsSet.equal t2 (SymbolsSet.singleton s2) then
          add s2 t1
        else
          SymbolsSet.fold (fun s1 acc ->
              SymbolsSet.fold (fun s2 acc ->
                  SymbolsSet.add (IndexSet.union s1 s2) acc
                ) t2 acc
            ) t1 SymbolsSet.empty

  let union t1 t2 =
    if SymbolsSet.equal t1 epsilon || SymbolsSet.equal t2 epsilon
    then epsilon
    else SymbolsSet.union t1 t2

  let minimal_placeholders =
    let module Solver = Fix.Make(Table)(struct
        type property = SymbolsSet.t
        let bottom = SymbolsSet.empty
        let equal = SymbolsSet.equal
        let is_maximal _ = false
      end)
    in
    Solver.lfp (eval {
      stuck    = SymbolsSet.empty;
      const    = (fun x -> if x = infinity then SymbolsSet.empty else epsilon);
      shift    = (fun x sym ->
          if x = infinity then SymbolsSet.singleton (IndexSet.singleton sym)
          else epsilon
        );
      reduce   = (fun x _prod -> if x = infinity then SymbolsSet.empty else epsilon);
      sequence = join;
      choice   = union;
      var      = (fun var x -> match var with
          | Goto gt when cost_of var = infinity ->
            add (IndexSet.singleton (Symbol.inj_r (Transition.goto_symbol gt))) x
          | _ -> x);
    })
end
