open Fix.Indexing
open Utils
open Misc

module type S = sig
  module Info : Mid.Info.S
  open Info

  type variable =
    | Goto of Transition.goto index
    | Tail of Lr1.t * Item.t

  val variable_to_string : variable -> string

  val goto_candidates : Transition.goto index -> Production.set

  type tail_solution =
    | Tail_reduce of float
    | Tail_follow of float * Transition.any index * Item.t

  val tail_candidates : Lr1.t -> Item.t -> float * tail_solution

  val cost_of  : variable -> float

  type action =
    | Abort
    | Reduce of Production.t
    | Shift  of Symbol.t
    | Var    of variable

  val action : variable -> float * action list

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
    | Tail of Lr1.t * Item.t

  let variable_to_string = function
    | Goto gt ->
      let tr = Transition.of_goto gt in
      Printf.sprintf "Goto (#%d-%s->#%d)"
        (Index.to_int (Transition.source tr))
        (Nonterminal.to_string (Transition.goto_symbol gt) )
        (Index.to_int (Transition.target tr))
    | Tail (st, item) ->
      let prod, pos = Item.prj item in
      Printf.sprintf "Tail (#%d, p%d, %d)"
        (Index.to_int st) (Index.to_int prod) pos

  let goto_candidates gt =
    let src, n = Transition.(source (of_goto gt), goto_symbol gt) in
    let rec find_prods prods = function
      | (item, _la) :: rest when Item.position item = 0 ->
        let prod = Item.production item in
        let prods =
          if Production.lhs prod = n
          then IndexSet.add prod prods
          else prods
        in
        find_prods prods rest
      | _ -> prods
    in
    find_prods IndexSet.empty (Lr1.effective_items src)

  type tail_solution =
    | Tail_reduce of float
    | Tail_follow of float * Transition.any index * Item.t

  let tail_candidates st item =
    let penalty = penalty_of_item item in
    let prod, pos = Item.prj item in
    let rhs = Production.rhs prod in
    if pos = Array.length rhs then
      (penalty, Tail_reduce (cost_of_prod prod))
    else
      let sym = rhs.(pos) in
      match
        List.find
          (fun tr -> Index.compare sym (Transition.symbol tr) = 0)
          (Transition.successors st)
      with
      (* No more missing transitions: effective items should prevent
       * attempting to reduce a missing transition *)
      | exception Not_found -> assert false
      | tr ->
        (penalty, Tail_follow (cost_of_symbol sym, tr, Item.inj prod (pos + 1)))

  type 'a interpretation = {
    stuck: 'a;
    follow: float -> Transition.any index -> 'a;
    reduce: float -> Production.t -> 'a;
    sequence: 'a -> 'a -> 'a;
    choice: 'a -> 'a -> 'a;
    penalty: float -> 'a -> 'a;
  }

  let eval sem = function
    | Goto gt ->
      let src = Transition.(source (of_goto gt)) in
      let prods = goto_candidates gt in
      fun fix ->
        IndexSet.fold (fun prod x -> sem.choice x (fix (Tail (src, Item.inj prod 0)))) prods sem.stuck
    | Tail (st, item) ->
      match tail_candidates st item with
      | penalty, Tail_reduce cost ->
        Fun.const (sem.penalty penalty (sem.reduce cost (Item.production item)))
      | penalty, Tail_follow (cost, tr, item') ->
        let x = sem.follow cost tr in
        let var = Tail (Transition.target tr, item') in
        match Transition.split tr with
        | L gt when cost = infinity ->
          fun fix ->
            let x = sem.choice x (fix (Goto gt)) in
            sem.penalty penalty (sem.sequence x (fix (var)))
        | _ -> fun fix -> sem.penalty penalty (sem.sequence x (fix var))

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
      follow   = (fun x _ -> x);
      reduce   = (fun x _ -> x);
      sequence = (+.);
      choice   = Float.min;
      penalty  = (+.);
    })

  type action =
    | Abort
    | Reduce of Production.t
    | Shift of Symbol.t
    | Var of variable

  let action =
    let abort = (infinity, [Abort]) in
    let ret x p = if x = infinity then abort else (x, p) in
    let sem = eval {
        stuck    = abort;
        follow   = (fun x tr -> ret x [Shift (Transition.symbol tr)]);
        reduce   = (fun x prod -> ret x [Reduce prod]);
        sequence = (fun (x1, s1) (x2, s2) -> ret (x1 +. x2) (s1 @ s2));
        choice   = (fun (x1, _ as t1) (x2, _ as t2) -> if x1 <= x2 then t1 else t2);
        penalty  = (fun p (x, s as t) -> if p = 0.0 then t else ret (x +. p) s);
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
    if SymbolsSet.is_empty t1 || SymbolsSet.is_empty t2
    then SymbolsSet.empty
    else
      let s1 = SymbolsSet.choose t1 in
      if SymbolsSet.equal t1 (SymbolsSet.singleton s1)
      then add s1 t2
      else
        let s2 = SymbolsSet.choose t2 in
        if SymbolsSet.equal t2 (SymbolsSet.singleton s2)
        then add s2 t1
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
      follow   = (fun x tr ->
          if x = infinity then
            SymbolsSet.singleton (IndexSet.singleton (Transition.symbol tr))
          else epsilon
        );
      reduce   = (fun x _prod -> if x = infinity then SymbolsSet.empty else epsilon);
      sequence = join;
      choice   = union;
      penalty  = (fun _ x -> x);
    })
end
