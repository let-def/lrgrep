open Fix.Indexing
open Utils
open Misc

(* This module works with an LR automaton and finds ways to synthesize the symbols necessary to take a goto transition (that is, given an arbitrary parser configuration, how to produce a list of action that is guaranteed to produce a non-terminal, provided that this is a correct continuation of this configuration).
   This is used by [Recovery] to generate an Abstract Syntax Tree (AST) even for invalid inputs.

   The module is implemented as a functor taking an LR automaton and metadata to guide the synthesis, and computing the cheapest sequence of actions to take a given transition or completing a partially recognized rule.
   Cheapest because there are, in general, infinitely many solutions to these problems. A large part of this module deals with cost: each action is given a cost (by default [1.0] per symbol but this can be arbitrarily customized), and the sequence with the lowest is selected.
*)
module type S = sig
  module Info : Mid.Info.S
  open Info

  (* Synthesis operates by answering two kind of questions:
     - how to take a Goto transition (a transition labelled by a non-terminal; this amounts to synthesizing the non-terminal
     - how to complete a partially recognized rule (represented as a pair of a rule and an integer representing a position in this rule); this amounts to synthesizing all symbols at the right of this position
     These questions are represented by the [variable] type. *)
  type variable =
    | Goto of Transition.goto index
    | Tail of Lr1.t * Item.t (* In [Tail (st, item)], item must be long to [Lr1.effective_items st] *)

  (* Converts a variable to a string for debugging and logging purposes *)
  val variable_to_string : variable -> string

  (* Finds the productions that can be used to take a goto transition.
     In other words, if a parser is in state [Transition.(source (of_goto gt))],
     synthesizing the symbols of any production in [goto_candidates gt] and reducing this production
     causes the parser to follow the transition [gt]. *)
  val goto_candidates : Transition.goto index -> Production.set

  (* To solve a tail problem, one needs to look at the item:
     - if it is positioned at the end of the production, this production should be reduced
     - otherwise it is necessary to follow a transition to another state, advancing the item by one symbol *)
  type tail_solution =
    | Tail_reduce of float (* [Tail_reduce c]: the production should be reduced, costing [c] *)
    | Tail_follow of float * Transition.any index * Item.t
      (* [Tail_follow (c, tr, it)]: the transition [tr] should be followed.
         Directly following the transition would cost [c].
         If [c] is infinite, it is necessary to decompose the problem:
         - if [tr] is labelled by a terminal, this tail is stuck
         - if [tr] is labelled by a non-terminal, it is a goto transition and this should be solved first *)

  (* Determines the best solution for a tail item, returning the penalty for this item and the action to take *)
  val tail_candidates : Lr1.t -> Item.t -> float * tail_solution

  (* Calculates the cost of the solution of a variable *)
  val cost_of  : variable -> float

  (* Type representing an action to take in the LR automaton for synthesizing symbols. *)
  type action =
    | Reduce of Production.t (* Reduce a production *)
    | Shift  of Symbol.t     (* Follow the transition labelled by a symbol *)
    | Var    of variable     (* Apply the solution to this sub-problem *)

  (* Determines the actions with the lowest cost solving a given variable and returns their cost alongside.
     If the cost is infinite, the actions are incomplete. Synthesis is stuck and needs more placeholder values to proceed. *)
  val action : variable -> float * action list

  (* An obstacle prevent a variable from being synthesized:
     - it is a symbol if this symbol requires a semantic value and none has
       been provided, or if the symbol has been given an infinite cost
     - it is a production if this production has been given an infinite cost
     - it is an item if this item has been given an infinite penalty:w
   *)
  module Obstacle : sig
    include CARDINAL
    type t = n index

    type desc =
      | Symbol of Symbol.t
      | Production of Production.t
      | Item of Item.t

    val inj : desc -> t
    val prj : t -> desc
  end

  (* Represent a set of sets of obstacles *)
  module ObstacleSet : Set.S with type elt = Obstacle.n indexset

  (* Finds the obstacles preventing the synthesis of a given variable.

     If the variable can be solved ([cost_of var < infinity]), this set is
     empty. Otherwise, the cost is infinite because some symbols necessary to
     synthesize the variable cannot be synthesized (they are missing semantic
     values of attributed an infinite cost), or because some items or
     productions were given an infinite penalty.

     This function returns a set of sets: solving all obstacles of one of the
     sets is sufficient to make the variable synthesizable. *)
  val obstacles : variable -> ObstacleSet.t
end

module Make
    (Info : Mid.Info.S)
    (A : Recover_attrib.S with module Info := Info)
  : S with module Info := Info =
struct
  open Info

  (* Ensures that costs are non-negative and returns them *)
  let check_cost r =
    assert (r >= 0.); r

  (* Calculates the cost of a production using the provided attribute module *)
  let cost_of_prod p    = check_cost (A.cost_of_prod p)
  (* Calculates the cost of a symbol using the provided attribute module *)
  let cost_of_symbol s  = check_cost (A.cost_of_symbol s)
  (* Calculates the penalty associated with an item using the provided attribute module *)
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
         attempting to reduce a missing transition *)
      | exception Not_found -> assert false
      | tr ->
        (penalty, Tail_follow (cost_of_symbol sym, tr, Item.inj prod (pos + 1)))

  (* Interpretation of the actions to solve a variable weighted by cost *)
  type 'a interpretation = {
    stuck: 'a;
    follow: float -> Transition.any index -> 'a;
    reduce: float -> Production.t -> 'a;
    sequence: 'a -> 'a -> 'a;
    choice: 'a -> 'a -> 'a;
    item: float -> Item.t -> 'a -> 'a;
  }

  (* Evaluates the solutions of a variable based on the provided interpretation *)
  let eval sem = function
    | Goto gt ->
      let src = Transition.(source (of_goto gt)) in
      let prods = goto_candidates gt in
      fun fix ->
        IndexSet.fold (fun prod x -> sem.choice x (fix (Tail (src, Item.inj prod 0)))) prods sem.stuck
    | Tail (st, item) ->
      match tail_candidates st item with
      | penalty, Tail_reduce cost ->
        Fun.const (sem.item penalty item (sem.reduce cost (Item.production item)))
      | penalty, Tail_follow (cost, tr, item') ->
        let x = sem.follow cost tr in
        let var = Tail (Transition.target tr, item') in
        match Transition.split tr with
        | L gt when cost = infinity ->
          fun fix ->
            let x = sem.choice x (fix (Goto gt)) in
            sem.item penalty item (sem.sequence x (fix (var)))
        | _ -> fun fix -> sem.item penalty item (sem.sequence x (fix var))

  (* Module for hash tables to store intermediate solutions associated to variables *)
  module Table = struct
    type key = variable
    type 'a t = (key, 'a) Hashtbl.t
    let create () = Hashtbl.create 7
    let find k tbl = Hashtbl.find tbl k
    let add k v tbl = Hashtbl.add tbl k v
    let iter f tbl = Hashtbl.iter f tbl
    let clear = Hashtbl.clear
  end

  (* Calculates the cost of a variable using fixed-point computation *)
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
      item     = (fun c _ c' -> c +. c');
    })

  type action =
    | Reduce of Production.t
    | Shift of Symbol.t
    | Var of variable

  let action =
    let abort = (infinity, []) in
    let ret x p = if x = infinity then abort else (x, p) in
    let sem = eval {
        stuck    = abort;
        follow   = (fun x tr -> ret x [Shift (Transition.symbol tr)]);
        reduce   = (fun x prod -> ret x [Reduce prod]);
        sequence = (fun (x1, s1) (x2, s2) -> ret (x1 +. x2) (s1 @ s2));
        choice   = (fun (x1, _ as t1) (x2, _ as t2) -> if x1 <= x2 then t1 else t2);
        item     = (fun p _ (x, s as t) -> if p = 0.0 then t else ret (x +. p) s);
      } in
    fun var -> sem var (fun var' -> (cost_of var', [Var var']))

  module Obstacle = struct
    module PI = Sum(Production)(Item)
    module SPI = Sum(Symbol)(PI)

    type n = SPI.n
    let n = SPI.n
    type t = n index

    type desc =
      | Symbol of Symbol.t
      | Production of Production.t
      | Item of Item.t

    let inj = function
      | Symbol sym -> SPI.inj_l sym
      | Production p -> SPI.inj_r (PI.inj_l p)
      | Item it -> SPI.inj_r (PI.inj_r it)

    let prj t = match SPI.prj t with
      | L sym -> Symbol sym
      | R pi -> match PI.prj pi with
        | L p -> Production p
        | R it -> Item it
  end

  (* Represent a set of sets of obstacles *)
  module ObstacleSet = Set.Make(struct
    type t = Obstacle.n indexset
    let compare = IndexSet.compare
  end)

  (* The singleton {{}} represents a reachable solution -- with no obstacles. *)
  let epsilon = ObstacleSet.singleton IndexSet.empty

  (* Adds a symbol set to another set of symbol sets,
     with a fast path when the added set is empty *)
  let add t ts =
    if IndexSet.is_empty t
    then ts
    else ObstacleSet.map (IndexSet.union t) ts

  (* If a set is a singleton, returns its only element *)
  let is_singleton t =
    let s = ObstacleSet.choose t in
    if ObstacleSet.equal t (ObstacleSet.singleton s)
    then Some s
    else None

  (* Joins two sets of symbol sets, with fast paths for empty and singleton cases *)
  let join t1 t2 =
    if ObstacleSet.is_empty t1 || ObstacleSet.is_empty t2
    then ObstacleSet.empty
    else
      match is_singleton t1 with
      | Some s -> add s t2
      | None ->
        match is_singleton t2 with
        | Some s -> add s t1
        | None ->
          ObstacleSet.fold (fun s1 acc ->
              ObstacleSet.fold (fun s2 acc ->
                  ObstacleSet.add (IndexSet.union s1 s2) acc
                ) t2 acc
            ) t1 ObstacleSet.empty

  (* Unions two sets of symbol sets, treating the empty set as a wildcard *)
  let union t1 t2 =
    if ObstacleSet.equal t1 epsilon || ObstacleSet.equal t2 epsilon
    then epsilon
    else ObstacleSet.union t1 t2

  (* Calculates the obstacles on the different solutions of a variable using fixed-point computation *)
  let obstacles =
    let module Solver = Fix.Make(Table)(struct
        type property = ObstacleSet.t
        let bottom = ObstacleSet.empty
        let equal = ObstacleSet.equal
        let is_maximal _ = false
      end)
    in
    let singleton obs =
      ObstacleSet.singleton (IndexSet.singleton (Obstacle.inj obs))
    in
    Solver.lfp (eval {
      stuck    = ObstacleSet.empty;
      follow   = (fun penalty tr ->
          if penalty = infinity
          then singleton (Symbol (Transition.symbol tr))
          else epsilon
        );
      reduce   = (fun penalty prod ->
          if penalty = infinity
          then singleton (Production prod)
          else epsilon);
      sequence = join;
      choice   = union;
      item     = (fun penalty it x ->
          if penalty = infinity
          then join (singleton (Item it)) x
          else x);
    })
end
