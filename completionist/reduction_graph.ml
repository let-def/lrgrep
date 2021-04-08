open Utils
open Middle
module Fin = Strong.Finite

module Make (Sigma : Intf.SIGMA)() = struct
  module Sigma = Sigma
  module Lr1 = Sigma.Lr1
  module G = Lr1.Grammar
  module LookaheadSet = BitSet.Make(G.Terminal)

  let reductions = G.Lr1.tabulate begin fun lr1 ->
      G.Lr1.reductions lr1
      |> List.map (fun (lookahead, prods) -> lookahead, List.hd prods)
      |> List.filter (fun (_, prod) ->
          match G.Nonterminal.kind (G.Production.lhs prod) with
          | `START -> false
          | `REGULAR -> true
        )
      |> Misc.group_by
        ~compare:(fun (_, p1) (_, p2) ->
            Int.compare (G.Production.to_int p1) (G.Production.to_int p2))
        ~group:(fun (lookahead, p) others ->
            let add set (l, _) = LookaheadSet.add l set in
            let lookaheads =
              List.fold_left add (LookaheadSet.singleton lookahead) others
            in
            p, lookaheads
          )
    end

  module Graph : sig
    type state = private
      | Lr1 of Lr1.t
      | Goto of { lr1: Lr1.t; next: state; }

    val from_lr1 : Lr1.t -> state
    val stack_top : state -> Lr1.t

    type transition =
      | Targets of {pop: int; dispatch: (Lr1.t * state) list}
      | Epsilon of state

    val transitions : state -> transition list
  end = struct

    type state =
      | Lr1 of Lr1.t
      | Goto of { lr1: Lr1.t; next: state; }

    let from_lr1 lr1 = Lr1 lr1

    type transition =
      | Targets of {pop: int; dispatch: (Lr1.t * state) list}
      | Epsilon of state

    type builder =
      | Abstract of state
      | Concrete of Lr1.Set.t * int

    let start_from state = Abstract state

    let pop = function
      | Abstract (Goto g) -> Abstract g.next
      | Abstract (Lr1 lr1) ->
        Concrete (Lr1.predecessors_of_state lr1, 0)
      | Concrete (hd, pop) ->
        Concrete (Lr1.predecessors_of_states hd, pop + 1)

    let stack_top (Goto {lr1; _} | Lr1 lr1) = lr1

    let goto nt = function
      | Abstract next ->
        Epsilon (Goto {lr1 = Lr1.goto (stack_top next) nt; next})
      | Concrete (current, pop) ->
        let goto parent =
          (parent, Goto {lr1 = Lr1.goto parent nt; next = Lr1 parent})
        in
        Targets {pop; dispatch = List.map goto (Lr1.Set.elements current)}

    let transitions state =
      let reduce (prod, _) =
        let builder = start_from state in
        let builder =
          Array.fold_right
            (fun _ builder -> pop builder)
            (G.Production.rhs prod)
            builder
        in
        goto (G.Production.lhs prod) builder
      in
      List.map reduce (reductions (stack_top state))
  end

  let prepare_transitions transitions =
    let split (dispatched, epsilons) = function
      | Graph.Epsilon target -> (dispatched, target :: epsilons)
      | Graph.Targets {pop; dispatch} ->
        let add dispatched (lr1,target) = (pop,lr1,target) :: dispatched in
        (List.fold_left add dispatched dispatch, epsilons)
    in
    let dispatched, epsilons = List.fold_left split ([], []) transitions in
    let dispatched =
      Misc.group_by
        ~compare:(fun (pop1,_,_) (pop2,_,_) -> Int.compare pop1 pop2)
        ~group:(fun (pop,_,_ as first) others ->
            let prj (_pop, lr1, target) = (lr1, target) in
            let targets = prj first :: List.map prj others in
            (pop, List.sort_uniq compare targets)
          )
        dispatched
    in
    (dispatched, epsilons)

  let print_graphviz oc ~iter =
    output_string oc "digraph G {\n";
    let state_index = Hashtbl.create 7 in
    let dispatch_index = Hashtbl.create 7 in
    let rec visit state =
      try Hashtbl.find state_index state
      with Not_found ->
        let id = Hashtbl.length state_index in
        Hashtbl.add state_index state id;
        let dispatch, epsilons =
          prepare_transitions (Graph.transitions state)
        in
        List.iter (fun target ->
            let id' = visit target in
            Printf.fprintf oc "  S%04d -> S%04d [label=\"epsilon\"];\n" id id'
          ) epsilons;
        List.iter (fun (pop, targets) ->
            Printf.fprintf oc "  S%04d -> D%04d [label=\"pop %d\"];\n"
              id (visit_dispatch targets) pop
          ) dispatch;
        id
    and visit_dispatch targets =
      try Hashtbl.find dispatch_index targets
      with Not_found ->
        let id = Hashtbl.length dispatch_index in
        Hashtbl.add dispatch_index targets id;
        Printf.fprintf oc "  D%04d [label=\"dispatch\",shape=diamond];\n" id;
        List.iter (fun (label, target) ->
            let id' = visit target in
            Printf.fprintf oc "  D%04d -> S%04d [label=\"%d\"];\n"
              id id' (G.Lr1.to_int label)
          ) targets;
        id
    in
    iter (fun lr1 -> ignore (visit (Graph.from_lr1 lr1)));
    output_string oc "}\n"

  module Concrete : sig
    type states
    val states : states Fin.set

    type state = states Fin.elt

    val from_lr1 : Lr1.t -> state
    val represent : state -> Graph.state

    val epsilons : state -> state list
    val immediate_transitions : state -> (int * (Lr1.t * state) list) list
  end = struct
    module States = Fin.Set.Gensym()
    type states = States.n
    type state = states Fin.elt

    let state_index = Hashtbl.create 7
    let tr_index = Hashtbl.create 7

    let rec visit state =
      try Hashtbl.find state_index state
      with Not_found ->
        let id = States.fresh () in
        Hashtbl.add state_index state id;
        let disp, eps = prepare_transitions (Graph.transitions state) in
        let eps = List.map visit eps in
        let disp =
          let visit_tgt (lr1, tgt) = lr1, visit tgt in
          let visit_disp (pop, tgts) = pop, List.map visit_tgt tgts in
          List.map visit_disp disp
        in
        Hashtbl.add tr_index id (state, eps, disp);
        id

    let from_lr1 = G.Lr1.tabulate (fun lr1 -> visit (Graph.from_lr1 lr1))

    let states = States.freeze ()
    let tr_table = Fin.Array.init states (Hashtbl.find tr_index)

    let epsilons st = let _, eps, _ = Fin.(tr_table.(st)) in eps

    let immediate_transitions st =
      let _, _, disp = Fin.(tr_table.(st)) in
      disp

    let represent st = let st, _, _ = Fin.(tr_table.(st)) in st
  end

  let () =
    let oc = open_out "red.dot" in
    print_graphviz oc ~iter:G.Lr1.iter;
    close_out oc
end
