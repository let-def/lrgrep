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

    val transitions : state -> (LookaheadSet.t * transition) list
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

    let max_dispatch = ref 0
    let max_dispatch n =
      if n > !max_dispatch then (
        Printf.eprintf "max dispatch: %d\n" n;
        max_dispatch := n
      )

    let goto nt lookahead = function
      | Abstract next ->
        lookahead,
        Epsilon (Goto {lr1 = Lr1.goto (stack_top next) nt; next})
      | Concrete (current, pop) ->
        max_dispatch (Lr1.Set.cardinal current);
        let goto parent =
          (parent, Goto {lr1 = Lr1.goto parent nt; next = Lr1 parent})
        in
        lookahead,
        Targets {pop; dispatch = List.map goto (Lr1.Set.elements current)}

    let transitions state =
      let reduce (prod, lookahead) =
        let builder = start_from state in
        let builder =
          Array.fold_right
            (fun _ builder -> pop builder)
            (G.Production.rhs prod)
            builder
        in
        goto (G.Production.lhs prod) lookahead builder
      in
      List.map reduce (reductions (stack_top state))
  end
end
