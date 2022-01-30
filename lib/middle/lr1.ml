open Utils

module Make (G : MenhirSdk.Cmly_api.GRAMMAR) =
struct
  module Grammar = G
  type t = G.lr1
  module Set = BitSet.Make(G.Lr1)

  let all_states = G.Lr1.fold Set.add Set.empty

  let set_bind : Set.t -> (Set.element -> Set.t) -> Set.t =
    fun s f -> Set.fold (fun lr1 acc -> Set.union acc (f lr1)) s Set.empty

  module type FINITE = sig
    type t
    val count : int
    val to_int : t -> int
    val iter: (t -> unit) -> unit
  end

  let reverse_index
      (type row index acc)
      ((module Row) : (module FINITE with type t = row))
      ((module Index) : (module FINITE with type t = index))
      ~(indexes_of : row -> f:(index -> unit) -> unit)
      ~(default : acc)
      ~(add : index -> row -> acc -> acc)
    : index -> acc
    =
    let table = Array.make Index.count default in
    Row.iter (fun row ->
        indexes_of row ~f:(fun index ->
            let index' = Index.to_int index in
            table.(index') <- add index row table.(index')
          )
      );
    fun index -> table.(Index.to_int index)

  let predecessors_of_state =
    reverse_index
      (module G.Lr1)
      (module G.Lr1)
      ~indexes_of:(fun lr1 ~f ->
          List.iter (fun (_,s2) -> f s2) (G.Lr1.transitions lr1))
      ~default:Set.empty
      ~add:(fun _target source sources-> Set.add source sources)

  let predecessors_of_states states =
    set_bind states predecessors_of_state

  module Symbols = struct
    type t = G.symbol

    let terminals = G.Terminal.count

    let count = terminals + G.Nonterminal.count

    let to_int = function
      | G.T t -> G.Terminal.to_int t
      | G.N n -> terminals + G.Nonterminal.to_int n

    let iter f =
      G.Terminal.iter (fun t -> f (G.T t));
      G.Nonterminal.iter (fun n -> f (G.N n))
  end

  let incoming_symbol lr1 =
    G.Lr0.incoming (G.Lr1.lr0 lr1)

  let by_incoming_symbol =
    (* Compute lr1 predecessor relation *)
    reverse_index
      (module G.Lr1)
      (module Symbols)
      ~indexes_of:(fun lr1 ~f ->
          match G.Lr0.incoming (G.Lr1.lr0 lr1) with
          | None -> ()
          | Some sym -> f sym
        )
      ~default:Set.empty
      ~add:(fun _nt lr1 set -> Set.add lr1 set)

  let goto = G.Lr1.tabulate (fun lr1 ->
      let goto_transitions =
        List.filter_map (function
            | (G.N nt, target) -> Some (nt, target)
            | (G.T _, _) -> None
          ) (G.Lr1.transitions lr1)
      in
      fun nt ->
        match List.assq_opt nt goto_transitions with
        | Some target -> target
        | None ->
          Printf.ksprintf failwith
            "Internal error: Lr1.goto: no transition from #%d on %S"
            (G.Lr1.to_int lr1) (G.Nonterminal.name nt)
    )

  let states_reducing =
    reverse_index
      (module G.Lr1)
      (module G.Production)
      ~indexes_of:(fun lr1 ~f ->
          List.iter (fun (_, prods) ->
              let prod = List.hd prods in
              if G.Production.kind prod = `REGULAR then f prod
            )
            (G.Lr1.reductions lr1)
        )
      ~default:Set.empty
      ~add:(fun _prod lr1 set -> Set.add lr1 set)

  let productions_for_states
    : ?incoming:G.nonterminal -> Set.t -> (G.production * Set.t) list
    =
    let nonnullable_productions, nullable_productions, candidate_productions =
      let nonnullable = ref [] in
      let nullable = ref [] in
      let prod_by_nt = Array.make G.Nonterminal.count [] in
      G.Production.iter (fun prod ->
          if G.Production.kind prod = `REGULAR then
            match Misc.array_last (G.Production.rhs prod) with
            | None -> nullable := prod :: !nullable
            | Some last ->
              nonnullable := prod :: !nonnullable;
              match last with
              | (G.T _, _, _) -> ()
              | (G.N nt, _, _) ->
                let index = G.Nonterminal.to_int nt in
                prod_by_nt.(index) <- prod :: prod_by_nt.(index)
        );
      !nonnullable,
      !nullable,
      (fun nt -> prod_by_nt.(G.Nonterminal.to_int nt))
    in
    fun ?incoming states ->
      let filter_production acc prod =
        let inter = Set.inter states (states_reducing prod) in
        if Set.is_empty inter
        then acc
        else (prod, inter) :: acc
      in
      let prods = List.fold_left filter_production [] nullable_productions in
      let guess = match incoming with
        | None -> nonnullable_productions
        | Some nt -> candidate_productions nt
      in
      let prods = List.fold_left filter_production prods guess in
      prods

  include Refine.Make(struct
      type 'a t = Set.t
      include (Set : BitSet.S0 with type t := Set.t)
    end)

  let maybe_has_lhs prod = function
    | None -> true
    | Some lhs -> lhs = G.Production.lhs prod

  let maybe_match_sym (sym, _, _) = function
    | None -> true
    | Some sym' -> sym = sym'

  let forall_i f l =
    match List.iteri (fun i x -> if not (f i x) then raise Exit) l with
    | () -> true
    | exception Exit -> false

  let item_match lhs (lp, prefix) (ls, suffix) (prod, pos) =
    maybe_has_lhs prod lhs &&
    pos >= lp &&
    let rhs = G.Production.rhs prod in
    Array.length rhs >= pos + ls &&
    forall_i (fun i sym -> maybe_match_sym rhs.(pos - i - 1) sym) prefix &&
    forall_i (fun i sym -> maybe_match_sym rhs.(pos + i) sym) suffix

  let states_by_items ~lhs ~prefix ~suffix =
    let prefix' = List.length prefix, List.rev prefix in
    let suffix' = List.length suffix, suffix in
    let result =
      G.Lr1.fold (fun lr1 acc ->
          if List.exists
              (item_match lhs prefix' suffix')
              (G.Lr0.items (G.Lr1.lr0 lr1))
          then Set.add lr1 acc
          else acc
        ) Set.empty
    in
    let lhs = match lhs with
      | None -> ""
      | Some nt -> G.Nonterminal.name nt ^ ": "
    in
    let sym_list l = String.concat " " (List.map (function
        | Some sym -> G.symbol_name sym
        | None -> "_"
      ) l)
    in
    Printf.eprintf "[%s%s . %s] = %d states\n"
      lhs (sym_list prefix) (sym_list suffix) (Set.cardinal result);
    result
end
