open Syntax

let linearize_symbol =
  let buffer = Buffer.create 32 in
  function
  | Name s -> s
  | sym ->
    Buffer.reset buffer;
    let rec aux = function
      | Name s -> Buffer.add_string buffer s
      | Apply (s, args) ->
        Buffer.add_string buffer s;
        Buffer.add_char buffer '(';
        List.iteri (fun i sym ->
            if i > 0 then Buffer.add_char buffer ',';
            aux sym
          ) args;
        Buffer.add_char buffer ')'
    in
    aux sym;
    Buffer.contents buffer

exception Error of string

let fail fmt =
  Printf.ksprintf (fun str -> raise (Error str)) fmt

module Make(Regex : Middle.Intf.REGEX) = struct
  module Sigma = Regex.Sigma
  module Lr1 = Sigma.Lr1
  module Grammar = Lr1.Grammar

  module Derive_modulo = struct

    type state = {
      mutable scheduled: bool;
      mutable explored: Lr1.Set.t;
      transitions: Regex.transition list;
    }

    let get_state dfa expr =
      match Regex.Map.find expr dfa with
      | state -> dfa, state
      | exception Not_found ->
        let transitions = Regex.derive expr in
        let state = {
          scheduled = false;
          explored = Lr1.Set.empty;
          transitions;
        } in
        Regex.Map.add expr state dfa, state

    let full = Lr1.Grammar.Lr1.fold Lr1.Set.add Lr1.Set.empty

    let initial dfa expr =
      let dfa, state = get_state dfa expr in
      state.scheduled <- true;
      state.explored <- full;
      (dfa, [state])

    let rec flush dfa = function
      | [] -> dfa
      | state :: states ->
        assert state.scheduled;
        state.scheduled <- false;
        let predecessors = Lr1.predecessors_of_states state.explored in
        let process_transition (dfa, states) (sigma, _, expr) =
          let sigma' = match sigma with
            | Sigma.Pos sigma -> Lr1.Set.inter sigma predecessors
            | Sigma.Neg sigma -> Lr1.Set.diff sigma predecessors
          in
          let dfa, state' = get_state dfa expr in
          let states =
            if not (Lr1.Set.subset sigma' state'.explored) then (
              state'.explored <- Lr1.Set.union sigma' state'.explored;
              if not state'.scheduled then
                (state'.scheduled <- true; state' :: states)
              else
                states
            ) else states
          in
          (dfa, states)
        in
        let dfa, states =
          List.fold_left process_transition (dfa, states) state.transitions
        in
        flush dfa states
  end

  let symbols : (string, Grammar.symbol) Hashtbl.t =
    Hashtbl.create 107

  let () =
    let add_symbol s =
      Hashtbl.add symbols (Grammar.symbol_name ~mangled:false s) s
    in
    Grammar.Terminal.iter (fun t -> add_symbol (Grammar.T t));
    Grammar.Nonterminal.iter (fun n -> add_symbol (Grammar.N n))

  let translate_symbol sym =
    let str = linearize_symbol sym in
    try Hashtbl.find symbols str
    with Not_found ->
      fail "Unknown symbol %s\n" str

  let translate_nonterminal sym =
    match translate_symbol sym with
    | Grammar.N n -> n
    | Grammar.T t ->
      fail "Expecting a non-terminal but %s is a terminal\n"
        (Grammar.Terminal.name t)

  let translate_producers list =
    List.map (Option.map translate_symbol) list

  let rec translate_term : Syntax.regular_term -> Regex.Expr.t =
    function
    | Symbol sym ->
      let sym = translate_symbol sym in
      let states = Lr1.by_incoming_symbol sym in
      Regex.Expr.set (Sigma.Pos states)
    | Item { lhs; prefix; suffix } ->
      let lhs = Option.map translate_nonterminal lhs in
      let prefix = translate_producers prefix in
      let suffix = translate_producers suffix in
      let states = Lr1.states_by_items ?lhs ~prefix ~suffix in
      Regex.Expr.set (Sigma.Pos states)
    | Wildcard -> Regex.Expr.set Sigma.full
    | Alternative (re1, re2) ->
      Regex.Expr.(|.) (translate_expression re1) (translate_expression re2)
    | Repetition (re, _) ->
      Regex.Expr.star (translate_expression re)
    | Reduce (re, _) ->
      Regex.simulate_reductions (translate_expression re)

  and translate_expression (terms : Syntax.regular_expression) : Regex.Expr.t =
    let terms =
      (* The reverse is important:
         in lrgrep, the expression is read from right-to-left (matching from
         the top of the stack, to the root of the analysis) *)
      List.rev_map (fun (term, _) -> translate_term term) terms
    in
    Regex.Expr.concatenation terms

  let translate (def : Syntax.lexer_definition) =
    let time0 = Sys.time () in
    let action_counter = ref 0 in
    let entries = List.map (fun entry ->
        let clauses = List.map (fun clause ->
            let expr = translate_expression clause.pattern in
            let action = !action_counter in
            incr action_counter;
            Regex.Expr.(expr ^. label (Regex.Action.accept action))
          ) entry.clauses
        in
        Regex.Expr.disjunction clauses
      ) def.entrypoints
    in
    let time1 = Sys.time () in
    let dfa = ref Regex.Map.empty in
    let states =
      List.map (fun expr ->
          let dfa', state = Derive_modulo.initial !dfa expr in
          dfa := dfa';
          state)
        entries
      |> List.flatten
    in
    let dfa = Derive_modulo.flush !dfa states in
    let dfa = Regex.Map.filter_map
        (fun _ {Derive_modulo. explored; transitions; _} ->
           if Lr1.Set.is_empty explored then None else
             Some (
               let sigma0 = Lr1.predecessors_of_states explored in
               List.fold_left (fun acc (sigma, label, target) ->
                   let sigma' = match sigma with
                       | Sigma.Pos sigma' -> Lr1.Set.inter sigma0 sigma'
                       | Sigma.Neg sigma' -> Lr1.Set.diff sigma0 sigma'
                   in
                   if Lr1.Set.is_empty sigma' then acc
                   else (Sigma.Pos sigma', label, target) :: acc
                 )
                 [] transitions
             )
        ) dfa
    in
    let time2 = Sys.time () in
    Printf.eprintf "translation time: regexp in %.02fms, dfa in %.02fms, %d states\n%!"
      ((time1 -. time0) *. 1000.0)
      ((time2 -. time1) *. 1000.0)
      (Regex.Map.cardinal dfa);
    begin (* Minimization *)
      let module Fin = Utils.Strong.Finite in
      let module States = Fin.Set.Gensym() in
      let dfa_st = Regex.Map.map (fun tr -> States.fresh (), tr) dfa in
      let states = States.freeze () in
      let dfa_tr = Regex.Map.fold (fun _expr (source', tr) acc ->
          List.fold_left (fun acc (sigma, _label, target) ->
              match Regex.Map.find target dfa_st with
              | exception Not_found -> acc
              | (target', _) -> (source', sigma, target') :: acc
            ) acc tr
        ) dfa_st []
      in
      let module Tr = Fin.Array.Of_array (struct
          type a = States.n Fin.elt * Sigma.t * States.n Fin.elt
          let table = Array.of_list dfa_tr
        end)
      in
      let module Valmari_DFA = struct
        type states = States.n
        let states = states
        type transitions = Tr.n
        let transitions = Tr.n
        let source x = let (a,_,_) = Fin.(Tr.table.(x)) in a
        let label x = let (_,a,_) = Fin.(Tr.table.(x)) in a
        let target x = let (_,_,a) = Fin.(Tr.table.(x)) in a

        let initials =
          List.map (fun entry -> fst (Regex.Map.find entry dfa_st)) entries
          |> Array.of_list

        let finals =
          Regex.Map.fold (fun expr (st, _) acc ->
              if Regex.Action.compare (Regex.Expr.get_label expr) Regex.Action.empty = 0
              then acc
              else st :: acc
            ) dfa_st []
          |> Array.of_list

        let refinements ~refine =
          (* States that have different actions should belong to different
             classes) *)
          let classes =
            Regex.Map.bindings dfa_st
            |> List.map (fun (expr, (st, _)) -> st, Regex.Expr.get_label expr)
            |> List.sort (fun (_,g1) (_,g2) -> Regex.Action.compare g1 g2)
            |> Utils.Misc.merge_group
              ~equal:(fun g1 g2 -> Regex.Action.compare g1 g2 = 0)
              ~group:(fun _ sts -> sts)
          in
          List.iter (fun classe -> refine ~iter:(fun f -> List.iter f classe))
            classes
      end in
      let module Min = Utils.Valmari.Minimize(Sigma)(Valmari_DFA) in
      Printf.eprintf "Minimized to %d states\n%!" (Fin.Set.cardinal Min.states)
    end;
    entries, dfa
end
