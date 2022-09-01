open Fix.Indexing
open Utils
open Misc

module Make (Info : Sigs.INFO)() : Sigs.REDGRAPH with module Info = Info =
struct
  module Info = Info
  open Info

  module State = struct
    module Extra = Gensym()
    include Sum(Lr1)(Extra)
    let of_lr1 = inj_l
  end

  type frame = {
    states: Lr1.set;
    goto_nt: Terminal.set Nonterminal.map;
    parent: State.Extra.n index option;
  }

  let extra : (State.Extra.n, frame) IndexBuffer.t =
    IndexBuffer.make
      {states = IndexSet.empty; goto_nt = IndexMap.empty; parent = None}

  let alloc_extra frame =
    let index = State.Extra.fresh () in
    IndexBuffer.set extra index frame;
    index

  let init_root lr1 =
    let rec visit_reductions n states = function
      | [] -> {states; goto_nt = IndexMap.empty; parent = None}
      | (n', prod, _, ts) :: rest when n = n' ->
        let result = visit_reductions n states rest in
        let nt = Production.lhs prod in
        let goto_nt =
          IndexMap.update nt (function
              | None -> Some ts
              | Some ts' -> Some (IndexSet.union ts ts')
            ) result.goto_nt
        in
        {result with goto_nt}
      | ((n', _, _, _) :: _) as reds ->
        assert (n' > n);
        let frame = visit_reductions (n + 1) (Lr1.set_predecessors states) reds in
        {states; goto_nt = IndexMap.empty; parent = Some (alloc_extra frame)}
    in
    visit_reductions 1 (Lr1.predecessors lr1) (Lr1.closed_reductions lr1)

  let roots = Vector.init Lr1.n init_root

  let extra = IndexBuffer.contents extra State.Extra.n

  let frame index =
    match State.prj index with
    | L r -> Vector.get roots r
    | R x -> Vector.get extra x

  (* Goto closure *)

  let state_lr1s i = (frame i).states
  let state_parent i = Option.map State.inj_r (frame i).parent
  let state_goto_nt i = (frame i).goto_nt

  type goto_closure = {
    sources: Lr1.set;
    targets: Lr1.set;
    lookahead: Terminal.set;
  }

  let goto_closure =
    let table = Vector.make State.n [] in
    let close i =
      let close_lr1 lr1_src =
        let visited = ref IndexMap.empty in
        let rec visit_nt la0 nt la =
          let la = IndexSet.inter la0 la in
          if not (IndexSet.is_empty la) then
            let lr1_tgt =
              try Transition.find_goto_target lr1_src nt
              with Not_found -> assert false
            in
            match IndexMap.find_opt lr1_tgt !visited with
            | None ->
              visited := IndexMap.add lr1_tgt la !visited;
              visit_goto la (State.of_lr1 lr1_tgt)
            | Some la' ->
              if not (IndexSet.subset la la') then (
                visited := IndexMap.add lr1_tgt (IndexSet.union la la') !visited;
                visit_goto la' (State.of_lr1 lr1_tgt)
              )
        and visit_goto la st =
          IndexMap.iter (visit_nt la) (state_goto_nt st)
        in
        visit_goto Terminal.all i;
        !visited
      in
      let add_lr1 st acc = (close_lr1 st, st) :: acc in
      if not (IndexMap.is_empty (state_goto_nt i)) then
        Vector.set table i (
          IndexSet.fold add_lr1 (state_lr1s i) []
          |> List.concat_map (fun (map, src) ->
              let by_lookhead tgt la acc = (tgt, la, src) :: acc in
              IndexMap.fold by_lookhead map []
            )
          |> Misc.group_by
            ~compare:(fun (_tgt, la1, src1) (_tgt, la2, src2) ->
                let c = IndexSet.compare la1 la2 in
                if c <> 0 then c else
                  compare_index src1 src2
              )
            ~group:(fun (tgt, la, src) others ->
                List.fold_left
                  (fun acc (tgt, _, _) -> IndexSet.add tgt acc)
                  (IndexSet.singleton tgt) others,
                (la, src)
              )
          |> IndexRefine.annotated_partition
          |> List.concat_map (fun (targets, la_src) ->
              IndexRefine.annotated_partition la_src
              |> List.map (fun (lookahead, srcs) ->
                  {lookahead; targets; sources = IndexSet.of_list srcs}
                )
            )
        )
    in
    Index.iter State.n close;
    table

  let reachable_goto =
    (* Compute reachable goto closure *)
    let module X =
      Fix.Fix.ForType
        (struct type t = State.n index end)
        (struct
          type property = Lr1.set
          let bottom = IndexSet.empty
          let equal = IndexSet.equal
          let is_maximal _ = false
        end)
    in
    let equations index =
      let all_goto =
        List.fold_left
          (fun acc gc -> IndexSet.union gc.targets acc)
          IndexSet.empty
          (Vector.get goto_closure index)
      in
      let targets =
        IndexSet.fold
          (fun lr1 acc -> IndexSet.add (State.of_lr1 lr1) acc)
          all_goto IndexSet.empty
      in
      fun valuation ->
        let frame = frame index in
        let acc = all_goto in
        let acc = match frame.parent with
          | None -> acc
          | Some parent -> IndexSet.union acc (valuation (State.inj_r parent))
        in
        IndexSet.fold
          (fun target acc -> IndexSet.union (valuation target) acc)
          targets acc
    in
    tabulate_finset State.n (X.lfp equations)

  let state_goto_closure x = Vector.get goto_closure x
  let state_reachable = reachable_goto

  (* Compute the trie of derivations *)

  type derivation = {
    mutable children: derivation Lr1.map;
    mutable children_domain: Lr1.set;
    mutable goto_targets: Lr1.set;
  }

  let fresh_derivation () = {
    children = IndexMap.empty;
    children_domain = IndexSet.empty;
    goto_targets = IndexSet.empty;
  }

  let derivation_root = fresh_derivation ()

  let () =
    let count = ref 0 in
    let delta d lr1 =
      match IndexMap.find_opt lr1 d.children with
      | Some d' -> d'
      | None ->
        incr count;
        let d' = fresh_derivation () in
        d.children <- IndexMap.add lr1 d' d.children;
        d.children_domain <- IndexSet.add lr1 d.children_domain;
        d'
    in
    let rec process_stack d = function
      | [] -> assert false
      | [lr1] -> d.goto_targets <- IndexSet.add lr1 d.goto_targets
      | lr1' :: stack -> process_stack (delta d lr1') stack
    in
    let process_root lr1 =
      List.iter (process_stack derivation_root) (Lr1.internal_stacks lr1)
    in
    Index.iter Lr1.n process_root;
    Printf.eprintf "derivation trie has %d nodes\n" !count

  let () =
    Index.iter Lr1.n (fun lr1 ->
        Printf.eprintf "deriving root %s\n" (Lr1.to_string lr1);
        List.iter (fun stack ->
            Printf.eprintf "deriving stack %s\n" (Lr1.list_to_string stack);
          ) (Lr1.internal_stacks lr1);
      )

  let derive ~root ~step ~join =
    let map = ref IndexMap.empty in
    let rec visit acc d =
      IndexSet.iter (fun lr1 ->
          match IndexMap.find_opt lr1 !map with
          | None -> map := IndexMap.add lr1 (ref [acc]) !map
          | Some cell -> push cell acc
        ) d.goto_targets;
      IndexMap.iter (fun lr1 d' ->
          match step acc lr1 with
          | None -> ()
          | Some acc -> visit acc d'
        ) d.children
    in
    visit root derivation_root;
    IndexMap.map (fun cell -> join !cell) !map

  let () =
    if false then
      let rec visit path node =
        let print_target lr1 =
          Printf.eprintf "%s <- %s\n"
            (string_concat_map " -> " Lr1.to_string (List.rev path))
            (Lr1.to_string lr1)
        in
        IndexSet.iter print_target node.goto_targets;
        IndexMap.iter (fun lr1 tgt -> visit (lr1 :: path) tgt) node.children
      in
      visit [] derivation_root
end
