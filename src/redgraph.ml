open Fix.Indexing
open Utils
open BitSet
open Misc

module Make (Info : Sigs.INFO)() : Sigs.REDGRAPH with module Info = Info =
struct
  module Info = Info
  open Info

  let reductions = Vector.init Lr1.n (fun lr1 ->
      let prepare_goto (p, la) =
        match Production.kind p with
        | `REGULAR ->
          Some (Array.length (Production.rhs p), Production.lhs p, la)
        | `START -> None
      in
      let order (d1, n1, la1) (d2, n2, la2) =
        let c = Int.compare d1 d2 in
        if c <> 0 then c else
          let c = compare_index n1 n2 in
          if c <> 0 then c else
            IndexSet.compare la1 la2
      in
      let productions =
        Lr1.reductions lr1
        |> List.filter_map prepare_goto
        |> List.sort_uniq order
      in
      let depth = List.fold_left (fun x (d, _, _) -> max x d) (-1) productions in
      let vector = Array.make (depth + 1) [] in
      List.iter (fun (d,n,la) -> array_cons vector d (n,la)) productions;
      assert (depth = -1 || vector.(depth) <> []);
      vector
    )

  (* Representation of concrete stack suffix *)

  type concrete_frame = {
    state: Lr1.t;
    mutable goto: concrete_frame Lr1.map;
    mutable lookahead: Terminal.set;
    parent: concrete_frame option;
  }

  (* Representation of abstract stack suffix *)

  module State = struct
    module Extra = Gensym()
    include Sum(Lr1)(Extra)
    let of_lr1 = inj_l
    let fresh () = inj_r (Extra.fresh ())
  end

  type frame = {
    states: Lr1.set;
    mutable goto_nt: Terminal.set Nonterminal.map;
    mutable parent: State.n index option;
  }

  let frames : (State.n, frame) IndexBuffer.t =
    IndexBuffer.make {
      states = IndexSet.empty;
      goto_nt = IndexMap.empty;
      parent = None;
    }

  let make_abstract_frame states =
    { states; goto_nt = IndexMap.empty; parent = None }

  (* Initialize abstract frames associated to each lr1 state *)
  let () = Index.iter Lr1.n (fun lr1 ->
      IndexBuffer.set frames (State.inj_l lr1)
        (make_abstract_frame (lr1_predecessors lr1))
    )

  let fresh_abstract_frame states =
    let index = State.fresh () in
    let frame = make_abstract_frame states in
    IndexBuffer.set frames index frame;
    index

  type stack =
    | Concrete of concrete_frame
    | Abstract of State.n index

  let fail_on_closure = Vector.make Lr1.n IndexSet.empty

  (* Tabulate the roots, populating all concrete and abstract stacks *)
  let concrete_frames =
    let pop = function
      | Concrete t ->
        begin match t.parent with
          | None -> Abstract (State.of_lr1 t.state)
          | Some t' -> Concrete t'
        end
      | Abstract t ->
        let frame = IndexBuffer.get frames t in
        match frame.parent with
        | Some t' -> Abstract t'
        | None ->
          let t' = fresh_abstract_frame (lr1set_predecessors frame.states) in
          frame.parent <- Some t';
          Abstract t'
    in
    let make_frame state parent =
      {state; parent; goto=IndexMap.empty; lookahead=IndexSet.empty}
    in
    vector_tabulate Lr1.n (fun state ->
        let fails = ref (Lr1.fail_on state) in
        let frame = make_frame state None in
        let rec goto parent la (nt, la') =
          let la = IndexSet.inter la la' in
          if not (IndexSet.is_empty la) then (
            match parent with
            | Abstract t ->
              let frame = IndexBuffer.get frames t in
              frame.goto_nt <- indexmap_update frame.goto_nt nt (function
                  | None -> la
                  | Some la' -> IndexSet.union la la'
                )
            | Concrete t ->
              let state = Transition.find_goto_target t.state nt in
              begin match IndexMap.find_opt state t.goto with
                | Some t' -> populate la t'
                | None ->
                  let target = make_frame state (Some t) in
                  t.goto <- IndexMap.add state target t.goto;
                  populate la target
              end
          )
        and populate la state =
          if not (IndexSet.subset la state.lookahead) then (
            fails := IndexSet.union !fails (IndexSet.inter la (Lr1.fail_on state.state));
            state.lookahead <- IndexSet.union la state.lookahead;
            let frame = ref (Concrete state) in
            let reductions = Vector.get reductions state.state in
            for i = 0 to Array.length reductions - 1 do
              if i <> 0 then frame := pop !frame;
              List.iter (goto !frame la) reductions.(i);
            done
          )
        in
        populate (Lr1.reduce_on state) frame;
        Vector.set fail_on_closure state !fails;
        frame
      )

  let fail_on_closure = Vector.get fail_on_closure

  let () =
    (* In theory, fail_on and fail_on_closure can differ if a transition that
       follows a nullable-reduction has been removed because of a conflict.
       That should not be a problem in practice.

       Haven't seen this in the wild yet, I will leave this code to check that
       for now... *)
    Index.iter Lr1.n (fun lr1 ->
        let fail_on = Lr1.fail_on lr1 in
        let fail_on' = fail_on_closure lr1 in
        if not (IndexSet.equal fail_on fail_on')
        then (
          prerr_endline ("fail_on and fail_on_closure differs for state " ^ Lr1.to_string lr1 ^ ":");
          prerr_endline (string_concat_map ", " Terminal.to_string (IndexSet.elements fail_on));
          prerr_endline (string_concat_map ", " Terminal.to_string (IndexSet.elements fail_on'));
          exit 1
        )
      )

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
    (* let count = ref 0 in *)
    let delta lr1 d =
      match IndexMap.find_opt lr1 d.children with
      | Some d' -> d'
      | None ->
        let d' = fresh_derivation () in
        d.children <- IndexMap.add lr1 d' d.children;
        d.children_domain <- IndexSet.add lr1 d.children_domain;
        d'
    in
    let rec visit frame =
      List.map
        (delta frame.state)
        (derivation_root :: visit_children frame)
    and visit_children frame =
      IndexMap.fold
        (fun _ frame' acc -> visit frame' @ acc)
        frame.goto
        []
    in
    Index.iter Lr1.n (fun lr1 ->
        let derivations = visit (concrete_frames lr1) in
        List.iter (fun d ->
            (*if not (IndexSet.mem lr1 d.goto_targets) then incr count;*)
            d.goto_targets <- IndexSet.add lr1 d.goto_targets
          ) derivations;
      )

  (* Goto closure *)

  (* Force the AbstractFrames set, no new frames should be added from now on *)
  let frames = IndexBuffer.contents frames State.n

  let state_lr1s x = (Vector.get frames x).states
  let state_parent x = (Vector.get frames x).parent
  let state_goto_nt x = (Vector.get frames x).goto_nt

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
              (*if not (IndexSet.is_singleton targets) then (
                prerr_endline ("multiple targets in goto_closure: {" ^
                               string_concat_map ", " Lr1.to_string
                                 (IndexSet.elements targets) ^
                               "}")
              );*)
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
        let frame = Vector.get frames index in
        let acc = all_goto in
        let acc = match frame.parent with
          | None -> acc
          | Some parent -> IndexSet.union acc (valuation parent)
        in
        IndexSet.fold
          (fun target acc -> IndexSet.union (valuation target) acc)
          targets acc
    in
    vector_tabulate State.n (X.lfp equations)

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

  let state_goto_closure x = Vector.get goto_closure x
  let state_reachable = reachable_goto
end
