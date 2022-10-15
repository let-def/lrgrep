open Fix.Indexing
open Utils
open Misc

module Make (Info : Sigs.INFO)() : Sigs.REDGRAPH with module Info = Info =
struct
  module Info = Info
  open Info

  (** The reduction graph states are either:
      - an Lr1 state (a "root", where reductions starts from)
      - an intermediate state (one or more reductions going on),
        represented by [State.Intermediate].

      Intermediate states represents the successive concrete stack frames that
      can be visited when reducing a production.

      An lr1 state [st] with an item [X := a b c .] will be represented by
      the "root" [State.of_lr1 st].
      Then there will be three intermediate states:
      - [Some n1 = state_parent (State.of_lr1 st)] with item [X := a b . c]
      - [Some n2 = state_parent n1] with item [X := a . b c]
      - [Some n3 = state_parent n2] with item [X := . a b c]
  *)
  module State = struct
    module Intermediate = Gensym()
    include Sum(Lr1)(Intermediate)
    let of_lr1 = inj_l
  end

  type state = State.n index

  (** A frame store all the information associated with intermediate state.
      It is an abstract representation of the frame that can be found on a
      concrete parser stack when the .
  *)
  type frame = {
    states: Lr1.set;
    (** A concrete parser stack will have a state in [states] when this frame
        applies. *)

    goto_nt: Terminal.set Nonterminal.map;
    (** The goto transitions that can be followed from this state, represented
        by a partial mapping from a non-terminal (the label of the goto
        transition) and a set of lookahead tokens that permit following this
        transition. *)

    parent: State.Intermediate.n index option;
    (** The next intermediate state,
        or [None] if no more reductions are going on. *)
  }

  (** The mapping between frames and intermediate states *)
  let intermediate : (State.Intermediate.n, frame) IndexBuffer.t =
    IndexBuffer.make
      {states = IndexSet.empty; goto_nt = IndexMap.empty; parent = None}

  (** Allocate a new intermediate state for a given frame *)
  let new_intermediate frame =
    let index = State.Intermediate.fresh () in
    IndexBuffer.set intermediate index frame;
    index

  (** Initialize a root state for LR1 state [lr1], visiting all reductions
      starting from it, creating intermediate states when necessary. *)
  let init_root lr1 =
    let rec visit_reductions n states = function
      | [] -> {states; goto_nt = IndexMap.empty; parent = None}
      | (c : Lr1.closed_reduction) :: rest when c.pop = n ->
        let result = visit_reductions n states rest in
        let goto_nt =
          IndexMap.update (Production.lhs c.prod) (function
              | None -> Some c.lookahead
              | Some ts' -> Some (IndexSet.union c.lookahead ts')
            ) result.goto_nt
        in
        {result with goto_nt}
      | (c :: _) as reds ->
        assert (c.pop > n);
        let frame = visit_reductions (n + 1) (Lr1.set_predecessors states) reds in
        {states; goto_nt = IndexMap.empty; parent = Some (new_intermediate frame)}
    in
    visit_reductions 1 (Lr1.predecessors lr1) (Lr1.closed_reductions lr1)

  (** All roots *)
  let roots = Vector.init Lr1.n init_root

  (** Freeze intermediate frames *)
  let intermediate = IndexBuffer.contents intermediate State.Intermediate.n

  (** Get the frame for a state, looking up Lr1 states in [roots] and
      intermediate ones in [intermediate] *)
  let frame index =
    match State.prj index with
    | L r -> Vector.get roots r
    | R x -> Vector.get intermediate x

  (* Accessors to get information stored in a frame directly from
     a state index *)

  let state_lr1s i = (frame i).states
  let state_parent i = Option.map State.inj_r (frame i).parent
  let state_goto_nt i = (frame i).goto_nt

  (* Goto closure *)

  (** See [Sigs.REDGRAPH.goto_closure] for documentation on [goto_closure].

      FIXME: is it really useful to explicitly compute goto_closure?
      Maybe it is efficient enough to recompute the closure as needed. *)
  type goto_closure = {
    sources: Lr1.set;
    targets: Lr1.set;
    lookahead: Terminal.set;
  }

  let goto_closure =
    let table = Vector.make State.n [] in
    let close i =
      (* Close each possible concrete state separately *)
      let close_lr1 lr1_src =
        (* Visit a goto transition labelled by [nt] when looking ahead at a
           token ∈  [la] ⋂ [la0].
           [acc] is a map populated with {target -> lookahead}, where [target]
           is the lr1 state reached by following the goto transition and
           [lookahead] is the non-empty set of tokens that allowed to reach
           this state.
        *)
        let rec visit_nt la0 nt la acc =
          let la = Terminal.intersect la0 la in
          if IndexSet.is_empty la then
            acc
          else
            let lr1_tgt =
              try Transition.find_goto_target lr1_src nt
              with Not_found -> assert false
            in
            let update = function
              | None -> Some la
              | Some la' -> Some (IndexSet.union la la')
            in
            visit_goto la
              (State.of_lr1 lr1_tgt)
              (IndexMap.update lr1_tgt update acc)

        (* Visit all goto transitions*)
        and visit_goto la st acc =
          IndexMap.fold (visit_nt la) (state_goto_nt st) acc
        in
        visit_goto Terminal.all i IndexMap.empty
      in
      if not (IndexMap.is_empty (state_goto_nt i)) then
        let add_lr1 st acc = (close_lr1 st, st) :: acc in
        (* Populate the table with closed gotos.
           This code could be expressed as a SQL query: it groups the closures
           by sources, targets and lookaheads, maximizing the size of each
           subset (to have the smallest partition).
        *)
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
    (* Compute all reachable goto, as a fixed point of the goto closures
       reachable by following [state_parent] and the [targets] of
       [goto_closure]. *)
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
      | lr1 :: stack ->
        let d = delta d lr1 in
        match stack with
        | [] -> d.goto_targets <- IndexSet.add lr1 d.goto_targets
        | stack -> process_stack d stack
    in
    let process_root lr1 =
      List.iter (process_stack derivation_root) (Lr1.internal_stacks lr1)
    in
    Index.iter Lr1.n process_root
    (*; Printf.eprintf "derivation trie has %d nodes\n" !count*)

  (*let () =
    Index.iter Lr1.n (fun lr1 ->
        Printf.eprintf "deriving root %s\n" (Lr1.to_string lr1);
        List.iter (fun stack ->
            Printf.eprintf "deriving stack %s\n" (Lr1.list_to_string stack);
          ) (Lr1.internal_stacks lr1);
      )*)

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
