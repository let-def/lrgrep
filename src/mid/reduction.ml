open Fix.Indexing
open Utils
open Misc

module Make(Redgraph : Sigs.REDGRAPH) :
  Sigs.REDUCTION with module Info = Redgraph.Info =
struct
  module Info = Redgraph.Info
  open Info

  module type DERIVABLE = sig
    type t
    val derive : t -> t partial_derivative list
    val merge : t list -> t
    val compare : t -> t -> int
    val cmon : t -> Cmon.t
  end

  module Cache (D : DERIVABLE) = struct
    type t = {
      d: D.t;
      mutable tr: t partial_derivative list option;
    }

    let lift d = {d; tr=None}
    let unlift t = t.d

    let derive t =
      match t.tr with
      | Some tr -> tr
      | None ->
        let dir = D.derive t.d in
        let tr = List.map (fun (sg, d) -> (sg, lift d)) dir in
        t.tr <- Some tr;
        tr

    let merge = function
      | [x] -> x
      | xs -> lift (D.merge (List.map unlift xs))

    let compare t1 t2 =
      D.compare t1.d t2.d

    let cmon t =
      Cmon.constructor "Cache" (D.cmon t.d)
  end

  module Make (D : DERIVABLE) = struct

    module DMap = Map.Make(D)

    type compilation = {
      source: D.t;
      (** The derivable object that was compiled *)

      continuations: D.t Lr1.map;
      (** [continuations] is the map [lr1 -> d'] of derivations of [source]
          (obtained by deriving one or more times), that applies when the LR
          automaton reaches state [lr1].
          Either because [lr1] is the current state or because we reached a
          goto transition targeting [lr1].
          [d'] has already been derived by [lr1]. Only non-empty [d'] are kept
          in the map. *)

      domain: Lr1.set;
      (** The domain of [continuations], used to speed-up the case where none
          of the continuations applies. *)
    }

    type compilation_cache = compilation DMap.t ref

    let make_compilation_cache () = ref DMap.empty

    let compile source =
      let find_tr lr1 (lr1s, x) =
        if IndexSet.mem lr1 lr1s then Some x else None
      in
      let continuations =
        Redgraph.derive
          ~root:source
          ~step:(fun d lr1 -> List.find_map (find_tr lr1) (D.derive d))
          ~join:D.merge
      in
      {source; continuations; domain = IndexMap.domain continuations}

    let compile cache source =
      match DMap.find_opt source !cache with
      | Some c -> c
      | None ->
        let result = compile source in
        cache := DMap.add source result !cache;
        result

    let cmon compiled =
      IndexMap.fold
        (fun lr1 d acc ->
           Cmon.tuple [Cmon.constant (Lr1.to_string lr1); D.cmon d] :: acc)
        compiled.continuations []
      |> List.rev
      |> Cmon.list

    type t = {
      derivations: compilation;
      state: Redgraph.State.n index;
      lookahead: Terminal.set;
    }

    type transitions = D.t partial_derivative list * t partial_derivative list

    let compare t1 t2 =
      let c = compare_index t1.state t2.state in
      if c <> 0 then c else
        let c = IndexSet.compare t1.lookahead t2.lookahead in
        if c <> 0 then c else
          D.compare t1.derivations.source t2.derivations.source

    let add_abstract_state derivations lookahead sg state xs =
      if IndexSet.disjoint (Redgraph.state_reachable state) derivations.domain
      then xs
      else (sg, {derivations; state; lookahead}) :: xs

    let initial derivations =
      let add_direct lr1 d xs = (IndexSet.singleton lr1, d) :: xs in
      let add_reducible lr1 xs =
        add_abstract_state derivations Terminal.all
          (IndexSet.singleton lr1) (Redgraph.State.of_lr1 lr1) xs
      in
      let direct = IndexMap.fold add_direct derivations.continuations [] in
      let reducible = index_fold Lr1.n [] add_reducible in
      (direct, reducible)

    let filter_tr sg1 (sg2, v) =
      let sg' = IndexSet.inter sg1 sg2 in
      if IndexSet.is_empty sg' then
        None
      else
        Some (sg', v)

    let derive t =
      let direct = ref [] in
      let reducible = ref (
          match Redgraph.state_parent t.state with
          | None -> []
          | Some state ->
            add_abstract_state t.derivations Terminal.all Lr1.all state []
        )
      in
      let visit_goto {Redgraph. sources; targets; lookahead} =
        let lookahead = IndexSet.inter lookahead t.lookahead in
        if not (IndexSet.is_empty lookahead) then
          IndexSet.iter begin fun target ->
            begin match IndexMap.find_opt target t.derivations.continuations with
              | None -> ()
              | Some d ->
                List.iter
                  (fun tr -> Option.iter (push direct) (filter_tr sources tr))
                  (D.derive d)
            end;
            begin match Redgraph.state_parent (Redgraph.State.of_lr1 target) with
              | None -> ()
              | Some st ->
                reducible :=
                  add_abstract_state t.derivations lookahead sources st !reducible
            end;
          end targets;
      in
      List.iter visit_goto (Redgraph.state_goto_closure t.state);
      (!direct, !reducible)

  end
end
