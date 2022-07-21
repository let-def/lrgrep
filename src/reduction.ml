open Fix.Indexing
open Utils
open BitSet
open Misc

open Grammar

module type DERIVABLE = sig
  type t
  val derive : t -> t dfa_transition list
  val merge : t list -> t
  val compare : t -> t -> int
  val cmon : t -> Cmon.t
end

module Cache (D : DERIVABLE) = struct
  type t = {
    d: D.t;
    mutable tr: t dfa_transition list option;
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

  type compilation = {
    source: D.t;
    continuations: D.t Lr1.map;
    domain: Lr1.set;
  }

  module DMap = Map.Make(D)

  type compilation_cache = compilation DMap.t ref

  let make_compilation_cache () = ref DMap.empty

  let compile cache source =
    match DMap.find_opt source !cache with
    | Some c -> c
    | None ->
      let find_tr lr1 (lr1s, x) =
        if IndexSet.mem lr1 lr1s then Some x else None
      in
      let continuations =
        Redgraph.derive
          ~root:source
          ~step:(fun d lr1 -> List.find_map (find_tr lr1) (D.derive d))
          ~join:D.merge
      in
      let domain = IndexMap.domain continuations in
      let result = {source; continuations; domain} in
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

  type transitions = D.t dfa_transition list * t dfa_transition list

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
    let reducible = ref [] in
    begin match Redgraph.state_parent t.state with
      | None -> ()
      | Some state ->
        reducible :=
          add_abstract_state t.derivations Terminal.all Lr1.all state !reducible
    end;
    let visit_goto {Redgraph. sources; targets; lookahead} =
      (*prerr_endline (
        string_of_indexset ~string_of_index:Lr1.to_string sources ^ " -> " ^
        string_of_indexset ~string_of_index:Lr1.to_string targets
        );*)
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
    (*prerr_endline "visiting goto closure";*)
    List.iter visit_goto (Redgraph.state_goto_closure t.state);
    (!direct, !reducible)

end
