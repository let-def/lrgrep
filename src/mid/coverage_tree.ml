open Utils
open Misc
open Fix.Indexing

module Make (Graph : sig
    include CARDINAL
    type terminal
    type transition
    val initials : (n index -> unit) -> unit
    val successors : n index -> (transition -> n index -> unit) -> unit
    val rejected : n index -> terminal indexset
    val potential_reject : n index -> terminal indexset
  end) :
sig
  type suffix =
    | Init of Graph.n index
    | Step of Graph.n index * Graph.transition * suffix

  val enum_sentences :
    cover:'n cardinal -> index:(Graph.n index -> 'n index) ->
    (suffix -> Graph.terminal indexset -> unit) -> unit
end =
struct
  type suffix =
    | Init of Graph.n index
    | Step of Graph.n index * Graph.transition * suffix

  type node = {
    suffix: suffix;
    mutable committed: Graph.terminal indexset;
    mutable child: node list;
  }

  let suffix_state (Init st | Step (st, _, _)) = st

  let remainings ~cover ~index =
    let table = Vector.make cover IndexSet.empty in
    Index.iter Graph.n (fun st ->
        let i = index st in
        let set = Vector.get table i in
        Vector.set table i
          (IndexSet.union (Graph.potential_reject st) set)
      );
    table

  let build_arborescence ~cover ~index =
    let remainings = remainings ~cover ~index in
    let visited = Boolvector.make Graph.n in
    let todo = ref [] in
    let need_visit st =
      if not (Boolvector.test visited st)
      then (Boolvector.set visited st; true)
      else false
    in
    let visit_node node =
      let st = suffix_state node.suffix in
      let i = index st in
      let remaining = Vector.get remainings i in
      node.committed <- IndexSet.inter remaining (Graph.rejected st);
      Vector.set remainings i (IndexSet.diff remaining node.committed);
      Graph.successors st (fun transition st' ->
          if need_visit st' then (
            let node' = {
              suffix = Step (st', transition, node.suffix);
              committed = IndexSet.empty;
              child = [];
            } in
            node.child <- node' :: node.child;
            Misc.push todo node'
          )
        )
    in
    let initials = ref [] in
    Graph.initials (fun st ->
        assert (need_visit st);
        let node = {
          suffix = Init st;
          committed = IndexSet.empty;
          child = [];
        } in
        visit_node node;
        Misc.push initials node
      );
    let initials = !initials in
    while !todo <> [] do
      let todo' = !todo in
      todo := [];
      List.iter visit_node todo'
    done;
    (remainings, initials)

  let build_remainder remaining suffix =
    let remaining = ref remaining in
    let todo = ref [] in
    let visit node =
      let st = suffix_state node.suffix in
      node.committed <- IndexSet.inter !remaining (Graph.rejected st);
      if not (IndexSet.is_empty node.committed) then
        remaining := IndexSet.diff !remaining node.committed;
      Graph.successors st @@ fun transition st' ->
      if not (IndexSet.disjoint !remaining (Graph.potential_reject st)) then (
        let node' = {
          suffix = Step (st', transition, node.suffix);
          committed = IndexSet.empty;
          child = [];
        } in
        node.child <- node' :: node.child;
        Misc.push todo node'
      )
    in
    let root = {suffix; committed = IndexSet.empty; child = []} in
    visit root;
    while !todo <> [] do
      let todo' = !todo in
      todo := [];
      List.iter visit todo'
    done;
    root

  let enum_sentences ~cover ~index f =
    let remainings, forest = build_arborescence ~cover ~index in
    let visited = Boolvector.make Graph.n in
    let rec visit in_remainder node =
      let processed =
        List.fold_left
          (fun acc node -> IndexSet.union (visit in_remainder node) acc)
          IndexSet.empty node.child
      in
      let processed =
        let remainder = IndexSet.diff node.committed processed in
        if IndexSet.is_empty remainder then processed else (
          f node.suffix remainder;
          IndexSet.union remainder processed
        )
      in
      let processed =
        let state = suffix_state node.suffix in
        Boolvector.set visited state;
        let index = index state in
        let remaining = Vector.get remainings index in
        if in_remainder then (
          let remaining' = IndexSet.diff remaining processed in
          if remaining != remaining' then
            Vector.set remainings index remaining';
          processed
        ) else
          let remainder =
            IndexSet.inter remaining (Graph.potential_reject state)
          in
          if IndexSet.is_empty remainder then processed else (
            let node' = build_remainder remainder node.suffix in
            let remainder' = visit true node' in
            assert (IndexSet.subset remainder remainder');
            assert (IndexSet.disjoint (Vector.get remainings index) remainder);
            IndexSet.union remainder' processed
          )
      in
      processed
    in
    List.iter (fun node -> ignore (visit false node)) forest;
    Vector.iter (fun set -> assert (IndexSet.is_empty set)) remainings
end
