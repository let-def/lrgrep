open Fix.Indexing
open Utils
open Misc
open Info

(* # Computing trigrams.

   The goal is to quickly find in which states the parser can accept certain
   trigrams.
   E.g. given `let x =`, in which states can the LR(1) parser be to
   accept this input?
   This is only an over-approximation, for instance for `) ) )`
   only the first `)` can be guaranteed to be recognized given the current
   state, the rest depends on the contents of the stack.

   These trigrams are then used to efficiently select recovery and completion
   candidates.

   For completion, the candidates can be generated from the prefix alone.
   But to find which candidate is more likely in the current context, it is
   useful to peek at the suffix and rank the candidates compatible with the
   closest trigrams first.

   Similarly, for recovery, our heuristics produce many candidates which are
   then pruned by trigrams, because a good recovery not only preserve the prefix
   but should also reconnect rapidly with the remaining suffix.

   # Trigrams generation

   To pre-compute the trigrams, we start from shift transitions, the reduction
   closure and the reduction graph to construct a map `shift_closure` from a
   shift transition to a set of shift transitions such that
   `sh' ∈ shift_closure(sh)` if there exists a stack ending in `sh` that reduces
   to a stack ending in `sh'` when looking-ahead at `symbol(sh')`.

   For each shift transition `sh`, we also list the LR(1) states a parser can go
   through on the way to reducing to a state where this transition can be
   followed.

   From these two pieces of information, it is easy to compute a reasonably precise
   approximation of trigrams.
*)

let fast_enum (type g)
    (g : g grammar)
    (rcs : (g, g lr1) Redgraph.reduction_closures)
    (grcs : (g, g goto_transition) Redgraph.reduction_closures)
    : (g shift_transition, g shift_transition indexset * g lr1 indexset) vector
  =
  (* Construct a precise reduction graph *)
  stopwatch 1 "begin fast_enum 2";
  let open IndexBuffer in
  let module Nodes = Gen.Make() in
  let nodes = Nodes.get_generator () in
  let table = Hashtbl.create 7 in
  let rec visit lookahead (lazy {lvalue; lnext}, acc) nts =
    let acc =
      IndexMap.fold begin fun nt lookahead' acc ->
        let lookahead = Terminal.intersect g lookahead lookahead' in
        if IndexSet.is_empty lookahead then
          acc
        else
          IndexSet.fold begin fun lr1 acc ->
            goto (Transition.find_goto g lr1 nt) lookahead :: acc
          end lvalue acc
      end nts acc
    in
    (lnext, acc)
  and goto gt lookahead =
    let key = (gt, lookahead) in
    match Hashtbl.find_opt table key with
    | Some index -> index
    | None ->
      let index = Gen.reserve nodes in
      Hashtbl.add table key (Gen.index index);
      let tr = Transition.of_goto g gt in
      let predecessors = Lr1.predecessors g (Transition.source g tr) in
      let _, targets =
        List.fold_left
          (visit lookahead)
          (predecessors.lnext, [])
          grcs.:(gt).reductions
      in
      Gen.commit nodes index (key, targets);
      Gen.index index
  in
  let shifts = Transition.shift g in
  let shift =
    Vector.init shifts @@ fun sh ->
    let tr = Transition.of_shift g sh in
    let tgt = Transition.target g tr in
    let predecessors = Lr1.predecessors g (Transition.source g tr) in
    let _, targets =
      List.fold_left
        (visit (Terminal.all g))
        (lazy predecessors, [])
        rcs.:(tgt).reductions
    in
    targets
  in
  let nodes = Gen.freeze nodes in
  stopwatch 2 "built graph";
  let count = Sum.cardinal shifts Nodes.n in
  let succ f node =
    let targets = match Sum.prj shifts node with
      | L sh -> shift.:(sh)
      | R node -> snd nodes.:(node)
    in
    List.iter (fun node' -> f (Sum.inj_r shifts node')) targets
  in
  let scc = Tarjan.indexed_scc count ~succ in
  stopwatch 2 "built scc";
  let pred = Vector.make count IndexSet.empty in
  Index.rev_iter count begin fun node ->
    succ (fun node' -> pred.@(node') <- IndexSet.add node) node
  end;
  let pred f i = IndexSet.iter f pred.:(i) in
  stopwatch 2 "precomputed predecessors";
  let decompose index =
    match Sum.prj shifts index with
    | L sh -> (Terminal.all g, Transition.of_shift g sh)
    | R node ->
      let (gt, la), _ = nodes.:(node) in
      (la, Transition.of_goto g gt)
  in
  let sh_predecessors =
    Vector.init count
      (fun index ->
         match Sum.prj shifts index with
         | L sh -> IndexSet.singleton sh
         | R _ -> IndexSet.empty
      )
      (*(fun index -> IndexSet.singleton (snd (decompose index)))*)
  in
  stopwatch 2 "initialized shift predecessors";
  let lr_predecessors =
    Vector.init count
      (fun index ->
         let _, tr = decompose index in
         IndexSet.singleton (Transition.target g tr)
      )
      (*(fun index -> IndexSet.singleton (snd (decompose index)))*)
  in
  stopwatch 2 "initialized lr predecessors";
  Tarjan.iter_backward scc ~pred begin fun ~cluster ~links ->
    let all = IndexSet.union cluster links in
    let sh = IndexSet.bind all (Vector.get sh_predecessors) in
    let lr = IndexSet.bind all (Vector.get lr_predecessors) in
    IndexSet.iter begin fun node ->
      sh_predecessors.:(node) <- sh;
      lr_predecessors.:(node) <- lr;
    end cluster
  end;
  stopwatch 2 "closed predecessors";
  let result = Vector.make shifts (IndexSet.empty, IndexSet.empty) in
  Vector.rev_iteri2 begin fun index shs lrs ->
    let lookahead, tr = decompose index in
    IndexSet.rev_iter begin fun tr' ->
      match Transition.split g tr' with
      | L _gt -> ()
      | R sh ->
        if IndexSet.mem (Transition.shift_symbol g sh) lookahead then (
          let shs', lrs' = result.:(sh) in
          result.:(sh) <- (IndexSet.union shs shs', IndexSet.union lrs lrs')
        )
    end (Transition.successors g (Transition.target g tr))
  end sh_predecessors lr_predecessors;
  stopwatch 2 "filled result";
  stopwatch 1 "fast_enum 2";
  result

let trigrams (type g)
    (g : g grammar)
    (enum : (g shift_transition, g shift_transition indexset * g lr1 indexset) vector)
  =
  (* Terminals that can follow a shift transition *)
  let after = Vector.make (Transition.shift g) IndexSet.empty in
  Vector.iteri begin fun shift (before, _) ->
    let add_term = IndexSet.add (Transition.shift_symbol g shift) in
    IndexSet.iter (fun sh -> after.@(sh) <- add_term) before
  end enum;
  stopwatch 2 "Indexed lookaheads";
  (* *)
  let trie =
    Vector.init (Terminal.cardinal g) @@ fun _ ->
    Vector.init (Terminal.cardinal g) @@ fun _ ->
    Vector.make (Terminal.cardinal g) IndexSet.empty
  in
  stopwatch 2 "Allocated trie";
  Vector.rev_iteri2 begin fun middle (before, _) after ->
    let level = trie.:(Transition.shift_symbol g middle) in
    IndexSet.rev_iter begin fun term ->
      let level = level.:(term) in
      IndexSet.rev_iter begin fun sh ->
        let _, lr1 = enum.:(sh) in
        level.@(Transition.shift_symbol g sh) <- IndexSet.union lr1
      end before
    end after
  end enum after;
  stopwatch 2 "Indexed trie";
  let uniq_sets = Hashtbl.create 7 in
  Vector.iter (Vector.iter (Vector.iter (fun set ->
      if not (Hashtbl.mem uniq_sets set) then
        Hashtbl.add uniq_sets set ();
    ))) trie;
  Printf.eprintf "Unique sets: %d\n" (Hashtbl.length uniq_sets)
