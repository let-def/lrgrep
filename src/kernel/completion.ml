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
    let t1 = Transition.shift_symbol g middle in
    IndexSet.rev_iter begin fun s0 ->
      let _, lr1 = enum.:(s0) in
      let t0 = Transition.shift_symbol g s0 in
      IndexSet.rev_iter begin fun t2 ->
        trie.:(t2).:(t1).@(t0) <- IndexSet.union lr1
      end after
    end before
  end enum after;
  stopwatch 2 "Computed trie";
let uniq_sets = Hashtbl.create 7 in
  Vector.iter (Vector.iter (Vector.iter (fun set ->
      if not (Hashtbl.mem uniq_sets set) then
        Hashtbl.add uniq_sets set ();
    ))) trie;
  Printf.eprintf "Unique sets: %d\n" (Hashtbl.length uniq_sets);
  let b = Buffer.create 100000 in
  let var_int i =
    assert (i >= 0);
    if i <= 127 then
      Buffer.add_int8 b i
    else if i <= 16383 then (
      Buffer.add_int8 b (0x80 lor (i lsr 8));
      Buffer.add_int8 b (i land 0xFF);
    ) else (
      assert (i <= 0x3FFFFFFF);
      Buffer.add_int8 b (0xC0 lor (i lsr 24));
      Buffer.add_int8 b ((i lsr 16) land 0xFF);
      Buffer.add_int8 b ((i lsr 8) land 0xFF);
      Buffer.add_int8 b (i land 0xFF);
    )
  in
  let store_set set =
    var_int (IntSet.cardinal set);
    let last = ref (-1) in
    IntSet.iter (fun i ->
        var_int (i - !last - 1);
        last := i;
      ) set;
  in
  let sets =
    Hashtbl.fold (fun k () acc -> (IndexSet.cardinal k, k) :: acc) uniq_sets []
    |> List.sort (fun (i,s1) (j,s2) ->
        match Int.compare j i with
        | 0 -> IndexSet.compare s1 s2
        | n -> n)
    |> List.mapi (fun i (_,set) -> (set, i+1))
  in
  let compute_one_delta candidates candidate =
    List.fold_left (fun (best_dist, _, _ as acc) (candidate', index) ->
        let dist = IntSet.hamming_distance candidate candidate' in
        if dist < best_dist then
          (dist, IntSet.xor candidate candidate', index)
        else
          acc
      ) (IntSet.cardinal candidate, candidate, 0) candidates
  in
  let rec compute_delta = function
    | [] -> []
    | (x, _) :: xs ->
      let (_, delta, index) = compute_one_delta xs x in
      (delta, index) :: compute_delta xs
  in
  let delta = compute_delta (sets : (_ IndexSet.t * _) list :> (IntSet.t * _) list) in
  List.iter (fun (delta, index) ->
      var_int index;
      store_set delta
    ) delta;
  Printf.eprintf "Sets storage size: %d\n" (Buffer.length b);
  let cul = open_out_bin "cul" in
  output_string cul (Buffer.contents b);
  close_out cul

  (*let level2 = Hashtbl.create 7 in
  let packer2 = Lrgrep_support_packer.make () in
  let level1 = Hashtbl.create 7 in
  let packer1 = Lrgrep_support_packer.make () in
  let level0 = Hashtbl.create 7 in
  let packer0 = Lrgrep_support_packer.make () in
  let index_of table packer pack key =
    match Hashtbl.find_opt table key with
    | Some promise -> promise
    | None ->
      let promise = Lrgrep_support_packer.add_row packer (pack key) in
      Hashtbl.add table key promise;
      promise
  in
  let pack_set set =
    IntSet.portable_sparse_representation set
  in
  let pack_level lvl =
    Vector.fold_righti (fun k v acc ->
        match v with
        | None -> acc
        | Some p -> (Index.to_int k, p) :: acc
      ) lvl []
  in
  let trie' =
    Vector.map (fun l0 ->
        let l0' = Vector.map (fun l1 ->
            let l1' = Vector.map (fun l2 ->
                let l2 = (l2 : _ IndexSet.t :> IntSet.t) in
                if IntSet.is_empty l2
                then None
                else Some (index_of level2 packer2 pack_set l2)
              ) l1
            in
            if Vector.for_all Option.is_none l1' then
              None
            else
              Some (index_of level1 packer1 pack_level l1')
          ) l0
        in
        if Vector.for_all Option.is_none l0' then
          None
        else
          Some (index_of level0 packer0 pack_level l0')
      ) trie
  in
  let mapping2, table2 =
    Lrgrep_support_packer.pack packer2 Fun.id
  in
  let mapping1, table1 =
    Lrgrep_support_packer.pack packer1 (Lrgrep_support_packer.resolve mapping2)
  in
  let mapping0, table0 =
    Lrgrep_support_packer.pack packer0 (Lrgrep_support_packer.resolve mapping1)
  in
  let root = Vector.map (Option.map (Lrgrep_support_packer.resolve mapping0)) trie' in
  let source2, _ = Lrgrep_support_packer.encode table2 in
  let source1, _ = Lrgrep_support_packer.encode table1 in
  let source0, _ = Lrgrep_support_packer.encode table0 in
  stopwatch 2 "Indexed trie, size: %d+%d+%d = %d, + 1 array of %d ints"
    (String.length source0)
    (String.length source1)
    (String.length source2)
    (String.length source0 + String.length source1 + String.length source2)
    (Vector.length_as_int root)
  ;
  ignore trie';*)

  (*let uniq_sets = Hashtbl.create 7 in
  Vector.iter (Vector.iter (Vector.iter (fun set ->
      if not (Hashtbl.mem uniq_sets set) then
        Hashtbl.add uniq_sets set ();
    ))) trie;
  Printf.eprintf "Unique sets: %d\n" (Hashtbl.length uniq_sets);
  let total = ref 0 in
  let var_int i =
    assert (i >= 0);
    if i <= 127 then
      incr total
    else if i <= 32767 then
      total := !total + 2
    else (
      assert (i <= 0x7FFFFFFF);
      total := !total + 4
    )
  in
  let store_set set =
    var_int (IntSet.cardinal set);
    let last = ref (-1) in
    IntSet.iter (fun i ->
        var_int (i - !last - 1);
        last := i;
      ) set;
  in
  Hashtbl.iter (fun set () ->
      store_set (set : _ IndexSet.t :> IntSet.t)
    ) uniq_sets;
  Printf.eprintf "Sets storage size: %d\n" !total*)
  (*let part =
    (IndexRefine.partition (Hashtbl.fold (fun set () acc -> set :: acc) uniq_sets []))
  in
  Printf.eprintf "Classes: %d\n" (List.length part);
  let rsets = Vector.make (Lr1.cardinal g) IntSet.empty in
  let k = ref (Hashtbl.length uniq_sets) in
  Hashtbl.iter (fun set () ->
      decr k;
      IndexSet.iter (fun lr1 -> rsets.@(lr1) <- IntSet.add !k) set
    ) uniq_sets;
  Printf.eprintf "Elements: %d\n" (Hashtbl.fold (fun k () k' -> k' + IndexSet.cardinal k) uniq_sets 0);
  if false then
  Vector.iter (fun set ->
      Printf.eprintf "- %s\n" (string_concat_map "," string_of_int (IntSet.elements set))
    ) rsets;
  let compute_delta candidates candidate =
    List.fold_left (fun best candidate' ->
        Int.min best (IntSet.hamming_distance candidate candidate')
      ) (IntSet.cardinal candidate) candidates
  in
  let rec count_delta = function
    | [] -> 0
    | x :: xs ->
      count_delta xs + compute_delta xs x
  in
  let rsets_table = Hashtbl.create 7 in
  Vector.iter (fun set ->
      Hashtbl.replace rsets_table set ();
    ) rsets;
  let rsets' =
    Hashtbl.fold (fun k () acc -> (IntSet.cardinal k, k) :: acc) rsets_table []
    |> List.sort (fun (i,_) (j,_) -> Int.compare i j)
    |> List.map snd
  in
  Printf.eprintf "Unique rsets: %d (of %d); elements: %d; delta: %d\n"
    (Hashtbl.length rsets_table) (Vector.length_as_int rsets)
    (Hashtbl.fold (fun set () sum -> sum + IntSet.cardinal set) rsets_table 0)
    (count_delta rsets');
  let compute_delta candidates candidate =
    List.fold_left (fun best candidate' ->
        Int.min best (IndexSet.hamming_distance candidate candidate')
      ) (IndexSet.cardinal candidate) candidates
  in
  let rec count_delta = function
    | [] -> 0
    | x :: xs ->
      count_delta xs + compute_delta xs x
  in
  let sets =
    Hashtbl.fold (fun k () acc -> (IndexSet.cardinal k, k) :: acc) uniq_sets []
    |> List.sort (fun (i,_) (j,_) -> Int.compare i j)
    |> List.map snd
  in
  let delta = count_delta sets in
  Printf.eprintf "Delta-tree: %d\n" delta;
  let table = Hashtbl.create 7 in
  let shared = ref 0 in
  let pop lvl = Vector.fold_left (fun acc set -> acc + IndexSet.cardinal set) 0 lvl in
  Vector.iter begin fun level ->
    Vector.iter begin fun level ->
      begin match Hashtbl.find_opt table level with
        | Some b -> if b then incr shared;
        | None -> Hashtbl.add table level (pop level > 0)
      end
    end level;
  end trie;
  Printf.eprintf "Unique-levels: %d, shared: %d\n" (Hashtbl.length table) !shared;
  Printf.eprintf "Elements: %d\n" (Hashtbl.fold (fun set _ acc -> acc + pop set) table 0);
  if false then begin
    let table = Hashtbl.create 7 in
    Vector.iter begin fun level ->
      begin match Hashtbl.find_opt table level with
        | Some () -> Printf.eprintf "- shared\n"
        | None ->
          Hashtbl.add table level ();
          let non_empty = ref 0 in
          Vector.iter begin fun level ->
            Vector.iter begin fun set ->
              if IndexSet.is_not_empty set then
                incr non_empty
            end level
          end level;
          Printf.eprintf "- nonempty: %d\n" !non_empty
      end
    end trie;
  end;
  if false then begin
    let subsets = ref 0 in
    Vector.iter begin
      Vector.iter begin fun level ->
        Hashtbl.clear uniq_sets;
        let nonempty = ref 0 in
        Vector.iter (fun set ->
            if IndexSet.is_not_empty set then (
              incr nonempty;
              if not (Hashtbl.mem uniq_sets set) then
                Hashtbl.add uniq_sets set ();
            )
          ) level;
        if !nonempty > 0 then (
          subsets := !subsets + (Hashtbl.length uniq_sets);
          let elements = Hashtbl.fold (fun k () k' -> IndexSet.cardinal k + k') uniq_sets 0 in
          let all_elements = Hashtbl.fold (fun k () k' -> IndexSet.union k k') uniq_sets IndexSet.empty in
          let common = Hashtbl.fold (fun k () k' -> IndexSet.inter k k') uniq_sets all_elements in
          let delta = Hashtbl.fold (fun k () k' -> k' + IndexSet.cardinal (IndexSet.diff all_elements k)) uniq_sets 0 in
          Printf.eprintf "- non-empty sub-sets: %d, unique sub-sets: %d (elements: %d, unique: %d, delta: %d, common: %d)\n"
            !nonempty
            (Hashtbl.length uniq_sets)
            elements (IndexSet.cardinal all_elements)
            delta (IndexSet.cardinal common)
        )
      end
    end trie;
    Printf.eprintf "Number of sub-sets: %d\n" !subsets;
    end;*)
