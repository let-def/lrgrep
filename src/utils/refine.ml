module type DECOMPOSABLE = sig
  type 'a t
  val is_empty : 'a t -> bool
  val compare : 'a t -> 'a t -> int
  val compare_minimum : 'a t -> 'a t -> int
  val sorted_union : 'a t list -> 'a t
  val extract_unique_prefix : 'a t -> 'a t -> 'a t * 'a t
  val extract_shared_prefix : 'a t -> 'a t -> 'a t * ('a t * 'a t)
end

module type S = sig
  type 'a t
  val partition : 'a t list -> 'a t list
  val annotated_partition : ('a t * 'b) list -> ('a t * 'b list) list
  val partition_and_total : 'a t list -> 'a t list * 'a t
  val annotated_partition_and_total : ('a t * 'b) list -> ('a t * 'b list) list * 'a t
  val iter_decomposition : ('a t * 'b) list -> ('a t -> (('b -> unit) -> unit) -> unit) -> unit
  val iter_merged_decomposition : ('a t * 'b) list -> ('a t -> 'b list -> unit) -> unit
end

module Make (Set : DECOMPOSABLE) : S with type 'a t := 'a Set.t = struct
  type ('a, 'b) leftist =
    | Leaf
    | Node of ('a, 'b) leftist * 'a Set.t * 'b * ('a, 'b) leftist * int

  let singleton k v = Node (Leaf, k, v, Leaf, 1)
  let rank = function Leaf -> 0 | Node (_,_,_,_,r) -> r

  let rec merge t1 t2 = match t1,t2 with
    | Leaf, t | t, Leaf -> t
    | Node (l1, k1, v1, r1, _), Node (l2, k2, v2, r2, _) ->
      if Set.compare_minimum k1 k2 > 0 then
        merge_lt l2 k2 v2 r2 t1
      else
        merge_lt l1 k1 v1 r1 t2

  and merge_lt l k v r t2 =
    let merged = merge r t2 in (* always merge with right *)
    let rank_left = rank l and rank_right = rank merged in
    if rank_left >= rank_right
    then Node (l, k, v, merged, rank_right+1)
    else Node (merged, k, v, l, rank_left+1) (* left becomes right due to being shorter *)

  let heap_insert k v t = merge (singleton k v) t

  (*type ('a, 'b) heap_pop1 =
    | Pop1 of 'a Set.t * 'b * ('a, 'b) leftist
    | Done1

  let heap_pop1 = function
    | Leaf -> Done1
    | Node (l, k, v, r, _) ->
      Pop1 (k, v, merge l r)*)

  type ('a, 'b) pop2 =
    | Head of 'a Set.t * 'b * 'a Set.t * 'b * ('a, 'b) leftist
    | Tail of 'a Set.t * 'b
    | Done

  let heap_pop2 = function
    | Leaf -> Done
    | Node (Leaf, k, v, _, _) ->
      Tail (k, v)
    | Node (Node (ll, lk, lv, lr, _), k, v, Leaf, _) ->
      Head (k, v, lk, lv, merge ll lr)
    | Node (
        (Node (ll, lk, lv, lr, _) as l),
        k, v,
        (Node (rl, rk, rv, rr, _) as r),
        _
      ) ->
      if Set.compare_minimum lk rk <= 0
      then Head (k, v, lk, lv, merge (merge ll lr) r)
      else Head (k, v, rk, rv, merge (merge rl rr) l)

  let compute_parts xs =
    let heap, _ = List.fold_left
        (fun (h, i) s -> heap_insert s (IntSet.singleton i) h, i + 1)
        (Leaf, 0) xs
    in
    let rec aux parts heap =
      match heap_pop2 heap with
      | Head (s1, k1, s2, k2, heap) ->
        let sp, s1 = Set.extract_unique_prefix s1 s2 in
        let sc, (s1, s2) = Set.extract_shared_prefix s1 s2 in
        let parts =
          if not (Set.is_empty sp) then (sp, k1) :: parts else parts
        in
        let heap =
          if not (Set.is_empty sc)
          then heap_insert sc (IntSet.union k1 k2) heap
          else heap
        in
        let heap =
          if not (Set.is_empty s1) then heap_insert s1 k1 heap else heap
        in
        let heap =
          if not (Set.is_empty s2) then heap_insert s2 k2 heap else heap
        in
        aux parts heap
      | Tail (k, v) -> (k, v) :: parts
      | Done -> parts
    in
    aux [] heap

  let union_parts parts =
    match List.sort (fun (_, k1) (_, k2) -> IntSet.compare k1 k2) parts with
    | [] -> []
    | (s1, k1) :: rest ->
      let rec merge acc ss key = function
        | [] -> Set.sorted_union ss :: acc
        | (s, key') :: rest ->
          if IntSet.equal key key' then
            merge acc (s :: ss) key rest
          else
            merge (Set.sorted_union ss :: acc) [s] key' rest
      in
      merge [] [s1] k1 rest

  let partition xs =
    let xs = List.sort_uniq Set.compare xs in
    union_parts (compute_parts xs)

  let partition_and_total xs =
    let xs = List.sort_uniq Set.compare xs in
    let parts = compute_parts xs in
    let total = Set.sorted_union (List.rev_map fst parts) in
    let union = union_parts parts in
    union, total

  let keyed_union_parts parts =
    match List.sort (fun (_, k1) (_, k2) -> IntSet.compare k1 k2) parts with
    | [] -> []
    | (s1, k1) :: rest ->
      let rec merge acc ss key = function
        | [] -> (Set.sorted_union ss, key) :: acc
        | (s, key') :: rest ->
          if IntSet.equal key key' then
            merge acc (s :: ss) key rest
          else
            merge ((Set.sorted_union ss, key) :: acc) [s] key' rest
      in
      merge [] [s1] k1 rest

  let count = ref 0
  let largest = ref 0
  let total = ref 0

  let annotated_partition xs =
    let sets, annotations = List.split xs in
    let sets' = List.sort_uniq Set.compare sets in
    let l = List.length sets and l' = List.length sets' in
    largest := Int.max !largest l;
    total := !total + l;
    if l <> l' then
      Printf.eprintf "annotate_partition %d: %d non-unique (largest so far: %d, total: %d)\n" !count (l - l') !largest !total;
    incr count;
    let annotations = Array.of_list annotations in
    let parts = compute_parts sets in
    let union = keyed_union_parts parts in
    let annotate key =
      List.rev (IntSet.fold (fun i acc -> annotations.(i) :: acc) key [])
    in
    List.map (fun (set, key) -> set, annotate key) union

  let annotated_partition_and_total xs =
    let sets, annotations = List.split xs in
    let annotations = Array.of_list annotations in
    let parts = compute_parts sets in
    let total = Set.sorted_union (List.rev_map fst parts) in
    let union = keyed_union_parts parts in
    let annotate key =
      IntSet.fold (fun i acc -> annotations.(i) :: acc) key []
    in
    (List.map (fun (set, key) -> set, annotate key) union, total)

  type 'a join_tree =
    | Leaf of 'a
    | Join of 'a join_tree * 'a join_tree

  let rec iter_join_tree t f = match t with
    | Leaf x -> f x
    | Join (l, r) ->
      iter_join_tree l f;
      iter_join_tree r f

  let iter_decomposition (xs : ('a Set.t * 'b) list) (f : 'a Set.t -> _ -> unit) : unit =
    let heap = List.fold_left (fun h (s,a) -> heap_insert s (Leaf a) h) Leaf xs in
    let rec aux heap =
      match heap_pop2 heap with
      | Head (s1, k1, s2, k2, heap) ->
        let sp, s1 = Set.extract_unique_prefix s1 s2 in
        let sc, (s1, s2) = Set.extract_shared_prefix s1 s2 in
        if not (Set.is_empty sp) then
          f sp (iter_join_tree k1);
        let heap =
          if not (Set.is_empty sc)
          then heap_insert sc (Join (k1, k2)) heap
          else heap
        in
        let heap =
          if not (Set.is_empty s1) then heap_insert s1 k1 heap else heap
        in
        let heap =
          if not (Set.is_empty s2) then heap_insert s2 k2 heap else heap
        in
        aux heap
      | Tail (k, v) -> f k (iter_join_tree v)
      | Done -> ()
    in
    aux heap

  let rec merge_uniq compare xxs yys =
    match xxs, yys with
    | [], l | l, [] -> l
    | x :: xs, y :: ys ->
      let c = compare x y in
      if c = 0 then
        x :: merge_uniq compare xs ys
      else if c < 0 then
        x :: merge_uniq compare xs yys
      else
        y :: merge_uniq compare xxs ys

  let iter_merged_decomposition (xs : ('a Set.t * 'b) list) (f : 'a Set.t -> 'b list -> unit) : unit =
    let heap =
      let count = ref 0 in
      List.fold_left (fun h (s,a) ->
          let result = heap_insert s [!count, a] h in
          incr count;
          result
        ) Leaf xs
    in
    let cmp (i1, _) (i2, _) =
      Int.compare i1 i2
    in
    let rec aux heap =
      match heap_pop2 heap with
      | Head (s1, k1, s2, k2, heap) ->
        process s1 k1 s2 k2 heap
      | Tail (k, v) -> f k (List.map snd v)
      | Done -> ()

    and process s1 k1 s2 k2 = function
      | Node (l, s3, k3, r, _) when Set.compare s2 s3 = 0 ->
        process s1 k1 s2 (merge_uniq cmp k2 k3) (merge l r)
      | heap ->
        let sp, s1 = Set.extract_unique_prefix s1 s2 in
        let sc, (s1, s2) = Set.extract_shared_prefix s1 s2 in
        if not (Set.is_empty sp) then
          f sp (List.map snd k1);
        let heap =
          if not (Set.is_empty sc)
          then heap_insert sc (merge_uniq cmp k1 k2) heap
          else heap
        in
        let heap =
          if not (Set.is_empty s1) then heap_insert s1 k1 heap else heap
        in
        let heap =
          if not (Set.is_empty s2) then heap_insert s2 k2 heap else heap
        in
        aux heap
    in
    aux heap
end
