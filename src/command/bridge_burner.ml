open Fix.Indexing
open Utils
open Misc

type 'b problem = {
  (* Graph definition *)
  mutable pred_l: ('b, 'b indexset) vector;
  mutable pred_r: ('b, 'b indexset) vector;
  mutable succ_l: ('b, 'b indexset) vector;
  mutable succ_r: ('b, 'b indexset) vector;
  mutable bridges: ('b, 'b indexset) vector;

  (* Worklist for immediate simplifications (Khan's algorithm *)
  mutable pending_trivial: 'b index list;

  (* Union-find on active bridges *)
  mutable repr: ('b, 'b index) vector;
  mutable cluster: ('b, 'b indexset) vector;

  (* Solution being constructed *)
  mutable burned: 'b indexset;
}

let make (type b) (n : b cardinal) = {
  pred_l  = Vector.make n IndexSet.empty;
  pred_r  = Vector.make n IndexSet.empty;
  succ_l  = Vector.make n IndexSet.empty;
  succ_r  = Vector.make n IndexSet.empty;
  bridges = Vector.make n IndexSet.empty;
  pending_trivial = [];
  burned  = IndexSet.empty;
  repr = Vector.init n Fun.id;
  cluster = Vector.make n IndexSet.empty;
}

let link_l (type b) (t : b problem) (x : b index) (y : b index) =
  t.succ_l.@(x) <- IndexSet.add y;
  t.pred_l.@(y) <- IndexSet.add x

let link_r (type b) (t : b problem) (x : b index) (y : b index) =
  t.succ_r.@(x) <- IndexSet.add y;
  t.pred_r.@(y) <- IndexSet.add x

let link_b (type b) (t : b problem) (x : b index) (y : b index) =
  if not (Index.equal x y) then begin
    t.bridges.@(x) <- IndexSet.add y;
    t.bridges.@(y) <- IndexSet.add x
  end

let is_trivial (type b) (t : b problem) (b : b index) =
  IndexSet.is_empty t.bridges.:(b) && begin
    let pl = IndexSet.is_empty t.pred_l.:(b) in
    let pr = IndexSet.is_empty t.pred_r.:(b) in
    let sl = IndexSet.is_empty t.succ_l.:(b) in
    let sr = IndexSet.is_empty t.succ_r.:(b) in
    (* Not already disconnnected *)
    not (pl && pr && sl && sr) && (
      (pl && pr) || (* 0 in-degree *)
      (sl && sr) || (* 0 out-degree *)
      (pl && sl) || (* 0 L-connection *)
      (pr && sr) (* 0 R-connection *)
    )
  end

let schedule_if_trivial t b =
  if is_trivial t b then
    t.pending_trivial <- b :: t.pending_trivial

let remove_element t b arr from =
  let set = arr.:(from) in
  assert (IndexSet.mem b set);
  let set = IndexSet.remove b set in
  arr.:(from) <- set;
  if IndexSet.is_empty set then
    schedule_if_trivial t from

let link2 t s1 link s2 =
  if IndexSet.is_not_empty s2 then
    IndexSet.iter (fun x -> IndexSet.iter (link t x) s2) s1

let is_connected t b =
  IndexSet.is_not_empty t.pred_l.:(b) ||
  IndexSet.is_not_empty t.pred_r.:(b) ||
  IndexSet.is_not_empty t.succ_l.:(b) ||
  IndexSet.is_not_empty t.succ_r.:(b) ||
  IndexSet.is_not_empty t.bridges.:(b)

let clear_relation t b forward backward =
  IndexSet.iter (remove_element t b backward) forward.:(b);
  forward.:(b) <- IndexSet.empty

let clear_relations t b =
  clear_relation t b t.pred_l t.succ_l;
  clear_relation t b t.pred_r t.succ_r;
  clear_relation t b t.succ_l t.pred_l;
  clear_relation t b t.succ_r t.pred_r;
  clear_relation t b t.bridges t.bridges

let contract t b =
  let pred_l = t.pred_l.:(b) in
  let succ_l = t.succ_l.:(b) in
  let pred_r = t.pred_r.:(b) in
  let succ_r = t.succ_r.:(b) in
  link2 t pred_l link_l succ_l;
  link2 t pred_l link_l succ_r;
  link2 t pred_r link_l succ_l;
  link2 t pred_l link_r succ_r;
  link2 t pred_r link_r succ_l;
  link2 t pred_r link_r succ_r;
  clear_relations t b

let burn (type b) (t : b problem) (b : b index) =
  clear_relations t b;
  t.burned <- IndexSet.add b t.burned

let rec flush_pending t =
  match t.pending_trivial with
  | [] -> ()
  | pending ->
    t.pending_trivial <- [];
    List.iter (contract t) pending;
    flush_pending t

(* Reconstruct union-find on the fly *)
let rec uf_find t i =
  let i' = t.repr.:(i) in
  if i = i' then i else
    let i'' = uf_find t i' in
    if i' <> i'' then
      t.repr.:(i) <- i'';
    i''

let uf_union t i j =
  let i = uf_find t i and j = uf_find t j in
  if i <> j then t.repr.:(i) <- j

let fold_clusters t ~iter acc f =
  (* Reset *)
  iter (fun b -> t.repr.:(b) <- b; t.cluster.:(b) <- IndexSet.empty);
  (* Union *)
  iter (fun b -> IndexSet.iter (uf_union t b) t.bridges.:(b));
  (* Cluster *)
  iter (fun b -> t.cluster.@(uf_find t b) <- IndexSet.add b);
  (* Return *)
  let acc = ref acc in
  iter begin fun b ->
    if Index.equal b (uf_find t b) then
      acc := f t.cluster.:(b) !acc
  end;
  !acc

let solve t =
  let n = Vector.length t.bridges in
  (* Immediate simplifications *)
  Index.iter n (fun b -> if is_trivial t b then contract t b);
  flush_pending t;
  (* Non-simplifiable elements *)
  let get_remaining () =
    Index.fold n [] (fun acc b -> cons_if (is_connected t b) b acc)
  in
  let filter_remaining remaining =
    flush_pending t;
    List.filter (is_connected t) remaining
  in
  let select scoring x (score, _ as candidate) =
    let score' : int = scoring x in
    if score' > score then (score', x) else candidate
  in
  (* Heuristic: pick a bridge to burn in a cluster *)
  let eval_element x =
    IndexSet.cardinal t.pred_l.:(x) +
    IndexSet.cardinal t.pred_r.:(x) +
    IndexSet.cardinal t.succ_l.:(x) +
    IndexSet.cardinal t.succ_r.:(x)
  in
  let pick_in_cluster cluster =
    let x = IndexSet.choose cluster in
    if IndexSet.is_singleton cluster
    then x
    else snd (IndexSet.fold (select eval_element) cluster (0, x))
  in
  let iter remaining f = List.iter f remaining in
  (* Phase 1: simplify trivial clusters, break ones with internal loops *)
  begin
    let break_internal cluster acc =
      if
        (* Contract trivial cluster *)
        let in_l_0 = IndexSet.for_all (fun b -> IndexSet.is_empty t.pred_l.:(b)) cluster in
        let in_r_0 = IndexSet.for_all (fun b -> IndexSet.is_empty t.pred_r.:(b)) cluster in
        let out_l_0 = IndexSet.for_all (fun b -> IndexSet.is_empty t.succ_l.:(b)) cluster in
        let out_r_0 = IndexSet.for_all (fun b -> IndexSet.is_empty t.succ_r.:(b)) cluster in
        (in_l_0 && in_r_0) || (out_l_0 && out_r_0) || (in_l_0 && out_l_0) || (in_r_0 && out_r_0)
      then (
        IndexSet.iter (contract t) cluster;
        List.filter (fun b -> not (IndexSet.mem b cluster)) acc
      ) else if
        (* Check for internal loops *)
        IndexSet.for_all (fun b -> IndexSet.disjoint cluster t.pred_l.:(b)) cluster &&
        IndexSet.for_all (fun b -> IndexSet.disjoint cluster t.pred_r.:(b)) cluster
      then
        acc
      else (
        (* Internal loop *)
        let b = pick_in_cluster cluster in
        burn t b;
        IndexSet.fold List.cons (IndexSet.remove b cluster) acc
      )
    in
    let rec loop = function
      | [] -> ()
      | remaining ->
        flush_pending t;
        loop (fold_clusters t ~iter:(iter remaining) [] break_internal)
    in
    loop (get_remaining ());
  end;
  (* Cluster simplification loop *)
  let eval_cluster_rel cluster arr =
    IndexSet.diff
      (IndexSet.fold (fun b s -> IndexSet.union arr.:(b) s) cluster IndexSet.empty)
      cluster
    |> IndexSet.cardinal
  in
  let eval_cluster cluster =
    let pl = eval_cluster_rel cluster t.pred_l in
    let pr = eval_cluster_rel cluster t.pred_r in
    let sl = eval_cluster_rel cluster t.succ_l in
    let sr = eval_cluster_rel cluster t.succ_r in
    pl * sr + pr * sl + Int.max pl sr + Int.max pr sl
  in
  let rec loop remaining =
    let _, cluster =
      fold_clusters t ~iter:(iter remaining) ((-1), IndexSet.empty) (select eval_cluster)
    in
    if IndexSet.is_not_empty cluster then (
      let b = pick_in_cluster cluster in
      burn t b;
      loop (filter_remaining remaining)
    )
  in
  loop (get_remaining ());
  t.burned
