open Fix.Indexing
open Utils
open Misc

type ('n, 'm) edges = {
  succ_l: ('n, 'm indexset) vector;
  succ_r: ('n, 'm indexset) vector;
  pred_l: ('n, 'm indexset) vector;
  pred_r: ('n, 'm indexset) vector;
}

type ('n, 'b) _t = {
  edges : ('n, ('n, 'b) Sum.n) edges;
  bridges: ('n, 'b indexset) vector;
  count: 'b cardinal;
  fresh: unit -> 'b index;
}

type 'n t = T : ('n, 'b) _t -> 'n t

let make_edges (type n) (n : n cardinal) =
  let pred_l = Vector.make n IndexSet.empty in
  let pred_r = Vector.make n IndexSet.empty in
  let succ_l = Vector.make n IndexSet.empty in
  let succ_r = Vector.make n IndexSet.empty in
  {pred_l; pred_r; succ_l; succ_r}

let make (type n) (n : n cardinal) =
  let module B = Gensym() in
  let edges = make_edges n in
  let bridges = Vector.make n IndexSet.empty in
  T {edges; bridges; count=B.n; fresh=B.fresh}

let link_l t i1 i2 =
  t.succ_l.@(i1) <- IndexSet.add i2;
  t.pred_l.@(i2) <- IndexSet.add i1

let link_r t i1 i2 =
  t.succ_r.@(i1) <- IndexSet.add i2;
  t.pred_r.@(i2) <- IndexSet.add i1

let link_left t i1 i2 =
  t.edges.succ_l.@(i1) <- IndexSet.add (Sum.inj_l i2);
  t.edges.pred_l.@(i2) <- IndexSet.add (Sum.inj_l i1)

let link_right t i1 i2 =
  t.edges.succ_r.@(i1) <- IndexSet.add (Sum.inj_l i2);
  t.edges.pred_r.@(i2) <- IndexSet.add (Sum.inj_l i1)

let link2 t s1 link s2 =
  if IndexSet.is_not_empty s2 then
    IndexSet.iter (fun x -> IndexSet.iter (link t x) s2) s1

let bridge t i1 i2 =
  let b = t.fresh () in
  t.bridges.@(i1) <- IndexSet.add b;
  t.bridges.@(i2) <- IndexSet.add b;
  b

let remove_element i arr j =
  arr.@(j) <- IndexSet.remove i

let clear_relation b forward backward =
  IndexSet.iter (remove_element b backward) forward.:(b);
  forward.:(b) <- IndexSet.empty

let clear_relations t b =
  clear_relation b t.pred_l t.succ_l;
  clear_relation b t.pred_r t.succ_r;
  clear_relation b t.succ_l t.pred_l;
  clear_relation b t.succ_r t.pred_r

(* Connect predecessors to successors *)
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

let is_connected t b =
  IndexSet.is_not_empty t.pred_l.:(b) ||
  IndexSet.is_not_empty t.pred_r.:(b) ||
  IndexSet.is_not_empty t.succ_l.:(b) ||
  IndexSet.is_not_empty t.succ_r.:(b)

let dump_graph path edges node_name =
  let oc = open_out path in
  let p fmt = Printf.kfprintf (fun oc -> output_char oc '\n') oc fmt in
  p "digraph G {";
  let pnode i = p "  p%d[label=%S];" (Index.to_int i) (node_name i) in
  let pedge i label j =
    p "  p%d -> p%d [label=%S];" (Index.to_int i) (Index.to_int j) label
  in
  Index.iter (Vector.length edges.pred_l) begin fun i ->
    if is_connected edges i then begin
      pnode i;
      IndexSet.iter (pedge i "L") edges.succ_l.:(i);
      IndexSet.iter (pedge i "R") edges.succ_r.:(i)
    end;
  end;
  p "}";
  close_out oc

let create_problem (type n b) (t : (n, b) _t) =
  let empty = Vector.make t.count IndexSet.empty in
  let edges = {
    succ_l = Vector.concat t.edges.succ_l empty;
    succ_r = Vector.concat t.edges.succ_r empty;
    pred_l = Vector.concat t.edges.pred_l empty;
    pred_r = Vector.concat t.edges.pred_r empty
  } in
  let n = Vector.length t.edges.pred_l in
  let inj_br b = Sum.inj_r n b in
  let get_br i =
    match Sum.prj n i with
    | L _ -> assert false
    | R b -> b
  in
  let node_name i = match Sum.prj n i with
    | L i -> "N" ^ string_of_index i
    | R i -> "B" ^ string_of_index i
  in
  (* Link bridges *)
  let link2' t f s1 link g s2 =
    if IndexSet.is_not_empty s2 then
      IndexSet.iter (fun x ->
          let x = f x in
          IndexSet.iter (fun y -> link t x (g y)) s2
        ) s1
  in
  let bridge_collection =
    Vector.fold_lefti begin fun acc i bs ->
      if IndexSet.is_empty bs then
        acc
      else
        let acc = cons_if (not (IndexSet.is_singleton bs)) bs acc in
        Printf.eprintf "bridges: %d\n" (IndexSet.cardinal bs);
        let i = Sum.inj_l i in
        link2' edges Fun.id edges.pred_l.:(i) link_l inj_br bs;
        link2' edges Fun.id edges.pred_r.:(i) link_r inj_br bs;
        link2' edges inj_br bs link_l Fun.id edges.succ_l.:(i);
        link2' edges inj_br bs link_r Fun.id edges.succ_r.:(i);
        IndexSet.iter begin fun b ->
          let b' = inj_br b in
          Printf.eprintf "bridge B%d has links (%d,%d,%d,%d)\n"
            (Index.to_int b)
            (IndexSet.cardinal edges.pred_l.:(b'))
            (IndexSet.cardinal edges.pred_r.:(b'))
            (IndexSet.cardinal edges.succ_l.:(b'))
            (IndexSet.cardinal edges.succ_r.:(b'))
        end bs;
        acc
    end [] t.bridges
  in
  if false then dump_graph "g1_bridged.dot" edges node_name;
  (* Eliminate nodes *)
  Index.iter n (fun i0 -> contract edges (Sum.inj_l i0));
  if false then dump_graph "g2_eliminated.dot" edges node_name;
  let burner = Bridge_burner.make t.count in
  List.iter begin fun bs ->
    IndexSet.iter (fun x -> IndexSet.iter (Bridge_burner.link_b burner x) bs) bs
  end bridge_collection;
  Index.iter t.count begin fun b ->
    let ib = inj_br b in
    IndexSet.iter (fun b' -> Bridge_burner.link_l burner b (get_br b'))
      edges.succ_l.:(ib);
    IndexSet.iter (fun b' -> Bridge_burner.link_r burner b (get_br b'))
      edges.succ_r.:(ib)
  end;
  (t.count, burner)
