open Utils

(* Graph (from Wikipedia):

         [1]
          |
          v
         [2]  -
        / | ^  \
       /  |  \  \
      v   v   |  v
    [3]  [4]  | [6]
      \   /  /
       \ /  /
        v  /
        [5]
*)

let graph_successors = function
  | 1 -> [2]
  | 2 -> [3; 4; 6]
  | 3 -> [5]
  | 4 -> [5]
  | 5 -> [2]
  | 6 -> []
  | _ -> invalid_arg "graph_successors: Node is not part of graph"

let graph_predecessors = function
  | 1 -> []
  | 2 -> [1; 5]
  | 3 -> [2]
  | 4 -> [2]
  | 5 -> [3; 4]
  | 6 -> [2]
  | _ -> invalid_arg "graph_predecessors: Node is not part of graph"

let graph : int Fastdom.graph = {
  Fastdom.
  memoize = begin fun f ->
    let tbl = Hashtbl.create 7 in
    fun k ->
      try Hashtbl.find tbl k
      with Not_found ->
        let v = f k in
        Hashtbl.add tbl k v;
        v
  end;
  successors = begin fun f acc n ->
    List.fold_left f acc (graph_successors n)
  end;
}

let postorder, get_dominance = Fastdom.dominance graph 1

let%expect_test _ =
  for i = 1 to 6 do
    let info = get_dominance i in
    assert (Fastdom.is_reachable info);
    assert (postorder.(Fastdom.postorder_index info) == info);
    assert (Fastdom.node info = i);
    let preds = graph_predecessors i in
    let preds' = Fastdom.predecessors info in
    assert (
      preds |> List.for_all (fun pred ->
          List.mem (get_dominance pred) preds');
    );
    assert (
      preds' |> List.for_all (fun pred ->
          List.mem (Fastdom.node pred) preds);
    );
    Printf.printf "dominator %d = %d\n"
      i Fastdom.(node (dominator info))
  done
