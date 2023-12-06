open Utils
module StringSet = Set.Make(String)

(* Command-line parsing. *)

let opt_grammar_file = ref None
let opt_verbose = ref false

let usage =
  Printf.sprintf
    "lrgrep, a menhir lexer\n\
     usage: %s [options] <source>"
    Sys.argv.(0)

let print_version_num () =
  print_endline "0.1";
  exit 0

let print_version_string () =
  print_string "The Menhir parser lexer generator :-], version ";
  print_version_num ()

let error {Front.Syntax. line; col} fmt =
  Printf.eprintf "Error line %d, column %d: " line col;
  Printf.kfprintf (fun oc -> output_char oc '\n'; flush oc; exit 1) stderr fmt

let warn {Front.Syntax. line; col} fmt =
  Printf.eprintf "Warning line %d, column %d: " line col;
  Printf.kfprintf (fun oc -> output_char oc '\n'; flush oc) stderr fmt

let eprintf = Printf.eprintf

let specs = [
  "-g", Arg.String (fun x -> opt_grammar_file := Some x),
  " <file.cmly>  Path of the Menhir compiled grammar to analyse (*.cmly)";
  "-v", Arg.Set opt_verbose,
  " Increase output verbosity";
  "-version", Arg.Unit print_version_string,
  " Print version and exit";
  "-vnum", Arg.Unit print_version_num,
  " Print version number and exit";
]

let () = Arg.parse specs (fun arg -> failwith ("Unexpected argument: " ^ arg)) usage

let grammar_file = match !opt_grammar_file with
  | Some filename -> filename
  | None ->
    Format.eprintf "No grammar provided (-g), stopping now.\n";
    Arg.usage specs usage;
    exit 1

let () = Stopwatch.step Stopwatch.main "Beginning"

module Grammar = MenhirSdk.Cmly_read.Read(struct let filename = grammar_file end)

let () = Stopwatch.step Stopwatch.main "Loaded grammar"

type 'a closure = {
  ler: 'a;
  rel_star: 'a;
  ler_star: 'a;
}

let closure_and_reversal rel =
  let ler = Misc.relation_reverse rel in
  let rel_star = Misc.relation_closure ~reverse:ler rel in
  let ler_star = Misc.relation_closure ~reverse:rel ler in
  {ler;rel_star;ler_star}

module Info = Mid.Info.Make(Grammar)
module Viable = Mid.Viable_reductions.Make(Info)()
module Reachability = Mid.Reachability.Make(Info)()
(*module Lrc = Mid.Lrc.Make(Info)(Reachability)*)
(* Re-enable when minimization is fixed *)
(*module Lrc = Mid.Lrc.Minimize(Info)(Mid.Lrc.Make(Info)(Reachability))
  module Reachable = Mid.Reachable_reductions.Make2(Info)(Viable)(Lrc)()*)
module Reachable = Mid.Reachable_reductions.Make2(Info)(Viable)(Mid.Lrc.Make(Info)(Reachability))()

open Fix.Indexing

type tree = {
  mutable next: (Reachable.state index * tree) list;
}

let sentinel = { next = [] }

let add_node root state = 
  let node = { next = [] } in
  root.next <- (state, node) :: root.next;
  node

let rec count depth (sum, steps as acc) = function
  | { next = [] } -> (sum + 1, steps + depth)
  | { next } -> List.fold_left (count_transitions (depth + 1)) acc next

and count_transitions depth acc (_, node) =
  count depth acc node

let measure node =
  let count, steps = IndexMap.fold (fun _ node acc -> count 1 acc node) node (0, 0) in
  Printf.sprintf "%d sentences, average length %.02f" count (float steps /. float count)

module BFS_Reduction_coverage = struct
  let root =
    let tree = Vector.make Reachable.state sentinel in
    let visited st = Vector.get tree st != sentinel in
    let enter parent acc (st, _) =
      if visited st then 
        acc
      else (
        Vector.set tree st (add_node parent st);
        st :: acc
      )
    in
    let visit acc st =
      let node = Vector.get tree st in
      assert (node != sentinel);
      let _, transitions = Vector.get Reachable.states st in
      Reachable.fold_targets (enter node) acc transitions
    in
    let rec loop = function
      | [] -> ()
      | acc -> loop (List.fold_left visit [] acc)
    in
    let acc = ref [] in
    let bfs =
      IndexMap.map (fun tr -> 
          let node = { next = [] } in
          acc := Reachable.fold_targets (enter node) !acc tr;
          node
        ) Reachable.initial
    in
    loop !acc;
    bfs

  let () = Printf.eprintf "BFS Reduction coverage: %s\n" (measure root)
end

module DFS_Reduction_coverage = struct
  let root =
    let tree = Vector.make Reachable.state sentinel in
    let visited st = Vector.get tree st != sentinel in
    let rec enter parent (st, _) =
      if not (visited st) then (
        let node = add_node parent st in
        Vector.set tree st node;
        let _, tr = Vector.get Reachable.states st in
        Reachable.iter_targets tr (enter node) 
      )
    in
    IndexMap.map (fun tr -> 
      let node = { next = [] } in
      Reachable.iter_targets tr (enter node);
      node
    ) Reachable.initial

  let () = Printf.eprintf "DFS Reduction coverage: %s\n" (measure root)
end

(*module DFS_Lookahead_coverage = struct
  let root =
    let tree = Vector.init Reachable.state Reachable.potential_reject in
    let visited st = Vector.get tree st != sentinel in
    let rec enter parent st =
      if not (visited st) then (
        let node = add_node parent st in
        Vector.set tree st node;
        IndexSet.iter (enter node) (Vector.get Reachable.successors st)
      )
    in
    IndexMap.map (fun tr -> 
      let node = { next = [] } in
      Reachable.iter_targets tr (enter node);
      node
    ) Reachable.initial

  let () = Printf.eprintf "DFS Reduction coverage: %s\n" (measure root)
end*)

(*module Lookahead_coverage = struct
  let bfs =
    let visited = Vector.make Reachable.state IndexSet.empty in
    let enter (parent, la, st) acc =
      if visited st then 
        acc
      else (
        Vector.set tree st (add_node parent st);
        st :: acc
      )
    in
    let visit acc st =
      let node = Vector.get tree st in
      assert (node != sentinel);
      IndexSet.fold (enter node) (Vector.get reach_forward st) acc
    in
    let rec loop = function
      | [] -> ()
      | acc -> loop (List.fold_left visit [] acc)
    in
    let acc = ref [] in
    let bfs =
      IndexMap.map (fun tr -> 
          let node = { next = [] } in
          acc := Reachable.fold_targets (enter node) tr !acc;
          node
        ) Reachable.initial
    in
    loop !acc;
    bfs

  let () = 
    Printf.eprintf "Reduction coverage with %d sentences\n"
      (IndexMap.fold (fun _ node acc -> count acc node) bfs 0)
end*)

(*let {ler = reach_backward; 
     rel_star = reach_forward_closure;
     ler_star = reach_backward_closure}
  = closure_and_reversal reach_forward*)

(* Check for well-formed reachability

  let () =
    Vector.iteri (fun st preds ->
        IndexSet.iter (fun pred ->
            let _, tr = Vector.get Reachable.states pred in
            assert (
              Reachable.fold_targets (fun st' acc -> acc || st = st')
                tr false
            )
          ) preds
      ) reach_backward
  
  let () =
    Vector.iteri (fun st succs ->
        let _, tr = Vector.get Reachable.states st in
        Reachable.iter_targets tr (fun st' -> assert (IndexSet.mem st' succs));
        IndexSet.iter (fun st' ->
            assert (
              Reachable.fold_targets (fun st'' acc -> acc || st'' = st')
                tr false
            )
          ) succs
      ) reach_forward

  let () =
    Vector.iteri (fun st all_succs ->
        let succs = Vector.get reach_forward st in
        assert (IndexSet.subset succs all_succs);
        IndexSet.iter (fun st' -> 
            assert (IndexSet.subset (Vector.get reach_forward st') all_succs);
            assert (IndexSet.subset (Vector.get reach_forward_closure st') all_succs);
          ) succs;
      ) reach_forward_closure
*)

(*module SCC = Tarjan.IndexedSCC(struct
  type n = Reachable.state
  let n = Reachable.state
  let successors f n = IndexSet.iter f (Vector.get reach_forward n)
end)

let scc_forward = 
  Vector.map (fun nodes ->
    Misc.indexset_bind  nodes
      (fun node ->
        IndexSet.map 
          (Vector.get SCC.component)
          (Vector.get reach_forward node)
      )
  ) SCC.nodes

let {ler = scc_backward; 
     rel_star = scc_forward_closure;
     ler_star = scc_backward_closure}
  = closure_and_reversal scc_forward

let () = Stopwatch.step Stopwatch.main "Computed closures"

let unvisited = 
  ref (IndexSet.init_interval 
         (Index.of_int SCC.n 0)
         (Index.of_int SCC.n (cardinal SCC.n - 1)))

let paths = ref 0

let () =
  let pick_transition avoid i =
    match
      let is = Vector.get scc_forward i in
      IndexSet.minimum (IndexSet.inter is !unvisited)
    with
    | Some j -> Some j
    | None ->
      let measure j = 
        IndexSet.inter (Vector.get scc_forward_closure j) !unvisited
        |> IndexSet.cardinal
      in
      IndexSet.fold (fun j candidate ->
          if IndexSet.mem j !avoid then
            candidate
          else
            let count = measure j in
            match candidate with
            | None -> Some (j, count)
            | Some (_, count') ->
              if count >= count' then
                Some (j, count)
              else
                candidate
        ) (Vector.get scc_forward i) None
      |> Option.map fst
  in
  let rec loop () = 
    IndexMap.iter (fun _lrc transitions ->
        let initial = 
          Reachable.fold_targets
            (fun st acc ->
               IndexSet.add (Vector.get SCC.component st) acc)
            transitions IndexSet.empty
        in
        IndexSet.iter (fun i ->
            let avoid = ref IndexSet.empty in
            let rec loop_path i =
              match pick_transition avoid i with
              | None -> []
              | Some j ->
                unvisited := IndexSet.remove j !unvisited;
                avoid := IndexSet.add j !avoid;
                j :: loop_path j
            in
            let rec loop_cand () =
              let unvisited' = !unvisited in
              avoid := IndexSet.empty;
              let path = loop_path i in
              Printf.eprintf "length: %d\n" (List.length path);
              if unvisited' != !unvisited then (
                incr paths;
                loop_cand ()
              )
            in
            loop_cand ()
          ) initial
      ) Reachable.initial;
    if not (IndexSet.is_empty !unvisited) then (
      Printf.eprintf "paths: %d\nunvisited: %d\n%!" 
        !paths (IndexSet.cardinal !unvisited);
      loop ()
    )
  in
  loop ()*)
