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
module Lrc = Mid.Lrc.Make(Info)(Reachability)
module Reachable = Mid.Reachable_reductions.Make2(Info)(Viable)(Lrc)()

open Fix.Indexing

module Reduction_coverage = struct

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

  let bfs =
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

  let dfs =
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


  let () = Printf.eprintf "Reduction coverage: dfs:%s, bfs:%s\n%!" (measure dfs) (measure bfs)
end

module Lookahead_coverage = struct
  open Info

  type tree = {
    parent: tree;
    state: Reachable.state index;
    mutable covered: Terminal.set;
    mutable next: tree list;
  }


  type status = {
    visited: Reachable.state IndexSet.t;
    rejected: Terminal.set;
    accepted: Terminal.set;
    node: tree;
  }

  let rec count depth (sum, steps as acc) = function
    | { next = []; _ } -> (sum + 1, steps + depth)
    | { next; _ } -> List.fold_left (count (depth + 1)) acc next

  let measure map =
    let count, steps = IndexMap.fold (fun _ nodes acc ->
      List.fold_left (fun acc node -> count 1 acc node) acc nodes
    ) map (0, 0) in
    Printf.sprintf "%d sentences, average length %.02f" count (float steps /. float count)

  let update todo status state =
    let accept = IndexSet.diff (Reachable.immediate_accept state) status.rejected in
    let reject = Reachable.immediate_reject state in
    let reject = IndexSet.diff reject status.accepted in
    let rejected = IndexSet.union reject status.rejected in
    let accepted = IndexSet.union accept status.accepted in
    let node = {parent = status.node; state; covered = IndexSet.empty; next = []} in
    let visited = if rejected != status.rejected then
        IndexSet.singleton state
      else
        IndexSet.add state status.visited
    in
    let rec update_node node =
      let todo' = Vector.get todo node.state in
      let reject' = IndexSet.inter rejected todo' in
      if not (IndexSet.is_empty reject') then (
        let rem = IndexSet.diff todo' reject' in
        Printf.eprintf "Removing %d lookaheads\n%!" (IndexSet.cardinal rem);
        Vector.set todo node.state rem;
        node.covered <- IndexSet.union node.covered reject';
        update_node node.parent
      )
    in
    update_node node;
    {accepted; rejected; node; visited}

  let visit_successor todo status state =
    not (IndexSet.mem state status.visited) && (
      let pra = IndexSet.diff (Reachable.potential_reject_after state) status.accepted in
      if IndexSet.equal status.rejected pra then (
        not (IndexSet.disjoint status.rejected (Vector.get todo state))
      ) else (
        assert (IndexSet.subset status.rejected pra);
        true
      )
    )

  let root =
    let todo = Vector.init Reachable.state
        (fun st -> IndexSet.union
            (Reachable.potential_reject_after st)
            (Reachable.potential_reject_before st)
        )
    in
    let rec enter status st =
      let status = update todo status st in
      IndexSet.iter (fun st' ->
          if visit_successor todo status st' then
            enter status st'
        ) (Vector.get Reachable.successors st)
    in
    IndexMap.mapi (fun lrc tr ->
      let lr1 = Lrc.lr1_of_lrc lrc in
      let accepted = Lr1.shift_on lr1 in
      let rejected = Lr1.reject lr1 in
      Reachable.fold_targets (fun acc (state, _) ->
        Printf.eprintf "New exploration\n%!";
        let rec node = {parent = node; state; covered = rejected; next = []} in
        let status = {accepted; rejected; node; visited = IndexSet.singleton state} in
        enter status state;
        node :: acc
      ) [] tr
    ) Reachable.initial

  let () =
    Printf.eprintf "DONE\n%!";
    Printf.eprintf "DFS Lookahead coverage: %s\n" (measure root)
end
