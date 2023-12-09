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
    depth: int;
    mutable next: (Reachable.state index * tree) list;
  }

  let sentinel = { next = []; depth = 0 }

  let add_node parent state =
    let node = {depth = parent.depth + 1; next = []} in
    parent.next <- (state, node) :: parent.next;
    node

  type status = {
    rejected: Terminal.set;
    accepted: Terminal.set;
    node: tree;
  }

  let rec count (sum, steps as acc) = function
    | { next = []; depth } -> (sum + 1, steps + depth)
    | { next; _ } -> List.fold_left count_transitions acc next

  and count_transitions acc (_st, node) = count acc node

  let measure map =
    let count, steps =
      IndexMap.fold
        (fun _ nodes acc -> List.fold_left count acc nodes)
        map (0, 0)
    in
    Printf.sprintf "%d sentences, average length %.02f" count (float steps /. float count)

  (*let node_after = Vector.make Reachable.state sentinel
  let node_before = Vector.make Reachable.state sentinel*)
  let enter () =
    let node_pra = Vector.init Reachable.state Reachable.potential_reject_after in
    let node_prb = Vector.init Reachable.state Reachable.potential_reject_before in
    fun status st ->
    let rejected = IndexSet.diff (Reachable.immediate_reject st) status.accepted in
    let accepted = IndexSet.diff (Reachable.immediate_accept st) status.rejected in
    let rejected = IndexSet.union rejected status.rejected in
    let accepted = IndexSet.union accepted status.accepted in
    let prb = Vector.get node_prb st in
    let prb' = IndexSet.diff prb rejected in
    let pra = Vector.get node_pra st in
    let pra' = IndexSet.diff pra accepted in
    if prb == prb' || IndexSet.is_empty pra' then
      None
    else (
      let node = add_node status.node st in
      Vector.set node_prb st prb';
      Vector.set node_pra st (IndexSet.diff pra pra');
      Some {accepted; rejected; node}
    )

  let dfs =
    let enter = enter () in
    let rec visit status st =
      match enter status st with
      | None -> ()
      | Some status' ->
        IndexSet.iter (fun st' -> visit status' st')
          (Vector.get Reachable.successors st);
    in
    IndexMap.mapi (fun lrc tr ->
      let lr1 = Lrc.lr1_of_lrc lrc in
      let accepted = Lr1.shift_on lr1 in
      let rejected = Lr1.reject lr1 in
      Reachable.fold_targets (fun acc (state, _) ->
        let node = {next = []; depth = 0} in
        let status = {accepted; rejected; node} in
        visit status state;
        node :: acc
      ) [] tr
    ) Reachable.initial

  let bfs =
    let enter = enter () in
    let visit acc (status, st) =
      match enter status st with
      | None -> acc
      | Some status' ->
        IndexSet.fold
          (fun st' acc -> (status', st') :: acc)
          (Vector.get Reachable.successors st) acc;
    in
    let todo, map =
      let todo = ref [] in
      let map =
        IndexMap.mapi (fun lrc tr ->
            let lr1 = Lrc.lr1_of_lrc lrc in
            let accepted = Lr1.shift_on lr1 in
            let rejected = Lr1.reject lr1 in
            Reachable.fold_targets (fun acc (state, _) ->
                let node = {next = []; depth = 0} in
                let status = {accepted; rejected; node} in
                todo := (status, state) :: !todo;
                node :: acc
              ) [] tr
          ) Reachable.initial
      in
      !todo, map
    in
    let rec loop = function
      | [] -> ()
      | todo' -> loop (List.fold_left visit [] todo')
    in
    loop todo;
    map

  let () =
    Printf.eprintf "Lookahead coverage: dfs:%s bfs:%s\n" (measure dfs) (measure bfs)
end
