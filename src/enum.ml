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
    depth: int;
    mutable next: (Reachable.state index * tree) list;
  }

  let add_node root state =
    let node = { depth = root.depth + 1; next = [] } in
    root.next <- (state, node) :: root.next;
    node

  let rec count (sum, steps as acc) = function
    | { depth; next = [] } -> (sum + 1, steps + depth)
    | { depth = _; next } -> List.fold_left count_transitions acc next

  and count_transitions acc (_, node) =
    count acc node

  let measure node =
    let count, steps = IndexMap.fold (fun _ node acc -> count acc node) node (0, 0) in
    Printf.sprintf "%d sentences, average length %.02f" count (float steps /. float count)

  let bfs =
    let visited = Vector.make Reachable.state false in
    let enter parent acc (st, _) =
      if Vector.get visited st then
        acc
      else (
        let node = add_node parent st in
        Vector.set visited st true;
        (node, st) :: acc
      )
    in
    let visit acc (node, st) =
      Reachable.fold_targets (enter node) acc
        (Vector.get Reachable.states st).transitions
    in
    let rec loop = function
      | [] -> ()
      | acc -> loop (List.fold_left visit [] acc)
    in
    let acc = ref [] in
    let bfs =
      IndexMap.map (fun st ->
        let node = { depth = 1; next = [] } in
        acc := Reachable.fold_targets (enter node) !acc
            (Vector.get Reachable.states st).transitions;
        node
      ) Reachable.initial
    in
    loop !acc;
    bfs

  let dfs =
    let visited = Vector.make Reachable.state false in
    let rec enter parent (st, _) =
      if not (Vector.get visited st) then
        visit (add_node parent st) st
    and visit node st =
      Vector.set visited st true;
      Reachable.iter_targets
        (Vector.get Reachable.states st).transitions
        (enter node)
    in
    IndexMap.map (fun st ->
        let node = { depth = 1; next = [] } in
        visit node st;
        node
      ) Reachable.initial


  let () = Printf.eprintf "Reduction coverage: dfs:%s, bfs:%s\n%!" (measure dfs) (measure bfs)
end

module Lookahead_coverage = struct
  open Info

  type 'a node = {
    state: 'a;
    depth: int;
    committed: Terminal.set;
    rejected: Terminal.set;
    mutable next: tree list;
  }

  and tree = (Reachable.state index * Info.Reduction.t) node

  type root = Lrc.t node

  type status = Status : {
    accepted: Terminal.set;
    node: 'a node;
  } -> status

  let root lrc ~accepted ~rejected st =
    let committed = Reachable.potential_reject_after st in
    let committed = IndexSet.diff committed accepted in
    let committed = IndexSet.diff committed rejected in
    {state = lrc; depth = 0; committed; rejected; next = []}

  let add_node parent ~committed ~rejected state =
    let node = {state; depth = parent.depth + 1; committed; rejected; next = []} in
    parent.next <- node :: parent.next;
    node

  let rec count : type a. (int * int) -> a node -> (int * int) =
    fun (sum, steps as acc) -> function
      | { next = []; depth; _ } -> (sum + 1, steps + depth)
      | { next; _ } -> List.fold_left count acc next

  let measure map =
    let count, steps =
      IndexMap.fold
        (fun _ nodes acc -> count acc nodes)
        map (0, 0)
    in
    Printf.sprintf "%d sentences, average length %.02f" count (float steps /. float count)

  let enter () =
    let node_pra = Vector.init Reachable.state Reachable.potential_reject_after in
    let node_prb = Vector.init Reachable.state Reachable.potential_reject_before in
    fun (Status status) (st, _red as tr) ->
    let rejected = IndexSet.diff (Reachable.immediate_reject st) status.accepted in
    let accepted = IndexSet.diff (Reachable.immediate_accept st) status.node.rejected in
    let rejected = IndexSet.union rejected status.node.rejected in
    let accepted = IndexSet.union accepted status.accepted in
    let prb = Vector.get node_prb st in
    let prb' = IndexSet.diff prb rejected in
    let pra = Vector.get node_pra st in
    let pra' = IndexSet.diff pra accepted in
    if prb == prb' || IndexSet.is_empty pra' then
      None
    else (
      let committed = IndexSet.diff (IndexSet.union status.node.committed pra') rejected in
      let node = add_node status.node ~committed ~rejected tr in
      Vector.set node_prb st prb';
      Vector.set node_pra st (IndexSet.diff pra pra');
      Some (Status {accepted; node})
    )

  let dfs =
    let enter = enter () in
    let rec visit status (st, _ as tr) =
      match enter status tr with
      | None -> ()
      | Some status' ->
        Reachable.iter_targets
          (Vector.get Reachable.states st).transitions
          (fun tr' -> visit status' tr');
    in
    IndexMap.mapi (fun lrc st ->
      let lr1 = Lrc.lr1_of_lrc lrc in
      let accepted = Lr1.shift_on lr1 in
      let rejected = Lr1.reject lr1 in
      let node = root lrc ~accepted ~rejected st in
      Reachable.iter_targets
        (Vector.get Reachable.states st).transitions
        (fun tr -> visit (Status {accepted; node}) tr);
      node
    ) Reachable.initial

  let bfs =
    let enter = enter () in
    let visit acc (status, (st, _ as tr)) =
      match enter status tr with
      | None -> acc
      | Some status' ->
        Reachable.fold_targets
          (fun acc tr -> (status', tr) :: acc)
          acc (Vector.get Reachable.states st).transitions
    in
    let todo, map =
      let todo = ref [] in
      let map = IndexMap.mapi (fun lrc st ->
          let lr1 = Lrc.lr1_of_lrc lrc in
          let accepted = Lr1.shift_on lr1 in
          let rejected = Lr1.reject lr1 in
          let node = root lrc ~accepted ~rejected st in
          Reachable.iter_targets
            (Vector.get Reachable.states st).transitions
            (fun tr -> Misc.push todo (Status {accepted; node}, tr));
          node
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
    Printf.eprintf "Abstract lookahead coverage: dfs:%s, bfs:%s\n" (measure dfs) (measure bfs)

  let measure_lookaheads map =
    let count = ref 0 in
    let remainder = ref 0 in
    let rec visit : type a . a node -> Terminal.n IndexSet.t =
      fun node ->
      let rejected =
        match node.next with
        | [] ->
          count := !count + IndexSet.cardinal node.rejected;
          node.rejected
        | children ->
          List.fold_left
            (fun acc node -> IndexSet.union acc (visit node))
            IndexSet.empty children
      in
      assert (IndexSet.subset node.rejected rejected);
      remainder := !remainder + IndexSet.cardinal (IndexSet.diff node.committed rejected);
      IndexSet.union rejected node.committed
    in
    IndexMap.iter (fun _ node -> ignore (visit node)) map;
    Printf.sprintf "%d sentences (%d direct, %d indirect)" (!count + !remainder) !count !remainder

  let () =
    Printf.eprintf "Concrete lookahead coverage: dfs:%s, bfs:%s\n"
      (measure_lookaheads dfs)
      (measure_lookaheads bfs)

  type suffix =
    | Top of Lrc.t
    | Reduce of (Reachable.state index * Info.Reduction.t) * suffix

  type protosentence = suffix * Terminal.set

  let enum_sentences map f =
    let rec visit_suffix : type a. suffix -> a node -> Terminal.set =
      fun suffix node ->
      let rejected =
        match node.next with
        | [] ->
          f suffix node.rejected;
          node.rejected
        | children ->
          List.fold_left
            (fun acc node -> IndexSet.union acc (visit_node suffix node))
            IndexSet.empty children
      in
      assert (IndexSet.subset node.rejected rejected);
      let remainder = IndexSet.diff node.committed rejected in
      ignore remainder; (*TODO*)
      IndexSet.union rejected node.committed
    and visit_node suffix node =
      visit_suffix (Reduce (node.state, suffix)) node
    in
    IndexMap.iter (fun lrc node -> ignore (visit_suffix (Top lrc) node)) map

  let () =
    let rec print_suffix = function
      | Top lrc ->
        let lr1 = Lrc.lr1_of_lrc lrc in
        print_string (Lr1.to_string lr1)
      | Reduce ((redn, _red), suffix) ->
        print_string ("redn:" ^ Misc.string_of_index redn ^ " ");
        print_suffix suffix
    in
    enum_sentences bfs (fun suffix lookaheads ->
        print_suffix suffix;
        print_newline ();
        print_endline (Misc.string_of_indexset ~index:Terminal.to_string lookaheads)
    )

end
