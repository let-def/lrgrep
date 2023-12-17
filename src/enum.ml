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

let lrc_successors =
  Vector.get (Misc.relation_reverse' Lrc.n Lrc.predecessors)

let lrc_prefix =
  let table = Vector.make Lrc.n [] in
  let expand prefix state acc =
    match Vector.get table state with
    | [] ->
      Vector.set table state prefix;
      let prefix = state :: prefix in
      let successors = lrc_successors state in
      if IndexSet.is_empty successors then
        acc
      else
        ((successors, prefix) :: acc)
    | _ -> acc
  in
  let visit acc (successors, prefix) =
    IndexSet.fold (expand prefix) successors acc
  in
  let rec loop = function
    | [] -> ()
    | other -> loop (List.fold_left visit [] other)
  in
  let todo = ref [] in
  Index.iter Info.Lr1.n (fun lr1 ->
      if Option.is_none (Info.Lr1.incoming lr1) then
        todo := expand [] (Lrc.first_lrc_of_lr1 lr1) !todo
    );
  loop !todo;
  Vector.get table


module Lookahead_coverage = struct
  open Info

  type node = {
    state: Reachable.state index;
    depth: int;
    committed: Terminal.set;
    rejected: Terminal.set;
    mutable next: (Info.Reduction.t * node) list;
  }

  type status = {
    accepted: Terminal.set;
    node: node;
  }

  let root state ~accepted ~rejected =
    let committed = Reachable.potential_reject_after state in
    let committed = IndexSet.diff committed accepted in
    let committed = IndexSet.diff committed rejected in
    {state; depth = 1; committed; rejected; next = []}

  let add_node parent ~committed ~rejected (state, reduction) =
    let node = {state; depth = parent.depth + 1; committed; rejected; next = []} in
    parent.next <- (reduction, node) :: parent.next;
    node

  let rec count (sum, steps as acc) = function
    | { next = []; depth; _ } -> (sum + 1, steps + depth)
    | { next; _ } -> List.fold_left count_transitions acc next

  and count_transitions acc (_, node) =
    count acc node

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
    fun status (st, _red as tr) ->
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
      Some {accepted; node}
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
      let node = root ~accepted ~rejected st in
      Reachable.iter_targets
        (Vector.get Reachable.states st).transitions
        (fun tr -> visit {accepted; node} tr);
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
          let node = root ~accepted ~rejected st in
          Reachable.iter_targets
            (Vector.get Reachable.states st).transitions
            (fun tr -> Misc.push todo ({accepted; node}, tr));
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
    let rec visit node =
      let rejected =
        match node.next with
        | [] ->
          count := !count + IndexSet.cardinal node.rejected;
          node.rejected
        | children ->
          List.fold_left
            (fun acc (_, node) -> IndexSet.union acc (visit node))
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
    | Top of Reachable.state index
    | Reduce of Reachable.state index * Info.Reduction.t * suffix

  let enum_sentences map f =
    let rec visit_node : suffix -> node -> Terminal.set =
      fun suffix node ->
      let rejected =
        match node.next with
        | [] ->
          f suffix node.rejected;
          node.rejected
        | children ->
          List.fold_left
            (fun acc node -> IndexSet.union acc (visit_child suffix node))
            IndexSet.empty children
      in
      assert (IndexSet.subset node.rejected rejected);
      let remainder = IndexSet.diff node.committed rejected in
      ignore remainder; (*TODO*)
      IndexSet.union rejected node.committed
    and visit_child suffix (reduction, node) =
      visit_node (Reduce (node.state, reduction, suffix)) node
    in
    IndexMap.iter (fun _lrc node -> ignore (visit_node (Top node.state) node)) map

  (*let prepend_reduction _st0 red st1 suffix =
    let stack =
      Misc.list_drop (Production.length (Reduction.production red))
        (Viable.get_stack (Vector.get Reachable.states st1).config.source)
    in
    List.filter_map Lr1.incoming stack @ suffix*)

  module Form_generator : sig
    type t
    val start : Lrc.set -> t
    val grow : Lrc.set -> int -> t -> t
    val finish : t -> Lrc.set list
  end = struct
    type t = Lrc.set list

    let start x = [x]

    let grow x n t =
      let y = List.hd t in
      if n <= 0 then (
        assert (IndexSet.subset x y);
        t
      ) else (
        let rec candidates acc y = function
          | 1 -> x :: acc
          | n ->
            let y' = Misc.indexset_bind y Lrc.predecessors in
            candidates (y' :: acc) y' (n - 1)
        in
        let rec prepend = function
          | [] -> assert false
          | [x] -> x :: t
          | x :: y :: ys ->
            let y' = Misc.indexset_bind x lrc_successors in
            let y' = IndexSet.inter y y' in
            assert (not (IndexSet.is_empty y'));
            x :: prepend (y' :: ys)
        in
        prepend (candidates [] y n)
      )

    (*let rec finish = function
      | [] -> []
      | x :: xs ->
        match finish xs with
        | [] -> [x]
        | x' :: _ as xs' ->
          IndexSet.inter x (Misc.indexset_bind x' Lrc.predecessors) :: xs'*)
    let finish x = x
  end

  let () =
    (*let rec print_suffix = function
      | Top state ->
        let desc = Vector.get Reachable.states state in
        let lr1 = Lrc.lr1_of_lrc (IndexSet.choose desc.config.lrcs) in
        print_string (Lr1.to_string lr1)
      | Reduce (_state, reduction, suffix) ->
        let prod = Reduction.production reduction in
        let lhs = Production.lhs prod in
        let rhs = Production.rhs prod in
        Printf.printf "[%s: %s] "
          (Nonterminal.to_string lhs)
          (Misc.string_concat_map " " Symbol.name (Array.to_list rhs));
        print_suffix suffix
    in
    let rec get_symbols = function
      | Top state ->
        let sentence =
          (Vector.get Reachable.states state).config.lrcs
          |> IndexSet.choose
          |> Lrc.lr1_of_lrc
          |> Lr1.incoming
          |> Option.to_list
        in
        state, sentence
      | Reduce (st', red, suffix) ->
        let st, sentence = get_symbols suffix in
        let sentence = prepend_reduction st' red st sentence in
        (st', sentence)
    in*)
    let print_stack state =
      let viable = (Vector.get Reachable.states state).config.source in
      let stack = Viable.get_stack viable in
      let lr1s = List.rev (List.filter_map Lr1.incoming stack) in
      Printf.printf "stack: %s\n" (Misc.string_concat_map " " Symbol.name lr1s)
    in
    let print_reduction red =
      let prod = Reduction.production red in
      let lhs = Production.lhs prod in
      let rhs = Production.rhs prod in
      Printf.printf "[%s: %s]\n"
        (Nonterminal.to_string lhs)
        (Misc.string_concat_map " " Symbol.name (Array.to_list rhs));
    in
    let consumed_symbols red state =
      let desc = Vector.get Reachable.states state in
      let stack = Viable.get_stack desc.config.source in
      let rhs = Production.rhs (Reduction.production red) in
      let count = Array.length rhs - List.length stack in
      if count > 0 then (
        let symbols = Array.sub rhs 0 count in
        Array.to_list symbols
      ) else []
    in
    let construct_form suffix =
      let rec loop =  function
        | Top state ->
          let lrcs = (Vector.get Reachable.states state).config.lrcs in
          (state, Form_generator.start lrcs)
        | Reduce (state, red, suffix) ->
          let state', gen = loop suffix in
          let lrcs = (Vector.get Reachable.states state).config.lrcs in
          let stack = Viable.get_stack (Vector.get Reachable.states state').config.source in
          let count = Production.length (Reduction.production red) - List.length stack in
          (state, Form_generator.grow lrcs count gen)
      in
      let _, gen = loop suffix in
      Form_generator.finish gen
    in
    let rec dump_suffix = function
      | Top state ->
        Printf.printf "top symbol: %s\n"
          (Misc.string_concat_map " " Symbol.name
             (Option.to_list
                (Lr1.incoming
                   (Lrc.lr1_of_lrc
                      (IndexSet.choose
                         (Vector.get Reachable.states state).config.lrcs)))));
        print_stack state;
        state
      | Reduce (state, red, suffix) ->
        let state' = dump_suffix suffix in
        let symbols = consumed_symbols red state' in
        if symbols <> [] then
          Printf.printf "symbols: %s\n"
            (Misc.string_concat_map " " Symbol.name symbols);
        print_reduction red;
        print_stack state;
        state
    in
    let dump_prefix (Top state | Reduce (state, _, _)) =
      let lrc = IndexSet.choose (Vector.get Reachable.states state).config.lrcs in
      let prefix = lrc_prefix lrc in
      let prefix = List.filter_map Lr1.incoming (List.rev_map Lrc.lr1_of_lrc prefix) in
      Printf.printf "prefix: %s\n"
        (Misc.string_concat_map " " Symbol.name prefix)
    in
    enum_sentences dfs (fun suffix lookaheads ->
        print_endline "------------";
        ignore (dump_suffix suffix);
        dump_prefix suffix;
        begin
          let rec select_one = function
            | [] -> []
            | [x] -> [IndexSet.choose x]
            | x :: y :: ys ->
              let x = IndexSet.choose x in
              x :: select_one (IndexSet.inter (lrc_successors x) y :: ys)
          in
          let form = select_one (construct_form suffix) in
          let form = lrc_prefix (List.hd form) @ form in
          print_string (
            Misc.string_concat_map " "
              (fun x -> Lr1.to_string (Lrc.lr1_of_lrc x)) form
          )
        end;
        print_newline ();
        print_endline (Misc.string_of_indexset ~index:Terminal.to_string lookaheads)
      )

end
