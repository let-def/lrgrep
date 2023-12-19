open Utils

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
  let todo = ref [] in
  let expand prefix state =
    match Vector.get table state with
    | [] ->
      Vector.set table state prefix;
      let prefix = state :: prefix in
      let successors = lrc_successors state in
      if not (IndexSet.is_empty successors) then
        Misc.push todo (successors, prefix)
    | _ -> ()
  in
  let visit (successors, prefix) =
    IndexSet.iter (expand prefix) successors
  in
  let rec loop = function
    | [] -> ()
    | other ->
      todo := [];
      List.iter visit other;
      loop !todo
  in
  Index.iter Info.Lr1.n (fun lr1 ->
      if Option.is_none (Info.Lr1.incoming lr1) then
        expand [] (Lrc.first_lrc_of_lr1 lr1)
    );
  loop !todo;
  Vector.get table

let output_item oc (prod, dot) =
  let open Info in
  output_string oc " /";
  output_string oc (Nonterminal.to_string (Production.lhs prod));
  output_char oc ':';
  let rhs = Production.rhs prod in
  for i = 0 to dot - 1 do
    output_char oc ' ';
    output_string oc (Symbol.name rhs.(i));
  done;
  output_string oc " .";
  for i = dot to Array.length rhs - 1 do
    output_char oc ' ';
    output_string oc (Symbol.name rhs.(i));
  done

let pr_lrc lrc =
  let lr1 = Lrc.lr1_of_lrc lrc in
  Info.Lr1.to_string lr1 ^ "@" ^
  if (lr1 : _ index :> int) = 601 then
    Misc.string_of_indexset
      ~index:Info.Terminal.to_string
      (Lrc.lookahead lrc)
  else
    string_of_int (Lrc.class_index lrc)

(* Validate prefixes *)
let () =
  if false then
    Index.iter Lrc.n (fun lrc ->
        let open Info in
        let lr1 = Lrc.lr1_of_lrc lrc in
        let p, n =
          match Lr1.items lr1 with
          | [] -> assert false
          | (p, n) :: other ->
            List.fold_left
              (fun (_, n as acc) (_, m as it) -> if m > n then it else acc)
              (p, n) other
        in
        let rhs = Production.rhs p in
        let prefix = lrc_prefix lrc in
        let invalid () =
          Printf.printf "Invalid prefix: %s\n"
            (Misc.string_concat_map " " pr_lrc prefix);
          Printf.printf "For state with items:\n";
          List.iter (fun x ->
              output_item stdout x;
              print_newline ()
            ) (Lr1.items lr1);
        in
        if prefix = [] then
          invalid ()
        else
          let cursor = ref (lrc_prefix lrc) in
          for i = n - 2 downto 0 do
            let valid = match !cursor with
              | [] -> false
              | hd :: tl ->
                cursor := tl;
                Lr1.incoming (Lrc.lr1_of_lrc hd) = Some rhs.(i)
            in
            if not valid then (
              invalid();
              assert false
            )
          done
      )

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

  let items_from_suffix suffix =
    let items_of_state state =
      let desc = Vector.get Reachable.states state in
      let config = Viable.get_config desc.config.source in
      Lr1.items config.top
    in
    let rec loop acc = function
      | Reduce (state, _, next) ->
        loop (items_of_state state :: acc) next
      | Top state ->
        items_of_state state :: acc
    in
    loop [] suffix

  let enum_sentences map f =
    let rec visit_node suffix node =
      let rejected = match node.next with
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
    IndexMap.iter (fun _lrc node ->
      ignore (visit_node (Top node.state) node)) map

  module Form_generator : sig
    type t
    val top : potential:Lrc.set -> top:Lrc.t -> t
    val reduce : potential:Lrc.set -> length:int -> t -> t
    val finish : t -> Lrc.set list
  end = struct
    type t = {
      stack: Lrc.set list;
      potential: Lrc.set;
      pushed: int;
    }

    let top ~potential ~top = { stack = [IndexSet.singleton top]; potential; pushed = 1 }

    let reduce ~potential ~length t =
      let pushed = t.pushed - length in
      if pushed >= 0
      then (
        assert (
          if pushed > 0
          then IndexSet.equal potential t.potential
          else IndexSet.subset potential t.potential
        );
        {t with pushed = pushed + 1; potential}
      )
      else (
        let rec expand acc lrcs = function
          | 1 -> acc
          | n ->
            let lrcs = Misc.indexset_bind lrcs Lrc.predecessors in
            expand (lrcs :: acc) lrcs (n - 1)
        in
        let rec narrow lrcs = function
          | [] -> t.stack
          | x :: xs ->
            let lrcs = Misc.indexset_bind lrcs lrc_successors in
            IndexSet.inter x lrcs :: narrow lrcs xs
        in
        let stack = narrow potential (expand [t.potential] t.potential (- pushed)) in
        {potential; stack; pushed = 1}
      )

    let finish t =
      if IndexSet.is_empty t.potential
      then t.stack
      else t.potential :: t.stack

  end

  let rec cells_of_lrc_list = function
    | [] -> assert false
    | [_] ->  []
    | (x :: (y :: _ as tail)) ->
      let xl = Lrc.lr1_of_lrc x in
      let yl = Lrc.lr1_of_lrc y in
      let yi = Lrc.class_index y in
      let tr =
        List.find
          (fun tr -> Transition.source tr = xl)
          (Transition.predecessors yl)
      in
      let open Reachability in
      let xi =
        match Classes.pre_transition tr with
        | [|c_pre|] when IndexSet.is_singleton c_pre ->
          if not (IndexSet.subset c_pre (Lrc.lookahead x)) then (
            Printf.eprintf "pre:%s expected:%s\nfrom:%s to:%s after:%s\n%!"
              (Misc.string_of_indexset ~index:Terminal.to_string c_pre)
              (Misc.string_of_indexset ~index:Terminal.to_string (Lrc.lookahead x))
              (Lr1.to_string xl)
              (Lr1.to_string yl)
              (if IndexSet.equal (Lrc.lookahead y) Terminal.all
               then "all"
               else Misc.string_of_indexset ~index:Terminal.to_string (Lrc.lookahead y))
            ;
            assert false
          );
          0
        | _ -> Lrc.class_index x
      in
      let yi = (Coercion.infix (Classes.post_transition tr) (Classes.for_lr1 yl)).backward.(yi)
      in
      Cells.encode (Tree.leaf tr) xi yi :: cells_of_lrc_list tail

  (*let expand_node xi node yi acc =
    let open Reachability in
    match Cells.cost (Cells.encode node xi yi) with
    | 0 -> acc
    | n ->
      let _null, eqns = Tree.goto_equations goto in
      let pre = Tree.pre_classes node in
      let check_eqn (node', lookahead) =
        match Coercion.pre pre (Tree.pre_classes node') with
        | None -> None
        | Some (Pre_singleton xi') ->
          if xi = xi' then
            Some (expand_transition (0,
                                     | Some Pre_identity ->
      in
      Option.get (List.find_map check_eqn eqns)*)

  exception Break of Terminal.t list

  let rec prepend_word cell acc =
    let open Reachability in
    let node, i_pre, i_post = Cells.decode cell in
    match Tree.split node with
    | L tr ->
      (* The node corresponds to a transition *)
      begin match Transition.split tr with
        | R shift ->
          (* It is a shift transition, just shift the symbol *)
          Transition.shift_symbol shift :: acc
        | L goto ->
          (* It is a goto transition *)
          let nullable, non_nullable = Tree.goto_equations goto in
          let c_pre = (Tree.pre_classes node).(i_pre) in
          let c_post = (Tree.post_classes node).(i_post) in
          if not (IndexSet.is_empty nullable) &&
             IndexSet.quick_subset c_post nullable &&
             not (IndexSet.disjoint c_pre c_post) then
            (* If a nullable reduction is possible, don't do anything *)
            acc
          else
            (* Otherwise look at all equations that define the cost of the
               goto transition and recursively visit one of minimal cost *)
            let current_cost = Cells.cost cell in
            match
              List.find_map (fun (node', lookahead) ->
                  if IndexSet.disjoint c_post lookahead then
                    (* The post lookahead class does not permit reducing this
                       production *)
                    None
                  else
                    let costs = Vector.get Cells.table node' in
                    match Tree.pre_classes node' with
                    | [|c_pre'|] when IndexSet.disjoint c_pre' c_pre ->
                      (* The pre lookahead class does not allow to enter this
                         branch. *)
                      None
                    | pre' ->
                      (* Visit all lookahead classes, pre and post, and find
                         the mapping between the parent node and this
                         sub-node *)
                      let pred_pre _ c_pre' = IndexSet.quick_subset c_pre' c_pre in
                      let pred_post _ c_post' = IndexSet.quick_subset c_post c_post' in
                      match
                        Misc.array_findi pred_pre 0 pre',
                        Misc.array_findi pred_post 0 (Tree.post_classes node')
                      with
                      | exception Not_found -> None
                      | i_pre', i_post' ->
                        let offset = Cells.offset node' i_pre' i_post' in
                        if costs.(offset) = current_cost then
                          (* We found a candidate of minimal cost *)
                          Some (Cells.encode_offset node' offset)
                        else
                          None
                ) non_nullable
            with
            | None ->
              Printf.eprintf "abort, cost = %d\n%!" current_cost;
              assert false
            | Some cell' ->
              (* Solve the sub-node *)
              prepend_word cell' acc
      end
    | R (l, r) ->
      (* It is an inner node.
         We decompose the problem in a left-hand and a right-hand
         sub-problems, and find sub-solutions of minimal cost *)
      let current_cost = Cells.cost cell in
      let coercion =
        Coercion.infix (Tree.post_classes l) (Tree.pre_classes r)
      in
      let l_index = Cells.encode l in
      let r_index = Cells.encode r in
      begin try
          Array.iteri (fun i_post_l all_pre_r ->
              let l_cost = Cells.cost (l_index i_pre i_post_l) in
              Array.iter (fun i_pre_r ->
                  let r_cost = Cells.cost (r_index i_pre_r i_post) in
                  if l_cost + r_cost = current_cost then (
                    let acc = prepend_word (r_index i_pre_r i_post) acc in
                    let acc = prepend_word (l_index i_pre i_post_l) acc in
                    raise (Break acc)
                  )
                ) all_pre_r
            ) coercion.Coercion.forward;
          assert false
        with Break acc -> acc
      end

  let () =
    let construct_form suffix =
      let get_potential state =
        (Vector.get Reachable.states state).config.lrcs
      in
      let rec loop = function
        | Top state ->
          let viable = (Vector.get Reachable.states state).config.source in
          let top = Lrc.first_lrc_of_lr1 (Viable.get_config viable).top in
          Form_generator.top ~potential:(get_potential state) ~top
        | Reduce (state, red, suffix) ->
          Form_generator.reduce (loop suffix)
            ~potential:(get_potential state)
            ~length:(Production.length (Reduction.production red))
      in
      Form_generator.finish (loop suffix)
    in
    let print_terminal t = print_char ' '; print_string (Terminal.to_string t) in
    let print_items items =
      print_string " [";
      List.iter (output_item stdout) items;
      print_string " ]";
    in
    let rec select_one = function
      | [] -> []
      | [x] -> [IndexSet.choose x]
      | x :: y :: ys ->
        let x = IndexSet.choose x in
        x :: select_one (IndexSet.inter (lrc_successors x) y :: ys)
    in
    enum_sentences dfs (fun suffix lookaheads ->
        let lrcs = select_one (construct_form suffix) in
        let lrcs = List.rev_append (lrc_prefix (List.hd lrcs)) lrcs in
        Printf.printf "form: %s\n" (Misc.string_concat_map " " pr_lrc lrcs);
        let entrypoint =
          List.hd lrcs
          |> Lrc.lr1_of_lrc
          |> Lr1.entrypoint
          |> Option.get
          |> Nonterminal.to_string
        in
        let entrypoint = String.sub entrypoint 0 (String.length entrypoint - 1) in
        let cells = cells_of_lrc_list lrcs in
        let word = List.fold_right prepend_word cells [] in
        print_string entrypoint;
        List.iter print_terminal word;
        print_string " @";
        IndexSet.iter print_terminal lookaheads;
        List.iter print_items (items_from_suffix suffix);
        print_newline ()
      )
end
