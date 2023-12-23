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

module Lrc = Mid.Lrc.Make(Info)(Reachability)
module Reach = Mid.Reachable_reductions.Make3(Info)(Viable)(Lrc)()

(*module Failure =  struct
  (*module Lrc = Mid.Lrc.Minimize(Info)(Lrc)
  module Reach = Mid.Reachable_reductions.Make3(Info)(Viable)(Lrc)()*)
  module Failure = Mid.Reachable_reductions.FailureNFA(Info)(Viable)(Lrc)(Reach)()
end*)

open Fix.Indexing

let lrc_prefix =
  let table = Vector.make Lrc.n [] in
  let todo = ref [] in
  let expand prefix state =
    match Vector.get table state with
    | [] ->
      Vector.set table state prefix;
      let prefix = state :: prefix in
      let successors = Lrc.successors state in
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

module Sentence_gen = struct
  open Info

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
end

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
          let lrcs = Misc.indexset_bind lrcs Lrc.successors in
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

module Boolvector : sig
  type 'n t

  val make : 'n cardinal -> 'n t
  val test : 'n t -> 'n index -> bool
  val set : 'n t -> 'n index -> unit
  val clear : 'n t -> 'n index -> unit

end = struct
  type 'n t = bytes

  let make c =
    let n = cardinal c in
    Bytes.make ((n + 7) lsr 3) '\000'

  let test b n =
    let n = (n : _ index :> int) in
    let cell = n lsr 3 in
    let bit = n land 7 in
    Char.code (Bytes.unsafe_get b cell) land (1 lsl bit) <> 0

  let set b n =
    let n = (n : _ index :> int) in
    let cell = n lsr 3 in
    let bit = n land 7 in
    Bytes.unsafe_set b cell
      (Char.unsafe_chr (Char.code (Bytes.unsafe_get b cell) lor (1 lsl bit)))

  let clear b n =
    let n = (n : _ index :> int) in
    let cell = n lsr 3 in
    let bit = n land 7 in
    Bytes.unsafe_set b cell
      (Char.unsafe_chr (Char.code (Bytes.unsafe_get b cell) land lnot (1 lsl bit)))
end

module Lookahead_coverage = struct
  open Info

  let lr1_of state =
    (Viable.get_config (Vector.get Reach.states state).config.viable).top

  module Cover : sig
    type n
    val n : n cardinal
    val index : Reach.n index -> n index
  end = struct
    type n = Lr0.n
    let n = Lr0.n
    let index st = Lr1.to_lr0 (lr1_of st)
  end

  let remainings () =
    let table = Vector.make Cover.n IndexSet.empty in
    Index.iter Reach.n (fun st ->
        let i = Cover.index st in
        let set = Vector.get table i in
        Vector.set table i
          (IndexSet.union (Reach.potential_reject st) set)
      );
    table

  type suffix =
    | Top of Reach.n index
    | Reduce of Reach.n index * Info.Reduction.t * suffix

  type node = {
    suffix: suffix;
    mutable committed: Terminal.set;
    mutable child: node list;
  }

  let suffix_state (Top st | Reduce (st, _, _)) = st

  let build_arborescence () =
    let remainings = remainings () in
    let visited = Boolvector.make Reach.n in
    let todo = ref [] in
    let need_visit st =
      if not (Boolvector.test visited st)
      then (Boolvector.set visited st; true)
      else false
    in
    let visit_node node =
      let st = suffix_state node.suffix in
      let i = Cover.index st in
      let remaining = Vector.get remainings i in
      node.committed <- IndexSet.inter remaining (Reach.rejected st);
      Vector.set remainings i (IndexSet.diff remaining node.committed);
      Reach.iter_targets
        (Vector.get Reach.states st).transitions
        (fun (st', red) ->
           if need_visit st' then (
             let node' = {
               suffix = Reduce (st', red, node.suffix);
               committed = IndexSet.empty;
               child = [];
             } in
             node.child <- node' :: node.child;
             Misc.push todo node'
           )
        )
    in
    let initials =
      IndexMap.fold (fun _lrc st acc ->
          assert (need_visit st);
          let node = {
            suffix = Top st;
            committed = IndexSet.empty;
            child = [];
          } in
          visit_node node;
          node :: acc
        ) Reach.initial []
    in
    while !todo <> [] do
      let todo' = !todo in
      todo := [];
      List.iter visit_node todo'
    done;
    (remainings, initials)

  let build_remainder remaining suffix =
    let remaining = ref remaining in
    let todo = ref [] in
    let visit node =
      let st = suffix_state node.suffix in
      node.committed <- IndexSet.inter !remaining (Reach.rejected st);
      if not (IndexSet.is_empty node.committed) then
        remaining := IndexSet.diff !remaining node.committed;
      Reach.iter_targets (Vector.get Reach.states st).transitions
        (fun (st', red) ->
           if not (IndexSet.disjoint !remaining (Reach.potential_reject st))
           then (
             let node' = {
               suffix = Reduce (st', red, node.suffix);
               committed = IndexSet.empty;
               child = [];
             } in
             node.child <- node' :: node.child;
             Misc.push todo node'
           )
        )
    in
    let root = {suffix; committed = IndexSet.empty; child = []} in
    visit root;
    while !todo <> [] do
      let todo' = !todo in
      todo := [];
      List.iter visit todo'
    done;
    root

  let enum_sentences f =
    let remainings, forest = build_arborescence () in
    let visited = Boolvector.make Reach.n in
    let rec visit in_remainder node =
      let processed =
        List.fold_left
          (fun acc node -> IndexSet.union (visit in_remainder node) acc)
          IndexSet.empty node.child
      in
      let processed =
        let remainder = IndexSet.diff node.committed processed in
        if IndexSet.is_empty remainder then processed else (
          f node.suffix remainder;
          IndexSet.union remainder processed
        )
      in
      let processed =
        let state = suffix_state node.suffix in
        Boolvector.set visited state;
        let index = Cover.index state in
        let remaining = Vector.get remainings index in
        if in_remainder then (
          let remaining' = IndexSet.diff remaining processed in
          if remaining != remaining' then
            Vector.set remainings index remaining';
          processed
        ) else
          let remainder =
            IndexSet.inter remaining (Reach.potential_reject state)
          in
          if IndexSet.is_empty remainder then processed else (
            let node' = build_remainder remainder node.suffix in
            let remainder' = visit true node' in
            assert (IndexSet.subset remainder remainder');
            assert (IndexSet.disjoint (Vector.get remainings index) remainder);
            IndexSet.union remainder' processed
          )
      in
      processed
    in
    List.iter (fun node -> ignore (visit false node)) forest;
    Vector.iter (fun set -> assert (IndexSet.is_empty set)) remainings

  let items_from_suffix suffix =
    let items_of_state state = Lr1.items (lr1_of state) in
    let rec loop acc = function
      | Reduce (state, _, next) ->
        loop (items_of_state state :: acc) next
      | Top state ->
        items_of_state state :: acc
    in
    loop [] suffix

  let get_entrypoint lrc =
    Nonterminal.to_string
      (Option.get (Lr0.entrypoint (Lr1.to_lr0 (Lrc.lr1_of_lrc lrc))))

  let rec select_one = function
    | [] -> []
    | [x] -> [IndexSet.choose x]
    | x :: y :: ys ->
      let x = IndexSet.choose x in
      x :: select_one (IndexSet.inter (Lrc.successors x) y :: ys)

  let print_terminal t =
    print_char ' ';
    print_string (Terminal.to_string t)

  let print_items items =
    print_string " [";
    List.iter (output_item stdout) items;
    print_string " ]"

  let form_from_suffix suffix =
    let get_lrcs state = (Vector.get Reach.states state).config.lrcs in
    let rec loop = function
      | Top state ->
        Form_generator.top
          ~potential:(get_lrcs state)
          ~top:(Lrc.first_lrc_of_lr1 (lr1_of state))
      | Reduce (state, red, suffix) ->
        Form_generator.reduce (loop suffix)
          ~potential:(get_lrcs state)
          ~length:(Production.length (Reduction.production red))
    in
    Form_generator.finish (loop suffix)

  let () = enum_sentences (fun suffix lookaheads ->
      let lrcs = select_one (form_from_suffix suffix) in
      let lrcs = List.rev_append (lrc_prefix (List.hd lrcs)) lrcs in
      let entrypoint = get_entrypoint (List.hd lrcs) in
      let entrypoint = String.sub entrypoint 0 (String.length entrypoint - 1) in
      let cells = Sentence_gen.cells_of_lrc_list lrcs in
      let word = List.fold_right Sentence_gen.prepend_word cells [] in
      print_string entrypoint;
      List.iter print_terminal word;
      print_string " @";
      IndexSet.iter print_terminal lookaheads;
      List.iter print_items (items_from_suffix suffix);
      print_newline ()
    )
end
