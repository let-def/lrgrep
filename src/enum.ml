open Utils
open Fix.Indexing

module Make
    (Info : Kernel.Info.S)
    (Reachability : Kernel.Reachability.S with module Info := Info)
    (Viable : Kernel.Viable_reductions.S with module Info := Info)
    (Lrc : Kernel.Lrc.S with module Info := Info)
    (Reach : Kernel.Reachable_reductions.S
     with module Info := Info
      and module Viable := Viable
      and module Lrc := Lrc)
    ()
=
struct
  open Info

  let get_reduce_filter st =
    let desc = Vector.get Reach.states st in
    let get_items lr1 =
      IndexSet.filter (fun it -> not (Item.is_reducible it)) (Lr1.items lr1)
    in
    let reduce, filter =
      match Reach.Source.prj desc.config.source with
      | L viable ->
        let stack = Viable.get_stack viable in
        (List.tl (List.rev stack), List.hd stack)
      | R lr1 -> ([], lr1)
    in
    let reduce = List.filter_map Lr1.incoming reduce in
    let filter = get_items filter in
    (reduce, filter)


  let hashtable = Hashtbl.create 7

  let () = Index.iter Reach.n begin fun st ->
      match Reach.rejectable st with
      | lookaheads, [] ->
        let key = get_reduce_filter st in
        let lookaheads =
          match Hashtbl.find_opt hashtable key with
          | None -> lookaheads
          | Some lookaheads' -> IndexSet.union lookaheads lookaheads'
        in
        Hashtbl.replace hashtable key lookaheads
      | _ -> ()
    end

  let print_items oc items =
    let first = ref true in
    IndexSet.iter (fun item ->
        if !first then first := false else
          output_char oc ' ';
        output_char oc '/';
        output_string oc (Item.to_string item)
      ) items

  let () =
    Printf.eprintf "Maximal reduce-filter patterns:\n";
    Hashtbl.iter begin fun (reduce, filter) lookaheads ->
      begin match reduce with
      | [] -> print_items stderr filter
      | syms ->
        Printf.eprintf "[%s %a]"
          (Misc.string_concat_map "; " Symbol.name syms)
          print_items filter
      end;
      Printf.eprintf " @";
      let count = ref 0 in
      IndexSet.iter (fun elt ->
          incr count;
          if !count < 10 then
            Printf.eprintf " %s" (Terminal.to_string elt)
          else if !count = 10 then
            Printf.eprintf " ..."
        ) lookaheads;
      Printf.eprintf "\n"
    end hashtable

  let output_raw =
    let output_terminal oc t =
      output_char oc ' ';
      output_string oc (Terminal.to_string t)
    in
    let output_suffix oc (top, suffix) =
      output_string oc " [";
      let suffix = List.rev suffix in
      List.iteri (fun i lr1 ->
          if i > 0 then output_string oc "; ";
          output_string oc (Lr1.symbol_to_string lr1);
        ) suffix;
      if suffix <> [] then output_char oc ' ';
      print_items oc (Lr1.items top);
      output_string oc "]"
    in
    fun oc (entrypoint, terminals, lookaheads, suffixes) ->
      print_string entrypoint;
      List.iter (output_terminal oc) terminals;
      print_string " @";
      IndexSet.iter (output_terminal oc) lookaheads;
      List.iter (output_suffix oc) suffixes;
      print_newline ()

  let output_json =
    let output_symbol oc t =
      output_char oc '"';
      output_string oc (Symbol.name t);
      output_char oc '"'
    in
    let output_list f oc xs =
      output_char oc '[';
      List.iteri (fun i x ->
          if i <> 0 then output_char oc ',';
          f oc x
        ) xs;
      output_char oc ']'
    in
    let output_item oc item =
      let (prod, dot) = Item.desc item in
      Printf.fprintf oc
        "{\"lhs\":%a,\"rhs\":%a,\"dot\":%d}"
        output_symbol
          (Symbol.inj_r (Production.lhs prod))
        (output_list output_symbol)
          (Array.to_list (Production.rhs prod))
        dot
    in
    let output_state oc st =
      output_string oc (Lr1.symbol_to_string st)
    in
    let output_suffix oc (top, suffix) =
      Printf.fprintf oc
        "{\"reduce\": %a, \"filter\": %a}"
        (output_list output_state) (List.rev suffix)
        (output_list output_item) (IndexSet.elements (Lr1.items top))
    in
    let output_terminal oc t =
      output_symbol oc (Symbol.inj_l t)
    in
    fun oc (entrypoint, terminals, lookaheads, suffixes) ->
      Printf.fprintf oc
        "{\"entrypoint\":%S,\"sentence\":%a,\"lookaheads\":%a,\"states\":%a}\n"
        entrypoint
        (output_list output_terminal) terminals
        (output_list output_terminal) (IndexSet.elements lookaheads)
        (output_list output_suffix) suffixes

  module Sentence_gen =
  struct
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
        let coercion = Coercion.infix (Classes.post_transition tr) (Classes.for_lr1 yl) in
        let yi = coercion.backward.(yi) in
        Cell.encode (Tree.leaf tr) ~pre:xi ~post:yi :: cells_of_lrc_list tail

    exception Break of Terminal.t list

    let rec prepend_word cell acc =
      let open Reachability in
      let node, i_pre, i_post = Cell.decode cell in
      match Tree.split node with
      | L tr ->
        (* The node corresponds to a transition *)
        begin match Transition.split tr with
          | R shift ->
            (* It is a shift transition, just shift the symbol *)
            Transition.shift_symbol shift :: acc
          | L goto ->
            (* It is a goto transition *)
            let eqn = Tree.goto_equations goto in
            let c_pre = (Tree.pre_classes node).(i_pre) in
            let c_post = (Tree.post_classes node).(i_post) in
            if not (IndexSet.is_empty eqn.nullable_lookaheads) &&
              IndexSet.quick_subset c_post eqn.nullable_lookaheads &&
              not (IndexSet.disjoint c_pre c_post) then
              (* If a nullable reduction is possible, don't do anything *)
              acc
            else
              (* Otherwise look at all equations that define the cost of the
               goto transition and recursively visit one of minimal cost *)
              let current_cost = Analysis.cost cell in
              match
              List.find_map (fun ({lookahead; _}, node') ->
                if IndexSet.disjoint c_post lookahead then
                  (* The post lookahead class does not permit reducing this
                       production *)
                  None
                else
                  let encode = Cell.encode node' in
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
                      let index = encode ~pre:i_pre' ~post:i_post' in
                      if Analysis.cost index = current_cost then
                        (* We found a candidate of minimal cost *)
                        Some index
                      else
                        None
              ) eqn.non_nullable
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
        let current_cost = Analysis.cost cell in
        let coercion =
          Coercion.infix (Tree.post_classes l) (Tree.pre_classes r)
        in
        let l_index = Cell.encode l in
        let r_index = Cell.encode r in
        begin try
          Array.iteri (fun i_post_l all_pre_r ->
            let l_cost = Analysis.cost (l_index ~pre:i_pre ~post:i_post_l) in
            Array.iter (fun i_pre_r ->
              let r_cost = Analysis.cost (r_index ~pre:i_pre_r ~post:i_post) in
              if l_cost + r_cost = current_cost then (
                let acc = prepend_word (r_index ~pre:i_pre_r ~post:i_post) acc in
                let acc = prepend_word (l_index ~pre:i_pre ~post:i_post_l) acc in
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
    val top : Lrc.t -> t
    val reduce : potential:Lrc.set -> length:int -> t -> t
    val finish : t -> Lrc.set list
  end = struct
    type t = {
      stack: Lrc.set list;
      potential: Lrc.set;
      pushed: int;
    }

    let top lrc = { stack = []; potential = IndexSet.singleton lrc; pushed = 0 }

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

  module Covering_tree =
    Kernel.Reachable_reductions.Covering_tree
      (Info)
      (Viable)
      (Lrc)
      (Reach)
      ()

  module Coverage = struct

    let suffix_of state =
      match Reach.Source.prj (Vector.get Reach.states state).config.source with
      | L viable ->
        let st = Viable.get_stack viable in
        (List.hd st, st)
      | R lr1 -> (lr1, [])

    let form_from_reductions reds =
      let get_lrcs state = (Vector.get Reach.states state).config.lrcs in
      let rec loop = function
        | [] -> assert false
        | [state] ->
          Form_generator.top
            (Lrc.first_lrc_of_lr1 (fst (suffix_of state)))
        | state :: (state' :: _ as reds) ->
          let red = IndexSet.choose (Reach.reductions state' state) in
          Form_generator.reduce (loop reds)
            ~potential:(get_lrcs state)
            ~length:(Production.length (Reduction.production red))
      in
      Form_generator.finish (loop reds)

    let get_entrypoint lrc =
      match Lr1.is_entrypoint (Lrc.lr1_of_lrc lrc) with
      | None -> assert false
      | Some p -> Symbol.name (Production.rhs p).(0)

    let rec select_one = function
      | [] -> []
      | [x] -> [IndexSet.choose x]
      | x :: y :: ys ->
        let x = IndexSet.choose x in
        x :: select_one (IndexSet.inter (Lrc.successors x) y :: ys)

    let generate_sentence reds lookaheads =
      let lrcs = select_one (form_from_reductions reds) in
      let lrcs = List.rev_append (Lrc.some_prefix (List.hd lrcs)) lrcs in
      let entrypoint = get_entrypoint (List.hd lrcs) in
      let entrypoint = String.sub entrypoint 0 (String.length entrypoint - 1) in
      let cells = Sentence_gen.cells_of_lrc_list lrcs in
      let terminals = List.fold_right Sentence_gen.prepend_word cells [] in
      (entrypoint, terminals, lookaheads, List.rev_map suffix_of reds)
  end

  let enumerate output =
    Covering_tree.enum_sentences
      (fun suffix lookaheads ->
        output (Coverage.generate_sentence suffix lookaheads))
end
