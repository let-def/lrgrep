open Utils
open Fix.Indexing

module Make
    (Info : Mid.Info.S)
    (Reachability : Mid.Reachability.S with module Info := Info)
    (Viable : Mid.Viable_reductions.S with module Info := Info)
    (Lrc : Mid.Lrc.S with module Info := Info)
    (Reach : Mid.Reachable_reductions2.S
     with module Info := Info
      and module Viable := Viable
      and module Lrc := Lrc)
    ()
=
struct
  open Info

  let output_raw =
    let output_terminal oc t =
      output_char oc ' ';
      output_string oc (Terminal.to_string t)
    in
    let output_item oc item =
      let (prod, dot) = Item.prj item in
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
    in
    let output_items oc items =
      output_string oc " [";
      List.iter (output_item oc) items;
      output_string oc " ]"
    in
    fun oc (entrypoint, terminals, lookaheads, items) ->
      print_string entrypoint;
      List.iter (output_terminal oc) terminals;
      print_string " @";
      IndexSet.iter (output_terminal oc) lookaheads;
      List.iter (output_items oc) items;
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
      let (prod, dot) = Item.prj item in
      Printf.fprintf oc
        "{\"lhs\":%a,\"rhs\":%a,\"dot\":%d}"
        output_symbol
          (Symbol.inj_r (Production.lhs prod))
        (output_list output_symbol)
          (Array.to_list (Production.rhs prod))
        dot
    in
    let output_terminal oc t =
      output_symbol oc (Symbol.inj_l t)
    in
    fun oc (entrypoint, terminals, lookaheads, items) ->
      Printf.fprintf oc
        "{\"entrypoint\":%S,\"sentence\":%a,\"lookaheads\":%a,\"states\":%a}\n"
        entrypoint
        (output_list output_terminal) terminals
        (output_list output_terminal) (IndexSet.elements lookaheads)
        (output_list (output_list output_item))  items

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
    Mid.Reachable_reductions2.Covering_tree
      (Info)
      (Viable)
      (Lrc)
      (Reach)
      ()

  module Coverage = struct

    let lr1_of state =
      match Reach.Source.prj (Vector.get Reach.states state).config.source with
      | L viable -> (Viable.get_config viable).top
      | R lr1 -> lr1

    let items_from_suffix suffix =
      let items_of_state state = Lr1.lr0_items (lr1_of state) in
      List.rev_map items_of_state suffix

    let form_from_suffix suffix =
      let get_lrcs state = (Vector.get Reach.states state).config.lrcs in
      let rec loop = function
        | [] -> assert false
        | [state] ->
          Form_generator.top
            (Lrc.first_lrc_of_lr1 (lr1_of state))
        | state :: (state' :: _ as suffix) ->
          let red = IndexSet.choose (Reach.reductions state' state) in
          Form_generator.reduce (loop suffix)
            ~potential:(get_lrcs state)
            ~length:(Production.length (Reduction.production red))
      in
      Form_generator.finish (loop suffix)

    let get_entrypoint lrc =
      Nonterminal.to_string
        (Option.get (Lr0.entrypoint (Lr1.to_lr0 (Lrc.lr1_of_lrc lrc))))

    let rec select_one = function
      | [] -> []
      | [x] -> [IndexSet.choose x]
      | x :: y :: ys ->
        let x = IndexSet.choose x in
        x :: select_one (IndexSet.inter (Lrc.successors x) y :: ys)

    let generate_sentence suffix lookaheads =
      let lrcs = select_one (form_from_suffix suffix) in
      let lrcs = List.rev_append (Lrc.some_prefix (List.hd lrcs)) lrcs in
      let entrypoint = get_entrypoint (List.hd lrcs) in
      let entrypoint = String.sub entrypoint 0 (String.length entrypoint - 1) in
      let cells = Sentence_gen.cells_of_lrc_list lrcs in
      let terminals = List.fold_right Sentence_gen.prepend_word cells [] in
      (entrypoint, terminals, lookaheads, items_from_suffix suffix)
  end

  let enumerate output =
    Covering_tree.enum_sentences
      (fun suffix lookaheads ->
        output (Coverage.generate_sentence suffix lookaheads))
end
