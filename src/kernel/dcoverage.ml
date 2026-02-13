open Utils
open Misc
open Fix.Indexing
open Info

type ('g, 'st, 'en) state = {
  mac: 'st index;
  enu: 'en index;
  mutable accepted: 'g terminal indexset;
}

let coverage (type g r st tr lrc en)
    (*(g : g grammar)*)
    (*(branches : (g, r) Spec.branches)*)
    (machine : (g, r, st, tr) Automata.Machine.t)
    (stacks : (g, lrc) Automata.stacks)
    (enum : (g, lrc, en) Denumeration._graph)
  =
  let reachable = Vector.make (Vector.length machine.outgoing) IndexMap.empty in
  let todo = ref [] in
  (* Reached configuration `(mac,enu)` (mac a machine state, enu an enum state),
     while lookaheads `accepted` have already been accepted. *)
  let schedule mac enu accepted =
    let map = reachable.:(mac) in
    match IndexMap.find_opt enu map with
    | Some st ->
      let accepted0 = st.accepted in
      let accepted' = IndexSet.inter accepted st.accepted in
      if accepted' != accepted0 then (
        st.accepted <- accepted';
        push todo st
      )
    | None ->
      let st = {mac; enu; accepted} in
      reachable.:(mac) <- IndexMap.add enu st map;
      push todo st
  in
  let process_transition prj tr candidates =
    let mac = machine.target.:(tr) in
    let filter = machine.label.:(tr).filter in
    List.filter begin fun cand ->
      let enu = prj cand in
      if IndexSet.mem (stacks.label enum.states.:(enu).lrc) filter then (
        schedule mac enu IndexSet.empty;
        false
      ) else
        true
    end candidates
  in
  let trs =
    Option.fold ~none:IndexSet.empty
      ~some:(Vector.get machine.outgoing) machine.initial
  in
  let unhandled_initials =
    IndexMap.fold (fun _ en acc -> en :: acc) enum.initials []
    |> IndexSet.fold (process_transition Fun.id) trs
  in
  let propagate cst =
    let trs = machine.outgoing.:(cst.mac) in
    let edges = enum.states.:(cst.enu).successors in
    let unhandled =
      IndexSet.fold (process_transition (fun edge -> edge.Denumeration.target)) trs edges
    in
    ignore unhandled
  in
  fixpoint ~propagate todo;



(*let coverage (type g r st tr lrc)
    (g : g grammar)
    (branches : (g, r) Spec.branches)
    (machine : (g, r, st, tr) Automata.Machine.t)
    (stacks : (g, lrc) Automata.stacks)
    (rcs : (g lr1, g Redgraph.reduction_closure) vector)
    (positions : g positions)
    initial
  : (g, st, lrc) machine_coverage
  =
  let state_count = Vector.length machine.outgoing in
  let reached = Vector.make state_count IndexMap.empty in
  let transitions = Vector.make state_count [] in
  let unhandled_predecessors = Vector.make state_count [] in
  let pending = ref [] in
  let todo = Vector.make state_count IndexMap.empty in
  let schedule source source_position target target_position la =
    let reached = get_map reached target target_position in
    let lookahead = IndexSet.diff la !reached in
    if IndexSet.is_not_empty lookahead then (
      reached @:= IndexSet.union lookahead;
      let todo = get_map todo target target_position in
      if IndexSet.is_empty !todo then
        push pending target;
      todo @:= IndexSet.union lookahead;
      transitions.@(target) <-
        List.cons {source; source_position; target_position; lookahead}
    )
  in
  let collect_unhandled_lrc unhandled =
    List.fold_right
      (fun (_lr1,lrcs) set -> IndexSet.union lrcs set)
      unhandled IndexSet.empty
  in
  let propagate_position st lp la =
    let la =
      List.fold_left begin fun la (br, _, _) ->
        if Boolvector.test branches.is_partial br then
          la
        else
          (* FIXME: check for unreachable clauses *)
          match branches.lookaheads.:(br) with
          | None -> IndexSet.empty
          | Some la' -> IndexSet.diff la la'
      end la machine.accepting.:(st)
    in
    if IndexSet.is_not_empty la then
      let pos, lrc = unpack_position positions lp in
      match previous_position positions pos with
      | Left nt ->
        let src = stacks.label lrc in
        let tgt = Transition.find_goto_target g src nt in
        List.iteri begin fun pos' nts ->
          IndexMap.iter begin fun nt' la' ->
            let la = IndexSet.inter la la' in
            if IndexSet.is_not_empty la then
              schedule st lp st (pack_inject positions lrc nt' pos') la
          end nts
        end rcs.:(tgt).all_reductions;
        let la = IndexSet.inter la rcs.:(tgt).failing in
        if IndexSet.is_not_empty la then
          schedule st lp st (pack_position positions Opt.none lrc) la
      | Right pos' ->
        let lrcs = stacks.prev lrc in
        if IndexSet.is_empty lrcs then
          (* Only initial state has no predecessors.  But all lookaheads should
             have been handled before reaching this configuration. *)
          unhandled_predecessors.@(st) <-
            List.cons (lp, IndexSet.empty, la)
        else
          (* Group by lr1 core *)
          let lrcs = IndexSet.split_by_run stacks.label lrcs in
          let trs = machine.outgoing.:(st) in
          let process tr lrcs =
            let st' = machine.target.:(tr) in
            let filter = machine.label.:(tr).filter in
            List.filter begin fun (lr1, lrcs) ->
              if IndexSet.mem lr1 filter then (
                IndexSet.iter
                  (fun lrc' -> schedule st lp st' (pack_position positions pos' lrc') la)
                  lrcs;
                false
              ) else
                true
            end lrcs
          in
          let unhandled = IndexSet.fold process trs lrcs in
          if not (list_is_empty unhandled) then
            unhandled_predecessors.@(st) <-
              List.cons (lp, collect_unhandled_lrc unhandled, la)
  in
  let propagate st =
    let map = todo.:(st) in
    todo.:(st) <- IndexMap.empty;
    IndexMap.iter (fun lp set -> propagate_position st lp !set) map
  in
  let counter = ref 0 in
  let unhandled_initial =
    let lrcs = IndexSet.split_by_run stacks.label stacks.tops in
    let process_initial tr lrcs =
      let st = machine.target.:(tr) in
      let filter = machine.label.:(tr).filter in
      List.filter begin fun (lr1, lrcs) ->
        if IndexSet.mem lr1 filter then begin
          List.iteri begin fun pos nts ->
            IndexMap.iter begin fun nt la ->
              let pos = inject_position positions nt (pos + 1) in
              IndexSet.iter begin fun lrc ->
                let lp = pack_position positions pos lrc in
                schedule st lp st lp la
              end lrcs
            end nts
          end rcs.:(lr1).all_reductions;
          false
        end else
          true
      end lrcs
    in
    let trs =
      Option.fold ~none:IndexSet.empty
        ~some:(Vector.get machine.outgoing) initial
    in
    collect_unhandled_lrc (IndexSet.fold process_initial trs lrcs)
  in
  let total_list_elements v = Vector.fold_left (fun acc xs -> acc + List.length xs) 0 v in
  fixpoint ~counter ~propagate pending;
  stopwatch 2 "computed coverage (%d transitions, %d iterations, %d uncovered initial states, \
               %d states with uncovered predecessors)"
    (total_list_elements transitions)
    !counter
    (IndexSet.cardinal unhandled_initial)
    (total_list_elements unhandled_predecessors)
  ;
  {transitions; unhandled_initial; unhandled_predecessors}

let string_of_items_for_filter g lr0 =
  let decompose item =
    let prod, pos = Item.desc g item in
    let rhs = Production.rhs g prod in
    (Production.lhs g prod,
     Array.sub rhs 0 pos,
     Array.sub rhs pos (Array.length rhs - pos))
  in
  let lines = ref [] in
  let append item =
    let lhs, pre, post = decompose item in
    match pre with
    (* Optimization 1: skip items of the form symbol: symbol . ... *)
    | [|first|] when Index.equal (Symbol.inj_n g lhs) first -> ()
    | _ ->
      (* Optimization 2: group items of the form
         sym: α . x . β₁, sym: α . x.β₂, ...
         as sym: α . x _* *)
      match !lines with
      | (lhs', pre', post') :: rest
        when Index.equal lhs lhs' && array_equal Index.equal pre pre' ->
        begin match post', post with
          | `Suffix [||], _ | _, [||] ->
            push lines (lhs, pre, `Suffix post)
          | `Suffix post', post when Index.equal post'.(0) post.(0) ->
            lines := (lhs', pre', `Wild post.(0)) :: rest
          | `Wild post0, post when Index.equal post0 post.(0) ->
            ()
          | _ ->
            push lines (lhs, pre, `Suffix post)
        end
      | _ -> push lines (lhs, pre, `Suffix post)
  in
  IndexSet.iter append (Lr0.items g lr0);
  let print_item (lhs, pre, post) =
    let syms syms = Array.to_list (Array.map (Symbol.to_string g) syms) in
    String.concat " " @@
    (Nonterminal.to_string g lhs ^ ":")
    :: syms pre
    @ "." :: match post with
    | `Suffix post -> syms post
    | `Wild sym -> [Symbol.to_string g sym; "_*"]
  in
  List.rev_map print_item !lines

type ('g, 'lrc) uncovered_case = {
  main_pattern: 'g lr0 index;
  shared_patterns: 'g lr0 indexset;
  shared_prefix: 'lrc index list;
  suffixes: ('lrc index list * 'g terminal indexset * 'g lr0 indexset) list;
}

let uncovered_cases (type lrc)
  grammar rcs (stacks : (_, lrc) Automata.stacks) positions
  {transitions; unhandled_initial; unhandled_predecessors}
  =
  let synthesize_suffix =
    let transition_cache = Vector.make (Vector.length transitions) IndexMap.empty in
    let get_transitions st =
      match transition_cache.:(st) with
      | map when IndexMap.is_empty map ->
        let update map ct = IndexMap.update ct.target_position (cons_update ct) map in
        let map = List.fold_left update IndexMap.empty transitions.:(st) in
        transition_cache.:(st) <- map;
        map
      | map -> map
    in
    let rec aux prefix st lp la =
      let prefix = (st, lp) :: prefix in
      match IndexMap.find_opt lp (get_transitions st) with
      | None -> [prefix, la] (* Done ? *)
      | Some cts ->
        List.concat_map begin fun ct ->
          let la = IndexSet.inter la ct.lookahead in
          if IndexSet.is_empty la then
            []
          else if Index.equal st ct.source && Index.equal lp ct.source_position then
            [prefix, la]
          else
            aux prefix ct.source ct.source_position la
        end cts
    in
    aux []
  in
  let free_predecessors, enum_predecessors =
    Vector.fold_lefti begin fun acc st transitions ->
      List.fold_left begin fun (free, enum) (lp, lrcs, la) ->
        let pos, _ = unpack_position positions lp in
        match Opt.prj pos with
        | None -> ((lrcs, lazy (synthesize_suffix st lp la)) :: free, enum)
        | Some pos ->
          let rec complete_suffixes acc = function
            | 0 -> acc
            | pos ->
              let extend_path (lrc, path) =
                let path = lrc :: path in
                IndexSet.rev_map_elements (stacks.prev lrc) (fun lrc' -> (lrc', path))
              in
              complete_suffixes (List.concat_map extend_path acc) (pos - 1)
          in
          let suffix = lazy (synthesize_suffix st lp la) in
          let goto, dot = project_position positions pos in
          let completions =
            complete_suffixes
              (IndexSet.rev_map_elements lrcs (fun lrc -> (lrc, [lrc]))) (dot - 1)
          in
          let enum = list_rev_mappend
              (fun (lrc, compl) -> Enumeration.kernel lrc ~goto la, (compl, suffix))
              completions enum
          in
          (free, enum)
      end acc transitions
    end ([], []) unhandled_predecessors
  in
  (* Pursue with initials and predecessors, delegating the work to the enumeration module. *)
  let enum_initials =
    let la = Terminal.regular grammar in
    IndexSet.rev_map_elements unhandled_initial
      (fun lrc -> Enumeration.kernel lrc la, ([lrc], lazy [([], la)]))
  in
  let Enumeration.Graph graph =
    Enumeration.make_graph grammar rcs stacks
      (enum_initials @ enum_predecessors)
  in
  let cover = Enumeration.cover_all grammar rcs stacks graph in
  let mark lr1 la =
    let cover lr1 la = Enumeration.mark_covered cover (Lr1.to_lr0 grammar lr1) la in
    List.iter begin fun (stack, _) ->
      cover (List.hd stack) la
    end rcs.:(lr1).all_stacks
  in
  let direct =
    Seq.concat_map begin fun (_lrcs, lazy suffixes) ->
      Seq.map begin fun (suffix, la) ->
        List.iter begin fun (_, lp) ->
          let pos, lrc = unpack_position positions lp in
          match previous_position positions pos with
          | Either.Left nt ->
            mark (Transition.find_goto_target grammar (stacks.label lrc) nt) la
          | Either.Right _ -> ()
        end suffix;
        let pattern = ref None in
        let suffix =
          List.fold_left begin fun acc (_, lp) ->
            let pos, lrc = unpack_position positions lp in
            match previous_position positions pos with
            | Either.Left nt ->
              pattern := Some (lrc, nt);
              (*let lr1 = Transition.find_goto_target grammar (stacks.label lrc) nt in
              Printf.eprintf "goto: %s @ %s\n%s\n" (Lr1.to_string grammar lr1) (Terminal.lookaheads_to_string grammar la)
                (string_concat_map "\n" (Item.to_string grammar) (IndexSet.elements (Lr1.items grammar lr1)));*)
              acc
            | Either.Right _ -> lrc :: acc
          end [] suffix
        in
        let lrc, nt = Option.get !pattern in
        let lr1 = Transition.find_goto_target grammar (stacks.label lrc) nt in
        mark lr1 la;
        let main_pattern = Lr1.to_lr0 grammar (List.hd (fst (List.hd rcs.:(lr1).all_stacks))) in
        (*assert (IndexSet.for_all (fun it -> not (Item.is_reducible grammar it)) (Lr0.items grammar main_pattern));*)
        { main_pattern; shared_patterns = IndexSet.empty;
          shared_prefix = []; suffixes = [suffix, la, IndexSet.empty] }
      end (List.to_seq suffixes)
    end (List.to_seq free_predecessors)
  in
  let enumerated =
    Seq.filter_map begin fun {Enumeration. first=_; pattern; edges; failing; entry} ->
      let (suffix0, lazy suffixes) = entry in
      let suffixes =
        List.filter_map begin fun (suffix, la) ->
          let la' = IndexSet.inter failing la in
          if IndexSet.is_empty la' then None else
            let suffix, patterns =
              List.fold_left begin fun (acc, patterns) (_, lp) ->
                let pos, lrc = unpack_position positions lp in
                match previous_position positions pos with
                | Either.Left nt ->
                  let goto = Transition.find_goto_target grammar (stacks.label lrc) nt in
                  let visit patterns (stack, _) =
                    let lr1 = List.hd stack in
                    let lr0 = Lr1.to_lr0 grammar lr1 in
                    Enumeration.mark_covered cover lr0 la;
                    IndexSet.add lr0 patterns
                  in
                  let patterns =
                    List.fold_left visit patterns rcs.:(goto).all_stacks
                  in
                  (acc, patterns)
                | Either.Right _ ->
                  (lrc :: acc, patterns)
              end ([], IndexSet.empty) suffix
            in
            Some (suffix, la', patterns)
        end suffixes
      in
      if list_is_empty suffixes then None
      else Some (
          let patterns = ref IndexSet.empty in
          let middle =
            List.concat_map begin fun (edge : _ Enumeration.edge) ->
              let lr0 = Enumeration.get_lr0_state grammar stacks graph.ker.:(edge.source) in
              patterns := List.fold_left begin fun patterns -> function
                  | top :: _ -> IndexSet.add (Lr1.to_lr0 grammar top) patterns
                  | [] -> patterns
                end (IndexSet.add lr0 !patterns) edge.intermediate;
              edge.path
            end edges
          in
          let shared_patterns = !patterns in
          let middle = middle @ suffix0 in
          let shared_prefix, suffixes =
            match suffixes with
            | [[], la, lr0s] -> ([], [middle, la, lr0s])
            | _ -> (middle, suffixes)
          in
          assert (IndexSet.for_all (fun it -> not (Item.is_reducible grammar it)) (Lr0.items grammar pattern));
          {main_pattern=pattern; shared_patterns; shared_prefix; suffixes}
        )
    end (Enumeration.to_seq cover)
  in
  seq_memoize (Seq.append direct enumerated)

let report_cases grammar (stacks : _ Automata.stacks) reachability
    ~output ~get_prefix main_pattern cases =
  let p fmt = Printf.ksprintf output fmt in
  let p_items lr0 =
    List.iteri begin fun i line ->
      p "%c /%s\n" (if i = 0 then '|' else ' ') line
    end (string_of_items_for_filter grammar lr0);
  in
  p "Some uncovered stacks can be caught by this pattern:\n";
  p "```\n";
  p_items main_pattern;
  p "```\n";
  let samples = ref 0 in
  List.iteri begin fun i case ->
    let prefix = match case.shared_prefix with
      | [] -> []
      | lrc :: _ as prefix ->
        list_rev_mappend stacks.label (get_prefix lrc)
          (List.map stacks.label prefix)
    in
    let prefix_shared =
      not (list_is_empty prefix) &&
      (List.compare_length_with case.suffixes 1 > 0)
    in
    List.iter begin fun (suffix0, lookaheads, patterns') ->
      let suffix = List.map stacks.label suffix0 in
      incr samples;
      p "\n### Sample %d\n\n" !samples;
      p "Stacks ending in:\n\
         ```\n\
         %s\n\
         ```\n"
        (string_concat_map " " (Lr1.symbol_to_string grammar) suffix);
      p "are rejected without an error message when looking ahead at:\n\
         ```\n\
         %s\n\
         ```\n"
        (String.concat ", " (List.rev_map (Terminal.to_string grammar) (IndexSet.elements lookaheads)));
      let prefix =
        if list_is_empty prefix then
          match suffix0 with
          | [] -> []
          | hd :: _ -> List.rev_map stacks.label (get_prefix hd)
        else prefix
      in
      if (not prefix_shared || i = 0) && not (list_is_empty prefix) then
        p "Sample prefix%s:\n\
           ```\n\
           %s\n\
           ```\n"
          (if prefix_shared
           then " (shared with the next samples)"
           else "")
          (string_concat_map " " (Lr1.symbol_to_string grammar) prefix);
      let sentence =
        Sentence_generation.sentence_of_stack grammar reachability (prefix @ suffix)
      in
      p "Sample sentence:\n\
         ```\n\
         %s\n\
         ```\n"
        (string_concat_map " " (Terminal.to_string grammar) sentence);
      let patterns = IndexSet.remove case.main_pattern (IndexSet.union case.shared_patterns patterns') in
      if IndexSet.is_not_empty patterns then (
        p "Also covered by these intermediate patterns:\n\
           ```\n";
        IndexSet.iter p_items patterns;
        p "```\n"
      )
    end case.suffixes
  end cases

let report_case grammar stacks reachability ~output ~get_prefix case =
  report_cases grammar stacks reachability ~output ~get_prefix
    case.main_pattern [case]
*)
