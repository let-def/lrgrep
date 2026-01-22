open Utils
open Misc
open Fix.Indexing
open Info

(* Compact representation of a position in a rule *)
module Position = Unsafe_cardinal()
type 'g position = 'g Position.t

type 'g position_desc = 'g nonterminal index * int

type 'g positions = {
  desc: ('g position, 'g position_desc) vector;
  zero: ('g nonterminal, 'g position index) vector;
}

let make_positions (type g) (g : g grammar) : g positions =
  let length = Vector.make (Nonterminal.cardinal g) 0 in
  Index.iter (Production.cardinal g) (fun prod ->
      length.@(Production.lhs g prod) <- Int.max (Production.length g prod)
    );
  let open Position.Const(struct
      type t = g
      let cardinal =
        Vector.fold_left (+) (1 + Vector.length_as_int length) length
    end)
  in
  let desc = Vector.make' n (fun () -> Index.of_int (Nonterminal.cardinal g) 0, 0) in
  let enum = Index.enumerate n in
  let zero = Vector.mapi (fun nt count ->
      let zero = enum () in
      desc.:(zero) <- (nt, 0);
      for i = 1 to count do
        desc.:(enum ()) <- (nt, i);
      done;
      zero
    ) length
  in
  {desc; zero}

let inject_position (type g) (p : g positions) nt pos =
  assert (pos >= 0);
  let p0 = p.zero.:(nt) in
  let pn = Index.of_int (Vector.length p.desc) ((p0 :> int) + pos) in
  let (nt', _) = p.desc.:(pn)  in
  assert (Index.equal nt nt');
  Opt.some pn

let project_position (type g) (p : g positions) pos =
  p.desc.:(pos)

let previous_position (type g) (p : g positions) pos =
  match Opt.prj pos with
  | None -> Either.Right Opt.none
  | Some pos' ->
    match p.desc.:(pos') with
    | (nt, 0) -> Either.Left nt
    | _ -> Either.Right (Option.get (Index.pred pos))

let pack_position positions i j =
  Prod.inj (Opt.cardinal (Vector.length positions.desc)) i j

let pack_inject positions lrc nt pos =
  pack_position positions (inject_position positions nt pos) lrc

let unpack_position positions i =
  Prod.prj (Opt.cardinal (Vector.length positions.desc)) i

let get_map v i j =
  let map = v.:(i) in
  match IndexMap.find_opt j map with
  | Some r -> r
  | None ->
    let r = ref IndexSet.empty in
    v.:(i) <- IndexMap.add j r map;
    r

let (@:=) r f =
  r := f !r

(* Compute coverage of a machine (an automaton realizing an error
   specification).*)

type ('g, 'lrc) lrc_position = ('g position Opt.n, 'lrc) Prod.n index

type ('g, 'st, 'lrc) coverage_transition = {
  source: 'st index;
  source_position: ('g, 'lrc) lrc_position;
  target_position: ('g, 'lrc) lrc_position;
  lookahead: 'g terminal indexset;
}

type ('g, 'st, 'lrc) machine_coverage = {
  transitions: ('st, ('g, 'st, 'lrc) coverage_transition list) vector;
  unhandled_initial: 'lrc indexset;
  unhandled_predecessors: ('st, (('g, 'lrc) lrc_position * 'lrc indexset * 'g terminal indexset) list) vector;
}

let coverage (type g r st tr lrc)
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
        end rcs.:(tgt).reductions;
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
          end rcs.:(lr1).reductions;
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
     Some (Array.sub rhs pos (Array.length rhs - pos)))
  in
  let lines = ref [] in
  let append item =
    let lhs, pre, post = decompose item in
    match pre with
    (* Optimization 1: skip items of the form symbol: symbol . ... *)
    | [|first|] when Index.equal (Symbol.inj_n g lhs) first -> ()
    | _ ->
      match !lines with
      | (lhs', pre', post) :: rest
        when Index.equal lhs lhs' && array_equal Index.equal pre pre' ->
        if Option.is_some post then
          (* Optimization 2: group items of the form sym: α . β₁, sym: α . β₂, ...
             as sym: α . _* *)
          lines := (lhs', pre', None) :: rest
      | lines' -> lines := (lhs, pre, post) :: lines'
  in
  IndexSet.iter append (Lr0.items g lr0);
  let print_item (lhs, pre, post) =
    let syms syms = Array.to_list (Array.map (Symbol.name g) syms) in
    String.concat " " @@
    (Nonterminal.to_string g lhs ^ ":")
    :: syms pre
    @ "." :: match post with
    | None -> ["_*"]
    | Some post -> syms post
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
              (IndexSet.rev_map_elements lrcs (fun lrc -> (lrc, []))) (dot - 1)
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
  let direct =
    Seq.concat_map begin fun (_lrcs, lazy suffixes) ->
      Seq.map begin fun (suffix, la) ->
        List.iter begin fun (_, lp) ->
          let pos, lrc = unpack_position positions lp in
          match previous_position positions pos with
          | Either.Left nt ->
            let lr1 = Transition.find_goto_target grammar (stacks.label lrc) nt in
            Enumeration.mark_covered cover (Lr1.to_lr0 grammar lr1) la
          | Either.Right _ -> ()
        end suffix;
        let pattern = ref None in
        let suffix =
          List.fold_left begin fun acc (_, lp) ->
            let pos, lrc = unpack_position positions lp in
            match previous_position positions pos with
            | Either.Left nt ->
              let lr1 = Transition.find_goto_target grammar (stacks.label lrc) nt in
              pattern := Some (Lr1.to_lr0 grammar lr1);
              acc
            | Either.Right _ -> lrc :: acc
          end [] suffix
        in
        {
          main_pattern = Option.get !pattern;
          shared_patterns = IndexSet.empty;
          shared_prefix = [];
          suffixes = [suffix, la, IndexSet.empty];
        }
      end (List.to_seq suffixes)
    end (List.to_seq free_predecessors)
  in
  let enumerated =
    Seq.filter_map begin fun {Enumeration. first; edges; failing; entry} ->
      let (suffix0, lazy suffixes) = entry in
      let suffixes =
        List.filter_map begin fun (suffix, la) ->
          let la' = IndexSet.inter failing la in
          if IndexSet.is_empty la' then None else
            let suffix, patterns = List.fold_left begin fun (acc, patterns) (_, lp) ->
                let pos, lrc = unpack_position positions lp in
                match previous_position positions pos with
                | Either.Left nt ->
                  let goto = Transition.find_goto_target grammar (stacks.label lrc) nt in
                  let lr0 = Lr1.to_lr0 grammar goto in
                  Enumeration.mark_covered cover lr0 la;
                  (acc, IndexSet.add lr0 patterns)
                | Either.Right _ ->
                  (lrc :: acc, patterns)
              end ([], IndexSet.empty) suffix
            in
            Some (suffix, la', patterns)
        end suffixes
      in
      if list_is_empty suffixes then None
      else Some (
          let ker = graph.ker.:(first) in
          let main_pattern = Enumeration.get_lr0_state grammar stacks ker in
          let patterns = ref IndexSet.empty in
          let middle =
            List.concat_map (fun (edge : _ Enumeration.edge) ->
                let lr0 = Enumeration.get_lr0_state grammar stacks graph.ker.:(edge.source) in
                patterns := IndexSet.add lr0 !patterns;
                edge.path
              ) edges
          in
          let shared_patterns = !patterns in
          let middle = middle @ suffix0 in
          let shared_prefix, suffixes =
            match suffixes with
            | [[], la, lr0s] -> ([], [middle, la, lr0s])
            | _ -> (middle, suffixes)
          in
          {main_pattern; shared_patterns; shared_prefix; suffixes}
        )
    end (Enumeration.to_seq cover)
  in
  Seq.memoize (Seq.append direct enumerated)

let report_case grammar (stacks : _ Automata.stacks) reachability
    ~output ~get_prefix case =
  let p fmt = Printf.ksprintf output fmt in
  p "Some uncovered stacks can be caught by this pattern:\n\
     ```\n\
     %s\n\
     ```\n\n"
    (String.concat "\n" (string_of_items_for_filter grammar case.main_pattern));
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
  List.iteri begin fun i (suffix0, lookaheads, patterns') ->
    let suffix = List.map stacks.label suffix0 in
    p "### Sample %d\n\n" (i + 1);
    p "Stacks ending in:\n\
       ```\n\
       %s\n\
       ```\n"
      (string_concat_map " " (Lr1.symbol_to_string grammar) suffix);
    p "are rejected without an error message when looking ahead at:\n\
       ```\n\
       %s\n\
       ```\n"
      (String.concat ", " (IndexSet.rev_map_elements lookaheads (Terminal.to_string grammar)));
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
      IndexSet.iter (fun pattern ->
          p "%s\n"
            (String.concat "\n" (string_of_items_for_filter grammar pattern))
        ) patterns;
      p "```\n"
    );
    p "\n"
  end case.suffixes
