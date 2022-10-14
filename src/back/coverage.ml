open Utils
open Misc
open Fix.Indexing

module Make(Dfa : Sigs.DFA)() = struct
  module Info = Dfa.Regexp.Info
  open Info

  (** {1 Miscellaneous definitions} *)

  (** Lazy list *)
  type 'a lazy_list =
    | LNil
    | LCons of 'a * 'a lazy_list lazy_t

  let rec lazy_list_to_seq = function
    | LNil -> Seq.Nil
    | LCons (x, xs) ->
      Seq.Cons (x, fun () -> lazy_list_to_seq (Lazy.force xs))

  (** Lazy stream (infinite list) *)
  type 'a lazy_stream = Scons of 'a * 'a lazy_stream lazy_t

  let rec stream_nth (Scons (v, vs)) = function
    | 0 -> v
    | n -> stream_nth (Lazy.force vs) (n - 1)

  (** {1 NFA intersection}

      Check coverage by intersecting the DFA with an NFA and finding NFA paths
      that are not covered. *)

  (** The definition of an NFA suitable for intersection with the DFA.
      This notion of NFA has the following restriction:
      - states can only have a single incoming symbol (transitions targeting
        the same state should have the same label)
      - the initial state is represented by the set of its transitions

      Below we provide two NFA instances:
      - [Lrc.NFA] represents all stacks that are ready to consume an input
        token
      - [Lrce.NFA] represents all stacks that lead to an error for at least one
        input token.
  *)
  module type NFA = sig
    include CARDINAL

    (** [label n] returns the label of all transitions going to [n] *)
    val label : n index -> Lr1.t

    (** Since a state can only have a single incoming symbol, a transition is
        not represented by a pair of a label and a target but directly by the
        target state. The label is implicitly [label target].
        A set of transitions is therefore directly a set of states. *)
    val transitions : n index -> n indexset

    (** Transitions leaving the initial state *)
    val initials : n indexset
  end

  (** [Check_dfa(Nfa)] implements the intersection between [Dfa] and [Nfa] *)
  module Check_dfa(NFA : NFA) : sig

    (** The result of the intersection *)
    type t

    (** A [DFA.State.t] annotated with coverage information *)
    type state

    (** [analyse initial_st] compute the analysis starting from state
        [initial_st] (which should be the initial state). *)
    val analyse : Dfa.State.t -> t

    (** [partial_states t] returns the list of states with partial coverage *)
    val partial_states : t -> state list

    (** [repr st] is the DFA state represented by [st] *)
    val repr : state -> Dfa.State.t

    (** [unhandled st] returns all the NFA transitions that are not covered
        by DFA state [repr st]. *)
    val unhandled : state -> NFA.n indexset

    (** [paths t] lazily enumerates shortest paths to partial states.
        Only one path is given per partial state. If multiple shortest paths
        exist, one is picked arbitrarily.

        A path is given as a triple (st, tr, path) where:
        - [st] is the partially covered state reached by this path
        - [tr] is the uncovered transition
        - [path] is a list of NFA transitions starting from the initial state
          and ending at the partially covered state.
    *)
    val paths : t -> (state * NFA.n index * NFA.n index list) Seq.t

  end = struct
    type state = {
      repr: Dfa.State.t;
      transitions: state lazy_t Lr1.map;
      mutable visited: NFA.n indexset;
      mutable scheduled: NFA.n indexset;
      mutable unhandled: NFA.n indexset;
      mutable predecessors: IntSet.t;
    }

    let make_state repr transitions = {
      repr; transitions;
      visited = IndexSet.empty;
      scheduled = IndexSet.empty;
      unhandled = IndexSet.empty;
      predecessors = IntSet.empty;
    }

    let make_state_table () =
      let table = Hashtbl.create 7 in
      let rec aux repr =
        let id = Dfa.State.id repr in
        match Hashtbl.find_opt table id with
        | Some t -> t
        | None ->
          let trs = ref IndexMap.empty in
          let add_lr1 target lr1 = trs := IndexMap.add lr1 target !trs in
          Dfa.State.iter_transitions repr (fun lr1s _vars target ->
              let target = lazy (aux target) in
              IndexSet.iter (add_lr1 target) lr1s
            );
          let t = make_state repr !trs in
          Hashtbl.add table id t;
          t
      in
      table, aux

    let make_scheduler () =
      let todo = ref [] in
      let schedule source target states =
        let states = IndexSet.diff states target.visited in
        if not (IndexSet.is_empty states) then (
          if IndexSet.is_empty target.scheduled then push todo target;
          target.scheduled <- IndexSet.union target.scheduled states;
          target.predecessors <- IntSet.add (Dfa.State.id source.repr) target.predecessors;
        )
      in
      let rec flush f = match List.rev !todo with
        | [] -> ()
        | todo' ->
          todo := [];
          List.iter f todo';
          flush f
      in
      (schedule, flush)

    type t = {
      initial: state;
      partial: state list;
      table: (int, state) Hashtbl.t;
    }

    let analyse initial =
      let table, lift = make_state_table () in
      let schedule, flush = make_scheduler () in
      let partial = ref [] in
      let process_transition state tr =
        let lr1 = NFA.label tr in
        match IndexMap.find_opt lr1 state.transitions with
        | None ->
          if IndexSet.is_empty state.unhandled then
            push partial state;
          state.unhandled <- IndexSet.add tr state.unhandled
        | Some (lazy target) ->
          schedule state target (NFA.transitions tr)
      in
      let process_state state =
        let to_visit = state.scheduled in
        state.visited <- IndexSet.union state.visited to_visit;
        state.scheduled <- IndexSet.empty;
        if IntSet.is_empty (Dfa.State.accepted state.repr) then
          IndexSet.iter (process_transition state) to_visit
      in
      let initial = lift initial in
      schedule initial initial NFA.initials;
      flush process_state;
      { initial; partial = !partial; table }

    let nfa_predecessors = lazy (
      let predecessors = Vector.make NFA.n IndexSet.empty in
      Index.iter NFA.n (fun nfa ->
          let successors = NFA.transitions nfa in
          IndexSet.iter (fun successor ->
              Vector.set predecessors successor
                (IndexSet.add nfa (Vector.get predecessors successor)))
            successors
        );
      Vector.get predecessors
    )

    let repr st = st.repr
    let unhandled st = st.unhandled
    let partial_states t = t.partial

    let paths t =
      let visited = Hashtbl.create 7 in
      let visited st =
        match Hashtbl.find_opt visited (Dfa.State.id st.repr) with
        | Some set -> set
        | None ->
          let set = ref IndexSet.empty in
          Hashtbl.add visited (Dfa.State.id st.repr) set;
          set
      in
      let found = ref [] in
      let candidates = ref [] in
      let add_candidate (path, st0, nfa0, finished) st nfa =
        if not !finished then (
          let path = nfa :: path in
          if st == t.initial then (
            push found (st0, nfa0, path);
            finished := true;
          ) else
            let visited = visited st in
            if not (IndexSet.mem nfa !visited) then (
              visited := IndexSet.add nfa !visited;
              push candidates (st, nfa, (path, st0, nfa0, finished))
            )
        )
      in
      let predecessors (st, nfa, path) =
        let preds = Lazy.force nfa_predecessors nfa in
        IntSet.iter (fun id ->
            let st' = Hashtbl.find t.table id in
            let visited = IndexSet.inter preds st'.visited in
            IndexSet.iter (fun nfa' ->
                match IndexMap.find_opt (NFA.label nfa') st'.transitions with
                | None -> ()
                | Some st'' ->
                  if Lazy.is_val st'' && Lazy.force st'' == st then
                    add_candidate path st' nfa'
              ) visited
          ) st.predecessors
      in
      List.iter (fun st ->
          IndexSet.iter
            (fun nfa -> add_candidate ([], st, nfa, ref false) st nfa)
            st.unhandled
        ) t.partial;
      let rec look () =
        match !candidates with
        | [] -> LNil
        | candidates' ->
          candidates := [];
          List.iter predecessors candidates';
          enum ()
      and enum () =
        match !found with
        | [] -> look ()
        | examples ->
          found := [];
          LCons (List.to_seq examples, Lazy.from_fun look)
      in
      let node = lazy_list_to_seq (look ()) in
      Seq.concat (fun () -> node)
  end

  (** The LRijkstra classes for each state are needed to compute the possible
      LR stacks. *)
  module LRijkstra = LRijkstraFast.Make(Info)()

  (** [Lrc] is a refinement of the [Lr1] automaton.
      Each [Lrc.t] state is a pair of an [Lr1.t] state [s] and a class of this
      state (a member of [LRijkstra.Classes.for_lr1 s]).

      To enumerate possible stacks, it is necessary to keep track of [Lrc]
      states and not just [Lr1] states because certain [Lr1] transitions might
      be unreachable in practice due to conflicting requirements on lookahead
      tokens.
      The classes annotating [Lrc] states are enough to detect these cases.
  *)
  module Lrc : sig
    include Mid.Sigs.INDEXED

    val lr1_of_lrc : t -> Lr1.t
    val lrcs_of_lr1 : Lr1.t -> set
    val first_lrc_of_lr1 : Lr1.t -> t
    val predecessors : t -> set
    val predecessors_by_lr1 : t -> set Lr1.map
    val set_predecessors_by_lr1 : set -> set Lr1.map
    val lookahead : n index -> Terminal.set
    val decompose : n index -> Lr1.t * Terminal.set

    (** The parsers that are accepted by our system are the one that are
        waiting for more inputs (in Menhir terminology, they are checkpoints
        with the [InputNeeded] constructor).
        The [offering_states] set contains all [Lrc] states that can be the
        current state of a parser waiting for input. *)
    val offering_states : set

    module NFA : NFA with type n = n
  end = struct
    let index_shift n i offset =
      Index.of_int n ((i : _ index :> int) + offset)

    let index_delta (type n) (i : n index) (j : n index) =
      (i :> int) - (j :> int)

    include Const(struct
        let cardinal =
          let count lr1 = Array.length (LRijkstra.Classes.for_lr1 lr1) in
          let sum = ref 0 in
          Index.iter Lr1.n (fun lr1 -> sum := !sum + count lr1);
          !sum
      end)

    let () =
      Printf.eprintf "%d lr1 states, %d lrc states\n"
        (cardinal Lr1.n) (cardinal n)

    type t = n index
    type set = n indexset
    type 'a map = (n, 'a) indexmap

    let lr1_of_lrc, lrcs_of_lr1, first_lrc_of_lr1 =
      let lr1_of_lrc = Vector.make' n (fun () -> Index.of_int Lr1.n 0) in
      let count = ref 0 in
      let init_lr1 lr1 =
        let classes = LRijkstra.Classes.for_lr1 lr1 in
        assert (Array.length classes > 0);
        let first = Index.of_int n !count in
        let all = ref IndexSet.empty in
        count := !count + Array.length classes;
        for i = Array.length classes - 1 downto 0 do
          let lrc = index_shift n first i in
          all := IndexSet.add lrc !all;
          Vector.set lr1_of_lrc lrc lr1
        done;
        !all
      in
      let lrcs_of_lr1 = Vector.init Lr1.n init_lr1 in
      (Vector.get lr1_of_lrc,
       Vector.get lrcs_of_lr1,
       (fun lr1 -> Option.get (IndexSet.minimum (Vector.get lrcs_of_lr1 lr1)))
      )

    let offering_states =
      let all = ref IndexSet.empty in
      Index.iter Lr1.n (fun lr1 ->
          match Lr1.incoming lr1 with
          | Some (N _) -> ()
          | Some (T t) when
              Grammar.Terminal.kind (Terminal.to_g t) = `EOF -> ()
          | None | Some (T _) ->
            assert (Array.length (LRijkstra.Classes.for_lr1 lr1) = 1);
            all := IndexSet.add (first_lrc_of_lr1 lr1) !all
        );
      !all

    let predecessors =
      let predecessors = Vector.make n IndexSet.empty in
      let t0 = Sys.time () in
      let interval n i j =
        let rec loop i j acc =
          if j >= i
          then loop i (j - 1) (IndexSet.add (Index.of_int n j) acc)
          else acc
        in
        loop (Index.to_int i) (Index.to_int j) IndexSet.empty
      in
      let process lr1 =
        let first_lrc = first_lrc_of_lr1 lr1 in
        match Lr1.incoming lr1 with
        | None ->
          Vector.set predecessors first_lrc @@
          IndexSet.empty
        | Some (Symbol.T _) ->
          Vector.set predecessors first_lrc @@
          List.fold_left (fun acc tr ->
              let src = Transition.source tr in
              let lrc_first = first_lrc_of_lr1 src in
              let count = Array.length (LRijkstra.Classes.for_lr1 src) in
              let lrc_last = index_shift n lrc_first (count - 1) in
              IndexSet.union acc (interval n lrc_first lrc_last)
            ) IndexSet.empty (Transition.predecessors lr1)
        | Some (Symbol.N _) ->
          let process_transition tr =
            let source_lrc = first_lrc_of_lr1 (Transition.source tr) in
            let node = LRijkstra.Tree.leaf tr in
            let table = Vector.get LRijkstra.Cells.table node in
            let pre_classes = LRijkstra.Classes.pre_transition tr in
            let post_classes = LRijkstra.Classes.post_transition tr in
            let coercion =
              LRijkstra.Coercion.infix post_classes (LRijkstra.Classes.for_lr1 lr1)
            in
            let pre_classes = Array.length pre_classes in
            let post_classes = Array.length post_classes in
            for post = 0 to post_classes - 1 do
              let reachable = ref IndexSet.empty in
              for pre = 0 to pre_classes - 1 do
                let index = LRijkstra.Cells.table_index ~post_classes ~pre ~post in
                if table.(index) < max_int then (
                  let source_lrc = Index.of_int n ((source_lrc :> int) + pre) in
                  reachable := IndexSet.add source_lrc !reachable
                )
              done;
              let reachable = !reachable in
              Array.iter (fun index ->
                  let target_lrc = Index.of_int n ((first_lrc :> int) + index) in
                  Vector.set predecessors target_lrc reachable
                ) coercion.forward.(post)
            done
          in
          List.iter process_transition (Transition.predecessors lr1)
      in
      Index.iter Lr1.n process;
      Printf.eprintf "computed Lrc predecessors in %.02fms\n"
        ((Sys.time () -. t0) *. 1000.0);
      Vector.get predecessors

    let t0 = Sys.time ()

    let predecessors_by_lr1 = tabulate_finset n (fun lrc ->
        let all = predecessors lrc in
        IndexSet.fold (fun lr1 acc ->
            let preds = IndexSet.inter (lrcs_of_lr1 lr1) all in
            if IndexSet.is_empty preds
            then acc
            else IndexMap.add lr1 preds acc
          ) (Lr1.predecessors (lr1_of_lrc lrc)) IndexMap.empty
      )

    let set_predecessors_by_lr1 lrcs =
      IndexSet.fold (fun lrc acc ->
          IndexMap.union
            (fun _ s1 s2 -> Some (IndexSet.union s1 s2))
            (predecessors_by_lr1 lrc)
            acc
        ) lrcs IndexMap.empty

    let () =
      Printf.eprintf "classified Lrc predecessors in %.02fms\n"
        ((Sys.time () -. t0) *. 1000.0)

    let lookahead lrc =
      let lr1 = lr1_of_lrc lrc in
      let classe = index_delta lrc (first_lrc_of_lr1 lr1) in
      (LRijkstra.Classes.for_lr1 lr1).(classe)

    let decompose lrc =
      let lr1 = lr1_of_lrc lrc in
      let classe = index_delta lrc (first_lrc_of_lr1 lr1) in
      (lr1, (LRijkstra.Classes.for_lr1 lr1).(classe))

    module NFA = struct
      type nonrec n = n
      let n = n

      let initials = offering_states
      let transitions = predecessors
      let label = lr1_of_lrc
    end

  end

  (** The Lrce NFA enumerates all stacks that lead to an error, for at least
      one input token.
      All paths generated by the NFA are of the form [U . F]:
      - an "uncertain prefix" U, that might lead to a failure but not enough is
        known at that point to be sure.
      - a "failed suffix" F, once an error has been detected, and which looks
        very similar to the [Lrc] NFA (Lrc enumerates all valid stacks, and
        once we found an error, all valid stack suffixes should be considered).

      A path with an empty U is one which fails immediately: it is for states
      that can immediately tell whether an input token is accepted or rejected.

      A non-empty U means we have to look deeper to tell. This is generally
      because of default reductions: we have to simulate them and then look
      again if the target state that has been reached can consume the input
      token.

      Certain paths can have an empty F:
      - it is because we had to look at least that deep to rule out the
        possibility of an error
      - they should be "minimals", there should always be a path with the same
        prefix and a non-empty [F], so that looking that deep was indeed
        necessary (if there was not a least one possible continuation that was
        failing, then we didn't need to look).
  *)
  module Lrce : sig
    module NFA : sig
      include NFA
      val lrcs : n index -> Lrc.set
    end

    val follow_lookahead_path :
      Terminal.set -> NFA.n index -> NFA.n index list -> Terminal.set
    val compute_lookahead : NFA.n index list -> Terminal.set
  end =
  struct
    let rec predecessor_stream lrcs = Scons (
        lrcs,
        lazy (predecessor_stream (indexset_bind lrcs Lrc.predecessors))
      )

    let reached_from = Vector.make Lrc.n IndexSet.empty

    let print_item (prod, dot) =
      let rhs = Production.rhs prod in
      let symbols = ref [] in
      for i = Array.length rhs - 1 downto 0 do
        push symbols (Symbol.name rhs.(i));
        if i = dot then push symbols ".";
      done;
      String.concat " " !symbols

    let lrc_goto nt lrc =
      let lr1 = Lrc.lr1_of_lrc lrc in
      try Transition.find_goto_target lr1 nt
      with Not_found ->
        Printf.eprintf "Failed to go to non-terminal nt %s from state %s, \
                        with items:\n%s\n"
          (Nonterminal.to_string nt)
          (Lr1.to_string lr1)
          (string_concat_map "\n" print_item (Lr1.items lr1));
        exit 1

    let check_target source ts target =
      let ts' = Lrc.lookahead target in
      let ts' = IndexSet.diff ts' (Lr1.reject (Lrc.lr1_of_lrc target)) in
      if IndexSet.subset ts' ts then
        vector_set_add reached_from target source
      else if not (IndexSet.disjoint ts ts') then (
        let print_terminals = string_of_indexset ~index:Terminal.to_string in
        Printf.eprintf "EXPECTED RELATED SETS\n\
                        INCOMPATIBLE ON %s\n\
                        INTERSECT ON %s\n"
          (print_terminals (IndexSet.diff ts' ts))
          (print_terminals (IndexSet.inter ts' ts))
      )

    let check_reductions lrc =
      let (lr1, ts) = Lrc.decompose lrc in
      let preds = predecessor_stream (IndexSet.singleton lrc) in
      let check_reduction (prod, ts') =
        match Production.kind prod with
        | `START -> ()
        | `REGULAR ->
          let ts = Terminal.intersect ts ts' in
          if not (IndexSet.is_empty ts) then (
            let preds = stream_nth preds (Array.length (Production.rhs prod)) in
            let targets = IndexSet.map (lrc_goto (Production.lhs prod)) preds in
            let check_target = check_target lrc ts in
            IndexSet.iter
              (fun target -> IndexSet.iter check_target (Lrc.lrcs_of_lr1 target))
              targets
          )
      in
      List.iter check_reduction (Lr1.reductions lr1)

    let () = Index.iter Lrc.n check_reductions

    let fail_states =
      IndexSet.init_from_set Lrc.n
        (fun lrc ->
           let lr1, la = Lrc.decompose lrc in
           not (IndexSet.disjoint la (Lr1.reject lr1)))

    let t0 = Sys.time ()

    let can_fail_states =
      let rec loop states =
        let states' = indexset_bind states (Vector.get reached_from) in
        if IndexSet.equal states states' then
          states
        else
          loop states'
      in
      loop fail_states

    let () = Printf.eprintf "computed can fail in %.02fms\n"
        ((Sys.time () -. t0) *. 1000.)

    type lr1_paths = {
      goto: Terminal.set Lr1.map;
      pops: lr1_paths Lr1.map;
    }

    let intermediate_lr1_steps lr1 =
      let rec process_reduction n lr1 = function
        | [] ->
          {goto = IndexMap.empty; pops = IndexMap.empty}

        | (n', prod, _prods, ts) :: reds when n = n' ->
          assert (not (IndexSet.is_empty ts));
          let paths = process_reduction n lr1 reds in
          begin match Production.kind prod with
            | `START -> paths
            | `REGULAR ->
              let nt = Production.lhs prod in
              let target = Transition.find_goto_target lr1 nt in
              let goto =
                IndexMap.update target (function
                    | None -> Some ts
                    | Some ts' ->
                      Some (IndexSet.union ts ts')
                  ) paths.goto
              in
              {paths with goto}
          end

        | ((n', _, _, _) :: _) as reds  ->
          assert (n' > n);
          process_predecessors (n + 1) (Lr1.predecessors lr1) reds

      and process_predecessor n reds lr1' acc =
        IndexMap.add lr1' (process_reduction n lr1' reds) acc

      and process_predecessors n lr1s reds = {
        goto = IndexMap.empty;
        pops = IndexSet.fold (process_predecessor n reds) lr1s IndexMap.empty
      }

      in
      process_predecessors 1 (Lr1.predecessors lr1) (Lr1.closed_reductions lr1)

    module Intermediate = Gensym()

    module Paths = Sum(Lrc)(Intermediate)

    type lrc_paths =
      | Fail of Lrc.set
      | Empty
      | Step of {
          states: Lrc.set;
          index: Paths.n index;
          goto: Lrc.set;
          pops: lrc_paths Lr1.map;
        }

    let paths = IndexBuffer.make Empty

    let mk_step ?initial states goto pops =
      if IndexSet.is_empty goto && IndexMap.is_empty pops then
        Empty
      else
        let index = match initial with
          | None -> Paths.inj_r (Intermediate.fresh ())
          | Some lrc -> Paths.inj_l lrc
        in
        let result = Step {states; index; goto; pops} in
        IndexBuffer.set paths index result;
        result

    let t0 = Sys.time ()

    let () =
      let process_lr1 lr1 =
        let lr1_steps = intermediate_lr1_steps lr1 in
        let compute_paths lrc =
          let la = Lrc.lookahead lrc in
          let rec visit ipaths state =
            let initial, lrcs = match state with
              | `Initial lrc -> (Some lrc, IndexSet.singleton lrc)
              | `Continue lrcs -> (None, lrcs)
            in
            let exception Can_fail in
            let process_goto target la' acc =
              if IndexSet.disjoint la la' then acc else (
                if not (IndexSet.disjoint la (Lr1.closed_reject target)) then
                  raise Can_fail;
                IndexSet.union acc (
                  IndexSet.filter (fun lrc ->
                      IndexSet.mem lrc can_fail_states &&
                      let la' = Lrc.lookahead lrc in
                      not (IndexSet.disjoint la la')
                    ) (Lrc.lrcs_of_lr1 target)
                )
              )
            in
            match IndexMap.fold process_goto ipaths.goto IndexSet.empty with
            | exception Can_fail -> Fail lrcs
            | goto ->
              let pops =
                IndexMap.fold (fun lr1 lrcs' opops ->
                    match IndexMap.find_opt lr1 ipaths.pops with
                    | None -> opops
                    | Some ipaths' ->
                      match visit ipaths' (`Continue lrcs') with
                      | Empty -> opops
                      | Fail _ | Step _ as opaths ->
                        IndexMap.add lr1 opaths opops
                  ) (Lrc.set_predecessors_by_lr1 lrcs) IndexMap.empty
              in
              mk_step ?initial lrcs goto pops
          in
          visit lr1_steps (`Initial lrc)
        in
        let process_lrc lrc =
          assert (lr1 = Lrc.lr1_of_lrc lrc);
          if IndexSet.mem lrc fail_states then
            IndexBuffer.set paths (Paths.inj_l lrc)
              (Fail (IndexSet.singleton lrc))
          else if IndexSet.mem lrc can_fail_states then
            match compute_paths lrc with
            | Empty -> ()
            | Fail _ as x -> IndexBuffer.set paths (Paths.inj_l lrc) x
            | Step x -> assert (IndexSet.is_empty x.goto)
        in
        IndexSet.iter process_lrc (Lrc.lrcs_of_lr1 lr1)
      in
      Index.iter Lr1.n process_lr1

    let paths = Vector.get (IndexBuffer.contents paths Paths.n)

    let () =
      Printf.eprintf "computed lrc paths in %.02fms\n"
        ((Sys.time () -. t0) *. 1000.);
      Printf.eprintf "%d intermediate steps\n"
        (cardinal Intermediate.n)

    module NFA = struct
      include Sum(Lrc)(Paths)

      let encode_normal_set lrcs =
        (* TODO: This encoding is actually the identity,
                 find a way to skip it at some point.
           Measurement indicates a 3% decrease of total runtime for OCaml
           grammar coverage. *)
        IndexSet.map inj_l lrcs

      let initials : n indexset =
        IndexSet.fold (fun lrc acc ->
            let p = Paths.inj_l lrc in
            match paths p with
            | Empty -> acc
            | Fail lrcs -> IndexSet.union (encode_normal_set lrcs) acc
            | Step _ -> IndexSet.add (inj_r p) acc
          )
          (IndexSet.inter can_fail_states Lrc.offering_states) IndexSet.empty

      let fold_path_transitions ~lookahead ~follow_goto ~reach ~fail = function
        | Empty | Fail _ -> assert false
        | Step {states; goto; pops; _} ->
          let lr1 = Lrc.lr1_of_lrc (IndexSet.choose states) in
          IndexMap.iter (fun _lr1 step ->
              match step with
              | Empty -> assert false
              | Fail lrcs -> fail lrcs lookahead
              | Step s' -> reach s'.index lookahead
            ) pops;
          let rec visit lookahead lrc =
            let lookahead = follow_goto lrc lookahead in
            match paths (Paths.inj_l lrc) with
            | Empty -> ()
            | Fail lrcs ->
              IndexSet.iter begin fun lrc ->
                match IndexMap.find_opt lr1 (Lrc.predecessors_by_lr1 lrc) with
                | None -> assert false
                | Some lrcs -> fail lrcs lookahead
              end lrcs
            | Step t0 ->
              assert (IndexSet.is_empty t0.goto);
              begin match IndexMap.find_opt lr1 t0.pops with
                | None -> ()
                | Some Empty -> assert false
                | Some (Fail lrcs) -> fail lrcs lookahead
                | Some (Step t) ->
                  reach t.index lookahead;
                  visit_set lookahead t.goto
              end
          and visit_set lookahead set =
            IndexSet.iter (visit lookahead) set
          in
          visit_set lookahead goto

      let step_transitions path =
        let set = ref IndexSet.empty in
        fold_path_transitions path
          ~lookahead:()
          ~follow_goto:(fun _ () -> ())
          ~reach:(fun step () -> set := IndexSet.add (inj_r step) !set)
          ~fail:(fun lrcs () -> set := IndexSet.union (encode_normal_set lrcs) !set);
        !set

      let transitions n =
        match prj n with
        | L n -> encode_normal_set (Lrc.predecessors n)
        | R n ->
          begin match paths n with
            | Empty -> IndexSet.empty
            | Fail _ ->
              begin match Paths.prj n with
                | L (n : Lrc.t) -> encode_normal_set (Lrc.predecessors n)
                | R (_ : Intermediate.n index) -> assert false
              end
            | Step _ as paths -> step_transitions paths
          end

      let lrcs n =
        match prj n with
        | L l -> IndexSet.singleton l
        | R p ->
          match paths p with
          | Empty | Fail _ -> assert false
          | Step s -> s.states

      let label n =
        match prj n with
        | L l -> Lrc.lr1_of_lrc l
        | R p ->
          match paths p with
          | Empty | Fail _ -> assert false
          | Step s -> Lrc.lr1_of_lrc (IndexSet.choose s.states)

      (* let () =
         Index.iter Paths.n (fun p ->
            match paths p with
            | Step t -> assert (t.path == p)
            | _ -> ()) *)
    end

    let rec follow_lookahead_path lookahead state = function
      | [] -> lookahead
      | x :: xs ->
        match NFA.prj state with
        | L _ -> lookahead
        | R path ->
          let lookahead' = ref IndexSet.empty in
          NFA.fold_path_transitions (paths path)
            ~lookahead
            ~follow_goto:(fun lrc la ->
                let lr1 = Lrc.lr1_of_lrc lrc in
                let la = Terminal.intersect (Lrc.lookahead lrc) la in
                let la = IndexSet.diff la (Info.Lr1.closed_reject lr1) in
                la
              )
            ~reach:(fun path lookahead ->
                if equal_index path x then
                  lookahead' := IndexSet.union !lookahead' lookahead
              )
            ~fail:(fun lrcs lookahead ->
                match NFA.prj x with
                | L lrc' when IndexSet.mem lrc' lrcs ->
                  lookahead' := IndexSet.union !lookahead' lookahead
                | _ -> ()
              );
          follow_lookahead_path !lookahead' x xs

    let compute_lookahead = function
      | [] -> assert false
      | entry :: rest ->
        match NFA.prj entry with
        | L lrc ->
          let la = Lrc.lookahead lrc in
          let la =
            IndexSet.inter la (Info.Lr1.closed_reject (Lrc.lr1_of_lrc lrc))
          in
          assert (not (IndexSet.is_empty la));
          la
        | R path ->
          match paths path with
          | Empty | Fail _ -> assert false
          | Step s ->
            assert (IndexSet.is_singleton s.states);
            let lrc = IndexSet.choose s.states in
            follow_lookahead_path (Lrc.lookahead lrc) entry rest
  end
end
