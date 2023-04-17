open Utils
open Misc
open Fix.Indexing

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

let map_from_set xs f =
  IndexSet.fold (fun x acc -> IndexMap.add x (f x) acc)
    xs IndexMap.empty

module Make (I : Info.S)() =
struct
  open I

  let time =
    let t = ref (Sys.time ()) in
    fun msg ->
      let t' = Sys.time () in
      Printf.eprintf "%s in %.02fms\n" msg ((t' -. !t) *. 1000.0);
      t := t'

  module LRijkstra = LRijkstraFast.Make(I)()

  let () = time "LRijkstra"

  module Lrc : sig
    include Info.INDEXED

    val idle : set
    val lr1_of_lrc : t -> Lr1.t
    val lrcs_of_lr1 : Lr1.t -> set
    val first_lrc_of_lr1 : Lr1.t -> t
    val predecessors : t -> set
    val predecessors_by_lr1 : t -> set Lr1.map
    val set_predecessors_by_lr1 : set -> set Lr1.map
    val lookahead : n index -> Terminal.set
    val decompose : n index -> Lr1.t * Terminal.set
  end =
  struct

    let n =
      let count lr1 = Array.length (LRijkstra.Classes.for_lr1 lr1) in
      let sum = ref 0 in
      Index.iter Lr1.n (fun lr1 -> sum := !sum + count lr1);
      !sum

    include Const(struct let cardinal = n end)

    type t = n index
    type set = n indexset
    type 'a map = (n, 'a) indexmap

    let index_shift (i : n index) offset =
      Index.of_int n ((i :> int) + offset)

    let index_delta (type n) (i : n index) (j : n index) =
      (i :> int) - (j :> int)

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
          let lrc = index_shift first i in
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

    let idle = IndexSet.map first_lrc_of_lr1 Lr1.idle

    let decompose lrc =
      let lr1 = lr1_of_lrc lrc in
      let lrc0 = first_lrc_of_lr1 lr1 in
      let lookaheads = LRijkstra.Classes.for_lr1 lr1 in
      (lr1, lookaheads.(index_delta lrc lrc0))

    let lookahead lrc = snd (decompose lrc)

    let predecessors =
      let predecessors = Vector.make n IndexSet.empty in
      let t0 = Sys.time () in
      let process lr1 =
        let first_lrc = first_lrc_of_lr1 lr1 in
        match Lr1.incoming lr1 with
        | None -> ()
        | Some sym when Symbol.is_terminal sym ->
          Vector.set predecessors first_lrc @@
          List.fold_left (fun acc tr ->
              let src = Transition.source tr in
              let lrc_first = first_lrc_of_lr1 src in
              let count = Array.length (LRijkstra.Classes.for_lr1 src) in
              let lrc_last = index_shift lrc_first (count - 1) in
              IndexSet.union acc (IndexSet.init_interval lrc_first lrc_last)
            ) IndexSet.empty (Transition.predecessors lr1)
        | Some _ ->
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
                  Vector.set predecessors (index_shift first_lrc index) reachable
                ) coercion.forward.(post)
            done
          in
          List.iter process_transition (Transition.predecessors lr1)
      in
      Index.iter Lr1.n process;
      Printf.eprintf "computed Lrc predecessors in %.02fms\n"
        ((Sys.time () -. t0) *. 1000.0);
      Vector.get predecessors

    let predecessors_by_lr1 =
      tabulate_finset n @@ fun lrc ->
      let all = predecessors lrc in
      IndexSet.fold begin fun lr1 acc ->
        let preds = IndexSet.inter (lrcs_of_lr1 lr1) all in
        if IndexSet.is_empty preds
        then acc
        else IndexMap.add lr1 preds acc
      end (Lr1.predecessors (lr1_of_lrc lrc)) IndexMap.empty

    let set_predecessors_by_lr1 lrcs =
      IndexSet.fold begin fun lrc acc ->
        IndexMap.union
          (fun _ s1 s2 -> Some (IndexSet.union s1 s2))
          (predecessors_by_lr1 lrc)
          acc
      end lrcs IndexMap.empty
  end

  let () = time "lrc"

  module Redgraph = struct

    type transition =
      | Inner of node
      | Outer of {
          pop: int;
          reach: Transition.goto indexset;
        }

    and reduction = {
      production: Production.t;
      lookahead: Terminal.set;
      transition: transition;
    }

    and node = {
      lr1: Lr1.t;
      reductions: reduction list;
      mutable accept: Terminal.set;
      mutable partition: Terminal.n IndexSet.Set.t;
      mutable imm_fail : Terminal.set;
      mutable may_fail : Terminal.set;
    }

    type stack =
      | Goto of Lr1.t * stack
      | Bottom of int * Lr1.set

    let initial, goto_transitions =
      let pop = function
        | Goto (_, stack) -> stack
        | Bottom (n, lr1s) -> Bottom (n + 1, Lr1.set_predecessors lr1s)
      in
      let pop_to (n, stack) prod =
        let rec pop_n stack = function
          | 0 -> stack
          | n -> pop_n (pop stack) (n - 1)
        in
        let m = Production.length prod in
        (m, pop_n stack (m - n))
      in
      let rec reductions_at stack lr1 =
        let reductions = simulate_reductions (0, Goto (lr1, stack)) (Lr1.reductions lr1) in
        {
          lr1;
          accept = IndexSet.empty;
          reductions;
          partition = IndexSet.Set.empty;
          imm_fail = IndexSet.empty;
          may_fail = IndexSet.empty;
        }
      and simulate_reductions (n, stack) = function
        | [] -> []
        | (production, lookahead) :: rest ->
          let (n, stack) = pop_to (n, stack) production in
          let result = simulate_reductions (n, stack) rest in
          let transition =
            let lhs = Production.lhs production in
            match stack with
            | Bottom (pop, lr1s) ->
              Outer {pop; reach = IndexSet.map (fun lr1 -> Transition.find_goto lr1 lhs) lr1s}
            | Goto (lr1, _) ->
              Inner (reductions_at stack (Transition.find_goto_target lr1 lhs))
          in
          {production; lookahead; transition} :: result
      in
      let initial =
        map_from_set Lr1.idle (fun lr1 ->
            reductions_at (Bottom (1, Lr1.predecessors lr1)) lr1)
      and goto_transitions =
        Vector.init Transition.goto @@ fun tr ->
        let any = Transition.of_goto tr in
        let src = Transition.source any in
        let tgt = Transition.target any in
        reductions_at (Bottom (2, IndexSet.singleton src)) tgt
      in
      (initial, goto_transitions)

    let () = time "Redgraph: generate nodes"

    let () =
      let todo = Vector.make Transition.goto IndexSet.Set.empty in
      let todo_dom = ref IndexSet.empty in
      let propagate_goto la tr =
        let node = Vector.get goto_transitions tr in
        if not (IndexSet.Set.mem la node.partition) then (
          let todo' = Vector.get todo tr in
          if IndexSet.Set.is_empty todo' then
            todo_dom := IndexSet.add tr !todo_dom;
          Vector.set todo tr (IndexSet.Set.add la todo');
        )
      in
      let rec propagate_node la node =
        if not (IndexSet.Set.mem la node.partition) then (
          node.partition <- IndexSet.Set.add la node.partition;
          List.iter (propagate_reduction la) node.reductions;
        )
      and propagate_reduction la red =
        let la = Terminal.intersect la red.lookahead in
        if not (IndexSet.is_empty la) then
          match red.transition with
          | Inner node -> propagate_node la node
          | Outer t -> IndexSet.iter (propagate_goto la) t.reach
      in
      IndexMap.iter
        (fun _ node -> propagate_node Terminal.all node)
        initial;
      while not (IndexSet.is_empty !todo_dom) do
        let todo_dom' = !todo_dom in
        todo_dom := IndexSet.empty;
        IndexSet.iter (fun tr ->
            let todo' = Vector.get todo tr in
            Vector.set todo tr IndexSet.Set.empty;
            IndexSet.Set.iter
              (fun la -> propagate_node la (Vector.get goto_transitions tr)) todo'
          ) todo_dom'
      done

    let () = time "Redgraph: partition lookaheads"

    let () =
      let rec normalize_node node =
        let partition, total =
          IndexRefine.partition_and_total
            (IndexSet.Set.elements node.partition)
        in
        node.partition <- IndexSet.Set.of_list partition;
        node.imm_fail <-
          IndexSet.(diff (diff Terminal.all total) node.accept);
        node.reductions |> List.iter @@ fun red ->
        match red.transition with
        | Inner node -> normalize_node node
        | Outer _ -> ()
      in
      IndexMap.iter (fun _ node -> normalize_node node) initial;
      Vector.iter normalize_node goto_transitions

    let failures =
      let rec failures node =
        if IndexSet.is_empty node.may_fail then (
          node.may_fail <- Terminal.all;
          update_failures node;
        );
        node.may_fail
      and update_failures node =
        node.may_fail <-
          IndexSet.diff
            (List.fold_left next_failures IndexSet.empty node.reductions)
            node.accept;

      and next_failures acc red =
        IndexSet.union acc @@
        Terminal.intersect red.lookahead @@
        match red.transition with
        | Inner node -> failures node
        | Outer {reach; pop=_} ->
          IndexSet.fold
            (fun goto acc -> IndexSet.union acc (failures (Vector.get goto_transitions goto)))
            reach IndexSet.empty
      in
      (*TODO: look for a fixed point
        For OCaml grammar:
        first pass removes 9327 partitions
        with 4-passes, we can remove up to 9363 partitions
      *)
      failures

    let () =
      let filtered = ref 0 in
      let filter_node node =
        let failures = failures node in
        node.partition <- IndexSet.Set.filter_map (fun set ->
            let set = IndexSet.inter set failures in
            if IndexSet.is_empty set then
              (incr filtered; None)
            else
              Some set
          ) node.partition;
        node.accept <- IndexSet.union
            (Lr1.shift_on node.lr1)
            (IndexSet.diff (Lr1.reduce_on node.lr1) node.may_fail)
      in
      let rec filter_tree node =
        filter_node node;
        List.iter
          (fun red -> match red.transition with
             | Inner node -> filter_tree node
             | Outer _ -> ())
          node.reductions
      in
      IndexMap.iter (fun _ node -> filter_tree node) initial;
      Vector.iter filter_tree goto_transitions;
      Printf.eprintf "filtered %d partitions\n" !filtered

    let () = time "Redgraph: filter non-failing partitions"
  end

  (* Check that goto transitions are non-deterministic for Lrc.
     => Yep *)
  (*let () =
    let nondet = ref 0 in
    Index.iter Transition.goto (fun tr ->
        let target = Transition.(target (of_goto tr)) in
        let preds = ref IndexSet.empty in
        IndexSet.iter (fun lrc ->
            let preds' = Lrc.predecessors lrc in
            if not (IndexSet.disjoint !preds preds') then
              incr nondet;
            preds := IndexSet.union !preds preds'
          ) (Lrc.lrcs_of_lr1 target)
      );
    Printf.eprintf "%d nondeterministic lrc-goto transitions\n" !nondet*)



  (* What is the relation between the lookahead partition computed at each goto
     node and LRijkstra classes of the target of a goto transition ? *)
  (*
  let () =
    let same = ref 0 in
    let lrijkstra_included_in_redgraph = ref 0 in
    let redgraph_included_in_lrijkstra = ref 0 in
    let overlap = ref 0 in
    Vector.iteri (fun tr node ->
        tr
        |> Transition.of_goto
        |> Transition.target
        |> LRijkstra.Classes.for_lr1
        |> Array.iter @@ fun classe ->
        node.Redgraph.partitions |> IndexSet.Set.iter @@ fun classe' ->

        if not (IndexSet.disjoint classe classe') then (
          if IndexSet.equal classe classe' then
            incr same
          else if IndexSet.subset classe classe' then
            incr lrijkstra_included_in_redgraph
          else if IndexSet.subset classe' classe then
            incr redgraph_included_in_lrijkstra
          else
            incr overlap
        )
      ) Redgraph.goto_transitions;
    Printf.eprintf
      "Relation between LRijkstra partitions and Redgraph partitions\n\
      - LRijkstra == Redgraph  : %d times\n\
      - LRijkstra <= Redgraph  : %d times\n\
      - Redgraph  <= LRijkstra : %d times\n\
      - LRijkstra ## Redgraph  : %d times\n"
      !same
      !lrijkstra_included_in_redgraph
      !redgraph_included_in_lrijkstra
      !overlap
  *)

  (*module type NFA = sig
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

  module Faillible =
  struct

    (*let fold_outer_steps trs f la acc =
      let rec visit_inner_candidate la acc {R. target; lookahead; filter = ()} =
        let la = Terminal.intersect lookahead la in
        if IndexSet.is_empty la then
          acc
        else
          visit_transitions la (R.get_transitions target) acc
      and visit_transitions la {R. inner; outer} acc =
        let acc =
          List.fold_left (fun acc {R.reachable=_; candidates} ->
              List.fold_left (visit_inner_candidate la) acc candidates)
            acc inner
        in
        f la outer acc
      in
      visit_transitions la trs acc

    let rec unfold_stack lrcs = function
      | [] -> ()
      | {R. reachable=_; candidates=[]} :: steps ->
        let lrcs = indexset_bind lrcs Lrc.predecessors in
        unfold_stack lrcs steps
      | {R. reachable=_; candidates} :: steps ->
        let lrcs = Lrc.set_predecessors_by_lr1 lrcs in
        List.iter (fun
        Lrc.lookahead
        unfold_stack lrcs steps


    let reject = Vector.init Lrc.n @@ fun lrc ->
      let lr1, la = Lrc.decompose lrc in
      let result = Lr1.reject lr1 in
      let inter = Terminal.intersect (Lr1.reduce_on lr1) la in
    *)

  end

  (*module Lrce : sig
    module NFA : sig
      include NFA
      val lrcs : n index -> Lrc.set
    end

    val follow_lookahead_path :
      Terminal.set -> NFA.n index -> NFA.n index list -> Lr1.t list list list * Terminal.set
    val compute_lookahead : NFA.n index list -> Lr1.t list list list * Terminal.set
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

    (*type lr1_paths = {
      lr1_goto: Lr1.closed_reduction list Lr1.map;
      lr1_pops: lr1_paths Lr1.map;
    }

    let intermediate_lr1_steps lr1 =
      let rec process_reduction n lr1 = function
        | [] ->
          {lr1_goto = IndexMap.empty; lr1_pops = IndexMap.empty}

        | (c : Lr1.closed_reduction) :: reds when n = c.pop ->
          assert (not (IndexSet.is_empty c.lookahead));
          let paths = process_reduction n lr1 reds in
          begin match Production.kind c.prod with
            | `START -> paths
            | `REGULAR ->
              let nt = Production.lhs c.prod in
              let target = Transition.find_goto_target lr1 nt in
              let lr1_goto =
                IndexMap.update target (function
                    | None -> Some [c]
                    | Some cs -> Some (c :: cs)
                  ) paths.lr1_goto
              in
              {paths with lr1_goto}
          end

        | (c :: _) as reds  ->
          assert (c.pop > n);
          process_predecessors (n + 1) (Lr1.predecessors lr1) reds

      and process_predecessor n reds lr1' acc =
        IndexMap.add lr1' (process_reduction n lr1' reds) acc

      and process_predecessors n lr1s reds = {
        lr1_goto = IndexMap.empty;
        lr1_pops = IndexSet.fold (process_predecessor n reds) lr1s IndexMap.empty
      }

      in
      process_predecessors 1 (Lr1.predecessors lr1) (Lr1.closed_reductions lr1)

    module Intermediate = Gensym()

    module Paths = Sum(Lrc)(Intermediate)

    type lrc_paths = {
      states: Lrc.set;
      index: Paths.n index;
      fail: (Lr1.closed_reduction * Terminal.set) list;
      goto: (Lr1.closed_reduction * Lrc.set) list;
      pops: lrc_paths Lr1.map;
    }

    let empty = {
      states = IndexSet.empty;
      index = Paths.inj_r (Intermediate.fresh ());
      fail = [];
      goto = [];
      pops = IndexMap.empty;
    }

    let paths = IndexBuffer.make empty

    let mk_step ?initial states ~fail ~goto pops =
      match fail, goto with
      | [], [] when IndexMap.is_empty pops -> empty
      | _ ->
        let index = match initial with
          | Some lrc -> Paths.inj_l lrc
          | None -> Paths.inj_r (Intermediate.fresh ())
        in
        let result = {states; index; fail; goto; pops} in
        IndexBuffer.set paths index result;
        result

    let is_empty = function
      | {fail=[]; goto=[]; pops; _} -> IndexMap.is_empty pops
      | _ -> false

    let t0 = Sys.time ()

    let () =
      (* FIXME: this code stops after the first failure found (there is at
         least one lookahead token that will reach the erroneous state).
         It is therefore partially incomplete:
         - if it reports no failure, there is no failure
         - if there are failures, it will report some of them (the first it can
           reach), but will not distinguish paths that fail differently for
           some lookahead tokens.

         Possible fix:
         - refine the exploration, remembering which lookahead tokens have been
           explored or not
         - an intuition that I am not sure will work, but is likely
           preferable if it does: refine LRijkstra to also propagate failing
           subsets when computing partitions. That way, "LRC" states will be
           fine enough to distinguish all failure paths. (The risks are either
           that it will blow up or that this last hypothesis is wrong.)
      *)
      let process_lr1 lr1 =
        let lr1_steps = intermediate_lr1_steps lr1 in
        let compute_paths lrc =
          let la = Lrc.lookahead lrc in
          let rec visit ipaths state =
            let initial, lrcs = match state with
              | `Initial lrc -> (Some lrc, IndexSet.singleton lrc)
              | `Continue lrcs -> (None, lrcs)
            in
            let failures = ref [] in
            let goto = ref [] in
            let process_goto target (c : Lr1.closed_reduction) =
              if not (IndexSet.disjoint la c.lookahead) then (
                let fail_la = Terminal.intersect la (Lr1.closed_reject target) in
                let reach_lrc =
                  IndexSet.filter (fun lrc ->
                      IndexSet.mem lrc can_fail_states &&
                      let la' = Lrc.lookahead lrc in
                      not (IndexSet.disjoint la la')
                    ) (Lrc.lrcs_of_lr1 target)
                in
                if not (IndexSet.is_empty fail_la) then
                  push failures (c, fail_la);
                if not (IndexSet.is_empty reach_lrc) then
                  push goto (c, reach_lrc);
              )
            in
            IndexMap.iter
              (fun target cs -> List.iter (process_goto target) cs)
              ipaths.lr1_goto;
            let pops =
              IndexMap.fold (fun lr1 lrcs' opops ->
                  match IndexMap.find_opt lr1 ipaths.lr1_pops with
                  | None -> opops
                  | Some ipaths' ->
                    match visit ipaths' (`Continue lrcs') with
                    | opaths when is_empty opaths -> opops
                    | opaths -> IndexMap.add lr1 opaths opops
                ) (Lrc.set_predecessors_by_lr1 lrcs) IndexMap.empty
            in
            mk_step ?initial lrcs ~fail:!failures ~goto:!goto pops
          in
          visit lr1_steps (`Initial lrc)
        in
        let process_lrc lrc =
          assert (lr1 = Lrc.lr1_of_lrc lrc);
          (*if IndexSet.mem lrc fail_states then
            IndexBuffer.set paths (Paths.inj_l lrc) (
              mk_step (IndexSet.singleton lrc)
                ~fail:Terminal.all ~goto:[] ~pops:IndexMap.empty
            )*)
          if IndexSet.mem lrc can_fail_states then (
            let paths = compute_paths lrc in
            assert (paths.goto = [])
          )
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
            let paths = paths p in
            if is_empty paths
            then acc
            else match paths.fail with
              | (_ :: _) -> encode_normal_set (IndexSet.singleton lrc)
              | [] -> IndexSet.add (inj_r p) acc
          )
          (IndexSet.inter can_fail_states Lrc.offering_states) IndexSet.empty

      let fold_path_transitions ~goto ~reach ~fail path acc =
        match path.fail with
        | _ when is_empty path -> ()
        | (_ :: _) -> fail path.states acc
        | [] ->
          let lr1 = Lrc.lr1_of_lrc (IndexSet.choose path.states) in
          IndexMap.iter (fun _lr1 next -> reach next.index acc) path.pops;
          let rec visit acc lrc =
            let acc = goto lrc acc in
            match paths (Paths.inj_l lrc) with
            | p when is_empty p -> ()
            | {states; fail = (_ :: _); _ } ->
              IndexSet.iter begin fun lrc ->
                match IndexMap.find_opt lr1 (Lrc.predecessors_by_lr1 lrc) with
                | None -> assert false
                | Some lrcs -> fail lrcs acc
              end states
            | p ->
              assert (p.goto = []);
              begin match IndexMap.find_opt lr1 p.pops with
                | None -> ()
                | Some {fail = (_::_); states; _} -> fail states acc
                | Some p' ->
                  reach p'.index acc;
                  visit_set acc p'.goto
              end
          and visit_set acc sets =
            List.iter (fun (_, set) -> IndexSet.iter (visit acc) set) sets
          in
          visit_set acc path.goto

      let step_transitions path =
        let set = ref IndexSet.empty in
        fold_path_transitions path ()
          ~goto:(fun _ () -> ())
          ~reach:(fun step () -> set := IndexSet.add (inj_r step) !set)
          ~fail:(fun lrcs () -> set := IndexSet.union (encode_normal_set lrcs) !set);
        !set

      let transitions n =
        match prj n with
        | L n -> encode_normal_set (Lrc.predecessors n)
        | R n ->
          begin match paths n with
            | p when is_empty p -> IndexSet.empty
            | {fail = (_ :: _); states; _} ->
              begin match Paths.prj n with
                | L (n : Lrc.t) ->
                  assert (states = IndexSet.singleton n);
                  encode_normal_set (Lrc.predecessors n)
                | R (_ : Intermediate.n index) ->
                  encode_normal_set (indexset_bind states Lrc.predecessors)
              end
            | paths -> step_transitions paths
          end

      let lrcs n =
        match prj n with
        | L l -> IndexSet.singleton l
        | R p ->
          let paths = paths p in
          assert (not (is_empty paths));
          paths.states

      let label n =
        match prj n with
        | L l -> Lrc.lr1_of_lrc l
        | R p ->
          let paths = paths p in
          assert (not (is_empty paths));
          Lrc.lr1_of_lrc (IndexSet.choose paths.states)

      (* let () =
         Index.iter Paths.n (fun p ->
            match paths p with
            | Step t -> assert (t.path == p)
            | _ -> ()) *)
    end

    let rec follow_lookahead_path gotos lookahead state = function
      | [] -> (gotos, lookahead)
      | x :: xs ->
        match NFA.prj state with
        | L _ -> (gotos, lookahead)
        | R path ->
          let gotos' = ref [] in
          let lookahead' = ref IndexSet.empty in
          NFA.fold_path_transitions (paths path) ([], lookahead)
            ~goto:(fun lrc (gotos, la) ->
                let lr1 = Lrc.lr1_of_lrc lrc in
                let la = Terminal.intersect (Lrc.lookahead lrc) la in
                let la = IndexSet.diff la (Info.Lr1.closed_reject lr1) in
                if true then assert false;
                (lr1 :: gotos, la)
              )
            ~reach:(fun path (gotos, lookahead) ->
                if equal_index path x then (
                  push gotos' gotos;
                  lookahead' := IndexSet.union !lookahead' lookahead
                )
              )
            ~fail:(fun lrcs (gotos, lookahead) ->
                match NFA.prj x with
                | L lrc' when IndexSet.mem lrc' lrcs ->
                  push gotos' gotos;
                  lookahead' := IndexSet.union !lookahead' lookahead
                | _ -> ()
              );
          let gotos = List.sort_uniq compare !gotos' :: gotos in
          follow_lookahead_path gotos !lookahead' x xs

    let follow_lookahead_path lookahead state states =
      follow_lookahead_path [] lookahead state states

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
          ([], la)
        | R path ->
          let paths = paths path in
          assert (not (is_empty paths));
          assert (IndexSet.is_singleton paths.states);
          let lrc = IndexSet.choose paths.states in
          follow_lookahead_path (Lrc.lookahead lrc) entry rest
    *)
    end*)
  *)
end
