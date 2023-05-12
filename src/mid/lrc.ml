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

module type Failure_NFA = sig
  type lr1
  type terminal
  type n
  val n : n cardinal

  val initials : n indexset
  val label : n index -> lr1 indexset
  val next : n index -> n indexset
  val fail : n index -> terminal indexset option
  (* [Some empty]    : the current state doesn't fail (but the prefix or
                       suffix may fail)
     [Some nonempty] : the current state fails for any of these lookaheads
     [None]          : the current suffix already failed, try again with a
                       shorter suffix
  *)
end

module Make (I : Info.S)() =
struct
  open I

  let time = ref (Sys.time ())
  let time fmt =
      let t' = Sys.time () in
      let dt = t' -. !time in
      time := t';
      Printf.ksprintf (fun msg ->
          Printf.eprintf "%s in %.02fms\n" msg (dt *. 1000.0);
        ) fmt

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
        count := !count + Array.length classes;
        let all = ref IndexSet.empty in
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
                if table.(index) < max_int then
                  reachable := IndexSet.add (index_shift source_lrc pre) !reachable
              done;
              let reachable = !reachable in
              Array.iter
                (fun index ->
                   vector_set_union predecessors
                     (index_shift first_lrc index) reachable)
                coercion.forward.(post)
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

    (*include IndexBuffer.Gen.Make()*)

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
      (*index: n index;*)
      lr1: Lr1.t;
      reductions: reduction list;
      mutable accept: Terminal.set;
      mutable partition: Terminal.n IndexSet.Set.t;
      mutable imm_fail : Terminal.set;
      mutable may_fail : Terminal.set;
    }

    (*let nodes = get_generator ()*)

    type stack =
      | Goto of Lr1.t * stack
      | Bottom of int * Lr1.set

    let count = ref 0

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
        (*let reservation = IndexBuffer.Gen.reserve nodes in*)
        incr count;
        let result = {
          lr1;
          (*index = IndexBuffer.Gen.index reservation;*)
          accept = IndexSet.empty;
          reductions;
          partition = IndexSet.Set.empty;
          imm_fail = Lr1.reject lr1; (*IndexSet.empty;*)
          may_fail = IndexSet.empty;
        } in
        (*IndexBuffer.Gen.commit nodes reservation result;*)
        result

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
        (*reductions_at (Bottom (1, IndexSet.singleton src)) tgt*)
        (*reductions_at (Bottom (2, IndexSet.singleton src)) tgt*)
        reductions_at (Goto (src, Bottom (1, Lr1.predecessors src))) tgt
      in
      (initial, goto_transitions)

    let () = time "Redgraph: generated %d nodes" !count

    (*let nodes = IndexBuffer.Gen.freeze nodes*)

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
        let partition =
          IndexRefine.partition
            (IndexSet.Set.elements node.partition)
        in
        node.partition <- IndexSet.Set.of_list partition;
        (*node.imm_fail <-
          IndexSet.(diff (diff Terminal.all total) node.accept);*)
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

    (* Compute direct access to outer transitions *)

    type outer_transition = {
      steps: reduction list;
      pop: int;
      reach: Transition.goto indexset;
      lookahead: Terminal.set;
    }

    let compute_outer_transitions =
      let rec visit_node path la acc node =
        List.fold_left (visit_reduction path la) acc node.reductions
      and visit_reduction path la acc (r : reduction) =
        let la = Terminal.intersect la r.lookahead in
        assert (not (IndexSet.is_empty la));
        match r.transition with
        | Inner node -> visit_node (r :: path) la acc node
        | Outer {pop; reach}  ->
          { steps = r :: path; pop; reach; lookahead = la} :: acc
      in
      fun node ->
        let trs = visit_node [] Terminal.all [] node in
        let trs = List.sort (fun t1 t2 -> Int.compare t1.pop t2.pop) trs in
        (node, trs)

    let initial_outer =
      IndexMap.map compute_outer_transitions initial

    let goto_outer =
      Vector.map compute_outer_transitions goto_transitions
  end

  module Redgraph_la : sig
    type n
    val n : n cardinal
    val initials : n indexset
    val next : n index -> n indexset
    val label : n index -> Lr1.t option
    val fail : n index -> Terminal.set
  end = struct

    include IndexBuffer.Gen.Make()

    type t = {
      index: n index;
      label: Lr1.t option;
      fail: Terminal.set;
      mutable next: n indexset;
    }

    let nodes = get_generator ()

    let imported_parts = Vector.make Transition.goto IndexSet.Map.empty

    let rec import_goto goto la =
      let node = Vector.get Redgraph.goto_transitions goto in
      let nodes =
        IndexSet.Set.fold (fun la' acc ->
            assert (IndexSet.disjoint la' la || IndexSet.subset la' la);
            if not (IndexSet.quick_subset la' la) then acc else
              IndexSet.add (import_part goto la') acc
          ) node.partition IndexSet.empty
      in
      nodes

    and import_part (goto : Transition.goto index) la =
      let parts = Vector.get imported_parts goto in
      match IndexSet.Map.find_opt la parts with
      | Some index -> index
      | None ->
        let node, transitions = Vector.get Redgraph.goto_outer goto in
        let reservation = IndexBuffer.Gen.reserve nodes in
        let source = Transition.source (Transition.of_goto goto) in
        let index = IndexBuffer.Gen.index reservation in
        let node = {
          index;
          label = Some source;
          next = IndexSet.empty;
          fail = Terminal.intersect la node.imm_fail;
        } in
        IndexBuffer.Gen.commit nodes reservation node;
        Vector.set imported_parts goto (IndexSet.Map.add la index parts);
        node.next <- import_transitions la (Lr1.predecessors source) 1 transitions;
        index

    and import_transitions la lr1s n = function
      | [] -> IndexSet.empty
      | (tr : Redgraph.outer_transition) :: trs when n = tr.pop ->
        let rest = import_transitions la lr1s n trs in
        if IndexSet.disjoint la tr.lookahead
        then rest
        else IndexSet.union rest (indexset_bind tr.reach (fun g ->
            let lr1 = Transition.source (Transition.of_goto g) in
            if not (IndexSet.mem lr1 lr1s) then (
              Printf.eprintf "expected %s to be a member of %s\n%s\n"
                (Lr1.to_string lr1)
                (Lr1.set_to_string lr1s)
                (Format.asprintf "%a"
                   Grammar.Print.itemset
                   (List.concat_map
                      (fun lr1 ->
                         List.map (fun (p, pos) -> (Production.to_g p, pos)) (Lr1.items lr1))
                      (IndexSet.elements lr1s)))
              ;
              assert false
            );
            import_goto g la
          ))
      | transitions ->
        let targets = import_transitions la (Lr1.set_predecessors lr1s) (n + 1) transitions in
        if IndexSet.is_empty targets then
          IndexSet.empty
        else
          let reservation = IndexBuffer.Gen.reserve nodes in
          let index = IndexBuffer.Gen.index reservation in
          let node = {index; label = None; next = targets; fail = IndexSet.empty} in
          IndexBuffer.Gen.commit nodes reservation node;
          IndexSet.singleton index

    let import_initial lr1 ((node : Redgraph.node), trs) =
      let reservation = IndexBuffer.Gen.reserve nodes in
      let next = import_transitions Terminal.all (Lr1.predecessors lr1) 1 trs in
      let index = IndexBuffer.Gen.index reservation in
      let node = {index; label = Some lr1; next; fail = node.imm_fail} in
      IndexBuffer.Gen.commit nodes reservation node;
      index

    let initials =
      IndexMap.fold
        (fun lr1 def acc -> import_initial lr1 def :: acc)
        Redgraph.initial_outer []
      |> List.rev
      |> IndexSet.of_list

    let nodes = IndexBuffer.Gen.freeze nodes

    let fail n = (Vector.get nodes n).fail
    let next n = (Vector.get nodes n).next
    let label n = (Vector.get nodes n).label

    let () =
      time "Lookahead-specialized redgraph with %d nodes" (cardinal n)
  end

  module Redgraph_lrc_la : sig
    type n
    val n : n cardinal
    val initials : n indexset
    val next : n index -> n indexset
    val label : n index -> Lr1.set
    val fail : n index -> Terminal.set
    val lrcs : n index -> Lrc.set
  end = struct
    type 'n t = {
      index: 'n index;
      label: Lr1.set;
      fail: Terminal.set;
      mutable next: 'n indexset;
      lrcs: Lrc.set;
    }

    module Full = IndexBuffer.Gen.Make()
    let nodes = Full.get_generator ()

    let table = Hashtbl.create 7

    let normalize lrcs la =
      match Redgraph_la.label la with
      | None -> lrcs
      | Some lr1 -> IndexSet.inter lrcs (Lrc.lrcs_of_lr1 lr1)

    let rec import lrcs la =
      let lrcs = normalize lrcs la in
      if IndexSet.is_empty lrcs then
        None
      else
        let key = (la, lrcs) in
        match Hashtbl.find_opt table key with
        | Some result -> Some result
        | None ->
          let label = match Redgraph_la.label la with
            | None -> IndexSet.map Lrc.lr1_of_lrc lrcs
            | Some lr1 -> IndexSet.singleton lr1
          in
          let reservation = IndexBuffer.Gen.reserve nodes in
          let index = IndexBuffer.Gen.index reservation in
          Hashtbl.add table key index;
          let result = {
            index; label; lrcs;
            next = IndexSet.empty;
            fail = Redgraph_la.fail la;
          } in
          IndexBuffer.Gen.commit nodes reservation result;
          let lrcs = indexset_bind lrcs Lrc.predecessors in
          let las = Redgraph_la.next la in
          let next = IndexSet.filter_map (import lrcs) las in
          result.next <- next;
          Some index

    let full_initials =
      IndexSet.filter_map (fun la ->
          let lrcs = Lrc.lrcs_of_lr1 (Option.get (Redgraph_la.label la)) in
          import lrcs la
        ) Redgraph_la.initials

    let nodes = IndexBuffer.Gen.freeze nodes

    let () = time "la and lrc intersection, with %d nodes" (cardinal Full.n)

    module Live = Gensym()

    let live_map =
      let reverse = Vector.make Full.n IndexSet.empty in
      Vector.iter (fun t ->
          IndexSet.iter (fun t' -> vector_set_add reverse t' t.index) t.next;
        ) nodes;
      let map = Vector.make Full.n None in
      let rec reach t =
        if Option.is_none (Vector.get map t) then (
          Vector.set map t (Some (Live.fresh ()));
          IndexSet.iter reach (Vector.get reverse t)
        )
      in
      Vector.iter (fun t ->
          if not (IndexSet.is_empty t.fail) then
            reach t.index
        ) nodes;
      map

    let live_index n =
      Vector.get live_map n

    let () =
      time "%d live nodes (%d dead-ends)"
        (cardinal Live.n) (cardinal Full.n - cardinal Live.n)

    include Live

    let fail = Vector.make n IndexSet.empty
    let next = Vector.make n IndexSet.empty
    let label = Vector.make n IndexSet.empty
    let lrcs = Vector.make n IndexSet.empty

    let () = Vector.iteri (fun full live ->
        match live with
        | None -> ()
        | Some live ->
          let node = Vector.get nodes full in
          Vector.set fail  live node.fail;
          Vector.set next  live (IndexSet.filter_map live_index node.next);
          Vector.set label live node.label;
          Vector.set lrcs  live node.lrcs;
      ) live_map

    let initials = IndexSet.filter_map live_index full_initials
    let fail  = Vector.get fail
    let next  = Vector.get next
    let label = Vector.get label
    let lrcs  = Vector.get lrcs
  end

  module type Failure_NFA =
    Failure_NFA with type lr1 := Lr1.n
                 and type terminal := Terminal.n

  module Lrc_NFA : Failure_NFA with type n = Lrc.n = struct
    include Lrc
    let initials = indexset_bind Lr1.idle Lrc.lrcs_of_lr1
    let label i = IndexSet.singleton (lr1_of_lrc i)
    let next = predecessors
    let fail _ = None
  end

  module Lrce : Failure_NFA = struct
    include Sum(Lrc)(Redgraph_lrc_la)

    let initials = IndexSet.map inj_r Redgraph_lrc_la.initials

    let rednext =
      Vector.init Redgraph_lrc_la.n begin fun i ->
        let base = IndexSet.map inj_r (Redgraph_lrc_la.next i) in
        if IndexSet.is_empty (Redgraph_lrc_la.fail i) then
          base
        else
          IndexSet.union base
            (IndexSet.map inj_l
               (indexset_bind (Redgraph_lrc_la.lrcs i) Lrc_NFA.next))
      end

    let next n =
      match prj n with
      | L lrc -> IndexSet.unsafe_to_indexset (Lrc.predecessors lrc)
      | R red -> Vector.get rednext red

    let label n =
      match prj n with
      | L lrc -> Lrc_NFA.label lrc
      | R red -> Redgraph_lrc_la.label red

    let fail n =
      match prj n with
      | L _ -> None
      | R red -> Some (Redgraph_lrc_la.fail red)
  end


end
