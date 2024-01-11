open Utils
open Misc
open Fix.Indexing

module type RAW0 = sig
  include Info.INDEXED
  module Info : Info.S
  open Info

  val all_idle : set
  val lr1_of_lrc : t -> Lr1.t
  val lrcs_of_lr1 : Lr1.t -> set
  val first_lrc_of_lr1 : Lr1.t -> t
  val all_successors : t -> set
end

module type RAW = sig
  include RAW0
  val lookahead : n index -> Info.Terminal.set
  val class_index : n index -> int
end

module type S = sig
  include RAW
  val reachable : n indexset
  val idle : n indexset
  val predecessors : n index -> n indexset
  val successors : n index -> n indexset
  val some_prefix : n index -> n index list
end

module Close(Info : Info.S)
    (Lrc : RAW with module Info := Info)
    (For : sig val initials : Lrc.set end)
  : S with module Info := Info and type n = Lrc.n =
struct
  include Lrc

  type n = Lrc.n
  let n = Lrc.n

  let reachable = ref IndexSet.empty

  let successors =
    let table = Vector.make Lrc.n IndexSet.empty in
    let todo = ref For.initials in
    let populate i =
      reachable := IndexSet.add i !reachable;
      if IndexSet.is_empty (Vector.get table i) then (
        let succ = Lrc.all_successors i in
        todo := IndexSet.union succ !todo;
        Vector.set table i succ;
      )
    in
    while not (IndexSet.is_empty !todo) do
      let todo' = !todo in
      todo := IndexSet.empty;
      IndexSet.rev_iter populate todo';
    done;
    table

  let predecessors = Misc.relation_reverse successors

  let successors = Vector.get successors
  let predecessors = Vector.get predecessors

  let reachable = !reachable

  let idle = IndexSet.inter Lrc.all_idle reachable

  let some_prefix = lazy (
    let table = Vector.make Lrc.n [] in
    let todo = ref [] in
    let expand prefix state =
      match Vector.get table state with
      | [] ->
        Vector.set table state prefix;
        let prefix = state :: prefix in
        let succ = successors state in
        if not (IndexSet.is_empty succ) then
          push todo (succ, prefix)
      | _ -> ()
    in
    Index.iter Info.Lr1.n (fun lr1 ->
        if Option.is_none (Info.Lr1.incoming lr1) then
          expand [] (Lrc.first_lrc_of_lr1 lr1)
      );
    let propagate (succ, prefix) =
      IndexSet.iter (expand prefix) succ
    in
    fixpoint ~propagate todo;
    Vector.get table
  )

  let some_prefix st = Lazy.force some_prefix st

end

module Make
    (I : Info.S)
    (Reachability : Reachability.S with module Info := I)
: RAW with module Info := I
=
struct
  open I

  let time = Stopwatch.enter Stopwatch.main "Lrc"

  let n =
    let count lr1 = Array.length (Reachability.Classes.for_lr1 lr1) in
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

  let lr1_of_lrc = Vector.make' n (fun () -> Index.of_int Lr1.n 0)

  let lrcs_of_lr1 =
    let count = ref 0 in
    let init_lr1 lr1 =
      let classes = Reachability.Classes.for_lr1 lr1 in
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
    Vector.init Lr1.n init_lr1

  let first_lrc_of_lr1 = Vector.map (fun x -> Option.get (IndexSet.minimum x)) lrcs_of_lr1

  let lr1_of_lrc       = Vector.get lr1_of_lrc
  let lrcs_of_lr1      = Vector.get lrcs_of_lr1
  let first_lrc_of_lr1 = Vector.get first_lrc_of_lr1

  let all_idle = IndexSet.map first_lrc_of_lr1 Lr1.idle

  let class_index lrc =
    let lr1 = lr1_of_lrc lrc in
    let lrc0 = first_lrc_of_lr1 lr1 in
    index_delta lrc lrc0

  let lookahead lrc =
    let lr1 = lr1_of_lrc lrc in
    let lrc0 = first_lrc_of_lr1 lr1 in
    let lookaheads = Reachability.Classes.for_lr1 lr1 in
    lookaheads.(index_delta lrc lrc0)

  let () = Stopwatch.step time "Computed LRC set"

  let all_successors =
    (* TODO: this computes the predecessors and then reverse the relation.
     * We could compute the successors directly. *)
    let table = Vector.make n IndexSet.empty in
    let process lr1 =
      let tgt_first = first_lrc_of_lr1 lr1 in
      match Option.map Symbol.desc (Lr1.incoming lr1) with
      | None -> ()
      | Some (T t) ->
        Vector.set table tgt_first @@
        List.fold_left (fun acc tr ->
          let src = Transition.source tr in
          let class_index =
            Misc.array_findi
              (fun _ classe -> IndexSet.mem t classe) 0 (Reachability.Classes.for_lr1 src)
          in
          let src_index = index_shift (first_lrc_of_lr1 src) class_index in
          IndexSet.add src_index acc
        ) IndexSet.empty (Transition.predecessors lr1)
      | Some _ ->
        let process_transition tr =
          let node = Reachability.Tree.leaf tr in
          let cells = Vector.get Reachability.Cells.table node in
          let pre_classes = Reachability.Classes.pre_transition tr in
          let post_classes = Reachability.Classes.post_transition tr in
          let coercion =
            Reachability.Coercion.infix post_classes
              (Reachability.Classes.for_lr1 lr1)
          in
          let src_first = first_lrc_of_lr1 (Transition.source tr) in
          let pre_classes = Array.length pre_classes in
          let post_classes = Array.length post_classes in
          for post = 0 to post_classes - 1 do
            let reachable = ref IndexSet.empty in
            for pre = 0 to pre_classes - 1 do
              let index = Reachability.Cells.table_index ~post_classes ~pre ~post in
              if cells.(index) < max_int then
                reachable := IndexSet.add (index_shift src_first pre) !reachable
            done;
            let reachable = !reachable in
            Array.iter
              (fun index ->
                vector_set_union table
                  (index_shift tgt_first index) reachable)
              coercion.forward.(post)
          done
        in
        List.iter process_transition (Transition.predecessors lr1)
    in
    Index.iter Lr1.n process;
    Vector.get (Misc.relation_reverse table)

  let () = Stopwatch.leave time
end

module Minimize(Info : Info.S)(Lrc : RAW0 with module Info := Info) : RAW0 with module Info := Info =
struct
  open Info
  let time = Stopwatch.enter Stopwatch.main "Minimizing Lrc"

  module DFA = struct
    type states = Lrc.n
    let states = Lrc.n

    module Transitions = Vector.Of_array(struct
      type a = Lrc.n index * Lrc.n index
      let array =
        let count = ref 0 in
        Index.iter Lrc.n
          (fun lrc -> count := !count + IndexSet.cardinal (Lrc.all_successors lrc));
        let array = Array.make !count (Index.of_int Lrc.n 0, Index.of_int Lrc.n 0) in
        let index = ref 0 in
        Index.iter Lrc.n
          (fun src ->
            IndexSet.iter (fun tgt ->
              array.(!index) <- (src, tgt);
              incr index;
            ) (Lrc.all_successors src)
          );
        array
    end)

    type transitions = Transitions.n
    let transitions = Vector.length Transitions.vector

    let label tr =
      let src, _ = Vector.get Transitions.vector tr in
      Lrc.lr1_of_lrc src

    let source tr = fst (Vector.get Transitions.vector tr)

    let target tr = snd (Vector.get Transitions.vector tr)

    let initials f =
      IndexSet.iter f Lrc.all_idle

    let finals f =
      Index.iter Lr1.n
        (fun lr1 ->
          match Lr1.incoming lr1 with
          | None -> IndexSet.iter f (Lrc.lrcs_of_lr1 lr1)
          | Some _ -> ())

    let refinements (f : (add:(states index -> unit) -> unit) -> unit) =
      Index.iter Lr1.n
        (fun lr1 ->
          match Lr1.incoming lr1 with
          | None ->
            f (fun ~add -> IndexSet.iter add (Lrc.lrcs_of_lr1 lr1))
          | Some _ -> ()
        )
  end

  module MDFA = Valmari.Minimize(struct
    type t = Lr1.t
    let compare = compare_index
  end)(DFA)

  type n = MDFA.states
  let n = MDFA.states

  type t = n index
  type set = n indexset
  type 'a map = (n, 'a) indexmap

  let all_idle =
    let set = ref IndexSet.empty in
    Array.iter (fun t -> set := IndexSet.add t !set) MDFA.initials;
    !set
    (*IndexSet.filter_map MDFA.transport_state Lrc.idle*)

  let lr1_of_lrc =
    tabulate_finset n
      (fun t -> Lrc.lr1_of_lrc (MDFA.represent_state t))

  let lrcs_of_lr1 =
    let table = Vector.make Lr1.n IndexSet.empty in
    Index.iter n
      (fun lrc -> vector_set_add table (lr1_of_lrc lrc) lrc);
    Vector.get table

  let first_lrc_of_lr1 lr1 =
    Option.get (IndexSet.minimum (lrcs_of_lr1 lr1))

  let all_successors =
    let table = Vector.make n IndexSet.empty in
    Index.iter MDFA.transitions
      (fun tr -> vector_set_add table (MDFA.target tr) (MDFA.source tr));
    Vector.get table

  let () =
    Stopwatch.step time "Minimized Lrc from %d states to %d"
      (cardinal Lrc.n) (cardinal MDFA.states);
    Stopwatch.leave time
end
