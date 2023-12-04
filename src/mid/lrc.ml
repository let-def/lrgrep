open Utils
open Misc
open Fix.Indexing

module type S = sig
  include Info.INDEXED
  module Info : Info.S
  open Info

  val idle : set
  val lr1_of_lrc : t -> Lr1.t
  val lrcs_of_lr1 : Lr1.t -> set
  val first_lrc_of_lr1 : Lr1.t -> t
  val predecessors : t -> set
end

module Make
    (I : Info.S)
    (Reachability : Reachability.S with module Info := I) :
sig
  include S with module Info := I
  val lookahead : n index -> I.Terminal.set
end =
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

  let lr1_of_lrc, lrcs_of_lr1, first_lrc_of_lr1 =
    let lr1_of_lrc = Vector.make' n (fun () -> Index.of_int Lr1.n 0) in
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
    let lrcs_of_lr1 = Vector.init Lr1.n init_lr1 in
    (Vector.get lr1_of_lrc,
      Vector.get lrcs_of_lr1,
      (fun lr1 -> Option.get (IndexSet.minimum (Vector.get lrcs_of_lr1 lr1)))
    )

  let idle = IndexSet.map first_lrc_of_lr1 Lr1.idle

  let lookahead lrc =
    let lr1 = lr1_of_lrc lrc in
    let lrc0 = first_lrc_of_lr1 lr1 in
    let lookaheads = Reachability.Classes.for_lr1 lr1 in
    lookaheads.(index_delta lrc lrc0)

  let () = Stopwatch.step time "Computed LRC set"

  let predecessors =
    let predecessors = Vector.make n IndexSet.empty in
    let process lr1 =
      let first_lrc = first_lrc_of_lr1 lr1 in
      match Lr1.incoming lr1 with
      | None -> ()
      | Some sym when Symbol.is_terminal sym ->
        Vector.set predecessors first_lrc @@
        List.fold_left (fun acc tr ->
          let src = Transition.source tr in
          let lrc_first = first_lrc_of_lr1 src in
          let count = Array.length (Reachability.Classes.for_lr1 src) in
          let lrc_last = index_shift lrc_first (count - 1) in
          IndexSet.union acc (IndexSet.init_interval lrc_first lrc_last)
        ) IndexSet.empty (Transition.predecessors lr1)
      | Some _ ->
        let process_transition tr =
          let source_lrc = first_lrc_of_lr1 (Transition.source tr) in
          let node = Reachability.Tree.leaf tr in
          let table = Vector.get Reachability.Cells.table node in
          let pre_classes = Reachability.Classes.pre_transition tr in
          let post_classes = Reachability.Classes.post_transition tr in
          let coercion =
            Reachability.Coercion.infix post_classes
              (Reachability.Classes.for_lr1 lr1)
          in
          let pre_classes = Array.length pre_classes in
          let post_classes = Array.length post_classes in
          for post = 0 to post_classes - 1 do
            let reachable = ref IndexSet.empty in
            for pre = 0 to pre_classes - 1 do
              let index = Reachability.Cells.table_index ~post_classes ~pre ~post in
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
    Vector.get predecessors

  let () = Stopwatch.step time "Computed predecessors"

  let () = Stopwatch.leave time
end

(* FIXME: TODO: The minimization code has a bug when reaching an initial state. (limit condition) *)
module Minimize(Info : Info.S)(Lrc : S with module Info := Info) : S with module Info := Info =
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
          (fun lrc -> count := !count + IndexSet.cardinal (Lrc.predecessors lrc));
        let array = Array.make !count (Index.of_int Lrc.n 0, Index.of_int Lrc.n 0) in
        let index = ref 0 in
        Index.iter Lrc.n
          (fun src ->
            IndexSet.iter (fun tgt ->
              array.(!index) <- (src, tgt);
              incr index;
            ) (Lrc.predecessors src)
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
      IndexSet.iter f Lrc.idle

    let finals f =
      Index.iter Lr1.n
        (fun lr1 ->
          match Lr1.incoming lr1 with
          | None -> IndexSet.iter f (Lrc.lrcs_of_lr1 lr1)
          | Some _ -> ())

    let refinements _ = ()
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

  let idle =
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

  let predecessors =
    let table = Vector.make n IndexSet.empty in
    Index.iter MDFA.transitions
      (fun tr -> vector_set_add table (MDFA.source tr) (MDFA.target tr));
    Vector.get table

  let () =
    Stopwatch.step time "Minimized Lrc from %d states to %d"
      (cardinal Lrc.n) (cardinal MDFA.states);
    Stopwatch.leave time
end
