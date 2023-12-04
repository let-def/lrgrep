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
  val predecessors_by_lr1 : t -> set Lr1.map
  val set_predecessors_by_lr1 : set -> set Lr1.map
  val lookahead : n index -> Terminal.set
  val decompose : n index -> Lr1.t * Terminal.set
end

module Make (I : Info.S)(Reachability : Reachability.S with module Info := I) : S with module Info := I =
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

  let decompose lrc =
    let lr1 = lr1_of_lrc lrc in
    let lrc0 = first_lrc_of_lr1 lr1 in
    let lookaheads = Reachability.Classes.for_lr1 lr1 in
    (lr1, lookaheads.(index_delta lrc lrc0))

  let lookahead lrc = snd (decompose lrc)

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

  let () = Stopwatch.leave time
end

module Minimize(I : Info.S)(Lrc : S with module Info := I) : sig
  (*open I*)
  (*include Info.INDEXED

  val idle : set
  val lr1_of_lrc : t -> Lr1.t
  val lrcs_of_lr1 : Lr1.t -> set
  val first_lrc_of_lr1 : Lr1.t -> t
  val predecessors : t -> set
  val lookahead : n index -> Terminal.set
  val decompose : n index -> Lr1.t * Terminal.set*)
end =
struct
  open I
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
      let _, tgt = Vector.get Transitions.vector tr in
      let lr1, _ = Lrc.decompose tgt in
      lr1

    let source tr = fst (Vector.get Transitions.vector tr)

    let target tr = snd (Vector.get Transitions.vector tr)

    let initials f =
      IndexSet.iter f Lrc.idle

    let finals f =
      Index.iter Lrc.n (fun lrc ->
        let lr1, _ = Lrc.decompose lrc in
        match Lr1.incoming lr1 with
        | None -> f lrc
        | Some _ -> ()
      )

    let refinements _ = ()
  end

  module MDFA = Valmari.Minimize(struct
    type t = Lr1.t
    let compare = compare_index
  end)(DFA)

  let () =
    Stopwatch.step time "Minimized Lrc from %d states to %d\n"
      (cardinal Lrc.n) (cardinal MDFA.states);
    Stopwatch.leave time
end
