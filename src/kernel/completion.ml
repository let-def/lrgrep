open Fix.Indexing
open Utils
open Misc
open Info

module Make(G : sig
    type g
    val g : g grammar
  end) =
struct
  open G

  (*let shift_transitions = Vector.make Lr1.n IndexSet.empty

    let () =
    Index.rev_iter Transition.shift @@ fun tr ->
    vector_set_add shift_transitions Transition.(source (of_shift tr)) tr

    let reductions_at =
    tabulate_finset Lr1.n @@ fun lr1 ->
    IndexSet.filter
      (fun red ->
         let prod = Reduction.production red in
         match Production.kind prod with
         | `START -> false
         | `REGULAR -> true)
      (Reduction.from_lr1 lr1)

    type stack =
    | Con of Lr1.n indexset * stack
    | Abs of Lr1.n indexset

    let rec pred_n states = function
    | 0 -> states
    | n ->
      assert (n >= 0);
      let n = n - 1 in
      match states with
      | Con (_, stack) -> pred_n stack n
      | Abs states -> pred_n (Abs (indexset_bind states Lr1.predecessors)) n

    let states (Con (s, _) | Abs s) = s

    let table = Hashtbl.create 7

    let populate path =
    match Hashtbl.find_opt table path with
    | Some r -> r
    | None ->
      let r = ref IndexSet.empty in
      Hashtbl.add table path r;
      r

    let count = ref 0

    let register path _stack =
    incr count;
    let r = populate path in
    let source = Index.of_int Lr1.n 0 in
    r := IndexSet.add source !r;
    Printf.eprintf "path %d: %s %s\n"
      !count
      (Lr1.to_string source)
      (string_concat_map " " Terminal.to_string path)
    (*Printf.eprintf "stack: %s\n"
      (Lr1.list_to_string (List.rev stack))*)

    let rec explore path stack = function
    | 0 -> register path stack
    | n ->
      explore_state path stack (n - 1) (ref IndexSet.empty) Terminal.all

    and explore_state path stack n visited lookaheads =
    let states = states stack in
    let targets =
      IndexSet.fold (fun st map ->
          IndexSet.fold (fun tr map ->
              let sym = Transition.shift_symbol tr in
              if lookaheads == Terminal.all || IndexSet.mem sym lookaheads then
                IndexMap.update sym (function
                    | None -> Some (IndexSet.singleton Transition.(target (of_shift tr)))
                    | Some s -> Some (IndexSet.add Transition.(target (of_shift tr)) s)
                  ) map
              else
                map
            ) shift_transitions.:(st) map
        ) states IndexMap.empty
    in
    IndexMap.iter (fun sym set ->
        explore (sym :: path) (Con (set, stack)) n
      ) targets;
    let reductions = indexset_bind states reductions_at in
    let cache = Array.make 8 IndexSet.empty in
    let rec naive depth states =
      if depth = 0 then
        states
      else
        naive (depth - 1) (indexset_bind states Lr1.predecessors)
    in
    let rec get_cache depth states =
      if cache.(depth) != IndexSet.empty then
        cache.(depth)
      else
        let states = if depth = 0 then states else get_cache (depth - 1) states in
        let states = indexset_bind states Lr1.predecessors in
        cache.(depth) <- states;
        states
    in
    let rec get_stack depth = function
      | stack when depth = 0 -> stack
      | Con (_, stack) -> get_stack (depth - 1) stack
      | Abs states ->
        if depth >= 8 then
          let states = get_cache 7 states in
          Abs (naive (depth - 8) states)
        else
          Abs (get_cache depth states)
    in
    let stack depth = get_stack depth stack in
    IndexSet.iter (reduce path stack n visited lookaheads) reductions

    and reduce path stack n visited lookaheads red =
    let lookaheads' = IndexSet.inter lookaheads (Reduction.lookaheads red) in
    if not (IndexSet.is_empty lookaheads') then (
      let stack = stack (Production.length (Reduction.production red)) in
      let states' =
        IndexSet.filter_map (fun source ->
            try
              Some (Transition.find_goto_target source
                    (Production.lhs (Reduction.production red)))
            with Not_found -> None
          ) (states stack)
      in
      let states'' = IndexSet.diff states' !visited in
      if not (IndexSet.is_empty states'') then (
        visited := IndexSet.union states'' !visited;
        explore_state path (Con (states'', stack)) n visited lookaheads'
      )
    )

    let () =
    Vector.iter (fun trs ->
        IndexSet.iter (fun tr ->
            let source, target =
              let tr = Transition.of_shift tr in
              Transition.(source tr, target tr)
            in
            explore [Transition.shift_symbol tr] (Con (IndexSet.singleton target, Abs (IndexSet.singleton source))) 1
          ) trs
      ) shift_transitions


    let () =
    let total = cardinal Lr1.n * cardinal Terminal.n * cardinal Terminal.n in
    Printf.eprintf "Found %d paths with three terminals out of %d (%.02f%%)\n" !count total
      (float !count /. float total *. 100.0);
    let sets = Hashtbl.fold (fun _ v acc -> !v :: acc) table [] in
    let sets = List.sort_uniq IndexSet.compare sets in
    Printf.eprintf "%d possible sequences out of %d (%.02f%%), %d classifying set of states\n"
      (Hashtbl.length table)
      (cardinal Terminal.n * cardinal Terminal.n)
      (float (Hashtbl.length table) /. float (cardinal Terminal.n * cardinal Terminal.n) *. 100.0)
      (List.length sets);
  *)

  (* Helper to compute the iterated predecessors *)
  let pred_n state rdepth depth =
    let states = ref (Lr1.predecessors g state) in
    assert (!rdepth <= depth);
    while !rdepth < depth do
      states := Lazy.force states.contents.lnext;
      incr rdepth;
    done;
    states.contents.lvalue

  let rec is_sorted compare = function
    | [] | [_] -> true
    | x :: (y :: _ as ys) ->
      compare x y <= 0 && is_sorted compare ys

  (* Compute which states an LR(1) state can reduce to.
     E.g. [(st', la) ∈ reduce_to.:(st)] if it is possible, when in state [st] and
     looking ahead at a symbol in [la], to reduce to a configuration ending in
     state [st'].

     Initially, relate only states reachable by a single reduction... *)
  let reduce_to =
    Vector.init (Lr1.cardinal g) @@ fun lr1 ->
    let reductions =
      IndexSet.fold begin fun red acc ->
        let prod = Reduction.production g red in
        let depth = Production.length g prod in
        (depth, red) :: acc
      end (Reduction.from_lr1 g lr1) []
    in
    assert (is_sorted (fun (d1, _) (d2, _) -> Int.compare d1 d2) reductions);
    let states_at = pred_n lr1 (ref 0) in
    List.fold_left begin fun acc (depth, r) ->
      let nt = Production.lhs g (Reduction.production g r) in
      let lookaheads = Reduction.lookaheads g r in
      let update = function
        | None -> Some lookaheads
        | Some lookaheads' -> Some (IndexSet.union lookaheads lookaheads')
      in
      IndexSet.fold
        (fun lr1 acc ->
           let target = Transition.find_goto_target g lr1 nt in
           IndexMap.update target update acc)
        (states_at depth) acc
    end IndexMap.empty reductions

  (* Now close the relation, relate states reachable by any chain of reductions.
     (fixed point computation) *)
  let () =
    let table = Vector.make (Lr1.cardinal g) IndexMap.empty in
    Vector.iteri begin fun source targets ->
      IndexMap.iter begin fun target lookaheads ->
        let update = function
          | None -> Some lookaheads
          | Some lookaheads' -> Some (IndexSet.union lookaheads lookaheads')
        in
        table.:(target) <- IndexMap.update source update table.:(target)
      end targets
    end reduce_to;
    let deltas = ref [] in
    let apply_delta tgt delta =
      IndexMap.iter begin fun src la ->
        let map = reduce_to.:(src) in
        let delta = IndexMap.filter_map
            (fun goal la' ->
               let la = IndexSet.inter la la' in
               if IndexSet.is_empty la
               then None
               else match IndexMap.find_opt goal map with
                 | None -> Some la
                 | Some la' ->
                   let la = IndexSet.diff la la' in
                   if IndexSet.is_empty la
                   then None
                   else Some la)
            delta
        in
        if not (IndexMap.is_empty delta) then (
          push deltas (src, delta);
          reduce_to.:(src) <-
            IndexMap.union
              (fun _ la la' -> Some (IndexSet.union la la'))
              map delta
        )
      end table.:(tgt)
    in
    Vector.iteri apply_delta reduce_to;
    let counter = ref 0 in
    fixpoint ~counter ~propagate:(fun (tgt, delta) -> apply_delta tgt delta) deltas;
    Printf.eprintf "Converged after %d iterations\n" !counter

  (* Find all states that it is possible to shift to from a given state
     (that is, by simply following a shift transition). *)
  let shift_to =
    let table = Vector.make (Lr1.cardinal g) IndexSet.empty in
    Index.rev_iter (Transition.shift g)
      (fun tr -> table.@(Transition.(source g (of_shift g tr))) <- IndexSet.add tr);
    table

  (* Now compose reduce_to and shift_to to find all states that it is possible to
     shift, allowing an arbitrary number of reductions in between. *)
  let shift_closure =
    Vector.init (Transition.shift g) @@ fun tr ->
    let lr1 = Transition.(target g (of_shift g tr)) in
    IndexMap.fold begin fun lr1' lookaheads targets ->
      let targets' =
        IndexSet.filter
          (fun tr -> IndexSet.mem (Transition.shift_symbol g tr) lookaheads)
          shift_to.:(lr1')
      in
      IndexSet.union targets' targets
    end reduce_to.:(lr1) shift_to.:(lr1)
  (*|> group_by_terminal*)

  (* Now reverse the relation *)
  let rev_shift_closure =
    relation_reverse (Vector.length shift_closure) shift_closure

  let () =
    stopwatch 1 "Precomputed data structures for fast enumeration"

  let add_update x = function
    | None -> Some (IndexSet.singleton x)
    | Some xs -> Some (IndexSet.add x xs)

  (* Now quickly enumerate the predecessors and successors of a shift transition.
     This gives us the terminal trigrams that are allowed by the grammar together
     with the states in which they are allowed.

     In [let pred, succ = fast_enum sh], where [sh] is a shift transition:
     - [pred] is a map from terminals to lr1 states such that
       [st ∈ pred(t)] if when in state [st], looking ahead at [t] it is possible
       to get to a configuration in which [sh] will be reachable next.
     - [succ] is the set of terminals that will be permitted after following [sh]
  *)
  let fast_enum tr =
    let terminals =
      IndexSet.map (Transition.shift_symbol g) shift_closure.:(tr)
    in
    let pred = rev_shift_closure.:(tr) in
    let group =
      IndexSet.fold begin fun tr acc ->
        IndexMap.update (Transition.shift_symbol g tr)
          (add_update (Transition.(source g (of_shift g tr)))) acc
      end pred IndexMap.empty
    in
    (group, terminals)

  let () =
    let term = Terminal.cardinal g in
    (* Allocate a big cube to map a trigram to the set of LR(1) states it could
       follow (if there exists a parser in state [st] that accepts an input
       starting by [t₀t₁t₂], then [st ∈ path3.(t₀).(t₁).(t₂)]). *)
    let term3 = Prod.cardinal term (Prod.cardinal term term) in
    let path3 =
      Vector.init term (fun _ ->
          Vector.init term (fun _ ->
              Vector.make term IndexSet.empty))
    in
    (* Same contents, but as a linear array (just experimenting, clean up
       later). *)
    let paths = Vector.make term3 IndexSet.empty in
    let cons x y = Prod.inj term x y in
    (* Populate the arrays *)
    Index.iter (Transition.shift g) begin fun tr ->
      let (pred, succ) = fast_enum tr in
      let p0 = Transition.shift_symbol g tr in
      let path = p0 in
      IndexSet.iter begin fun s ->
        let p1 = s in
        let path = cons s path in
        IndexMap.iter begin fun t img ->
          let p2 = t in
          let path = cons t path in
          paths.:(path) <- IndexSet.union img paths.:(path);
          path3.:(p0).:(p1).:(p2) <- IndexSet.union img path3.:(p0).:(p1).:(p2);
        end pred
      end succ;
    end;
    (* Now we are trying to devise a compact and efficient representation for
       marshalling the arrays.  Let's compute some statistics. *)
    (* Number of unique mappings from the last element of a trigram to set of
       states *)
    let uniq_last = Hashtbl.create 7 in
    Vector.iter begin Vector.iter begin fun a ->
        if not (Hashtbl.mem uniq_last a) then
          Hashtbl.add uniq_last a (Hashtbl.length uniq_last);
      end end path3;
    (* Number of unique mappings from the middle element of a trigram to mappings
       of the last element to set of states *)
    let uniq_mid = Hashtbl.create 7 in
    Vector.iter begin fun a ->
      let a' = Vector.map (Hashtbl.find uniq_last) a in
      if not (Hashtbl.mem uniq_mid a') then
        Hashtbl.add uniq_mid a' (Hashtbl.length uniq_mid);
    end path3;
    Printf.eprintf "Last images: %d/%d = %.02f%%\n%!"
      (Hashtbl.length uniq_last) (cardinal term * cardinal term)
      (float (Hashtbl.length uniq_last) /.
       float (cardinal term * cardinal term) *. 100.0)
    ;
    (* ... Basically, we are trying to find which level of the trie mapping
       is the most efficient to share. *)

    (* Other states:
       - count empty and non-empty cells of the cube
       - count the number of unique and shared sets, on average and at most per
         last-level
    *)
    let empty = ref 0 in
    let non_empty = ref 0 in
    Hashtbl.iter begin fun a _ ->
      Vector.iter begin fun s ->
        if IndexSet.is_empty s then incr empty else incr non_empty
      end a
    end uniq_last;
    let diff_sets = ref 0 in
    let max_sets = ref 0 in
    let count = ref 0 in
    let all_sets = Hashtbl.create 7 in
    let shared_sets = Hashtbl.create 7 in
    Hashtbl.iter begin fun a _ ->
      let sets = Hashtbl.create 7 in
      Vector.iter (fun s ->
          if not (Hashtbl.mem sets s) then (
            if Hashtbl.mem all_sets s then
              Hashtbl.replace shared_sets s ()
            else
              Hashtbl.add all_sets s ();
            Hashtbl.add sets s ()
          )
        ) a;
      diff_sets := !diff_sets + Hashtbl.length sets;
      max_sets := max !max_sets (Hashtbl.length sets);
      incr count
    end uniq_last;
    Printf.eprintf "Filled: %d/%d = %.02f%%\n%!"
      !non_empty (!non_empty + !empty)
      (float !non_empty /. float (!non_empty + !empty) *. 100.0)
    ;
    Printf.eprintf "Different sets: average:%.02f shared:%d max:%d\n"
      (float !diff_sets /. float !count)
      (Hashtbl.length shared_sets)
      !max_sets
    ;
    Printf.eprintf "Mid images: %d/%d = %.02f%%\n%!"
      (Hashtbl.length uniq_mid) (cardinal term)
      (float (Hashtbl.length uniq_mid) /. float (cardinal term) *. 100.0)
    ;
    let seq_count =
      Vector.fold_left (fun sum path ->
          if IndexSet.is_empty path
          then sum
          else sum + 1
        ) 0 paths
    in
    (* How many trigrams are allowed out of all possible ones? *)
    Printf.eprintf "Sequence efficiency: %d/%d = %.02f%%\n%!"
      seq_count (cardinal term3)
      (float seq_count /. float (cardinal term3) *. 100.0);

    (* Now lets try a packing scheme... *)

    (* First, give a unique, monotonically increasing, index to each set.
       [-1] is reserved for the empty set. *)
    let index = Hashtbl.create 7 in
    let index_of st =
      if IndexSet.is_empty st then -1 else
        match Hashtbl.find_opt index st with
        | Some x -> x
        | None ->
          let x = Hashtbl.length index in
          Hashtbl.add index st x;
          x
    in
    let indexes = Vector.map index_of paths in
    (*let sets = Hashtbl.fold (fun x _ acc -> x :: acc) index [] in
      let tolerance = 2 in (* one tenth of elements can differ *)
      let rec visit = function
      | [] -> []
      | set :: sets ->
        let c = IndexSet.cardinal set in
        let sets =
          List.filter (fun set' ->
              let c' = IndexSet.cardinal set' in
              if c * tolerance < c' * (tolerance + 1) && c' * tolerance < c * (tolerance + 1) then (
                let diff = IndexSet.cardinal (IndexSet.diff set set') in
                let diff' = IndexSet.cardinal (IndexSet.diff set' set) in
                if (diff + diff') < c / tolerance then (
                  Printf.eprintf "found sets of size %d and %d differing by %d elements\n"
                    c c' (diff + diff');
                  false
                ) else true
              ) else true
            ) sets
        in
        set :: visit sets;
      in
      let sets' = visit sets in*)
    let card = Array.make (Hashtbl.length index) 0 in
    Hashtbl.iter (fun st i -> card.(i) <- IndexSet.cardinal st) index;
    (* Number of elements, if we were to represent all sets as a contiguous sequence of elements. *)
    let st_count = Vector.fold_left (fun sum i -> if i = -1 then sum else sum + card.(i)) 0 indexes in
    let st_max = cardinal term3 * cardinal (Lr1.cardinal g) in
    (* Estimate how much bits would be needed to store all the sets, assuming a
       state fit in a 16 bit integer and we use delta encoding. *)
    Printf.eprintf "State efficiency: %d/%d = %.02f%%, %d groups\n%!" (* (or %d with %.02f%% error)\n%! *)
      st_count st_max (float st_count /. float st_max *. 100.0) (Hashtbl.length index) (*(List.length sets') (100. /. float tolerance)*);
    let output_b16 i =
      let b0 = i land 0xFF in
      let b1 = (i lsr 8) land 0xFF in
      output_byte stdout b1;
      output_byte stdout b0;
    in
    let unused = ref 0 in
    Vector.iter begin fun i ->
      if i = -1 then incr unused
      (*else output_b16 (i + 1)*)
    end indexes;
    let bits = ref 0 in
    Hashtbl.iter (fun st _ ->
        let c = IndexSet.cardinal st in
        output_b16 c;
        let last = ref 0 in
        IndexSet.iter (fun i ->
            let i = Index.to_int i in
            let d = i - !last - 1 in
            if d < 8 then
              bits := !bits + 4
            else if d < 128 then
              bits := !bits + 8
            else
              bits := !bits + 16;
            (*output_b16 (i - !last);*)
            last := i
          ) st;
      ) index;
    Printf.eprintf "Sets: %d bits, %d bytes\n" !bits ((!bits + 7) / 8);
    Printf.eprintf "Unused cells: %d / %d (%.02f%%)\n" !unused
      (Vector.length_as_int indexes)
      (100. *. float !unused /. float (Vector.length_as_int indexes));
    stopwatch 1 "Enumerated paths";
    (* Idea for the future:
       - represent the trigrams as a sequence of tries, with sharing (use statistics to estimate the cost)
       - give an index to each unique non-empty set of states, then:
       - store a mapping from trigrams to indices
       - store for each LR(1) state the indices of sets that contain it
    *)
end
