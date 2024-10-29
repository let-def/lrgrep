open Fix.Indexing
open Utils
open Misc

let grammar_filename =
  let filename, oc = Filename.open_temp_file "lrgrep-interpreter" "cmly" in
  output_string oc Interpreter_data.grammar;
  close_out oc;
  filename

module Grammar = MenhirSdk.Cmly_read.Read(struct let filename = grammar_filename end)
let () = Sys.remove grammar_filename

module Info = Mid.Info.Make(Grammar)
open Info

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
          ) (Vector.get shift_transitions st) map
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

let time = Stopwatch.create ()

let pred_n states rdepth depth =
  assert (!rdepth <= depth);
  while !rdepth < depth do
    states := indexset_bind !states Lr1.predecessors;
    incr rdepth;
  done;
  !states

let reduce_to =
  Vector.init Lr1.n (fun lr1 ->
      let reductions =
        IndexSet.fold (fun red acc ->
            let prod = Reduction.production red in
            let depth = Production.length prod in
            (depth, red) :: acc
          ) (Reduction.from_lr1 lr1) []
        |> List.sort (fun (d1, _) (d2, _) -> Int.compare d1 d2)
      in
      let states_at = pred_n (ref (IndexSet.singleton lr1)) (ref 0) in
      List.fold_left (fun acc (depth, r) ->
          let nt = Production.lhs (Reduction.production r) in
          let lookaheads = Reduction.lookaheads r in
          let update = function
            | None -> Some lookaheads
            | Some lookaheads' -> Some (IndexSet.union lookaheads lookaheads')
          in
          IndexSet.fold
            (fun lr1 acc ->
               let target = Transition.find_goto_target lr1 nt in
               IndexMap.update target update acc)
            (states_at depth) acc
        ) IndexMap.empty reductions
    )

let () =
  let table = Vector.make Lr1.n IndexMap.empty in
  Vector.iteri (fun source targets ->
      IndexMap.iter (fun target lookaheads ->
          let update = function
            | None -> Some lookaheads
            | Some lookaheads' -> Some (IndexSet.union lookaheads lookaheads')
          in
          Vector.set table target
            (IndexMap.update source update (Vector.get table target))
        ) targets
    ) reduce_to;
  let deltas = ref [] in
  let apply_delta tgt delta =
    IndexMap.iter (fun src la ->
        let map = Vector.get reduce_to src in
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
          Vector.set reduce_to src
            (IndexMap.union
               (fun _ la la' -> Some (IndexSet.union la la'))
               map delta)
        )
      )
      (Vector.get table tgt)
  in
  Vector.iteri apply_delta reduce_to;
  let iter = ref 0 in
  while !deltas <> [] do
    incr iter;
    let deltas' = !deltas in
    deltas := [];
    List.iter (fun (tgt, delta) -> apply_delta tgt delta) deltas';
  done;
  Printf.eprintf "Converged after %d iterations\n" !iter

let shift_to =
  let table = Vector.make Lr1.n IndexSet.empty in
  Index.iter Transition.shift
    (fun tr -> vector_set_add table Transition.(source (of_shift tr)) tr);
  table

let shift_closure =
  Vector.init Transition.shift (fun tr ->
      let lr1 = Transition.(target (of_shift tr)) in
      IndexMap.fold (fun lr1' lookaheads targets ->
          let targets' = Vector.get shift_to lr1' in
          let targets' =
            IndexSet.filter
              (fun tr -> IndexSet.mem (Transition.shift_symbol tr) lookaheads)
              targets'
          in
          IndexSet.union targets' targets
        )
        (Vector.get reduce_to lr1)
        (Vector.get shift_to lr1)
      (*|> group_by_terminal*)
    )

let rev_shift_closure = relation_reverse shift_closure

let () =
  Stopwatch.step time "Precomputed data structures for fast enumeration"

let fast_enum tr =
  let terminals =
    IndexSet.map Transition.shift_symbol (Vector.get shift_closure tr)
  in
  let pred = Vector.get rev_shift_closure tr in
  let group =
    IndexSet.fold (fun tr acc ->
        let src = Transition.(source (of_shift tr)) in
        IndexMap.update (Transition.shift_symbol tr) (function
            | None -> Some (IndexSet.singleton src)
            | Some targets -> Some (IndexSet.add src targets)
          ) acc
      ) pred IndexMap.empty
  in
  (group, terminals)

let cons t i = cardinal Terminal.n * i + Index.to_int t

let () =
  let seq_max = cardinal Terminal.n * cardinal Terminal.n * cardinal Terminal.n in
  let paths = Array.make seq_max IndexSet.empty in
  Index.iter Transition.shift (fun tr ->
      let (pred, succ) = fast_enum tr in
      let path = Index.to_int (Transition.shift_symbol tr) in
      IndexSet.iter (fun s ->
          let path = cons s path in
          IndexMap.iter (fun t img ->
              let path = cons t path in
              paths.(path) <- IndexSet.union img paths.(path)
            ) pred
        ) succ;
    );
  let seq_count = ref 0 in
  for i = 0 to seq_max - 1 do
    if not (IndexSet.is_empty paths.(i)) then
      incr seq_count;
  done;
  Printf.eprintf "Sequence efficiency: %d/%d = %.02f%%\n%!"
    !seq_count seq_max (float !seq_count /. float seq_max *. 100.0);
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
  let indexes = Array.map index_of paths in
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
  let st_count = Array.fold_left (fun sum i -> if i = -1 then sum else sum + card.(i)) 0 indexes in
  let st_max = seq_max * cardinal Lr1.n in
  Printf.eprintf "State efficiency: %d/%d = %.02f%%, %d groups\n%!" (* (or %d with %.02f%% error)\n%! *)
    st_count st_max (float st_count /. float st_max *. 100.0) (Hashtbl.length index) (*(List.length sets') (100. /. float tolerance)*);
  let output_b16 i =
    let b0 = i land 0xFF in
    let b1 = (i lsr 8) land 0xFF in
    output_byte stdout b1;
    output_byte stdout b0;
  in
  Array.iter (fun i -> output_b16 (i + 1)) indexes;
  Hashtbl.iter (fun st _ ->
      let c = IndexSet.cardinal st in
      output_b16 c;
      let last = ref 0 in
      IndexSet.iter (fun i ->
          let i = Index.to_int i in
          output_b16 (i - !last);
          last := i
        ) st;
    ) index;
  Stopwatch.step time "Enumerated paths"
