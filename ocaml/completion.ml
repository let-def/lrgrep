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

let shift_transitions = Vector.make Lr1.n IndexSet.empty

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



(*let reduce_to =
  Vector.init Lr1.n (fun lr1 ->
      let reductions =
        IndexSet.fold (fun red acc ->
            let prod = Reduction.production red in
            let depth = Production.length prod in
            (depth, red) :: acc
          ) (Reduction.from_lr1 lr1) []
        |> List.sort (fun (d1, _) (d2, _) -> Int.compare d1 d2)
      in
      let states_at =
        let states = ref (IndexSet.singleton lr1) in
        let rdepth = ref 0 in
        fun depth ->
          states := pred_n !states (depth - !rdepth);
          rdepth := depth;
          !states
      in
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

let shift_to = Vector.make Lr1.n IndexSet.empty

let () = Index.iter Transition.shift (fun tr ->
    let tr = Transition.of_shift tr in
    vector_set_add shift_to
      (Transition.source tr)
      (Transition.target tr)
  )

let group_by_terminal set =
  IndexSet.fold (fun lr1 acc ->
      match Lr1.incoming lr1 with
      | None -> acc
      | Some sym ->
        match Symbol.desc sym with
        | N _ -> acc
        | T t ->
          IndexMap.update t (function
              | None -> Some (IndexSet.singleton lr1)
              | Some targets -> Some (IndexSet.add lr1 targets)
            ) acc
    ) set IndexMap.empty

let shift_closure =
  Vector.init Lr1.n (fun lr1 ->
      IndexMap.fold (fun lr1' lookaheads targets ->
          let targets' = Vector.get shift_to lr1' in
          IndexSet.fold (fun target targets ->
              match Lr1.incoming target with
              | None -> targets
              | Some sym ->
                match Symbol.desc sym with
                | T t when IndexSet.mem t lookaheads ->
                  IndexSet.add target targets
                | T _ | N _ -> targets
            ) targets' targets
        )
        (Vector.get reduce_to lr1)
        (Vector.get shift_to lr1)
      |> group_by_terminal
    )

let get targets =
  IndexSet.fold (fun target acc ->
    IndexMap.union
    (fun _ a b -> Some (IndexSet.union a b))
    (Vector.get shift_closure target) acc
  ) targets IndexMap.empty


let () =
  let total = ref 0 in
  let rec check states n =
    if IndexSet.is_empty states then  ()
    else if n = 0 then
      incr total
    else
      IndexMap.iter
        (fun _ states' -> check states' (n - 1))
        (get states)
  in
  let maximum = cardinal Lr1.n in
  let terminals = ref 1 in
  for depth = 0 to 3 do
    Printf.eprintf "Depth %d\n" depth;
    total := 0;
    Index.iter Lr1.n (fun lr1 -> check (IndexSet.singleton lr1) depth);
    let maximum = maximum * !terminals in
    Printf.eprintf "Efficiency: %d/%d = %.02f%%\n"
      !total maximum (float !total /. float maximum *. 100.0);
    total := 0;
    check Lr1.all depth;
    Printf.eprintf "Valid sequences: %d/%d = %.02f%%\n"
      !total !terminals (float !total /. float !terminals *. 100.0);
    terminals := !terminals * (cardinal Terminal.n);
  done

let () = flush_all ()

let () =
  let population = Hashtbl.create 7 in
  let populate path =
    match Hashtbl.find_opt population path with
    | Some r -> r
    | None ->
      let r = ref IndexSet.empty in
      Hashtbl.add population path r;
      r
  in
  let rec check initial path states n =
    if IndexSet.is_empty states then  ()
    else if n = 0 then
      let r = populate path in
      r := IndexSet.add initial !r
    else
      IndexMap.iter
        (fun t states' -> check initial (t :: path) states' (n - 1))
        (get states)
  in
  Index.iter Lr1.n (fun lr1 -> check lr1 [] (IndexSet.singleton lr1) 3);
  let sets = Hashtbl.fold (fun _ v acc -> !v :: acc) population [] in
  let sets = List.sort_uniq IndexSet.compare sets in
  Printf.eprintf
    "At depth 3: %d sets, %d unique\n" (Hashtbl.length population) (List.length sets)
*)
