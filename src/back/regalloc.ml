open Fix.Indexing
open Utils
open Misc

module Make (Dfa: Sigs.DFA) = struct
  (*module RegMap = Map.Make(struct
      type t = Dfa.thread index * RE.var index
      let compare (t1, v1) (t2, v2) =
        let c = compare_index t1 t2 in
        if c <> 0 then c else
          compare_index v1 v2
    end)*)

  (*type regset = (Dfa.thread, IntSet.t) indexmap*)

  (*type register_bank = {
    mapping: int RegMap.t;
    count: int;
  }

  let empty = {
    mapping = RegMap.empty;
    count = 0;
  }

  let alloc bank key =
    match RegMap.find_opt key bank.mapping with
    | Some _ -> bank
    | None ->
      let mapping = RegMap.add key bank.count bank.mapping in
      {mapping; count = bank.count + 1}*)

  (*let alloc bank key =
    match RegMap.find_opt key bank.mapping with
    | Some var -> bank, var
    | None ->
      let mapping = RegMap.add key bank.count bank.mapping in
      {mapping; count = bank.count + 1}, bank.count*)

  let liveness vars dfa =
    let count = Array.length dfa in
    let registers = Array.make count IndexMap.empty in
    let pending = Array.make count IndexMap.empty in
    let todo = ref [] in
    Array.iter (fun st ->
        match Dfa.accepted st with
        | [] -> ()
        | accepted ->
          push todo (Dfa.index st);
          let set = ref IndexMap.empty in
          List.iter (fun (clause, thread) ->
              let vars = vars.(Index.to_int clause) in
              set := IndexMap.add thread vars !set;
            ) accepted;
          pending.(Dfa.index st) <- !set;
      ) dfa;
    let schedule st vars =
      if not (IndexMap.is_empty vars) then (
        let vars' = pending.(st) in
        if IndexMap.is_empty vars' then (
          push todo st;
          pending.(st) <- vars
        ) else
          pending.(st) <- IndexMap.union
              (fun _ s1 s2 -> Some (IndexSet.union s1 s2)) vars vars';
      )
    in
    let process_pending st =
      let p = pending.(st) in
      let r = registers.(st) in
      let p = IndexMap.filter_map (fun tr vars ->
          match IndexMap.find_opt tr r with
          | None -> Some vars
          | Some vars' ->
            let vars = IndexSet.diff vars vars' in
            if IndexSet.is_empty vars then
              None
            else
              Some vars
        ) p
      in
      registers.(st) <- IndexMap.union (fun _ s1 s2 ->
          Some (IndexSet.union s1 s2)
        ) p r;
      pending.(st) <- IndexMap.empty;
      IndexMap.iter (fun target_thread vars ->
          List.iter (fun tr ->
              let mapping = Dfa.reverse_mapping tr ~target_thread in
              let vars =
                IndexMap.filter_map (fun _ vars' ->
                    let vars = IndexSet.diff vars vars' in
                    if IndexSet.is_empty vars then
                      None
                    else
                      Some vars
                  ) mapping
              in
              schedule (Dfa.source tr) vars
            ) (Dfa.backward dfa.(st))
        ) p
    in
    let rec loop () =
      match !todo with
      | [] -> ()
      | todo' ->
        todo := [];
        List.iter process_pending todo';
        loop ()
    in
    loop ();
    let optional_var =
      IndexMap.fold
        (fun _ vars acc -> acc + IndexSet.cardinal vars)
        registers.(0) 0
    in
    let max_count =
      Array.fold_left
        (fun acc bank -> max acc (IndexMap.fold (fun _ set acc -> acc + IndexSet.cardinal set) bank 0))
        0 registers
    in
    Printf.eprintf "optional variables: %d\n" optional_var;
    Printf.eprintf "max register count: %d\n" max_count;
    max_count, registers

end
