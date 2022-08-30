open Fix.Indexing
open Utils
open Misc
open Mid

module Make(Regexp : Sigs.REGEXP)() =
struct
  open Regexp
  open Info

  module Redgraph = Redgraph.Make(Info)()
  module Reduction = Reduction.Make(Redgraph)

  module CachedKRESet = Reduction.Cache(struct
      type t = KRESet.t
      let compare = KRESet.compare
      let derive = KRESet.derive_reduce
      let merge ts = List.fold_left KRESet.union KRESet.empty ts
      let cmon = KRESet.cmon
    end)

  module Red = Reduction.Make(CachedKRESet)

  module RedSet = Set.Make(Red)

  module State = struct
    type t = {
      direct: KRESet.t;
      reduce: RedSet.t;
    }

    let make ?(reduce=RedSet.empty) direct = { direct; reduce }

    let cmon t =
      let rs = "{" ^ string_of_int (RedSet.cardinal t.reduce) ^ " reductions}" in
      Cmon.record ["direct", KRESet.cmon t.direct; "reduce", Cmon.constant rs]

    let compare t1 t2 =
      let c = KRESet.compare t1.direct t2.direct in
      if c <> 0 then c else
        RedSet.compare t1.reduce t2.reduce

    let lift_direct (sg, vars, k) =
      (sg, (vars, {direct = KRESet.singleton k; reduce = RedSet.empty}))

    let lift_cached (sg, k) =
      (sg, ([], {direct = CachedKRESet.unlift k; reduce = RedSet.empty}))

    let lift_reduce (sg, k) =
      (sg, ([], {direct = KRESet.empty; reduce = RedSet.singleton k}))

    let lift_red (d, r) =
      List.map lift_cached d @ List.map lift_reduce r

    let add_redset red tr = lift_red (Red.derive red) @ tr

    let empty = {
      direct = KRESet.empty;
      reduce = RedSet.empty;
    }

    let union t1 t2 = {
      direct = KRESet.union t1.direct t2.direct;
      reduce = RedSet.union t1.reduce t2.reduce;
    }

    let compare_transition (_v1, k1) (_v2, k2) =
      compare k1 k2

    let merge_transition xs =
      let merge_one (vars, ks) (var, k) = (var @ vars, union ks k) in
      List.fold_left merge_one ([], empty) xs

    let derive ~reduction_cache t =
      let visited = ref KRESet.empty in
      let reached = ref [] in
      let direct = ref [] in
      let reduce = ref [] in
      let loop k = KRESet.prederive ~visited ~reached ~reduce ~direct k in
      KRESet.iter loop t.direct;
      let tr =
        let reduce = CachedKRESet.lift (KRESet.of_list !reduce) in
        let compiled = Red.compile reduction_cache reduce in
        (*Printf.eprintf "compiled reductions:\n%a\n"
          print_cmon (Red.cmon compiled);*)
        lift_red (Red.initial compiled)
      in
      let tr = RedSet.fold add_redset t.reduce tr in
      let tr =
        dfa_normalize_and_merge
          ~compare:compare_transition
          ~merge:merge_transition
          (List.map lift_direct !direct @ tr)
      in
      (IntSet.of_list !reached, tr)

    let interpret st stack =
      let reduction_cache = Red.make_compilation_cache () in
      let rec loop st stack =
        Printf.eprintf "------------------------\n";
        Printf.eprintf "Matcher state:\n%a\n" print_cmon (cmon st);
        let accepted, transitions = derive ~reduction_cache st in
        Printf.eprintf "Matching actions: [%s]\n"
          (string_concat_map ";" string_of_int (IntSet.elements accepted));
        match stack with
        | [] -> Printf.eprintf "End of stack\n"
        | x :: stack' ->
          let lr1 = Index.of_int Lr1.n x in
          Printf.eprintf "Parser in state %s\n" (Lr1.to_string lr1);
          let match_transition (sg, st') =
            if IndexSet.mem lr1 sg
            then Some st'
            else None
          in
          let targets = List.filter_map match_transition transitions in
          let count = List.length targets in
          Printf.eprintf "Transition: %d target%s\n" count
            (match count with
             | 0 -> " (ending analysis)"
             | 1 -> " (deterministic)"
             | _ -> "s (non-deterministic)");
          if count > 0 then
            loop (List.fold_left union empty (List.map snd targets)) stack'
      in
      loop st stack
  end

  module StateMap = Map.Make(State)

  module Repr = struct
    type t = {
      st: State.t;
      id: int;
      accepted: IntSet.t;
      transitions: (Lr1.set * RE.var list * t lazy_t) list;
      mutable visited: Lr1.set;
      mutable scheduled: Lr1.set;
    }

    let gen expr =
      let next_id =
        let k = ref 0 in
        fun () ->
          let id = !k in
          incr k;
          id
      in
      let reduction_cache = Red.make_compilation_cache () in
      let dfa : t StateMap.t ref = ref StateMap.empty in
      let rec find_state st =
        match StateMap.find_opt st !dfa with
        | Some state -> state
        | None ->
          let accepted, transitions = State.derive ~reduction_cache st in
          let state = {
            st; id = next_id ();
            visited = IndexSet.empty;
            scheduled = IndexSet.empty;
            accepted;
            transitions = List.map make_transition transitions;
          } in
          dfa := StateMap.add st state !dfa;
          state
      and make_transition (sg, (vars, k)) =
        (sg, vars, lazy (find_state k))
      in
      let todo = ref [] in
      let schedule st sg =
        if not (IndexSet.is_empty sg) then (
          let lazy st = st in
          let unvisited = IndexSet.diff sg st.visited in
          if not (IndexSet.is_empty unvisited) then (
            if IndexSet.is_empty st.scheduled then push todo st;
            st.scheduled <- IndexSet.union st.scheduled unvisited;
          )
        )
      in
      let process st =
        let sg = st.scheduled in
        st.visited <- IndexSet.union sg st.visited;
        st.scheduled <- IndexSet.empty;
        List.iter
          (fun (sg', _vars, st') ->
             schedule st' (Lr1.set_predecessors (IndexSet.inter sg' sg)))
          st.transitions
      in
      let rec loop () =
        match List.rev !todo with
        | [] -> ()
        | todo' ->
          todo := [];
          List.iter process todo';
          loop ()
      in
      let initial = find_state expr in
      schedule (lazy initial) Lr1.all;
      loop ();
      (!dfa, initial)
  end

  type t = Repr.t StateMap.t

  let iter_transitions r f =
    let visit_transition (lr1s, vars, st') =
      if Lazy.is_val st' then
        let lazy st' = st' in
        let cases = IndexSet.inter r.Repr.visited lr1s in
        if not (IndexSet.is_empty cases) then
          f cases vars st'
    in
    List.iter visit_transition r.transitions

  let rec eval dfa st stack =
    let id, actions, tr = StateMap.find st dfa in
    Printf.eprintf "------------------------\n";
    Printf.eprintf "Matcher in state %d:\n%a\n" id print_cmon (State.cmon st);
    Printf.eprintf "Matching actions: [%s]\n"
      (string_concat_map ";" string_of_int (IntSet.elements actions));
    match stack with
    | [] -> Printf.eprintf "End of stack\n"
    | x :: xs ->
      let lr1 = Index.of_int Lr1.n x in
      Printf.eprintf "Parser in state %s\n" (Lr1.to_string lr1);
      begin match List.find_opt (fun (lr1s, _) -> IndexSet.mem lr1 lr1s) tr with
        | None ->
          Printf.eprintf "No transitions, ending analysis\n"
        | Some (_, st') ->
          eval dfa st' xs
      end

  let gen_table dfa =
    let states = Array.make (StateMap.cardinal dfa) (None, IntSet.empty, []) in
    StateMap.iter (fun _ (r : Repr.t) ->
        let accept = IntSet.minimum r.accepted in
        let transitions = ref [] in
        let halting = ref (r.visited :> IntSet.t) in
        iter_transitions r
          (fun is vars target ->
             let is = (is : _ IndexSet.t :> IntSet.t) in
             halting := IntSet.diff !halting is;
             push transitions (is, (vars, target.id));
          );
        states.(r.id) <- (accept, !halting, !transitions)
      ) dfa;
    Lrgrep_support.compact states
end
