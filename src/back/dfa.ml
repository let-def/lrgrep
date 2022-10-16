open Utils
open Misc

module Make(Regexp : Mid.Sigs.REGEXP)()
  : Sigs.DFA with module Regexp = Regexp
=
struct
  module Regexp = Regexp
  open Regexp
  open Info

  module Redgraph = Mid.Redgraph.Make(Info)()
  module Reduction = Mid.Reduction.Make(Redgraph)

  module CachedKRESet = Reduction.Cache(struct
      type t = KRESet.t
      let compare = KRESet.compare
      let derive = KRESet.derive_in_reduction
      let merge ts = List.fold_left KRESet.union KRESet.empty ts
      let cmon = KRESet.cmon
    end)

  module Red = Reduction.Make(CachedKRESet)

  module RedSet = Set.Make(Red)

  module Expr = struct
    type t = {
      direct: KRESet.t;
      reduce: RedSet.t;
    }

    let make direct = {direct; reduce = RedSet.empty}

    let cmon t =
      let rs = Printf.sprintf "{%d reductions}" (RedSet.cardinal t.reduce) in
      Cmon.record [
        "direct", KRESet.cmon t.direct;
        "reduce", Cmon.constant rs;
      ]

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
      let accept = ref [] in
      let direct = ref [] in
      let reduce = ref [] in
      let loop k = KRESet.derive_kre ~visited ~accept ~reduce ~direct k in
      KRESet.iter loop t.direct;
      let reduce_one kre =
        let compiled =
          Red.compile reduction_cache (CachedKRESet.lift (KRESet.singleton kre))
        in
        (*Printf.eprintf "compiled reductions:\n%a\n"
          print_cmon (Red.cmon compiled);*)
        lift_red (Red.initial compiled)
      in
      let tr =
        List.concat_map reduce_one (List.sort_uniq KRE.compare !reduce)
      in
      let tr = RedSet.fold add_redset t.reduce tr in
      let tr =
        determinize_derivatives
          ~compare:compare_transition
          ~merge:merge_transition
          (List.map lift_direct !direct @ tr)
      in
      (IntSet.of_list !accept, tr)

    let interpret st ~stack =
      let reduction_cache = Red.make_compilation_cache () in
      let rec loop st stack =
        Printf.eprintf "------------------------\n";
        Printf.eprintf "Matcher state:\n%a\n" print_cmon (cmon st);
        let accepted, transitions = derive ~reduction_cache st in
        Printf.eprintf "Matching actions: [%s]\n"
          (string_concat_map ";" string_of_int (IntSet.elements accepted));
        match stack with
        | [] -> Printf.eprintf "End of stack\n"
        | lr1 :: stack' ->
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

  module ExprMap = Map.Make(Expr)

  module State = struct
    type t = {
      expr: Expr.t;
      id: int;
      accepted: IntSet.t;
      transitions: (Lr1.set * RE.var list * t lazy_t) list;
      mutable visited: Lr1.set;
      mutable scheduled: Lr1.set;
    }

    let id t = t.id

    let accepted t = t.accepted

    let derive_dfa expr =
      let next_id =
        let k = ref 0 in
        fun () ->
          let id = !k in
          incr k;
          id
      in
      let reduction_cache = Red.make_compilation_cache () in
      let dfa : t ExprMap.t ref = ref ExprMap.empty in
      let rec find_state st =
        match ExprMap.find_opt st !dfa with
        | Some state -> state
        | None ->
          let accepted, transitions = Expr.derive ~reduction_cache st in
          let state = {
            expr; id = next_id ();
            visited = IndexSet.empty;
            scheduled = IndexSet.empty;
            accepted;
            transitions = List.map make_transition transitions;
          } in
          dfa := ExprMap.add st state !dfa;
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

    let iter_transitions r f =
      let visit_transition (lr1s, vars, st') =
        if Lazy.is_val st' then
          let lazy st' = st' in
          let cases = IndexSet.inter r.visited lr1s in
          if not (IndexSet.is_empty cases) then
            f cases vars st'
      in
      List.iter visit_transition r.transitions

  end

  type t = State.t ExprMap.t

  let derive_dfa = State.derive_dfa

  let number_of_states = ExprMap.cardinal

  let rec eval (dfa : t) (st : State.t) ~stack =
    Printf.eprintf "------------------------\n";
    Printf.eprintf "Matcher in state %d:\n%a\n"
      (State.id st) print_cmon (Expr.cmon st.expr);
    Printf.eprintf "Matching actions: [%s]\n"
      (string_concat_map ";" string_of_int (IntSet.elements st.accepted));
    match stack with
    | [] -> Printf.eprintf "End of stack\n"
    | lr1 :: xs ->
      Printf.eprintf "Parser in state %s\n" (Lr1.to_string lr1);
      let filter_tr (lr1s, _vars, _target) = IndexSet.mem lr1 lr1s in
      begin match List.find_opt filter_tr st.transitions with
        | Some (_, _, st') when Lazy.is_val st' ->
          eval dfa (Lazy.force st') ~stack:xs
        | None | Some _ ->
          Printf.eprintf "No transitions, ending analysis\n"
      end

  let gen_table dfa =
    let dummy =
      {Lrgrep_support. accept=None; halting=IntSet.empty; transitions=[]}
    in
    let states = Array.make (number_of_states dfa) dummy in
    ExprMap.iter (fun _ (r : State.t) ->
        let accept = IntSet.minimum r.accepted in
        let transitions = ref [] in
        let halting = ref (r.visited :> IntSet.t) in
        State.iter_transitions r
          (fun is vars target ->
             let is = (is : _ IndexSet.t :> IntSet.t) in
             halting := IntSet.diff !halting is;
             push transitions (is, (vars, target.id));
          );
        let halting = !halting in
        let transitions = !transitions in
        states.(r.id) <- {Lrgrep_support. accept; halting; transitions}
      ) dfa;
    Lrgrep_support.compact states
end
