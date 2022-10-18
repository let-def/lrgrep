open Utils
open Misc
open Fix.Indexing

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

  include (struct
    type thread = RE.var
    let thread = RE.var
  end : sig
    type thread
    val thread : int -> thread index
  end)

  module Kern = struct
    type t = {
      direct: KRE.t array;
      reduce: Red.t array;
    }

    type direct_map = (thread, RE.var indexset) indexmap array
    type reduce_map = thread indexset array

    type transition = (direct_map * reduce_map * t)

    let make direct =
      {direct = Array.of_list (KRESet.elements direct); reduce = [||]}

    let empty = {direct = [||]; reduce = [||]}

    let cmon t =
      let rs = Printf.sprintf "{%d reductions}" (Array.length t.reduce) in
      Cmon.record [
        "direct", Cmon.array_map KRE.cmon t.direct;
        "reduce", Cmon.constant rs;
      ]

    let compare t1 t2 =
      let c = array_compare KRE.compare t1.direct t2.direct in
      if c <> 0 then c else
        array_compare Red.compare t1.reduce t2.reduce

    let compare_transition (d1, r1, t1 : transition) (d2, r2, t2 : transition) =
      let c = compare t1 t2 in
      if c <> 0 then c else
        let c = array_compare (IndexMap.compare IndexSet.compare) d1 d2 in
        if c <> 0 then c else
          array_compare IndexSet.compare r1 r2

    let make_from_elts elts =
      let direct, reduce = List.partition_map (fun x -> x) elts in
      let direct =
        let combine_vars vars (_, vars') =
          let vars_union _ vs1 vs2 = Some (IndexSet.union vs1 vs2) in
          IndexMap.union vars_union vars vars'
        in
        sort_and_merge
          (compare_fst KRE.compare)
          (fun (k, vars) rest -> (k, List.fold_left combine_vars vars rest))
          direct
      in
      let reduce =
        let combine_ix ix (_, ix') = IndexSet.union ix ix' in
        sort_and_merge
          (compare_fst Red.compare)
          (fun (r, ix) rest -> (r, List.fold_left combine_ix ix rest))
          reduce
      in
      let direct, direct_tr = array_split (Array.of_list direct) in
      let reduce, reduce_tr = array_split (Array.of_list reduce) in
      (direct_tr, reduce_tr, {direct; reduce})

    let derive ~reduction_cache t =
      let output = ref [] in
      let push_direct sg ix k = push output (sg, Either.left (k, ix)) in
      let make_varmap (ix : thread indexset) vars =
        IndexSet.fold (fun i map -> IndexMap.add i vars map) ix IndexMap.empty
      in
      let output_reductions ix (ds, rs) =
        let ix' = make_varmap ix IndexSet.empty in
        List.iter (fun (sg, k) ->
          KRESet.iter (push_direct sg ix') (CachedKRESet.unlift k)
          ) ds;
        List.iter (fun (sg, r) ->
            push output (sg, Either.right (r, ix))
          ) rs;
      in
      let accept, direct, reduce =
        let accept = ref [] and direct = ref [] and reduce = ref [] in
        let loop i k =
          KRESet.derive_kre k (thread i)
            ~visited:(ref KRESet.empty) ~accept ~reduce ~direct
        in
        Array.iteri loop t.direct;
        (!accept, !direct, !reduce)
      in
      List.iter (fun (kre, ix) ->
          CachedKRESet.lift (KRESet.singleton kre)
          |> Red.compile reduction_cache
          |> Red.initial
          |> output_reductions ix
        ) (sort_and_merge
             (compare_fst KRE.compare)
             (fun (kre, i) is -> (kre, IndexSet.of_list (i :: List.map snd is)) )
             reduce
          );
      Array.iteri (fun i r ->
          output_reductions
            (IndexSet.singleton (thread (Array.length t.direct + i)))
            (Red.derive r)
        ) t.reduce;
      List.iter (fun (sg, vars, k, i) ->
          push_direct sg (IndexMap.singleton i vars) k
        ) direct;
      let tr =
        IndexRefine.annotated_partition !output
        |> List.map (fun (sg, elts) -> (sg, make_from_elts elts))
        |> Misc.sort_and_merge
          (compare_snd compare_transition)
          (fun (sg, e) rest ->
             let union_sg sg (sg', _) = IndexSet.union sg sg' in
             (List.fold_left union_sg sg rest, e))
      in
      (accept, tr)

    let _interpret st ~stack =
      let reduction_cache = Red.make_compilation_cache () in
      let rec loop st stack =
        Printf.eprintf "------------------------\n";
        Printf.eprintf "Matcher state:\n%a\n" print_cmon (cmon st);
        let accepted, transitions = derive ~reduction_cache st in
        Printf.eprintf "Matching actions: [%s]\n"
          (string_concat_map ";" (fun (x, _) -> string_of_index x) accepted);
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
             | _ -> "s (non-deterministic), error!");
          match targets with
          | [] -> loop empty stack'
          | [_, _, st'] -> loop st' stack'
          | _ -> assert false
      in
      loop st stack
  end

  module KernMap = Map.Make(Kern)

  module Pre = struct

    type t = {
      expr: Kern.t;
      id: int;
      accepted: (KRE.clause index * thread index) list;
      mutable transitions: transition list;
      mutable visited: Lr1.set;
      mutable scheduled: Lr1.set;
    }

    and transition = Lr1.set * Kern.direct_map * Kern.reduce_map * t lazy_t

    let derive_dfa expr =
      let next_id =
        let k = ref 0 in
        fun () ->
          let id = !k in
          incr k;
          id
      in
      let reduction_cache = Red.make_compilation_cache () in
      let dfa : t KernMap.t ref = ref KernMap.empty in
      let rec find_state st =
        match KernMap.find_opt st !dfa with
        | Some state -> state
        | None ->
          let accepted, transitions = Kern.derive ~reduction_cache st in
          let state = {
            expr; id = next_id ();
            visited = IndexSet.empty;
            scheduled = IndexSet.empty;
            accepted;
            transitions = List.map make_transition transitions;
          } in
          dfa := KernMap.add st state !dfa;
          state
      and make_transition (sg, (direct_tr, reduce_tr, k)) =
        (sg, direct_tr, reduce_tr, lazy (find_state k))
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
        List.iter (fun (sg', _, _, st') ->
            schedule st' (Lr1.set_predecessors (IndexSet.inter sg' sg))
          ) st.transitions
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
      !dfa
  end

  type state_index = int

  type transition = {
    label: Lr1.set;
    source: state_index;
    target: state_index;
    direct_map: Kern.direct_map;
    reduce_map: Kern.reduce_map;
  }

  type state = {
    index: state_index;
    kern: Kern.t;
    visited: Lr1.set;
    accepted: (KRE.clause index * thread index) list;
    forward: transition list;
    mutable backward: transition list;
  }

  type dfa = state array

  let threads st =
    Array.length st.kern.direct + Array.length st.kern.reduce

  let label  tr = tr.label
  let source tr = tr.source
  let target tr = tr.target

  let index    st = st.index
  let forward  st = st.forward
  let backward st = st.backward
  let accepted st = st.accepted

  let reverse_mapping tr ~target_thread =
    let target_thread = Index.to_int target_thread in
    let direct_count = Array.length tr.direct_map in
    if target_thread < direct_count then
      tr.direct_map.(target_thread)
    else
      IndexSet.fold
        (fun thread acc -> IndexMap.add thread IndexSet.empty acc)
        tr.reduce_map.(target_thread - direct_count) IndexMap.empty

  let derive_dfa expr =
    let dfa = Pre.derive_dfa (Kern.make expr) in
    let states =
      Array.make (KernMap.cardinal dfa)
        {index=0; visited=IndexSet.empty; kern=Kern.empty;
         accepted=[]; forward=[]; backward=[]}
    in
    KernMap.iter (fun kern {Pre. id=index; accepted; transitions; visited; _} ->
        let forward = List.filter_map
            (fun (label, direct_map, reduce_map, tgt) ->
               if Lazy.is_val tgt then (
                 let target = (Lazy.force_val tgt).Pre.id in
                 Some {label; source=index; target; direct_map; reduce_map}
               ) else None
            ) transitions
        in
        states.(index) <- {index; kern; forward; visited; accepted; backward=[]}
      ) dfa;
    Array.iter (fun src ->
        List.iter (fun tr ->
            let tgt = states.(tr.target) in
            tgt.backward <- tr :: tgt.backward
          ) src.forward
      ) states;
    states

  let rec eval (dfa : dfa) (st : state_index) ~stack =
    Printf.eprintf "------------------------\n";
    Printf.eprintf "Matcher in state %d:\n%a\n"
      st print_cmon (Kern.cmon dfa.(st).kern);
    Printf.eprintf "Matching actions: [%s]\n"
      (string_concat_map ";" (fun (x, _) -> string_of_index x) dfa.(st).accepted);
    match stack with
    | [] -> Printf.eprintf "End of stack\n"
    | lr1 :: xs ->
      Printf.eprintf "Parser in state %s\n" (Lr1.to_string lr1);
      let filter_tr tr = IndexSet.mem lr1 tr.label in
      begin match List.find_opt filter_tr dfa.(st).forward with
        | Some tr -> eval dfa tr.target ~stack:xs
        | None ->
          Printf.eprintf "No transitions, ending analysis\n"
      end

  let compute_action dfa liveness tr =
    let src_live = liveness.(source tr) in
    let tgt_live = liveness.(target tr) in
    let index liveness (thr, var) =
      let before, elt, _ = IndexMap.split thr liveness in
      let offset =
        IndexMap.fold (fun _ set acc -> acc + IndexSet.cardinal set) before 0
      in
      match elt with
      | None -> assert false
      | Some vars ->
        IndexSet.fold (fun var' acc -> if var <= var' then acc + 1 else acc) vars offset
    in
    let move = ref [] in
    let store = ref [] in
    for i = 0 to threads dfa.(target tr) - 1 do
      let tgt_thr = thread i in
      match IndexMap.find_opt tgt_thr tgt_live with
      | None -> ()
      | Some tgt_vars ->
        IndexMap.iter (fun src_thr store_vars ->
            IndexSet.iter (fun var ->
                if IndexSet.mem var store_vars then
                  push store (index tgt_live (tgt_thr, var))
                else
                  push move (index tgt_live (tgt_thr, var),
                             index src_live (src_thr, var))
              ) tgt_vars;
          ) (reverse_mapping tr ~target_thread:tgt_thr)
    done;
    { Lrgrep_support.
      store = !store;
      move = !move;
      target = target tr;
    }

  type liveness = (thread, RE.var indexset) indexmap array

  let gen_table dfa liveness =
    let states = Array.map (fun (r : state) ->
        let accept = IntSet.of_list (List.map fst r.accepted :> int list) in
        let transitions = ref [] in
        let halting = ref r.visited in
        List.iter (fun tr ->
            halting := IndexSet.diff !halting tr.label;
            let action = compute_action dfa liveness tr in
            push transitions ((tr.label :> IntSet.t), action);
          ) r.forward;
        let halting = (!halting :> IntSet.t) in
        let transitions = !transitions in
        {Lrgrep_support. accept; halting; transitions}
      ) dfa
    in
    Lrgrep_support.compact states
end
