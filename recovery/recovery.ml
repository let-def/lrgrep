open Fix.Indexing
open Utils
open Misc

(*open Synthesis*)

let grammar_filename =
  let filename, oc = Filename.open_temp_file "lrgrep-interpreter" "cmly" in
  output_string oc Interpreter_data.grammar;
  close_out oc;
  filename

module Grammar = MenhirSdk.Cmly_read.Read(struct let filename = grammar_filename end)
let () = Sys.remove grammar_filename

let get_default tbl key default =
  match Hashtbl.find_opt tbl key with
  | None ->
    let r = ref default in
    Hashtbl.add tbl key r;
    r
  | Some r -> r

let get_list tbl key =
  match Hashtbl.find_opt tbl key with
  | None ->
    let r = ref [] in
    Hashtbl.add tbl key r;
    r
  | Some r -> r

let append_list tbl key v =
  match Hashtbl.find_opt tbl key with
  | None ->
    let r = ref [v] in
    Hashtbl.add tbl key r
  | Some r -> push r v

module Info = Mid.Info.Make(Grammar)
open Info

module Item : sig
  type n
  val n : n cardinal
  type t = n index
  val inj : Production.t -> int -> t
  val prj : n index -> Production.t * int
  val production : n index -> Production.t
  val position : n index -> int
  val pred : t -> t option
end = struct
  let count = ref 0
  let first = Vector.init Production.n (fun p ->
      let index = ! count in
      count := !count + Production.length p + 1;
      index
    )
  let count = !count

  include Const(struct let cardinal = count end)
  type t = n index

  let production = Vector.make' n (fun () -> Index.of_int Production.n 0)

  let () =
    Vector.iteri (fun r index ->
        for i = index to index + Production.length r do
          Vector.set production (Index.of_int n i) r
        done
      ) first

  let inj r pos =
    assert (pos >= 0 && pos <= Production.length r);
    Index.of_int n (Vector.get first r + pos)

  let prj t =
    let r = Vector.get production t in
    (r, (t :> int) - Vector.get first r)

  let pred t =
    let r, p = prj t in
    if p > 0
    then Some (inj r (p - 1))
    else None

  let position t =
    let r = Vector.get production t in
    ((t :> int) - Vector.get first r)

  let production = Vector.get production

end

let effective_items : Lr1.t -> (Item.t * Terminal.set) list =
  let table = Vector.make Lr1.n IndexMap.empty in
  let rec populate states items =
    let map = IndexMap.of_seq (List.to_seq items) in
    IndexSet.iter (fun lr1 ->
        let map' = Vector.get table lr1 in
        let union _k v v' = Some (IndexSet.union v v') in
        Vector.set table lr1 (IndexMap.union union map map')
      ) states;
    let previous_item (it, la) = match Item.pred it with
      | None -> None
      | Some it' -> Some (it', la)
    in
    match List.filter_map previous_item items with
    | [] -> ()
    | items' ->
      populate (indexset_bind states Lr1.predecessors) items'
  in
  Index.iter Lr1.n begin fun lr1 ->
    let initial_items =
      IndexSet.fold (fun red acc ->
          let prod = Reduction.production red in
          let lookaheads = Reduction.lookaheads red in
          (Item.inj prod (Production.length prod), lookaheads) :: acc
        ) (Reduction.from_lr1 lr1) []
    in
    populate (IndexSet.singleton lr1) initial_items
  end;
  let bindings_by_length map =
    List.sort (fun (it1, _) (it2, _) ->
        let _, n1 = Item.prj it1 in
        let _, n2 = Item.prj it2 in
        Int.compare n1 n2
      ) (IndexMap.bindings map)
  in
  Vector.get (Vector.map bindings_by_length table)

let productive_items st =
  let rec loop = function
    | (it, _) :: rest when Item.position it = 0 -> loop rest
    | otherwise -> otherwise
  in
  loop (effective_items st)

let time = Stopwatch.create ()

module NFA = struct
  include Gensym()

  type config = {
    goto: Lr1.t;
    base: Lr1.t;
    lookaheads: Terminal.set;
  }

  let ignore_lookaheads = true

  type state = {
    index: n index;
    config: config;
    mutable epsilon: n indexset;
    mutable transitions: n indexset list;
  }

  let table = Hashtbl.create 7

  let rec explore states la n = function
    | [] -> []
    | (it, la') :: xs when Item.position it = n ->
      let la' =
        if ignore_lookaheads then Terminal.all else IndexSet.inter la la'
      in
      if IndexSet.is_empty la' then
        explore states la n xs
      else
        let x', xs' = match explore states la n xs with
          | [] -> (IndexSet.empty, [])
          | x' :: xs' -> (x', xs')
        in
        let p = Item.production it in
        let nt = Production.lhs p in
        IndexSet.fold (fun base acc ->
            let goto = Transition.find_goto_target base nt in
            let config = {base; goto; lookaheads = la'} in
            IndexSet.add (get_state config) acc
          ) states x' :: xs'
    | xs ->
      IndexSet.empty :: explore (indexset_bind states Lr1.predecessors) la (n + 1) xs

  and get_state config =
    match Hashtbl.find_opt table config with
    | Some st -> st.index
    | None ->
      let st = {
        index = fresh (); config;
        epsilon=IndexSet.empty;
        transitions=[]
      } in
      Hashtbl.add table config st;
      begin match
          explore
            (IndexSet.singleton st.config.base)
            config.lookaheads 1
            (productive_items st.config.goto)
        with
        | [] -> ()
        | eps :: rest ->
          st.epsilon <- eps;
          st.transitions <- rest
      end;
      st.index

  let initial =
    let acc = ref [] in
    Index.iter Lr1.n (fun lr1 ->
        match Lr1.incoming lr1 with
        | Some s when Symbol.is_nonterminal s -> ()
        | _ ->
          let states = IndexSet.singleton lr1 in
          let items = productive_items lr1 in
          push acc (lr1, explore states Terminal.all 0 items)
      );
    !acc

  let states = Vector.make' n (fun () ->
      let dummy_lr1 = Index.of_int Lr1.n 0 in {
        index = Index.of_int n 0;
        config = {base = dummy_lr1; goto = dummy_lr1; lookaheads = Terminal.all};
        epsilon = IndexSet.empty;
        transitions = [];
      }
    )

  let () = Hashtbl.iter
      (fun _ st -> Vector.set states st.index st) table

  let () =
    Printf.eprintf "Non-deterministic full reduction graph: %d states\n" (cardinal n)
end

module DFA = struct

  module Tr_merge : sig
    type t
    val empty : t
    val add : NFA.n indexset list -> t -> t
    val pop : t -> (NFA.n indexset * t) option
    val is_empty : t -> bool
  end = struct
    let is_empty = function [] -> true | _ -> false

    type t = NFA.n indexset list

    let empty = []

    let rec add xs ys =
      match xs, ys with
      | v, [] | [], v -> v
      | (x :: xs'), (y :: ys') ->
        IndexSet.union x y :: add xs' ys'

    let pop = function
      | [] -> None
      | x :: xs -> Some (x, xs)
  end

  include Gensym()

  type state = {
    index: n index;
    mutable reached: NFA.n indexset;
    mutable wildcard: state option;
    mutable transitions: (Lr1.t * state) list;
  }

  let accumulate_trs ss trs =
    let rec add_one state (reached, trs) =
      let state = Vector.get NFA.states state in
      let trs = Tr_merge.add state.transitions trs in
      add_many state.epsilon (reached, trs)
    and add_many states (reached, trs) =
      let reached' = IndexSet.union states reached in
      if reached == reached' then
        (reached, trs)
      else
        IndexSet.fold add_one states (reached', trs)
    in
    add_many ss (IndexSet.empty, trs)

  let table = Hashtbl.create 7

  let group ss =
    IndexSet.fold (fun state map ->
        IndexMap.update (Vector.get NFA.states state).config.base (function
            | None -> Some (IndexSet.singleton state)
            | Some states -> Some (IndexSet.add state states)
          ) map
      ) ss IndexMap.empty

  let todo = ref []

  let rec follow_transitions (reached, trs as key) =
    match Hashtbl.find_opt table key with
    | Some ds -> ds
    | None ->
      let ds = {index = fresh(); reached; wildcard = None; transitions = []} in
      Hashtbl.add table key ds;
      begin match Tr_merge.pop trs with
        | None -> ()
        | Some (states, next) when IndexSet.is_empty states ->
          assert (Option.is_none ds.wildcard);
          ds.wildcard <- Some (follow_transitions (IndexSet.empty, next))
        | Some (states, next) ->
          IndexMap.iter (fun lr1 states ->
              push todo (ds, lr1, (accumulate_trs states next))
            ) (group states)
      end;
      ds

  let initial = {
    index = fresh ();
    reached = IndexSet.empty;
    wildcard = None;
    transitions = List.map (fun (lr1, tr) ->
        let tr = Tr_merge.add tr Tr_merge.empty in
        (lr1, follow_transitions (IndexSet.empty, tr))
      ) NFA.initial;
  }

  let () =
    let rec loop = function
      | [] ->
        begin match !todo with
          | [] -> ()
          | todo' ->
            todo := [];
            loop todo'
        end
      | (ds, lr1, key) :: xs ->
        ds.transitions <- (lr1, follow_transitions key) :: ds.transitions;
        loop xs
    in
    loop []

  let states = Vector.make n initial

  let () =
    Hashtbl.iter (fun _ ds -> Vector.set states ds.index ds) table

  let () =
    let direct = ref 0 in
    let wildcard = ref 0 in
    let mixed = ref 0 in
    let none = ref 0 in
    Vector.iter (fun ds ->
        begin match ds.transitions, ds.wildcard with
          | [], None -> incr none
          | [], Some _ -> incr wildcard
          | (_ :: _), Some _ -> incr mixed
          | (_ :: _), None -> ()
        end;
        direct := !direct + List.length ds.transitions
      ) states;
    Printf.eprintf "Deterministic full reduction graph:\n\
                    - %d states\n\
                    - %d labelled transitions\n\
                    - %d states with only a wildcard transition\n\
                    - %d states with both wildcard and labelled transitions\n\
                    - %d states without transitions\n"
      (cardinal n) !direct !wildcard !mixed !none
end

module Output = struct
  module MDFA = struct

    module Label = struct
      type t = Lr1.t option
      let compare t1 t2 =
        match t1, t2 with
        | None, None -> 0
        | Some _, None -> +1
        | None, Some _ -> -1
        | Some l1, Some l2 -> Index.compare l1 l2
    end

    module Transitions = struct
      include IndexBuffer.Gen.Make()

      let table = get_generator ()

      let () =
        Vector.iter (fun ds ->
            begin match ds.DFA.wildcard with
              | None -> ()
              | Some ds' ->
                let _ = IndexBuffer.Gen.add table (ds.index, None, ds'.index) in
                ()
            end;
            List.iter (fun (lr1, ds') ->
                let _ = IndexBuffer.Gen.add table (ds.index, Some lr1, ds'.DFA.index) in
                ()
              ) ds.transitions
          ) DFA.states

      let table = IndexBuffer.Gen.freeze table
    end

    module Input = struct
      type states = DFA.n
      let states = DFA.n

      type transitions = Transitions.n
      let transitions = Transitions.n

      let label tr =
        let _, x, _ = Vector.get Transitions.table tr in
        x

      let source tr =
        let x, _, _ = Vector.get Transitions.table tr in
        x

      let target tr =
        let _, _, x = Vector.get Transitions.table tr in
        x

      let initials f =
        f DFA.initial.index

      let finals f =
        Hashtbl.iter
          (fun _ ds -> if not (IndexSet.is_empty ds.DFA.reached) then f ds.index)
          DFA.table

      let refinements f =
        let by_reached = Hashtbl.create 7 in
        let get_key ds =
          IndexSet.map
            (fun ns -> (Vector.get NFA.states ns).config.goto)
            ds.DFA.reached
        in
        let register ds =
          if not (IndexSet.is_empty ds.DFA.reached) then
            let key = get_key ds in
            match Hashtbl.find_opt by_reached key with
            | Some r -> push r ds.index
            | None ->
              let r = ref [ds.index] in
              Hashtbl.add by_reached key r
        in
        Hashtbl.iter (fun _ ds -> register ds) DFA.table;
        Hashtbl.iter (fun _ r -> f (fun ~add -> List.iter add !r)) by_reached
    end

    include Valmari.Minimize(Label)(Input)

    let () =
      Printf.eprintf "Minimized to: %d states and %d transitions\n"
        (cardinal states)
        (cardinal transitions)
  end

  module Pack = struct
    module Packer = Lrgrep_support.Sparse_packer

    let packer = Packer.make ()

    let outgoing = Vector.make MDFA.states []

    let () =
      Index.iter MDFA.transitions (fun tr -> Vector.set_cons outgoing (MDFA.source tr) tr);
      let unique_dom = Hashtbl.create 7 in
      let unique_map = Hashtbl.create 7 in
      Vector.iter (function
          | [] | [_] -> ()
          | trs ->
            let dom = List.filter_map MDFA.label trs in
            let dom = List.sort compare dom in
            incr (get_default unique_dom dom 0);
            let map = List.filter_map (fun tr -> match MDFA.label tr with None -> None | Some x -> Some (x, MDFA.target tr)) trs in
            let map = List.sort compare map in
            incr (get_default unique_map map 0);
        ) outgoing;
      Printf.eprintf "%d unique domains, %d unique maps\n" (Hashtbl.length unique_dom) (Hashtbl.length unique_map);
      Printf.eprintf "%d unique transition\n" (Hashtbl.fold (fun map _ sum ->
          sum + List.length map
        ) unique_map 0);
      (*let doms = Array.of_seq (Hashtbl.to_seq unique_dom) in
        Array.fast_sort (fun (y,_) (z,_) -> List.compare_lengths y z) doms;
        Array.iter (fun (dom,c) ->
          Printf.eprintf "%d: [%s]\n"
            !c
            (string_concat_map "," string_of_index dom)
        ) doms;*)
      let trs =
        Vector.fold_left (fun acc trs ->
            let by_target = Hashtbl.create 7 in
            List.iter (fun tr -> match MDFA.label tr with
                | None -> ()
                | Some lr1 ->
                  let target = MDFA.target tr in
                  append_list by_target target ((lr1 :> int), target)
              ) trs;
            let longest = ref 0 in
            Hashtbl.iter
              (fun _target labels -> longest := Int.max !longest (List.length !labels))
              by_target;
            let trs =
              Hashtbl.fold (fun _target labels acc ->
                  if List.length !labels = !longest then
                    (longest := -1; acc)
                  else
                    List.rev_append !labels acc
                ) by_target []
            in
            trs :: acc
          ) [] outgoing
      in
      let trs = List.filter_map (function
          (*| [] | [_] -> None*)
          | x -> Some (List.sort compare x)) trs in
      let trs = List.sort_uniq (List.compare (compare_pair Int.compare Index.compare)) trs in
      let trs = List.sort List.compare_lengths trs in
      List.iter (fun x -> ignore (Packer.add_vector packer x)) (List.rev trs)

    let table = Packer.pack packer Index.to_int

    let () =
      Printf.eprintf "Transitions packed to a %d bytes table\n"
        (String.length table)

    let () = output_string stdout table
  end

  let () =
    if false then
      Index.iter Lr1.n (fun lr1 ->
          let lr1s = Info.Lr1.predecessors lr1 in
          if not (IndexSet.is_singleton lr1s || IndexSet.is_empty lr1s) then
            Printf.eprintf "%s: %s\n" (string_of_index lr1) (string_of_indexset lr1s)
        )
end
