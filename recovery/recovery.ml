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

module Attr = Recover_attrib.Make(Info)
module Synth = Synthesis.Make(Info)(Attr)

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

let productive_items =
  let rec loop = function
    | (it, _) :: rest when Item.position it = 0 -> loop rest
    | otherwise -> otherwise
  in
  let add_cost st (it, la) =
    let prod, pos = Item.prj it in
    (Synth.cost_of (Tail (st, prod, pos)), it, la)
  in
  let with_cost st =
    let items = effective_items st in
    let items =
      if IndexSet.is_empty (Lr1.predecessors st)
      then items
      else loop items
    in
    List.map (add_cost st) items
  in
  tabulate_finset Lr1.n with_cost

let synthesizable_items st =
  List.filter_map (fun (cost, it, la) ->
      if cost < infinity then
        Some (it, la)
      else
        None
    ) (productive_items st)

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
        epsilon = IndexSet.empty;
        transitions = [];
      } in
      Hashtbl.add table config st;
      begin match
          explore
            (IndexSet.singleton st.config.base)
            config.lookaheads 1
            (synthesizable_items st.config.goto)
        with
        | [] -> ()
        | eps :: rest ->
          st.epsilon <- eps;
          st.transitions <- rest
      end;
      st.index

  let initial =
    let acc = ref [] in
    Index.iter Lr1.n begin fun lr1 ->
      match Lr1.incoming lr1 with
      | Some s when Symbol.is_nonterminal s -> ()
      | _ ->
        let states = IndexSet.singleton lr1 in
        let items = synthesizable_items lr1 in
        push acc (lr1, explore states Terminal.all 0 items)
    end;
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
    mutable transitions: transitions
  }
  and transitions =
    | Wildcard of state
    | Labelled of (Lr1.t * state) list

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
      let ds = {index = fresh(); reached; transitions = Labelled []} in
      Hashtbl.add table key ds;
      begin match Tr_merge.pop trs with
        | None -> ()
        | Some (states, next) ->
          if IndexSet.is_empty states then
            match ds.transitions with
            | Labelled [] ->
              ds.transitions <- Wildcard (follow_transitions (IndexSet.empty, next))
            | _ -> assert false
          else
            IndexMap.iter (fun lr1 states ->
                push todo (ds, lr1, (accumulate_trs states next))
              ) (group states)
      end;
      ds

  let initial = {
    index = fresh ();
    reached = IndexSet.empty;
    transitions = Labelled (
        List.map (fun (lr1, tr) ->
            let tr = Tr_merge.add tr Tr_merge.empty in
            (lr1, follow_transitions (IndexSet.empty, tr))
          ) NFA.initial
      );
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
        begin match ds.transitions with
        | Wildcard _ -> assert false
        | Labelled trs ->
          ds.transitions <- Labelled ((lr1, follow_transitions key) :: trs);
          loop xs
        end
    in
    loop []

  let states = Vector.make n initial

  let () =
    Hashtbl.iter (fun _ ds -> Vector.set states ds.index ds) table

  type stuck_item = {
    item: Item.t;
    symbols: Symbol.set;
  }

  let dead_ends = Hashtbl.create 7

  let print_stuck_item sitem =
    let prod, pos = Item.prj sitem.item in
    let lhs = Production.lhs prod in
    let rhs = Production.rhs prod in
    prerr_string "- ";
    prerr_string (Nonterminal.to_string lhs);
    prerr_char ':';
    for i = 0 to pos - 1 do
      prerr_char ' ';
      prerr_string (Symbol.name rhs.(i));
    done;
    prerr_string " .";
    for i = pos to Array.length rhs - 1 do
      prerr_char ' ';
      prerr_string (Symbol.name rhs.(i));
    done;
    prerr_string "\n  stuck because ";
    let first = ref true in
    IndexSet.iter (fun sym ->
        if !first then first := false else prerr_string ", ";
        prerr_string (Symbol.name sym);
      ) sitem.symbols;
    if not !first then
      prerr_string " cannot be synthesized";
    if Attr.cost_of_prod prod = infinity then (
      if not !first then prerr_string ", and ";
      prerr_string "production has an infinite cost"
    );
    prerr_char '\n'

  let print_dead_ends () =
    Hashtbl.iter (fun stucks stacks ->
        prerr_endline "Cannot recover from stacks ending in:";
        List.iter (fun stack ->
            Printf.eprintf "- %s\n"
              (string_concat_map " " (function
                   | None -> "_"
                   | Some lr1 -> Lr1.to_string lr1) stack);
          ) !stacks;
        prerr_endline "At least one of these items has to be completed:";
        List.iter print_stuck_item stucks;
        prerr_newline ();
      ) dead_ends

  (* Look for dead-ends *)
  let () =
    let visited = Boolvector.make n false in
    let todo = ref [(initial, [])] in
    let schedule st path =
      if not (Boolvector.test visited st.index) then (
        Boolvector.set visited st.index;
        push todo (st, path)
      )
    in
    let dead_end st path items =
      let reached_lhs =
        IndexSet.filter_map (fun nfa ->
            Lr1.incoming (Vector.get NFA.states nfa).config.goto)
          st.reached
      in
      let reasons = ref [] in
      List.iter (fun (lr1, item, _) ->
          let prod, pos = Item.prj item in
          let lhs = Production.lhs prod in
          let rhs = Production.rhs prod in
          let stuck_symbols = ref IndexSet.empty in
          if true || pos > 1 || not (IndexSet.mem (Symbol.inj_r lhs) reached_lhs) then (
            let lr1 = ref lr1 in
            for i = pos to Array.length rhs - 1 do
              let sym = rhs.(i) in
              let tr = List.find
                  (fun tr -> Transition.symbol tr = sym)
                  (Transition.successors !lr1)
              in
              if Attr.penalty_of_item (prod, i) = infinity ||
                 match Transition.split tr with
                 | L gt -> Synth.cost_of (Goto gt) = infinity
                 | R _ -> Attr.cost_of_symbol sym = infinity
              then
                stuck_symbols := IndexSet.add sym !stuck_symbols;
              lr1 := Transition.target tr;
            done;
            push reasons {item; symbols = !stuck_symbols}
          )
        ) items;
      let paths = match Hashtbl.find_opt dead_ends !reasons with
        | Some r -> r
        | None ->
          let r = ref [] in
          Hashtbl.add dead_ends !reasons r;
          r
      in
      push paths (List.map fst path)
    in
    let visit (st, path) =
        match st.transitions with
        | Wildcard st' -> schedule st' ((None, st) :: path)
        | Labelled [] ->
          let prepare_items lr1 items =
            List.map (fun (_cost, it, la) -> (lr1, it, la)) items
          in
          if IndexSet.is_empty st.reached then (
            match path with
            | [Some lr1, _initial] ->
              (* Initial state without transition *)
              dead_end st path (prepare_items lr1 (productive_items lr1))
            | _ ->
              (* Analysis really succeeded *)
              ()
          ) else (
            (* Inner state: some NFA states were reached, but none of them had
             * a transition with finite synthesis cost *)
            match
              List.concat_map
                (fun nfa ->
                   let lr1 = (Vector.get NFA.states nfa).config.goto in
                   let items = productive_items lr1 in
                   if List.exists (fun (cost, _, _) -> cost < infinity) items then
                     raise Exit;
                   prepare_items lr1 items
                ) (IndexSet.elements st.reached)
            with
            | [] -> ()
            | exception Exit -> ()
            | items -> dead_end st path items
          )
        | Labelled trs ->
          List.iter
            (fun (lr1, st') -> schedule st' ((Some lr1, st) :: path))
            trs
    in
    let rec loop () = match !todo with
      | [] -> ()
      | todo' ->
        todo := [];
        List.iter visit todo';
        loop ()
    in
    loop ()

  let () =
    let labelled = ref 0 in
    let wildcard = ref 0 in
    let none = ref 0 in
    Vector.iter (fun ds ->
        begin match ds.transitions with
          | Wildcard _ -> incr wildcard
          | Labelled [] -> incr none
          | Labelled l -> labelled := !labelled + List.length l
        end;
      ) states;
    Printf.eprintf "Deterministic full reduction graph:\n\
                    - %d states\n\
                    - %d labelled transitions\n\
                    - %d states with a wildcard transition\n\
                    - %d states without transitions\n"
      (cardinal n) !labelled !wildcard !none

  let () = print_dead_ends ()

  module SymbolsSetMap = Map.Make(Synth.SymbolsSet)

  let () =
    let dead_symbols =
      Hashtbl.fold
        (fun sitems _ set ->
          List.fold_left (fun set sitem -> IndexSet.union sitem.symbols set) set sitems)
        dead_ends IndexSet.empty
    in
    let minimize tss =
      Synth.SymbolsSet.filter (fun is ->
          not (Synth.SymbolsSet.exists (fun is' -> IndexSet.subset is' is && not (IndexSet.equal is is')) tss)
        ) tss
    in
    let map = ref SymbolsSetMap.empty in
    Index.iter Transition.goto begin fun gt ->
      let nt = Transition.goto_symbol gt in
      if IndexSet.mem (Symbol.inj_r nt) dead_symbols then (
        let tss = Synth.minimal_placeholders (Goto gt) in
        if not (Synth.SymbolsSet.exists IndexSet.is_empty tss) then (
          map := SymbolsSetMap.update (minimize tss) (function
              | None -> Some (IndexSet.singleton nt)
              | Some nts -> Some (IndexSet.add nt nts)
            ) !map
        )
      )
    end;
    let dead_terminals =
      IndexSet.filter
        (fun sym -> Symbol.is_terminal sym && Attr.cost_of_symbol sym = infinity)
        dead_symbols
    in
    if not (IndexSet.is_empty dead_terminals) then
      Printf.eprintf "To complete more situations, \
                      the following terminals could be given placeholding values:\n%s\n\n"
        (string_concat_map " " Symbol.name (IndexSet.elements dead_terminals));
    SymbolsSetMap.iter (fun tss nts ->
        Printf.eprintf "Non-terminals %s\n\
                        could be completed if at least one of these set of symbols had placeholder values:\n"
          (string_concat_map ", " Nonterminal.to_string (IndexSet.elements nts));
        Synth.SymbolsSet.iter (fun ts ->
            Printf.eprintf "- %s\n"
              (string_concat_map ", " Symbol.name (IndexSet.elements ts))
          ) tss
      ) !map
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
        let add_tr ds label ds' =
          ignore (IndexBuffer.Gen.add table
                    (ds.DFA.index, label, ds'.DFA.index) : _ index)
        in
        Vector.iter (fun ds ->
            match ds.DFA.transitions with
            | Wildcard ds' -> add_tr ds None ds'
            | Labelled trs ->
              List.iter (fun (lr1, ds') -> add_tr ds (Some lr1) ds') trs
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
      let trs = List.map (List.sort compare) trs in
      let trs = List.sort_uniq (List.compare (compare_pair Int.compare Index.compare)) trs in
      let trs = List.sort List.compare_lengths trs in
      List.iter (fun x -> ignore (Packer.add_vector packer x)) (List.rev trs)

    let table = Packer.pack packer Index.to_int

    let () =
      Printf.eprintf "Transitions packed to a %d bytes table\n%!"
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
