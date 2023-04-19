open Utils
open Misc
open Front
open Fix.Indexing
open Regexp

module Entry
    (Transl : Transl.S)
    (E : sig
       val parser_name : string
       val entry : Syntax.entry
     end)() =
struct
  open Transl
  open Regexp
  open Info

  type k = K.t
  type label = K.label

  module Clauses = struct
    include Vector.Of_array(struct
        type a = Syntax.clause
        let array = Array.of_list E.entry.clauses
      end)

    let n = Vector.length vector

    let total =
      IndexSet.init_from_set (Vector.length vector) (fun index ->
          let clause = Vector.get vector index in
          match clause.action with
          | Syntax.Unreachable -> true
          | Syntax.Total _ -> clause.lookaheads = []
          | Syntax.Partial _ -> false
        )
  end

  module NFA = struct
    type t = {
      (* State construction *)
      uid: int;
      k: k;
      transitions: (label * t lazy_t) list;
      accept: bool;
      clause: Clauses.n index;
      mutable mark: unit ref;
    }

    let compare t1 t2 =
      Int.compare t1.uid t2.uid

    let uid =
      let k = ref 0 in
      fun () -> incr k; !k

    let default_mark = ref ()

    let make clause =
      let nfa = ref Regexp.KMap.empty in
      let rec aux k =
        match Regexp.KMap.find_opt k !nfa with
        | Some t -> t
        | None ->
          let accept = ref false in
          let rec process_transitions = function
            | [] -> []
            | (label, target) :: rest ->
              begin match target with
                | None when K.is_immediate_label label ->
                  accept := true;
                  []
                | None ->
                  (label, accepting) :: process_transitions rest
                | Some k' ->
                  (label, lazy (aux k')) :: process_transitions rest
              end
          in
          let transitions = process_transitions (K.derive k) in
          let uid = uid () in
          let accept = !accept in
          let t = {uid; k; transitions; accept; clause; mark=default_mark} in
          nfa := Regexp.KMap.add k t !nfa;
          t
      and accepting = lazy (aux K.Done)
      in
      aux

    let process_clause ~capture index (def : Syntax.clause) =
      let capture_def = ref IndexMap.empty in
      let capture kind name =
        let index = capture () in
        capture_def := IndexMap.add index (kind, name) !capture_def;
        index
      in
      let captures, re =
        Transl.transl ~capture ~for_reduction:false def.pattern
      in
      let initial = make index Regexp.K.(More (re, Done)) in
      (initial, (captures, !capture_def))

    let clauses =
      Vector.mapi (process_clause ~capture:(Capture.gensym ())) Clauses.vector
  end

  module DFA = struct
    open IndexBuffer

    let group_make (type a) (prj : a -> NFA.t) (ts : a list) : a array =
      let mark = ref () in
      let last_accepted = ref `None in
      let ts = List.filter (fun a ->
          let th = prj a in
          (th.mark != mark) && (
            th.mark <- mark;
            match !last_accepted with
            | `Total clause -> assert (clause <= th.clause); false
            | `Some clause
              when assert (clause <= th.clause); th.clause = clause -> false
            | _ ->
              if th.accept then (
                if IndexSet.mem th.clause Clauses.total
                then last_accepted := `Total th.clause
                else last_accepted := `Some th.clause
              );
              true
          )
        ) ts
      in
      Array.of_list ts

    let group_fold f x acc =
      let acc = ref acc in
      Vector.iteri (fun i x -> acc := f i x !acc) x;
      !acc

    let group_index_of (type n) th (t : (n, NFA.t) vector) =
      let exception Found of n index in
      match
        Vector.iteri
          (fun i x -> if NFA.compare x th = 0 then raise (Found i))
          t
      with
      | () -> raise Not_found
      | exception (Found n) -> n

    module State = Gen.Make()
    type n = State.n
    let n = State.n

    type ('src, 'tgt) _mapping = ('tgt, 'src index * Capture.set) vector

    type 'n t = {
      index: n index;
      group: ('n, NFA.t) vector;
      transitions: (Lr1.set * 'n mapping lazy_t) list;
      mutable visited: Lr1.set;
      mutable scheduled: Lr1.set;
    }
    and 'src mapping = Mapping : ('src, 'tgt) _mapping * 'tgt t -> 'src mapping

    type packed = Packed : 'n t -> packed [@@ocaml.unboxed]

    let states = State.get_generator ()

    module GroupMap = Map.Make(struct
        type t = NFA.t array
        let compare g1 g2 = array_compare NFA.compare g1 g2
      end)

    let determinize group =
      let map = ref GroupMap.empty in
      let rec aux : type n . (n, NFA.t) vector -> n t =
        fun group ->
          match GroupMap.find_opt (Vector.as_array group) !map with
          | Some (Packed t') ->
            let Refl = assert_equal_cardinal
                (Vector.length group) (Vector.length t'.group)
            in
            t'
          | None ->
            let rev_transitions =
              let make i ({K. filter; captures}, t) = (filter, (i, captures, t)) in
              group_fold
                (fun i (nfa : NFA.t) acc -> List.rev_map (make i) nfa.transitions @ acc)
                group []
            in
            let rev_partitions = IndexRefine.annotated_partition rev_transitions in
            let process_class (label, rev_targets) =
              label, lazy (
                let prepare_target (index, captures, lazy nfa) =
                  nfa, (index, captures)
                in
                let Packed result =
                  rev_targets
                  |> List.rev_map prepare_target
                  |> group_make fst
                  |> Vector.of_array
                in
                Mapping ((Vector.map snd result), aux (Vector.map fst result))
              )
            in
            let transitions = List.map process_class rev_partitions in
            let reservation = Gen.reserve states in
            let state = {
              index = Gen.index reservation;
              group; transitions;
              scheduled=IndexSet.empty; visited=IndexSet.empty
            } in
            Gen.commit states reservation (Packed state);
            map := GroupMap.add (Vector.as_array group) (Packed state) !map;
            state
      in
      aux group

    let initial =
      let Vector.Packed group = Vector.of_array (
          NFA.clauses
          |> Vector.map fst
          |> Vector.to_list
          |> group_make Fun.id
        )
      in
      (determinize group).index

    let () =
      let todo = ref [] in
      let schedule i set =
        let Packed t as packed = Gen.get states i in
        if not (IndexSet.subset set t.visited) then (
          let set = IndexSet.diff set t.visited in
          if IndexSet.is_empty t.scheduled then (
            push todo packed;
            t.scheduled <- set
          ) else (
            t.scheduled <- IndexSet.union t.scheduled set
          )
        )
      in
      let update (Packed t) =
        let todo = t.scheduled in
        t.visited <- IndexSet.union t.visited todo;
        t.scheduled <- IndexSet.empty;
        List.iter (fun (label, target) ->
            let label' = IndexSet.inter label todo in
            if not (IndexSet.is_empty label') then
              let lazy (Mapping (_, t')) = target in
              schedule t'.index (Lr1.set_predecessors label')
          ) t.transitions
      in
      let rec loop () =
        match !todo with
        | [] -> ()
        | todo' ->
          todo := [];
          List.iter update todo';
          loop ()
      in
      schedule initial Lr1.idle;
      loop ()

    let states = Gen.freeze states

    let iter_transitions t f =
      List.iter (fun (label, target) ->
          if Lazy.is_val target then (
            let lazy mapping = target in
            f label mapping
          )
        ) t.transitions

    let iter_refined_transitions t f =
      let set = t.visited in
      List.iter (fun (label, target) ->
          if Lazy.is_val target then (
            let lazy mapping = target in
            f (IndexSet.inter set label) mapping
          )
        ) t.transitions

    let liveness =
      let reserve (Packed t) =
        Vector.Packed (Vector.make (Vector.length t.group) IndexSet.empty) in
      Vector.map reserve states

    let liveness (type m) (t : m t) : (m, Capture.set) vector =
      let Vector.Packed v = Vector.get liveness t.index in
      let Refl = assert_equal_cardinal
          (Vector.length v) (Vector.length t.group)
      in
      v

    let () =
      let todo = ref [] in
      let process (Packed ti) =
        let live_i = liveness ti in
        let process_transition _label (Mapping (mapping, tj)) =
          let changed = ref false in
          let live_j = liveness tj in
          let process_mapping j (i, captures) =
            let live = IndexSet.union (Vector.get live_i i) captures in
            let live' = Vector.get live_j j in
            if not (IndexSet.equal live live') then (
              Vector.set live_j j live;
              changed := true;
            )
          in
          Vector.iteri process_mapping mapping;
          if !changed then
            push todo (Packed tj)
        in
        iter_transitions ti process_transition
      in
      Vector.iter process states;
      let rec loop () =
        match !todo with
        | [] -> ()
        | todo' ->
          todo := [];
          List.iter process todo';
          loop ()
      in
      loop ()

    let empty_registers = Vector.Packed Vector.empty

    let registers = Vector.make (Vector.length states) empty_registers

    let get_registers (type m) (st : m t) : (m, _) vector =
      let Vector.Packed regs = Vector.get registers st.index in
      let Refl = assert_equal_cardinal (Vector.length regs) (Vector.length st.group) in
      regs

    let () =
      let allocate_successor registers allocated mapping target =
        let live = liveness target in
        let in_use = ref (
            Vector.fold_right
              (fun (ii, _) set -> IntSet.union (Vector.get allocated ii) set)
              mapping IntSet.empty
          )
        in
        (* Allocate fresh registers *)
        Vector.mapi begin fun j (i, _) ->
          let allocated = Vector.get registers i in
          let live = Vector.get live j in
          let to_allocate = IndexSet.diff live (IndexMap.domain allocated) in
          IndexSet.fold (fun cap allocated ->
              let index, in_use' = IntSet.allocate !in_use in
              in_use := in_use';
              IndexMap.add cap index allocated
            ) to_allocate allocated
        end mapping
      in
      let init (Packed src) =
        let regs = get_registers src in
        let allocated = Vector.map (fun x -> IndexMap.fold (fun _ -> IntSet.add) x IntSet.empty) regs in
        iter_transitions src @@ fun _ (Mapping (mapping, target)) ->
        if Vector.get registers target.index == empty_registers then
          let regs' = allocate_successor regs allocated mapping target in
          Vector.set registers target.index (Vector.Packed regs')
      in
      Vector.set registers initial
        (let Packed t = Vector.get states initial in
         Packed (Vector.make (Vector.length t.group) IndexMap.empty));
      Vector.iter init states

    let register_count =
      let max_live = ref 0 in
      let max_index = ref (-1) in
      let check_state (Packed state) =
        let regs = get_registers state in
        let max_live' =
          Vector.fold_left (fun sum map -> sum + IndexMap.cardinal map) 0 regs
        in
        max_live := max !max_live max_live';
        max_index := Vector.fold_right (IndexMap.fold (fun _ -> max)) regs !max_index
      in
      Vector.iter check_state states;
      Printf.eprintf "register allocation:\nmax live registers: %d\nregister count: %d\n" !max_live !max_index;
      !max_index + 1
  end

  let () =
    if false then
      let rstate = ref (Vector.get DFA.states DFA.initial) in
      let rec loop () =
        let DFA.Packed state = !rstate in
        let group = state.group in
        Vector.iter (fun nfa ->
            Format.eprintf "- clause %d\n%!%a\n%!"
              (Index.to_int nfa.NFA.clause)
              Cmon.format (K.cmon nfa.NFA.k)
          ) group;
        Printf.eprintf "state %d\n> %!" (Index.to_int state.index);
        let line = input_line stdin in
        let lr1 = Index.of_int Lr1.n (int_of_string line) in
        Printf.eprintf "input: %s\n" (Lr1.to_string lr1);
        (*let red = Vector.get Regexp.Redgraph.initial lr1 in
          Printf.eprintf "no transition (but %d inner and %d outer reductions)\n"
          (List.length red.inner)
          (List.length (List.filter (fun x -> IndexSet.mem lr1 (List.hd x.Regexp.Redgraph.candidates).filter) red.outer));*)
        let target = ref None in
        let check_transition filter (DFA.Mapping (_, target')) =
          if IndexSet.mem lr1 filter then
            target := Some (DFA.Packed target')
        in
        DFA.iter_refined_transitions state check_transition;
        begin match !target with
          | None -> Printf.eprintf "no transition\n"
          | Some target -> rstate := target
        end;
        loop ()
      in
      loop ()

  module RunDFA = struct
    type states = DFA.n
    let states = DFA.n

    module Label = struct
      type t = {
        filter: Lr1.set;
        captures: (Capture.n, int) indexmap;
        clear: IntSet.t;
        moves: int IntMap.t;
      }

      let compare t1 t2 =
        let c = IndexSet.compare t1.filter t2.filter in
        if c <> 0 then c else
          let c = IndexMap.compare Int.compare t1.captures t2.captures in
          if c <> 0 then c else
            let c = IntMap.compare Int.compare t1.moves t2.moves in
            if c <> 0 then c else
              let c = IntSet.compare t1.clear t1.clear in
              c
    end

    let partial_captures = ref IndexSet.empty

    module Transition = struct
      open IndexBuffer

      type t = {
        source: states index;
        target: states index;
        label: Label.t;
      }
      include Gen.Make()

      let all = get_generator ()

      let () =
        let process_state (DFA.Packed source) =
          let src_regs = DFA.get_registers source in
          DFA.iter_refined_transitions source @@
          fun filter (DFA.Mapping (mapping, target)) ->
          let tgt_regs = DFA.get_registers target in
          let captures = ref IndexMap.empty in
          let moves = ref IntMap.empty in
          let clear = ref IntSet.empty in
          let process_mapping (src_i, captured) tgt_bank =
            let src_bank = Vector.get src_regs src_i in
            IndexMap.iter (fun capture tgt_reg ->
                if IndexSet.mem capture captured then
                  captures := IndexMap.add capture tgt_reg !captures
                else
                  match IndexMap.find_opt capture src_bank with
                  | Some src_reg ->
                    if src_reg <> tgt_reg then
                      moves := IntMap.add src_reg tgt_reg !moves
                  | None ->
                    partial_captures := IndexSet.add capture !partial_captures;
                    clear := IntSet.add tgt_reg !clear
              ) tgt_bank
          in
          Vector.iter2 process_mapping mapping tgt_regs;
          let captures = !captures and moves = !moves and clear = !clear in
          let label = {Label.filter; captures; moves; clear} in
          ignore (Gen.add all {source = source.index; target = target.index; label})
        in
        Vector.iter process_state DFA.states

      let all = Gen.freeze all
    end

    type transitions = Transition.n
    let transitions = Transition.n

    let partial_captures =
      let acc = !partial_captures in
      Vector.fold_left begin fun acc (DFA.Packed st) ->
        Vector.fold_left2 begin fun acc (nfa : NFA.t) regs ->
          if nfa.accept then
            let _, (cap, _) = Vector.get NFA.clauses nfa.clause in
            IndexSet.fold begin fun var acc ->
              if IndexMap.mem var regs
              then acc
              else IndexSet.add var acc
            end cap acc
          else acc
        end acc st.group (DFA.get_registers st)
      end acc DFA.states

    let label i = (Vector.get Transition.all i).label
    let source i = (Vector.get Transition.all i).source
    let target i = (Vector.get Transition.all i).target

    let initials f = f DFA.initial
    let finals f =
      Vector.iter (fun (DFA.Packed st) ->
          let is_final acc nfa = acc || nfa.NFA.accept in
          if Vector.fold_left is_final false st.group then
            f st.index
        ) DFA.states

    let refinements refine =
      (* Refine states by accepted actions *)
      let acc = ref [] in
      Vector.iter (fun (DFA.Packed st) ->
          let add_final acc nfa =
            if nfa.NFA.accept then IndexSet.add nfa.NFA.clause acc else acc
          in
          let accepted = Vector.fold_left add_final IndexSet.empty st.group in
          if not (IndexSet.is_empty accepted) then
            push acc (accepted, st.index)
        ) DFA.states;
      Misc.group_by !acc
        ~compare:(fun (a1, _) (a2, _) -> IndexSet.compare a1 a2)
        ~group:(fun (_, st) sts -> st :: List.map snd sts)
      |> List.iter (fun group -> refine (fun ~add -> List.iter add group))
  end

  module MinDFA = Valmari.Minimize (RunDFA.Label) (RunDFA)

  let captures_lr1 =
    let map = ref IndexMap.empty in
    Index.iter MinDFA.transitions (fun tr ->
        let label = MinDFA.label tr in
        let map' = IndexMap.map (fun _ -> label.filter) label.captures in
        map := IndexMap.union (fun _ a b -> Some (IndexSet.union a b)) !map map'
      );
    !map

  let recover_type index =
    try
      let lr1s = IndexMap.find index captures_lr1 in
      let symbols = IndexSet.map (fun lr1 ->
          match Lr1.incoming lr1 with
          | None -> raise Not_found
          | Some sym -> sym
        ) lr1s
      in
      let typ = IndexSet.fold (fun sym acc ->
          let typ = match Symbol.semantic_value sym with
            | None -> raise Not_found
            | Some typ -> String.trim typ
          in
          match acc with
          | None -> Some typ
          | Some typ' ->
            if typ <> typ' then raise Not_found;
            acc
        ) symbols None
      in
      match typ with
      | None -> None
      | Some typ -> Some (symbols, typ)
    with Not_found -> None

  let print_loc (loc : Syntax.location) =
    Printf.sprintf "# %d %S\n%s"
      loc.start_line loc.loc_file
      (String.make loc.start_col ' ')

  let symbol_matcher s = match Info.Symbol.prj s with
    | L t -> "T T_" ^ Info.Terminal.to_string t
    | R n -> "N N_" ^ Grammar.Nonterminal.mangled_name (Info.Nonterminal.to_g n)

  let bytes_match b i str =
    Bytes.length b >= i + String.length str &&
    let exception Exit in
    match
      for j = 0 to String.length str - 1 do
        if Bytes.get b (i + j) <> String.get str j then
          raise Exit
      done
    with
    | () -> true
    | exception Exit -> false

  let rewrite_loc_keywords str =
    let b = Bytes.of_string str in
    let l = Bytes.length b in
    let i = ref 0 in
    while !i < l do
      if Bytes.get b !i = '$' &&
         (bytes_match b (!i + 1) "startloc(" ||
          bytes_match b (!i + 1) "endloc(")
      then (
        Bytes.set b !i '_';
        while Bytes.get b !i <> '(' do incr i; done;
        Bytes.set b !i '_';
        while !i < l  && Bytes.get b !i <> ')' do incr i; done;
        if !i < l then Bytes.set b !i '_'
      )
      else incr i
    done;
    Bytes.to_string b

  let bind_capture ~print ~roffset index (def, name) =
    let print fmt = Printf.ksprintf print fmt in
    let is_optional = IndexSet.mem index RunDFA.partial_captures in
    let none = if is_optional then "None" else "assert false" in
    let some x = if is_optional then "Some (" ^ x ^ ")" else x in
    let offset = !roffset in
    incr roffset;
    match def with
    | Value ->
      let typ = recover_type index in
      print
        "    let %s, _startloc_%s_, _endloc_%s_ = match __registers.(%d) with \n\
        \      | None -> %s\n\
        \      | Some (%s.MenhirInterpreter.Element (%s, %s, startp, endp)%s) ->"
        name name name offset
        (if is_optional then "(None, None, None)" else "assert false")
        E.parser_name
        (if Option.is_none typ then "_" else "st")
        (if Option.is_none typ then "_" else "x")
        (if Option.is_none typ then "as x" else "");
      begin match typ with
        | None -> ()
        | Some (symbols, typ) ->
          let matchers =
            List.map symbol_matcher (IndexSet.elements symbols)
          in
          print "      let x = match %s.MenhirInterpreter.incoming_symbol st with\n\
                \        | %s -> (x : %s) \n\
                \        | _ -> assert false\n\
                \      in\n"
            E.parser_name (String.concat " | " matchers) typ
      end;
      print "      (%s, %s, %s)\n" (some "x") (some "startp") (some "endp");
      print "    in\n";
      print "    let _ = %s in\n" name
    | Start_loc ->
      print
        "    let _startloc_%s_ = match __registers.(%d) with\n\
        \      | None -> %s\n\
        \      | Some (%s.MenhirInterpreter.Element (_, _, p, _)) -> p\n\
        \    in"
        name offset
        none
        E.parser_name
    | End_loc ->
      print
        "    let _endloc_%s_ = match __registers.(%d) with\n\
        \      | None -> %s\n\
        \      | Some (%s.MenhirInterpreter.Element (_, _, _, p)) -> p\n\
        \    in"
        name offset
        none
        E.parser_name

  let output_code oc =
    let print fmt = Printf.fprintf oc fmt in
    print
      "let execute_%s %s\n
      \  (__clause, (__registers : %s.MenhirInterpreter.element option array))\n\
      \  ((token : %s.MenhirInterpreter.token), _startloc_token_, _endloc_token_)\n\
      \  : _ option = match __clause, token with\n"
      E.entry.name (String.concat " " E.entry.args)
      E.parser_name E.parser_name;
    let output_clause index (_nfa, (_captures, captures_def)) =
      let clause = Vector.get Clauses.vector index in
      let roffset = ref 0 in
      let lookahead_constraint = match clause.Syntax.lookaheads with
        | [] -> None
        | symbols ->
          let lookahead_msg =
            "Lookahead can either be a terminal or `first(nonterminal)'"
          in
          let term_pattern t =
            let name = Info.Terminal.to_string t in
            match Info.Terminal.semantic_value t with
            | None -> name
            | Some _ -> name ^ " _"
          in
          let sym_pattern (sym, pos) =
            match sym with
            | Syntax.Apply ("first", [sym]) ->
              begin match Info.Symbol.prj (Indices.get_symbol pos sym) with
                | L t ->
                  let t = Info.Terminal.to_string t in
                  failwith (lookahead_msg ^ "; in first(" ^ t ^ "), " ^
                            t ^ " is a terminal")
                | R n ->
                  Info.Nonterminal.to_g n
                  |> Grammar.Nonterminal.first
                  |> List.map Info.Terminal.of_g
                  |> List.map term_pattern
              end
            | Syntax.Name _ ->
              begin match Info.Symbol.prj (Indices.get_symbol pos sym) with
              | R n ->
                failwith (lookahead_msg ^ "; " ^
                          Info.Nonterminal.to_string n ^ " is a nonterminal")
              | L t -> [term_pattern t]
              end
            | _ ->
              failwith lookahead_msg
          in
          Some (string_concat_map ~wrap:("(",")") "|" Fun.id @@
                List.concat_map sym_pattern symbols)
      in
      print "  | %d, %s -> begin\n"
        (Index.to_int index)
        (Option.value lookahead_constraint ~default:"_");
      IndexMap.iter (bind_capture ~print:(output_string oc) ~roffset) captures_def;
      begin match clause.Syntax.action with
        | Unreachable ->
          print "    failwith \"Should be unreachable\"\n"
        | Partial (loc, str) ->
          print "%s%s\n"
            (print_loc loc) (rewrite_loc_keywords str)
        | Total (loc, str) ->
          print "    Some (\n%s%s\n    )\n"
            (print_loc loc) (rewrite_loc_keywords str)
      end;
      print "    end\n";
      if Option.is_some lookahead_constraint then
        print "  | %d, _ -> None\n" (Index.to_int index)
    in
    Vector.iteri output_clause NFA.clauses;
    print "  | _ -> failwith \"Invalid action (internal error or API misuse)\"\n\n"
end
