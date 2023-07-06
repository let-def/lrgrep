open Utils
open Misc
open Front
open Fix.Indexing
open Regexp
open Lrgrep_support

module type STACKS = sig
  type lr1
  type n
  val n : n cardinal
  val initials : n indexset
  val next : n index -> n indexset
  val label : n index -> lr1 indexset
end

module Printer : sig
  type t
  val create : filename:string -> ?line:int -> (string -> unit) -> t
  val print : ?loc:Syntax.location -> t -> string -> unit
  val fmt : ?loc:Syntax.location -> t -> ('a, unit, string, unit) format4 -> 'a
end = struct
  type t = {
    filename: string;
    mutable line: int;
    output: string -> unit;
    mutable reloc: bool;
    mutable last_is_nl: bool;
  }

  let output t = function
    | "" -> ()
    | str ->
      let nl = ref 0 in
      let last = String.length str - 1 in
      for i = 0 to last do
        if str.[i] = '\n' then incr nl;
      done;
      t.line <- t.line + !nl;
      t.last_is_nl <- str.[last] = '\n';
      t.output str

  let create ~filename ?(line=1) output =
    {filename; line; output; reloc = false; last_is_nl = true}

  let print_loc_dir t filename line =
    output t (Printf.sprintf "# %d %S\n" (line + 1) filename)

  let print_loc t (loc : Syntax.location) =
    print_loc_dir t loc.loc_file loc.start_line;
    output t (String.make loc.start_col ' ')

  let print ?loc t msg =
    match loc with
    | None ->
      if t.reloc then (
        if not t.last_is_nl then output t "\n";
        print_loc_dir t t.filename t.line;
        t.reloc <- false;
      );
      output t msg
    | Some loc ->
      if not t.last_is_nl then output t "\n";
      print_loc t loc;
      t.reloc <- true;
      output t msg

  let fmt ?loc t fmt =
    Printf.ksprintf (print ?loc t) fmt
end

module Entry
    (Transl : Transl.S)
    (Stacks: STACKS with type lr1 := Transl.Regexp.Info.Lr1.n)
    (E : sig
       val parser_name : string
       val entry : Syntax.entry
     end)
    () :
sig
  open Transl.Regexp.Info

  module Clause : sig
    include CARDINAL
    type t = n index
    val total : n indexset
    val captures : t -> Capture.set
  end

  module BigDFA : sig
    type n
    val n : n cardinal
  end

  module Label : sig
    type t = {
      filter: Lr1.set;
      captures: (Capture.t * Register.t) list;
      clear: Register.set;
      moves: Register.t Register.map;
    }
    val register_count : int
    val compare : t -> t -> int
  end

  module MinDFA : sig
    type states
    val states : states cardinal
  end

  module OutDFA : sig
    type states
    val states : states cardinal
    val initial : states index

    type transitions
    val transitions : transitions cardinal

    val source : transitions index -> states index
    val label : transitions index -> Label.t
    val target : transitions index -> states index

    (* Labels which are reachable (there exists stacks that could t
       but for which the state defines no transition.
       They should be rejected at runtime. *)
    val unhandled : states index -> Lr1.set

    val outgoing : states index -> transitions indexset
    val matching : states index -> (Clause.t * Register.t Capture.map) list
    val threads : states index -> (bool * Clause.t * Register.t Capture.map) list
  end

  val output_code : Printer.t -> unit
end =
struct
  open Transl
  open Regexp
  open Info

  module Preclause = struct
    include Vector.Of_array(struct
        type a = Syntax.clause
        let array = Array.of_list E.entry.clauses
      end)

    let n = Vector.length vector

    type t = n index

    let total =
      IndexSet.init_from_set (Vector.length vector) (fun index ->
          let clause = Vector.get vector index in
          match clause.action with
          | Syntax.Unreachable -> true
          | Syntax.Total _ -> clause.lookaheads = []
          | Syntax.Partial _ -> false
        )
  end

  module LazyNFA = struct
    type t = {
      (* State construction *)
      uid: int;
      k: K.t;
      transitions: (K.label * t lazy_t) list;
      accept: bool;
      clause: Preclause.t;
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
      let capture_tbl = Hashtbl.create 7 in
      let capture_def = ref IndexMap.empty in
      let capture kind name =
        let key = (kind, name) in
        match Hashtbl.find_opt capture_tbl key with
        | Some index -> index
        | None ->
          let index = capture () in
          Hashtbl.add capture_tbl key index;
          capture_def := IndexMap.add index key !capture_def;
          index
      in
      let captures, re =
        Transl.transl ~capture ~for_reduction:false def.pattern
      in
      let initial = make index Regexp.K.(More (re, Done)) in
      (initial, (captures, !capture_def))

    let clauses =
      Vector.mapi (process_clause ~capture:(Capture.gensym ())) Preclause.vector
  end

  module Clause = struct
    include Preclause

    let captures clause =
      let _, (cap, _) = Vector.get LazyNFA.clauses clause in
      cap
  end

  module BigDFA : sig
    include CARDINAL

    type ('src, 'tgt) _mapping = ('tgt, 'src index * Capture.set) vector

    type 'n t = private {
      index: n index;
      group: ('n, LazyNFA.t) vector;
      transitions: (Lr1.set * 'n mapping lazy_t) list;
      mutable visited_labels: Lr1.set;
      mutable visited: Stacks.n indexset;
      mutable scheduled: Stacks.n indexset;
    }
    and 'src mapping = Mapping : ('src, 'tgt) _mapping * 'tgt t -> 'src mapping

    type packed = Packed : 'n t -> packed [@@unboxed]

    (*val determinize : ('a, LazyNFA.t) vector -> 'a t*)
    val initial : n index
    val states : (n, packed) vector
    (*val iter_transitions : 'a t -> (Lr1.set -> 'a mapping -> unit) -> unit*)
    val iter_refined_transitions :
      'a t -> (Lr1.n indexset -> 'a mapping -> unit) -> unit
    val get_registers : 'm t -> ('m, Register.t Capture.map) vector
    val register_count : int
  end =
  struct
    open IndexBuffer

    let group_make (type a) (prj : a -> LazyNFA.t) (ts : a list) : a array =
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
                if IndexSet.mem th.clause Clause.total
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

    (*let group_index_of (type n) th (t : (n, LazyNFA.t) vector) =
      let exception Found of n index in
      match
        Vector.iteri
          (fun i x -> if LazyNFA.compare x th = 0 then raise (Found i))
          t
      with
      | () -> raise Not_found
      | exception (Found n) -> n*)

    module State = Gen.Make()
    type n = State.n
    let n = State.n

    type ('src, 'tgt) _mapping = ('tgt, 'src index * Capture.set) vector

    type 'n t = {
      index: n index;
      group: ('n, LazyNFA.t) vector;
      transitions: (Lr1.set * 'n mapping lazy_t) list;
      mutable visited_labels: Lr1.set;
      mutable visited: Stacks.n indexset;
      mutable scheduled: Stacks.n indexset;
    }
    and 'src mapping = Mapping : ('src, 'tgt) _mapping * 'tgt t -> 'src mapping

    type packed = Packed : 'n t -> packed [@@ocaml.unboxed]

    let states = State.get_generator ()

    module GroupMap = Map.Make(struct
        type t = LazyNFA.t array
        let compare g1 g2 = array_compare LazyNFA.compare g1 g2
      end)

    let determinize group =
      let map = ref GroupMap.empty in
      let rec aux : type n . (n, LazyNFA.t) vector -> n t =
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
                (fun i (nfa : LazyNFA.t) acc -> List.rev_map (make i) nfa.transitions @ acc)
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
              visited_labels=IndexSet.empty;
              scheduled=IndexSet.empty;
              visited=IndexSet.empty
            } in
            Gen.commit states reservation (Packed state);
            map := GroupMap.add (Vector.as_array group) (Packed state) !map;
            state
      in
      aux group

    let initial =
      let Vector.Packed group = Vector.of_array (
          LazyNFA.clauses
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
        List.iter begin fun (label, target) ->
          let really_empty = ref true in
          let expand_stack stack =
            if IndexSet.disjoint (Stacks.label stack) label
            then IndexSet.empty
            else (really_empty := false; Stacks.next stack)
          in
          let stacks = indexset_bind todo expand_stack in
          if not !really_empty then
            let lazy (Mapping (_, t')) = target in
            if not (IndexSet.is_empty stacks) then
              schedule t'.index stacks
        end t.transitions
      in
      let rec loop () =
        match !todo with
        | [] -> ()
        | todo' ->
          todo := [];
          List.iter update todo';
          loop ()
      in
      schedule initial Stacks.initials;
      loop ()

    let states = Gen.freeze states

    let () =
      Vector.iter (fun (Packed t) ->
          t.visited_labels <- indexset_bind t.visited Stacks.label
        ) states

    let iter_transitions t f =
      List.iter (fun (label, target) ->
          if Lazy.is_val target then (
            let lazy mapping = target in
            f label mapping
          )
        ) t.transitions

    let iter_refined_transitions t f =
      let set = t.visited_labels in
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
      let process (Packed src) =
        let live_src = liveness src in
        let process_transition _label (Mapping (mapping, tgt)) =
          let changed = ref false in
          let live_tgt = liveness tgt in
          let process_mapping tgt_j (src_i, captures) =
            let live = IndexSet.union (Vector.get live_src src_i) captures in
            let live' = Vector.get live_tgt tgt_j in
            if not (IndexSet.equal live live') then (
              Vector.set live_tgt tgt_j live;
              changed := true;
            )
          in
          Vector.iteri process_mapping mapping;
          if !changed then
            push todo (Packed tgt)
        in
        iter_transitions src process_transition
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

    let registers : (n, Register.t Capture.map Vector.packed) vector =
      Vector.make (Vector.length states) empty_registers

    let get_registers (type m) (st : m t) : (m, _) vector =
      let Vector.Packed regs = Vector.get registers st.index in
      let Refl = assert_equal_cardinal (Vector.length regs) (Vector.length st.group) in
      regs

    (* Naive allocator *)
    (*let () =
      let inflate_set (f : 'n index -> 'a) (set : 'n indexset) : ('n, 'a) indexmap =
        IndexSet.fold (fun i map -> IndexMap.add i (f i) map) set IndexMap.empty
      in
      let init (Packed state) =
        let in_use = ref IntSet.empty in
        let alloc caps = inflate_set (fun _ -> Register.of_int (IntSet.allocate in_use)) caps in
        Vector.set registers state.index
          (Vector.Packed (Vector.map alloc (liveness state)))
      in
      Vector.iter init states*)

    (* Smarter allocator (minimizing moves) *)
    let () =
      let allocate_successor (registers : (_, Register.t Capture.map) vector) allocated mapping target =
        let live = liveness target in
        let in_use = ref (
            Vector.fold_right
              (fun (ii, _) set -> IndexSet.union (Vector.get allocated ii) set)
              mapping IndexSet.empty
            : Register.set :> IntSet.t
          )
        in
        (* Allocate fresh registers *)
        Vector.mapi begin fun j (i, _) ->
          let allocated = Vector.get registers i in
          let live = Vector.get live j in
          let to_allocate = IndexSet.diff live (IndexMap.domain allocated) in
          IndexSet.fold (fun cap allocated ->
              IndexMap.add cap (Register.of_int (IntSet.allocate in_use)) allocated
            ) to_allocate allocated
        end mapping
      in
      let init (Packed src) =
        let regs = get_registers src in
        let allocated = Vector.map (fun x -> IndexMap.fold (fun _ reg map -> IndexSet.add reg map) x IndexSet.empty) regs in
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
        Vector.iter (IndexMap.iter (fun _ reg ->
            max_index := max !max_index (Index.to_int reg))) regs;
      in
      Vector.iter check_state states;
      Printf.eprintf "register allocation:\nmax live registers: %d\nregister count: %d\n"
        !max_live (!max_index + 1);
      !max_index + 1
  end

  module Label = struct
    type t = {
      filter: Lr1.set;
      captures: (Capture.t * Register.t) list;
      clear: Register.set;
      moves: Register.t Register.map;
    }

    let compare t1 t2 =
      let c = IndexSet.compare t1.filter t2.filter in
      if c <> 0 then c else
        let c =
          List.compare
            (compare_pair compare_index compare_index)
            t1.captures t2.captures
        in
        if c <> 0 then c else
          let c = IndexMap.compare compare_index t1.moves t2.moves in
          if c <> 0 then c else
            let c = IndexSet.compare t1.clear t1.clear in
            c

    let register_count = BigDFA.register_count
  end

  module RunDFA = struct
    type states = BigDFA.n
    let states = BigDFA.n

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
        let process_state (BigDFA.Packed source) =
          let src_regs = BigDFA.get_registers source in
          BigDFA.iter_refined_transitions source @@
          fun filter (BigDFA.Mapping (mapping, target)) ->
          let tgt_regs = BigDFA.get_registers target in
          let captures = ref [] in
          let moves = ref IndexMap.empty in
          let clear = ref IndexSet.empty in
          let process_mapping (src_i, captured) tgt_bank =
            let src_bank = Vector.get src_regs src_i in
            IndexMap.iter (fun capture tgt_reg ->
                if IndexSet.mem capture captured then
                  push captures (capture, tgt_reg)
                else
                  match IndexMap.find_opt capture src_bank with
                  | Some src_reg ->
                    if src_reg <> tgt_reg then
                      moves := IndexMap.add src_reg tgt_reg !moves
                  | None ->
                    partial_captures := IndexSet.add capture !partial_captures;
                    clear := IndexSet.add tgt_reg !clear
              ) tgt_bank
          in
          Vector.iter2 process_mapping mapping tgt_regs;
          let captures = !captures and moves = !moves and clear = !clear in
          let label = {Label.filter; captures; moves; clear} in
          ignore (Gen.add all {source = source.index; target = target.index; label})
        in
        Vector.iter process_state BigDFA.states

      let all = Gen.freeze all
    end

    type transitions = Transition.n
    let transitions = Transition.n

    let partial_captures =
      let acc = !partial_captures in
      Vector.fold_left begin fun acc (BigDFA.Packed st) ->
        Vector.fold_left2 begin fun acc (nfa : LazyNFA.t) regs ->
          if nfa.accept then
            let _, (cap, _) = Vector.get LazyNFA.clauses nfa.clause in
            IndexSet.fold begin fun var acc ->
              if IndexMap.mem var regs
              then acc
              else IndexSet.add var acc
            end cap acc
          else acc
        end acc st.group (BigDFA.get_registers st)
      end acc BigDFA.states

    let label i = (Vector.get Transition.all i).label
    let source i = (Vector.get Transition.all i).source
    let target i = (Vector.get Transition.all i).target

    let initials f = f BigDFA.initial
    let finals f =
      Vector.iter (fun (BigDFA.Packed st) ->
          let is_final acc nfa = acc || nfa.LazyNFA.accept in
          if Vector.fold_left is_final false st.group then
            f st.index
        ) BigDFA.states

    let refinements refine =
      (* Refine states by accepted actions *)
      let acc = ref [] in
      Vector.iter (fun (BigDFA.Packed st) ->
          let add_final acc nfa =
            if nfa.LazyNFA.accept then IndexSet.add nfa.LazyNFA.clause acc else acc
          in
          let accepted = Vector.fold_left add_final IndexSet.empty st.group in
          if not (IndexSet.is_empty accepted) then
            push acc (accepted, st.index)
        ) BigDFA.states;
      Misc.group_by !acc
        ~compare:(fun (a1, _) (a2, _) -> IndexSet.compare a1 a2)
        ~group:(fun (_, st) sts -> st :: List.map snd sts)
      |> List.iter (fun group -> refine (fun ~add -> List.iter add group))
  end

  module MinDFA = Valmari.Minimize (Label) (RunDFA)

  module OutDFA = struct

    include MinDFA
      let initial = initials.(0)

    (*include RunDFA
    let initial = BigDFA.initial
    let transport_state state = Some state
    let represent_state state = state*)

    let outgoing = Vector.make states IndexSet.empty
    let unhandled = Vector.make states IndexSet.empty

    let () =
      (* Initialize with all reachable labels *)
      Vector.iter begin fun (BigDFA.Packed source) ->
        match transport_state source.index with
        | None -> ()
        | Some index ->
          vector_set_union unhandled index source.visited_labels
      end BigDFA.states;
      (* Remove the ones for which transitions exist *)
      Index.rev_iter transitions begin fun tr ->
        let index = source tr in
        let label = label tr in
        let visited = Vector.get unhandled index in
        let visited = IndexSet.diff visited label.filter in
        Vector.set unhandled index visited;
        vector_set_add outgoing index tr;
      end

    let outgoing = Vector.get outgoing
    let unhandled = Vector.get unhandled

    let matching state =
      let BigDFA.Packed source =
        Vector.get BigDFA.states (represent_state state)
      in
      let add_accepting {LazyNFA. accept; clause; _} regs acc =
        if not accept then acc else
          (clause, regs) :: acc
      in
      let registers = BigDFA.get_registers source in
      Vector.fold_right2 add_accepting source.group registers []

    let threads state =
      let BigDFA.Packed source =
        Vector.get BigDFA.states (represent_state state)
      in
      let add_accepting {LazyNFA. accept; clause; _} regs acc =
        (accept, clause, regs) :: acc
      in
      let registers = BigDFA.get_registers source in
      Vector.fold_right2 add_accepting source.group registers []
  end

  let captures_lr1 =
    let map = ref IndexMap.empty in
    Index.iter OutDFA.transitions (fun tr ->
        let label = OutDFA.label tr in
        map := List.fold_left (fun map (cap, _reg) ->
            IndexMap.update cap (function
                | None -> Some label.filter
                | Some set' -> Some (IndexSet.union set' label.filter)
              ) map
          ) !map label.captures
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

  let bind_capture out ~roffset index (def, name) =
    let is_optional = IndexSet.mem index RunDFA.partial_captures in
    let none = if is_optional then "None" else "assert false" in
    let some x = if is_optional then "Some (" ^ x ^ ")" else x in
    let offset = !roffset in
    incr roffset;
    match def with
    | Value ->
      let typ = recover_type index in
      Printer.fmt out
        "    let %s, _startloc_%s_, _endloc_%s_ = match __registers.(%d) with \n\
        \      | None -> %s\n\
        \      | Some (%s.MenhirInterpreter.Element (%s, %s, startp, endp)%s) ->\n"
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
          Printer.fmt out
            "        let x = match %s.MenhirInterpreter.incoming_symbol st with\n\
            \          | %s -> (x : %s) \n\
            \          | _ -> assert false\n\
            \        in\n"
            E.parser_name (String.concat " | " matchers) typ
      end;
      Printer.fmt out "        (%s, %s, %s)\n" (some "x") (some "startp") (some "endp");
      Printer.fmt out "    in\n";
      Printer.fmt out "    let _ = %s in\n" name
    | Start_loc ->
      Printer.fmt out
        "    let _startloc_%s_ = match __registers.(%d) with\n\
        \      | None -> %s\n\
        \      | Some (%s.MenhirInterpreter.Element (_, _, p, _)) -> %sp\n\
        \    in\n"
        name offset
        none
        E.parser_name
        (if is_optional then "Some " else "")
    | End_loc ->
      Printer.fmt out
        "    let _endloc_%s_ = match __registers.(%d) with\n\
        \      | None -> %s\n\
        \      | Some (%s.MenhirInterpreter.Element (_, _, _, p)) -> %sp\n\
        \    in\n"
        name offset
        none
        E.parser_name
        (if is_optional then "Some " else "")

  let output_code out =
    Printer.fmt out
      "let execute_%s %s\n
      \  (__clause, (__registers : %s.MenhirInterpreter.element option array))\n\
      \  ((token : %s.MenhirInterpreter.token), _startloc_token_, _endloc_token_)\n\
      \  : _ option = match __clause, token with\n"
      E.entry.name (String.concat " " E.entry.args)
      E.parser_name E.parser_name;
    let output_clause index (_nfa, (_captures, captures_def)) =
      let clause = Vector.get Clause.vector index in
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
      Printer.fmt out
        "  | %d, %s ->\n"
        (Index.to_int index)
        (Option.value lookahead_constraint ~default:"_");
      IndexMap.iter (bind_capture out ~roffset) captures_def;
      begin match clause.Syntax.action with
        | Unreachable ->
          Printer.print out "    failwith \"Should be unreachable\"\n"
        | Partial (loc, str) ->
          Printer.print out "    (\n";
          Printer.fmt out ~loc "%s\n" (rewrite_loc_keywords str);
          Printer.print out "    )\n"
        | Total (loc, str) ->
          Printer.print out "    Some (\n";
          Printer.fmt out ~loc "%s\n" (rewrite_loc_keywords str);
          Printer.print out "    )\n"
      end;
      if Option.is_some lookahead_constraint then
        Printer.fmt out "  | %d, _ -> None\n" (Index.to_int index)
    in
    Vector.iteri output_clause LazyNFA.clauses;
    Printer.print out "  | _ -> failwith \"Invalid action (internal error or API misuse)\"\n\n"
end
