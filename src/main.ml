open Utils
open Misc

module StringSet = Set.Make(String)

(* Command-line parsing. *)

let source_name = ref None
let output_name = ref None
let grammar_file = ref None
let check_coverage = ref false
let verbose = ref false

let escape_and_align_left fmt =
  Printf.ksprintf (fun str ->
      let str = Bytes.unsafe_of_string (String.escaped str) in
      for i = 0 to Bytes.length str - 2 do
        if Bytes.get str (i+0) = '\\' &&
           Bytes.get str (i+1) = 'n' then
          Bytes.set str (i+1) 'l'
      done;
      ("\"" ^ Bytes.unsafe_to_string str ^ "\\l\"")
    ) fmt

let usage =
  Printf.sprintf
    "lrgrep, a menhir lexer\n\
     usage: %s [options] <source>"
    Sys.argv.(0)

let print_version_num () =
  print_endline "0.1";
  exit 0

let print_version_string () =
  print_string "The Menhir parser lexer generator :-], version ";
  print_version_num ()

let error {Front.Syntax. line; col} fmt =
  Printf.eprintf "Error line %d, column %d: " line col;
  Printf.kfprintf (fun oc -> output_char oc '\n'; flush oc; exit 1) stderr fmt

let warn {Front.Syntax. line; col} fmt =
  Printf.eprintf "Warning line %d, column %d: " line col;
  Printf.kfprintf (fun oc -> output_char oc '\n'; flush oc) stderr fmt

let eprintf = Printf.eprintf

let specs = [
  "-o", Arg.String (fun x -> output_name := Some x),
  " <file.ml>  Set output file name to <file> (defaults to <source>.ml)";
  "-g", Arg.String (fun x -> grammar_file := Some x),
  " <file.cmly>  Path of the Menhir compiled grammar to analyse (*.cmly)";
  "-v", Arg.Set verbose,
  " Increase output verbosity";
  "-version", Arg.Unit print_version_string,
  " Print version and exit";
  "-vnum", Arg.Unit print_version_num,
  " Print version number and exit";
  "-coverage", Arg.Set check_coverage,
  " Check error coverage";
]

let () = Arg.parse specs (fun name -> source_name := Some name) usage

let source_file = match !source_name with
  | None ->
    Format.eprintf "No source provided, stopping now.\n";
    Arg.usage specs usage;
    exit 1
  | Some name -> name

let grammar_file = match !grammar_file with
  | Some filename -> filename
  | None ->
    Format.eprintf "No grammar provided (-g), stopping now.\n";
    Arg.usage specs usage;
    exit 1

let print_parse_error_and_exit lexbuf exn =
  let bt = Printexc.get_raw_backtrace () in
  begin match exn with
    | Front.Parser.Error ->
      let p = Lexing.lexeme_start_p lexbuf in
      Printf.fprintf stderr
        "File \"%s\", line %d, character %d: syntax error.\n"
        p.Lexing.pos_fname p.Lexing.pos_lnum
        (p.Lexing.pos_cnum - p.Lexing.pos_bol)
    | Front.Lexer.Lexical_error {msg; file; line; col} ->
      Printf.fprintf stderr
        "File \"%s\", line %d, character %d: %s.\n"
        file line col msg
    | _ -> Printexc.raise_with_backtrace exn bt
  end;
  exit 3

let lexer_definition =
  let ic = open_in_bin source_file in
  Front.Lexer.ic := Some ic;
  let lexbuf = Lexing.from_channel ~with_positions:true ic in
  Lexing.set_filename lexbuf source_file;
  let result =
    try Front.Parser.lexer_definition Front.Lexer.main lexbuf
    with exn -> print_parse_error_and_exit lexbuf exn
  in
  Front.Lexer.ic := None;
  result

module Grammar = MenhirSdk.Cmly_read.Read(struct let filename = grammar_file end)
module Info = Mid.Info.Make(Grammar)
open Mid.Regexp
module Regexp = Make(Info)()

(* Index LR(1) states by incoming symbol, goto transitions, items, ... *)

module Lr1_index = struct
  open Front
  open Info
  open Fix.Indexing

  (* Group by incoming symbol *)

  let states_of_symbol = Vector.make Symbol.n IndexSet.empty

  let () =
    Index.iter Lr1.n (fun lr1 ->
        match Lr1.incoming lr1 with
        | None -> ()
        | Some sym -> vector_set_add states_of_symbol sym lr1
      )

  let states_of_symbol = Vector.get states_of_symbol

  let states_by_item_suffix = Vector.make Symbol.n IndexSet.empty

  let () =
    Index.iter Lr1.n (fun state ->
        List.iter (fun (prod, dot) ->
            if dot < Production.length prod then (
              vector_set_add states_by_item_suffix
                (Production.rhs prod).(dot) state
            )
          ) (Lr1.items state)
      )

  let states_by_item_suffix = Vector.get states_by_item_suffix

  (*let states_by_item_suffix ?(candidates=Lr1.all) ~anchored = function
    | [] ->
      if anchored then
        IndexSet.filter (fun lr1 ->
            List.exists
              (fun (prod, pos) -> pos = Production.length prod)
              (Lr1.items lr1)
          ) candidates
      else
        candidates
    | (p0 :: _) as pattern ->
      let len_pat = List.length pattern in
      let match_item (prod, pos) =
        let len_suf = Production.length prod - pos in
        let len_pat = len_pat in
        (if anchored then len_pat = len_suf else len_pat <= len_suf) &&
        match
          let rhs = Production.rhs prod in
          List.iteri (fun i sym' ->
              if not (match_sym rhs.(pos + i) sym') then
                raise Not_found
            ) pattern
        with
        | () -> true
        | exception Not_found -> false
      in
      let match_state state = List.exists match_item (Lr1.items state) in
      let candidates =
        match p0 with
        | None -> candidates
        | Some sym ->
          Lr1.intersect candidates
            (Vector.get states_by_item_suffix sym)
      in
      IndexSet.filter match_state candidates

  let states_by_item_prefix ?(candidates=Lr1.all) ~anchored = function
    | [] ->
      if anchored then
        IndexSet.filter (fun lr1 ->
            List.exists
              (fun (_, pos) -> pos = 0)
              (Lr1.items lr1)
          ) candidates
      else
        candidates
    | (p0 :: _) as pattern ->
      let len_pat = List.length pattern in
      let match_item (prod, len_pre) =
        (if anchored then len_pat = len_pre else len_pat <= len_pre) &&
        match
          let rhs = Production.rhs prod in
          List.iteri (fun i sym' ->
              if not (match_sym rhs.(len_pre - i - 1) sym') then
                raise Not_found
            ) pattern
        with
        | () -> true
        | exception Not_found -> false
      in
      let match_state state = List.exists match_item (Lr1.items state) in
      let candidates =
        match p0 with
        | None -> candidates
        | Some sym ->
          Lr1.intersect candidates (states_of_symbol sym)
      in
      IndexSet.filter match_state candidates*)

  (* Map symbol names to actual symbols *)

  let linearize_symbol =
    let buffer = Buffer.create 32 in
    function
    | Syntax.Name s -> s
    | sym ->
      Buffer.reset buffer;
      let rec aux = function
        | Syntax.Name s -> Buffer.add_string buffer s
        | Syntax.Apply (s, args) ->
          Buffer.add_string buffer s;
          Buffer.add_char buffer '(';
          List.iteri (fun i sym ->
              if i > 0 then Buffer.add_char buffer ',';
              aux sym
            ) args;
          Buffer.add_char buffer ')'
      in
      aux sym;
      Buffer.contents buffer

  let find_symbol =
    let table = Hashtbl.create 7 in
    let add_symbol s = Hashtbl.add table (Symbol.name ~mangled:false s) s in
    Index.iter Symbol.n add_symbol;
    fun name -> Hashtbl.find_opt table (linearize_symbol name)

  let get_symbol pos sym =
    match find_symbol sym with
    | None -> error pos "Unknown symbol %s" (linearize_symbol sym)
    | Some sym -> sym
end

module Transl = struct
  open Info
  open Front.Syntax
  open Fix.Indexing
  open Regexp

  let is_immediate_label {K. filter; captures} =
    IndexSet.equal filter Lr1.all &&
    IndexSet.is_empty captures

  let match_sym sym = function
    | None -> true
    | Some sym' -> equal_index sym sym'

  let transl_filter position ~lhs ~pre_anchored ~prefix ~suffix ~post_anchored =
    let transl_sym = Option.map (Lr1_index.get_symbol position) in
    let lhs = transl_sym lhs in
    let prefix = List.rev_map transl_sym prefix in
    let suffix = List.map transl_sym suffix in
    let check_len anchored pat len =
      if anchored
      then pat = len
      else pat <= len
    in
    let len_pre = List.length prefix in
    let len_suf = List.length suffix in
    let check_item (prod, pos) =
      let rhs = Production.rhs prod in
      check_len pre_anchored len_pre pos &&
      check_len post_anchored len_suf (Production.length prod - pos) &&
      match_sym (Symbol.inj_r (Production.lhs prod)) lhs &&
      list_foralli (fun i pat -> match_sym rhs.(pos + i) pat) suffix &&
      list_foralli (fun i pat -> match_sym rhs.(pos - i - 1) pat) prefix
    in
    let check_state state =
      List.exists check_item (Lr1.items state)
    in
    let candidates = Lr1.all in
    let candidates = match suffix with
      | Some x :: _ ->
        Lr1.intersect candidates (Lr1_index.states_by_item_suffix x)
      | _ -> candidates
    in
    let candidates = match prefix with
      | Some x :: _ ->
        Lr1.intersect candidates (Lr1_index.states_of_symbol x)
      | _ -> candidates
    in
    IndexSet.filter check_state candidates

  type capture_kind =
    | Start_loc
    | End_loc
    | Value

  type lr1_trie = {
    mutable sub: lr1_trie Lr1.map;
    mutable reached: Redgraph.state indexset;
  }

  let lr1_trie_root =
    let root = {sub = IndexMap.empty; reached = IndexSet.empty} in
    let rec visit_trie node = function
      | [] -> node
      | x :: xs ->
        let node' = match IndexMap.find_opt x node.sub with
          | Some node' -> node'
          | None ->
            let node' = {sub = IndexMap.empty; reached = IndexSet.empty} in
            node.sub <- IndexMap.add x node' node.sub;
            node'
        in
        visit_trie node' xs
    in
    Index.iter Redgraph.state (fun state ->
        let top, rest = Redgraph.get_stack state in
        let node = visit_trie root (top :: rest) in
        node.reached <- IndexSet.add state node.reached
      );
    root

  let compile_reduce_expr re =
    let reached = ref IndexSet.empty in
    let immediate = ref IndexSet.empty in
    let rec step node k =
      let process_next = function
        | (label, None) ->
          if node == lr1_trie_root then
            immediate := IndexSet.union !immediate label.K.filter
          else
            reached := (
              if IndexSet.equal Lr1.all label.filter then
                IndexSet.union node.reached !reached
              else
                IndexMap.fold (fun lr1 node' acc ->
                    if IndexSet.mem lr1 label.filter
                    then IndexSet.union acc node'.reached
                    else acc
                  ) node.sub !reached
            )
        | (label, Some k') ->
          IndexMap.iter (fun lr1 node' ->
              if IndexSet.mem lr1 label.K.filter then
                step node' k'
            ) node.sub
      in
      List.iter process_next (K.derive k)
    in
    step lr1_trie_root (K.More (re, K.Done));
    (!reached, !immediate)


  let transl ~capture ~for_reduction re =
    let all_cap = ref IndexSet.empty in
    let mk_capture kind name =
      let index = capture kind name in
      all_cap := IndexSet.add index !all_cap;
      IndexSet.singleton index
    in
    let rec transl ~for_reduction re =
      let desc = match re.desc with
        | Atom (capture, symbol) ->
          if for_reduction && Option.is_some capture then
            error re.position "Captures are not allowed inside reductions";
          let set = match symbol with
            | None -> Lr1.all
            | Some sym ->
              let sym = Lr1_index.get_symbol re.position sym in
              if for_reduction && Symbol.is_terminal sym then
                warn re.position "A reduction can only match non-terminals";
              Lr1_index.states_of_symbol sym
          in
          let cap = match capture with
            | None -> IndexSet.empty
            | Some name -> mk_capture Value name
          in
          RE.Set (set, cap)
        | Alternative res ->
          RE.Alt (List.map (transl ~for_reduction) res)
        | Repetition {expr; policy} ->
          RE.Star (transl ~for_reduction expr, policy)
        | Reduce {capture; policy; expr} ->
          if for_reduction then
            error re.position "Reductions cannot be nested";
          (* print_cmon stderr (Front.Syntax.cmon_regular_expression expr);*)
          let re = transl ~for_reduction:true expr in
          let pattern, immediate = compile_reduce_expr re in
          warn re.position
            "Reduce pattern is matching %d/%d cases (and matches immediately for %d states)"
            (IndexSet.cardinal pattern) (cardinal Redgraph.state)
            (IndexSet.cardinal immediate);
          let capture0, capture = match capture with
            | None -> IndexSet.empty, IndexSet.empty
            | Some name ->
              let capture0 = mk_capture Start_loc name in
              let capture = mk_capture End_loc name in
              (capture0, capture)
          in
          let r = RE.Reduce (capture0, {capture; pattern; policy}) in
          if IndexSet.is_empty immediate then
            r
          else if immediate == Lr1.all then
            RE.Alt [RE.make re.position r; RE.make re.position (RE.Seq [])]
          else
            RE.Alt [RE.make re.position r; RE.make re.position (RE.Filter immediate)]
        | Concat res ->
          RE.Seq (List.map (transl ~for_reduction) res)
        | Filter {lhs; pre_anchored; prefix; suffix; post_anchored} ->
          let states =
            transl_filter re.position ~lhs ~pre_anchored ~prefix ~suffix ~post_anchored
          in
          if IndexSet.is_empty states then
            warn re.position "No items match this filter";
          RE.Filter states
      in
      RE.make re.position desc
    in
    let result = transl ~for_reduction re in
    (!all_cap, result)


end

(*module Clause = struct
  open Info
  open Front.Syntax
  open Regexp
  end*)

open Front

let parser_module =
  String.capitalize_ascii (Filename.basename Grammar.Grammar.basename)

module Automata = struct
  open Info

  module K = Regexp.K
  module KMap = Map.Make(K)

  type k = K.t
  type label = K.label

  module Entry (E : sig val entry : Syntax.entry end)() = struct
    open Fix.Indexing

    module Clauses = struct
      include Vector.Of_array(struct
          type a = Syntax.clause
          let array = Array.of_list E.entry.clauses
        end)

      let n = Vector.length vector

      let total =
        IndexSet.init_from_set (Vector.length vector)
          (fun clause ->
             match (Vector.get vector clause).action with
             | Syntax.Total _ -> true
             | Syntax.Unreachable -> true
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

      exception Accept of (label * k) list

      let default_mark = ref ()

      let make clause =
        let nfa = ref KMap.empty in
        let rec aux k =
          match KMap.find_opt k !nfa with
          | Some t -> t
          | None ->
            let accept = ref false in
            let rec process_transitions = function
              | [] -> []
              | (label, target) :: rest ->
                begin match target with
                  | None when Transl.is_immediate_label label ->
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
            nfa := KMap.add k t !nfa;
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

    module LazyDFA = struct

      type thread = NFA.t

      module Threads : sig
        type index = private int
        type t
        val compare : t -> t -> int
        val make : (thread * 'a) list -> t * 'a array
        val fold : (index -> thread -> 'a -> 'a) -> t -> 'a -> 'a
        val index_of : thread -> t -> index
        val count : t -> int
        val get_index : 'a array -> index -> 'a
        val set_index : 'a array -> index -> 'a -> unit
      end = struct
        type index = int
        type t = thread array

        let compare t1 t2 =
          array_compare NFA.compare t1 t2

        let make ts =
          let mark = ref () in
          let last_accepted = ref `None in
          let ts = List.filter (fun ((th : thread), _) ->
              (th.mark != mark) && (
                th.mark <- mark;
                match !last_accepted with
                | `Total -> false
                | `Some clause when th.clause = clause -> false
                | _ ->
                  if th.accept then (
                    if IndexSet.mem th.clause Clauses.total
                    then last_accepted := `Total
                    else last_accepted := `Some th.clause
                  );
                  true
              )
            ) ts
          in
          let txs = Array.of_list ts in
          (Array.map fst txs, Array.map snd txs)

        let fold f x acc =
          let acc = ref acc in
          for i = 0 to Array.length x - 1 do
            acc := f i x.(i) !acc;
          done;
          !acc

        let index_of th t =
          let rec loop i =
            if NFA.compare t.(i) th = 0
            then i
            else loop (i + 1)
          in
          loop 0

        let count = Array.length
        let get_index = Array.get
        let set_index = Array.set
      end

      type threads = Threads.t
      type thread_mapping = (Threads.index * Capture.set) array

      module ThMap = Map.Make(Threads)

      type 'st t = {
        uid: int;
        threads: threads;
        transitions: (Lr1.set * (thread_mapping * 'st index) lazy_t) list;
        mutable visited: Lr1.set;
        mutable scheduled: Lr1.set;
      }

      include IndexBuffer.Gen(struct type nonrec 'a t = 'a t end)()

      let determinize threads =
        let ruid = ref 0 in
        let map = ref ThMap.empty in
        let rec aux threads =
          match ThMap.find_opt threads !map with
          | Some index -> index
          | None ->
            let rev_transitions =
              let make i ({K. filter; captures}, t) = (filter, (i, captures, t)) in
              Threads.fold
                (fun i (th : thread) acc -> List.rev_map (make i) th.NFA.transitions @ acc)
                threads []
            in
            let rev_partitions = IndexRefine.annotated_partition rev_transitions in
            let process_class (label, rev_targets) =
              label, lazy (
                let prepare_target (index, captures, lazy nfa) =
                  nfa, (index, captures)
                in
                let threads, mapping =
                  Threads.make (List.rev_map prepare_target rev_targets)
                in
                mapping, aux threads
              )
            in
            let transitions = List.map process_class rev_partitions in
            let uid = !ruid in
            incr ruid;
            let index = add {
              uid; threads; transitions;
              scheduled=IndexSet.empty; visited=IndexSet.empty
            } in
            map := ThMap.add threads index !map;
            index
        in
        aux threads

      let initial =
        let target, _mapping = Threads.make (Vector.to_list NFA.clauses) in
        determinize target

      let () =
        let todo = ref [] in
        let schedule i set =
          let t = get i in
          if not (IndexSet.subset set t.visited) then (
            let set = IndexSet.diff set t.visited in
            if IndexSet.is_empty t.scheduled then (
              push todo t;
              t.scheduled <- set
            ) else (
              t.scheduled <- IndexSet.union t.scheduled set
            )
          )
        in
        let update t =
          let todo = t.scheduled in
          t.visited <- IndexSet.union t.visited todo;
          t.scheduled <- IndexSet.empty;
          List.iter (fun (label, target) ->
              let label' = IndexSet.inter label todo in
              if not (IndexSet.is_empty label') then
                let lazy (_, t') = target in
                schedule t' (Lr1.set_predecessors label')
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

      let vector = freeze ()

      let iter_transitions i f =
        let t = Vector.get vector i in
        List.iter (fun (label, target) ->
            if Lazy.is_val target then (
              let lazy (mapping, t') = target in
              f label mapping t'
            )
          ) t.transitions

      let iter_refined_transitions i f =
        let t = Vector.get vector i in
        let set = t.visited in
        List.iter (fun (label, target) ->
            if Lazy.is_val target then (
              let lazy (mapping, t') = target in
              f (IndexSet.inter set label) mapping t'
            )
          ) t.transitions

      let liveness =
        let reserve t = Array.make (Threads.count t.threads) IndexSet.empty in
        Vector.map reserve vector

      let () =
        let todo = ref [] in
        let process i =
          let bank = Vector.get liveness i in
          let process_transition _label mapping j =
            let changed = ref false in
            let bank' = Vector.get liveness j in
            let process_mapping jj (ii, captures) =
              let live = IndexSet.union (Threads.get_index bank ii) captures in
              let live' = bank'.(jj) in
              if not (IndexSet.equal live live') then (
                bank'.(jj) <- live;
                changed := true;
              )
            in
            Array.iteri process_mapping mapping;
            if !changed then
              push todo j
          in
          iter_transitions i process_transition
        in
        Index.iter (Vector.length vector) process;
        let rec loop () =
          match !todo with
          | [] -> ()
          | todo' ->
            todo := [];
            List.iter process todo';
            loop ()
        in
        loop ()

      let registers = Vector.make (Vector.length vector) [||]

      let register_count =
        let process i threads =
          iter_transitions i @@ fun _ mapping j ->
          let allocated = Array.map (fun x -> IndexMap.fold (fun _ -> IntSet.add) x IntSet.empty) threads in
          if Array.length (Vector.get registers j) = 0 then (
            let live = Vector.get liveness j in
            let in_use =
              ref (Array.fold_right
                     (fun (ii, _) set -> IntSet.union set (Threads.get_index allocated ii))
                     mapping IntSet.empty)
            in
            (* Allocate fresh registers *)
            let bank =
              Array.mapi begin fun jj (ii, _) ->
                let allocated = Threads.get_index threads ii in
                let live = live.(jj) in
                let to_allocate = IndexSet.diff live (IndexMap.domain allocated) in
                IndexSet.fold (fun cap allocated ->
                    let index, in_use' = IntSet.allocate !in_use in
                    in_use := in_use';
                    IndexMap.add cap index allocated
                  ) to_allocate allocated
              end mapping
            in
            Vector.set registers j bank
          )
        in
        let zero = Index.of_int (Vector.length vector) 0 in
        Vector.set registers zero
          (Array.make (Threads.count (Vector.get vector zero).threads) IndexMap.empty);
        Vector.iteri process registers;
        let max_live = ref 0 in
        let max_index = ref (-1) in
        Vector.iter (fun threads ->
            let max_live' =
              Array.fold_left (fun sum map -> sum + IndexMap.cardinal map) 0 threads
            in
            max_live := max !max_live max_live';
            max_index := Array.fold_right (IndexMap.fold (fun _ -> max)) threads !max_index
          ) registers;
        Printf.eprintf "register allocation:\nmax live registers: %d\nregister count: %d\n" !max_live !max_index;
        !max_index + 1
    end

    module MinimizableDFA = struct
      type states = LazyDFA.n
      let states = LazyDFA.n

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
        type t = {
          source: states index;
          target: states index;
          label: Label.t;
        }
        include IndexBuffer.Gen(struct type nonrec _ t = t end)()

        let () =
          Index.iter states @@ fun source ->
          let src_regs = Vector.get LazyDFA.registers source in
          LazyDFA.iter_refined_transitions source @@ fun filter mapping target ->
          let tgt_regs = Vector.get LazyDFA.registers target in
          let captures = ref IndexMap.empty in
          let moves = ref IntMap.empty in
          let clear = ref IntSet.empty in
          let process_mapping (src_i, captured) tgt_bank =
            let src_bank = LazyDFA.Threads.get_index src_regs src_i in
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
          Array.iter2 process_mapping mapping tgt_regs;
          let captures = !captures and moves = !moves and clear = !clear in
          let label = {Label.filter; captures; moves; clear} in
          ignore (add {source; target; label})
      end

      type transitions = Transition.n
      let transitions = Transition.n

      let label i = (Transition.get i).label
      let source i = (Transition.get i).source
      let target i = (Transition.get i).target

      let initials f = f LazyDFA.initial
      let finals f =
        Index.iter states
          (fun st ->
             if LazyDFA.Threads.fold
                 (fun _ nfa acc -> acc || nfa.NFA.accept)
                 (LazyDFA.get st).threads false
             then f st)

      let refinements refine =
        (* Refine states by accepted actions *)
        let acc = ref [] in
        Index.iter states (fun st ->
            let accepted =
              LazyDFA.Threads.fold
                (fun _ nfa acc -> if nfa.NFA.accept then IndexSet.add nfa.NFA.clause acc else acc)
                (LazyDFA.get st).threads IndexSet.empty
            in
            if not (IndexSet.is_empty accepted) then
              push acc (accepted, st)
          );
        Misc.group_by !acc
          ~compare:(fun (a1, _) (a2, _) -> IndexSet.compare a1 a2)
          ~group:(fun (_, st) sts -> st :: List.map snd sts)
        |> List.iter (fun group -> refine (fun ~add -> List.iter add group))
    end

    module MinDFA = Valmari.Minimize (MinimizableDFA.Label) (MinimizableDFA)

    let captures_lr1 =
      let map = ref IndexMap.empty in
      Index.iter MinDFA.transitions (fun tr ->
          let label = MinDFA.label tr in
          let map' = IndexMap.map (fun _ -> label.filter) label.captures in
          map := IndexMap.union (fun _ a b -> Some (IndexSet.union a b)) !map map'
        );
      !map

    let output_code oc =
      let print fmt = Printf.fprintf oc fmt in
      print
        "let execute_%s %s : \
         (int * %s.MenhirInterpreter.element option array) * %s.token \
         -> _ option = function\n"
        E.entry.name (String.concat " " E.entry.args)
        parser_module parser_module;
      let output_clause index (_nfa, (captures, captures_def)) =
        let clause = Vector.get Clauses.vector index in
        let recover_types =
          let symbol_matcher s = match Info.Symbol.prj s with
            | L t -> "T T_" ^ Info.Terminal.to_string t
            | R n -> "N N_" ^ Grammar.Nonterminal.mangled_name (Info.Nonterminal.to_g n)
          in
          IndexMap.fold begin fun index (_def, name) acc ->
            let is_optional = IndexSet.mem index !MinimizableDFA.partial_captures in
            let typ =
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
            in
            let typ =
              match typ with
              | None -> None
              | Some (symbols, typ) ->
                let matchers =
                  List.map symbol_matcher (IndexSet.elements symbols)
                in
                Some (
                  Printf.sprintf "\
                  match %s.MenhirInterpreter.incoming_symbol st with \
                  | %s -> ((x : %s), startp, endp)
                  | _ -> assert false
                  " parser_module
                    (String.concat " | " matchers) typ
                )
            in
            match is_optional, typ with
            | true, None -> acc
            | true, Some typ ->
              Printf.sprintf "\
              let %s = match %s with \
                | None -> None \
                | Some (%s.MenhirInterpreter.Element (st, x, startp, endp)) -> \
                  Some (%s) in" name name parser_module typ
              :: acc
            | false, None ->
              Printf.sprintf "let %s = match %s with None -> assert false | Some x -> x in"
                name name :: acc
            | false, Some types ->
              Printf.sprintf "\
              let %s = match %s with None -> assert false \
                | Some (%s.MenhirInterpreter.Element (st, x, startp, endp)) -> \
                  %s in" name name parser_module types :: acc
          end captures_def []
          |> String.concat "\n"
        in
        let print_loc (loc : Syntax.location) =
          Printf.sprintf "# %d %S\n%s"
            loc.start_line loc.loc_file
            (String.make loc.start_col ' ')
        in
        let lookahead_constraint = match clause.Syntax.lookaheads with
          | [] -> None
          | terminals ->
            let sym_pattern (sym, pos) =
              match Info.Symbol.prj (Lr1_index.get_symbol pos (Syntax.Name sym)) with
              | R n ->
                failwith ("Lookahead should be a terminal, " ^
                          Info.Nonterminal.to_string n ^ " is a nonterminal")
              | L t ->
                let name = Info.Terminal.to_string t in
                match Info.Terminal.semantic_value t with
                | None -> name
                | Some _ -> name ^ " _"
            in
            Some (string_concat_map ~wrap:("(",")") "|" sym_pattern terminals)
        in
        print "  | (%d, [|%s|]), %s -> %s begin\n%s\n    end\n"
          (Index.to_int index)
          (String.concat ";" (List.map (fun (_,(_,x)) -> x) (IndexMap.bindings captures_def)))
          (Option.value lookahead_constraint ~default:"_")
          recover_types
          (match clause.Syntax.action with
           | Unreachable -> "failwith \"Should be unreachable\""
           | Partial (loc, str) ->
             print_loc loc ^ str
           | Total (loc, str) ->
             "Some (\n" ^ print_loc loc ^ str ^ ")");
        if Option.is_some lookahead_constraint then
          print "  | (%d, [|%s|]), _ -> None\n"
            (Index.to_int index)
            (string_concat_map ";" (fun _ -> "_") (IndexSet.elements captures))
      in
      Vector.iteri output_clause NFA.clauses;
      print "  | _ -> failwith \"Invalid action (internal error or API misuse)\"\n\n"
  end

  let process_entry oc (entry : Syntax.entry) = (
    let open Fix.Indexing in
    let open Entry(struct let entry = entry end)() in
    Printf.eprintf "DFA states: %d\n" (cardinal (Vector.length LazyDFA.vector));
    Printf.eprintf "Minimized DFA states: %d\n" (cardinal MinDFA.states);
    Printf.eprintf "Time spent: %.02fms\n" (Sys.time () *. 1000.);
    let transitions = Vector.make MinDFA.states IndexSet.empty in
    let halting = Vector.make MinDFA.states IndexSet.empty in
    Vector.iteri (fun source (def : _ LazyDFA.t) ->
        match MinDFA.transport_state source with
        | None -> ()
        | Some index ->
          let visited = Vector.get halting index in
          let visited = IndexSet.union def.visited visited in
          Vector.set halting index visited
      ) LazyDFA.vector;
    Index.rev_iter MinDFA.transitions begin fun tr ->
      let index = MinDFA.source tr in
      let label = MinDFA.label tr in
      let visited = Vector.get halting index in
      let visited = IndexSet.diff visited label.filter in
      Vector.set halting index visited;
      vector_set_add transitions index tr;
    end;
    let get_state_for_compaction index =
      let index' = MinDFA.represent_state index in
      let threads = (Vector.get LazyDFA.vector index').threads in
      let registers = Vector.get LazyDFA.registers index' in
      let add_accepting i {NFA. accept; clause; _} acc =
        if not accept then acc else
          let _, (cap, _) = Vector.get NFA.clauses clause in
          let cap_registers = LazyDFA.Threads.get_index registers i in
          let registers =
            let add_reg cap acc =
              let reg = IndexMap.find_opt cap cap_registers in
              if Option.is_none reg then
                MinimizableDFA.partial_captures :=
                  IndexSet.add cap !MinimizableDFA.partial_captures;
              reg :: acc
            in
            Array.of_list (List.rev (IndexSet.fold add_reg cap []))
          in
          (clause, registers) :: acc
      in
      let add_transition tr acc =
        let {MinimizableDFA.Label. filter; captures; clear; moves} = MinDFA.label tr in
        let actions = {
          Lrgrep_support.
          move = IntMap.bindings moves;
          store = List.map snd (IndexMap.bindings captures);
          clear = IntSet.elements clear;
          target = MinDFA.target tr;
        } in
        (filter, actions) :: acc
      in
      {
        Lrgrep_support.
        accept = List.rev (LazyDFA.Threads.fold add_accepting threads []);
        halting = Vector.get halting index;
        transitions =
          IndexSet.fold add_transition (Vector.get transitions index) [];
      }
    in
    LazyDFA.register_count,
    Index.to_int MinDFA.initials.(0),
    let program = Lrgrep_support.compact MinDFA.states get_state_for_compaction in
    Option.iter output_code oc;
    program
  )

end


let output_table oc entry (registers, initial, (program, table, remap)) =
  let print fmt = Printf.fprintf oc fmt in
  print "module Table_%s : Lrgrep_runtime.Parse_errors = struct\n"
    entry.Syntax.name;
  print "  let registers = %d\n" registers;
  print "  let initial = %d\n" remap.(initial);
  print "  let table = %S\n" table;
  print "  let program = %S\n" program;
  print "end\n"

let () = (
  (*if !verbose then (
    let doc = Cmon.list_map Regexp.K.cmon kst.direct in
    Format.eprintf "%a\n%!" Cmon.format (Syntax.print_entrypoints entry);
    Format.eprintf "%a\n%!" Cmon.format doc;
    );*)
  let oc = Option.map open_out_bin !output_name in

  oc |> Option.iter (fun oc -> output_string oc (snd lexer_definition.header));

  List.iter (fun entry ->
      let program = Automata.process_entry oc entry in
      Option.iter (fun oc -> output_table oc entry program) oc
    ) lexer_definition.entrypoints;

  oc |> Option.iter (fun oc ->
      output_char oc '\n';
      output_string oc (snd lexer_definition.trailer);
    );

  Option.iter close_out oc;
  (* Print matching functions *)
)
