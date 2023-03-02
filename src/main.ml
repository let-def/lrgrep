open Utils
open Misc

module StringSet = Set.Make(String)

(* Command-line parsing. *)

let source_name = ref None
let output_name = ref None
let grammar_file = ref None
let check_coverage = ref false

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
  "-v", Arg.Unit print_version_string,
  " Print version and exit";
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
      K.derive
        ~accept:(fun label clause ->
            assert (Index.to_int clause = 0);
            assert (IndexSet.is_empty label.vars);
            if node == lr1_trie_root then
              immediate := IndexSet.union !immediate label.filter
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
          )
        ~direct:(fun label k' ->
            IndexMap.iter (fun lr1 node' ->
                if IndexSet.mem lr1 label.filter then
                  step node' k'
              ) node.sub
          )
        k
    in
    step lr1_trie_root (K.More (re, K.Done (Clause.of_int 0)));
    (!reached, !immediate)


  let transl ~capture ~ordering ~for_reduction re =
    let all_ord = ref IndexSet.empty in
    let all_cap = ref IndexSet.empty in
    let mk_ordering kind =
      let negative = match kind with
        | Shortest -> true
        | Longest  -> false
      in
      let index = ordering ~negative in
      all_ord := IndexSet.add index !all_ord;
      IndexSet.singleton index
    in
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
            | Some name -> mk_capture `Value name
          in
          RE.Set (set, cap)
        | Alternative res ->
          RE.Alt (List.map (transl ~for_reduction) res)
        | Repetition {expr; kind} ->
          RE.Star (transl ~for_reduction expr,
                   mk_ordering kind)
        | Reduce {capture; kind; expr} ->
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
              let capture0 = mk_capture `Start_loc name in
              let capture = mk_capture `End_loc name in
              (capture0, capture)
          in
          let ordering = mk_ordering kind in
          let r = RE.Reduce (capture0, {capture; pattern; ordering}) in
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
    (!all_ord, !all_cap, result)


end

(*module Clause = struct
  open Info
  open Front.Syntax
  open Regexp
  end*)

open Front

let parser_module =
  String.capitalize_ascii (Filename.basename Grammar.Grammar.basename)

module Nfa = struct
  open Info
  module K = Regexp.K
  module KMap = Map.Make(K)
  module KSet = Set.Make(K)
  module KSetMap = Map.Make(KSet)

  type label = Regexp.K.label

  type state = {
    (* State construction *)
    k: K.t;
    accept: Clause.set;
    transitions: (label * state lazy_t) list;

    (* NFA construction *)
    mutable visited: Lr1.set;
    mutable scheduled: Lr1.set;
  }

  let is_immediate_label {Regexp.K. filter; vars; ordering} =
    IndexSet.equal filter Lr1.all &&
    IndexSet.is_empty vars &&
    IndexSet.is_empty ordering

  let merge_labels (l1 : label) (l2 : label) : label = {
    filter   = IndexSet.union l1.filter   l2.filter;
    vars     = IndexSet.union l1.vars     l2.vars;
    ordering = IndexSet.union l1.ordering l2.ordering;
  }

  let get_transitions st =
    let set' = Lr1.set_predecessors st.visited in
    st.transitions
    |> List.filter_map (fun ((label : label), st') ->
        if Lazy.is_val st' then
          let filter = Lr1.intersect label.filter set' in
          Some ({label with filter}, Lazy.force st')
        else
          None
      )

  module Entry (E : sig val entry : Syntax.entry end)() = struct
    open Fix.Indexing

    module NFA = struct
      let get_state () =
        let nfa = ref KMap.empty in
        let rec aux k =
          match KMap.find_opt k !nfa with
          | Some t -> t
          | None ->
            let accepted = ref IndexSet.empty in
            let transitions = ref KMap.empty in
            let direct label k' =
              match KMap.find_opt k' !transitions with
              | Some label' -> label' := merge_labels label !label'
              | None -> transitions := KMap.add k' (ref label) !transitions
            in
            let accept label x =
              if is_immediate_label label then
                accepted := IndexSet.add x !accepted
              else
                direct label (Regexp.K.Done x)
            in
            Regexp.K.derive ~accept ~direct k;
            let accept = !accepted in
            let transitions =
              KMap.fold
                (fun k' label acc -> (!label, lazy (aux k')) :: acc)
                !transitions []
            in
            let t = {
              k; accept; transitions;
              visited = IndexSet.empty;
              scheduled = IndexSet.empty;
            } in
            nfa := KMap.add k t !nfa;
            t
        in
        nfa, aux

      let force_lr1_stacks initial =
        let scheduled = ref [] in
        let schedule st lr1s =
          let lr1s = IndexSet.diff lr1s st.visited in
          if not (IndexSet.is_empty lr1s) then (
            if IndexSet.is_empty st.scheduled then
              push scheduled st;
            st.scheduled <- IndexSet.union st.scheduled lr1s
          )
        in
        let process st =
          let states = Lr1.set_predecessors st.scheduled in
          st.visited <- IndexSet.union st.scheduled st.visited;
          st.scheduled <- IndexSet.empty;
          List.iter (fun (label, st) ->
              let states' = IndexSet.inter states label.Regexp.K.filter in
              if not (IndexSet.is_empty states') then
                schedule (Lazy.force st) states'
            ) st.transitions
        in
        let rec flush () =
          match !scheduled with
          | [] -> ()
          | todo ->
            scheduled := [];
            List.iter process todo;
            flush ()
        in
        schedule initial Lr1.idle;
        flush ()

      let process_clause ~capture ~ordering i (clause : Syntax.clause) =
        let all_cap, all_ord, re =
          Transl.transl ~capture ~ordering ~for_reduction:false clause.pattern
        in
        let nfa, get_state = get_state () in
        let initial = get_state Regexp.K.(More (re, Done (Clause.of_int i))) in
        force_lr1_stacks initial;
        (all_cap, all_ord, !nfa, initial)

      let initial =
        let ordering = Ordering.gensym () in
        let capture = Var.gensym () in
        let captures_spec = ref IndexMap.empty in
        let capture kind name =
          let index = capture () in
          captures_spec := IndexMap.add index (kind, name) !captures_spec;
          index
        in
        let result = List.mapi (process_clause ~capture ~ordering) E.entry.clauses in
        result

      let total =
        let result = ref IndexSet.empty in
        List.iteri (fun i (clause : Syntax.clause) ->
            let is_total =
              match clause.action with
              | Syntax.Total _ -> true
              | Syntax.Unreachable -> true
              | Syntax.Partial _ -> false
            in
            if is_total then
              result := IndexSet.add (Clause.of_int i) !result
          ) E.entry.clauses;
        !result
    end

    module DFA = struct
      module State = struct
        type 'st t = {
          k: KSet.t;
          accept: Clause.set;
          visited: Lr1.set;
          mutable fwd: (label * 'st index) list;
          mutable bkd: (label * 'st index) list;
        }
        include IndexBuffer.Gen(struct type nonrec 'st t = 'st t end)()
      end

      let map = ref KSetMap.empty

      let rec determinize states =
        let k = List.fold_left (fun k st -> KSet.add st.k k) KSet.empty states in
        match KSetMap.find_opt k !map with
        | Some index -> index
        | None ->
          let visited =
            List.fold_left (fun acc st -> Lr1.intersect acc st.visited)
              Lr1.all states
          in
          let accept =
            List.fold_left (fun acc st -> IndexSet.union st.accept acc)
              IndexSet.empty states
          in
          let source_state = {State. k; accept; visited; fwd = []; bkd = []} in
          let source = State.add source_state in
          map := KSetMap.add k source !map;
          let prepare_for_partition ((l : label), st') = (l.filter, (l, st')) in
          let add_back_transition (filter, labelled_states) =
            let label : label = {
              filter;
              vars = List.fold_left
                  (fun acc ((l' : label), _) -> IndexSet.union acc l'.vars)
                  IndexSet.empty labelled_states;
              ordering = List.fold_left
                  (fun acc ((l' : label), _) -> IndexSet.union acc l'.ordering)
                  IndexSet.empty labelled_states;
            } in
            let states = List.map snd labelled_states in
            let target = determinize states in
            let target_state = State.get target in
            target_state.bkd <- (label, source) :: target_state.bkd;
            source_state.fwd <- (label, target) :: source_state.fwd;
          in
          states
          |> List.concat_map
            (fun st -> List.map prepare_for_partition (get_transitions st))
          |> IndexRefine.annotated_partition
          |> List.iter add_back_transition;
          source

      let initial = determinize (List.map (fun (_,_,_,t) -> t ) NFA.initial)
    end

    module Analysis = struct
      (* The DFA is initialized with backward transitions. Dataflow analysis:

         1) Reachability

         Determine which actions are reachable from each path
         - start from accepting states, backpropagate accepted states
         - don't propagate if an accepting state with higher priority has been reached
         - find fix point

         Remove bkd transitions which source is a total accepting state with
         higher priority.

         2) Captures
         Find which capture are total and the set of transitions that trigger them

         find total captures: backpropagate captured values for each clause

         3) Ordering

         4) Register allocation

      *)
    end

    module MinimizableDFA = struct
      open DFA

      type states = State.n
      let states = State.n

      module Transition = struct
        type t = {
          source: State.n index;
          target: State.n index;
          label: label;
        }
        include IndexBuffer.Gen(struct type nonrec _ t = t end)()

        let () =
          KSetMap.iter (fun _ source ->
              List.iter (fun (label, target) ->
                  ignore (add {source; target; label})
                ) (DFA.State.get source).fwd
            ) !DFA.map
      end

      type transitions = Transition.n
      let transitions = Transition.n

      let label i = (Transition.get i).label
      let source i = (Transition.get i).source
      let target i = (Transition.get i).target

      let initials f = f DFA.initial
      let finals f =
        Index.iter states
          (fun st -> if not (IndexSet.is_empty (State.get st).accept) then f st)

      let refinements refine =
        (* Refine states by accepted actions *)
        begin
          let acc = ref [] in
          Index.iter states (fun st ->
              if not (IndexSet.is_empty (State.get st).accept) then
                push acc st
            );
          Misc.group_by !acc
            ~compare:(fun st1 st2 ->
                IndexSet.compare (State.get st1).accept (State.get st2).accept)
            ~group:(fun st sts -> st :: sts)
          |> List.iter (fun group -> refine (fun ~add -> List.iter add group))
        end
        (* Refine states by the lr1 set that can reach them *)
        (*begin
            let module Lr1Set = struct
              type t = Lr1.set
              let compare = IndexSet.compare
            end in
            let module SetMap = Map.Make(Lr1Set) in
            let acc = ref SetMap.empty in
            Index.iter states (fun st ->
                acc := SetMap.update (State.get st).visited (function
                    | None -> Some [st]
                    | Some xs -> Some (st :: xs)
                  ) !acc
              );
            SetMap.iter (fun _ xs -> refine (fun ~add -> List.iter add xs)) !acc
          end*)
    end

    module MinDFA = Valmari.Minimize (struct
        type t = Regexp.K.label
        let compare = Regexp.K.compare_label
      end) (MinimizableDFA)
  end

  let process_entry _oc (entry : Syntax.entry) =
    let open Fix.Indexing in
    let open Entry(struct let entry = entry end)() in
    Printf.eprintf "DFA states: %d\n" (KSetMap.cardinal !DFA.map);
    let label_domain = Vector.make MinDFA.states IndexSet.empty in
    Index.iter MinimizableDFA.states (fun st ->
        match MinDFA.transport_state st with
        | None -> ()
        | Some st' ->
          let domain = Vector.get label_domain st' in
          let domain = IndexSet.union domain (DFA.State.get st).visited in
          Vector.set label_domain st' domain
      );
    Printf.eprintf "Minimized DFA states: %d\n" (cardinal MinDFA.states)
    (*List.iter2 (fun init (clause : Syntax.clause) ->
        if not init.reach_accept then
          Printf.eprintf "Clause line %d is unreachable\n"
            clause.pattern.position.line;
      ) initial_states entry.clauses;*)
end


let () = (
  (*let doc = Cmon.list_map (KRE.cmon ()) kst.direct in
  if verbose then (
    Format.eprintf "%a\n%!" Cmon.format (Syntax.print_entrypoints entry);
    Format.eprintf "%a\n%!" Cmon.format doc;
  );*)
  (*let oc = match !output_name with
    | None -> None
    | Some path ->
      let oc = open_out_bin path in
      output_string oc (snd lexer_definition.header);
      Some oc
    in*)
  let oc = None in
  prerr_newline ();
  List.iter (Nfa.process_entry oc) lexer_definition.entrypoints;
  (*begin match oc with
    | None -> ()
    | Some oc ->
      output_char oc '\n';
      output_string oc (snd lexer_definition.trailer);
      close_out oc
    end;*)
  (* Print matching functions *)
)
