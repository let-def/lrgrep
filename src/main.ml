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
module Capture = Mid.Regexp.Capture
module Regexp = Mid.Regexp.Make(Info)()
module Transl = Mid.Transl.Make(Regexp)
open Front

let parser_module =
  String.capitalize_ascii (Filename.basename Grammar.Grammar.basename)

module Automata = struct
  open Info
  open Regexp

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
          (fun index ->
             let clause = Vector.get vector index in
             match clause.action with
             | Syntax.Total _ -> clause.lookaheads = []
             | Syntax.Unreachable -> true
             | Syntax.Partial _ -> false
          )
    end

    module NFA = struct
      type t = {
        (* State construction *)
        uid: int;
        k: K.t;
        transitions: (K.label * t lazy_t) list;
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
      open IndexBuffer

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

      module State = Gen.Make()
      type n = State.n
      let n = State.n

      type t = {
        index: n index;
        threads: threads;
        transitions: (Lr1.set * (thread_mapping * n index) lazy_t) list;
        mutable visited: Lr1.set;
        mutable scheduled: Lr1.set;
      }

      let states = State.get_generator ()

      let determinize threads =
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
            let reservation = Gen.reserve states in
            let index = Gen.index reservation in
            Gen.commit states reservation {
                  index; threads; transitions;
                  scheduled=IndexSet.empty; visited=IndexSet.empty
                };
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
          let t = Gen.get states i in
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

      let states = Gen.freeze states

      let iter_transitions i f =
        let t = Vector.get states i in
        List.iter (fun (label, target) ->
            if Lazy.is_val target then (
              let lazy (mapping, t') = target in
              f label mapping t'
            )
          ) t.transitions

      let iter_refined_transitions i f =
        let t = Vector.get states i in
        let set = t.visited in
        List.iter (fun (label, target) ->
            if Lazy.is_val target then (
              let lazy (mapping, t') = target in
              f (IndexSet.inter set label) mapping t'
            )
          ) t.transitions

      let liveness =
        let reserve t = Array.make (Threads.count t.threads) IndexSet.empty in
        Vector.map reserve states

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
        Index.iter (Vector.length states) process;
        let rec loop () =
          match !todo with
          | [] -> ()
          | todo' ->
            todo := [];
            List.iter process todo';
            loop ()
        in
        loop ()

      let registers = Vector.make (Vector.length states) [||]

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
        let zero = Index.of_int (Vector.length states) 0 in
        Vector.set registers zero
          (Array.make (Threads.count (Vector.get states zero).threads) IndexMap.empty);
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

    let () =
      if false then
      let state = ref LazyDFA.initial in
      let rec loop () =
        let threads = (Vector.get LazyDFA.states !state).threads in
        LazyDFA.Threads.fold (fun _ nfa () ->
            Format.eprintf "- clause %d\n%!%a\n%!"
              (Index.to_int nfa.NFA.clause)
              Cmon.format (K.cmon nfa.NFA.k)
          ) threads ();
        Printf.eprintf "state %d\n> %!" (Index.to_int !state);
        let line = input_line stdin in
        let lr1 = Index.of_int Lr1.n (int_of_string line) in
        Printf.eprintf "input: %s\n" (Lr1.to_string lr1);
        (*let red = Vector.get Regexp.Redgraph.initial lr1 in
        Printf.eprintf "no transition (but %d inner and %d outer reductions)\n"
          (List.length red.inner)
          (List.length (List.filter (fun x -> IndexSet.mem lr1 (List.hd x.Regexp.Redgraph.candidates).filter) red.outer));*)
        let target = ref None in
        let check_transition filter _ target' =
          if IndexSet.mem lr1 filter then
            target := Some target'
        in
        LazyDFA.iter_refined_transitions !state check_transition;
        begin match !target with
          | None -> Printf.eprintf "no transition\n"
          | Some target -> state := target
        end;
        loop ()
      in
      loop ()

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
        open IndexBuffer

        type t = {
          source: states index;
          target: states index;
          label: Label.t;
        }
        include Gen.Make()

        let all = get_generator ()

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
          ignore (Gen.add all {source; target; label})

        let all = Gen.freeze all
      end

      type transitions = Transition.n
      let transitions = Transition.n

      let label i = (Vector.get Transition.all i).label
      let source i = (Vector.get Transition.all i).source
      let target i = (Vector.get Transition.all i).target

      let initials f = f LazyDFA.initial
      let finals f =
        Index.iter states
          (fun st ->
             if LazyDFA.Threads.fold
                 (fun _ nfa acc -> acc || nfa.NFA.accept)
                 (Vector.get LazyDFA.states st).threads false
             then f st)

      let refinements refine =
        (* Refine states by accepted actions *)
        let acc = ref [] in
        Index.iter states (fun st ->
            let accepted =
              LazyDFA.Threads.fold
                (fun _ nfa acc -> if nfa.NFA.accept then IndexSet.add nfa.NFA.clause acc else acc)
                (Vector.get LazyDFA.states st).threads IndexSet.empty
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
              match Info.Symbol.prj (Transl.Indices.get_symbol pos (Syntax.Name sym)) with
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
    Printf.eprintf "DFA states: %d\n" (cardinal (Vector.length LazyDFA.states));
    Printf.eprintf "Minimized DFA states: %d\n" (cardinal MinDFA.states);
    Printf.eprintf "Time spent: %.02fms\n" (Sys.time () *. 1000.);
    let transitions = Vector.make MinDFA.states IndexSet.empty in
    let halting = Vector.make MinDFA.states IndexSet.empty in
    Vector.iteri (fun source (def : LazyDFA.t) ->
        match MinDFA.transport_state source with
        | None -> ()
        | Some index ->
          let visited = Vector.get halting index in
          let visited = IndexSet.union def.visited visited in
          Vector.set halting index visited
      ) LazyDFA.states;
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
      let threads = (Vector.get LazyDFA.states index').threads in
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
