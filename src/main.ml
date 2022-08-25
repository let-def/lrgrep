(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Utils.Misc

(* Command-line parsing. *)

let source_name = ref None
let output_name = ref None
let grammar_file = ref None

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
    | Parser.Error ->
      let p = Lexing.lexeme_start_p lexbuf in
      Printf.fprintf stderr
        "File \"%s\", line %d, character %d: syntax error.\n"
        p.Lexing.pos_fname p.Lexing.pos_lnum
        (p.Lexing.pos_cnum - p.Lexing.pos_bol)
    | Lexer.Lexical_error {msg; file; line; col} ->
      Printf.fprintf stderr
        "File \"%s\", line %d, character %d: %s.\n"
        file line col msg
    | _ -> Printexc.raise_with_backtrace exn bt
  end;
  exit 3

let lexer_definition =
  let ic = open_in_bin source_file in
  Lexer.ic := Some ic;
  let lexbuf = Lexing.from_channel ~with_positions:true ic in
  Lexing.set_filename lexbuf source_file;
  let result =
    try Parser.lexer_definition Lexer.main lexbuf
    with exn -> print_parse_error_and_exit lexbuf exn
  in
  Lexer.ic := None;
  result

module Grammar = MenhirSdk.Cmly_read.Read(struct let filename = grammar_file end)

module Info = Grammar_info.Make(Grammar)

module Regexp = Regexp.Make(Info)

module Dfa = Dfa.Make(Regexp)()

(*let rec interp_kre kres reds stack =
  let visited = ref KRESet.empty in
  let reached = ref [] and direct = ref [] and reduce = ref [] in
  let kderive kre = KRESet.prederive ~visited ~reached ~direct ~reduce kre in
  KRESet.iter kderive kres;
  let reduce = KRESet.of_list !reduce in
  eprintf "------------------------\n";
  eprintf "Matcher definition:\n%a\n" print_cmon (KRESet.cmon kres);
  eprintf "Ongoing reductions: [%s]\n"
    (string_concat_map ";" string_of_index (IndexSet.elements reds));
  eprintf "Matching actions: [%s]\n"
    (string_concat_map ";" string_of_int !reached);
  eprintf "New reductions:\n%a\n" print_cmon (KRESet.cmon reduce);
  match stack with
  | [] -> eprintf "End of stack\n"
  | x :: xs -> (
    let lr1 = Index.of_int Lr1.n x in
    eprintf "Parser in state %d - %s\n" x (Lr1.to_string lr1);
    let reds =
      if KRESet.is_empty reduce
      then reds
      else redmap_add reds (Redgraph.State.of_lr1 lr1) reduce
    in
    let step_kre acc (sg, kre') =
      if IndexSet.mem lr1 sg
      then KRESet.add kre' acc
      else acc
    in
    let kres = List.fold_left step_kre KRESet.empty !direct in
    interp_kre kres reds xs
  )*)

let gen_code entry oc vars clauses =
  let print fmt = Printf.fprintf oc fmt in
  print
    "let execute_%s %s : int * %s.MenhirInterpreter.element option array -> _ option = function\n"
    entry.Syntax.name
    (String.concat " " entry.Syntax.args)
    (String.capitalize_ascii (Filename.basename Grammar.Grammar.basename));
  List.iteri (fun index (vars, clause) ->
      print "  | %d, [|%s|] -> begin\n%s\n    end\n"
        index
        (String.concat ";" vars)
        (match clause.Syntax.action with
         | Unreachable -> "failwith \"Should be unreachable\""
         | Partial (_, str) -> str
         | Total (_, str) -> "Some (" ^ str ^ ")")
    ) (List.combine vars clauses);
  print "  | _ -> failwith \"Invalid action\"\n\n"

let output_table oc entry vars (initial : Dfa.Repr.t) (program, table, remap) =
  let print fmt = Printf.fprintf oc fmt in
  print "module Table_%s : Lrgrep_runtime.Parse_errors = struct\n"
    entry.Syntax.name;
  print "  let arities = [|%s|]\n"
    (string_concat_map ";" (fun a -> string_of_int (List.length a)) vars);
  print "  let initial = %d\n" remap.(initial.id);
  print "  let table = %S\n" table;
  print "  let program = %S\n" program;
  print "end\n"

module Coverage = struct
  open Fix.Indexing
  open Utils
  open BitSet
  open Info

  module LRijkstra = LRijkstraFast.Make(Info)()

  module Lrc = struct
    let index_shift n i offset =
      Index.of_int n ((i : _ index :> int) + offset)

    let index_delta (type n) (i : n index) (j : n index) =
      (i :> int) - (j :> int)

    include Const(struct
        let cardinal =
          let count lr1 = Array.length (LRijkstra.Classes.for_lr1 lr1) in
          let sum = ref 0 in
          Index.iter Lr1.n (fun lr1 -> sum := !sum + count lr1);
          !sum
      end)

    let () =
      Printf.eprintf "%d lr1 states, %d lrc states\n"
        (cardinal Lr1.n) (cardinal n)

    type t = n index
    type set = n indexset
    type 'a map = (n, 'a) indexmap
    type 'a vector = (n, 'a) Vector.t

    let lr1_of_lrc, lrcs_of_lr1, first_lrc_of_lr1 =
      let lr1_of_lrc = Vector.make' n (fun () -> Index.of_int Lr1.n 0) in
      let count = ref 0 in
      let init_lr1 lr1 =
        let classes = LRijkstra.Classes.for_lr1 lr1 in
        assert (Array.length classes > 0);
        let first = Index.of_int n !count in
        let all = ref IndexSet.empty in
        count := !count + Array.length classes;
        for i = Array.length classes - 1 downto 0 do
          let lrc = index_shift n first i in
          all := IndexSet.add lrc !all;
          Vector.set lr1_of_lrc lrc lr1
        done;
        !all
      in
      let lrcs_of_lr1 = Vector.init Lr1.n init_lr1 in
      (Vector.get lr1_of_lrc,
       Vector.get lrcs_of_lr1,
       (fun lr1 -> Option.get (IndexSet.minimum (Vector.get lrcs_of_lr1 lr1)))
      )

    let offering_states =
      let all = ref IndexSet.empty in
      Index.iter Lr1.n (fun lr1 ->
          match Lr1.incoming lr1 with
          | Some (N _) -> ()
          | Some (T t) when
              Grammar.Terminal.kind (Terminal.to_g t) = `EOF -> ()
          | None | Some (T _) ->
            assert (Array.length (LRijkstra.Classes.for_lr1 lr1) = 1);
            all := IndexSet.add (first_lrc_of_lr1 lr1) !all
        );
      !all

    let predecessors =
      let predecessors = Vector.make n IndexSet.empty in
      let t0 = Sys.time () in
      let interval n i j =
        let rec loop i j acc =
          if j >= i
          then loop i (j - 1) (IndexSet.add (Index.of_int n j) acc)
          else acc
        in
        loop (Index.to_int i) (Index.to_int j) IndexSet.empty
      in
      let process lr1 =
        let first_lrc = first_lrc_of_lr1 lr1 in
        match Lr1.incoming lr1 with
        | None ->
          Vector.set predecessors first_lrc @@
          IndexSet.empty
        | Some (Symbol.T _) ->
          Vector.set predecessors first_lrc @@
          List.fold_left (fun acc tr ->
              let src = Transition.source tr in
              let lrc_first = first_lrc_of_lr1 src in
              let count = Array.length (LRijkstra.Classes.for_lr1 src) in
              let lrc_last = index_shift n lrc_first (count - 1) in
              IndexSet.union acc (interval n lrc_first lrc_last)
            ) IndexSet.empty (Transition.predecessors lr1)
        | Some (Symbol.N _) ->
          let process_transition tr =
            let source_lrc = first_lrc_of_lr1 (Transition.source tr) in
            let node = LRijkstra.Tree.leaf tr in
            let table = Vector.get LRijkstra.Cells.table node in
            let pre_classes = LRijkstra.Classes.pre_transition tr in
            let post_classes = LRijkstra.Classes.post_transition tr in
            let coercion =
              LRijkstra.Coercion.infix post_classes (LRijkstra.Classes.for_lr1 lr1)
            in
            let pre_classes = Array.length pre_classes in
            let post_classes = Array.length post_classes in
            for post = 0 to post_classes - 1 do
              let reachable = ref IndexSet.empty in
              for pre = 0 to pre_classes - 1 do
                let index = LRijkstra.Cells.table_index ~post_classes ~pre ~post in
                if table.(index) < max_int then (
                  let source_lrc = Index.of_int n ((source_lrc :> int) + pre) in
                  reachable := IndexSet.add source_lrc !reachable
                )
              done;
              let reachable = !reachable in
              Array.iter (fun index ->
                  let target_lrc = Index.of_int n ((first_lrc :> int) + index) in
                  Vector.set predecessors target_lrc reachable
                ) coercion.forward.(post)
            done
          in
          List.iter process_transition (Transition.predecessors lr1)
      in
      Index.iter Lr1.n process;
      Printf.eprintf "computed Lrc predecessors in %.02fms\n"
        ((Sys.time () -. t0) *. 1000.0);
      Vector.get predecessors

    let t0 = Sys.time ()

    let predecessors_by_lr1 =
      vector_tabulate n begin fun lrc ->
        let all = predecessors lrc in
        IndexSet.fold (fun lr1 acc ->
            let preds = IndexSet.inter (lrcs_of_lr1 lr1) all in
            cons_if (not (IndexSet.is_empty preds)) preds acc
          ) (Lr1.predecessors (lr1_of_lrc lrc)) []
      end

    let () =
      Printf.eprintf "classified Lrc predecessors in %.02fms\n"
        ((Sys.time () -. t0) *. 1000.0)

    let lookahead lrc =
      let lr1 = lr1_of_lrc lrc in
      let classe = index_delta lrc (first_lrc_of_lr1 lr1) in
      (LRijkstra.Classes.for_lr1 lr1).(classe)

    let decompose lrc =
      let lr1 = lr1_of_lrc lrc in
      let classe = index_delta lrc (first_lrc_of_lr1 lr1) in
      (lr1, (LRijkstra.Classes.for_lr1 lr1).(classe))
  end

  module Lrce = struct

    type lazy_stream = Scons of Lrc.set * lazy_stream lazy_t

    let rec predecessor_stream lrcs = Scons (
        lrcs,
        lazy (predecessor_stream (indexset_bind lrcs Lrc.predecessors))
      )

    let rec stream_nth (Scons (v, vs)) = function
      | 0 -> v
      | n -> stream_nth (Lazy.force vs) (n - 1)

    let reached_from = Vector.make Lrc.n IndexSet.empty

    let print_item (prod, dot) =
      let rhs = Production.rhs prod in
      let symbols = ref [] in
      for i = Array.length rhs - 1 downto 0 do
        push symbols (Symbol.name rhs.(i));
        if i = dot then push symbols ".";
      done;
      String.concat " " !symbols

    let lrc_goto nt lrc =
      let lr1 = Lrc.lr1_of_lrc lrc in
      try Transition.find_goto_target lr1 nt
      with Not_found ->
        Printf.eprintf "Failed to go non-terminal nt %s from state %s, with items:\n%s\n"
          (Nonterminal.to_string nt)
          (Lr1.to_string lr1)
          (string_concat_map "\n" print_item (Lr1.items lr1));
        exit 1

    let check_target source ts target =
      let ts' = Lrc.lookahead target in
      let ts' = IndexSet.diff ts' (Lr1.reject (Lrc.lr1_of_lrc target)) in
      if IndexSet.subset ts' ts then
        vector_set_add reached_from target source
      else if not (IndexSet.disjoint ts ts') then (
        Printf.eprintf "EXPECTED RELATED SETS\n\
                        INCOMPATIBLE ON %s\n\
                        INTERSECT ON %s\n"
          (string_of_indexset ~string_of_index:Terminal.to_string
             (IndexSet.diff ts' ts))
          (string_of_indexset ~string_of_index:Terminal.to_string
             (IndexSet.inter ts' ts))
      )

    let check_reductions lrc =
      let (lr1, ts) = Lrc.decompose lrc in
      let preds = predecessor_stream (IndexSet.singleton lrc) in
      let check_reduction (prod, ts') =
        if Production.kind prod = `REGULAR then
          let ts = IndexSet.inter ts ts' in
          if not (IndexSet.is_empty ts) then (
            let preds = stream_nth preds (Array.length (Production.rhs prod)) in
            let targets = IndexSet.map (lrc_goto (Production.lhs prod)) preds in
            let check_target = check_target lrc ts in
            IndexSet.iter
              (fun target -> IndexSet.iter check_target (Lrc.lrcs_of_lr1 target))
              targets
          )
      in
      List.iter check_reduction (Lr1.reductions lr1)

    let () = Index.iter Lrc.n check_reductions

    let filtered_subset n f =
      let set = ref IndexSet.empty in
      for i = cardinal n - 1 downto 0 do
        let i = Index.of_int n i in
        if f i then set := IndexSet.add i !set
      done;
      !set

    let fail_states =
      filtered_subset Lrc.n
        (fun lrc ->
           let lr1, la = Lrc.decompose lrc in
           not (IndexSet.disjoint la (Lr1.reject lr1)))

    let t0 = Sys.time ()

    let can_fail_states =
      let rec loop states =
        let states' = indexset_bind states (Vector.get reached_from) in
        if IndexSet.equal states states' then
          states
        else
          loop states'
      in
      loop fail_states

    let failing_offering_states =
      IndexSet.inter Lrc.offering_states can_fail_states

    let () = Printf.eprintf "computed can fail %.02fms\n"
        ((Sys.time () -. t0) *. 1000.)
    module Extra = Gensym()

    type lr1_paths = Lr1_paths of {
        goto: Terminal.set Lr1.map;
        pops: lr1_paths Lr1.map;
      }

    let t0 = Sys.time ()

    let intermediate_lr1_steps lr1 =
      let rec process n lr1 = function
        | [] ->
          Lr1_paths {goto = IndexMap.empty; pops = IndexMap.empty}

        | (n', nt, ts) :: reds when n = n' ->
          assert (not (IndexSet.is_empty ts));
          let paths = process n lr1 reds in
          begin match Nonterminal.kind nt with
            | `START -> paths
            | `REGULAR ->
              let Lr1_paths paths = paths in
              let target = Transition.find_goto_target lr1 nt in
              let goto =
                IndexMap.update target (function
                    | None -> Some ts
                    | Some ts' -> Some (IndexSet.union ts ts')
                  ) paths.goto
              in
              Lr1_paths {paths with goto}
          end

        | ((n', _, _) :: _) as reds  ->
          assert (n' > n);
          let preds = Lr1.predecessors lr1 in
          let process_predecessor lr1' acc =
            IndexMap.add lr1' (process (n + 1) lr1' reds) acc
          in
          let pops = IndexSet.fold process_predecessor preds IndexMap.empty in
          Lr1_paths {goto = IndexMap.empty; pops}
      in
      process (-1) lr1 (Lr1.closed_reductions lr1)

    let () = Index.iter Lr1.n (fun lr1 -> ignore (intermediate_lr1_steps lr1))
    let () = Printf.eprintf "computed intermediate steps in %.02fms\n"
        ((Sys.time () -. t0) *. 1000.)

    type ('n, 'goto) paths =
      | Fail
      | Continue of {
          goto: 'goto;
          pops: ('n, ('n, 'goto) paths) indexmap;
        }

    let empty_lr1 = Continue {goto = IndexMap.empty; pops = IndexMap.empty}

    let empty = Continue {goto = IndexSet.empty; pops = IndexMap.empty}

    let is_empty x = x == empty || match x with
      | Continue {goto; pops} ->
        IndexSet.is_empty goto && IndexMap.is_empty pops
      | Fail -> false

    let rec merge p1 p2 =
      match p1, p2 with
      | Fail, _ | _, Fail -> Fail
      | Continue c1, Continue c2 ->
        if is_empty p1 then p2
        else if is_empty p2 then p1
        else
          let sub_merge _ p1' p2' = Some (merge p1' p2') in
          Continue {
            goto = IndexSet.union c1.goto c2.goto;
            pops = IndexMap.union sub_merge c1.pops c2.pops;
          }


    let intermediate_steps =
      vector_tabulate Lrc.n @@ fun lrc ->
      if IndexSet.mem lrc fail_states then (
        Fail
      ) else (
        let lr1, la = Lrc.decompose lrc in
        let reduction_paths (prod, ts) =
          let ts = IndexSet.inter la ts in
          if IndexSet.is_empty ts
          then empty
          else
            let exception Can_fail in
            let rec simulate_prod lrc = function
              | 0 ->
                (* Consumed all producers, follow goto transitions *)
                let lr1 = Lrc.lr1_of_lrc lrc in
                let lr1' = Transition.find_goto_target lr1 (Production.lhs prod) in
                let reachability_pred lrc' =
                  if IndexSet.disjoint ts (Lrc.lookahead lrc')
                  then false
                  else if IndexSet.disjoint ts (Lr1.reject (Lrc.lr1_of_lrc lrc'))
                  then true
                  else raise Can_fail
                in
                begin match
                    IndexSet.filter reachability_pred (Lrc.lrcs_of_lr1 lr1')
                  with
                  | exception Can_fail -> Fail
                  | goto -> Continue {goto; pops = IndexMap.empty}
                end
              | n ->
                (* Follow each predecessor *)
                IndexSet.fold
                  (fun predecessor acc ->
                     let paths = simulate_prod predecessor (n - 1) in
                     if is_empty paths
                     then acc
                     else
                       let pops = IndexMap.singleton predecessor paths in
                       merge acc (Continue {goto = IndexSet.empty; pops})
                  ) (Lrc.predecessors lrc) empty
            in
            simulate_prod lrc (Array.length (Production.rhs prod))
        in
        List.fold_left
          (fun acc prod -> merge acc (reduction_paths prod))
          empty (Lr1.reductions lr1)
      )

    (*let intermediate_steps =
      vector_tabulate Lrc.n @@ fun lrc ->
      if IndexSet.mem lrc fail_states then (
        Fail
      ) else (
        let lr1, la = Lrc.decompose lrc in
        let reduction_paths (prod, ts) =
          let ts = IndexSet.inter la ts in
          if IndexSet.is_empty ts
          then empty
          else
            (*let exception Can_fail in*)
            let rec simulate_prod lr1 = function
              | 0 ->
                (* Consumed all producers, follow goto transitions *)
                let lr1' = Transition.find_goto_target lr1 (Production.lhs prod) in
                Continue {goto = IndexSet.singleton lr1'; pops = IndexMap.empty}
                (*let reachability_pred lrc' =
                  if IndexSet.disjoint ts (Lrc.lookahead lrc')
                  then false
                  else if IndexSet.disjoint ts (Lr1.reject (Lrc.lr1_of_lrc lrc'))
                  then true
                  else raise Can_fail
                in
                begin match
                    IndexSet.filter reachability_pred (Lrc.lrcs_of_lr1 lr1')
                  with
                  | exception Can_fail -> Fail
                  | goto -> Continue {goto; pops = IndexMap.empty}
                end*)
              | n ->
                (* Follow each predecessor *)
                IndexSet.fold
                  (fun predecessor acc ->
                     let paths = simulate_prod predecessor (n - 1) in
                     if is_empty paths
                     then acc
                     else
                       let pops = IndexMap.singleton predecessor paths in
                       merge acc (Continue {goto = IndexSet.empty; pops})
                  ) (Lr1.predecessors lr1) empty
            in
            simulate_prod lr1 (Array.length (Production.rhs prod))
        in
        List.fold_left
          (fun acc prod -> merge acc (reduction_paths prod))
          empty (Lr1.reductions lr1)
      )*)

    module NFA = Sum(Lrc)(Extra)
  end


  (*module Check_dfa(NFA : sig
      include CARDINAL
      val initials : n indexset
      val label : n index -> Lr1.t
      val transitions : n index -> n indexset
    end) :
  sig
    type t
    type state

    val analyse : Dfa.Repr.t -> t

    val partial_states : t -> state list
    val repr : state -> Dfa.Repr.t
    val unhandled : state -> NFA.n indexset
    val paths : t -> (state * NFA.n index * Lr1.t list) Seq.t
  end = struct
    type state = {
      repr: Dfa.Repr.t;
      transitions: state lazy_t Lr1.map;
      mutable visited: NFA.n indexset;
      mutable scheduled: NFA.n indexset;
      mutable unhandled: NFA.n indexset;
      mutable predecessors: IntSet.t;
    }

    let make_state repr transitions = {
      repr; transitions;
      visited = IndexSet.empty;
      scheduled = IndexSet.empty;
      unhandled = IndexSet.empty;
      predecessors = IntSet.empty;
    }

    (*let indexset_find f set =
      match IndexSet.iter (fun n -> if f n then raise Exit) set with
      | () -> false
      | exception Exit -> true*)

    let make_state_table () =
      let table = Hashtbl.create 7 in
      let rec aux repr =
        match Hashtbl.find_opt table repr.Dfa.Repr.id with
        | Some t -> t
        | None ->
          let trs = ref IndexMap.empty in
          let add_lr1 target lr1 = trs := IndexMap.add lr1 target !trs in
          Dfa.iter_transitions repr (fun lr1s _vars target ->
              let target = lazy (aux target) in
              IndexSet.iter (add_lr1 target) lr1s
            );
          let t = make_state repr !trs in
          Hashtbl.add table repr.Dfa.Repr.id t;
          t
      in
      table, aux

    let make_scheduler () =
      let todo = ref [] in
      let schedule source target states =
        let states = IndexSet.diff states target.visited in
        if not (IndexSet.is_empty states) then (
          if IndexSet.is_empty target.scheduled then push todo target;
          target.scheduled <- IndexSet.union target.scheduled states;
          target.predecessors <- IntSet.add source.repr.id target.predecessors;
        )
      in
      let rec flush f = match List.rev !todo with
        | [] -> ()
        | todo' ->
          todo := [];
          List.iter f todo';
          flush f
      in
      (schedule, flush)

    type t = {
      initial: state;
      partial: state list;
      table: (int, state) Hashtbl.t;
    }

    let analyse initial =
      let table, lift = make_state_table () in
      let schedule, flush = make_scheduler () in
      let partial = ref [] in
      let process_transition state tr =
        let lr1 = NFA.label tr in
        match IndexMap.find_opt lr1 state.transitions with
        | None ->
          if IndexSet.is_empty state.unhandled then
            push partial state;
          state.unhandled <- IndexSet.add tr state.unhandled
        | Some (lazy target) ->
          schedule state target (NFA.transitions tr)
      in
      let process_state state =
        let to_visit = state.scheduled in
        state.visited <- IndexSet.union state.visited to_visit;
        state.scheduled <- IndexSet.empty;
        if IntSet.is_empty state.repr.accepted then
          IndexSet.iter (process_transition state) to_visit
      in
      let initial = lift initial in
      schedule initial initial NFA.initials;
      flush process_state;
      { initial; partial = !partial; table }

    let nfa_predecessors = lazy (
      let predecessors = Vector.make NFA.n IndexSet.empty in
      Index.iter NFA.n (fun nfa ->
          let successors = NFA.transitions nfa in
          IndexSet.iter (fun successor ->
              Vector.set predecessors successor
                (IndexSet.add nfa (Vector.get predecessors successor)))
            successors
        );
      Vector.get predecessors
    )

    let repr st = st.repr
    let unhandled st = st.unhandled
    let partial_states t = t.partial

    type 'a lazy_list =
      | LNil
      | LCons of 'a * 'a lazy_list lazy_t

    let rec lazy_list_to_seq = function
      | LNil -> Seq.Nil
      | LCons (x, xs) ->
        Seq.Cons (x, fun () -> lazy_list_to_seq (Lazy.force xs))

    let paths t =
      let visited = Hashtbl.create 7 in
      let visited st =
        match Hashtbl.find_opt visited st.repr.id with
        | Some set -> set
        | None ->
          let set = ref IndexSet.empty in
          Hashtbl.add visited st.repr.id set;
          set
      in
      let found = ref [] in
      let candidates = ref [] in
      let add_candidate (path, st0, nfa0, finished) st nfa =
        if not !finished then (
          let path = NFA.label nfa :: path in
          if st == t.initial then (
            push found (st0, nfa0, path);
            finished := true;
          ) else
            let visited = visited st in
            if not (IndexSet.mem nfa !visited) then (
              visited := IndexSet.add nfa !visited;
              push candidates (st, nfa, (path, st0, nfa0, finished))
            )
        )
      in
      let predecessors (st, nfa, path) =
        let preds = Lazy.force nfa_predecessors nfa in
        IntSet.iter (fun id ->
            let st' = Hashtbl.find t.table id in
            let visited = IndexSet.inter preds st'.visited in
            IndexSet.iter (fun nfa' ->
                match IndexMap.find_opt (NFA.label nfa') st'.transitions with
                | None -> ()
                | Some st'' ->
                  if Lazy.is_val st'' && Lazy.force st'' == st then
                    add_candidate path st' nfa'
              ) visited
          ) st.predecessors
      in
      List.iter
        (fun st -> IndexSet.iter (fun nfa -> add_candidate ([], st, nfa, ref false) st nfa) st.unhandled) t.partial;
      let rec look () =
        match !candidates with
        | [] -> LNil
        | candidates' ->
          candidates := [];
          List.iter predecessors candidates';
          enum ()
      and enum () =
        match !found with
        | [] -> look ()
        | examples ->
          found := [];
          LCons (List.to_seq examples, Lazy.from_fun look)
      in
      let node = lazy_list_to_seq (look ()) in
      Seq.concat (fun () -> node)
  end

  module Offering_intersector = struct
    type n = Lrc.n
    let n = Lrc.n

    let initials = Lrc.offering_states
    let transitions = Lrc.predecessors
    let label = Lrc.lr1_of_lrc
  end

  module Refined_redgraph = struct
    module RG = Dfa.Redgraph

    let visit lr1 =
      let fail_on = RG.fail_on_closure lr1 in
      let rec reductions acc path = function
        | None -> acc
        | Some st ->
          let path = st :: path in
          let acc =
            match RG.state_goto_closure st with
            | [] -> acc
            | gcs ->
              let lookahead, fail_on =
                List.fold_left
                  (fun (lookahead, fail_on) (gc : RG.goto_closure) ->
                     let lookahead = IndexSet.union lookahead gc.lookahead in
                     let fail_on =
                       IndexSet.fold (fun tgt acc ->
                           IndexSet.union acc (RG.fail_on_closure tgt))
                         gc.targets fail_on
                     in
                     (lookahead, fail_on))
                  (IndexSet.empty, IndexSet.empty) gcs
              in
              (lookahead, IndexSet.inter fail_on lookahead, path) :: acc
          in
          reductions acc path (RG.state_parent st)
      in
      let paths = reductions [] [] (Some (RG.State.of_lr1 lr1)) in
      let _ =
        List.fold_left (fun acc (lookahead, _fail_on, _path) ->
            assert (IndexSet.disjoint acc lookahead);
            IndexSet.inter lookahead acc
          ) fail_on paths
      in
      let count = List.length paths in
      if count > 0 then (
        Printf.eprintf "Reduction for %s has %d paths\n"
          (Lr1.to_string lr1) count;
        List.iter (fun (lookahead, fail_on, path) ->
            prerr_endline (
              "- " ^
              string_concat_map "; " (fun st ->
                  st |>
                  RG.state_lr1s |> IndexSet.minimum |>
                  Option.get |> Lr1.to_string
                ) path;
            );
            Printf.eprintf
              "  %d lookahead tokens, with %d failings, %d transitions\n"
              (IndexSet.cardinal lookahead)
              (IndexSet.cardinal fail_on)
              (List.length (RG.state_goto_closure (List.hd path)))
          ) paths
      )

    let () = Index.iter Lr1.n visit



    (*let terms_to_ignore =
      Grammar.Terminal.fold (fun raw acc ->
          let ignore =
            (match Grammar.Terminal.kind raw with
             | `EOF | `PSEUDO | `ERROR -> true
             | _ -> false)
          in
          if ignore then
            IndexSet.add (Terminal.of_g raw) acc
          else
            acc
        ) IndexSet.empty

    let rec check_terminal_partitions partitions = function
      | None -> ()
      | Some st ->
        List.iter (fun (gc : RG.goto_closure) ->
            let partitions =
              partitions
              |> List.map (fun la -> IndexSet.inter la gc.lookahead)
              |> List.filter (fun set -> not (IndexSet.is_empty set))
            in
            IndexSet.iter (fun src ->
                IndexSet.iter (fun tgt ->
                    let nt = match Lr1.incoming tgt with
                      | None | Some (T _) -> assert false
                      | Some (N nt) -> nt
                    in
                    let tr = Transition.find_goto src nt in
                    let classes = LRijkstra.Classes.for_edge tr in
                    Array.iter (fun classe ->
                        let classe = IndexSet.diff classe terms_to_ignore in
                        List.iter (fun classe' ->
                            if not (IndexSet.subset classe' classe ||
                                    IndexSet.disjoint classe classe') then (
                              prerr_endline "Incompatible classes: ";
                              prerr_endline (
                                "{" ^
                                string_concat_map ", "
                                  Terminal.to_string (IndexSet.elements classe) ^
                                "}"
                              );
                              prerr_endline (
                                "{" ^
                                string_concat_map ", "
                                  Terminal.to_string (IndexSet.elements classe') ^
                                "}"
                              );
                            )
                          ) partitions
                      ) classes
                  ) gc.targets
              ) gc.sources
          ) (RG.state_goto_closure st);
        check_terminal_partitions partitions (RG.state_parent st)*)

    (*let () =
      Index.iter Lr1.n (fun lr1 ->
          check_terminal_partitions
            (Array.to_list (LRijkstra.Classes.for_lr1 lr1)
             |> List.map (fun c -> IndexSet.diff c terms_to_ignore))
            (Some (RG.State.of_lr1 lr1)))*)

    (*let expand_lrcs path lrcs acc =
      IndexSet.fold (fun lrc acc -> (lrc :: path) :: acc) lrcs acc

    let lrc_refinement lr1 =
      let rec loop all_lrcs paths = function
        | None -> (all_lrcs, paths)
        | Some st ->
          let paths =
            List.concat_map (fun path ->
                let lrc = List.hd path in
                expand_lrcs path (Lrc.predecessors lrc) []
              ) paths
          in
          let all_lrcs = indexset_bind all_lrcs Lrc.predecessors in
          loop all_lrcs paths (RG.state_parent st)
      in
      let all_lrcs = (Lrc.lrcs_of_lr1 lr1) in
      let paths = expand_lrcs [] all_lrcs [] in
      let st = RG.State.of_lr1 lr1 in
      let all_lrcs, all_paths = loop all_lrcs paths (Some st) in
      Printf.eprintf "state %s has %d paths / %d lrcs\n"
        (Lr1.to_string lr1) (List.length all_paths) (IndexSet.cardinal all_lrcs)

    let () =
      Index.iter Lr1.n lrc_refinement*)
  end
*)
end

let process_entry oc entry =
  let cases, vars =
    let transl_case i case =
      let var_count = ref 0 in
      let vars = ref [] in
      let alloc name =
        let id = !var_count in
        Utils.Misc.push vars name;
        incr var_count;
        (i, id)
      in
      let kre = Regexp.transl ~alloc ~clause:i case.Syntax.pattern in
      let vars = List.rev !vars in
      (kre, vars)
    in
    List.split (List.mapi transl_case entry.Syntax.clauses)
  in
  let cases = Regexp.KRESet.of_list cases in
  let dfa, initial = Dfa.Repr.gen (Dfa.State.make cases) in
  (*let module Check = Coverage.Check_dfa(Coverage.Offering_intersector) in
  let check = Check.analyse initial in
  if false then
    Seq.iter (fun (_st, _nfa, path) ->
        let path = "... " ^ string_concat_map " " Info.Lr1.to_string path in
        prerr_endline ("Found uncovered case: " ^ path);
        (*prerr_endline ("when looking ahead at: " ^
                       string_concat_map ", " Info.Terminal.to_string
                         (Utils.BitSet.IndexSet.elements
                            (snd (Coverage.Lrc.decompose nfa))))*)
      ) (Check.paths check);*)
  (*Coverage.Offering_check.visit initial;*)

  Format.eprintf "(* %d states *)\n%!" (Dfa.StateMap.cardinal dfa);
  output_char oc '\n';
  gen_code entry oc vars entry.Syntax.clauses;
  output_char oc '\n';
  output_table oc entry vars initial (Dfa.gen_table dfa)


(*
let () =
  Index.iter Info.Transition.goto (fun tr ->
      let tr' = Info.Transition.of_goto tr in
      let src = Info.Transition.source tr' in
      let tgt = Info.Transition.target tr' in
      let src_classes = LRijkstra.Classes.for_lr1 src in
      let tr_classes = LRijkstra.Classes.for_edge tr in
      let tgt_classes = LRijkstra.Classes.for_lr1 tgt in
      if Array.length tr_classes <> Array.length tgt_classes then (
        Printf.eprintf
          "source:%d classes ----> transition:%d classes ----> target:%d classes\n"
          (Array.length src_classes)
          (Array.length tr_classes)
          (Array.length tgt_classes);
        let card set = string_of_int (IndexSet.cardinal set) in
        Printf.eprintf
          "----> {%s} ----> {%s}\n"
          (string_concat_map "," card (Array.to_list tr_classes))
          (string_concat_map "," card (Array.to_list tgt_classes))
      )
    )
*)
let () = (
  (*let doc = Cmon.list_map (KRE.cmon ()) kst.direct in
  if verbose then (
    Format.eprintf "%a\n%!" Cmon.format (Syntax.print_entrypoints entry);
    Format.eprintf "%a\n%!" Cmon.format doc;
  );*)
  begin match !output_name with
    | None ->
      prerr_endline "No output file provided (option -o). Giving up.";
      exit 1
    | Some path ->
      let oc = open_out_bin path in
      output_string oc (snd lexer_definition.header);
      List.iter (process_entry oc) lexer_definition.entrypoints;
      output_char oc '\n';
      output_string oc (snd lexer_definition.trailer);
      close_out oc
  end;
  (*Array.iter (fun (name, stack) ->
      eprintf "Evaluating case %s\n" name;
      (*eval_dfa dfa initial stack;*)
      interp_st {ST.direct=cases; reduce=RedSet.empty} stack;
      (*interp_kre cases IndexSet.empty stack;*)
      eprintf "------------------------\n\n";
    ) Sample.tests*)
  (* Print matching functions *)
)
