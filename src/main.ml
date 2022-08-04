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
      Printf.eprintf "computed predecessors in %.02fms\n"
        ((Sys.time () -. t0) *. 1000.0);
      Vector.get predecessors

    let decompose lrc =
      let lr1 = lr1_of_lrc lrc in
      let classe = index_delta lrc (first_lrc_of_lr1 lr1) in
      (lr1, (LRijkstra.Classes.for_lr1 lr1).(classe))
  end

  module Check_dfa(Intersector : sig
      include Map.OrderedType
      type t
      val compare : t -> t -> int
      val initial : (Lr1.t * t) list
      val transitions : t -> (Lr1.set * t) list
      val accepting : t -> bool
    end) :
  sig
    type t
    val state : t -> Dfa.Repr.t
    val unhandled : t -> Lr1.t list Lrc.map

    val analyse : Dfa.Repr.t -> t list
  end = struct
    module Set = Set.Make(Intersector)
    module Map = Map.Make(Intersector)

    type mark = {
      state: Dfa.Repr.t;
      transitions: mark lazy_t Lr1.map;
      mutable visited: Lr1.t list Lrc.map Map.t;
      mutable scheduled: Lr1.t list Lrc.map Map.t;
      mutable unhandled: Lr1.t list Lrc.map;
    }

    let make_mark state transitions = {
      state; transitions;
      visited = Map.empty;
      scheduled = Map.empty;
      unhandled = IndexMap.empty;
    }

    let indexset_find f set =
      match IndexSet.iter (fun n -> if f n then raise Exit) set with
      | () -> false
      | exception Exit -> true

    module ReachFix =
      Fix.Fix.ForOrderedType(struct
        type t = Lrc.t * Intersector.t
        let compare (i1, l1) (i2, l2) =
          let c = compare_index i1 i2 in
          if c <> 0 then c else
            Intersector.compare l1 l2
      end)(Fix.Prop.Boolean)

    let reachability_cache () =
      ReachFix.lfp (fun (lrc, isector) ->
          if Intersector.accepting isector then
            (fun _valuation -> true)
          else fun valuation ->
            let lrcs = Lrc.predecessors lrc in
            List.exists (fun (lr1, isector') ->
                indexset_find (fun lrc' -> valuation (lrc', isector'))
                  (IndexSet.inter lrcs (Lrc.lrcs_of_lr1 lr1))
              ) (Intersector.transitions isector)
        )

    let make_mark_table () =
      let table = Hashtbl.create 7 in
      let rec aux state =
        match Hashtbl.find_opt table state.Dfa.Repr.id with
        | Some t -> t
        | None ->
          let trs = ref IndexMap.empty in
          let add_lr1 target lr1 = trs := IndexMap.add lr1 target !trs in
          Dfa.iter_transitions state (fun lr1s _vars target ->
              let target = lazy (aux target) in
              IndexSet.iter (add_lr1 target) lr1s
            );
          let t = make_mark state !trs in
          Hashtbl.add table state.Dfa.Repr.id t;
          t
      in
      aux

    let unmapped map isector lrcs =
      match Map.find_opt isector map with
      | None -> lrcs
      | Some lrcs_map ->
        IndexSet.fold (fun n acc ->
            if IndexMap.mem n lrcs_map
            then IndexSet.remove n acc
            else acc
          ) lrcs lrcs

    let make_scheduler () =
      let todo = ref [] in
      let schedule target path (lrcs, isector) =
        let lrcs =
          lrcs
          |> unmapped target.visited isector
          |> unmapped target.scheduled isector
        in
        if not (IndexSet.is_empty lrcs) then (
          if Map.is_empty target.scheduled then push todo target;
          target.scheduled <- Map.update isector (fun m ->
              let m = Option.value m ~default:IndexMap.empty in
              Some (IndexSet.fold (fun n m -> IndexMap.add n path m) lrcs m)
            ) target.scheduled;
        )
      in
      (todo, schedule)

    let analyse initial =
      let reachable = reachability_cache () in
      let get_mark = make_mark_table () in
      let todo, schedule = make_scheduler () in
      let partial = ref [] in
      let process_transition mark path lrcs (lr1, isector) =
        let lrcs' = Lrc.lrcs_of_lr1 lr1 in
        let lrcs = match lrcs with
          | None -> lrcs'
          | Some lrcs -> IndexSet.inter lrcs lrcs'
        in
        let path = lr1 :: path in
        match IndexMap.find_opt lr1 mark.transitions with
        | None ->
          if indexset_find (fun lrc -> reachable (lrc, isector)) lrcs then (
            if IndexMap.is_empty mark.unhandled then push partial mark;
            IndexSet.iter (fun n ->
                if not (IndexMap.mem n mark.unhandled) then
                  mark.unhandled <- IndexMap.add n path mark.unhandled
              ) lrcs
          )
        | Some (lazy target) ->
          schedule target path (lrcs, isector)
      in
      let process_mark mark =
        let to_visit = mark.scheduled in
        mark.visited <-
          Map.union (fun _isector m1 m2 ->
              Some (IndexMap.union (fun _ _ _ -> assert false) m1 m2))
            mark.visited to_visit;
        mark.scheduled <- Map.empty;
        if IntSet.is_empty mark.state.accepted then
          Map.iter (fun isector lrcm ->
              let trs = Intersector.transitions isector in
              IndexMap.iter (fun lrc path ->
                  let lrcs = Lrc.predecessors lrc in
                  List.iter (process_transition mark path (Some lrcs)) trs
                ) lrcm
            ) to_visit
      in
      let rec loop () =
        match List.rev !todo with
        | [] -> ()
        | todo' ->
          todo := [];
          List.iter process_mark todo';
          loop ()
      in
      List.iter (process_transition (get_mark initial) [] None) Intersector.initial;
      loop ();
      !partial

    type t = mark
    let state t = t.state
    let unhandled t = t.unhandled
  end

  module Offering_intersector = struct
    type t = Lrc.t

    let compare = compare_index

    let explicit_transitions ts =
      IndexSet.fold
        (fun lrc acc -> (Lrc.lr1_of_lrc lrc, lrc) :: acc)
        ts []

    let initial = explicit_transitions Lrc.offering_states

    let transitions t = explicit_transitions (Lrc.predecessors t)

    let accepting _ = true
  end

  module Error_stacks = struct
    type t =
      | Normal of {
          state: Dfa.Redgraph.State.n index;
          lookahead: Terminal.set;
        }
      | Fail

    let compare t1 t2 =
      match t1, t2 with
      | Fail, Fail -> 0
      | Fail, Normal _ -> -1
      | Normal _, Fail -> 1
      | Normal t1, Normal t2 ->
        let c = compare_index t1.state t2.state in
        if c <> 0 then c else
          IndexSet.compare t1.lookahead t2.lookahead


    let initial =
      IndexSet.fold (fun lrc acc ->
          let lr1, lookahead = Lrc.decompose lrc in
          let state = Dfa.Redgraph.State.of_lr1 lr1 in
          (lr1, Normal {state; lookahead}) :: acc
        ) Lrc.offering_states []

    let accepting = function
      | Normal _ -> false
      | Fail -> true

    let transitions = function
      | Fail -> [Lr1.all, Fail]
      | Normal t ->
        let acc =
          match Dfa.Redgraph.state_parent t.state with
          | None -> []
          | Some state ->
            [Lr1.all, Normal {state; lookahead = t.lookahead}]
        in
        let add_goto acc {Dfa.Redgraph. sources; targets; lookahead} =
          let lookahead = IndexSet.inter lookahead t.lookahead in
          if IndexSet.is_empty lookahead then acc else
            IndexSet.fold (fun target acc ->
                let fail_la =
                  IndexSet.inter lookahead
                    (Dfa.Redgraph.fail_on_closure target)
                in
                let acc =
                  if IndexSet.is_empty fail_la then acc else (
                    prerr_endline
                      ("non-empty fail_la for " ^ Lr1.to_string target);
                    (sources, Fail ) :: acc
                    )
                in
                let acc =
                  match Dfa.Redgraph.state_parent
                          (Dfa.Redgraph.State.of_lr1 target)
                  with
                  | None -> acc
                  | Some state ->
                    (sources, Normal {lookahead; state}) :: acc
                in
                acc
              ) targets acc
        in
        List.fold_left add_goto acc
          (Dfa.Redgraph.state_goto_closure t.state)
  end
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
  let module Check = Coverage.Check_dfa(Coverage.Error_stacks) in
  let reports = Check.analyse initial in
  List.iter (fun check ->
      Utils.IndexMap.iter (fun lrc path ->
          let path = "... " ^ string_concat_map " " Info.Lr1.to_string path in
          prerr_endline ("Found uncovered case: " ^ path);
          prerr_endline ("when looking ahead at: " ^
                         string_concat_map ", " Info.Terminal.to_string
                           (Utils.BitSet.IndexSet.elements
                              (snd (Coverage.Lrc.decompose lrc))))
        ) (Check.unhandled check)
    ) reports;
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
