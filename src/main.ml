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
            if IndexSet.is_empty preds
            then acc
            else IndexMap.add lr1 preds acc
          ) (Lr1.predecessors (lr1_of_lrc lrc)) IndexMap.empty
      end

    let predecessors_set_by_lr1 lrcs =
      IndexSet.fold (fun lrc acc ->
          IndexMap.union
            (fun _ s1 s2 -> Some (IndexSet.union s1 s2))
            (predecessors_by_lr1 lrc)
            acc
        ) lrcs IndexMap.empty

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

    let () = Printf.eprintf "computed can fail in %.02fms\n"
        ((Sys.time () -. t0) *. 1000.)

    module Intermediate = Gensym()

    type lr1_paths = {
      goto: Terminal.set Lr1.map;
      pops: lr1_paths Lr1.map;
    }

    let intermediate_lr1_steps lr1 =
      let rec process n lr1 = function
        | [] ->
          {goto = IndexMap.empty; pops = IndexMap.empty}

        | (n', nt, ts) :: reds when n = n' ->
          assert (not (IndexSet.is_empty ts));
          let paths = process n lr1 reds in
          begin match Nonterminal.kind nt with
            | `START -> paths
            | `REGULAR ->
              let target = Transition.find_goto_target lr1 nt in
              let goto =
                IndexMap.update target (function
                    | None -> Some ts
                    | Some ts' -> Some (IndexSet.union ts ts')
                  ) paths.goto
              in
              {paths with goto}
          end

        | ((n', _, _) :: _) as reds  ->
          assert (n' > n);
          process_predecessors (n + 1) (Lr1.predecessors lr1) reds

      and process_predecessor n reds lr1' acc =
        IndexMap.add lr1' (process n lr1' reds) acc

      and process_predecessors n lr1s reds = {
        goto = IndexMap.empty;
        pops = IndexSet.fold (process_predecessor n reds) lr1s IndexMap.empty
      }

      in
      process_predecessors 0 (Lr1.predecessors lr1) (Lr1.closed_reductions lr1)

    module Paths = Sum(Lrc)(Intermediate)

    type lrc_paths =
      | Fail of Lrc.set
      | Empty
      | Step of {
          states: Lrc.set;
          path: Paths.n index;
          goto: Lrc.set;
          pops: lrc_paths Lr1.map;
        }

    let paths = IndexBuffer.make Empty

    let mk_step initial states goto pops =
      if IndexSet.is_empty goto && IndexMap.is_empty pops then
        Empty
      else
        let path = match initial with
          | None -> Paths.inj_r (Intermediate.fresh ())
          | Some lrc -> Paths.inj_l lrc
        in
        let result = Step {states; path; goto; pops} in
        IndexBuffer.set paths path result;
        result

    let t0 = Sys.time ()

    let () =
      let process_lr1 lr1 =
        let lr1_steps = intermediate_lr1_steps lr1 in
        let compute_paths lrc =
          let la = Lrc.lookahead lrc in
          let rec visit initial ipaths lrcs =
            let exception Can_fail in
            let process_goto target la' acc =
              if IndexSet.disjoint la la' then acc else (
                if not (IndexSet.disjoint la (Lr1.closed_reject target)) then
                  raise Can_fail;
                IndexSet.union acc (
                  IndexSet.filter (fun lrc ->
                      IndexSet.mem lrc can_fail_states &&
                      let la' = Lrc.lookahead lrc in
                      not (IndexSet.disjoint la la')
                    ) (Lrc.lrcs_of_lr1 target)
                )
              )
            in
            match IndexMap.fold process_goto ipaths.goto IndexSet.empty with
            | exception Can_fail -> Fail lrcs
            | goto ->
              let pops =
                IndexMap.fold (fun lr1 lrcs' opops ->
                    match IndexMap.find_opt lr1 ipaths.pops with
                    | None -> opops
                    | Some ipaths' ->
                      match visit None ipaths' lrcs' with
                      | Empty -> opops
                      | Fail _ | Step _ as opaths ->
                        IndexMap.add lr1 opaths opops
                  ) (Lrc.predecessors_set_by_lr1 lrcs) IndexMap.empty
              in
              mk_step initial lrcs goto pops
          in
          visit (Some lrc) lr1_steps (IndexSet.singleton lrc)
        in
        let process_lrc lrc =
          assert (lr1 = Lrc.lr1_of_lrc lrc);
          if IndexSet.mem lrc fail_states then
            IndexBuffer.set paths (Paths.inj_l lrc) (Fail (IndexSet.singleton lrc))
          else if IndexSet.mem lrc can_fail_states then
            match compute_paths lrc with
            | Empty -> ()
            | Fail _ as x -> IndexBuffer.set paths (Paths.inj_l lrc) x
            | Step x -> assert (IndexSet.is_empty x.goto)
        in
        IndexSet.iter process_lrc (Lrc.lrcs_of_lr1 lr1)
      in
      Index.iter Lr1.n process_lr1

    let paths = Vector.get (IndexBuffer.contents paths Paths.n)

    let () = Printf.eprintf "computed lrc paths in %.02fms\n"
        ((Sys.time () -. t0) *. 1000.)

    let () = Printf.eprintf "%d intermediate steps\n" (cardinal Intermediate.n)

    module NFA = struct
      include Sum(Lrc)(Paths)

      let encode_normal_set lrcs =
        (* TODO: This encoding is actually the identity,
                 find a way to skip it at some point.
           Measurement indicates a 3% decrease of total runtime for OCaml
           grammar coverage. *)
        IndexSet.map inj_l lrcs

      let initials : n indexset =
        IndexSet.fold (fun lrc acc ->
            let p = Paths.inj_l lrc in
            match paths p with
            | Empty -> acc
            | Fail lrcs -> IndexSet.union (encode_normal_set lrcs) acc
            | Step _ -> IndexSet.add (inj_r p) acc
          )
          (IndexSet.inter can_fail_states Lrc.offering_states) IndexSet.empty

      let fold_path_transitions ~lookahead ~acc ~follow_goto ~reach ~fail =
        function
        | Empty | Fail _ -> assert false
        | Step {states; goto; pops; _} ->
          let lr1 = Lrc.lr1_of_lrc (IndexSet.choose states) in
          let acc =
            IndexMap.fold (fun _lr1 step acc ->
                match step with
                | Empty -> assert false
                | Fail lrcs -> fail lrcs lookahead acc
                | Step s' -> reach s'.path lookahead acc
              ) pops acc
          in
          let rec visit lookahead lrc acc =
            let lookahead = follow_goto lrc lookahead in
            match paths (Paths.inj_l lrc) with
            | Empty -> acc
            | Fail lrcs ->
              IndexSet.fold begin fun lrc acc ->
                match IndexMap.find_opt lr1 (Lrc.predecessors_by_lr1 lrc) with
                | None -> assert false
                | Some lrcs -> fail lrcs lookahead acc
              end lrcs acc
            | Step t0 ->
              assert (IndexSet.is_empty t0.goto);
              begin match IndexMap.find_opt lr1 t0.pops with
                | None -> acc
                | Some Empty -> assert false
                | Some (Fail lrcs) -> fail lrcs lookahead acc
                | Some (Step t) ->
                  visit_set lookahead t.goto (reach t.path lookahead acc)
              end
          and visit_set lookahead set acc =
            IndexSet.fold (visit lookahead) set acc
          in
          visit_set lookahead goto acc

      let step_transitions path =
        fold_path_transitions path
          ~lookahead:()
          ~acc:IndexSet.empty
          ~follow_goto:(fun _ () -> ())
          ~reach:(fun step () acc -> IndexSet.add (inj_r step) acc)
          ~fail:(fun lrcs () acc -> IndexSet.union (encode_normal_set lrcs) acc)

      let validate set =
        IndexSet.iter (fun elt -> match prj elt with
            | L _ -> ()
            | R n -> match paths n with
              | Empty | Fail _ -> assert false
              | Step _ -> ()
          ) set;
        set

      let initials = validate initials

      let transitions n =
        validate @@
        match prj n with
        | L n -> encode_normal_set (Lrc.predecessors n)
        | R n ->
          begin match paths n with
            | Empty -> IndexSet.empty
            | Fail _ ->
              begin match Paths.prj n with
                | L (n : Lrc.t) -> encode_normal_set (Lrc.predecessors n)
                | R (_ : Intermediate.n index) -> assert false
              end
            | Step _ as paths -> step_transitions paths
          end

      let label n =
        match prj n with
        | L l -> Lrc.lr1_of_lrc l
        | R p ->
          match paths p with
          | Empty | Fail _ -> assert false
          | Step s -> Lrc.lr1_of_lrc (IndexSet.choose s.states)

      let () =
        Index.iter Paths.n (fun p ->
            match paths p with
            | Step t -> assert (t.path == p)
            | _ -> ())

      (*let () =
        let done_ = ref IndexSet.empty in
        let todo = ref initials in
        let iter = ref 0 in
        while not (IndexSet.equal !done_ (validate !todo)) || !iter < 10 do
          done_ := !todo;
          todo := IndexSet.union !todo (indexset_bind !todo transitions);
          incr iter
        done;
        Printf.eprintf "Explored all transitions in %d iterations" !iter*)

    end
  end


  module Check_dfa(NFA : sig
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
    val paths : t -> (state * NFA.n index * NFA.n index list) Seq.t
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
          let path = nfa :: path in
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

end

open Utils.BitSet

let rec follow_lookahead_path lookahead state = function
  | [] -> lookahead
  | x :: xs ->
    let open Coverage in
    match Lrce.NFA.prj state with
    | L _ -> lookahead
    | R path ->
      let lookahead =
        Lrce.NFA.fold_path_transitions (Lrce.paths path)
          ~lookahead ~acc:IndexSet.empty
          ~follow_goto:(fun lrc la ->
              let lr1 = Lrc.lr1_of_lrc lrc in
              let la = IndexSet.inter (Lrc.lookahead lrc) la in
              let la = IndexSet.diff la (Info.Lr1.closed_reject lr1) in
              la
            )
          ~reach:(fun path lookahead acc ->
              if compare_index path x = 0
              then IndexSet.union acc lookahead
              else acc
            )
          ~fail:(fun lrcs lookahead acc ->
              match Lrce.NFA.prj x with
              | L lrc' ->
                if IndexSet.mem lrc' lrcs then
                  IndexSet.union acc lookahead
                else
                  acc
              | R _ -> acc
            )
      in
      follow_lookahead_path lookahead x xs

let compute_lookahead = function
  | [] -> assert false
  | entry :: rest ->
    match Coverage.Lrce.NFA.prj entry with
    | L lrc ->
      let la = Coverage.Lrc.lookahead lrc in
      let la =
        IndexSet.inter la (Info.Lr1.closed_reject (Coverage.Lrc.lr1_of_lrc lrc))
      in
      assert (not (IndexSet.is_empty la));
      la
    | R path ->
      match Coverage.Lrce.paths path with
      | Empty | Fail _ -> assert false
      | Step s ->
        assert (IndexSet.is_singleton s.states);
        let lrc = IndexSet.choose s.states in
        follow_lookahead_path (Coverage.Lrc.lookahead lrc) entry rest

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
  if true then (
    let module Check = Coverage.Check_dfa(Coverage.Lrce.NFA) in
    let check = Check.analyse initial in
    let count = ref 6 in
    try Seq.iter (fun (_st, _nfa, nfa_path) ->
        let initial = ref false in
        let print nfa =
          let lr1 = Coverage.Lrce.NFA.label nfa in
          match Info.Lr1.incoming lr1 with
          | None -> initial := true; Info.Lr1.to_string lr1
          | Some sym -> Info.Symbol.name sym
        in
        let path = List.rev_map print nfa_path in
        let path = if !initial then path else "..." :: path in
        let path = String.concat " " path in
        prerr_endline ("Found uncovered case:\n  " ^ path);
        let la = compute_lookahead nfa_path in
        prerr_endline ("Looking ahead at:\n  {" ^
                       string_concat_map ", " Info.Terminal.to_string
                         (Utils.BitSet.IndexSet.elements la) ^ "}\n");
        decr count;
        (*if !count = 0 then (
          prerr_endline "Press enter to get more cases, \
                         type anything else to stop";
          match read_line () with
          | "" -> count := 6
          | _ -> raise Exit
        )*)
      ) (Check.paths check)
    with Exit -> ()
  );
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
