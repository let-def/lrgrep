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
  "-v",  Arg.Unit print_version_string,
  " Print version and exit";
  "-version",  Arg.Unit print_version_string,
  " Print version and exit";
  "-vnum",  Arg.Unit print_version_num,
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

    let lr1_of_lrc, first_lrc_of_lr1 =
      let lr1_of_lrc = Vector.make' n (fun () -> Index.of_int Lr1.n 0) in
      let count = ref 0 in
      let init_lr1 lr1 =
        let classes = LRijkstra.Classes.for_lr1 lr1 in
        assert (Array.length classes > 0);
        let first = Index.of_int n !count in
        count := !count + Array.length classes;
        for i = 0 to Array.length classes - 1 do
          Vector.set lr1_of_lrc (index_shift n first i) lr1
        done;
        first
      in
      let first_lrc_of_lr1 = Vector.init Lr1.n init_lr1 in
      (Vector.get lr1_of_lrc, Vector.get first_lrc_of_lr1)


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

  module Offering_stacks = struct
    type n = Lrc.n
    type set = n indexset
    type 'a map = (n, 'a) indexmap

    let initials =
      let all = ref IndexSet.empty in
      Index.iter Lr1.n (fun lr1 ->
          match Lr1.incoming lr1 with
          | Some (N _) -> ()
          | None | Some (T _) ->
            assert (Array.length (LRijkstra.Classes.for_lr1 lr1) = 1);
            all := IndexSet.add (Lrc.first_lrc_of_lr1 lr1) !all
        );
      !all

    let accepting state =
      Option.is_none (Lr1.incoming (Lrc.lr1_of_lrc state))

    let transition = Lrc.predecessors
  end

  module Offering_check = struct

    type mark = {
      state: Dfa.Repr.t;
      transitions: mark lazy_t Lr1.map;
      mutable visited: Lr1.t list Offering_stacks.map;
      mutable scheduled: Lr1.t list Offering_stacks.map;
      mutable known_partial: bool;
    }

    let visit initial =
      let get_mark =
        let table = Hashtbl.create 7 in
        let rec aux state =
          match Hashtbl.find_opt table state.Dfa.Repr.id with
          | Some t -> t
          | None ->
            let map = ref IndexMap.empty in
            Dfa.iter_transitions state (fun lr1s _vars target ->
                let target = lazy (aux target) in
                let add_lr1 lr1 = map := IndexMap.add lr1 target !map in
                IndexSet.iter add_lr1 lr1s
              );
            let t = {
              state;
              transitions = !map;
              visited = IndexMap.empty;
              scheduled = IndexMap.empty;
              known_partial = false;
            } in
            Hashtbl.add table state.Dfa.Repr.id t;
            t
        in
        aux
      in
      let todo = ref [] in
      let schedule target path lrcs =
        let scheduled =
          IndexSet.fold (fun n acc ->
              if IndexMap.mem n target.visited
              then acc
              else IndexMap.add n path acc
            ) lrcs target.scheduled
        in
        if scheduled != target.scheduled then (
          if IndexMap.is_empty target.scheduled then push todo target;
          target.scheduled <- scheduled
        )
      in
      let process mark =
        let lrcs = mark.scheduled in
        mark.visited <- IndexMap.union (fun _ _ _ -> assert false) mark.visited lrcs;
        mark.scheduled <- IndexMap.empty;
        if IntSet.is_empty mark.state.accepted then
          IndexMap.iter (fun lrc path ->
              let lr1 = Lrc.lr1_of_lrc lrc in
              let path = lr1 :: path in
              match IndexMap.find_opt lr1 mark.transitions with
              | None ->
                if not mark.known_partial then (
                  mark.known_partial <- true;
                  let path =
                    "... " ^ string_concat_map " " Lr1.to_string path
                  in
                  prerr_endline ("Found uncovered case: " ^ path)
                )
              | Some (lazy target) ->
                let lrcs = Lrc.predecessors lrc in
                if false && IndexSet.mem lrc lrcs then (
                  let lr1 = Lrc.lr1_of_lrc lrc in
                  prerr_endline (Lr1.to_string lr1 ^ " in pred(" ^ Lr1.to_string lr1 ^")")
                );
                schedule target path lrcs
            ) lrcs
      in
      let rec loop () =
        match List.rev !todo with
        | [] -> ()
        | todo' ->
          todo := [];
          List.iter process todo';
          loop ()
      in
      schedule (get_mark initial) [] Offering_stacks.initials;
      loop ()
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
  Coverage.Offering_check.visit initial;
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
