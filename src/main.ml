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

(* The lexer generator. Command-line parsing. *)

let source_name = ref None
let output_name = ref None
let grammar_file = ref None

let usage = "usage: menhirlex [options] sourcefile"

let print_version_string () =
  print_string "The Menhir parser lexer generator :-], version ";
  print_string Sys.ocaml_version;
  print_newline ();
  exit 0

let print_version_num () =
  print_endline Sys.ocaml_version;
  exit 0

let specs = [
  "-o", Arg.String (fun x -> output_name := Some x),
  " <file>  Set output file name to <file>";
  "-g", Arg.String (fun x -> grammar_file := Some x),
  " <file.cmly>  Path of the Menhir compiled grammar to analyse (*.cmly)";
  "-q", Arg.Set Common.quiet_mode,
  " Do not display informational messages";
  "-n", Arg.Set Common.dry_run,
  " Process input but do not generate any file";
  "-d", Arg.Set Common.dump_parsetree,
  " Dump parsetree";
  "-v",  Arg.Unit print_version_string,
  " Print version and exit";
  "-version",  Arg.Unit print_version_string,
  " Print version and exit";
  "-vnum",  Arg.Unit print_version_num,
  " Print version number and exit";
]

let () = Arg.parse specs (fun name -> source_name := Some name) usage

let maybe_close lout =
  if Lazy.is_val lout then (
    let lazy (oc, tr) = lout in
    close_out oc;
    Common.close_tracker tr;
  )

module Analysis (X : sig val filename : string end)() =
struct
  module Grammar = MenhirSdk.Cmly_read.Read(X)
  module Lr1 = Middle.Lr1.Make(Grammar)
  module Sigma = Middle.Sigma.Make(Lr1)
  module Reduction_graph = Middle.Reduction_graph.Make(Sigma)()
  module Regex = Middle.Regex.Make(Sigma)(Reduction_graph)
  module Transl = Transl.Make(Regex)

  (*let dfa_to_file initial ic def (dfa : Regex.dfa) file =
    let oc = open_out file in
    output_string oc "digraph G {\n";
    output_string oc "  rankdir=LR;\n";
    let k = ref 0 in
    let uid_map = ref Regex.Map.empty in
    let uid expr =
      match Regex.Map.find expr !uid_map with
      | k -> k, false
      | exception Not_found ->
        let k' = !k in
        incr k;
        uid_map := Regex.Map.add expr k' !uid_map;
        k', true
    in
    let rec process expr =
      let src, todo = uid expr in
      if todo then (
        let transitions =
          Regex.Map.find expr dfa
          |> List.map (fun (set,_,target) -> set, process target)
          |> List.sort (fun (_,x1) (_,x2) -> compare x1 x2)
          |> Utils.Misc.merge_group
            ~equal:(=)
            ~group:(fun dst sets -> dst, List.fold_left Sigma.union Sigma.empty sets)
        in
        List.iter (fun (dst, set) ->
            let label = match set with
              | Sigma.Pos set -> Printf.sprintf "{%d}" (Lr1.Set.cardinal set)
              | Sigma.Neg set -> Printf.sprintf "{/%d}" (Lr1.Set.cardinal set)
            in
            Printf.ksprintf (output_string oc)
              "  S%d -> S%d [label=%S];\n" src dst label
          ) transitions
      );
      src
    in
    ignore (process initial : int);
    Printf.ksprintf (output_string oc) "  S%d [shape=box];\n"
      (Regex.Map.find initial !uid_map);
    Regex.Map.iter (fun expr id ->
        let rec string_of_expr : Regex.Expr.t -> string = function
          | Utils.Mulet.Epsilon -> "epsilon"
          | Utils.Mulet.Set _ -> "#"
          | Utils.Mulet.Closure e -> string_of_expr e ^ "*"
          | Utils.Mulet.Not e -> "not " ^ string_of_expr e
          | Utils.Mulet.Label _ -> "label"
          | Utils.Mulet.Abstract _ as e  ->
            if Regex.Expr.nullable e
            then "abstract?"
            else "abstract"
          | Utils.Mulet.Concat xs ->
            "(" ^ String.concat " . " (List.map string_of_expr xs) ^ ")"
          | Utils.Mulet.Or xs ->
            "(" ^ String.concat " | " (List.map string_of_expr xs) ^ ")"
          | Utils.Mulet.And xs ->
            "(" ^ String.concat " & " (List.map string_of_expr xs) ^ ")"
        in
        ignore string_of_expr;
        let action_to_strings {Regex.Action. accept} =
          match Utils.BitSet.IntSet.fold (fun x xs -> x :: xs) accept [] with
          | [] -> []
          | xs ->
            let entry = List.hd def.Syntax.entrypoints in
            List.map (fun x ->
                match (List.nth entry.clauses x).action with
                | None -> "."
                | Some loc -> Common.read_location ic loc
              ) xs
        in
        Printf.ksprintf (output_string oc) "  S%d [label=%S];\n" id
          (String.concat "\n"
             ((*string_of_expr expr ::*)
              action_to_strings (Regex.Expr.get_label expr)))
      ) !uid_map;
    output_string oc "}\n";
    close_out oc*)

  let initial_states : (Grammar.nonterminal * Grammar.lr1) list =
    Grammar.Lr1.fold begin fun lr1 acc ->
      let lr0 = Grammar.Lr1.lr0 lr1 in
      match Grammar.Lr0.incoming lr0 with
      | Some _ -> acc
      | None ->
        (* Initial state *)
        Format.eprintf "Initial state %d\n%a\n"
          (Grammar.Lr1.to_int lr1)
          Grammar.Print.itemset (Grammar.Lr0.items lr0);
        let (prod, _) = List.hd (Grammar.Lr0.items lr0) in
        let nt = match (Grammar.Production.rhs prod).(0) with
          | Grammar.N nt, _, _ -> nt
          | _ -> assert false
        in
        (nt, lr1) :: acc
    end []

  module Interp = Interp.Make(Grammar)

  (*let red_graph_to_dot focus =
    let exception Found of Grammar.nonterminal in
    match
      Grammar.Nonterminal.iter
        (fun nt -> if Grammar.Nonterminal.name nt = focus then raise (Found nt))
    with
    | () -> failwith ("Unkown nonterminal " ^ focus)
    | exception (Found focus) ->
      let module Fin = Utils.Strong.Finite in
      let module R = Reduction_graph in
      let derivations = R.Derivation.derive
          ~step:(fun nt nts -> nt :: nts)
          ~finish:(fun _lr1 stack ->
              if List.mem focus stack then
                List.rev stack
              else
                []
            )
          []
      in
      let non_empty = R.Derivation.filter (fun x -> x <> []) derivations in
      let interesting st =
        let reachable = R.Derivation.reachable st in
        not (R.Derivation.Set.disjoint non_empty reachable)
      in
      let module States = Fin.Set.Gensym() in
      let states_index = Hashtbl.create 7 in
      let state_index st =
        try Hashtbl.find states_index st
        with Not_found ->
          let result = States.fresh () in
          Hashtbl.add states_index st result;
          result
      in
      let transitions_def = ref [] in
      let states_label = ref [] in
      Fin.Set.iter R.Concrete.States.n (fun st ->
          if interesting st then (
            let id = state_index st in
            let label =
              R.Derivation.Set.fold (fun d acc ->
                  let d' = R.Derivation.get derivations d in
                  if d' <> [] then d' :: acc else acc
                ) (R.Derivation.reached st) []
              |> List.sort_uniq compare
            in
            states_label := (id, label) :: !states_label;
            List.iter (fun (_,tgt) ->
                if interesting tgt then (
                  let tgt = state_index tgt in
                  transitions_def := (id, tgt) :: !transitions_def
                )
              ) (R.Concrete.transitions st)
          )
        );
      let module Valmari_DFA = struct
        type states = States.n
        let states = States.freeze ()
        module Transitions =
          Fin.Array.Of_array(struct
            type a = (states Fin.elt * states Fin.elt)
            let table = Array.of_list !transitions_def
          end)
        type transitions = Transitions.n
        let transitions = Transitions.n
        let source tr = fst Fin.(Transitions.table.(tr))
        let target tr = snd Fin.(Transitions.table.(tr))
        let label _ = ()
        let initials = Fin.Array.(to_array (init states (fun i -> i)))
        let finals = [||]

        (* refinements : refine:((iter:(elt -> unit) -> unit) -> unit *)
        let refinements ~refine:_ = ()
          (*let classes =
            Utils.Misc.merge_group !states_label
              ~equal:(=) ~group:(fun _ states -> states)
          in
          List.iter (fun classe ->
              refine (fun ~iter -> List.iter iter classe)
            ) classes*)
      end in
      let module DFA' = Utils.Valmari.Minimize(struct
          type t = unit
          let compare _ _ = 0
        end)(Valmari_DFA) in
      let oc = open_out "red.dot" in
      Printf.fprintf oc "digraph G {\n";
      Fin.Set.iter R.Concrete.States.n (fun st ->
          if interesting st then (
            let label =
              R.Derivation.Set.fold (fun d acc ->
                  let d' = R.Derivation.get derivations d in
                  if d' <> [] then
                    String.concat " " (List.map Grammar.Nonterminal.name d')
                    :: acc
                  else
                    acc
                ) (R.Derivation.reached st) []
            in
            Printf.fprintf oc "  S%d[label=%S];\n"
              (Fin.Elt.to_int st)
              (String.concat "\n" label);
            List.iter (fun (_,tgt) ->
                if interesting tgt then
                  Printf.fprintf oc "  S%d -> S%d;\n"
                    (Fin.Elt.to_int st)
                    (Fin.Elt.to_int tgt)
              ) (R.Concrete.transitions st)
          )
        );
      Printf.fprintf oc "}\n";
      close_out oc

  let () = red_graph_to_dot "let_binding_body"*)

  let enumerate_productions =
    let all_gotos =
      Reduction_graph.Derivation.derive
        ~step:(fun nt nts -> nt :: nts)
        ~finish:(fun lr1 stack -> List.fold_left Lr1.goto lr1 stack)
        []
    in
    let follow state lr1 =
      Reduction_graph.Concrete.Set.fold (fun src acc ->
          List.fold_left (fun acc (lr1s, dst) ->
              if Lr1.Set.mem lr1 lr1s
              then Reduction_graph.Concrete.Set.add dst acc
              else acc
            ) acc (Reduction_graph.Concrete.transitions src)
        ) state Reduction_graph.Concrete.Set.empty
    in
    fun stack ->
      let rec loop acc state stack =
        let acc = Reduction_graph.Concrete.Set.fold (fun st acc ->
            Reduction_graph.Derivation.Set.fold
              (fun d acc -> Lr1.Set.add
                  (Reduction_graph.Derivation.get all_gotos d) acc)
              (Reduction_graph.Derivation.reached st)
              acc
          ) state acc
        in
        match stack with
        | [] -> acc
        | hd :: tl -> loop acc (follow state hd) tl
      in
      let reachable =
        match stack with
        | [] -> assert false
        | hd :: tl ->
          let red = Reduction_graph.Concrete.from_lr1 hd in
          loop (Lr1.Set.singleton hd) (Reduction_graph.Concrete.Set.singleton red) tl
      in
      let items =
        Lr1.Set.fold
          (fun lr1 acc -> Grammar.Lr0.items (Grammar.Lr1.lr0 lr1) @ acc)
          reachable []
      in
      Format.printf "Items:\n%a%!"
        Grammar.Print.itemset items

  let rec evaluate (ic, def) expr = function
    | [] -> ()
    | hd :: tl ->
      let _, expr' =
        Regex.Expr.left_delta expr (Sigma.Pos (Lr1.Set.singleton hd))
      in
      Format.printf "Consuming state %s\n@[<2>%a@]\n%!"
        (match Grammar.Lr0.incoming (Grammar.Lr1.lr0 hd) with
         | None -> "<start>"
         | Some sym -> Grammar.symbol_name sym)
        Utils.Cmon.format (Regex.cmon expr');
      let actions = Regex.Expr.get_label expr' in
      Utils.BitSet.IntSet.iter (fun x ->
          let entry = List.nth def.Syntax.entrypoints 0 in
          let clause = List.nth entry.clauses x in
          begin match clause.action with
            | Some location ->
              print_endline
                ("(eval) Matched action: " ^ Common.read_location ic location)
            | None ->
              print_endline
                ("(eval) Matched unreachable action! ("^string_of_int x ^ ")")
          end

        ) actions.accept;
      evaluate (ic, def) expr' tl

  let rec interpret (ic, def, dfa as program) state = function
    | [] -> ()
    | hd :: tl ->
      let transitions = Regex.Map.find state dfa in
      match List.find (fun (sigma, _, _) -> Sigma.mem hd sigma) transitions with
      | (_, _, state') ->
        let actions = Regex.Expr.get_label state' in
        Utils.BitSet.IntSet.iter (fun x ->
            let entry = List.nth def.Syntax.entrypoints 0 in
            let clause = List.nth entry.clauses x in
            begin match clause.action with
              | Some location ->
                print_endline
                  ("(interp) Matched action: " ^ Common.read_location ic location)
              | None ->
                print_endline
                  ("(interp) Matched unreachable action! ("^string_of_int x ^ ")")
            end

          ) actions.accept;
        interpret program state' tl
      | exception Not_found -> ()

  let analyse_stack ic def dfa initial stack =
    Format.printf "Stack:\n%s\n%!"
      (String.concat " "
         (List.rev_map (fun lr1 ->
              match Grammar.Lr0.incoming (Grammar.Lr1.lr0 lr1) with
              | None -> "<start>"
              | Some sym -> Grammar.symbol_name sym
            ) stack));
    (*begin match stack with
      | x :: _ ->
        Printf.printf "Transitions on: %s\n"
          (String.concat ", "
             (List.map (fun (sym, _) -> Grammar.symbol_name sym)
                (Grammar.Lr1.transitions x)));
        Printf.printf "Reduce to: %s\n%!"
          (String.concat ", "
             (List.map (fun (t, prods) ->
                  Grammar.Nonterminal.name
                    (Grammar.Production.lhs (List.hd prods)) ^ " on " ^
                  Grammar.Terminal.name t
                )
                 (Grammar.Lr1.reductions x)))
      | [] -> ()
    end;*)
    enumerate_productions stack;
    evaluate (ic, def) initial stack;
    interpret (ic, def, dfa) initial stack
end

let main () =
  let source_name = match !source_name with
    | None ->
      Arg.usage specs usage;
      exit 2
    | Some name -> name
  in
  let dest_name = match !output_name with
    | Some name -> name
    | None ->
      if Filename.check_suffix source_name ".mlyl"
      then Filename.chop_suffix source_name ".mlyl" ^ ".ml"
      else source_name ^ ".ml"
  in
  let ic = open_in_bin source_name in
  let out = lazy (
    let oc = open_out dest_name in
    let tr = Common.open_tracker dest_name oc in
    (oc, tr)
  ) in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.Lexing.lex_curr_p <-
    {Lexing.pos_fname = source_name; Lexing.pos_lnum = 1;
     Lexing.pos_bol = 0; Lexing.pos_cnum = 0};
  try
    let def = Parser.lexer_definition Lexer.main lexbuf in
    if !Common.dump_parsetree then
      Format.eprintf "Parsetree:\n%a\n"
        Utils.Cmon.format (Syntax.print_definition def);
    List.iter (fun entry ->
        List.iter (fun clause ->
            Syntax.check_wellformed clause.Syntax.pattern
          ) entry.Syntax.clauses
      ) def.Syntax.entrypoints;
    begin match !grammar_file with
      | None ->
        Format.eprintf "No grammar provided (-g), stopping now.\n"
      | Some path ->
        let module Analysis = Analysis(struct let filename = path end)() in
        let linearize_symbol = Transl.linearize_symbol in
        let open Analysis in
        let entries, dfa = Transl.translate def in
        (*dfa_to_file (List.hd entries) ic def dfa "test.dot";*)
        Format.printf "Interpreter. Select an entrypoint using <non-terminal> ':' \
                       then input sentences using <symbol>* '.' \n%!";
        let lexbuf = Lexing.from_channel stdin in
        let entrypoint = ref None in
        let rec loop () =
          match
            let prompt = Parser.prompt_sentence Lexer.main lexbuf in
            match prompt with
            | Syntax.Prompt_entrypoint new_entrypoint ->
              let symbol = Transl.translate_symbol new_entrypoint in
              let initial =
                try
                  match symbol with
                  | Grammar.N n -> List.assoc n initial_states
                  | Grammar.T _ -> raise Not_found
                with Not_found ->
                  Printf.ksprintf invalid_arg "%s is not an entrypoint"
                    (linearize_symbol new_entrypoint)
              in
              Printf.printf "Selected entrypoint %s\n%!"
                (Grammar.symbol_name symbol);
              entrypoint := Some initial
            | Syntax.Prompt_interpret symbols ->
              begin match !entrypoint with
                | None ->
                  Printf.ksprintf invalid_arg
                    "Select an entrypoint first using \"<non-terminal>:\"\n\
                     Known entrypoints:\n\
                     %s\n"
                    (String.concat "\n"
                       (List.map
                          (fun (nt, _) -> "- " ^ Grammar.Nonterminal.name nt)
                          initial_states))
                | Some initial ->
                  let symbols = List.map Transl.translate_symbol symbols in
                  begin match Interp.loop [initial] symbols with
                    | `Accept (_nt, _rest) ->
                      print_endline "Input accepted"
                    | `Continue stack ->
                      print_endline "Stopped in the middle of parse";
                      analyse_stack ic def dfa (List.hd entries) stack
                    | `No_transition (stack, _rest) ->
                      print_endline "Parser stuck";
                      analyse_stack ic def dfa (List.hd entries) stack
                  end;
              end;
          with
          | () -> loop ()
          | exception End_of_file -> ()
          | exception exn ->
            let msg = match exn with
              | Invalid_argument msg -> msg
              | other -> Printexc.to_string other;
            in
            print_endline msg;
            loop ()
        in
        loop ()
    end;
    close_in ic;
    maybe_close out;
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    close_in ic;
    if Lazy.is_val out then (
      maybe_close out;
      Sys.remove dest_name;
    );
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
      | Syntax.Illformed {msg; file; line; col} ->
        Printf.fprintf stderr
          "File \"%s\", line %d, character %d: %s.\n"
          file line col msg
      | Transl.Error str ->
        Printf.fprintf stderr "Error during translation: %s.\n" str
      | _ -> Printexc.raise_with_backtrace exn bt
    end;
    exit 3

let () =
  main ();
  exit 0
