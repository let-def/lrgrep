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
let interpret = ref false

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
  " <file.ml>  Set output file name to <file> (defaults to <source>.ml)";
  "-g", Arg.String (fun x -> grammar_file := Some x),
  " <file.cmly>  Path of the Menhir compiled grammar to analyse (*.cmly)";
  "-q", Arg.Set Common.quiet_mode,
  " Do not display informational messages";
  "-i", Arg.Set interpret,
  " Start an interpreter to test sentences (do not produce other output)";
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
  let result = Lazy.is_val lout in
  if result then (
    let lazy (oc, tr) = lout in
    close_out oc;
    Common.close_tracker tr;
  );
  result

(* Specification of a "parser lexer", an .mlyl file *)
type lexer = {
  (* The channel that reads in the file *)
  channel: in_channel;
  (* AST corresponding to the source *)
  def: Syntax.lexer_definition;
}

module Analysis (X : sig val filename : string end)() =
struct
  module Grammar = MenhirSdk.Cmly_read.Read(X)
  module Lr1 = Middle.Lr1.Make(Grammar)
  module Sigma = Middle.Sigma.Make(Lr1)
  module Reduction_graph = Middle.Reduction_graph.Make(Sigma)()
  module Regex = Middle.Regex.Make(Sigma)(Reduction_graph)
  module Transl = Transl.Make(Regex)

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

    let enumerate_productions =
    let all_gotos =
      Reduction_graph.Derivation.derive
        ~step:(fun lr1 _ -> Some lr1)
        ~finish:(fun lr1 stack -> Option.value stack ~default:lr1)
        None
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

  let rec evaluate lexer expr = function
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
          let entry = List.nth lexer.def.Syntax.entrypoints 0 in
          let clause = List.nth entry.clauses x in
          begin match clause.action with
            | Some location ->
              let body = Common.read_location lexer.channel location in
              Printf.printf "(eval) Matched action: %s\n" body
            | None ->
              Printf.printf "(eval) Matched unreachable action! (%d)\n" x
          end

        ) actions.accept;
      evaluate lexer expr' tl

  let rec interpret lexer dfa state = function
    | [] -> ()
    | hd :: tl ->
      let transitions = Regex.Map.find state dfa in
      match List.find (fun (sigma, _, _) -> Sigma.mem hd sigma) transitions with
      | (_, _, state') ->
        let actions = Regex.Expr.get_label state' in
        Utils.BitSet.IntSet.iter (fun x ->
            let entry = List.nth lexer.def.Syntax.entrypoints 0 in
            let clause = List.nth entry.clauses x in
            begin match clause.action with
              | Some location ->
                let body = Common.read_location lexer.channel location in
                Printf.printf "(interp) Matched action: %s\n" body
              | None ->
                Printf.printf "(eval) Matched unreachable action! (%d)\n" x
            end

          ) actions.accept;
        interpret lexer dfa state' tl
      | exception Not_found -> ()

  let analyse_stack lexer dfa initial stack =
    Format.printf "Stack:\n%s\n%!"
      (String.concat " "
         (List.rev_map (fun lr1 ->
              match Grammar.Lr0.incoming (Grammar.Lr1.lr0 lr1) with
              | None -> "<start>"
              | Some sym -> Grammar.symbol_name sym
            ) stack));
    enumerate_productions stack;
    evaluate lexer initial stack;
    interpret lexer dfa initial stack
end

let interpreter _grammar _lexer =
  ()
  (*let module Analysis = Analysis(struct let filename = grammar end)() in
    let linearize_symbol = Transl.linearize_symbol in
    let open Analysis in
    let entries, dfa = Transl.translate lexer.def in
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
                analyse_stack lexer dfa (List.hd entries) stack
              | `No_transition (stack, _rest) ->
                print_endline "Parser stuck";
                analyse_stack lexer dfa (List.hd entries) stack
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
  *)

let compiler _out grammar lexer =
  let module Analysis = Analysis(struct let filename = grammar end)() in
  (*let linearize_symbol = Transl.linearize_symbol in*)
  let open Analysis in
  let _, (module DFA) = Transl.translate lexer.def in
  Printf.printf "let %s =\n"
    (String.concat ", " (List.map (fun e -> e.Syntax.name) lexer.def.entrypoints));
  let st_fun st =
    "st_" ^ string_of_int (Utils.Strong.Finite.Elt.to_int st)
  in
  Utils.Strong.Finite.Set.iter DFA.states (fun st ->
      Printf.printf "  %s %s stack = match current_state stack with\n"
        (if Utils.Strong.Finite.Elt.to_int st = 0 then "let rec" else "and")
        (st_fun st);
      let expr = DFA.represent_state st in
      let action = Regex.Expr.get_label expr in
        let transitions =
          Utils.Misc.group_by ~compare:(fun tr1 tr2 ->
              let tgt1 = DFA.target tr1 and tgt2 = DFA.target tr2 in
              Int.compare
                (Utils.Strong.Finite.Elt.to_int tgt1)
                (Utils.Strong.Finite.Elt.to_int tgt2)
            )
            ~group:(fun tr trs ->
                DFA.target tr,
                List.fold_left
                  (fun sg tr -> Lr1.Set.union sg (DFA.label tr))
                  (DFA.label tr) trs
              )
            (DFA.transitions_from st)
        in
        List.iter (fun (target, sigma) ->
            Printf.printf "    | %s -> %s (next stack)\n"
              (String.concat "|"
                 (List.map
                    (fun lr1 -> string_of_int (lr1 : Grammar.lr1 :> int))
                    (Lr1.Set.elements sigma)))
              (st_fun target)
          ) transitions;
        Printf.printf "    | _ -> failwith \"Match failure\"\n";
      if Utils.BitSet.IntSet.is_empty action.Regex.Action.accept then (
      ) else (
        let index = Utils.BitSet.IntSet.choose action.Regex.Action.accept in
        let entry = List.hd lexer.def.entrypoints in
        let clause = List.nth entry.clauses index in
        match clause.action with
        | None -> failwith "Reached unreachable case!"
        | Some loc ->
          let body = Common.read_location lexer.channel loc in
          print_endline body
      )
    );
  Printf.printf "  %s\n"
    (String.concat ", " (List.map st_fun (Array.to_list DFA.initial)))

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
      Format.eprintf "Parsetree:\n%!%a"
        Utils.Cmon.format (Syntax.print_definition def);
    List.iter (fun entry ->
        List.iter (fun clause ->
            Syntax.check_wellformed clause.Syntax.pattern
          ) entry.Syntax.clauses
      ) def.Syntax.entrypoints;
    begin match !grammar_file with
      | None -> Format.eprintf "No grammar provided (-g), stopping now.\n"
      | Some path ->
        let lexer = {channel = ic; def} in
        if !interpret
        then interpreter path lexer
        else compiler out path lexer
    end;
    close_in ic;
    ignore (maybe_close out : bool);
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    close_in ic;
    if maybe_close out then
      Sys.remove dest_name;
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
