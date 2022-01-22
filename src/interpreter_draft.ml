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
      Cmon.format (Regex.cmon expr');
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

let rec interpret_loop lexer dfa state = function
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
      interpret_loop lexer dfa state' tl
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
  interpret_loop lexer dfa initial stack

let interpreter _lexer =
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
