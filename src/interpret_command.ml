open Utils
open Fix.Indexing

let opt_grammar_file = ref None
let opt_infile = ref []
let opt_parse_intf = ref false
let opt_stack_items = ref false
let opt_no_reductions = ref false
let opt_no_reductions_items = ref false
let opt_dump_states = ref false

let specs = [
  "-g", Arg.String (fun x -> opt_grammar_file := Some x),
  " <file.cmly> Path to the Menhir compiled grammar to analyse (*.cmly)";
  "-", Arg.Unit (fun () -> opt_infile := ["-"]),
  " Read input from stdin";
  "-no-reductions", Arg.Set opt_no_reductions,
  " Do not simulate reductions";
  "-no-reduction-items", Arg.Set opt_no_reductions_items,
  " Do not print items when simulating reductions";
  "-stack-items", Arg.Set opt_stack_items,
  " Print items of all states on stack";
  "-dump-states", Arg.Set opt_dump_states,
  " Print state numbers for debugging purpose";
]

module Run(P : sig
    val grammar_file : string
    val input_file : string
  end)() =
struct
  module Grammar = MenhirSdk.Cmly_read.Read(struct let filename = P.grammar_file end)

  module Info = Mid.Info.Make(Grammar)
  module Viable = Mid.Viable_reductions.Make(Info)()
  module Regexp = Mid.Regexp.Make(Info)(Viable)

  let print_loc ((loc_start : Lexing.position), (loc_end : Lexing.position)) =
    let sprintf = Printf.sprintf in
    let sline = loc_start.pos_lnum in
    let scol  = loc_start.pos_cnum - loc_start.pos_bol in
    let eline = loc_end.pos_lnum in
    let ecol  = loc_end.pos_cnum - loc_end.pos_bol in
    if sline = eline then
      sprintf "line %d:%d-%d\t" sline scol ecol
    else
      sprintf "from %d:%d to %d:%d\t" sline scol eline ecol

  let print_item (prod, pos) =
    let open Info in
    let rhs = Production.rhs prod in
    let path = ref [] in
    let add_dot i = if pos = i then path := "." :: !path in
    add_dot (Array.length rhs);
    for i = Array.length rhs - 1 downto 0 do
      path := Info.Symbol.name rhs.(i) :: !path;
      add_dot i;
    done;
    path := (Nonterminal.to_string (Production.lhs prod) ^ ":") :: !path;
    String.concat " " !path

  let print_items lr1 =
    List.map (fun item -> "\t\t  [" ^ print_item item ^ "]") (Info.Lr1.items lr1)


  type action = Shift of Info.Lr1.t
              | Reduce of Info.Production.t
              | Accept
              | Reject

  let get_action state terminal =
    let symbol = Info.Symbol.inj_l terminal in
    let find_shift tr =
      Info.Transition.symbol tr = symbol
    in
    let find_reduce red acc =
      if IndexSet.mem terminal (Info.Reduction.lookaheads red) then
        Some red
      else
        acc
    in
    match List.find_opt find_shift (Info.Transition.successors state) with
    | Some tr ->
      Shift (Info.Transition.target tr)
    | None ->
      match
        IndexSet.fold find_reduce (Info.Reduction.from_lr1 state) None
      with
      | Some red ->
        let prod = Info.Reduction.production red in
        begin match Info.Production.kind prod with
        | `START -> Accept
        | `REGULAR -> Reduce prod
        end
      | None -> Reject

  let get_action =
    let table = Hashtbl.create 7 in
    fun state terminal ->
      let key = (state, terminal) in
      match Hashtbl.find_opt table key with
      | Some action -> action
      | None ->
        let action = get_action state terminal in
        Hashtbl.add table key action;
        action

  let find_terminal =
    let table = Hashtbl.create 7 in
    Index.iter Info.Terminal.n
      (fun t -> Hashtbl.add table (Info.Terminal.to_string t) t);
    Hashtbl.find_opt table

  let rec parse_get_token stack lexbuf =
    match Front.Lexer.main lexbuf with
    | IDENT text ->
      begin match find_terminal text with
        | None ->
          Printf.eprintf "Invalid terminal %S\n" text;
          Printf.eprintf "Known terminals:";
          Index.iter Info.Terminal.n (fun t ->
              Printf.eprintf " %s" (Info.Terminal.to_string t)
            );
          Printf.eprintf "\n";
          parse_get_token stack lexbuf
        | Some terminal ->
          parse_consume stack stack lexbuf
            (terminal, lexbuf.lex_start_p, lexbuf.lex_curr_p)
      end
    | DOT | EOF -> stack
    | _ ->
      Printf.eprintf "Unexpected input %s\n" (Lexing.lexeme lexbuf);
      parse_get_token stack lexbuf

  and parse_consume stack0 stack lexbuf (terminal, start, stop as token) =
    let state, _, _ = List.hd stack in
    match get_action state terminal with
    | Accept -> stack0
    | Reject -> stack0
    | Shift state' ->
      parse_get_token ((state', start, stop) :: stack) lexbuf
    | Reduce prod ->
      let _, _, stop = List.hd stack in
      let stack = Misc.list_drop (Info.Production.length prod) stack in
      let state, start , _ = List.hd stack in
      let target = Info.Transition.find_goto_target state (Info.Production.lhs prod) in
      let stack = (target, start, stop) :: stack in
      parse_consume stack0 stack lexbuf token

  let print_lr1 state =
    match Info.Lr1.incoming state with
    | None -> None
    | Some sym -> Some (Info.Symbol.name sym)

  open Info

  let rec display_steps la n acc = function
    | [] -> acc
    | {Viable. reachable=_; candidates} :: rest ->
      let acc = List.fold_left (display_candidate la n) acc candidates in
      display_steps la (n - 1) acc rest

  and display_candidate
    : type a . Info.Terminal.set -> int -> _ -> a Viable.goto_candidate -> _ =
    fun la n acc {Viable. target; lookahead; filter=_; reduction=_} ->
    let la = IndexSet.inter la lookahead in
    if IndexSet.is_empty la then
      acc
    else
      let {Viable. inner; outer} = Viable.get_transitions target in
      let acc =
        if outer <> []
        then (la, outer) :: acc
        else acc
      in
      let acc = display_steps la (n + 1) acc inner in
      let config = Viable.get_config target in
      Printf.printf "\x1b[1;33m\t\t%s↱ %s\n"
        (String.make n ' ') (Option.get (print_lr1 config.top));
      acc

  let process_result stack =
    (*Format.printf "let stack = [%s]\n"
      (String.concat ";" (List.map string_of_int (List.map fst stack)));*)
    Format.printf "Parser stack (most recent first):\n%!";
    let outer = ref [] in
    List.iteri (fun i (state, start, stop) ->
        if i = 0 then (
          let top, _, _ = List.hd stack in
          outer := [Terminal.all, Vector.get Viable.initial top]
        );
        let rec process_steps acc = function
          | (_, []) -> acc
          | (la, step :: next) ->
            let candidates =
              List.filter
                (fun c -> IndexSet.mem state c.Viable.filter)
                step.Viable.candidates
            in
            let threads = List.fold_left (display_candidate la 1) [] candidates in
            (la, next) :: process_threads acc threads
        and process_threads acc = function
          | [] -> acc
          | thread :: threads ->
            process_threads (process_steps acc thread) threads
        in
        outer := process_threads [] !outer;
        if i = 0 || !opt_stack_items then (
          print_string "\x1b[0;36m";
          List.iter print_endline (print_items state);
        );
        print_string "\x1b[0m- ";
        print_string (print_loc (start, stop));
        print_string "\x1b[1m";
        begin match print_lr1 state with
          | None ->
            let find_state (_,_,state') = state' = Info.Lr1.to_g state in
            let nt, _prod, _ = List.find find_state Grammar.Grammar.entry_points in
            print_endline (Grammar.Nonterminal.name nt)
          | Some sym -> print_endline sym
        end;
        print_string "\x1b[0m";
      ) stack;
    if !opt_dump_states then
      Printf.printf "states = %s\n"
        (String.concat ","
           (List.map (fun (idx, _, _) -> string_of_int (idx : _ index :> int))
              stack))

  let () =
    let ic, filename, close_ic =
      match P.input_file with
      | "-" -> (stdin, "<stdin>", false)
      | file -> (open_in_bin file, file, true)
    in
    let lexbuf =
      let lexbuf = Lexing.from_channel ~with_positions:true ic in
      Lexing.set_filename lexbuf filename;
      lexbuf
    in
    begin match Front.Lexer.main lexbuf with
      | IDENT text ->
        begin match Hashtbl.find_opt Info.Lr1.entrypoints text with
          | None ->
            Printf.eprintf "Invalid entrypoint %S\n" text;
            Printf.eprintf "Valid entrypoints:";
            Hashtbl.iter (fun name _ -> Printf.eprintf " %s" name)
              Info.Lr1.entrypoints;
            Printf.eprintf "\n"
          | Some lr1 ->
            let stack = [lr1, lexbuf.lex_start_p, lexbuf.lex_curr_p] in
            process_result (parse_get_token stack lexbuf);
        end
      | _ ->
        Printf.eprintf "Unexpected input %s\n" (Lexing.lexeme lexbuf);
    end;
    if close_ic then
      close_in_noerr ic
end

let run args =
  match !opt_infile @ args with
  | [] ->
    Error "No input provided"
  | x1 :: x2 :: _ ->
    Error (
      Printf.sprintf
        "Unexpected arguments: %s %s...\n\
         Expecting a single input file or '-'" x1 x2
    )
  | [input_file] -> (
      match !opt_grammar_file with
      | None ->
        Error "No grammar provided (-g)"
      | Some grammar_file ->
        let module _ : sig end = Run(struct
            let input_file = input_file
            let grammar_file = grammar_file
          end)() in
        Ok ()
    )
