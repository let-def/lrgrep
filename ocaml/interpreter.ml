open Utils
open Fix.Indexing

(* The lexer generator. Command-line parsing. *)

let opt_infile = ref None
let opt_parse_intf = ref false
let opt_stack_items = ref false
let opt_dump_states = ref false
let opt_no_reduce_filter = ref false

let usage =
  Printf.sprintf
    "Parser interpreter\n\
     Prints detailed information to help working out error patterns.\n\
     \n\
     Usage: %s [-intf] [-no-items] [-no-reductions] [-all-items] <-|foo.ml|bar.mli>"
    Sys.argv.(0)

let print_version_num () =
  print_endline "0.1";
  exit 0

let print_version_string () =
  print_string "The Menhir parser lexer generator :-], version ";
  print_version_num ()

let specs = [
  "-", Arg.Unit (fun () -> opt_infile := Some "-"),
  " Read input from stdin";
  "-intf", Arg.Set opt_parse_intf,
  " Parse an interface (by default: use extension or parse an implementation)";
  "-no-reduce-filter", Arg.Set opt_no_reduce_filter,
  " Do not print reachable reduce-filter patterns";
  "-stack-items", Arg.Set opt_stack_items,
  " Print items of all states on stack";
  "-v",  Arg.Unit print_version_string,
  " Print version and exit";
  "-version",  Arg.Unit print_version_string,
  " Print version and exit";
  "-vnum",  Arg.Unit print_version_num,
  " Print version number and exit";
  "-dump-states", Arg.Set opt_dump_states,
  " Print state numbers for debugging purpose";
]

let () = Arg.parse specs (fun name -> opt_infile := Some name) usage

let grammar_filename =
  let filename, oc = Filename.open_temp_file "lrgrep-interpreter" "cmly" in
  output_string oc Interpreter_data.grammar;
  close_out oc;
  filename

module Grammar = MenhirSdk.Cmly_read.Read(struct let filename = grammar_filename end)

let () = Sys.remove grammar_filename

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

let print_item item =
  let open Info in
  let (prod, pos) = Item.prj item in
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

let print_items indent suffix items =
  Printf.printf "\t\t%s\x1b[0;32m[%s" (String.make indent ' ') suffix;
  let pad = String.make (indent + 1 + String.length suffix) ' ' in
  List.iteri (fun i item ->
    if i > 0 then
      Printf.printf "\n\t\t%s" pad;
    Printf.printf " / %s" (print_item item);
  ) items;
  Printf.printf "]\n"

let get_token =
  let state = Lexer_raw.make Lexer_raw.keyword_table in
  let rec extract_token = function
    | Lexer_raw.Return tok -> tok
    | Lexer_raw.Refill k -> extract_token (k ())
    | Lexer_raw.Fail (err, loc) ->
      Format.eprintf "%a\n%!"
        Location.print_report (Lexer_raw.prepare_error loc err);
      exit 1
  in
  fun lexbuf -> extract_token (Lexer_raw.token_without_comments state lexbuf)

let do_parse
    (type a)
    (checkpoint : Lexing.position -> a Parser_raw.MenhirInterpreter.checkpoint)
    lexbuf
  =
  let module I = Parser_raw.MenhirInterpreter in
  let rec loop : _ I.env -> _ I.checkpoint -> _ = fun env -> function
    | I.Shifting (_, _, _) | I.AboutToReduce (_, _) as cp ->
      loop env (I.resume cp)
    | I.Accepted _ -> None
    | I.Rejected -> assert false
    | I.HandlingError _ ->
      Some env
    | I.InputNeeded env' as cp ->
      match get_token lexbuf with
      | Parser_raw.EOF -> Some env'
      | token ->
        loop env' (I.offer cp (token, lexbuf.lex_start_p, lexbuf.lex_curr_p))
  in
  match checkpoint lexbuf.lex_curr_p with
  | I.InputNeeded env as cp -> loop env cp
  | _ -> assert false


let rec get_states acc env =
  let module I = Parser_raw.MenhirInterpreter in
  let loc =
    match I.top env with
    | Some (I.Element (_,_,start,stop)) -> Some (start, stop)
    | None -> None
  in
  let lr1 = Index.of_int Info.Lr1.n (I.current_state_number env) in
  let acc = (lr1, loc) :: acc in
  match I.pop env with
  | None -> acc
  | Some env' -> get_states acc env'

let get_states env =
  List.rev (get_states [] env)

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
    Printf.printf "\t\t%s\x1b[1;33m↱ %s\n"
      (String.make n ' ') (Option.get (print_lr1 config.top));
    if not !opt_no_reduce_filter then (
      let suffix = match config.rest with
        | [] | [_] -> [config.top]
        | _ :: rest -> config.top :: rest
      in
      let suffix =
        Utils.Misc.string_concat_map "; "
          (fun lr1 -> Option.get (print_lr1 lr1))
          suffix
      in
      print_items (n + 2) suffix (Lr1.lr0_items config.top);
    );
    acc

let process_result lexbuf = function
  | None -> print_endline "Successful parse"
  | Some env ->
    let stack = get_states env in
    (*Format.printf "let stack = [%s]\n"
      (String.concat ";" (List.map string_of_int (List.map fst stack)));*)
    Format.printf "%a, parser stack (most recent first):\n%!"
      Location.print_loc (Location.curr lexbuf);
    let outer = ref [] in
    List.iteri (fun i (state, loc) ->
        if i = 0 then (
          let top, _ = List.hd stack in
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
        let items = Info.Lr1.lr0_items state in
        if (i = 0 && not !opt_no_reduce_filter) then (
          print_items 0 "" items;
        ) else if !opt_stack_items then (
          print_string "\x1b[0;36m";
          List.iter
            (fun item -> print_endline ("\t\t  [" ^ print_item item ^ "]"))
            items;
        );
        print_string "\x1b[0m- ";
        print_string (
          match loc with
          | None -> "entrypoint\t"
          | Some loc -> print_loc loc
        );
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
           (List.map (fun (idx, _) -> string_of_int (idx : _ index :> int))
              stack))

let () =
  match !opt_infile with
  | None | Some "" ->
    Format.eprintf "No input provided, stopping now.\n";
    Arg.usage specs usage;
  | Some file ->
    let is_intf = !opt_parse_intf || Filename.check_suffix file "i" in
    let ic, filename, close_ic =
      if file = "-" then
        (stdin, "<stdin>", false)
      else
        (open_in_bin file, file, true)
    in
    let lexbuf =
      let lexbuf = Lexing.from_channel ~with_positions:true ic in
      Lexing.set_filename lexbuf filename;
      lexbuf
    in
    if is_intf then
      process_result lexbuf
        (do_parse Parser_raw.Incremental.interface lexbuf)
    else
      process_result lexbuf
        (do_parse Parser_raw.Incremental.implementation lexbuf);
    if close_ic then
      close_in_noerr ic
