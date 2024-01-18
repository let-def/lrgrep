module Interpreter = Parser.MenhirInterpreter
module Error_analyzer =
  Lrgrep_runtime.Interpreter(Parse_errors.Table_error_message)(Interpreter)

let rec parse
    (lexbuf : Lexing.lexbuf)
    (last_token : Parser.token * Lexing.position * Lexing.position)
    (last_env : _ Interpreter.env)
    (checkpoint : 'a Interpreter.checkpoint)
  : ('a, string) result
  =
  match checkpoint with
  | InputNeeded _ -> consume lexbuf checkpoint
  | Shifting (_, _, _) | AboutToReduce (_, _) as cp ->
    parse lexbuf last_token last_env (Interpreter.resume cp)
  | Accepted x -> Ok x
  | Rejected -> assert false
  | HandlingError _ ->
    handle_error last_token last_env

and handle_error last_token last_env =
  match Error_analyzer.run last_env with
  | [] -> Result.Error "Syntax error (no handler for it)"
  | matches -> (
      match List.find_map (fun m -> Parse_errors.execute_error_message m Lexing.dummy_pos last_token) matches with
      | None -> Result.Error "Syntax error (partial handler did not handle the case)"
      | Some err -> Result.Error err
    )

and consume lexbuf = function
  | Interpreter.InputNeeded env as checkpoint ->
    begin match Lexer.token lexbuf with
    | raw_token ->
      let token = (raw_token, lexbuf.lex_start_p, lexbuf.lex_curr_p) in
      parse lexbuf token env (Interpreter.offer checkpoint token)
    | exception Lexer.Error msg ->
      Result.Error msg
    end
  | _ -> assert false

let process (line : string) =
  let linebuf = Lexing.from_string line in
  match consume linebuf (Parser.Incremental.main linebuf.lex_start_p) with
  | Result.Ok value ->
    Printf.printf "%d\n%!" value
  | Result.Error msg ->
    Printf.fprintf stderr "%s%!" msg

let process (optional_line : string option) =
  match optional_line with
  | None -> ()
  | Some line -> process line

let rec repeat channel =
  (* Attempt to read one line. *)
  let optional_line, continue = Lexer.line channel in
  process optional_line;
  if continue then
    repeat channel

let () =
  repeat (Lexing.from_channel stdin)

