(* Incremental-parsing entry point for [Parser.parse_lexer_definition],
   reporting syntax errors via [Parse_errors.error_messages] (generated from
   parse_errors.lrgrep) instead of the bare, positionless [Parser.Error]
   exception that the classic (non-incremental) entry point raises. *)

module I = Parser.MenhirInterpreter

let parse_lexer_definition
    (state : Lexer.lexer_state) (lexbuf : Lexing.lexbuf)
  : (Syntax.lexer_definition, Lexing.position * string option) result
  =
  let last_token = ref (Parser.EOF, Lexing.dummy_pos, Lexing.dummy_pos) in
  let get_token () =
    let tok = Lexer.main state lexbuf in
    let triple = (tok, lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p) in
    last_token := triple;
    triple
  in
  (* [env] tracks the last [InputNeeded] environment (the state right before
     the token that ends up rejected), not [HandlingError]'s own bundled
     env - the generated [Parse_errors.error_messages] expects the former,
     matching what [lrgrep interpret]/[enumerate] simulate. Mirrors
     test/tiny/main.ml, which discards [HandlingError]'s payload the same
     way. *)
  let rec loop env = function
    | I.InputNeeded env' as cp ->
      loop env' (I.offer cp (get_token ()))
    | I.Shifting _ | I.AboutToReduce _ as cp ->
      loop env (I.resume cp)
    | I.Accepted x -> Ok x
    | I.Rejected -> assert false
    | I.HandlingError _ ->
      let (_, startp, _) = !last_token in
      Error (startp, Parse_errors.error_messages env !last_token)
  in
  match Parser.Incremental.parse_lexer_definition lexbuf.Lexing.lex_curr_p with
  | I.InputNeeded env as cp -> loop env (I.offer cp (get_token ()))
  | _ -> assert false
