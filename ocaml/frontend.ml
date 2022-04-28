let usage =
  "usage: ocaml-lrgrep <infile> <outfile>\n\
   ocaml-lrgrep is an alternative frontend for OCaml.\n\
   It is used to prototype better syntax error messages.\n\
   \n\
   To use it, pass `-pp ocaml-lrgrep` to ocaml compilers."

let infile =
  match Sys.argv with
  | [|_; infile|] -> infile
  | _ ->
    prerr_endline usage;
    exit 1

let () =
  Location.input_name := infile

let lexbuf =
  let ic = open_in infile in
  let lexbuf = Lexing.from_channel ~with_positions:true ic in
  Lexing.set_filename lexbuf infile;
  lexbuf

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
    (kind : a Pparse.ast_kind)
    (checkpoint : Lexing.position -> a Parser_raw.MenhirInterpreter.checkpoint)
  =
  let module I = Parser_raw.MenhirInterpreter in
  let module PE = Analyser_def.Interpreter(Parse_errors)(I) in
  let error_and_exit msg =
    let loc = Location.curr lexbuf in
    let report = {
      Location.
      kind = Location.Report_error;
      main = Location.msg ~loc "%s" msg;
      sub = [];
    } in
    Format.eprintf "%a\n" Location.print_report report;
    exit 1
  in
  let wrap_tok lexbuf tok =
    (tok, lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p)
  in
  let rec loop : _ I.env -> _ -> _ I.checkpoint -> _ = fun env tok -> function
    | I.InputNeeded env' as cp ->
      let tok' = get_token lexbuf in
      loop env' tok' (I.offer cp (wrap_tok lexbuf tok'))
    | I.Shifting (_, _, _) | I.AboutToReduce (_, _) as cp ->
      loop env tok (I.resume cp)
    | I.Accepted x -> x
    | I.Rejected -> assert false
    | I.HandlingError _ ->
      match PE.run env with
      | None -> error_and_exit "Syntax error (no handler for it)"
      | Some state ->
        error_and_exit (Parse_errors.execute state tok)
  in
  let start cp =
    match cp with
    | I.InputNeeded env ->
      let tok = get_token lexbuf in
      loop env tok (I.offer cp (wrap_tok lexbuf tok))
    | _ -> assert false
  in
  Pparse.write_ast kind "/dev/fd/1"
    (start (checkpoint lexbuf.lex_curr_p))

let () =
  let is_interface = Filename.check_suffix infile "i" in
  if is_interface then
    do_parse Pparse.Signature Parser_raw.Incremental.interface
  else
    do_parse Pparse.Structure Parser_raw.Incremental.implementation
