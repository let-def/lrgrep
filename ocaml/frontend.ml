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
  let error_and_exit msg =
    let loc = Location.curr lexbuf in
    let report = {
      Location.
      kind = Location.Report_error;
      main = Location.msg ~loc msg;
      sub = [];
    } in
    Format.eprintf "%a\n" Location.print_report report;
    exit 1
  in
  let rec loop : _ I.checkpoint -> _ = function
    | I.InputNeeded _ as cp ->
      let token = get_token lexbuf in
      loop (I.offer cp (token, lexbuf.lex_start_p, lexbuf.lex_curr_p))
    | I.Shifting (_, _, _) | I.AboutToReduce (_, _) as cp ->
      loop (I.resume cp)
    | I.Accepted x -> x
    | I.Rejected ->
      error_and_exit "Syntax error (no handler for it)"
    | I.HandlingError _ ->
      error_and_exit "Syntax error (no handler for it)"
  in
  Pparse.write_ast kind "/dev/fd/1"
    (loop (checkpoint lexbuf.lex_curr_p))

let () =
  let is_interface = Filename.check_suffix infile "i" in
  if is_interface then
    do_parse Pparse.Signature Parser_raw.Incremental.interface
  else
    do_parse Pparse.Structure Parser_raw.Incremental.implementation
