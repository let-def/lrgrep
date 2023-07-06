let usage =
  "Usage: ocaml-lrgrep <file>\n\
   --------------------------\n\n\
   ocaml-lrgrep is an alternative frontend for OCaml.\n\
   It is used to prototype better syntax error messages.\n\
   \n\
   To use it, pass `-pp ocaml-lrgrep` to ocaml compilers.\n\
   \n\
   Usage for tests: ocaml-lrgrep --test <file1> <file2...>\n\
   -------------------------------------------------------\n\n\
   In this mode, ocaml-lrgrep will parse each file in order.\n\
   Errors are reported on stdout and the AST is discarded.\n\
   Output for each file is separated by lines starting with \"---\".
   "

exception Abort_parsing

let oprintf oc fmt =
  Format.fprintf (Format.formatter_of_out_channel oc) fmt

let error_and_exit oc lexbuf msg =
  oprintf oc "%a%!" Location.print_report {
    Location.
    kind = Location.Report_error;
    main = Location.msg ~loc:(Location.curr lexbuf) "%s" msg;
    sub = [];
  };
  raise Abort_parsing

let get_token oc lexstate lexbuf =
  let rec extract_token = function
    | Lexer_raw.Return tok -> tok
    | Lexer_raw.Refill k -> extract_token (k ())
    | Lexer_raw.Fail (err, loc) ->
      oprintf oc "%a\n%!" Location.print_report
        (Lexer_raw.prepare_error loc err);
      raise Abort_parsing
  in
  let tok = extract_token (Lexer_raw.token_without_comments lexstate lexbuf) in
  (tok, lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p)

let parse
    (type a)
    oc
    (checkpoint : Lexing.position -> a Parser_raw.MenhirInterpreter.checkpoint)
    (lexbuf : Lexing.lexbuf)
    : a
  =
  Location.input_name := lexbuf.lex_start_p.pos_fname;
  let lexstate = Lexer_raw.make Lexer_raw.keyword_table in
  let module I = Parser_raw.MenhirInterpreter in
  let module PE = Lrgrep_runtime.Interpreter(Parse_errors.Table_error_message)(I) in
  let rec loop : _ I.env -> _ -> _ I.checkpoint -> _ = fun env tok -> function
    | I.InputNeeded env' as cp ->
      let tok' = get_token oc lexstate lexbuf in
      loop env' tok' (I.offer cp tok')
    | I.Shifting (_, _, _) | I.AboutToReduce (_, _) as cp ->
      loop env tok (I.resume cp)
    | I.Accepted x -> x
    | I.Rejected -> assert false
    | I.HandlingError _ ->
      match PE.run env with
      | [] -> error_and_exit oc lexbuf
                "Syntax error (no handler for it)"
      | matches ->
        if false then
          Printf.eprintf "Matches: %s\n"
            (String.concat ", " (List.map (fun (x, _) -> string_of_int x) matches));
        let rec loop = function
          | [] -> error_and_exit oc lexbuf
                    "Syntax error (partial handler did not handle the case)"
          | m :: ms ->
            match Parse_errors.execute_error_message m tok with
            | None -> loop ms
            | Some err -> error_and_exit oc lexbuf err
        in
        loop matches
  in
  match checkpoint lexbuf.lex_curr_p with
  | I.InputNeeded env as cp ->
    let tok = get_token oc lexstate lexbuf in
    loop env tok (I.offer cp tok)
  | _ -> assert false

let pp_output kind ast =
  Pparse.write_ast kind "/dev/fd/1" ast

let open_input path =
  let ic = open_in path in
  let lexbuf = Lexing.from_channel ~with_positions:true ic in
  Lexing.set_filename lexbuf path;
  let is_interface = Filename.check_suffix path "i" in
  ic, lexbuf, is_interface

let pp_parse kind entrypoint lexbuf =
  pp_output kind (parse stderr entrypoint lexbuf)

let pp_main path =
  let ic, lexbuf, is_interface = open_input path in
  match
    if is_interface then
      pp_parse Pparse.Signature Parser_raw.Incremental.interface lexbuf
    else
      pp_parse Pparse.Structure Parser_raw.Incremental.implementation lexbuf
  with
  | () -> close_in ic
  | exception Abort_parsing ->
    close_in ic;
    exit 1

let test_main path =
  Printf.printf "--- PARSING %s\n" path;
  let ic, lexbuf, is_interface = open_input path in
  try
    if is_interface then
      ignore (parse stdout Parser_raw.Incremental.interface lexbuf)
    else
      ignore (parse stdout Parser_raw.Incremental.implementation lexbuf);
    close_in ic;
    Printf.printf "--- OK %s\n" path;
  with Abort_parsing ->
    close_in ic;
    Printf.printf "--- KO %s\n" path

let main () =
  let len = Array.length Sys.argv in
  if len = 2 then
    pp_main Sys.argv.(1)
  else if len >= 2 && Sys.argv.(1) = "--test" then
    for i = 2 to len - 1 do
      test_main Sys.argv.(i);
      print_newline ();
    done
  else (
    prerr_endline usage;
    exit 2;
  )

let () = main ()
