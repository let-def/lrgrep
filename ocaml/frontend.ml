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
    footnote = None;
  };
  raise Abort_parsing

let get_token oc lexbuf =
  match Lexer_raw.token lexbuf with
  | exception Lexer_raw.Error (err, loc) ->
    oprintf oc "%a\n%!" Location.print_report
      (Lexer_raw.prepare_error loc err);
    raise Abort_parsing
  | tok ->
    (tok, lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p)

let parse
    (type a)
    oc
    (checkpoint : Lexing.position -> a Parser_raw.MenhirInterpreter.checkpoint)
    (lexbuf : Lexing.lexbuf)
    : a
  =
  let initial = lexbuf.lex_start_p in
  Location.input_name := initial.pos_fname;
  let module I = Parser_raw.MenhirInterpreter in
  let rec loop : a I.env -> _ -> a I.checkpoint -> _ = fun env tok -> function
    | I.InputNeeded env' as cp ->
      let tok' = get_token oc lexbuf in
      loop env' tok' (I.offer cp tok')
    | I.Shifting (_, _, _) | I.AboutToReduce (_, _) as cp ->
      begin match I.resume cp with
      | cp' -> loop env tok cp'
      | exception (Syntaxerr.Error _ as exn) ->
        match Location.error_of_exn exn with
        | Some (`Ok err) ->
          Format.eprintf "@[%a@]@."
            Location.print_report err;
          exit 1
        | _ -> assert false
      end
    | I.Accepted x -> x
    | I.Rejected -> assert false
    | I.HandlingError _ ->
      match Parse_errors.lrgrep_run Parse_errors.lrgrep_program_error_message env with
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
            match Parse_errors.lrgrep_execute_error_message m initial tok with
            | None -> loop ms
            | Some err -> error_and_exit oc lexbuf err
        in
        loop matches
  in
  match checkpoint lexbuf.lex_curr_p with
  | I.InputNeeded env as cp ->
    let tok = get_token oc lexbuf in
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
