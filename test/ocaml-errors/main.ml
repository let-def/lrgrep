(* Regression test for examples/ocaml/parser/errors.lrgrep, driven through a
   real incremental parse of [Ocaml_parser.Parser.Incremental.implementation]
   - exactly the entry point the actual OCaml compiler uses for ".ml" files -
   instead of just checking that the spec compiles. Each case is a
   deliberately malformed ".ml" snippet together with the exact message we
   intend for it - checked literally, character for character. *)

module I = Ocaml_parser.Parser.MenhirInterpreter

let format input =
  Ocaml_parser.Lexer.init ();
  let lexbuf = Lexing.from_string input in
  Lexing.set_filename lexbuf "<test>";
  let last_token =
    ref (Ocaml_parser.Parser.EOF, Lexing.dummy_pos, Lexing.dummy_pos)
  in
  let get_token () =
    let tok = Ocaml_parser.Lexer.token lexbuf in
    let triple = (tok, lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p) in
    last_token := triple;
    triple
  in
  (* [env] tracks the last [InputNeeded] environment, not [HandlingError]'s
     own bundled env - matches src/front/front_driver.ml and
     test/tiny/main.ml, and is what the generated [Errors.error_messages]
     expects. *)
  let rec loop env = function
    | I.InputNeeded env' as cp -> loop env' (I.offer cp (get_token ()))
    | I.Shifting _ | I.AboutToReduce _ as cp -> loop env (I.resume cp)
    | I.Accepted _ -> "OK"
    | I.Rejected -> assert false
    | I.HandlingError _ ->
      (match Ocaml_parser.Errors.error_messages env !last_token with
       | None -> "ERROR (no handler)"
       | Some msg -> "ERROR " ^ msg)
  in
  match Ocaml_parser.Parser.Incremental.implementation lexbuf.Lexing.lex_curr_p with
  | I.InputNeeded env as cp -> loop env (I.offer cp (get_token ()))
  | _ -> assert false

let driver () : bool =
  let cases =
    [ (* A well-formed implementation: confirms the incremental driver isn't
         itself breaking anything on the happy path. *)
      ("let x = 1\n", "OK");

      (* New in 5.5.1: `type t = external "name"` needs a quoted name after
         `external`; see errors.lrgrep's dedicated rule added for this. *)
      ("type t = external\n",
       "ERROR Expected a quoted external name after `external`");

      (* A keyword typed where a lowercase identifier is expected (here, a
         record field name) - exercises Lexer.as_keyword, the reverse
         token->keyword-spelling lookup added this session to fix a genuine
         bug (it didn't exist at all before, so this case couldn't have run
         until that fix landed). *)
      ("type t = { type : int }\n",
       "ERROR `type' is a keyword and cannot appear in this context (try type_)");

      (* First-class-module parameter type `(module M : S) -> t`: missing
         module type after `:`, and missing `->` after the closing `)`. *)
      ("type t = (module M :\n",
       "ERROR Expected module type after `:` in `(module M : ...)` parameter type");
      ("type t = (module M : S) int\n",
       "ERROR Expected `->` after type");

      (* Parenthesized polymorphic parameter type `('a 'b. t) -> u`: missing
         `.` after the type variables, missing type after `.`, and missing
         closing `)`. *)
      ("type t = ('a 'b -> int) -> int\n",
       "ERROR Expected `.` after type parameters");
      ("type t = ('a. int) ->\n",
       "ERROR Expected type after `->`");
      ("type t = ('a. int -> int\n",
       "ERROR Expected closing `)` after polymorphic type");

      (* Same polymorphic-type-annotated parameter, but on a pattern rather
         than in a function type: `(x : 'a 'b. t)`. *)
      ("let f (x : 'a . int\n",
       "ERROR Expected closing `)` after polymorphic type") ]
  in
  List.for_all (fun (input, expected) ->
      let actual = format input in
      let verdict = actual = expected in
      Printf.printf "input:    %s" input;
      Printf.printf "expected: %s\n" expected;
      Printf.printf "actual:   %s\n%!" actual;
      Printf.printf "=> %s\n%!" (if verdict then "PASS" else "FAIL");
      verdict) cases

let () =
  match Sys.argv with
  | [| _ |] ->
    let ok = driver () in
    exit (if ok then 0 else 1)
  | [| _; input |] -> print_endline (format input)
  | _ -> Printf.eprintf "main.exe [INPUT]"; exit 1
