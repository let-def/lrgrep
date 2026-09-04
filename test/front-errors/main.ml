(* Regression test for lrgrep's own front-end syntax error messages
   (src/front/parse_errors.lrgrep), driven through
   [Front.Front_driver.parse_lexer_definition] exactly as the real [lrgrep]
   and [minilrgrep] executables do. Each case is a deliberately malformed
   ".lrgrep" snippet (as a plain OCaml string) together with the exact
   message we intend for it - checked literally, character for character. *)

let format input =
  let state = Front.Lexer.fresh_state () in
  let lexbuf = Lexing.from_string input in
  let lexbuf = Front.Lexer.prepare_lexbuf state lexbuf in
  Lexing.set_filename lexbuf "<test>";
  match Front.Front_driver.parse_lexer_definition state lexbuf with
  | Ok _ -> "OK"
  | Error (pos, msg) ->
    Printf.sprintf "ERROR %d.%d: %s"
      pos.Lexing.pos_lnum
      (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
      (match msg with None -> "(no handler)" | Some msg -> msg)

let driver () : bool =
  let cases =
    [ (* A well-formed spec: confirms the incremental driver isn't itself
         breaking anything on the happy path. *)
      ("rule foo = parse error\n| OPAREN { \"x\" }\n", "OK");

      (* Missing '=': "parse"/"error" are themselves valid parameter names
         (see parser.mly's [ident] production), so they get silently
         consumed as extra parameters and the error only surfaces at the
         following '|'. *)
      ("rule foo\nparse error\n| { \"x\" }\n",
       "ERROR 3.0: Missing '=' after the rule's name and parameters. Note: \
        'parse', 'error' and 'rule' are themselves valid parameter names \
        here, so if you wrote e.g. 'rule foo parse error ...', 'parse' and \
        'error' were consumed as extra parameters instead of the keywords \
        you meant - write 'rule foo = parse ...' instead.");

      (* Missing 'parse' keyword after '='. *)
      ("rule foo =\n| { \"x\" }\n",
       "ERROR 2.0: Expected the 'parse' keyword here (optionally preceded \
        by '{ <ocaml definitions> }'). A rule has the shape: 'rule <name> \
        <params>* = [{ <definitions> }] parse [error] [(<start symbols>)] \
        <clauses>*'.");

      (* Unclosed '[' in an (uncaptured) reduce-pattern. *)
      ("rule foo = parse error\n| OPAREN; [chars\n  { \"x\" }\n",
       "ERROR 3.8: Unclosed '['. Expected ']' to close this reduce-pattern \
        ('[ <pattern> ]').");

      (* Unclosed '[' in a captured reduce-pattern. *)
      ("rule foo = parse error\n| c = [chars\n  { \"x\" }\n",
       "ERROR 3.8: Unclosed '['. Expected ']' to close this captured \
        reduce-pattern ('name = [ <pattern> ]').");

      (* Unclosed '(' around a grouped sub-pattern. *)
      ("rule foo = parse error\n| (OPAREN\n  { \"x\" }\n",
       "ERROR 3.8: Unclosed '('. Expected ')' to close this grouped \
        sub-pattern.");

      (* Missing action after a clause's pattern. *)
      ("rule foo = parse error\n| OPAREN\n",
       "ERROR 3.0: Missing action for this clause. Every '| <pattern>' \
        must be followed by an action: '{ <ocaml expr> }', '%partial \
        { <ocaml expr option> }', or 'UNREACHABLE'.");

      (* '%partial' not followed by an action block. *)
      ("rule foo = parse error\n| OPAREN %partial\n",
       "ERROR 3.0: '%partial' must be followed by an action block: \
        '%partial { <ocaml expr option> }'.");

      (* A file with no rule at all. *)
      ("{\n}\n",
       "ERROR 3.0: A '.lrgrep' file needs at least one rule ('rule <name> \
        = parse ...'); this file has none, or ends before any rule is \
        written.");

      (* Unclosed '(' in a parameterized symbol's argument list. *)
      ("rule foo = parse error\n| [seq(a, b\n  { \"x\" }\n",
       "ERROR 3.8: Unclosed '(' in a parameterized symbol's argument \
        list, e.g. 'name(arg1, arg2)'. Expected ')' (or ',' to continue \
        the list).");

      (* Unclosed '(' in the start-symbols list after 'parse'. *)
      ("rule foo = parse error (a, b\n| { \"x\" }\n",
       "ERROR 2.0: Unclosed '(' in the list of start symbols after \
        'parse'. Expected ')' (or ',' to continue the list).");

      (* Unexpected trailing token after a rule's clauses. *)
      ("rule foo = parse error\n| OPAREN { \"x\" }\n)\n",
       "ERROR 3.0: Unexpected token after this rule's clauses. Expected \
        another '| <pattern> <action>' clause, a '%shortest [ ... ]' \
        group, a new 'rule ...', a trailing '{ <ocaml code> }', or the \
        end of the file.");

      (* Trailing ',' with nothing after, in a symbol's argument list. *)
      ("rule foo = parse error\n| [seq(a, b,)\n  { \"x\" }\n",
       "ERROR 2.12: Expected another symbol after ',' in this argument \
        list (or remove the trailing ',').");

      (* Trailing ',' with nothing after, in the start-symbols list. *)
      ("rule foo = parse error (a, b,)\n| { \"x\" }\n",
       "ERROR 1.29: Expected another start-symbol name after ',' \
        (or remove the trailing ',').");

      (* Trailing ',' with nothing after, in an '@' lookahead list. *)
      ("rule foo = parse error\n| OPAREN @ a, b,\n  { \"x\" }\n",
       "ERROR 3.8: Expected another symbol after ',' in this lookahead \
        list (or remove the trailing ',').");

      (* Trailing ';' with nothing after, in a sequence pattern. *)
      ("rule foo = parse error\n| OPAREN;\n  { \"x\" }\n",
       "ERROR 3.8: Expected another pattern after ';' \
        (or remove the trailing ';').");

      (* 'rule' with no name at all. *)
      ("rule = parse error\n| { \"x\" }\n",
       "ERROR 1.5: Expected the rule's name after 'rule'.");

      (* Empty start-symbols list: 'parse ()'. *)
      ("rule foo = parse error ()\n| { \"x\" }\n",
       "ERROR 1.24: Expected a start-symbol name after '(' here \
        (the list of start symbols cannot be empty).");

      (* '%shortest' not followed by '['. *)
      ("rule foo = parse error\n%shortest\n",
       "ERROR 3.0: Expected '[' after '%shortest'. '%shortest \
        [ <clauses> ]' groups a set of clauses to be tried with the \
        shortest-match policy.");

      (* Unclosed '%shortest [ ... '. *)
      ("rule foo = parse error\n%shortest [\n  | OPAREN { \"x\" }\n",
       "ERROR 4.0: Expected a clause ('| <pattern> <action>') or ']' to \
        close this '%shortest [ ... ]' group.");

      (* '/' not followed by a filter. *)
      ("rule foo = parse error\n| OPAREN /\n  { \"x\" }\n",
       "ERROR 3.8: Expected a filter after '/': '.', '_*', a symbol \
        name, or 'name: ...' to label the stack slot being matched.");

      (* 'name:' (in a filter) not followed by any filter symbol. *)
      ("rule foo = parse error\n| OPAREN /a:\n  { \"x\" }\n",
       "ERROR 3.8: Expected at least one filter symbol after ':' \
        (e.g. '.', '_*', or a symbol name).");

      (* 'name =' (a capture) not followed by a symbol or '['. *)
      ("rule foo = parse error\n| c =\n  { \"x\" }\n",
       "ERROR 3.8: Expected a symbol or '[' after 'name ='. A capture \
        is written 'name = <symbol>' or 'name = [ <pattern> ]'.") ]
  in
  List.for_all (fun (input, expected) ->
      let actual = format input in
      let verdict = actual = expected in
      if not verdict then (
        Printf.printf "input:\n%s\n" input;
        Printf.printf "expected: %s\n" expected;
        Printf.printf "actual:   %s\n%!" actual;
        Printf.printf "=> FAIL\n%!"
      );
      verdict)
    cases

let () =
  let ok = driver () in
  if ok then print_endline "All front-error cases passed.";
  exit (if ok then 0 else 1)
