let parse
    (type a)
    (checkpoint : Lexing.position -> a Parser.MenhirInterpreter.checkpoint)
    (input : string)
    : (a, string) result
  =
  let module I = Parser.MenhirInterpreter in
  let idx = ref 0 in
  let last_token = ref None in
  let get_token () =
    let i = !idx in
    if i >= String.length input then
      Parser.EOF
    else (
      idx := i + 1;
      match input.[i] with
      | '(' -> OPAREN
      | ')' -> CPAREN
      | c -> C c)
  in
  let get_token () =
    let tok = get_token () in
    last_token := Some tok;
    (tok, Lexing.dummy_pos, Lexing.dummy_pos)
  in
  let s_token = function
    | Parser.OPAREN -> '('
    | CPAREN -> ')'
    | C c -> c
    | EOF -> '$'
  in
  let report msg =
    let token = Option.fold ~none:'^' ~some:s_token !last_token in
    Error (Printf.sprintf "Error at '%c': %s" token msg)
  in
  let rec loop : _ I.env -> _ -> _ I.checkpoint -> _ = fun env tok -> function
    | I.InputNeeded env' as cp ->
      let tok' = get_token () in
      loop env' tok' (I.offer cp tok')
    | I.Shifting (_, _, _) | I.AboutToReduce (_, _) as cp ->
      loop env tok (I.resume cp)
    | I.Accepted x -> Ok x
    | I.Rejected -> assert false
    | I.HandlingError _ ->
      match Parse_errors.error_message env tok with
      | None -> report "Syntax error (no handler)"
      | Some err -> report err
  in
  match checkpoint Lexing.dummy_pos with
  | I.InputNeeded env as cp ->
    let tok = get_token () in
    loop env tok (I.offer cp tok)
  | _ -> assert false


(* [format input] is the exact one-line output produced for [input]. *)
let format input =
  match parse Parser.Incremental.sentence input with
  | Ok _ -> "=> OK"
  | Error e -> "=> ERROR " ^ e

let run input =
  Printf.printf "%s\n" (format input)

(* -------------------------------------------------------------------------- *)

(* Test driver. *)

(* The expected outputs assume that the rule scope is SHARED across the clauses
   that are tried during a single invocation of [error_message] (so that a
   side-effect in the always-partial clause is visible to the succeeding
   clause). Under the current codegen the scope is instead re-created for every
   clause, so the [assert !n = 1] in the succeeding clauses fails and the driver
   crashes. That crash is the expected signal that the codegen needs fixing. *)

let driver () : bool =
  let cases =
    [ (* (R6) success case: the failing token is EOF ('$'), so the lookahead
         partial set [flag] and the ref-dependent partial succeeded. *)
      ("(", "=> ERROR Error at '$': A (eof, flag set)");
      (* (R6) fail case: the failing token is '(' (not EOF), so [flag] is not
         set and the ref-dependent partial fails; the total rule for A then
         succeeds, asserting [n] = 1 and setting it to 2. *)
      ("(a(", "=> ERROR Error at '(': A: unclosed '('");
      (* (R4) a %partial rule that succeeds, for the "expecting '('" situation. *)
      (")", "=> ERROR Error at ')': B: expecting '('");
      (* (R5) a total rule that succeeds, for the "expecting EOF" situation. *)
      ("(a)x", "=> ERROR Error at 'x': C: expecting EOF") ]
  in
  List.for_all (fun (input, expected) ->
      let actual = format input in
      let verdict = actual = expected in
      Printf.printf "input:    %s\n" input;
      Printf.printf "expected: %s\n" expected;
      Printf.printf "actual:   %s\n%!" actual;
      Printf.printf "=> %s\n%!" (if verdict then "PASS" else "FAIL");
      verdict) cases

let () =
  match Sys.argv with
  | [| _ |] ->
      let ok = driver () in
      exit (if ok then 0 else 1)
  | [| _; input |] -> run input
  | _ -> Printf.eprintf "main.exe [INPUT]"; exit 1
