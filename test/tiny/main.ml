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


let run input =
  match parse Parser.Incremental.sentence input with
  | Ok _ -> Printf.printf "%s => OK\n" input
  | Error e -> Printf.printf "%s => ERROR %s\n" input e

(* -------------------------------------------------------------------------- *)

(* Test driver. *)

(* [check input expectation] runs the parser on [input] and verifies that the
   result matches [expectation]. It prints the produced output as well as a
   PASS/FAIL verdict, and returns [true] when the expectation is met. *)

let check (input : string) (expectation : (char list, string) result -> bool) : bool =
  let result = parse Parser.Incremental.sentence input in
  let verdict = expectation result in
  (* Show the actual output, mirroring [run]. *)
  run input;
  Printf.printf "%s: %s\n" input (if verdict then "PASS" else "FAIL");
  verdict

(* [is_ok] holds when the parse succeeds. *)
let is_ok : (char list, string) result -> bool = function
  | Ok _ -> true
  | Error _ -> false

(* [is_error] holds when the parse fails. *)
let is_error : (char list, string) result -> bool = function
  | Ok _ -> false
  | Error _ -> true

(* [driver] runs a few well-known inputs and checks that the parser accepts the
   well-formed ones and rejects the malformed ones. It returns [true] when all
   checks pass. *)
let driver () : bool =
  let cases =
    [ ("()", is_ok);
      ("(a)", is_ok);
      ("(", is_error);
      ("(a", is_error) ]
  in
  List.for_all (fun (input, expectation) -> check input expectation) cases

let () =
  match Sys.argv with
  | [| _ |] ->
      let ok = driver () in
      exit (if ok then 0 else 1)
  | [| _; input |] -> run input
  | _ -> Printf.eprintf "main.exe [INPUT]"; exit 1
