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
      | c -> C c
    )
  in
  let get_token () =
    let tok = get_token () in
    last_token := Some tok;
    (tok, Lexing.dummy_pos, Lexing.dummy_pos) in
  let s_token = function | Parser.OPAREN -> '(' | CPAREN -> ')' | C c -> c | EOF -> '$' in
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
      match Parse_errors.error_message env Lexing.dummy_pos tok with
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

let () =
  match Sys.argv with
  | [|_|] -> List.iter run (["("; "(a"; "()"; "(a)"])
  | [|_; input|] -> run input
  | _ -> Printf.eprintf "main.exe [INPUT]"; exit 1
