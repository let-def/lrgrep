open Utils
open Misc

let name_string = "The Menhir parser lexer generator :-]"
let version_string = "0.1"

let usage command =
  Printf.sprintf
    "lrgrep, a menhir lexer\n\
     usage: %s [compile | enumerate | interpret] [options] <source>\n\
     arguments for %s command:"
    Sys.argv.(0) command

let print_version_num () =
  print_endline version_string;
  exit 0

let print_version_string () =
  print_string name_string;
  print_string ", version ";
  print_version_num ()

(* The action to perform during this run of LRgrep *)
type command =
  | Compile
  | Enumerate
  | Interpret

let command =
  if Array.length Sys.argv < 2 then Compile
  else (
    incr Arg.current;
    match Sys.argv.(1) with
    | "compile" -> Compile
    | "enumerate" -> Enumerate
    | "interpret" -> Interpret
    | _ ->
      decr Arg.current;
      Compile
  )

let base_specs = [
  "-version", Arg.Unit print_version_string,
  " Print version and exit";
  "-vnum", Arg.Unit print_version_num,
  " Print version number and exit";
  "-v", Arg.Unit (fun () -> incr Stopwatch.verbosity),
  " Increase log/profile verbosity level";
]

(* Command dispatch *)

let parse_args specs name run =
  let anon = ref [] in
  let specs = base_specs @ specs in
  let usage = usage name in
  Arg.parse specs (push anon) usage;
  match run (List.rev !anon) with
  | Ok () -> ()
  | Error msg ->
    Arg.usage specs (usage ^ "\n" ^ msg);
    exit 1

let () = match command with
  | Compile ->
    parse_args Compile_command.specs "compile" Compile_command.run
  | Enumerate ->
    parse_args Enum_command.specs "enumerate" Enum_command.run
  | Interpret ->
    parse_args Interpret_command.specs "interpret" Interpret_command.run
