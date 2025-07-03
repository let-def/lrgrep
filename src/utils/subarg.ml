(* An extension of Arg with sub-command support *)

open Arg

type spec = (Arg.key * Arg.spec * Arg.doc)

type command = {
  name : string;
  help : string;
  specs : spec list;
  enter : unit -> unit;
  anon : (string -> unit) option;
  commit : unit -> unit;
  abort : unit -> unit;
}

let usage_subcommands ?(current="") commands =
  let remove_help msg =
    String.split_on_char '\n' msg
    |> List.filter (function
        | "  -help  Display this list of options"
        | "  --help  Display this list of options" -> false
        | _ -> true
      )
    |> String.concat "\n"
  in
  ignore current;
  String.concat "\n" (
    Printf.sprintf "Commands:\n%s\n"
      (String.concat "\n"
         (List.map (fun cmd -> "  " ^ cmd.name ^ "  " ^ cmd.help) commands))
    ::
    List.map
      (fun cmd -> remove_help (Arg.usage_string cmd.specs (cmd.name ^ " options:")))
      commands
  )

let append_subcommands ?current msg = function
  | [] -> msg
  | commands -> msg ^ "\n" ^ usage_subcommands ?current commands

let usage_string specs commands errmsg =
  append_subcommands (Arg.usage_string specs errmsg) commands

let usage speclist commands errmsg =
  Printf.eprintf "%s"
    (usage_string speclist commands errmsg)

let command name help ?(enter=ignore) ?anon ?(commit=ignore) ?(abort=ignore) specs =
  {name; help; specs; enter; anon; commit; abort}

let parse_argv ?current argv specs0 commands anon0 msg =
  let abort = ref ignore in
  let current_name = ref "" in
  try
    let specs = ref specs0 in
    let commit = ref ignore in
    let anon = ref anon0 in
    Arg.parse_argv_dynamic ?current argv specs begin fun arg ->
      match List.find_opt (fun cmd -> cmd.name = arg) commands with
      | None -> !anon arg
      | Some command ->
        !commit ();
        current_name := command.name;
        specs := command.specs @ specs0;
        command.enter ();
        anon := Option.value command.anon ~default:anon0;
        commit := command.commit;
        abort := command.abort
    end msg;
    !commit ()
  with
  | Bad msg ->
    !abort ();
    raise (Bad (append_subcommands ~current:!current_name msg commands))
  | Help msg ->
    !abort ();
    raise (Help (append_subcommands ~current:!current_name msg commands))

let parse specs commands anon usage =
  try
    parse_argv Sys.argv specs commands anon usage
  with
  | Bad msg -> Printf.eprintf "%s" msg; exit 2
  | Help msg -> Printf.printf "%s" msg; exit 0
