(* An extension of Arg with sub-command support *)

open Arg
open Misc

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
      |> (fun x -> x @ [""])
      |> String.concat "\n"
  in
  ignore current;
  String.concat "" (
    Printf.sprintf "Commands:\n%s\n\n"
      (String.concat "\n"
         (List.map (fun cmd -> "  " ^ cmd.name ^ "  " ^ cmd.help) commands))
    ::
    List.filter_map
      (fun cmd ->
         if list_is_empty cmd.specs then
           None
         else
           let msg = Arg.usage_string cmd.specs (cmd.name ^ " options:") in
           Some (remove_help msg))
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

let parse_argv ?(current=Arg.current) argv specs0 commands
    ?default ?(warn_default=ignore) ?(no_subcommand=ignore)
    anon0 msg =
  let command_with_name name cmd = cmd.name = name in
  let default = Option.map (fun name ->
      try List.find (command_with_name name) commands
      with Not_found ->
        invalid_arg ("Subarg.parse_argv: default command (" ^ name ^
                     ") is not part of command list")
    ) default
  in
  let abort = ref ignore in
  let current_name = ref "" in
  let specs = ref specs0 in
  let commit = ref ignore in
  let anon = ref anon0 in
  let set_command command =
    !commit ();
    current_name := command.name;
    specs := command.specs @ specs0;
    command.enter ();
    anon := Option.value command.anon ~default:anon0;
    commit := command.commit;
    abort := command.abort
  in
  let run () =
      Arg.parse_argv_dynamic ~current argv specs begin fun arg ->
        match List.find_opt (command_with_name arg) commands with
        | None -> !anon arg
        | Some command -> set_command command
      end msg;
  in
  try
    begin
      try run ()
      with Bad _ as exn when !current > 0 && !current < Array.length argv &&
                             !current_name = "" &&
                             String.starts_with ~prefix:"-" argv.(!current) ->
        match default with
        | Some command when
            List.exists (fun (name, _, _) -> name = argv.(!current))
              command.specs ->
          set_command command;
          warn_default ();
          decr current;
          run ()
        | _ -> raise exn
    end;
    !commit ();
    if !current_name = "" then
      no_subcommand ()
  with
  | Bad msg ->
    !abort ();
    raise (Bad (append_subcommands ~current:!current_name msg commands))
  | Help msg ->
    !abort ();
    raise (Help (append_subcommands ~current:!current_name msg commands))

let parse specs commands
    ?default ?warn_default ?no_subcommand
    anon usage =
  try
    parse_argv Sys.argv specs commands
      ?default ?warn_default ?no_subcommand
      anon usage
  with
  | Bad msg -> Printf.eprintf "%s" msg; exit 2
  | Help msg -> Printf.printf "%s" msg; exit 0
