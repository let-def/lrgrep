open Utils
open Misc

(* Command-line parsing. *)

let opt_grammar_file = ref None

type enumerate_format =
  | Efmt_raw
  | Efmt_json

let opt_enumerate_format = ref Efmt_raw

let opt_enumerate_entrypoint = ref []

let specs = [
  "-g", Arg.String (fun x -> opt_grammar_file := Some x),
  " <file.cmly>  Path to the Menhir compiled grammar to analyse (*.cmly)";
  "-format", Arg.String (function
      | "raw" -> opt_enumerate_format := Efmt_raw
      | "json" -> opt_enumerate_format := Efmt_json
      | arg ->
        Printf.eprintf
          "-format: invalid value %S\n\
           From least verbose to most verbose:\n\
           - raw (default): custom format, with one sentence group per line\n\
           - json: a line-delimited sequence of json objects\n"
          arg;
      exit 1
  ),
  " <raw|json> Format used to output enumeration";
  "-entrypoint", Arg.String (push opt_enumerate_entrypoint),
  " <start-symbol> Enumerate starting from this entrypoint\n\
  \ Default is to enumerate all start symbols.";
]

module Run(P : sig val grammar_file : string end)() = struct
  let () = Stopwatch.step Stopwatch.main "Beginning"

  module Grammar = MenhirSdk.Cmly_read.Read(struct let filename = P.grammar_file end)

  let () = Stopwatch.step Stopwatch.main "Loaded grammar"

  module Info = Mid.Info.Make(Grammar)
  module Viable = Mid.Viable_reductions.Make(Info)()
  module Regexp = Mid.Regexp.Make(Info)(Viable)
  module Transl = Mid.Transl.Make(Regexp)
  module Reachability = Mid.Reachability.Make(Info)()
  module Lrc = Mid.Lrc.Make(Info)(Reachability)

  let translate_entrypoints prj loc err symbols =
    let unhandled = ref [] in
    let result =
      List.filter_map (fun sym ->
        let result = Hashtbl.find_opt Info.Lr1.entrypoints (prj sym) in
        if Option.is_none result then push unhandled sym;
        result
      ) symbols
    in
    match List.rev !unhandled with
    | [] -> IndexSet.of_list result
    | first :: rest as all ->
      Printf.ksprintf (fun msg -> err (loc first) msg)
        "Unknown start symbol%s %s.\nValid start symbols are %s."
        (if rest = [] then "" else "s")
        (string_concat_map ", " prj all)
        (string_concat_map ", " Fun.id
          (List.sort String.compare
            (Hashtbl.fold (fun key _ acc -> key :: acc) Info.Lr1.entrypoints [])))

  let () =
    let entrypoints =
      match List.rev !opt_enumerate_entrypoint with
      | [] -> Info.Lr1.all_entrypoints
      | syms ->
        translate_entrypoints Fun.id (Fun.const ())
          (fun () msg ->
            Printf.eprintf "Invalid -enumerate-entrypoint: %s" msg;
            exit 1)
          syms
    in
    let module Lrc = Mid.Lrc.Close(Info)(Lrc)(struct
      let entrypoints = indexset_bind entrypoints Lrc.lrcs_of_lr1
    end) in
    let module Reachable = Mid.Reachable_reductions2.Make(Info)(Viable)(Lrc)() in
    let module Failure = Mid.Reachable_reductions2.FailureNFA(Info)(Viable)(Lrc)(Reachable)() in
    let module Enum = Enum.Make(Info)(Reachability)(Viable)(Lrc)(Reachable)() in
    let output = match !opt_enumerate_format with
      | Efmt_raw -> Enum.output_raw
      | Efmt_json -> Enum.output_json
    in
    Enum.enumerate (output stdout)
end

let run = function
  | [] -> (
    match !opt_grammar_file with
    | None ->
      Error "No grammar provided (-g)"
    | Some grammar_file ->
      let module _ : sig end = Run(struct
        let grammar_file = grammar_file
      end)() in
      Ok ()
  )
  | x :: _ ->
    Error (Printf.sprintf "Unexpected argument: %s" x)
