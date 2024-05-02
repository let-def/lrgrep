open Utils
open Misc

(* Command-line parsing. *)

let opt_grammar_file = ref None

type precision =
  | Enum_lr0
  | Enum_lr1
  | Enum_goto

let opt_precision = ref Enum_lr0

type enumerate_format =
  | Efmt_raw
  | Efmt_json

let opt_enumerate_format = ref Efmt_raw

let opt_enumerate_entrypoint = ref []

let specs = [
  "-g", Arg.String (fun x -> opt_grammar_file := Some x),
  " <file.cmly>  Path to the Menhir compiled grammar to analyse (*.cmly)";
  "-precision", Arg.String (function
      | "lr0" -> opt_precision := Enum_lr0
      | "lr1" -> opt_precision := Enum_lr1
      | "goto" -> opt_precision := Enum_goto
      | arg ->
        Printf.eprintf
          "-precision: invalid value %S\n\
           Valid values, from least to most verbose:\n\
           - lr0 (default): enumerate sentences to reach lr0 states\n\
           - lr1: enumerate sentences to reach lr1 states\n\
           - goto: enumerate sentences to follow all goto transitions\n"
          arg;
      exit 1
  ),
  " <lr0|lr1|goto> Precision of the failing configurations to cover";
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
    let initials =
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
      let initials = indexset_bind initials Lrc.lrcs_of_lr1
    end) in
    let module Reachable = Mid.Reachable_reductions2.Make(Info)(Viable)(Lrc)() in
    let module Enum = Enum.Make(Info)(Reachability)(Viable)(Lrc)(Reachable) in
    let output = match !opt_enumerate_format with
      | Efmt_raw -> Enum.output_raw
      | Efmt_json -> Enum.output_json
    in
    match !opt_precision with
    | Enum_lr0 ->
      Enum.enumerate
        ~cover:Info.Lr0.n
        ~index:(fun x -> Info.Lr1.to_lr0 (Enum.Coverage.lr1_of x))
        (output stdout)
    | Enum_lr1 ->
      Enum.enumerate
        ~cover:Info.Lr1.n
        ~index:(fun x -> Enum.Coverage.lr1_of x)
        (output stdout)
    | Enum_goto ->
      Enum.enumerate
        ~cover:Reachable.n
        ~index:(fun x -> x)
        (output stdout)
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
