open Printf
open Lexing
type triple = Parser.token * position * position
module I = Parser.MenhirInterpreter
type 'a env = 'a I.env
type 'a checkpoint = 'a I.checkpoint

(* -------------------------------------------------------------------------- *)

(* [print_syntax_error content triple env] constructs and prints a syntax
   error message. The message is constructed according to the rules in the
   file [errors.mlyl].

   [content] is the content of the file that we are currently reading.

   [env] is the environment carried by the last event (checkpoint) of the form
   [InputNeeded env].

   [triple] is the triple that was fed to the parser in response to this
   event; therefore, it should also be the last triple that was obtained from
   the lexer. *)

let print_syntax_error (content : string) (triple : triple) (env : _ env) =
  Errors.content := Some content; (* TODO *)
  match Errors.error_message env dummy_pos triple with
  | Some msg ->
      (* This syntax error has an explanation. Print it. *)
      eprintf "Syntax error.\n%s\n%!" msg
  | None ->
      (* This syntax error has no explanation. This should never happen
         if [lrgrep] has reported that we have complete coverage of the
         error situations. *)
      eprintf "Syntax error (no explanation).\n%!";
      exit 2

(* -------------------------------------------------------------------------- *)

(* [print_positions triple] prints the start and end positions stored in the
   triple [triple]. *)

let print_positions triple =
  let (_, startp, endp) = triple in
  eprintf "%s" (MenhirLib.LexerUtil.range (startp, endp))

(* -------------------------------------------------------------------------- *)

(* A lexer normally consists of just a [next] function, which returns the next
   token and updates the lexer's internal state. It is useful to also have a
   [last] function, which returns the last token that was produced by the
   lexer, and does not affect the lexer's internal state. The function [last]
   must not be called until the [next] function has been called once. *)

(* [remember] accepts a lexer [next] and returns a pair [(next, last)] of a
   [next] function and a [last] function, as described above. *)

let remember (type a b) (next : a -> b) : (a -> b) * (unit -> b) =
  let cell : b option ref = ref None in
  let next a =
    let b = next a in
    cell := Some b;
    b
  and last () =
    match !cell with
    | Some b ->
        b
    | None ->
        (* [last] must not be called until [next] has been called once. *)
        assert false
  in
  next, last

(* -------------------------------------------------------------------------- *)

(* [succeed v] is invoked when the parser succeeds and produces the semantic
   value [v]. In this demo, the parser produces a trivial semantic value,
   whose type is [unit]. *)

let succeed (v : unit) =
  ignore v;
  printf "Success.\n%!"

(* [fail content last checkpoint _] is invoked when the parser fails.

   [content] is the content of the file that we are currently reading.

   The function [last] allows us to retrieve the last token that was read.

   [checkpoint] is the last checkpoint of the form [InputNeeded _]; any
   reductions that took place after this point in time are discarded. *)

let fail content (last : unit -> triple) (checkpoint : _ checkpoint) (_) =
  match checkpoint with
  | InputNeeded env ->
      let triple = last() in
      print_positions triple; (* TODO make sure positions are correct *)
      print_syntax_error content triple env;
      exit 1
  | _ ->
      assert false

(* -------------------------------------------------------------------------- *)

(* [process filename] reads and parses the file [filename]. *)

let process (filename : string) =
  (* Read the file's entire content and create a lexing buffer. *)
  let content, lexbuf = MenhirLib.LexerUtil.read filename in
  (* Create an initial checkpoint for the parser. *)
  let start = Parser.Incremental.main (lexeme_start_p lexbuf) in
  (* Package our lexer and lexing buffer as a [next] function. *)
  let (next : unit -> triple) = I.lexer_lexbuf_to_supplier Lexer.token lexbuf in
  (* Remember the last token that was produced. *)
  let (next : unit -> triple), (last : unit -> triple) = remember next in
  (* Invoke the parser with success and failure functions [succeed] and
     [fail], lexer function [next], and initial checkpoint [start]. *)
  try
    I.loop_handle_undo succeed (fail content last) next start
  with
  | Lexer.Error ((message, _, _) as triple) ->
      print_positions triple;
      eprintf "%s\n%!" message;
      exit 1

(* -------------------------------------------------------------------------- *)

(* Parse the command line, looking for a file name, and process this file. *)

let () =
  let it = ref None in
  let usage = sprintf "Usage: %s <filename>" Sys.argv.(0) in
  Arg.parse [] (fun filename -> it := Some filename) usage;
  match !it with
  | Some filename ->
      process filename
  | None ->
      eprintf "%s\n" usage;
      exit 1
