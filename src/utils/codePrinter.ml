(* MIT License
   Copyright (c) 2025 Frédéric Bour

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.*)

(** This module provides a simple framework for outputting text with line
  tracking and location printing, useful for compilers and interpreters that
  need to generate code or output messages with precise location information. *)

type t = {
  filename: string;         (* The name of the file being processed *)
  mutable line: int;        (* Current line number in the output *)
  output: string -> unit;   (* Function to output a string *)
  mutable reloc: bool;      (* Flag indicating if relocation info should be printed next *)
  mutable last_is_nl: bool; (* Flag indicating if the last output character was a newline *)
}

type location = {
  loc_file : string;   (* File name where the location points *)
  start_pos : int;     (* Starting position in the file *)
  end_pos : int;       (* Ending position in the file *)
  start_line : int;    (* Starting line in the file *)
  start_col : int;     (* Starting column in the file *)
}

(* Output a string, updating line count and newline status *)
let output t = function
  | "" -> ()
  | str ->
    let nl = ref 0 in
    let last = String.length str - 1 in
    for i = 0 to last do
      if str.[i] = '\n' then incr nl;
    done;
    t.line <- t.line + !nl;
    t.last_is_nl <- str.[last] = '\n';
    t.output str

(* Create a new code printer *)
let create ~filename ?(line=1) output =
  {filename; line; output; reloc = false; last_is_nl = true}

(* Print relocation information for a given file and line *)
let print_loc_dir t filename line =
  output t (Printf.sprintf "# %d %S\n" (line + 1) filename)

(* Print relocation information for a given location *)
let print_loc t loc =
  print_loc_dir t loc.loc_file (loc.start_line - 1);
  output t (String.make loc.start_col ' ')

(* Print a message, optionally with relocation information *)
let print ?loc t msg =
  match loc with
  | None ->
    if t.reloc then (
      if not t.last_is_nl then output t "\n";
      print_loc_dir t t.filename t.line;
      t.reloc <- false;
    );
    output t msg
  | Some loc ->
    if not t.last_is_nl then output t "\n";
    print_loc t loc;
    t.reloc <- true;
    output t msg

(* Print a formatted message, optionally with relocation information *)
let fmt ?loc t fmt =
  Printf.ksprintf (print ?loc t) fmt
