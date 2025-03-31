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
   SOFTWARE.
*)

(** Module for printing text with line tracking and location output.
    This is useful for compilers and preprocessors that need to generate code
    that refers to locations in external files.
    The module allows tracking of code positions and can output directives indicating
    where the code was originally located in the source files. *)

(** Opaque type representing the state of the code printer. *)
type t

(** [create ~filename ?line output_function] creates a new code printer.
    [filename] is the name of the output file.
    [line] is the line number at which output starts to be appended (defaults to 1).
    [output_function] is called to append a string to the output file.
*)
val create : filename:string -> ?line:int -> (string -> unit) -> t

(** [print t ?loc text] appends [text] to [t].
    If [loc] is provided, a directive
      # <loc.start_line> <loc.loc_file>
    is emitted to indicate that [text] was extracted from [loc].
    If [loc] is not provided, a directive
      # <output filename> <output current line number>
    is emitted if necessary to indicate that [text] is code specific to the
    printed file.
*)
val print : ?loc:Lexing.position -> t -> string -> unit

(** [fmt] is a variant of [print] that supports [Printf]-like format strings. *)
val fmt : ?loc:Lexing.position -> t -> ('a, unit, string, unit) format4 -> 'a
