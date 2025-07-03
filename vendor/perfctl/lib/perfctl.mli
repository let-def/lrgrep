(*
  MIT License

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

(**
  Module `Perfctl`: Control 'perf' profiling from within the program being profiled.

  This module provides functions to dynamically enable and disable profiling
  using the Linux `perf` tool. It allows the program being profiled to control
  when profiling starts and stops, enabling precise profiling of specific code sections.
*)

val is_available : bool
(** Check if the program is running under perfctl.

    This function returns `true` if the program is being profiled by perfctl,
    and `false` otherwise. It can be used to conditionally enable or disable
    profiling based on the current environment.
*)

val enable : unit -> unit
(** Enable profiling if perfctl is available.

    This function enables profiling if the program is running under perfctl.
    If perfctl is not available, this function does nothing.

    The call blocks while waiting for an answer from perf. Execution reumes only after perf confirms the request.
*)

val disable : unit -> unit
(** Disable profiling if perfctl is available.

    This function disables profiling if the program is running under perfctl.
    If perfctl is not available, this function does nothing.

    The call blocks while waiting for an answer from perf. Execution resumes only after perf confirms the request.
*)

val enabled : unit -> bool
(** Returns [true] iff the most recent call was to [enable ()] and not to [disable ()].
    This does not actually query the state of Perf and just returns based on the
    internal state of [Perfctl] module.
 *)
