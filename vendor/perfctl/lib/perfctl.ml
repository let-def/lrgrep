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

(* We need to workaround Unix.file_descr being abstract ¯\_(ツ)_/¯ *)

let file_descr_of_int : int -> Unix.file_descr = Obj.magic

(* Setup environment *)

let import_fd fn text =
  Option.map (fun fd -> fn (file_descr_of_int fd)) (int_of_string_opt text)

let fd_ctl_write =
  Option.bind
    (Sys.getenv_opt "PERFCTL_CTL_FD")
    (import_fd Unix.out_channel_of_descr)

let fd_ack_read =
  Option.bind
    (Sys.getenv_opt "PERFCTL_ACK_FD")
    (import_fd Unix.in_channel_of_descr)

let is_available = Option.is_some fd_ctl_write && Option.is_some fd_ack_read

let send_and_ack_command cmd =
  match fd_ctl_write, fd_ack_read with
  | Some w, Some r ->
    output_string w cmd;
    flush w;
    begin match input_line r with
    | "\000ack" | "ack" -> ()
    | str -> Printf.eprintf "perfctl: unexpected answer from perf (%S)\n" str
    end
  | _ -> ()

let status = ref false

let enable () =
  status := true;
  send_and_ack_command "enable\n"

let disable () =
  status := false;
  send_and_ack_command "disable\n"

let enabled () = !status
