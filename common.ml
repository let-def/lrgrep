(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Luc Maranget, projet Moscova,                              *)
(*                          INRIA Rocquencourt                            *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Printf
open Syntax

(* To copy the ML code fragments *)

type line_tracker = {
  file : string;
  oc : out_channel;
  ic : in_channel;
  mutable cur_line : int;
}

let open_tracker file oc = {
  file = file;
  oc = oc;
  ic = open_in_bin file;
  cur_line = 1;
}

let close_tracker tr = close_in_noerr tr.ic

let update_tracker tr =
  fprintf tr.oc "\n";
  flush tr.oc;
  let cr_seen = ref false in
  try while true do
    match input_char tr.ic with
    | '\010' when not !cr_seen -> tr.cur_line <- tr.cur_line + 1;
    | '\013' -> cr_seen := true; tr.cur_line <- tr.cur_line + 1;
    | _ -> cr_seen := false;
  done with End_of_file ->
  fprintf tr.oc "# %d \"%s\"\n" (tr.cur_line+1) tr.file

let copy_buffer = Bytes.create 1024

let copy_chars_unix ic oc start stop =
  let n = ref (stop - start) in
  while !n > 0 do
    let m = input ic copy_buffer 0 (min !n 1024) in
    output oc copy_buffer 0 m;
    n := !n - m
  done

let copy_chars_win32 ic oc start stop =
  for _i = start to stop - 1 do
    let c = input_char ic in
    if c <> '\r' then output_char oc c
  done

let copy_chars =
  match Sys.os_type with
    "Win32" | "Cygwin" -> copy_chars_win32
  | _       -> copy_chars_unix

let copy_chunk ic oc trl loc add_parens =
  if loc.start_pos < loc.end_pos || add_parens then begin
    fprintf oc "# %d \"%s\"\n" loc.start_line loc.loc_file;
    if add_parens then begin
      for _i = 1 to loc.start_col - 1 do output_char oc ' ' done;
      output_char oc '(';
    end else begin
      for _i = 1 to loc.start_col do output_char oc ' ' done;
    end;
    seek_in ic loc.start_pos;
    copy_chars ic oc loc.start_pos loc.end_pos;
    if add_parens then output_char oc ')';
    update_tracker trl;
  end

(* quiet flag *)
let quiet_mode = ref false
