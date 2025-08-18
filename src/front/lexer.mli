(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type lexer_state

val fresh_state : unit -> lexer_state

val prepare_lexbuf : lexer_state -> Lexing.lexbuf -> Lexing.lexbuf

val main : lexer_state -> Lexing.lexbuf -> Parser.token

exception Error of {msg: string; pos: Lexing.position}
