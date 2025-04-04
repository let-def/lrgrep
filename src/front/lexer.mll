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

(* The lexical analyzer for lexer definitions. Bootstrapped! *)

{
open Parser

(* Auxiliaries for the lexical analyzer *)

let brace_depth = ref 0
and comment_depth = ref 0

let ic = ref None

let in_pattern () = !brace_depth = 0 && !comment_depth = 0

exception Lexical_error of {msg: string; pos: Lexing.position}

let string_buff = Buffer.create 256

let read_location ic start stop =
  let pos = pos_in ic in
  seek_in ic start;
  let txt = really_input_string ic (stop - start) in
  seek_in ic pos;
  txt

let reset_string_buffer () = Buffer.clear string_buff

let store_string_char c = Buffer.add_char string_buff c
let store_string_uchar u = Buffer.add_utf_8_uchar string_buff u
let store_string_chars s = Buffer.add_string string_buff s

let char_for_backslash = function
    'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let raise_lexical_error lexbuf msg =
  let pos = Lexing.lexeme_start_p lexbuf in
  raise (Lexical_error {msg; pos})

let handle_lexical_error fn lexbuf =
  try fn lexbuf
  with Lexical_error {msg; pos = {pos_cnum = -1; _}} ->
    raise(Lexical_error{msg; pos = Lexing.lexeme_start_p lexbuf})

let warning lexbuf msg =
  let p = Lexing.lexeme_start_p lexbuf in
  Printf.eprintf "ocamllex warning:\nFile \"%s\", line %d, character %d: %s.\n"
    p.Lexing.pos_fname p.Lexing.pos_lnum
    (p.Lexing.pos_cnum - p.Lexing.pos_bol + 1) msg;
  flush stderr

let hex_digit_value d =
  let d = Char.code d in
  if d >= 97 then d - 87 else
  if d >= 65 then d - 55 else
  d - 48

let decimal_code c d u =
  100 * (Char.code c - 48) + 10 * (Char.code d - 48) + (Char.code u - 48)

let hexadecimal_code s =
  let rec loop acc i =
    if i < String.length s then
      let value = hex_digit_value s.[i] in
      loop (16 * acc + value) (i + 1)
    else acc in
  loop 0 0

let char_for_octal_code c d u =
  let c = 64 * (Char.code c - 48) +
           8 * (Char.code d - 48) +
               (Char.code u - 48) in
  Char.chr c

let char_for_hexadecimal_code d u =
  Char.chr (16 * (hex_digit_value d) + (hex_digit_value u))

let incr_loc lexbuf delta =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { pos with
    Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
    Lexing.pos_bol = pos.Lexing.pos_cnum - delta;
  }

let update_loc lexbuf opt_file line =
  let pos = lexbuf.Lexing.lex_curr_p in
  let new_file = match opt_file with
                 | None -> pos.Lexing.pos_fname
                 | Some f -> f
  in
  lexbuf.Lexing.lex_curr_p <- { pos with
    Lexing.pos_fname = new_file;
    Lexing.pos_lnum = line;
    Lexing.pos_bol = pos.Lexing.pos_cnum;
  }

}

let identstart =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255']
let identbody =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let backslash_escapes =
  ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']

let lowercase = ['a'-'z' '_']
let ident = identstart identbody* '\''?
let extattrident = ident ('.' ident)*
let blank = [' ' '\009' '\012']

rule main = parse
| [' ' '\013' '\009' '\012' ] +
  { main lexbuf }
| '\010'
  { incr_loc lexbuf 0;
    main lexbuf
  }
| "#" [' ' '\t']* (['0'-'9']+ as num) [' ' '\t']*
  ('\"' ([^ '\010' '\013' '\"']* as name) '\"')?
  [^ '\010' '\013']* '\010'
  { update_loc lexbuf name (int_of_string num);
    main lexbuf
  }
| "(*" (* Ignore comments *)
  { comment_depth := 1;
    handle_lexical_error comment lexbuf;
    main lexbuf
  }
| '_' (* For wildcards *)
  { UNDERSCORE }
| ident (* All names *)
  { match Lexing.lexeme lexbuf with
    | "rule" -> RULE
    | "parse" -> PARSE
    | "error" -> ERROR
    | s -> IDENT s
  }
| '\\' (ident as s) (* Workaround reserved names *)
  { IDENT s }
| "%partial"
  { PARTIAL }
| '{' [' ' '\009']* '.' [' ' '\009']* '}' (* unreachable clause *)
  { UNREACHABLE }
| '{'
  { let p = Lexing.lexeme_end_p lexbuf in
    brace_depth := 1;
    let end_pos = handle_lexical_error action lexbuf in
    let code =
      match !ic with
      | None -> assert false
      | Some ic -> read_location ic p.pos_cnum end_pos
    in
    ACTION (p, code)
  }
| '=' { EQUAL }
| '|' { BAR }
| '[' { LBRACKET }
| ']' { RBRACKET }
| '*' { STAR }
| '?' { QUESTION }
| '/' { SLASH }
| '@' { AT }
| '(' { LPAREN }
| ')' { RPAREN }
| ':' { COLON }
| ',' { COMMA }
| ';' { SEMI }
| '.' { DOT }
| eof { EOF }
| _
  { raise_lexical_error lexbuf
      ("illegal character " ^ String.escaped(Lexing.lexeme lexbuf))
  }

(* Lexers comment and action are quite similar.
   They should lex strings, quoted strings and characters,
   in order not to be confused by what is inside them. *)
and comment = parse
    "(*"
    { incr comment_depth; comment lexbuf }
  | "*)"
    { decr comment_depth;
      if !comment_depth = 0 then () else comment lexbuf }
  | '"'
    { reset_string_buffer();
      string lexbuf;
      reset_string_buffer();
      comment lexbuf }
  | '{' ('%' '%'? extattrident blank*)? (lowercase* as delim) "|"
    { quoted_string delim lexbuf;
      comment lexbuf }
  | "'"
    { skip_char lexbuf ;
      comment lexbuf }
  | eof
    { raise(Lexical_error{msg="unterminated comment"; pos = lexbuf.lex_curr_p}) }
  | '\010'
    { incr_loc lexbuf 0;
      comment lexbuf }
  | ident
    { comment lexbuf }
  | _
    { comment lexbuf }

(* Lex semantic actions: skip everything before finding '}', accounting
   for nested '{' '}' *)
and action = parse
    '{'
    { incr brace_depth;
      action lexbuf }
  | '}'
    { decr brace_depth;
      if !brace_depth = 0 then Lexing.lexeme_start lexbuf else action lexbuf }
  | '"'
    { reset_string_buffer();
      handle_lexical_error string lexbuf;
      reset_string_buffer();
      action lexbuf }
  | '{' ('%' '%'? extattrident blank*)? (lowercase* as delim) "|"
    { quoted_string delim lexbuf;
      action lexbuf }
  | "'"
    { skip_char lexbuf ;
      action lexbuf }
  | "(*"
    { comment_depth := 1;
      comment lexbuf;
      action lexbuf }
  | eof
    { raise (Lexical_error{msg="unterminated action"; pos = lexbuf.lex_curr_p}) }
  | '\010'
    { incr_loc lexbuf 0;
      action lexbuf }
  | ident
    { action lexbuf }
  | _
    { action lexbuf }

(* String parsing comes from the compiler lexer *)
and string = parse
    '"'
    { () }
  | '\\' ('\013'* '\010') ([' ' '\009'] * as spaces)
    { incr_loc lexbuf (String.length spaces);
      string lexbuf }
  | '\\' (backslash_escapes as c)
    { store_string_char(char_for_backslash c);
      string lexbuf }
  | '\\' (['0'-'9'] as c) (['0'-'9'] as d) (['0'-'9']  as u)
    { let v = decimal_code c d u in
      if in_pattern () then
        if v > 255 then
          raise_lexical_error lexbuf
            (Printf.sprintf
              "illegal backslash escape in string: '\\%c%c%c'" c d u)
        else
          store_string_char (Char.chr v);
      string lexbuf }
  | '\\' 'o' (['0'-'3'] as c) (['0'-'7'] as d) (['0'-'7'] as u)
    { store_string_char (char_for_octal_code c d u);
      string lexbuf }
  | '\\' 'x' (['0'-'9' 'a'-'f' 'A'-'F'] as d) (['0'-'9' 'a'-'f' 'A'-'F'] as u)
    { store_string_char (char_for_hexadecimal_code d u) ;
      string lexbuf }
  | '\\' 'u' '{' (['0'-'9' 'a'-'f' 'A'-'F'] + as s) '}'
    { let v = hexadecimal_code s in
      if in_pattern () then
        if not (Uchar.is_valid v) then
          raise_lexical_error lexbuf
            (Printf.sprintf
              "illegal uchar escape in string: '\\u{%s}'" s)
        else
          store_string_uchar (Uchar.unsafe_of_int v);
      string lexbuf }
  | '\\' (_ as c)
    {if in_pattern () then
       warning lexbuf
        (Printf.sprintf "illegal backslash escape in string: '\\%c'" c) ;
      store_string_char '\\' ;
      store_string_char c ;
      string lexbuf }
  | eof
    { raise(Lexical_error{msg="unterminated string"; pos = lexbuf.lex_curr_p}) }
  | '\013'* '\010' as s
    { if !comment_depth = 0 then
        warning lexbuf (Printf.sprintf "unescaped newline in string") ;
      store_string_chars s;
      incr_loc lexbuf 0;
      string lexbuf }
  | _ as c
    { store_string_char c;
      string lexbuf }

and quoted_string delim = parse
  | '\013'* '\010'
    { incr_loc lexbuf 0;
      quoted_string delim lexbuf }
  | eof
    { raise (Lexical_error{msg="unterminated string"; pos = lexbuf.lex_curr_p}) }
  | '|' (lowercase* as delim') '}'
    { if delim <> delim' then
      quoted_string delim lexbuf }
  | _
    { quoted_string delim lexbuf }

and skip_char = parse
  | '\\'? ('\013'* '\010') "'"
     { incr_loc lexbuf 1;
     }
  | [^ '\\' '\'' '\010' '\013'] "'" (* regular character *)
(* one character and numeric escape sequences *)
  | '\\' _ "'"
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
  | '\\' 'o' ['0'-'7'] ['0'-'7'] ['0'-'7'] "'"
  | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
     {()}
(* Perilous *)
  | "" {()}
