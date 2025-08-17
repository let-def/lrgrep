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

type lexer_state = {
  mutable brace_depth: int;
  mutable comment_depth: int;
  mutable track_from: int;
  mutable refills: Buffer.t;
}

let fresh_state () = {
  brace_depth = 0;
  comment_depth = 0;
  track_from = -1;
  refills = Buffer.create 0;
}

let prepare_lexbuf st (lexbuf : Lexing.lexbuf) =
  let refill = lexbuf.refill_buff in
  {lexbuf with refill_buff =
                 fun lexbuf ->
                   if st.track_from = -1 then
                     refill lexbuf
                   else (
                     let endp = lexbuf.lex_start_pos in
                     if st.track_from < endp then (
                       Buffer.add_subbytes st.refills lexbuf.lex_buffer
                         st.track_from (endp - st.track_from);
                       st.track_from <- endp;
                     );
                     refill lexbuf;
                     st.track_from <- lexbuf.lex_start_pos;
                   )}

(* Start copying lexed contents verbatim after current lexeme *)
let start_tracking st lexbuf =
  assert (st.track_from = -1);
  st.track_from <- lexbuf.Lexing.lex_curr_pos

(* End copy just before current lexeme and return contents *)
let end_tracking st (lexbuf : Lexing.lexbuf) =
  assert (st.track_from > -1);
  Buffer.add_subbytes st.refills lexbuf.lex_buffer
    st.track_from (lexbuf.lex_start_pos - st.track_from);
  st.track_from <- -1;
  let result = Buffer.contents st.refills in
  Buffer.clear st.refills;
  result

let in_pattern st = st.brace_depth = 0 && st.comment_depth = 0

exception Lexical_error of {msg: string; pos: Lexing.position}

let raise_lexical_error lexbuf fmt =
  let pos = Lexing.lexeme_start_p lexbuf in
  Printf.ksprintf (fun msg -> raise (Lexical_error {msg; pos}))
    fmt

let warning lexbuf fmt =
  Syntax.warn (Lexing.lexeme_start_p lexbuf) fmt

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

(*let char_for_octal_code c d u =
  let c = 64 * (Char.code c - 48) +
           8 * (Char.code d - 48) +
               (Char.code u - 48) in
  Char.chr c*)

(*let char_for_hexadecimal_code d u =
  Char.chr (16 * (hex_digit_value d) + (hex_digit_value u))*)

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

rule main st = parse
| [' ' '\r' '\009' '\012' ] +
  { main st lexbuf }
| '\n'
  { incr_loc lexbuf 0;
    main st lexbuf
  }
| "#" [' ' '\t']* (['0'-'9']+ as num) [' ' '\t']*
  ('\"' ([^ '\n' '\r' '\"']* as name) '\"')?
  [^ '\n' '\r']* '\n'
  { update_loc lexbuf name (int_of_string num);
    main st lexbuf
  }
| "(*" (* Ignore comments *)
  { st.comment_depth <- 1;
    comment st lexbuf;
    main st lexbuf
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
    st.brace_depth <- 1;
    start_tracking st lexbuf;
    action st lexbuf;
    let code = end_tracking st lexbuf in
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
      "illegal character %s"
      (String.escaped (Lexing.lexeme lexbuf))
  }

(* Lexers comment and action are quite similar.
   They should lex strings, quoted strings and characters,
   in order not to be confused by what is inside them. *)
and comment st = parse
    "(*"
    { st.comment_depth <- st.comment_depth + 1;
      comment st lexbuf
    }
  | "*)"
    { st.comment_depth <- st.comment_depth - 1;
      if st.comment_depth > 0 then
        comment st lexbuf
    }
  | '"'
    { string st lexbuf;
      comment st lexbuf
    }
  | '{' ('%' '%'? extattrident blank*)? (lowercase* as delim) "|"
    { quoted_string delim lexbuf;
      comment st lexbuf
    }
  | "'"
    { skip_char lexbuf;
      comment st lexbuf
    }
  | eof
    { raise_lexical_error lexbuf "unterminated comment" }
  | '\n'
    { incr_loc lexbuf 0;
      comment st lexbuf }
  | ident
    { comment st lexbuf }
  | _
    { comment st lexbuf }

(* Lex semantic actions: skip everything before finding '}', accounting
   for nested '{' '}' *)
and action st = parse
    '{'
    { st.brace_depth <- st.brace_depth + 1;
      action st lexbuf }
  | '}'
    { st.brace_depth <- st.brace_depth - 1;
      if st.brace_depth <> 0 then
        action st lexbuf
    }
  | '"'
    { string st lexbuf;
      action st lexbuf }
  | '{' ('%' '%'? extattrident blank*)? (lowercase* as delim) "|"
    { quoted_string delim lexbuf;
      action st lexbuf }
  | "'"
    { skip_char lexbuf;
      action st lexbuf }
  | "(*"
    { st.comment_depth <- 1;
      comment st lexbuf;
      action st lexbuf }
  | eof
    { raise_lexical_error lexbuf "unterminated action" }
  | '\n'
    { incr_loc lexbuf 0;
      action st lexbuf }
  | ident
    { action st lexbuf }
  | _
    { action st lexbuf }

(* String parsing comes from the compiler lexer *)
and string st = parse
    '"'
    { () }
  | '\\' '\r'* '\n' ([' ' '\009'] * as spaces)
    { incr_loc lexbuf (String.length spaces);
      string st lexbuf }
  | '\\' backslash_escapes
    { string st lexbuf }
  | '\\' (['0'-'9'] as c) (['0'-'9'] as d) (['0'-'9']  as u)
    { let v = decimal_code c d u in
      if in_pattern st then (
        if v > 255 then
          raise_lexical_error lexbuf
            "illegal backslash escape in string: '\\%c%c%c'" c d u
      );
      string st lexbuf }
  | '\\' 'o' ['0'-'3'] ['0'-'7'] ['0'-'7']
    { string st lexbuf }
  | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']
    { string st lexbuf }
  | '\\' 'u' '{' (['0'-'9' 'a'-'f' 'A'-'F']+ as s) '}'
    { let v = hexadecimal_code s in
      if in_pattern st then (
        if not (Uchar.is_valid v) then
          raise_lexical_error lexbuf
            "illegal uchar escape in string: '\\u{%s}'" s
      );
      string st lexbuf }
  | '\\' (_ as c)
    { if in_pattern st then
       warning lexbuf
        "illegal backslash escape in string: '\\%c'" c;
      string st lexbuf }
  | eof
    { raise_lexical_error lexbuf "unterminated string" }
  | '\r'* '\n'
    { if st.comment_depth = 0 then
        warning lexbuf "unescaped newline in string";
      incr_loc lexbuf 0;
      string st lexbuf }
  | _
    { string st lexbuf }

and quoted_string delim = parse
  | '\r'* '\n'
    { incr_loc lexbuf 0;
      quoted_string delim lexbuf }
  | eof
    { raise_lexical_error lexbuf "unterminated string" }
  | '|' (lowercase* as delim') '}'
    { if delim <> delim' then
        quoted_string delim lexbuf }
  | _
    { quoted_string delim lexbuf }

and skip_char = parse
  | '\\'? '\r'* '\n' "'"
     { incr_loc lexbuf 1 }
  | [^ '\\' '\'' '\n' '\r'] "'" (* regular character *)
    (* one character and numeric escape sequences *)
  | '\\' _ "'"
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
  | '\\' 'o' ['0'-'7'] ['0'-'7'] ['0'-'7'] "'"
  | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
     {()}

(* Perilous *)
  | "" {()}
