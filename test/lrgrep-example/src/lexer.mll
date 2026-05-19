{
  open Parser
  exception Error of (string * Lexing.position * Lexing.position)
}

rule token = parse
| "(*"
    { comment lexbuf }
| [' ' '\t']
    { token lexbuf }
| ['\n']
    { Lexing.new_line lexbuf; token lexbuf }
| eof
    { EOF }
| "let"
    { LET }
| "in"
    { IN }
| ['0'-'9']+ as i
    { INT (int_of_string i) }
| (['a'-'z'] ['a'-'z''0'-'9']*) as x
    { IDENT x }
| '='
    { EQUAL }
| '+'
    { PLUS }
| '-'
    { MINUS }
| '*'
    { TIMES }
| '/'
    { DIV }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| ';'
    { SEMI }
| _ as c
    { let startp = Lexing.lexeme_start_p lexbuf
      and endp = Lexing.lexeme_end_p lexbuf in
      let message = Printf.sprintf "Lexical error: unexpected character: '%c'." c in
      raise (Error (message, startp, endp)) }

and comment = parse
| "*)"
    { token lexbuf }
| _
    { comment lexbuf }
