{
  open Parser
  exception Error of (string * Lexing.position * Lexing.position)
}

rule token = parse
| [' ' '\t']
    { token lexbuf }
| ['\n']
    { Lexing.new_line lexbuf; token lexbuf }
| eof
    { EOF }
| ['0'-'9']+ as i
    { INT (int_of_string i) }
| ','
    { COMMA }
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
| _ as c
    { let startp = Lexing.lexeme_start_p lexbuf
      and endp = Lexing.lexeme_end_p lexbuf in
      let message = Printf.sprintf "Lexical error: unexpected character: '%c'." c in
      raise (Error (message, startp, endp)) }
