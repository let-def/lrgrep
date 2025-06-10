(* -------------------------------------------------------------------------- *)

(* These are the tokens that the lexer can produce. *)

(* Integer literals. *)
%token <int> INT
(* Arithmetic operators. *)
%token PLUS MINUS TIMES DIV
(* Parentheses. *)
%token LPAREN RPAREN
(* Punctuation. *)
%token COMMA
(* End of file. *)
%token EOF

(* -------------------------------------------------------------------------- *)

(* Precedence declarations, lowest (first line) to highest (last line). *)

%left PLUS MINUS
%left TIMES DIV
%nonassoc UMINUS

(* -------------------------------------------------------------------------- *)

(* The entry point is [main]. *)

(* Because this demo focuses on handling and reporting syntax errors, this
   parser does not produce abstract syntax trees; instead, when the input is
   syntactically correct, it returns just a unit value. This removes a lot of
   noise and makes it easier to modify the grammar. *)

%start <unit> main

%%

(* -------------------------------------------------------------------------- *)

main:
| expr EOF
    {}

expr:
| INT
| LPAREN expr RPAREN
| expr PLUS expr
| expr MINUS expr
| expr TIMES expr
| expr DIV expr
| MINUS expr %prec UMINUS
| LPAREN expr COMMA expr RPAREN
    {}
