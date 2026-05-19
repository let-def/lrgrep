(* -------------------------------------------------------------------------- *)

(* These are the tokens that the lexer can produce. *)

(* Integer literals. *)
%token <int> INT
(* Identifiers. *)
%token <string> IDENT
(* Arithmetic operators. *)
%token PLUS MINUS TIMES DIV EQUAL
(* Parentheses. *)
%token LPAREN RPAREN
(* Punctuation. *)
%token SEMI
(* Keywords. *)
%token LET IN
(* End of file. *)
%token EOF

(* -------------------------------------------------------------------------- *)

(* Precedence declarations, lowest (first line) to highest (last line). *)

%nonassoc IN
%right SEMI           (* let x = 0 in f(); y *)
%left PLUS MINUS      (* f(); y+1            *)
%left TIMES DIV       (* 1 + 2*3             *)

(* -------------------------------------------------------------------------- *)

(* The entry point is [file]. *)

(* Because this demo focuses on handling and reporting syntax errors, this
   parser does not produce abstract syntax trees; instead, when the input is
   syntactically correct, it returns just a unit value. This removes a lot of
   noise and makes it easier to modify the grammar. *)

%start <unit> file

%%

(* -------------------------------------------------------------------------- *)

file:
  declaration* EOF
    {}

declaration:
  LET binding
    {}

binding:
  IDENT EQUAL expr
    {}

expr:
| IDENT
| INT
| expr PLUS expr
| expr MINUS expr
| expr TIMES expr
| expr DIV expr
| MINUS expr
| expr SEMI expr
| LPAREN expr RPAREN
| LET binding IN expr
    {}
