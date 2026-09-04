%token OPAREN "("
%token CPAREN ")"
%token <char> C
%token EOF

%start <char list> sentence

%%

let sentence := "(" ; ~ = chars ; ")" ; EOF ; <>

let chars := ~ = list(char) ; <List.rev>
let char := ~ = C ; <>

let rlist(x) :=
  | {[]}
  | xs = rlist(x) ; x = x ; {x :: xs}
