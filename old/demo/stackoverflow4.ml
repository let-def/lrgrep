(* https://stackoverflow.com/questions/69595112/the-problem-with-writing-a-function-in-ocaml *)

let rec powList (x,n) =
  if n = 0 then []
  else  (let a = let rec power (x, n) =
        if n = 0 then 1
        else  x * power (x, n-1) in a) ::: powList(x, n-1);;
