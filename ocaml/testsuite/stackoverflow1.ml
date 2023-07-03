(* https://stackoverflow.com/questions/71847555/ocaml-syntax-error-but-i-dont-know-where-can-somoene-help-me *)

let n = read_int();

    let schroder n =
       let pointer = ref 0 in
         for i = 1 to n-2 do
           pointer := !pointer + (schroder i * schroder n-i-1)
         done;
      schroder n = 3 * schroder n-1 + !pointer
    !schroder n
