(* https://stackoverflow.com/questions/67361797/ocaml-syntax-error-in-let-expression-following-module-definition *)

module M = struct end

let x = 14 in x;;
