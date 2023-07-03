(* parsing bug on function types in variant type definitions
   https://github.com/ocaml/ocaml/issues/11445 *)

type t = Foo of int -> int

(*
  | [constructor_arguments] @ MINUSGREATER
    { "Issue #11445: in constructor arguments, \
       arrow types should be wrapped between parentheses" }
*)
