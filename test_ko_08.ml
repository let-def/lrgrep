(* parsing bug on function types in variant type definitions
   https://github.com/ocaml/ocaml/issues/11445 *)

type t = Foo of int -> int
