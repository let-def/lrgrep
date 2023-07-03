(* https://discuss.ocaml.org/t/syntax-error-compiling-code-with-functor-module/6352 *)
let () = module P = Csv(ShowInt)

(*
  | [_* / . seq_expr] @ MODULE
*)
