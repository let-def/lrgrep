(* https://github.com/ocaml/ocaml/issues/11108
   Report when a keyword has been typed in a context where a lowercase
   identifier is expected. *)

let mk_if cond ~then ~else = ()

(*
  | [_* / . LIDENT]
    partial { match Lexer_raw.as_keyword token with
      | None -> None
      | Some kw -> Some ...
    }
*)
