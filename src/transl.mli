open Regexp

val transl_kre : (string -> RE.var) -> Syntax.regular_expr -> int -> KRE.t
