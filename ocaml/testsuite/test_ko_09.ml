(* Misleading syntax error message for incorrect module definition #12280  
   https://github.com/ocaml/ocaml/issues/12280 *)

module Foo : sig
  type a = (string, string)   (* error is here: should be "string * string" *)
end
