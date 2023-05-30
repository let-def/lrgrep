open Asttypes
open Docstrings
open Parsetree

type let_binding = {
  lb_pattern: pattern;
  lb_expression: expression;
  lb_is_pun: bool;
  lb_attributes: attributes;
  lb_docs: docs Lazy.t;
  lb_text: text Lazy.t;
  lb_loc: Location.t;
}

type let_bindings = {
  lbs_bindings: let_binding list;
  lbs_rec: rec_flag;
  lbs_extension: string Asttypes.loc option;
}

let raise_error exn : unit = raise exn
