open Utils
open Misc

module type S = sig
  module Info : Mid.Info.S
  open Info

  val cost_of_prod    : Production.t -> float
  val penalty_of_item : Production.t * int -> float
  val cost_of_symbol  : Symbol.t -> float

  val default_prelude     : Format.formatter -> unit
  val default_terminal    : Terminal.t -> string option
  val default_nonterminal : Nonterminal.t -> string option
end

module Make (Info : Mid.Info.S) : S with module Info := Info = struct
  open Info

  module Attribute = Grammar.Attribute

  let cost_of_attributes attrs =
    List.fold_left
      (fun total attr ->
         if Attribute.has_label "cost" attr then
           total +. float_of_string (Attribute.payload attr)
         else total)
      (-0.0) attrs

  let symbol_attributes sym =
    match Symbol.desc sym with
    | T t -> Grammar.Terminal.attributes (Terminal.to_g t)
    | N n -> Grammar.Nonterminal.attributes (Nonterminal.to_g n)

  let production_attributes t =
    Grammar.Production.attributes (Production.to_g t)

  let cost_of_symbol =
    tabulate_finset Symbol.n (fun sym ->
        let default = match Symbol.desc sym with
          | T t -> Option.is_none (Terminal.semantic_value t)
          | N _ -> false
        in
        let attrs = symbol_attributes sym in
        let cost = cost_of_attributes attrs in
        if default || List.exists (Attribute.has_label "recovery") attrs then
          if Float.sign_bit cost then
            1.0
          else
            cost
        else (
          if not (Float.sign_bit cost) && cost < infinity then (
            Printf.eprintf "Warning: ignoring cost %.02f for symbol %s because no recovery value was provided\n"
              cost (Symbol.name sym);
            Printf.eprintf "         add attribute [%%recovery <ocaml code>] to enable recovery for this symbol\n";
          );
          infinity
        )
    )

  let cost_of_prod =
    tabulate_finset Production.n (fun p -> cost_of_attributes (production_attributes p))

  let penalty_of_item =
    let f = tabulate_finset Production.n @@ fun p ->
      Array.map
        (fun (_,_,a) -> cost_of_attributes a)
        (Grammar.Production.rhs (Production.to_g p))
    in
    fun (p,i) ->
      let costs = f p in
      if i < Array.length costs then costs.(i) else cost_of_prod p

  let default_prelude ppf =
    List.iter (fun a ->
        if Attribute.has_label "header" a ||
           Attribute.has_label "recovery.header" a then
          Format.fprintf ppf "%s\n" (Attribute.payload a)
      ) Grammar.Grammar.attributes

  let default_printer ?(fallback="raise Not_found") attrs =
    match List.find (Attribute.has_label "recovery") attrs with
    | exception Not_found -> fallback
    | attr -> Attribute.payload attr

  let default_terminal t =
    match Grammar.Terminal.kind (Terminal.to_g t) with
    | `REGULAR | `ERROR | `EOF ->
        let fallback = match Terminal.semantic_value t with
          | None -> Some "()"
          | Some _ -> None
        in
        Some (default_printer ?fallback (symbol_attributes (Symbol.inj_l t)))
    | `PSEUDO -> None

  let default_nonterminal n =
    match Nonterminal.kind n with
    | `REGULAR -> Some (default_printer (symbol_attributes (Symbol.inj_r n)))
    | `START -> None
end
