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

  let cost_of_attributes prj attrs =
    List.fold_left
      (fun total attr ->
         if Attribute.has_label "cost" attr then
           total +. float_of_string (Attribute.payload attr)
         else total)
      0. (prj attrs)

  let terminal_attributes t = Grammar.Terminal.attributes (Terminal.to_g t)
  let nonterminal_attributes t = Grammar.Nonterminal.attributes (Nonterminal.to_g t)
  let production_attributes t = Grammar.Production.attributes (Production.to_g t)

  let cost_of_symbol =
    let measure ~has_default prj attrs =
      if List.exists (Attribute.has_label "recovery") (prj attrs) || has_default
      then cost_of_attributes prj attrs
      else infinity
    in
    let ft = tabulate_finset Terminal.n (fun t ->
      if Option.is_none (Terminal.semantic_value t)
      then measure ~has_default:true terminal_attributes t
      else measure ~has_default:false terminal_attributes t
    ) in
    let fn =
      tabulate_finset Nonterminal.n (measure ~has_default:false nonterminal_attributes)
    in
    fun s ->
    match Symbol.desc s with
    | Symbol.T t -> ft t
    | Symbol.N n -> fn n

  let cost_of_prod =
    tabulate_finset Production.n (cost_of_attributes production_attributes)

  let penalty_of_item =
    let f = tabulate_finset Production.n @@ fun p ->
      Array.map (cost_of_attributes (fun (_,_,a) -> a))
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
        Some (default_printer ?fallback (terminal_attributes t))
    | `PSEUDO -> None

  let default_nonterminal n =
    match Nonterminal.kind n with
    | `REGULAR -> Some (default_printer (nonterminal_attributes n))
    | `START -> None
end
