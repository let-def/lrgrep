open Syntax

let linearize_symbol =
  let buffer = Buffer.create 32 in
  function
  | Name s -> s
  | sym ->
    Buffer.reset buffer;
    let rec aux = function
      | Name s -> Buffer.add_string buffer s
      | Apply (s, args) ->
        Buffer.add_string buffer s;
        Buffer.add_char buffer '(';
        List.iteri (fun i sym ->
            if i > 0 then Buffer.add_char buffer ',';
            aux sym
          ) args;
        Buffer.add_char buffer ')'
    in
    aux sym;
    Buffer.contents buffer

exception Error of string

let fail fmt =
  Printf.ksprintf (fun str -> raise (Error str)) fmt

module Make(Regex : Middle.Intf.REGEX) = struct
  module Sigma = Regex.Sigma
  module Lr1 = Sigma.Lr1
  module Grammar = Lr1.Grammar

  let symbols : (string, Grammar.symbol) Hashtbl.t =
    Hashtbl.create 107

  let () =
    let add_symbol s =
      Hashtbl.add symbols (Grammar.symbol_name ~mangled:false s) s
    in
    Grammar.Terminal.iter (fun t -> add_symbol (Grammar.T t));
    Grammar.Nonterminal.iter (fun n -> add_symbol (Grammar.N n))

  let translate_symbol sym =
    let str = linearize_symbol sym in
    try Hashtbl.find symbols str
    with Not_found ->
      fail "Unknown symbol %s\n" str

  let translate_nonterminal sym =
    match translate_symbol sym with
    | Grammar.N n -> n
    | Grammar.T t ->
      fail "Expecting a non-terminal but %s is a terminal\n"
        (Grammar.Terminal.name t)

  let translate_producers list =
    List.map (Option.map translate_symbol) list

  let rec translate_term : Syntax.regular_term -> Regex.Expr.t =
    function
    | Symbol sym ->
      let sym = translate_symbol sym in
      let states = Lr1.by_incoming_symbol sym in
      Regex.Expr.set (Sigma.Pos states)
    | Item { lhs; prefix; suffix } ->
      let lhs = Option.map translate_nonterminal lhs in
      let prefix = translate_producers prefix in
      let suffix = translate_producers suffix in
      let states = Lr1.states_by_items ?lhs ~prefix ~suffix in
      Regex.Expr.set (Sigma.Pos states)
    | Wildcard -> Regex.Expr.set Sigma.full
    | Alternative (re1, re2) ->
      Regex.Expr.(|.) (translate_expression re1) (translate_expression re2)
    | Repetition (re, _) ->
      Regex.Expr.star (translate_expression re)
    | Reduce (re, _) ->
      Regex.simulate_reductions (translate_expression re)

  and translate_expression (terms : Syntax.regular_expression) : Regex.Expr.t =
    let terms =
      (* The reverse is important:
         in lrgrep, the expression is read from right-to-left (matching from
         the top of the stack, to the root of the analysis) *)
      List.rev_map (fun (term, _) -> translate_term term) terms
    in
    Regex.Expr.concatenation terms

  let translate (def : Syntax.lexer_definition) =
    let action_counter = ref 0 in
    let entries = List.map (fun entry ->
        let clauses = List.map (fun clause ->
            let expr = translate_expression clause.pattern in
            let action = !action_counter in
            incr action_counter;
            Regex.Expr.(expr ^. label (Regex.Action.accept action))
          ) entry.clauses
        in
        Regex.Expr.disjunction clauses
      ) def.entrypoints
    in
    let dfa = Regex.add_to_dfa Regex.Map.empty entries in
    entries, dfa
end
