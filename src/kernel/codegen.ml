open Fix.Indexing
open Utils
open Misc
open Regexp
open Lrgrep_support

module type MACHINE = sig
  module Info : Info.S
  open Info

  type states
  val states : states cardinal
  val initial : states index option

  type transitions
  val transitions : transitions cardinal

  val source : transitions index -> states index
  val target : transitions index -> states index
  val label : transitions index -> Lr1.set
  val captures : transitions index -> (Capture.t * Register.t) list

  val partial_captures : Capture.set
end

module type CLAUSE = sig
  include CARDINAL
  val syntax : n index -> Syntax.clause
  val captures : n index -> (Capture.n, Syntax.capture_kind * string) indexmap
end

module type BRANCH = sig
  module Info : Info.S
  open Info

  include CARDINAL
  module Clause : CLAUSE
  val of_clauses : Clause.n index -> n indexset
  val lookaheads : n index -> Terminal.set option
end

module type RULE = sig
  val parser_name : string
  val rule : Syntax.rule
end

module Make
    (Info : Info.S)
    (R : RULE)
    (B : BRANCH with module Info := Info)
    (M : MACHINE with module Info := Info) =
struct
  open Info

  let captures_lr1 =
    let map = ref IndexMap.empty in
    Index.iter M.transitions (fun tr ->
        map := List.fold_left (fun map (cap, _reg) ->
            IndexMap.update cap (function
                | None -> Some (M.label tr)
                | Some set' -> Some (IndexSet.union set' (M.label tr))
              ) map
          ) !map (M.captures tr)
      );
    !map

  let recover_type index =
    try
      let lr1s = IndexMap.find index captures_lr1 in
      let symbols = IndexSet.map (fun lr1 ->
          match Lr1.incoming lr1 with
          | None -> raise Not_found
          | Some sym -> sym
        ) lr1s
      in
      let typ = IndexSet.fold (fun sym acc ->
          let typ = match Symbol.semantic_value sym with
            | None -> raise Not_found
            | Some typ -> String.trim typ
          in
          match acc with
          | None -> Some typ
          | Some typ' ->
            if typ <> typ' then raise Not_found;
            acc
        ) symbols None
      in
      match typ with
      | None -> None
      | Some typ -> Some (symbols, typ)
    with Not_found -> None

  let symbol_matcher s = match Info.Symbol.prj s with
    | L t -> "T T_" ^ Info.Terminal.to_string t
    | R n -> "N N_" ^ Grammar.Nonterminal.mangled_name (Info.Nonterminal.to_g n)

  let bytes_match b i str =
    Bytes.length b >= i + String.length str &&
    let exception Exit in
    match
      for j = 0 to String.length str - 1 do
        if Bytes.get b (i + j) <> String.get str j then
          raise Exit
      done
    with
    | () -> true
    | exception Exit -> false

  let rewrite_loc_keywords str =
    let b = Bytes.of_string str in
    let l = Bytes.length b in
    let i = ref 0 in
    while !i < l do
      if Bytes.get b !i = '$' &&
         (bytes_match b (!i + 1) "startloc(" ||
          bytes_match b (!i + 1) "endloc(")
      then (
        Bytes.set b !i '_';
        while Bytes.get b !i <> '(' do incr i; done;
        Bytes.set b !i '_';
        while !i < l  && Bytes.get b !i <> ')' do incr i; done;
        if !i < l then Bytes.set b !i '_'
      )
      else incr i
    done;
    Bytes.to_string b

  let bind_capture out ~roffset index (def, name) =
    let is_optional = IndexSet.mem index M.partial_captures in
    let none = if is_optional then "None" else "assert false" in
    let some x = if is_optional then "Some (" ^ x ^ ")" else x in
    let offset = !roffset in
    incr roffset;
    match def with
    | Syntax.Value ->
      let typ = recover_type index in
      Code_printer.fmt out
        "    let %s, _startloc_%s_, _endloc_%s_ = match __registers.(%d) with \n\
        \      | Empty -> %s\n\
        \      | Initial -> assert false\n\
        \      | Value (%s.MenhirInterpreter.Element (%s, %s, startp, endp)%s) ->\n"
        name name name offset
        (if is_optional then "(None, None, None)" else "assert false")
        R.parser_name
        (if Option.is_none typ then "_" else "st")
        (if Option.is_none typ then "_" else "x")
        (if Option.is_none typ then "as x" else "");
      begin match typ with
        | None -> ()
        | Some (symbols, typ) ->
          Code_printer.fmt out
            "        let x = match %s.MenhirInterpreter.incoming_symbol st with\n"
            R.parser_name;
          List.iter (fun symbol ->
              Code_printer.fmt out "          | %s -> (x : %s)\n"
                (symbol_matcher symbol) typ) (IndexSet.elements symbols);
          Code_printer.fmt out
            "          | _ -> assert false\n\
            \        in\n"
      end;
      Code_printer.fmt out "        (%s, %s, %s)\n" (some "x") (some "startp") (some "endp");
      Code_printer.fmt out "    in\n";
      Code_printer.fmt out "    let _ = %s in\n" name
    | Start_loc ->
      Code_printer.fmt out
        "    let _startloc_%s_ = match __registers.(%d) with\n\
        \      | Empty -> %s\n\
        \      | Initial -> %s\n\
        \      | Value (%s.MenhirInterpreter.Element (_, _, p, _)) -> %s\n\
        \    in\n"
        name offset
        none
        (some "__initialpos")
        R.parser_name (some "p")
    | End_loc ->
      Code_printer.fmt out
        "    let _endloc_%s_ = match __registers.(%d) with\n\
        \      | Empty -> %s\n\
        \      | Initial -> %s\n\
        \      | Value (%s.MenhirInterpreter.Element (_, _, _, p)) -> %s\n\
        \    in\n"
        name offset
        none
        (some "__initialpos")
        R.parser_name (some "p")

  let lookahead_constraint branch =
    match B.lookaheads branch with
    | None -> None
    | Some terms ->
      let term_pattern t =
        let name = Info.Terminal.to_string t in
        match Info.Terminal.semantic_value t with
        | None -> name
        | Some _ -> name ^ " _"
      in
      Some (string_concat_map ~wrap:("(",")") "|"
              term_pattern (IndexSet.elements terms))

  let output_code out =
    Code_printer.fmt out
      "let lrgrep_execute_%s %s\n\
      \  (__clause, (__registers : %s.MenhirInterpreter.element Lrgrep_runtime.register_values))\n\
      \  (__initialpos : Lexing.position)\n\
      \  ((token : %s.MenhirInterpreter.token), _startloc_token_, _endloc_token_)\n\
      \  : _ option = match __clause, token with\n"
      R.rule.name (String.concat " " R.rule.args)
      R.parser_name R.parser_name;
    let output_clause clause =
      let branches = B.of_clauses clause in
      let captures = B.Clause.captures clause in
      Code_printer.fmt out " ";
      IndexSet.iter (fun branch ->
          Code_printer.fmt out
            " | %d, %s"
            (Index.to_int branch)
            (Option.value (lookahead_constraint branch) ~default:"_");
        ) branches;
      Code_printer.fmt out " ->\n";
      IndexMap.iter (bind_capture out ~roffset:(ref 0)) captures;
      begin match (B.Clause.syntax clause).action with
        | Unreachable ->
          Code_printer.print out "    failwith \"Should be unreachable\"\n"
        | Partial (loc, str) ->
          Code_printer.print out "    (\n";
          Code_printer.fmt out ~loc "%s\n" (rewrite_loc_keywords str);
          Code_printer.print out "    )\n"
        | Total (loc, str) ->
          Code_printer.print out "    Some (\n";
          Code_printer.fmt out ~loc "%s\n" (rewrite_loc_keywords str);
          Code_printer.print out "    )\n"
      end;
      let constrained =
        IndexSet.filter
          (fun branch -> Option.is_some (B.lookaheads branch))
          branches
      in
      if not (IndexSet.is_empty constrained) then
        Code_printer.fmt out "  | (%s), _ -> None\n"
          (string_concat_map "|" string_of_index (IndexSet.elements constrained))
    in
    Index.iter B.Clause.n output_clause;
    Code_printer.print out "  | _ -> failwith \"Invalid action (internal error or API misuse)\"\n\n"
end
