open Fix.Indexing
open Utils
open Misc
open Info
open Regexp
open Lrgrep_support

type priority = int

type ('g, 'n) stacks = {
  tops : 'n indexset;
  prev : ('n, 'n indexset) vector;
  label : ('n, 'g lr1 indexset) vector;
}

type spec = {
  parser_name : string;
  lexer_definition : Syntax.lexer_definition;
}

module Clause = Unsafe_cardinal()
type ('g, 'r) clause = ('g * 'r) Clause.t

type ('g, 'r) clauses = {
  syntax : (('g, 'r) clause, Syntax.clause) vector;
  captures : (('g, 'r) clause, (Capture.n, Syntax.capture_kind * string) indexmap) vector;
}

module Branch = Unsafe_cardinal()
type ('g, 'r) branch = ('g * 'r) Branch.t

type ('g, 'r) branches = {
  of_clause : (('g, 'r) clause, ('g, 'r) branch indexset) vector;
  lookaheads : (('g, 'r) branch, 'g terminal indexset option) vector;
  clause: (('g, 'r) branch, ('g, 'r) clause index) vector;
  br_captures : (('g, 'r) branch, Capture.n indexset) vector;
}

module type MACHINE = sig
  type g
  type r

  type states
  val states : states cardinal
  val initial : states index option

  type transitions
  val transitions : transitions cardinal

  val register_count : int

  val source : transitions index -> states index
  val target : transitions index -> states index
  val label : transitions index -> g lr1 indexset
  val captures : transitions index -> (Capture.t * Register.t) list
  val clear : transitions index -> Register.set
  val moves : transitions index -> Register.t Register.map
  val priority : transitions index -> ((g, r) branch index * priority * priority) list

  val unhandled : states index -> g lr1 indexset
  val outgoing : states index -> transitions indexset
  val matching : states index -> ((g, r) branch index * priority * Register.t Capture.map) list
  val partial_captures : Capture.set
end

type ('g, 'r) machine = (module MACHINE with type g = 'g and type r = 'r)

let print_literal_code cp (loc, txt) =
  if txt <> "" then
    Code_printer.print cp ~loc txt

let output_table (type g r) out rule ((module M) : (g, r) machine) (program, table, remap) =
  let print fmt = Code_printer.fmt out fmt in
  print "let lrgrep_program_%s : Lrgrep_runtime.program = {\n"
    rule.Syntax.name;
  print "  registers = %d;\n" M.register_count;
  print "  initial = %d;\n" (
    match M.initial with
    | None -> 0
    | Some i -> remap.((i :> int))
  );
  print "  table = %S;\n" table;
  print "  code = %S;\n" program;
  print "}\n"

let output_wrapper out {Syntax.name; args; _} =
  let args = "lrgrep_initial" :: "lrgrep_lookahead" :: args in
  let args = String.concat " " args in
  Code_printer.fmt out
    "let %s _lrgrep_env %s = (\n\
    \  List.find_map \n\
    \    (fun m -> lrgrep_execute_%s m %s)\n\
    \    (lrgrep_run lrgrep_program_%s _lrgrep_env)\n\
     )\n"
    name args
    name args
    name

type printer = Code_printer.t option -> unit

let output_header (type g) ((module Info) : g info) spec : printer = function
  | None -> ()
  | Some out ->
    begin match Info.Grammar.Grammar.parameters with
      | [] -> ()
      | parameters ->
        Code_printer.print out "module Make";
        List.iter (Code_printer.fmt out "(%s)") parameters;
        Code_printer.fmt out
          "(%s : module type of %s.Make" spec.parser_name spec.parser_name;
        let extract_name name =
          match String.index_opt name ':' with
          | None -> name
          | Some index -> String.sub name 0 index
        in
        List.iter

          (fun param -> Code_printer.fmt out "(%s)" (extract_name param))
          parameters;
        Code_printer.print out ") = struct\n";
    end;
    print_literal_code out spec.lexer_definition.header;
    Code_printer.fmt out
      "include Lrgrep_runtime.Interpreter(%s.MenhirInterpreter)\n"
      spec.parser_name

let output_trailer (type g) ((module Info) : g info) spec : printer = function
  | None -> ()
  | Some out ->
    Code_printer.print out "\n";
    print_literal_code out spec.lexer_definition.Syntax.trailer;
    match Info.Grammar.Grammar.parameters with
    | [] -> ()
    | _ -> Code_printer.print out "\nend\n"

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

let output_rule (type g r) (info : g info) {parser_name; _} (rule : Syntax.rule)
    clauses (branches : (g, r) branches) ((module M) : (g, r) machine) : printer =
  function
  | None -> ()
  | Some out ->
    (* Step 1: output bytecode and transition tables *)
    let get_state_for_compaction index =
      let add_match (clause, priority, regs) =
        let cap = branches.br_captures.:(clause) in
        let registers =
          let add_reg cap acc =
            let reg = IndexMap.find_opt cap regs in
            (reg : _ index option :> int option) :: acc
          in
          Array.of_list (List.rev (IndexSet.fold add_reg cap []))
        in
        (clause, priority, registers)
      in
      let add_transition tr acc =
        let actions = {
          Lrgrep_support.
          move = IndexMap.bindings (M.moves tr);
          store = List.map snd (M.captures tr);
          clear = IndexSet.elements (M.clear tr);
          target = M.target tr;
          priority = M.priority tr;
        } in
        (M.label tr, actions) :: acc
      in
      {
        Lrgrep_support.
        accept = List.map add_match (M.matching index);
        halting = M.unhandled index;
        transitions = IndexSet.fold add_transition (M.outgoing index) [];
      }
    in
    let program = Lrgrep_support.compact M.states get_state_for_compaction in
    output_table out rule (module M) program;
    (* Step 2: output semantic actions *)
    let open (val info) in
    let captures_lr1 =
      let map = ref IndexMap.empty in
      let process_transitions tr =
        map := List.fold_left (fun map (cap, _reg) ->
            IndexMap.update cap (function
                | None -> Some (M.label tr)
                | Some set' -> Some (IndexSet.union set' (M.label tr))
              ) map
          ) !map (M.captures tr)
      in
      Index.iter M.transitions process_transitions;
      !map
    in
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
    in
    let symbol_matcher s = match Symbol.prj s with
      | L t -> "T T_" ^ Terminal.to_string t
      | R n -> "N N_" ^ Grammar.Nonterminal.mangled_name (Nonterminal.to_g n)
    in
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
          parser_name
          (if Option.is_none typ then "_" else "st")
          (if Option.is_none typ then "_" else "x")
          (if Option.is_none typ then "as x" else "");
        begin match typ with
          | None -> ()
          | Some (symbols, typ) ->
            Code_printer.fmt out
              "        let x = match %s.MenhirInterpreter.incoming_symbol st with\n"
              parser_name;
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
          parser_name (some "p")
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
          parser_name (some "p")
    in
    let lookahead_constraint branch =
      match branches.lookaheads.:(branch) with
      | None -> None
      | Some terms ->
        let term_pattern t =
          Terminal.to_string t ^
          match Terminal.semantic_value t with
          | None -> ""
          | Some _ -> " _"
        in
        Some (string_concat_map ~wrap:("(",")") "|"
                term_pattern (IndexSet.elements terms))
    in
    let output_execute_function out =
      Code_printer.fmt out
        "let lrgrep_execute_%s %s\n\
        \  (__clause, (__registers : %s.MenhirInterpreter.element Lrgrep_runtime.register_values))\n\
        \  (__initialpos : Lexing.position)\n\
        \  ((token : %s.MenhirInterpreter.token), _startloc_token_, _endloc_token_)\n\
        \  : _ option = match __clause, token with\n"
        rule.name (String.concat " " rule.args)
        parser_name parser_name;
      let output_clause_branches clause brs =
        let captures = clauses.captures.:(clause) in
        Code_printer.fmt out " ";
        IndexSet.iter (fun branch ->
            Code_printer.fmt out
              " | %d, %s"
              (Index.to_int branch)
              (Option.value (lookahead_constraint branch) ~default:"_");
          ) brs;
        Code_printer.fmt out " ->\n";
        IndexMap.iter (bind_capture out ~roffset:(ref 0)) captures;
        begin match clauses.syntax.:(clause).action with
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
            (fun branch -> Option.is_some branches.lookaheads.:(branch))
            brs
        in
        if not (IndexSet.is_empty constrained) then
          Code_printer.fmt out "  | (%s), _ -> None\n"
            (string_concat_map "|" string_of_index (IndexSet.elements constrained))
      in
      Vector.iteri output_clause_branches branches.of_clause;
      Code_printer.print out "  | _ -> failwith \"Invalid action (internal error or API misuse)\"\n\n"
    in
    output_execute_function out;
    (* Step 3: wrapper to glue interpreter and user actions *)
    output_wrapper out rule
