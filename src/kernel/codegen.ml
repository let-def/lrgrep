open Fix.Indexing
open Utils
open Misc
open Info
open Regexp
open Lrgrep_support

type priority = int

type spec = {
  parser_name : string;
  lexer_definition : Syntax.lexer_definition;
}

module Clause = Unsafe_cardinal()
type 's clause = 's Clause.t

type 's clauses = {
  syntax : ('s clause, Syntax.clause) vector;
  captures : ('s clause, (Capture.n, Syntax.capture_kind * string) indexmap) vector;
}

module type BRANCH = sig
  type g

  include CARDINAL
  module Clause : CLAUSE

  val of_clauses : Clause.n index -> n indexset
  val lookaheads : n index -> g terminal indexset option
  val captures : n index -> Capture.n indexset
end

module type MACHINE = sig
  module Info : Info.S
  open Info

  type branch

  type states
  val states : states cardinal
  val initial : states index option

  type transitions
  val transitions : transitions cardinal

  val register_count : int

  val source : transitions index -> states index
  val target : transitions index -> states index
  val label : transitions index -> Lr1.set
  val captures : transitions index -> (Capture.t * Register.t) list
  val clear : transitions index -> Register.set
  val moves : transitions index -> Register.t Register.map
  val priority : transitions index -> (branch index * priority * priority) list

  val unhandled : states index -> Lr1.set
  val outgoing : states index -> transitions indexset
  val matching : states index -> (branch index * priority * Register.t Capture.map) list
  val partial_captures : Capture.set
end


module type RULE = sig
  val rule : Syntax.rule
end

let print_literal_code cp (loc, txt) =
  if txt <> "" then
    Code_printer.print cp ~loc txt

let output_table out rule (registers, initial, (program, table, remap)) =
  let print fmt = Code_printer.fmt out fmt in
  print
    "let lrgrep_program_%s : Lrgrep_runtime.program = {\n"
    rule.Syntax.name;
  print "  registers = %d;\n" registers;
  print "  initial = %d;\n" remap.(initial);
  print "  table = %S;\n" table;
  print "  code = %S;\n" program;
  print "}\n"

let output_wrapper out rule =
  let {Syntax.name; args; _} = rule in
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

module Make (Info : Info.S) (Spec : SPEC) :
sig
  val output_header : printer
  val output_trailer : printer

  module Rule
      (R : RULE)
      (B : BRANCH with type g := Info.g)
      (M : MACHINE with module Info := Info and type branch := B.n) :
  sig
    val output : printer
  end
end =
struct
  open Info
  open Spec

  let output_header out =
    Option.iter begin fun out ->
      begin match Grammar.Grammar.parameters with
        | [] -> ()
        | parameters ->
          Code_printer.print out "module Make";
          List.iter (Code_printer.fmt out "(%s)") parameters;
          Code_printer.fmt out
            "(%s : module type of %s.Make" parser_name parser_name;
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
      print_literal_code out lexer_definition.Syntax.header
    end out;
    Option.iter begin fun out ->
      Code_printer.fmt out
        "include Lrgrep_runtime.Interpreter(%s.MenhirInterpreter)\n"
        parser_name;
    end out

  let output_trailer out =
    Option.iter begin fun out ->
      Code_printer.print out "\n";
      print_literal_code out lexer_definition.Syntax.trailer;
      begin match Grammar.Grammar.parameters with
        | [] -> ()
        | _ -> Code_printer.print out "\nend\n"
      end;
    end out

  module Rule
      (R : RULE)
      (B : BRANCH with type g := Info.g)
      (M : MACHINE with module Info := Info and type branch := B.n) =
  struct
    open R

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

    let output_execute_function out =
      Code_printer.fmt out
        "let lrgrep_execute_%s %s\n\
        \  (__clause, (__registers : %s.MenhirInterpreter.element Lrgrep_runtime.register_values))\n\
        \  (__initialpos : Lexing.position)\n\
        \  ((token : %s.MenhirInterpreter.token), _startloc_token_, _endloc_token_)\n\
        \  : _ option = match __clause, token with\n"
        rule.name (String.concat " " rule.args)
        parser_name parser_name;
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

    let get_state_for_compaction index =
      let add_match (clause, priority, regs) =
        let cap = B.captures clause in
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
      let transitions = M.outgoing index in
      {
        Lrgrep_support.
        accept = List.map add_match (M.matching index);
        halting = M.unhandled index;
        transitions = IndexSet.fold add_transition transitions [];
      }

    let output_table out (program, table, remap) =
      let print fmt = Code_printer.fmt out fmt in
      print
        "let lrgrep_program_%s : Lrgrep_runtime.program = {\n"
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

    let output_wrapper out =
      let {Syntax.name; args; _} = rule in
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

    let output out =
      Option.iter begin fun out ->
        let program =
          Lrgrep_support.compact M.states get_state_for_compaction
        in
        output_table out program;
        output_execute_function out;
        output_wrapper out
      end out
  end
end
