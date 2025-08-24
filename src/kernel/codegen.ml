open Fix.Indexing
open Utils
open Misc
open Info
open Spec

type priority = int

type spec = {
  parser_name : string;
  lexer_definition : Syntax.lexer_definition;
}

let print_literal_code cp (loc, txt) =
  if txt <> "" then
    Code_printer.print cp ~loc txt

let output_table (type g r) out rule (machine : (g, r, _, _) Automata.Machine.t)
    (program, table, remap) =
  let print fmt = Code_printer.fmt out fmt in
  print "let lrgrep_program_%s : Lrgrep_runtime.program = {\n"
    rule.Syntax.name;
  print "  registers = %d;\n" machine.register_count;
  print "  initial = %d;\n" (
    match machine.initial with
    | None -> 0
    | Some i -> remap.((i : _ index :> int))
  );
  print "  table = %s;\n" (fst (Lrgrep_support_packer.encode table));
  print "  code = %S;\n" program;
  print "}\n"

let output_wrapper out {Syntax.name; args; _} =
  let args = String.concat " " args in
  Code_printer.fmt out
    "let %s %s _lrgrep_env _lrgrep_lookahead = (\n\
    \  List.find_map\n\
    \    (fun m -> lrgrep_execute_%s %s m _lrgrep_lookahead)\n\
    \    (lrgrep_run lrgrep_program_%s _lrgrep_env)\n\
     )\n"
    name args
    name args
    name

type printer = Code_printer.t option -> unit

let grammar_parameters g =
  let (module Raw) = raw g in
  Raw.Grammar.parameters

let output_header (type g) (g : g grammar) spec : printer = function
  | None -> ()
  | Some out ->
    begin match grammar_parameters g with
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

let output_trailer (type g) (g : g grammar) spec : printer = function
  | None -> ()
  | Some out ->
    Code_printer.print out "\n";
    print_literal_code out spec.lexer_definition.Syntax.trailer;
    match grammar_parameters g with
    | [] -> ()
    | _ -> Code_printer.print out "\nend\n"

let output_rule (type g r) (g : g grammar) {parser_name; _} (rule : Syntax.rule)
    clauses (branches : (g, r) branches) (machine : (g, r, _, _) Automata.Machine.t) : printer =
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
        let label = machine.label.:(tr) in
        let actions = {
          Lrgrep_support.
          move = IndexMap.bindings label.moves;
          store = List.map snd label.captures;
          clear = IndexSet.elements label.clear;
          target = machine.target.:(tr);
          priority = label.priority;
        } in
        (label.filter, actions) :: acc
      in
      {
        Lrgrep_support.
        accept = List.map add_match machine.accepting.:(index);
        halting = machine.unhandled.:(index);
        transitions = IndexSet.fold add_transition machine.outgoing.:(index) [];
      }
    in
    let program = Lrgrep_support.compact (Automata.Machine.states machine) get_state_for_compaction in
    output_table out rule machine program;
    (* Step 2: output semantic actions *)
    let captures_lr1 =
      let map = ref IndexMap.empty in
      let process_transitions (label : _ Automata.Machine.label) =
        map := List.fold_left (fun map (cap, _reg) ->
            IndexMap.update cap (Misc.union_update label.filter) map
          ) !map label.captures
      in
      Vector.iter process_transitions machine.label;
      !map
    in
    let recover_type index =
      try
        let lr1s = IndexMap.find index captures_lr1 in
        let symbols = IndexSet.map (fun lr1 ->
            match Lr1.incoming g lr1 with
            | None -> raise Not_found
            | Some sym -> sym
          ) lr1s
        in
        let typ = IndexSet.fold (fun sym acc ->
            let typ = match Symbol.semantic_value g sym with
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
    let symbol_matcher s =
      (if Symbol.is_terminal g s then "T T_" else "N N_") ^
      Symbol.name g ~mangled:true s
    in
    let bind_capture out ~offset index (def, name, (_startpos, _endpos, positions)) =
      (* FIXME: variables should be introduced only if the relevant keyword appear in the action *)
      let is_optional = IndexSet.mem index machine.partial_captures in
      let none = if is_optional then "None" else "assert false" in
      let some x = if is_optional then "Some (" ^ x ^ ")" else x in
      match def with
      | Syntax.Value ->
        let typ = recover_type index in
        Code_printer.fmt out
          "    let %s, _startpos_%s_, _endpos_%s_, _positions_%s_ = match __registers.(%d) with \n\
          \      | Empty -> %s\n\
          \      | Location _ -> assert false\n\
          \      | Value (%s.MenhirInterpreter.Element (%s, %s, startp, endp)%s) ->\n"
          name name name name offset
          (if is_optional then "(None, None, None, None)" else "assert false")
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
        positions := false;
        Code_printer.fmt out "        (%s, %s, %s, %s)\n"
          (some "x") (some "startp") (some "endp") (some "(startp, endp)");
        Code_printer.fmt out "    in\n";
        Code_printer.fmt out "    let _ = %s in\n" name
      | Start_loc ->
        Code_printer.fmt out
          "    let _startpos_%s_ = match __registers.(%d) with\n\
          \      | Empty -> %s\n\
          \      | Location (p, _) | Value (%s.MenhirInterpreter.Element (_, _, p, _)) -> %s\n\
          \    in\n"
          name offset
          none
          parser_name (some "p")
      | End_loc ->
        Code_printer.fmt out
          "    let _endpos_%s_ = match __registers.(%d) with\n\
          \      | Empty -> %s\n\
          \      | Location (_, p) | Value (%s.MenhirInterpreter.Element (_, _, _, p)) -> %s\n\
          \    in\n"
          name offset
          none
          parser_name (some "p")
    in
    let lookahead_constraint branch =
      match branches.lookaheads.:(branch) with
      | None -> None
      | Some terms ->
        let term_pattern t =
          Terminal.to_string g t ^
          match Terminal.semantic_value g t with
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
        \  ((token : %s.MenhirInterpreter.token), _startloc_token_, _endloc_token_)\n\
        \  : _ option = match __clause, token with\n"
        rule.name (String.concat " " rule.args)
        parser_name parser_name;
      let output_clause_branches clause brs =
        let captures = clauses.captures.:(clause) in
        Code_printer.fmt out " ";
        (* Identify branches that lead to this action *)
        IndexSet.iter (fun branch ->
            Code_printer.fmt out
              " | %d, %s"
              (Index.to_int branch)
              (Option.value (lookahead_constraint branch) ~default:"_");
          ) brs;
        Code_printer.fmt out " ->\n";
        let vars = Hashtbl.create 7 in
        let captures =
          IndexMap.map begin fun (kind, var) ->
            let refs = match Hashtbl.find_opt vars var with
              | Some refs -> refs
              | None ->
                let refs = (ref false, ref false, ref false) in
                Hashtbl.add vars var refs;
                refs
            in
            (kind, var, refs)
          end captures
        in
        let body =
          match clauses.syntax.:(clause).action with
          | Unreachable -> ""
          | Partial (loc, str) | Total (loc, str) ->
            Misc.rewrite_keywords begin fun pos kw var ->
              match kw with
              (* FIXME Report an error message rather than a failure *)
              | "$startloc" -> Syntax.error pos "$startloc is now called $startpos"
              | "$endloc" ->  Syntax.error pos "$endloc is now called $endpos"
              | "$startpos" | "$endpos" | "$positions" ->
                (* FIXME Check if variable exists *)
                begin match Hashtbl.find_opt vars var with
                  | None -> Syntax.error pos "undefined variable %s" var
                  | Some (rstart, rend, rpos) ->
                    match kw with
                    | "$startpos" -> rstart := true
                    | "$endpos" -> rend := true
                    | "$positions" -> rpos := true
                    | _ -> ()
                end;
                true
              | kw ->
                Syntax.error pos "unknown keyword %S; did you mean $startpos, $endpos or $positions?" kw
            end loc str
        in
        let offset = ref 0 in
        IndexMap.iter
          (fun k v -> bind_capture out ~offset:!offset k v; incr offset)
          captures;
        IndexMap.iter (fun index (_, var, (_, _, positions)) ->
            if !positions then (
              let is_optional = IndexSet.mem index machine.partial_captures in
              if is_optional then
                Code_printer.fmt out
                  "    let _positions_%s_ = match _startpos_%s_, _endpos_%s_ with\n\
                  \      | Some s, Some e -> Some (s, e)\n\
                  \      | _ -> None in\n"
                  var var var
              else
                Code_printer.fmt out
                  "    let _positions_%s_ = (_startpos_%s_, _endpos_%s_) in\n"
                  var var var
            )
          ) captures;
        begin
          match clauses.syntax.:(clause).action with
          | Unreachable ->
            Code_printer.print out "    failwith \"Should be unreachable\"\n"
          | Partial (loc, _) ->
            Code_printer.print out "    (\n";
            Code_printer.fmt out ~loc "%s\n" body;
            Code_printer.print out "    )\n"
          | Total (loc, _) ->
            Code_printer.print out "    Some (\n";
            Code_printer.fmt out ~loc "%s\n" body;
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
