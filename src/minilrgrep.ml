open Utils
open Fix.Indexing

(* This file is a minimal implementation of LRgrep, supporting only
   Reduce-filter patterns (chapter 2) and with over-approximated coverage
   (to avoid dealing with rechability analysis (chapter 7) *)

let usage () =
  Printf.eprintf
    "Minilrgrep\n\
     \n\
     Usage:\n\
     %s compile <grammar.cmly> <spec.mlyl>\n\
    " Sys.argv.(0)

let grammar_file, spec_file =
  match Sys.argv with
  | [|_; "compile"; grammar_file; spec_file|] ->
    grammar_file, spec_file
  | _ ->
    usage ();
    exit 1

module Grammar = MenhirSdk.Cmly_read.Read(struct let filename = grammar_file end)

module Info = Kernel.Info.Make(Grammar)

let list_from_set ss f =
  IndexSet.fold (fun s acc ->
      match f s with
      | None -> acc
      | Some x -> x :: acc
    ) ss []

let time = Stopwatch.enter Stopwatch.main "Reduction automaton"

(* Implementation of 2.3.2 : The viable reduction NFA *)

module Reduction_NFA = struct
  open Info

  (* Define states as described by in the thesis.
     Two simplifications:
     - The stack suffix is represented by a single list with the current state
       as the first element.
       Thus, what is described in the document as [s . s'] is represented by a
       list [List.rev (s :: s')].
     - During reduction, we do not care about the actual symbols to remove
       (written α) but only by the number of symbols, thus we only store |α| as
       an [int] *)
  type state =
    | Initial
    | Suffix of Lr1.t list * Terminal.set
    | Reduce of Lr1.t list * Terminal.set * Nonterminal.t * int


  (* The label of a transition is an optional LR(1) state, where [None] denotes ϵ. *)
  type label = Lr1.t option

  (* An (outgoing) transition is represented as a pair of a label and a target state. *)
  type transition = label * state

  (* Since we are compiling error-matching patterns, we are only interested in
     the subset of the reduction automaton starting from wait states.
     (See 4.2.1). *)
  let initial_lr1_states = Lr1.wait

  let transitions : state -> transition list = function
    | Initial ->
      (* Visit all initial states *)
      list_from_set initial_lr1_states
        (fun lr1 -> Some (Some lr1, Suffix ([lr1], Terminal.all)))
    | Suffix (stack, lookaheads) ->
      (* The current state of the LR automaton is at the top of the stack *)
      let current = List.hd stack in
      (* Visit all reductions applicable to the top state *)
      list_from_set (Reduction.from_lr1 current)
        (fun red ->
           (* Find lookaheads compatible with this reduction *)
           let lookaheads =
             Terminal.intersect lookaheads (Reduction.lookaheads red)
           in
           if IndexSet.is_empty lookaheads then
             (* Give up if there are none *)
             None
           else
             (* Otherwise, enter a reduction state *)
             let production = Reduction.production red in
             let lhs = Production.lhs production in
             let rhs = Production.length production in
             Some (None, Reduce (stack, lookaheads, lhs, rhs))
        )
    (* End of a reduction sequence *)
    | Reduce (stack, lookaheads, lhs, 0) ->
      (* The current state of the LR automaton is at the top of the stack *)
      let current = List.hd stack in
      (* Find the target of the goto transition labelled [lhs] *)
      let target = Transition.find_goto_target current lhs in
      [None, Suffix (target :: stack, lookaheads)]
    (* Reducing an empty suffix: consume one more symbol of the stack *)
    | Reduce ([current], lookaheads, lhs, n) ->
      (* Look at possible predecessors *)
      list_from_set (Lr1.predecessors current)
        (fun lr1 ->
           Some (Some lr1, Reduce ([lr1], lookaheads, lhs, n - 1)))
    (* Reducing from the current suffix *)
    | Reduce (stack, lookaheads, lhs, n) ->
      [None, Reduce (List.tl stack, lookaheads, lhs, n - 1)]

  (* (* Explicit representation of the NFA *)
  include IndexBuffer.Gen.Make()

  let states = get_generator ()

  (* A hashtable to memoize visited states *)
  let table = Hashtbl.create 7

  let rec visit state =
    match Hashtbl.find_opt table state with
    | Some index -> index
    | None ->
      let slot = IndexBuffer.Gen.reserve states in
      Hashtbl.add table state (IndexBuffer.Gen.index slot);
      let transitions =
        List.map (fun (label, state) -> (label, visit state)) (transitions state)
      in
      IndexBuffer.Gen.commit states slot (state, transitions);
      IndexBuffer.Gen.index slot

  let initial = visit Initial

  let states = IndexBuffer.Gen.freeze states

  let () =
    Printf.eprintf "Constructed the Reduction NFA with %d states\n" (cardinal n) *)
end

(* Determinize the automaton and use an explicit representation *)

module Reduction_DFA = struct
  open Info

  (* Determinize and close over ϵ-transitions *)
  let rec fold_transitions acc (state : Reduction_NFA.state) : Reduction_NFA.state list Lr1.map =
    List.fold_left (fun acc (label, state') ->
        match label with
        | None -> fold_transitions acc state'
        | Some lr1 ->
          IndexMap.update lr1
            (function None -> Some [state'] | Some states -> Some (state' :: states))
            acc
      ) acc (Reduction_NFA.transitions state)

  (* Explicit representation focusing on suffixes *)

  type suffix = {
    stack: Lr1.t list;
    lookaheads: Terminal.set;
  }

  let time = Stopwatch.enter time "Determinization"

  include IndexBuffer.Gen.Make()

  type config = {
    suffixes: suffix list;
    transitions: n index Lr1.map;
  }

  let states = get_generator ()

  (* A hashtable to memoize visited states *)
  let table = Hashtbl.create 7

  let rec visit nstates =
    let nstates = List.sort_uniq compare nstates in
    match Hashtbl.find_opt table nstates with
    | Some index -> index
    | None ->
      let slot = IndexBuffer.Gen.reserve states in
      Hashtbl.add table nstates (IndexBuffer.Gen.index slot);
      let transitions =
        nstates
        |> List.fold_left fold_transitions IndexMap.empty
        |> IndexMap.map visit
      in
      let suffixes = List.filter_map (function
          | Reduction_NFA.Initial | Reduce _ -> None
          | Reduction_NFA.Suffix (stack, lookaheads) -> Some {stack; lookaheads}
        ) nstates
      in
      IndexBuffer.Gen.commit states slot {suffixes; transitions};
      IndexBuffer.Gen.index slot

  let initial = visit [Reduction_NFA.Initial]

  let states = IndexBuffer.Gen.freeze states

  let () = Stopwatch.step time
      "Constructed the Reduction DFA with %d states" (cardinal n)

  let predecessors = Vector.make n IndexSet.empty

  let () = Vector.iteri (fun source config ->
      IndexMap.iter
        (fun _lr1 target -> Misc.vector_set_add predecessors target source)
        config.transitions
    ) states

  let () = Stopwatch.leave time
end

(* A reduce-filter pattern translate to a set of suffixes.
   To quickly recognize patterns, we want to index Reduction_DFA states
   by the suffixes they can reach. *)
module Suffixes = struct

  let time = Stopwatch.enter time "Pre-computing suffixes"

  include IndexBuffer.Gen.Make()

  let desc = get_generator ()

  let of_state =
    let table = Hashtbl.create 7 in
    let visit_suffix suffix =
      match Hashtbl.find_opt table suffix with
      | Some index -> index
      | None ->
        let index = IndexBuffer.Gen.add desc suffix in
        Hashtbl.add table suffix index;
        index
    in
    let visit_state {Reduction_DFA.suffixes; _} =
      IndexSet.of_list (List.map visit_suffix suffixes)
    in
    Vector.map visit_state Reduction_DFA.states

  let desc = IndexBuffer.Gen.freeze desc

  let reachable = Vector.copy of_state

  let () =
    Misc.fix_relation Reduction_DFA.predecessors of_state
      ~propagate:(fun _ s _ s' -> IndexSet.union s s')

  let () = Stopwatch.leave time
end

module Input = struct
  let ic =
    try open_in_bin spec_file
    with
    | Sys_error msg ->
      Printf.eprintf "Error: Cannot open specification file (%S).\n" msg;
      exit 1
    | exn ->
      Printf.eprintf "Error: Cannot open specification file %S (%s).\n"
        spec_file (Printexc.to_string exn);
      exit 1

  let lexbuf =
    Front.Lexer.ic := Some ic;
    Lexing.from_channel ~with_positions:true ic

  let ast =
    Lexing.set_filename lexbuf spec_file;
    try Front.Parser.lexer_definition Front.Lexer.main lexbuf
    with
    | Front.Lexer.Lexical_error {msg; file; line; col} ->
      Printf.eprintf "Error %s:%d-%d: %s\n" file line col msg;
      exit 1
    | Front.Parser.Error ->
      let pos = lexbuf.lex_start_p in
      Printf.eprintf "Error %s:%d-%d: %s\n"
        pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol) "Parse error";
      exit 1

  let () =
    Front.Lexer.ic := None;
    close_in_noerr ic
end

module Transl = struct
  open Info
  open Kernel.Syntax

  let error pos fmt =
    Printf.eprintf "Error line %d, column %d: " pos.line pos.col;
    Printf.kfprintf (fun oc ->
        Printf.fprintf oc "\n";
        exit 1
      ) stderr fmt

  let string_of_symbol =
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

  let parse_symbol =
    let table = Hashtbl.create 7 in
    let add_symbol s = Hashtbl.add table (Symbol.name ~mangled:false s) s in
    Index.iter Symbol.n add_symbol;
    Hashtbl.find_opt table

  let parse_filter rhs =
    let dot = ref (-1) in
    let rec prepare_symbols index = function
      | [] ->
        if !dot = -1 then
          error (snd (List.hd rhs)) "expecting a '.' in the filter";
        []
      | (Skip, pos) :: _ ->
        error pos "'_*' syntax is not supported in filter"
      | (Dot, pos) :: rest ->
        if !dot <> -1 then
          error pos "only one '.' per filter is supported";
        dot := index;
        prepare_symbols index rest
      | (Find sym, pos) :: rest ->
        let sym = match sym with
          | None -> None
          | Some sym ->
            let sym = string_of_symbol sym in
            match parse_symbol sym with
            | Some sym -> Some sym
            | None ->
              error pos "unknown symbol %S" sym
        in
        sym :: prepare_symbols (index + 1) rest
    in
    let symbols = Array.of_list (prepare_symbols 0 rhs) in
    (symbols, !dot)

  let process_filter lhs rhs =
    let symbols, dot = parse_filter rhs in
    let match_prod prod =
      begin match lhs with
        | None -> true
        | Some n -> Index.equal n (Production.lhs prod)
      end &&
      Array.length symbols = Production.length prod &&
      Array.for_all2 (fun sym -> function
          | None -> true
          | Some sym' -> Index.equal sym sym'
        ) (Production.rhs prod) symbols
    in
    let productions = IndexSet.init_from_set Production.n match_prod in
    let match_lr1 lr1 =
      List.exists
        (fun (prod, n) -> n = dot && IndexSet.mem prod productions)
        (Lr1.items lr1)
    in
    IndexSet.init_from_set Lr1.n match_lr1

  let rec flatten expr = match expr.desc with
    | Concat exprs -> List.concat_map flatten exprs
    | desc -> [desc]

  let process_expr (expr : regular_expr) =
    match expr.desc with
    | Atom (_, _, _)
    | Alternative _
    | Repetition _
    | Reduce _
    | Concat _
    | Filter _ -> ()

  let process_pattern (pattern : pattern) =
    pattern.expr

  let process_clause (clause : clause) =
    clause.patterns

  let process_entry (entry : entry) =
    entry.clauses

  let entries = List.map process_entry Input.ast.entrypoints

end
