open Utils
open Fix.Indexing

(* This file is a minimal implementation of LRgrep, supporting only
   Reduce-filter patterns (chapter 2) and with over-approximated coverage
   (to avoid dealing with reachability analysis (chapter 7)). *)

(* Print usage information for the program *)
let usage () =
  Printf.eprintf
    "Minilrgrep\n\
     \n\
     Usage:\n\
     %s compile <grammar.cmly> <spec.mlyl>\n\
    " Sys.argv.(0)

(* Parse command line arguments for grammar and specification files *)
let grammar_file, spec_file =
  match Sys.argv with
  | [|_; "compile"; grammar_file; spec_file|] ->
    grammar_file, spec_file
  | _ ->
    usage ();
    exit 1

(* Read Menhir grammar and LR automaton using MenhirSdk *)
module Grammar = MenhirSdk.Cmly_read.Read(struct let filename = grammar_file end)

(* Information module is the representation of the grammar and LR automaton we
   are going to use. *)
module Info = Kernel.Info.Make(Grammar)

(* Helper function to convert a set to a list, applying a function [f] to each
   element *)
let list_from_set ss f =
  IndexSet.fold (fun s acc ->
      match f s with
      | None -> acc
      | Some x -> x :: acc
    ) ss []

(* Helper to print error message and exit *)
let error (pos : Kernel.Syntax.position) fmt =
  Printf.eprintf "Error line %d, column %d: " pos.line pos.col;
  Printf.kfprintf (fun oc -> Printf.fprintf oc "\n"; exit 1) stderr fmt

(* Helper to print warning message *)
let warn (pos : Kernel.Syntax.position) fmt =
  Printf.eprintf "Warning line %d, column %d: " pos.line pos.col;
  Printf.kfprintf (fun oc -> Printf.fprintf oc "\n") stderr fmt

(* Start timing the construction of the reduction automaton *)
let time = Stopwatch.enter Stopwatch.main "Reduction automaton"

(* Module implementing the reduction NFA.
   The implementation follows section 2.3.2. *)
module Reduction_NFA = struct
  open Info

  (* Define states of the reduction NFA.
     - [Initial]: initial state.
     - [Suffix]: represents a stack suffix with a set of lookaheads.
     - [Reduce]: represents a state during reduction with a stack suffix,
                 set of lookaheads, nonterminal being reduced, and number of symbols to reduce.

     Compared to the description in the thesis, the two main simplifications are:
     - The stack suffix is represented by a single list with the current state
       as the first element.
       Thus, what is described in the document as [s . s'] is represented by a
       list [List.rev (s :: s')].
     - During reduction, we do not care about the actual symbols to remove
       (written α) but only by the number of symbols, thus we only store |α| as
       an [int]
  *)
  type state =
    | Initial
    | Suffix of Lr1.t list * Terminal.set
    | Reduce of Lr1.t list * Terminal.set * Nonterminal.t * int

  (* Label of a transition is an optional LR(1) state, where [None] denotes ϵ (epsilon). *)
  type label = Lr1.t option

  (* Transition is represented as a pair of a label and a target state. *)
  type transition = label * state

  (* Reductions are initiated only from wait LR(1) states.

     In the thesis, the reduction automaton is built on the whole LR automaton,
     wait states are only introduced in section 4.2.1.
     Since we are only compiling reduce-filter patterns matching errors, we the
     subset of the reduction automaton starting from wait states is sufficient.
  *)
  let initial_reductions = Lr1.wait

  (* Function to get transitions from a given state *)
  let transitions : state -> transition list = function
    | Initial ->
      (* Visit all initial states *)
      list_from_set initial_reductions
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
end

(* Module to determinize the NFA and create an explicit representation of the DFA.

   If one is only interested in matching, the NFA is sufficient. The DFA is
   useful for enumeration and coverage.  Since we want all those features, it is
   simpler to implement everything on top of the DFA.
*)
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

  (* Explicit representation focusing on suffixes.

     A suffix [{stack; lookaheads}] abstracts the set of LR configurations whose
     stacks are suffixed by [stack] and looking ahead at a symbol in
     [lookaheads]. *)
  type suffix = {
    stack: Lr1.t list;
    lookaheads: Terminal.set;
  }

  (* Start timing the determinization process *)
  let time = Stopwatch.enter time "Determinization"

  include IndexBuffer.Gen.Make()

  (* Description of a DFA state.
     The DFA consumes a suffix of an LR stack and recognizes reductions that
     apply too it:
     - [suffixes] represent the intermediate configurations the can be reached
       by reductions applicable on this stack (reductions that succeded)
     - [transitions] list the transitions to other DFA states, when more
       reductions are potentially applicable but a longer suffix of the stack is
       needed to identify them.
  *)
  type desc = {
    suffixes: suffix list;
    transitions: n index Lr1.map;
  }

  (* Construct the DFA *)
  let initial, states =
    (* Generator for DFA states *)
    let states = get_generator () in
    (* Hashtable to memoize visited states. *)
    let table = Hashtbl.create 7 in
    (* Recursive function to visit and process states *)
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
            | Reduction_NFA.Initial | Reduction_NFA.Reduce _ -> None
            | Reduction_NFA.Suffix (stack, lookaheads) -> Some {stack; lookaheads}
          ) nstates
        in
        IndexBuffer.Gen.commit states slot {suffixes; transitions};
        IndexBuffer.Gen.index slot
    in
    (* Start exploration from the initial state *)
    let initial = visit [Reduction_NFA.Initial] in
    (* Freeze the DFA states *)
    let states = IndexBuffer.Gen.freeze states in
    (initial, states)

  (* Record the time taken for DFA construction *)
  let () = Stopwatch.step time "Constructed the Reduction DFA with %d states" (cardinal n)

  (* End timing the determinization process *)
  let () = Stopwatch.leave time
end

(* Optimization opportunities.

   These simple definitions lead to quite large automata. On OCaml 5.3.0
   grammar, ϵ-NFA has 90000 states and the DFA 40000 states.
   To make things faster, some optimizations are possible:
   - NFA: construct an NFA rather than an ϵ-NFA,
   - NFA: determinize reductions on-the-fly (explore all reductions applicable
     to a given state simultaneously)
   - NFA & DFA: introduce wildcard-labelled transitions (in 2.4.1)
   - NFA & DFA: merge goto-transitions with same target state
                (make the base of an abstract stack is a set of states)
*)

(* Pre-compute suffixes reachable from each DFA state.

   A reduce-filter pattern compiles to a set of suffixes,
   which themselves are recognized by scanning subset of the DFA
   reaching these suffixes.
   To make this efficient we pre-compute some informations:
   - The finite set of suffixes (they coincide with the "Reduction targets" of
     Section 2.3.3).
   - The subset of suffixes recognized by each DFA state.
   - The subset of suffixes recognized by states reachable from a DFA state.
*)
module Suffixes = struct

  (* Start timing the suffix computation *)
  let time = Stopwatch.enter time "Pre-computing suffixes"

  include IndexBuffer.Gen.Make()

  (* Generator for the finite set of suffixes *)
  let desc = get_generator ()

  (* Map each DFA state to a set of suffix indices *)
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

  (* Freeze the suffix descriptions *)
  let desc = IndexBuffer.Gen.freeze desc

  (* Vector to store reachable suffixes for each state *)
  let reachable = Vector.copy of_state

  (* Compute the transitive closure of reachable states *)
  let () =
    (* Pre-compute the predecessors for each DFA state *)
    let predecessors = Vector.make Reduction_DFA.n IndexSet.empty in
    let () = Vector.iteri (fun source desc ->
        IndexMap.iter
          (fun _lr1 target -> Misc.vector_set_add predecessors target source)
          desc.Reduction_DFA.transitions
      ) Reduction_DFA.states
    in
    (* Propagate reachable suffixes to predecessors of a state until reaching a
       fixed point. *)
    Misc.fix_relation predecessors of_state
      ~propagate:(fun _ s _ s' -> IndexSet.union s s')

  (* End timing the suffix computation *)
  let () = Stopwatch.leave time
end

(* We are done with preprocessing of the LR automaton.
   We now switch to processing the error specification. *)

(* Handle parsing using LRGrep's parser. *)
module Input = struct

  (* Open the specification file *)
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

  (* Create a lexer buffer from the file *)
  let lexbuf =
    Front.Lexer.ic := Some ic;
    Lexing.from_channel ~with_positions:true ic

  (* Parse the specification file into an abstract syntax tree (AST) *)
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

  (* Close the input channel *)
  let () =
    Front.Lexer.ic := None;
    close_in_noerr ic
end

(* Translate parsed specifications into subsets of suffixes.

   Since Minilrgrep only supports a subset of the DSL, many constructions are
   rejected during this step. *)
module Transl = struct
  open Info
  open Kernel.Syntax

  (* A more "semantic" representation of a filter.

     Filters have three representations in the pipeline.
     From highest to lowest level:
     - [Syntax.regular_expr], syntactic expressions
     - [filter], this type, which directly characterizes a stack suffix
       (with or without reductions)
     - [Suffixes.n indexset], a subset of suffixes we know can be reduced to
       (for reasoning modulo reduction)
  *)
  type filter =
    | Filter of Lr1.set * position
    | Consume of filter * Lr1.set * position

  (* Semantic representation of a branch. *)
  type branch = {
    (* Should the filter match literally or modulo reduction? *)
    reduce: bool;
    (* The filter the stack suffix should satisfy. *)
    filter: filter;
    (* The clause being recognized.  If this branch succeeds, semantic action
       from clause number [clause] should be executed. *)
    clause: int;
  }

  (* The default filter matches all states (restricting nothing) *)
  let default_filter position = Filter (Lr1.all, position)

  (* Refine a filter by intersecting it with another state set.

     Warn if no states are recognized. *)
  let refine_filter states = function
    | Filter (states', position) ->
      let states = Lr1.intersect states states' in
      if IndexSet.is_empty states then
        warn position "these filters reject all states";
      Filter (states, position)
    | Consume (filter', states', position) ->
      let states = Lr1.intersect states states' in
      if IndexSet.is_empty states then
        warn position "these filters reject all states";
      Consume (filter', states, position)

  (* A specification is a list of entries, made of a list of clauses, made of a
     list of branches. A branch is compiled to the set of suffixes that
     recognize it. *)
  type spec = (branch * Suffixes.n IndexSet.t) list

  (* Convert a symbol to its string representation.
     This serializes user-provided symbols to the format used by Menhir for
     monomorphized symbols, before mangling. *)
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

  (* Parse a symbol from its string representation.

     To recognize higher-order Menhir symbols (e.g. list(foo)), we index
     monomorphized symbols of the automaton in a hashtable and lookup
     user-provided ones by serializing them first.
  *)
  let parse_symbol =
    let table = Hashtbl.create 7 in
    let add_symbol s = Hashtbl.add table (Symbol.name ~mangled:false s) s in
    Index.iter Symbol.n add_symbol;
    fun pos sym ->
      let sym = string_of_symbol sym in
      match Hashtbl.find_opt table sym with
      | Some sym -> sym
      | None -> error pos "unknown symbol %S" sym

  (* Parse a filter to an item (symbol array and position of the dot) *)
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
        let sym = Option.map (parse_symbol pos) sym in
        sym :: prepare_symbols (index + 1) rest
    in
    let symbols = Array.of_list (prepare_symbols 0 rhs) in
    (symbols, !dot)

  (* Translate a filter to the set of matching LR(1) states *)
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

  (* Translate an atom to the set of LR1 states it matches *)
  let process_atom =
    (* Index states by their incoming symbol *)
    let table = Vector.make Symbol.n IndexSet.empty in
    Index.iter Lr1.n (fun lr1 -> match Lr1.incoming lr1 with
        | None -> ()
        | Some sym -> Misc.vector_set_add table sym lr1);
    fun pos -> function
    | None ->
      (* A wildcard symbol matches all states *)
      Lr1.all
    | Some sym ->
      (* A normal symbol matches states by their incoming symbol *)
      Vector.get table (parse_symbol pos sym)

  (* The subset of patterns supported by Minilrgrep can be summarized
     as a "sum of reduced products":
     - sum is a disjunction of branches
     - produces are concatenations of atoms and filters, identified modulo
       reductions.

     The following code extracts this subset from the AST and reject the rest.
  *)

  (* Flatten a concatenation expression into a filter *)
  let rec flatten_concat filter expr = match expr.desc with
    | Atom (Some _, _, _) | Reduce {capture = Some _; _} ->
      error expr.position "variable bindings are not supported"
    | Repetition _ ->
      error expr.position "repetitions are not supported"
    | Alternative _ ->
      error expr.position "only top-level disjunctions are supported"
    | Reduce _ ->
      error expr.position "only top-level reductions are supported"
    | Concat exprs ->
      List.fold_left flatten_concat filter exprs
    | Atom (None, sym, _) ->
      Consume (filter, process_atom expr.position sym, expr.position)
    | Filter {lhs; rhs} ->
      let lhs = match lhs with
        | None -> None
        | Some sym ->
          match Symbol.prj (parse_symbol expr.position sym) with
          | L _ -> error expr.position "expecting a non-terminal"
          | R n -> Some n
      in
      refine_filter (process_filter lhs rhs) filter

  (* Flatten an alternatives expression into a list of branches *)
  let rec flatten_alternatives clause expr = match expr.desc with
    | Atom (Some _, _, _) | Reduce {capture = Some _; _} ->
      error expr.position "variable bindings are not supported"
    | Repetition _ ->
      error expr.position "repetitions are not supported"
    | Alternative exprs ->
      List.concat_map (flatten_alternatives clause) exprs
    | Reduce {expr; _} ->
      let filter = flatten_concat (default_filter expr.position) expr in
      [{clause; filter; reduce = true}]
    | _ ->
      let filter = flatten_concat (default_filter expr.position) expr in
      [{clause; filter; reduce = false}]

  (* Extract branches from an entry in the specification *)
  let extract_branches (entry : entry) =
    let extract_pattern index (pattern : pattern) =
      begin match pattern.lookaheads with
        | [] -> ()
        | (_, pos) :: _ -> error pos "lookahead constraints are not supported"
      end;
      flatten_alternatives index pattern.expr
    in
    let extract_clause index (clause : clause) =
      List.concat_map (extract_pattern index) clause.patterns
    in
    List.concat (List.mapi extract_clause entry.clauses)

  (* Compile a branch into a set of suffixes that match the branch *)
  let compile_branch branch =
    let rec match_stack = function
      | [], _ | (_ :: _ :: _), Filter _ -> false
      | [top], Filter (states, _) ->
        IndexSet.mem top states
      | (top :: rest), Consume (filter, states, _) ->
        IndexSet.mem top states && match_stack (rest, filter)
    in
    match branch.filter with
    | Consume (_, _, pos) when branch.reduce ->
      error pos "atoms are not supported outside of reductions"
    | _ ->
      IndexSet.init_from_set Suffixes.n
        (fun suffix ->
           let desc = Vector.get Suffixes.desc suffix in
           match_stack (desc.stack, branch.filter))

  (* Produce a spec for each entry *)
  let branches : spec list =
    List.map (fun entry ->
        List.map
          (fun branch -> branch, compile_branch branch)
          (extract_branches entry)
      ) Input.ast.entrypoints
end
