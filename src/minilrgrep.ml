(** This file is a minimal implementation of LRgrep, supporting only
    Reduce-filter patterns (chapter 2) and with over-approximated coverage
    (to avoid dealing with reachability analysis, chapter 7). *)

open Utils
open Fix.Indexing
open Misc

(** Print usage information for the program *)
let usage () =
  Printf.eprintf
    "MiniLRGrep\n\
     \n\
     Usage:\n\
     %s compile <grammar.cmly> <spec.mlyl>\n\
     %s enumerate <grammar.cmly>\n\
     %s coverage <grammar.cmly> <spec.mlyl>\n\
    "
    Sys.argv.(0)
    Sys.argv.(0)
    Sys.argv.(0)

(** Parse command line arguments for grammar and specification files *)
let parse_arguments () =
  match Sys.argv with
  | [|_; "compile"; grammar_file; spec_file|] ->
    (true, grammar_file, Some spec_file)
  | [|_; "enumerate"; grammar_file|] ->
    (false, grammar_file, None)
  | _ ->
    usage ();
    exit 1

let compile, grammar_file, spec_file = parse_arguments ()

(* FIXME *)
let entry = ref ""

(* FIXME *)
let nt_inclusion = None
let clause_shadow = true

(** Read Menhir grammar and LR automaton using MenhirSdk *)
module Grammar = MenhirSdk.Cmly_read.Read(struct let filename = grammar_file end)

(** Information module is the representation of the grammar and LR automaton we
    are going to use. *)
open Kernel.Info
include Lift(Grammar)

(** {1 Helpers} *)

(** An invalid input position *)
let invalid_position = Kernel.Syntax.invalid_position

(** Accessing 'vectors' (strongly-typed arrays indexed by elements of a finite
    set) *)
let (.:()) = Vector.get
let (.:()<-) = Vector.set
let (.@()<-) v i f = v.:(i) <- f v.:(i)

(** Display an item as string *)
let item_to_string (prod, pos) =
  let comps = ref [] in
  let rhs = Production.rhs grammar prod in
  for i = Array.length rhs - 1 downto pos do
    comps := Symbol.name grammar rhs.(i) :: !comps
  done;
  comps := "." :: !comps;
  for i = pos - 1 downto 0 do
    comps := Symbol.name grammar rhs.(i) :: !comps
  done;
  Nonterminal.to_string grammar (Production.lhs grammar prod) ^ ": " ^ String.concat " " !comps

(** Convert a set to a list, mapping function [f] over elements *)
let list_from_set ss f =
  IndexSet.fold (fun s acc ->
      match f s with
      | None -> acc
      | Some x -> x :: acc
    ) ss []

(** Print position for error or warning messages *)
let print_file_position (pos : Kernel.Syntax.position) = match spec_file with
  | None -> ()
  | Some name ->
    Printf.eprintf "%s:" name;
    if pos.line > -1 then (
      Printf.eprintf "%d:" pos.line;
      if pos.col > -1 then
        Printf.eprintf "%d:" pos.col
    );
    Printf.eprintf " "

(** Print an error message and exit *)
let error pos fmt =
  print_file_position pos;
  Printf.eprintf "error: ";
  Printf.kfprintf (fun oc -> Printf.fprintf oc "\n"; exit 1) stderr fmt

(** Print a warning message *)
let warn (pos : Kernel.Syntax.position) fmt =
  print_file_position pos;
  Printf.eprintf "warning: ";
  Printf.kfprintf (fun oc -> Printf.fprintf oc "\n") stderr fmt

(** Check if a symbol is not nullable *)
let non_nullable_symbol sym =
  match Symbol.desc grammar sym with
  | T _ -> true
  | N n -> not (Nonterminal.nullable grammar n)

(** Lazily compute and memoize a function on a finite set *)
let lazy_lookup f =
  let vector = Lazy.from_fun f in
  fun x -> (Lazy.force vector).:(x)

(** When saving a graph, we can either generate a graphviz file or directly pipe
    the graph to 'xdot' *)
let open_file_or_xdot = function
  | "xdot" -> Unix.open_process_out "xdot -"
  | fname  -> open_out_bin fname

let close_file_or_xdot oc = function
  | "xdot" -> ignore (Unix.close_process_out oc)
  | _      -> close_out_noerr oc

(** {1 Analysis of the reductions} *)

(** Module implementing the reduction NFA, following section 2.3.2. *)
module Reduction_NFA = struct
  (** Define states of the reduction NFA.
      - [Initial]: initial state.
      - [Suffix]: represents a stack suffix with a set of lookaheads.
      - [Reduce]: represents a state during reduction with a stack suffix, set of
                  lookaheads, the nonterminal to `goto` at the end of the
                  reduction, and the number of symbols that remain to consume.

      Compared to the description in the thesis, the two main simplifications are:
      - The stack suffix is represented by a single list with the current state
        as the first element. Thus, what is described in the document as
        [s . s'] is represented by a list [List.rev (s :: s')].
      - During reduction, we do not care about the actual symbols to remove
        (written α) but only by the number of symbols, thus we only store |α| as
        an [int]
  *)
  type state =
    | Initial
    | Suffix of g lr1 index list * g terminal indexset
    | Reduce of g lr1 index list * g terminal indexset * g nonterminal index * int

  (** Label of a transition is an optional LR(1) state, where [None] denotes ϵ
      (epsilon). *)
  type label = g lr1 index option

  (** A transition is represented as a pair of a label and a target state. *)
  type transition = label * state

  (** Reductions are initiated only from wait LR(1) states.

      Since we are only recognizing reduce-filter patterns matching errors, the
      subset of the reduction automaton starting from wait states is sufficient
      (Definition 14 (wait states), section 4.2.1). *)
  let initial_reductions = Lr1.wait grammar

  (** The transition function *)
  let transitions : state -> transition list = function
    | Initial ->
      (* Visit all initial states *)
      list_from_set initial_reductions
        (fun lr1 -> Some (Some lr1, Suffix ([lr1], Terminal.all grammar)))
    | Suffix (stack, lookaheads) ->
      (* The current state of the LR automaton is at the top of the stack *)
      let current = List.hd stack in
      (* Visit all reductions applicable to the top state *)
      list_from_set (Reduction.from_lr1 grammar current)
        (fun red ->
           (* Find lookaheads compatible with this reduction *)
           let lookaheads =
             Terminal.intersect grammar lookaheads (Reduction.lookaheads grammar red)
           in
           if IndexSet.is_empty lookaheads then
             (* Give up if there are none *)
             None
           else
             (* Otherwise, enter a reduction state *)
             let production = Reduction.production grammar red in
             let lhs = Production.lhs grammar production in
             let rhs = Production.length grammar production in
             Some (None, Reduce (stack, lookaheads, lhs, rhs))
        )
    (* End of a reduction sequence *)
    | Reduce (stack, lookaheads, lhs, 0) ->
      (* The current state of the LR automaton is at the top of the stack *)
      let current = List.hd stack in
      (* Find the target of the goto transition labelled [lhs] *)
      let target = Transition.find_goto_target grammar current lhs in
      [None, Suffix (target :: stack, lookaheads)]
    (* Reducing an empty suffix: consume one more symbol of the stack *)
    | Reduce ([current], lookaheads, lhs, n) ->
      (* Look at possible predecessors *)
      list_from_set (Lr1.predecessors grammar current)
        (fun lr1 ->
           Some (Some lr1, Reduce ([lr1], lookaheads, lhs, n - 1)))
    (* Reducing from the current suffix *)
    | Reduce (stack, lookaheads, lhs, n) ->
      [None, Reduce (List.tl stack, lookaheads, lhs, n - 1)]
end

(** Module to determinize the NFA and create an explicit representation of the DFA

    If one is only interested in matching, the NFA is sufficient. The DFA is
    useful to make matching more efficient but necessary for coverage checks.
    Since we want all those features, it is simpler to implement everything on
    top of the DFA. *)
module Reduction_DFA = struct
  (** Explicit representation focusing on suffixes.

      A suffix [{stack; lookaheads}] abstracts the set of LR configurations
      whose stacks are suffixed by [stack] and looking ahead at a symbol
      in [lookaheads]. *)
  type suffix = {
    stack: g lr1 index list;
    lookaheads: g terminal indexset;
    child: suffix list;
  }

  (** Determinize states and close over ϵ-transitions, accumulating visited
      suffixes on the way while preserving the tree structure (such that we can
      tell which suffix allowed reaching another one via ϵ-transitions). *)
  let rec fold_transition acc (label, state) =
    match label with
    | None ->
      fold_transitions acc state
    | Some lr1 ->
      let suffixes, targets = acc in
      let targets =
        IndexMap.update lr1
          (function None -> Some [state] | Some states -> Some (state :: states))
          targets
      in
      (suffixes, targets)

  and fold_transitions acc (state : Reduction_NFA.state) =
    let transitions = Reduction_NFA.transitions state in
    match state with
    | Reduction_NFA.Initial | Reduce _ ->
      List.fold_left fold_transition acc transitions
    | Suffix (stack, lookaheads) ->
      let suffixes, targets = acc in
      let child, targets =
        List.fold_left fold_transition ([], targets) transitions
      in
      ({stack; lookaheads; child} :: suffixes, targets)

  include IndexBuffer.Gen.Make()

  (** Description of a DFA state.

      The DFA consumes a suffix of an LR stack and recognizes reductions that
      apply too it:
      - [suffixes] represent the intermediate configurations the can be reached
        by reductions applicable on this stack (reductions that succeded)
      - [transitions] list the transitions to other DFA states, when more
        reductions are potentially applicable but a longer suffix of the stack is
        needed to identify them. *)
  type desc = {
    suffixes: suffix list;
    transitions: (g lr1, n index) indexmap;
  }

  (** Construct the DFA *)
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
        let suffixes, transitions =
          List.fold_left fold_transitions ([], IndexMap.empty) nstates
        in
        let transitions = IndexMap.map visit transitions in
        IndexBuffer.Gen.commit states slot {suffixes; transitions};
        IndexBuffer.Gen.index slot
    in
    (* Start exploration from the initial state *)
    let initial = visit [Reduction_NFA.Initial] in
    (* Freeze the DFA states *)
    let states = IndexBuffer.Gen.freeze states in
    (initial, states)

  (** Record the time taken for DFA construction *)
  let () = stopwatch 2 "Constructed the Reduction DFA (%d states)" (cardinal n)

  (** Pre-compute the successors and predecessors *)
  let successors = Vector.map (fun desc ->
      IndexMap.fold (fun _ i is -> IndexSet.add i is)
        desc.transitions IndexSet.empty
    ) states

  let predecessors =
    Misc.relation_reverse n successors

  (** Record the time taken for computing successors/predecessors *)
  let () = stopwatch 2 "Pre-computed DFA predecessors and successors"

  (** End timing the determinization process *)
  let () = stopwatch 1 "Determinized the automaton"

  (* Pre-compute the shortest stack suffixes that permit reaching a DFA
     state.
     This is done by doing a BFS from the initial state and registering the
     first DFA-paths reaching each state. *)
  let stack_suffix =
    lazy_lookup begin fun () ->
      let table = Vector.make n [] in
      let todo = ref [initial, []] in
      let propagate (x, path) =
        match table.:(x) with
        | _ :: _ -> ()
        | [] ->
          table.:(x) <- path;
          IndexMap.iter
            (fun lr1 y -> Misc.push todo (y, lr1 :: path))
            states.:(x).transitions
      in
      Misc.fixpoint ~propagate todo;
      let () = stopwatch 2 "Computed stack suffixes" in
      table
    end

  (* Pre-compute the shortest stack prefix that permit reaching an LR(1)
     state (without including this state).
     This is done by doing a BFS from the entrypoints of the LR automaton and
     registering the first LR-paths reaching each state. *)
  let rev_stack_prefix =
    lazy_lookup begin fun () ->
      let table = Vector.make (Lr1.cardinal grammar) [] in
      let todo = ref [] in
      let propagate (x, path) =
        match table.:(x) with
        | _ :: _ -> ()
        | [] ->
          table.:(x) <- path;
          IndexSet.iter
            (fun tr -> Misc.push todo (Transition.target grammar tr, x :: path))
            (Transition.successors grammar x)
      in
      let entrypoints =
        Hashtbl.fold (fun _ -> IndexSet.add) (Lr1.entrypoint_table grammar) IndexSet.empty
      in
      IndexSet.iter
        (fun lr1 -> propagate (lr1, []))
        entrypoints;
      Misc.fixpoint ~propagate todo;
      let () = stopwatch 2 "Computed stack prefixes" in
      table
    end

  (** [stack_visiting dfa] returns an example LR(1) stack that reaches state
      [dfa] when analyzed. *)
  let stack_visiting dfa =
    let suffix = stack_suffix dfa in
    List.rev_append (rev_stack_prefix (List.hd suffix)) suffix

  (** In a few analyses, we are interested by the leaves of the DFA: the end of
      the sequences of reduction, after which no more reduction applies.

      By construction, they are simply the states with no successor: if we do a
      strongly connected components analysis, the leaves of the SCC DAG are
      always components with a single node. Otherwise, it means there is a path
      of the DFA that ends with a cycle, like two nodes A -> B -> A -> ...
      with no other successors. That would represent an infinite sequence of
      reductions consuming stack: not possible on a finite stack. *)
  let is_leaf st = IndexSet.is_empty successors.:(st)

  (* Optimization opportunities.

     These simple definitions lead to quite large automata.
     On OCaml 5.3 grammar, ϵ-NFA has 90000 states and the DFA 40000 states.
     To make things faster, some optimizations are possible:
     - NFA: compute the ϵ-closure on the fly, constructing an NFA rather than an ϵ-NFA,
     - NFA: determinize reductions on-the-fly (explore all reductions applicable
       to a given state simultaneously)
     - NFA & DFA: introduce wildcard-labelled transitions (from section 2.4.1);
                  in particular, this reduces the size of the DFA!
     - NFA & DFA: merge goto-transitions with same target state (by representing
                  the base of an abstract stack as a set of states); this
                  reduces the size of the DFA but make later analyses more
                  complex.
  *)
end

(** Reverse the action of reductions.

    Pre-compute the item closure for each state [st] (items [(prod,0)]: reducing
    [prod] is a way to follow a goto transition from [st]), and the paths that
    permit following a given goto transition.

    The item closure is useful for identifying which states match a filter, the
    paths are useful for generating sentences (see module [Sentence] below).
*)
module Unreductions = struct
  (** The productions appearing in items of the form [(prod, 0)] in the
      item-closure of an LR(1) state. *)
  let prods_by_lr1 = Vector.make (Lr1.cardinal grammar) IndexSet.empty

  (** A list of paths, expressed as a sequence of transitions, that permit
      following a goto transition when reduced.
      E.g., if [p⃗ ∈ paths.:(goto)], then if a stack ends in [p⃗], it is possible
      to reduce the rule [goto_symbol(goto): List.map Transition.symbol p⃗] and
      doing it leads to following the transition [goto].

      We can use this as part of answering reachability questions: which input
      sequences permits following the transition [goto]?*)
  let paths = Vector.make (Transition.goto grammar) []

  let () = Index.iter (Lr1.cardinal grammar) (fun lr1 ->
      let prods =
        IndexSet.elements (Reduction.from_lr1 grammar lr1)
        |> List.map (Reduction.production grammar)
        |> List.sort (fun p1 p2 ->
            Int.compare (Production.length grammar p1)
                        (Production.length grammar p2))
      in
      let rec steps depth lr1 path = function
        | [] -> ()
        | p :: ps when Production.length grammar p = depth ->
          prods_by_lr1.@(lr1) <- IndexSet.add p;
          paths.@(Transition.find_goto grammar lr1 (Production.lhs grammar p)) <- List.cons path;
          steps depth lr1 path ps
        | ps ->
          IndexSet.iter
            (fun tr -> steps (depth + 1) (Transition.source grammar tr) (tr :: path) ps)
            (Transition.predecessors grammar lr1)
      in
      steps 0 lr1 [] prods
    )

  let () = stopwatch 1 "Computed item closure"
end

(** Pre-compute suffixes reachable from each DFA state.

    A reduce-filter pattern compiles to a set of suffixes, which themselves are
    recognized by scanning the subset of the DFA reaching these suffixes.
    To make this efficient we pre-compute some information:
    - The finite set of suffixes (they coincide with the "Reduction targets" of
      Section 2.3.3).
    - The subset of suffixes recognized by each DFA state.
    - The subset of suffixes recognized by states reachable from a DFA state.
*)
module Suffix = struct
  (** Start timing the suffix computation *)
  include IndexBuffer.Gen.Make()
  type set = n indexset

  (** Generator for the finite set of suffixes *)
  let desc = get_generator ()

  (** Map each DFA state to a set of suffix indices it recognizes *)
  let of_dfa =
    let table = Hashtbl.create 7 in
    let visit_state dfa {Reduction_DFA.suffixes; _} =
      let rec visit_suffix acc suffix =
        match Hashtbl.find_opt table suffix with
        | Some is -> IndexSet.union is acc
        | None ->
          let index = IndexBuffer.Gen.add desc (dfa, suffix) in
          let is =
            List.fold_left visit_suffix (IndexSet.singleton index)
              suffix.Reduction_DFA.child
          in
          Hashtbl.add table suffix is;
          IndexSet.union is acc
      in
      List.fold_left visit_suffix IndexSet.empty suffixes
    in
    Vector.mapi visit_state Reduction_DFA.states

  (** Map each suffix to the DFA state to a set of suffix indices *)
  let to_dfa = lazy_lookup (fun () -> Misc.relation_reverse n of_dfa)

  (** Freeze the suffix descriptions *)
  let desc = IndexBuffer.Gen.freeze desc

  let () = stopwatch 2 "Generated set of all suffixes (%d unique suffixes)" (cardinal n)

  (** Compute the transitive closure of reachable states *)
  let reachable =
    (* Propagate reachable suffixes to predecessors of a state until reaching a
       fixed point. FIXME: Bottleneck, this computation is naive. *)
    let table = Vector.copy of_dfa in
    Misc.fix_relation Reduction_DFA.predecessors table
      ~propagate:(fun _ s _ s' -> IndexSet.union s s');
    table

  (** End timing the suffix computation *)
  let () = stopwatch 1 "Computed set of reachable suffixes"
end

(* We are done with preprocessing of the LR automaton.
   We now switch to processing the error specification. *)

(** Handle parsing using LRGrep's parser. *)
let ast = match spec_file with
  | None -> None
  | Some spec_file ->
    (* Open the specification file *)
    let ic =
      try open_in_bin spec_file
      with
      | Sys_error msg ->
        error invalid_position "cannot open specification file (%S).\n" msg;
      | exn ->
        error invalid_position "cannot open specification file (%S).\n"
          (Printexc.to_string exn)
    in
    (* Create a lexer buffer from the file *)
    let lexbuf =
      Front.Lexer.ic := Some ic;
      Lexing.from_channel ~with_positions:true ic
    in
    (* Parse the specification file into an abstract syntax tree (AST) *)
    let ast =
      Lexing.set_filename lexbuf spec_file;
      try Front.Parser.lexer_definition Front.Lexer.main lexbuf
      with
      | Front.Lexer.Lexical_error {msg; file=_; line; col} ->
        error {Kernel.Syntax. line; col} "%s" msg
      | Front.Parser.Error ->
        let pos = lexbuf.lex_start_p in
        let line = pos.pos_lnum in
        let col = pos.pos_cnum - pos.pos_bol in
        error {Kernel.Syntax.line; col} "syntax error"
    in
    (* Close the input channel *)
    let () =
      Front.Lexer.ic := None;
      close_in_noerr ic
    in
    Some ast

(** Translate parsed specifications into subsets of suffixes.

    MiniLRGrep only supports a subset of the DSL; unsupported constructions are
    rejected during this step. *)
module Transl = struct
  open Kernel.Syntax

  (** A more "semantic" representation of a filter.

      [Filter], the base case, restricts but do not consume accepted states.
      [Consume] recognizes stacks whose top states belong to a certain subset and
      apply another filter to the remainder of the stack.

      Filters have three representations in the pipeline.
      From highest to lowest level:
      - [Syntax.regular_expr], syntactic expressions
      - [filter], this type, which directly characterizes a stack suffix
        (with or without reductions)
      - [Suffix.n indexset], a subset of suffixes we know can be reduced to
        (for reasoning modulo reduction)
   *)
  type filter =
    | Filter of g lr1 indexset * position
    | Consume of filter * g lr1 indexset * position

  type reduce =
    | Reduce_yes
    | Reduce_no
    | Reduce_auto (* Special case for supporting [_* / ...] syntax which lets
                     LRGrep finds out what needs to be reduced, if necessary,
                     for a pattern to match *)

  (** Semantic representation of a branch. *)
  type branch = {
    (* Should the filter match literally or modulo reduction? *)
    reduce: reduce;
    (* The filter the stack suffix should satisfy. *)
    filter: filter;
    (* Starting position of the pattern defining this branch,
       for reporting error/warnings. *)
    position: position;
  }

  (** The default filter matches all states (rejecting nothing). *)
  let default_filter position = Filter (Lr1.all grammar, position)

  (** Refine a filter by intersecting it with another state set.
      Warn if no states are recognized. *)
  let refine_filter states = function
    | Filter (states', position) ->
      let states = Lr1.intersect grammar states states' in
      if IndexSet.is_empty states then
        warn position "these filters reject all states";
      Filter (states, position)
    | Consume (filter', states', position) ->
      let states = Lr1.intersect grammar states states' in
      if IndexSet.is_empty states then
        warn position "these filters reject all states";
      Consume (filter', states, position)

  (** An entry is translated to a list of branches.
      A branch is compiled to the set of suffixes that recognize it. *)
  type compiled_branch = branch * Suffix.set

  (** Convert a symbol to its string representation *)
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

  (** Parse a symbol from the surface syntax to an LR symbol.

      To recognize higher-order Menhir symbols (e.g. list(foo)), we index
      monomorphized symbols of the LR automaton in a hashtable and lookup
      user-provided ones by serializing them first. *)
  let parse_symbol =
    let table = Hashtbl.create 7 in
    let add_symbol s = Hashtbl.add table (Symbol.name grammar ~mangled:false s) s in
    Index.iter (Symbol.cardinal grammar) add_symbol;
    fun pos sym ->
      let sym = string_of_symbol sym in
      match Hashtbl.find_opt table sym with
      | Some sym -> sym
      | None -> error pos "unknown symbol %S" sym

  (** Match the right-hand side of a filter to an item *)
  let match_rhs rfilter item =
    let (prod, dot) = Item.desc grammar item in
    let found = ref false in
    let rec loop rhs index = function
      | [] ->
        (index = Array.length rhs) && !found
      | (Dot, _) :: rest ->
        if dot = index then
          found := true;
        if dot < index && not !found
        then false
        else loop rhs index rest
      | (Find sym, pos) :: rest ->
        index < Array.length rhs && (
          match sym with
          | None -> true
          | Some sym ->
            Index.equal (parse_symbol pos sym) rhs.(index)
        ) &&
        loop rhs (index + 1) rest
      | (Skip, _) :: rest ->
        let rec skip_skip = function
          | (Skip, _) :: rest -> skip_skip rest
          | (Dot, _) :: rest when dot = -1 -> skip_skip rest
          | rest -> rest
        in
        let rec skip_sym index rest =
          (index <= Array.length rhs) &&
          (loop rhs index rest ||
           skip_sym (index + 1) rest)
        in
        skip_sym index (skip_skip rest)
    in
    loop (Production.rhs grammar prod) 0 rfilter

  (** Translate a filter to the set of matching LR(1) states *)
  let process_filter lhs rhs =
    let match_lr1 lr1 =
      let match_lhs item = function
        | None -> true
        | Some lhs ->
          Index.equal lhs (Production.lhs grammar (Item.production grammar item))
      in
      let match_item item =
        match_lhs item lhs && match_rhs rhs item
      in
      let match_closed prod = match_item (Item.make grammar prod 0) in
      IndexSet.exists match_item (Lr1.items grammar lr1) ||
      IndexSet.exists match_closed
        Unreductions.prods_by_lr1.:(lr1)
    in
    IndexSet.init_from_set (Lr1.cardinal grammar) match_lr1

  (** Translate an atom to the set of LR1 states it matches *)
  let process_atom =
    (* Index states by their incoming symbol *)
    let table = Vector.make (Symbol.cardinal grammar) IndexSet.empty in
    Index.iter (Lr1.cardinal grammar) (fun lr1 ->
        match Lr1.incoming grammar lr1 with
        | None -> ()
        | Some sym -> table.@(sym) <- IndexSet.add lr1);
    fun pos -> function
    | None ->
      (* A wildcard symbol matches all states *)
      Lr1.all grammar
    | Some sym ->
      (* A normal symbol matches states by their incoming symbol *)
      table.:(parse_symbol pos sym)

  (* The subset of patterns supported by MiniLRGrep can be summarized
     as a "sum of reduced products":
     - the sum is a disjunction of branches
     - the products are concatenations of atoms and filters, identified modulo
       reductions.

     The following code extracts this subset from the AST and reject the rest.
  *)

  (** Flatten a concatenation expression into a filter *)
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
          match Symbol.desc grammar (parse_symbol expr.position sym) with
          | T _ -> error expr.position "expecting a non-terminal"
          | N n -> Some n
      in
      refine_filter (process_filter lhs rhs) filter

  (** Flatten an alternatives expression into a list of branches *)
  let rec flatten_alternatives expr = match expr.desc with
    | Atom (Some _, _, _) | Reduce {capture = Some _; _} ->
      error expr.position "variable bindings are not supported"
    | Repetition _ ->
      error expr.position "repetitions are not supported"
    | Alternative exprs ->
      List.concat_map flatten_alternatives exprs
    | Reduce {expr = {desc = Concat ({desc = Repetition {expr = {desc = Atom (_, None, _); _}; _}; _} :: rest); _}; _} ->
      let filter = List.fold_left flatten_concat (default_filter expr.position) rest in
      [{filter; reduce = Reduce_auto; position = expr.position}]
    | Reduce {expr; _} ->
      let filter = flatten_concat (default_filter expr.position) expr in
      [{filter; reduce = Reduce_yes; position = expr.position}]
    | _ ->
      let filter = flatten_concat (default_filter expr.position) expr in
      [{filter; reduce = Reduce_no; position = expr.position}]

  (** Extract branches from an entry in the specification *)
  let extract_clauses (rule : rule) =
    if not (fst rule.error) then
      error (snd rule.error) "only entries matching errors are supported";
    let extract_pattern (pattern : pattern) =
      match pattern.lookaheads with
      | [] -> flatten_alternatives pattern.expr
      | (_, pos) :: _ -> error pos "lookahead constraints are not supported"
    in
    let extract_clause (clause : clause) =
      (List.hd clause.patterns).expr.position,
      List.concat_map extract_pattern clause.patterns
    in
    List.map extract_clause rule.clauses

  (** Compile a branch into a set of suffixes that match the branch *)
  let compile_branch branch =
    let rec match_stack = function
      | [], _ -> false
      | (top :: rest), Filter (states, _) ->
        IndexSet.mem top states &&
        begin match rest, branch.reduce with
          | [], _ | _, Reduce_auto -> true
          | _ -> false
        end
      | (top :: rest), Consume (filter, states, _) ->
        IndexSet.mem top states && match_stack (rest, filter)
    in
    match branch.filter, branch.reduce with
    | Consume (_, _, pos), Reduce_no ->
      error pos "atoms are not supported outside of reductions"
    | _ ->
      IndexSet.init_from_set Suffix.n
        (fun suffix ->
           let _, desc = Suffix.desc.:(suffix) in
           match_stack (desc.stack, branch.filter))

  (** Compile one entry *)
  let compile_entry entry =
    let clauses = extract_clauses entry in
    let compile_branch branch =
      let suffixes = compile_branch branch in
      if IndexSet.is_empty suffixes then
        warn branch.position "pattern is unreachable";
      (branch, suffixes)
    in
    let compile_clause (position, branches) =
      (position, List.map compile_branch branches)
    in
    Array.of_list (List.map compile_clause clauses)

  (** Clauses of the compiled entry *)
  module Clause = struct
    include Vector.Of_array(struct
        type a = position * compiled_branch list
        let array =
          match !entry, ast with
          | "", None -> [||]
          | entry, None ->
            error invalid_position
              "-entry %S has been provided, but no specification was given\n" entry
          | "", Some ast ->
            begin match ast.rules with
              | [] -> [||]
              | [x] -> compile_entry x
              | x :: xs ->
                warn (snd x.error)
                  "processing only the first entry (%S); pass `-entry <name>` to process another entry (%s)"
                  x.name (string_concat_map ", " (fun x -> x.name) xs);
                compile_entry x
            end
          | name, Some ast ->
            begin match List.find_opt (fun x -> x.name = name) ast.rules with
              | None ->
                error invalid_position
                  "unknown entry %S; pass `-entry <name>` to process another entry (%s)"
                  name (string_concat_map ", " (fun x -> x.name) ast.rules)
              | Some x -> compile_entry x
            end
      end)
    let n = Vector.length vector

    let position c = fst vector.:(c)
    let branches c = snd vector.:(c)
  end
end

(** Compute a graph representing inclusion between non-terminals.
    (with an edge A -> B if L(B) ⊆ L(A)).

    This is useful to find the appropriate level of generalization for
    reduce-filter patterns.
    For instance, in a grammar including arithmetic, we can have:
      L(Expr) ⊆ L(Term) ⊆ L(Factor) ⊆ {number}
    Reporting `expecting an expression` when expecting a Expr is too specific
    and won't capture situations expecting a Factor.
    Conversely, matching for a number is too general: maybe numbers occur in
    a different context where the error message would be inappropriate.
    The right level is probably Factor.
    This is a call that grammar authors have to make, but we can at least
    generate the graph representing the inclusions to help them decide.
*)
let () = match nt_inclusion with
  | None -> ()
  | Some fname ->
    (* Generate the inclusion graph as a relation ⊆ N × N *)
    let inclusions = Vector.make (Nonterminal.cardinal grammar) IndexSet.empty in
    (* Visit each production to populate the graph *)
    Index.iter (Production.cardinal grammar) begin fun prod ->
      let rhs = Production.rhs grammar prod in
      (* Count non-nullable symbols to find how the lhs relates to the rhs *)
      let non_nullable = ref 0 in
      for i = 0 to Array.length rhs - 1 do
        if non_nullable_symbol rhs.(i) then incr non_nullable
      done;
      let inclusion = match !non_nullable with
        | 0 ->
          (* If there are only nullable symbols, each symbol contributes to L(lhs) *)
          Array.fold_left (fun set sym ->
              match Symbol.desc grammar sym with
              | T _ -> assert false
              | N n -> IndexSet.add n set
            ) IndexSet.empty rhs
        | 1 ->
          (* If there is only one non-nullable symbol A, then L(A) ⊆ L(lhs) *)
          begin match Array.find_opt non_nullable_symbol rhs with
            | None -> assert false
            | Some sym ->
              match Symbol.desc grammar sym with
              | T _ -> IndexSet.empty (* It is a terminal, ignore it *)
              | N n -> IndexSet.singleton n
          end
        | _ -> IndexSet.empty (* No direct relation, it is more complex *)
      in
      inclusions.@(Production.lhs grammar prod) <- IndexSet.union inclusion
    end;
    (* Output the graph (to a file or to xdot) *)
    let oc = open_file_or_xdot fname in
    let p fmt = Printf.fprintf oc fmt in
    p "digraph G {\n";
    p " rankdir=LR;\n";
    p " node[shape=rectangle];\n";
    (* List non-terminals appearing on the right-side of the relation *)
    let on_rhs = Vector.fold_left IndexSet.union IndexSet.empty inclusions in
    Vector.iteri begin fun nt nts ->
      (* .. to prune the trivial parts of the graph
         (non-terminals not related to any other) *)
      if not (IndexSet.is_empty nts) || IndexSet.mem nt on_rhs then (
        p " st%d[label=%S];\n" (Index.to_int nt) (Nonterminal.to_string grammar nt);
        IndexSet.iter
          (fun nt' -> p " st%d -> st%d;\n" (Index.to_int nt) (Index.to_int nt'))
          nts
      )
    end inclusions;
    p "}\n";
    close_file_or_xdot oc fname

(** Analyze the automaton to answer two main questions:
    - Are there states not covered by any clause?
    - Are there clauses not reachable by any stack?

    Answers to the first question are used to suggest new patterns to improve
    coverage.
    Answering the second question helps us warn the author about useless
    clauses. We can then refine it: which clauses of higher priority
    (lower index) shadows the unreachable clauses? *)
module Analyze = struct
  (** The clauses potentially accepted by a DFA state. *)
  let accepts : (Reduction_DFA.n, Transl.Clause.n indexset) vector =
    let table = Vector.make Reduction_DFA.n IndexSet.empty in
    (* Accumulate clauses recognizing suffixes reached by states *)
    Vector.iteri begin fun clause (_position, branches) ->
      List.iter (fun (_branch, suffixes) ->
          IndexSet.iter (fun suf ->
              IndexSet.iter (fun dfa -> table.@(dfa) <- IndexSet.add clause)
                (Suffix.to_dfa suf)
            ) suffixes
        ) branches
    end Transl.Clause.vector;
    table

  module Clause_or_uncovered = struct
    include Sum.Make(Transl.Clause)(Unit)
    let of_clause = inj_l
    let uncovered = inj_r Unit.element

    let prj c = match prj c with
      | L c -> Some c
      | R _ -> None
  end
  type clause_or_uncovered = Clause_or_uncovered.n

  (** The clause actually accepted by a DFA state.
      Earlier clauses (with lower indices) have the priority, so the minimal
      clause is the one accepted. *)
  let accepting dfa = IndexSet.minimum accepts.:(dfa)

  (** Determine the highest clause reachable by a state depending on the paths
      reaching it, by refining an approximation each time a new path is
      discovered.

      It is the min(accepting, max(incoming paths)), that is the min of the
      clauses accepted by the state and the max of the incoming paths.
  *)
  let refine_reachable_clause _path path_clause state approx =
    if Index.compare path_clause approx <= 0 then
      (* Our approximation tells us that a higher clause is already reachable
         from this state, keep it. *)
      approx
    else (* A higher-clause was discovered to be reachable *)
      match accepting state with
      | None -> path_clause (* Not an accepting state: just propagate *)
      | Some c -> Index.minimum (Clause_or_uncovered.of_clause c) path_clause (* Cannot reach higher than [c] *)

  (** Compute the highest clause accepted by a state reachable from a DFA state.

      If state [s] accepts no clauses, then a clause is reachable if it is
      reachable from a successor of [s].
      If it is accepting clause [c], then a clause [c'] is reachable from [s]:
      - if [c' < c] and it is reachable from a successor, or
      - if [c' = c] and at least one successor reaches a clause [c'' >= c']

      To compute this, we start assuming all clauses but the leaves
      reaches only the lowest clause, and back-propagates from the leaves
      until reaching a fixed-point. *)
  let highest_reachable : (Reduction_DFA.n, clause_or_uncovered index) vector =
    (* Initialize the table, starting from leaves *)
    let init_leaves st =
      if Reduction_DFA.is_leaf st then
        match accepting st with
        | None -> Clause_or_uncovered.uncovered
        | Some c -> Clause_or_uncovered.of_clause c
      else Index.of_int Clause_or_uncovered.n 0
    in
    let table = Vector.init Reduction_DFA.n init_leaves in
    Misc.fix_relation Reduction_DFA.predecessors table
      ~propagate:refine_reachable_clause;
    table

  (** Dual analysis: compute the highest clause accepted by paths reaching a
      state from the initial state. *)
  let highest_reached : (Reduction_DFA.n, clause_or_uncovered index) vector =
    let table = Vector.make Reduction_DFA.n (Index.of_int Clause_or_uncovered.n 0) in
    table.:(Reduction_DFA.initial) <- Clause_or_uncovered.uncovered;
    Misc.fix_relation Reduction_DFA.successors table
      ~propagate:refine_reachable_clause;
    table

  (** The highest clause accepted on a path passing by a state is the minimum
      accepted on a path reaching the state and a path leaving the state. *)
  let highest_accepted st =
    Index.minimum highest_reached.:(st) highest_reachable.:(st)

  (** A state is uncovered if [highest_accepted = uncovered]: there is at least
      one path from the initial state to a leaf passing by this state not
      accepting any clause. *)
  let is_uncovered st = Index.equal Clause_or_uncovered.uncovered (highest_accepted st )

  (** Find reachable clauses: those accepted by a state reachable by a path
      accepting no clause of lower priority. *)
  let is_clause_reachable =
    let table = Boolvector.make Transl.Clause.n false in
    Index.iter Reduction_DFA.n begin fun st ->
      match accepting st with
      | Some c ->
        if Index.equal (Clause_or_uncovered.of_clause c) (highest_accepted st) then
          Boolvector.set table c
      | _ -> ()
    end;
    Boolvector.test table
end

(** Computes which clauses are shadowed by clauses with higher priority.
    This is used to help the author of a specification understands why a certain
    clause is unreachable ([not (Analysis.is_clause_reachable c)]).

    The exact answer is difficult to compute and to interpret, because the
    actual shadowing relation is almost as complex as the transition relation
    of the DFA.

    Instead, we print only a few of the occluders of each clause.  The author
    can then adjust them and, by repeating, the analysis converge toward a
    satisfying solution.
*)
module Shadowing() = struct
  (** Create a vector to store some of the occluders for each clause. *)
  let occluders = Vector.make Transl.Clause.n IndexSet.empty

  (** Find occluders visible from a specific state. *)
  let process_state st =
    (* Get the clauses accepted by the current DFA state. *)
    let accepts = Analyze.accepts.:(st) in
    (* Determine the highest clause accepted by stacks going through the current
       state. *)
    let cs =
      match Analyze.Clause_or_uncovered.prj (Analyze.highest_accepted st) with
      | None -> IndexSet.empty (* No clause accepted *)
      | Some c -> IndexSet.singleton c (* Single clause accepted *)
    in
    (* Iterate over each clause accepted by the state. *)
    IndexSet.iter begin fun c' ->
      (* Check if the clause is not reachable. *)
      if not (Analyze.is_clause_reachable c') then
        (* Split the accepted clauses: occluders can only be clauses with
           lower indices. *)
        let l, _, _ = IndexSet.split c' accepts in
        occluders.@(c') <- IndexSet.union (IndexSet.union cs l)
    end accepts

  (** Iterate over all states to populate [occluders] *)
  let () = Index.iter Reduction_DFA.n process_state

  (** Print the results of the analysis by iterating over each clause *)
  let () = Vector.iteri begin fun c cs ->
      (* Check if the clause has any shadowers. *)
      if not (IndexSet.is_empty cs) then (
        (* Function to print the clause position. *)
        let print_clause c =
          Printf.sprintf "line %d" (Transl.Clause.position c).line
        in
        (* Print the unreachable clause and its shadowers. *)
        Printf.eprintf "Clause %s is unreachable. It is shadowed at least by clauses %s.\n"
          (print_clause c)
          (string_concat_map ", " print_clause (IndexSet.elements cs))
      )
    end occluders
end

(** Only proceed with the shadowing analysis if the [clause_shadow] flag is set. *)
let () = if clause_shadow then let module _ = Shadowing() in ()

(** Module to generate a sentence from an LR(1) stack.

    This is done by pre-computing the shortest inputs that can cause each
    transition to be taken.

    This is an approximation that ignores lookahead constraints: some generated
    sentences might be slightly invalid. See [Reachability] for a complete
    alternative. *)
module Sentence = struct

  (** The code work by reversing the effect of a reduction, replacing a goto
      transition by the stack suffix that would permit reducing it. *)
  type unreduce = {
    goto: g goto_transition index;     (** This [goto] transition... *)
    path: g transition index list; (** is reachable by reducing a stack suffixed by [path]. *)
    mutable unknowns: int;           (** Remaining number of transitions with unknown cost. *)
    mutable cost: int;               (** Cost of the known parts of the path. *)
  }

  (* The actual cost of an "unreduction" is [cost] + v[0] ... v[unknowns-1].
     If [unknowns] is 0, [cost] is the minimal cost.
     When running, the solver decreases [unknowns] until reaching a fixed-point.
     If [unknowns] is not 0 at the end of the run, the path has an infinite cost
     (there is no input that permit to takes it). This happens because
     conflicts resolution made some parts of the automaton unreachable. *)

  (** A heap data structure for managing transitions to be processed.

      The algorithm used to solve this problem is the application of Dijkstra's
      shortest paths algorithm to and-or graphs, described by Knuth in
      "A Generalization of Dijkstra’s Algorithm." *)
  module Heap = Heap.Make(struct
      type _ t = int
      let compare = Int.compare
    end)

  (** Vector to store the cost of reaching each goto transition.
      It is initialized with [max_int], representing an infinite cost. *)
  let transition_cost = Vector.make (Transition.goto grammar) max_int

  (** Vector to store the dependencies for each goto transition. *)
  let transition_deps = Vector.make (Transition.goto grammar) []

  (** Heap to manage transitions to be processed. *)
  let todo : (unit, g goto_transition index) Heap.t ref = ref Heap.empty

  (** Schedule an unreduce record for processing. *)
  let schedule {path = _; unknowns; goto; cost} =
    assert (unknowns = 0);
    let cost' = transition_cost.:(goto) in
    if cost < cost' then (
      transition_cost.:(goto) <- cost;
      todo := Heap.insert cost goto !todo
    )

  (** Compute the "unreduces" for each goto transition, initializing the minimal
      cost, the number of unknowns and registering the dependencies. *)
  let transition_reds = Vector.mapi begin fun goto paths ->
      List.map begin fun path ->
        let (unknowns, cost) = List.fold_left (fun (unknowns, cost) tr ->
            match Transition.split grammar tr with
            | R _sh -> (unknowns, cost + 1)
            | L _gt -> (unknowns + 1, cost)
          ) (0, 0) path
        in
        let unreduce = { path; goto; unknowns; cost } in
        if unknowns = 0 then schedule unreduce else
          List.iter (fun tr ->
              match Transition.split grammar tr with
              | R _  -> ()
              | L gt -> transition_deps.@(gt) <- List.cons unreduce
            ) path;
        unreduce
      end paths
    end Unreductions.paths

  let () =
    (* Relax the dependencies after solving the cost of a goto transition. *)
    let relax cost goto =
      List.iter (fun unreduce ->
          assert (unreduce.unknowns > 0);
          unreduce.unknowns <- unreduce.unknowns - 1;
          unreduce.cost <- unreduce.cost + cost;
          if unreduce.unknowns = 0 then
            (* All unknowns has been solved, this determined a new path reaching a
               goto transition. Propagate further. *)
            schedule unreduce;
        ) transition_deps.:(goto)
    in
    (* Main loop function to process the heap of transitions. *)
    let rec loop () =
      match Heap.pop !todo with
      | None -> ()
      | Some (k, v, heap') ->
        todo := heap';
        let k' = transition_cost.:(v) in
        if k = k' then (
          (* A cheaper path has been found.
             By construction, Dijkstra's algorithm guarantee that the lowest
             cost is found the first time a transition is visited. Therefore
             [k'] must be [max_int], its initial value, otherwise something went
             wrong. *)
          relax k v
        );
        loop ()
    in
    loop ()

  (** End timing the sentence generation process. *)
  let () = stopwatch 1 "Precomputed sentence generation information"

  (** Print the number of loops performed and the cost of reaching each goto transition. *)
  let () =
    Vector.iteri (fun goto reds ->
        if transition_cost.:(goto) = max_int then
          let tr = Transition.of_goto grammar goto in
          Printf.eprintf "Failed to synthesize %s -> %s, paths:\n"
            (Lr1.to_string grammar (Transition.source grammar tr))
            (Lr1.to_string grammar (Transition.target grammar tr));
          List.iter (fun unr ->
              Printf.eprintf "- %s\n"
                (Misc.string_concat_map " " (fun tr ->
                     let lr1 = Lr1.to_string grammar (Transition.target grammar tr) in
                     let cost =
                       match Transition.split grammar tr with
                       | R _ -> 1
                       | L g -> transition_cost.:(g)
                     in
                     lr1 ^ "(" ^ string_of_int cost ^ ")"
                   ) unr.path);
              Printf.eprintf " cost:%d unknowns:%d\n" unr.cost unr.unknowns
            ) reds
      ) transition_reds
end

(* Now we switch to enumeration support.

   Again, this implementation is an over-approximation
   because we do not account for precise reachability information
   (chapter 7).
   This is acceptable because:
   - This is correct for automata that have not went through conflict
     resolution
   - In other cases, the over-approximation means that some cases found by
     enumeration are not reachable in practice (it is possible to produce a
     sentential form for which there exists no sentence that can actually put
     an LR automaton in a configuration with this sentential form on the stack)
   It is not a big deal in this example as actual problems are quite rare and
   the enumeration is intended to guide grammar authors writing error messages.
   This is more problematic when using enumeration for fuzzing purposes (where
   the output is fed to another program). The full LRgrep implementation handles
   this.
*)

(** Enumerate uncovered patterns support *)
module Enum = struct

  (** Produce reduce-filter patterns for each leaf *)
  let maximal_patterns =
    let irreducible_item item =
      let p, i = Item.desc grammar item in
      let rhs = Production.rhs grammar p in
      i < Array.length rhs &&
      non_nullable_symbol rhs.(i)
    in
    let pattern_from_stack stack =
      let filter =
        IndexSet.filter irreducible_item (Lr1.items grammar (List.hd stack))
      in
      let max_dot =
        IndexSet.fold (fun item max -> Int.max max (Item.position grammar item)) filter 0
      in
      let filter = IndexSet.filter (fun item ->
          let (prod, dot) = Item.desc grammar item in
          dot = max_dot && (
            max_dot <> 1 ||
            not (Index.equal
                   (Symbol.inj_n grammar (Production.lhs grammar prod))
                   (Production.rhs grammar prod).(0))
          )
        ) filter in
      let reduce = List.filter_map (Lr1.incoming grammar) (List.tl (List.rev stack)) in
      (reduce, filter)
    in
    let table = Hashtbl.create 7 in
    let visit_suffix i =
      match Suffix.desc.:(i) with
      | _, {child = _ :: _; _} -> ()
      | _, {stack; _} ->
        let reduce, filter = pattern_from_stack stack in
        if not (IndexSet.is_empty filter) then
          match Hashtbl.find_opt table reduce with
          | None -> Hashtbl.add table reduce (ref [i, filter])
          | Some r -> r := (i, filter) :: !r
    in
    Vector.iteri (fun st suffixes ->
        if Reduction_DFA.is_leaf st && Analyze.is_uncovered st then
          IndexSet.iter visit_suffix suffixes)
      Suffix.of_dfa;
    Hashtbl.fold (fun reduce filters acc -> (reduce, !filters) :: acc) table []

  let totally_uncovered_lookaheads = ref IndexSet.empty
  let partially_uncovered_lookaheads = ref IndexSet.empty

  let generalized_patterns =
    let minimum_filters filters =
      let rec keep_minima acc = function
        | [] -> List.rev acc
        | (i, xs) :: xxs ->
          if List.exists
              (fun (_, xs') -> List.for_all (fun x' -> List.mem x' xs) xs')
              acc
          then keep_minima acc xxs
          else keep_minima ((i, xs) :: acc) xxs
      in
      filters
      |> List.map (fun (p,its) -> (p, IndexSet.elements its))
      |> List.sort (fun (_, l1) (_, l2) -> List.compare_lengths l1 l2)
      |> keep_minima []
    in
    let add_lookahead acc item =
      let (prod, pos) = Item.desc grammar item in
      let rhs = Production.rhs grammar prod in
      if pos < Array.length rhs then
        IndexSet.add rhs.(pos) acc
      else acc
    in
    let grow_set r s = r := IndexSet.union s !r in
    let patterns =
      maximal_patterns
      |> List.concat_map (fun (reduce, filters) ->
          filters
          |> minimum_filters
          |> List.map (fun (suffix, filter) ->
              let lookaheads =
                List.fold_left add_lookahead IndexSet.empty filter
              in
              if IndexSet.is_singleton lookaheads
              then grow_set totally_uncovered_lookaheads lookaheads
              else grow_set partially_uncovered_lookaheads lookaheads;
              (suffix, reduce, filter, lookaheads)
            )
        )
      |> List.sort (fun (_, _, _, l1) (_, _, _, l2) ->
          IndexSet.compare l1 l2
        )
      |> List.map (fun (s, r, f, _l) -> (s, r, f))
    in
    patterns

  let totally_uncovered_lookaheads   = !totally_uncovered_lookaheads
  let partially_uncovered_lookaheads = !partially_uncovered_lookaheads

  let () =
    Printf.eprintf "Uncovered expected symbols:\n";
    List.iteri (fun i sym ->
        Printf.eprintf "%s%s"
          (if i = 0 then "" else ", ")
          (Symbol.name grammar sym)
      ) (List.rev (IndexSet.elements totally_uncovered_lookaheads));
    Printf.eprintf "\n";
    Printf.eprintf "Partially uncovered expected symbols:\n";
    List.iteri (fun i sym ->
        Printf.eprintf "%s%s"
          (if i = 0 then "" else ", ")
          (Symbol.name grammar sym)
      ) (List.rev (IndexSet.elements partially_uncovered_lookaheads));
    Printf.eprintf "\n"

  let () =
    Printf.eprintf "%d patterns to cover:\n" (List.length generalized_patterns);
    List.iter (fun (suffix, reduce, filter) ->
        begin match reduce with
        | [] ->
          List.iteri (fun i item ->
              Printf.eprintf "%c /%s\n"
                (if i = 0 then '|' else ' ')
                (Item.to_string grammar item)
            ) filter
        | _ ->
          let reduce = Misc.string_concat_map "; " (Symbol.name grammar) reduce in
          let padding = String.make (String.length reduce) ' ' in
          List.iteri (fun i item ->
              if i = 0 then
                Printf.eprintf "| [%s /%s" reduce (Item.to_string grammar item)
              else
                Printf.eprintf "\n   %s /%s" padding (Item.to_string grammar item)
            ) filter;
          Printf.eprintf "]\n"
        end;
        let dfa, _ = Suffix.desc.:(suffix) in
        let stack = Reduction_DFA.stack_visiting dfa in
        let print_lr1_seq path =
          let symbols = List.filter_map (Lr1.incoming grammar) path in
          let symbols = List.filter non_nullable_symbol symbols in
          Misc.string_concat_map " " (Symbol.name grammar) symbols
        in
        Printf.eprintf "  (* %s *)\n  { failwith \"TODO\" }\n\n" (print_lr1_seq stack)
      ) generalized_patterns
end
