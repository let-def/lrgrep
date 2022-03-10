(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Utils
module IndexSet = BitSet.IndexSet
type 'a indexset = 'a IndexSet.t
type ('n, 'a) indexmap = ('n, 'a) IndexMap.t

(* The lexer generator. Command-line parsing. *)

let source_name = ref None
let output_name = ref None
let grammar_file = ref None
let interpret = ref false

let usage = "usage: menhirlex [options] sourcefile"

let print_version_num () =
  print_endline "0.1";
  exit 0

let print_version_string () =
  print_string "The Menhir parser lexer generator :-], version ";
  print_version_num ()

let specs = [
  "-o", Arg.String (fun x -> output_name := Some x),
  " <file.ml>  Set output file name to <file> (defaults to <source>.ml)";
  "-g", Arg.String (fun x -> grammar_file := Some x),
  " <file.cmly>  Path of the Menhir compiled grammar to analyse (*.cmly)";
  "-q", Arg.Set Common.quiet_mode,
  " Do not display informational messages";
  "-i", Arg.Set interpret,
  " Start an interpreter to test sentences (do not produce other output)";
  "-n", Arg.Set Common.dry_run,
  " Process input but do not generate any file";
  "-d", Arg.Set Common.dump_parsetree,
  " Dump parsetree";
  "-v",  Arg.Unit print_version_string,
  " Print version and exit";
  "-version",  Arg.Unit print_version_string,
  " Print version and exit";
  "-vnum",  Arg.Unit print_version_num,
  " Print version number and exit";
]

let () = Arg.parse specs (fun name -> source_name := Some name) usage

module Input = MenhirSdk.Cmly_read.Read (struct
    let filename = match !grammar_file with
      | Some filename -> filename
      | None ->
        Format.eprintf "No grammar provided (-g), stopping now.\n";
        Arg.usage specs usage;
        exit 1
  end)

let source_file = match !source_name with
  | None ->
    Format.eprintf "No specification, stopping now.\n";
    Arg.usage specs usage;
    exit 1
  | Some name -> name

let print_parse_error_and_exit lexbuf exn =
  let bt = Printexc.get_raw_backtrace () in
  begin match exn with
    | Parser.Error ->
      let p = Lexing.lexeme_start_p lexbuf in
      Printf.fprintf stderr
        "File \"%s\", line %d, character %d: syntax error.\n"
        p.Lexing.pos_fname p.Lexing.pos_lnum
        (p.Lexing.pos_cnum - p.Lexing.pos_bol)
    | Lexer.Lexical_error {msg; file; line; col} ->
      Printf.fprintf stderr
        "File \"%s\", line %d, character %d: %s.\n"
        file line col msg
    | _ -> Printexc.raise_with_backtrace exn bt
  end;
  exit 3

let lexer_definition =
  let ic = open_in_bin source_file in
  Lexer.ic := Some ic;
  let lexbuf = Lexing.from_channel ~with_positions:true ic in
  Lexing.set_filename lexbuf source_file;
  let result =
    try Parser.lexer_definition Lexer.main lexbuf
    with exn -> print_parse_error_and_exit lexbuf exn
  in
  Lexer.ic := None;
  result

open Fix.Indexing
open Input

module Transition_tree =
struct
  type ('n, 'lr1) map = ('n, 'lr1 indexset) indexmap

  type ('n, 'lr1) entry = {
    state: 'n index;
    covered: int;
    coverage: 'lr1 indexset;
    transitions: ('n, 'lr1) map;
  }

  type ('n, 'lr1) tree = {
    entry: ('n, 'lr1) entry;
    cost: int;
    forest: ('n, 'lr1) forest;
  }

  and ('n, 'lr1) forest = ('n, 'lr1) tree list ref

  let transitions_per_target
      (type n lr1)
      (states : n cardinal)
      (transitions : n index -> lr1 indexset * (n, lr1) map * n index option)
    =
    let per_fallback = Vector.make states [] in
    let no_fallback = ref [] in
    Index.iter states (fun state ->
        let coverage, transitions, fallback = transitions state in
        let entry = {
          state;
          covered = IndexSet.cardinal coverage;
          coverage;
          transitions;
        } in
        match fallback with
        | None -> no_fallback := entry :: !no_fallback
        | Some fb -> Vector.set_cons per_fallback fb entry
      );
    (per_fallback, !no_fallback)

  let initial_cost entry = entry.covered

  let size_of_diff e1 e2 =
    (IndexSet.cardinal (IndexSet.diff e1 e2))

  let cost_of entry ~wrt =
    IndexMap.fold
      (fun target targets sum ->
         sum +
         match IndexMap.find_opt target wrt.transitions with
         | None -> IndexSet.cardinal targets
         | Some targets' -> size_of_diff targets targets'
      )
      entry.transitions
      (size_of_diff wrt.coverage entry.coverage)

  let rec evaluate_solution entry depth current candidate =
    let cost = cost_of entry ~wrt:candidate.entry in
    let current =
      let (best_cost, best_depth, _) = current in
      if cost < best_cost || (cost = best_cost && depth < best_depth) then
        (cost, depth, candidate.forest)
      else
        current
    in
    evaluate_forest entry depth current candidate.forest

  and evaluate_forest entry depth current forest =
    List.fold_left
      (evaluate_solution entry (depth + 1))
      current !forest

  let process set =
    let set = List.sort (fun e1 e2 -> Int.compare e1.covered e2.covered) set in
    let forest = ref [] in
    List.iter (fun entry ->
        let initial = (initial_cost entry, 0, forest) in
        let (cost, _, best_forest) = evaluate_forest entry 0 initial forest in
        best_forest := {entry; cost; forest = ref []} :: !best_forest
      ) set;
    !forest

  let build_tree states transitions =
    let fallbacks, no_fallback = transitions_per_target states transitions in
    let roots = ref [] in
    let process_set set = roots := process set @ !roots in
    Index.iter states (fun state -> process_set (Vector.get fallbacks state));
    process_set no_fallback;
    !roots
end

let array_set_add arr index value =
  arr.(index) <- IndexSet.add value arr.(index)

let array_cons arr index value =
  arr.(index) <- value :: arr.(index)

let vector_iter v f =
  Index.iter (Vector.length v) (fun i -> f (Vector.get v i ))

let compare_index =
  (Int.compare : int -> int -> int :> _ index -> _ index -> int)

let string_of_index =
  (string_of_int : int -> string :> _ index -> string)

let cmon_index =
  (Cmon.int : int -> Cmon.t :> _ index -> Cmon.t)

let cmon_indexset xs =
  Cmon.constant (
    "[" ^ String.concat ";" (List.map string_of_index (IndexSet.elements xs)) ^ "]"
  )

let rec merge_uniq cmp l1 l2 =
  match l1, l2 with
  | [], l2 -> l2
  | l1, [] -> l1
  | h1 :: t1, h2 :: t2 ->
    let c = cmp h1 h2 in
    if c = 0
    then h1 :: merge_uniq cmp t1 t2
    else if c < 0
    then h1 :: merge_uniq cmp t1 l2
    else h2 :: merge_uniq cmp l1 t2

module TerminalSet = BitSet.Make(Terminal)

let all_terminals =
  let acc = ref TerminalSet.empty in
  for i = Terminal.count - 1 downto 0
  do acc := TerminalSet.add (Terminal.of_int i) !acc done;
  !acc

(* ---------------------------------------------------------------------- *)

(* [Lr1C] represents Lr1 states as elements of a [Numbering.Typed] set *)
module Lr1C = struct
  include (val const Lr1.count)
  type t = n index
  type set = n indexset
  let of_g lr1 = Index.of_int n (Lr1.to_int lr1)
  let to_g lr1 = Lr1.of_int (Index.to_int lr1)
  let to_lr0 lr1 = Lr1.lr0 (to_g lr1)
end

module Lr1Map = Map.Make(struct
    type t = Lr1C.t
    let compare = compare_index
  end)
module IndexRefine = Refine.Make(Utils.BitSet.IndexSet)

let all_states =
  let acc = ref IndexSet.empty in
  for i = (cardinal Lr1C.n) - 1 downto 0
  do acc := IndexSet.add (Index.of_int Lr1C.n i) !acc done;
  !acc

let indexset_bind : 'a indexset -> ('a index -> 'b indexset) -> 'b indexset =
  fun s f ->
  IndexSet.fold (fun lr1 acc -> IndexSet.union acc (f lr1)) s IndexSet.empty

module LRijkstra =
  LRijkstraFast.Make(Input)(TerminalSet)(Lr1C)
    (struct let all_terminals = all_terminals end)
    ()

(* State indices *)

module State_indices =
struct

  (* Precompute states associated to symbols *)

  let states_of_terminals =
    Array.make Terminal.count IndexSet.empty

  let states_of_nonterminals =
    Array.make Nonterminal.count IndexSet.empty

  let () =
    Index.iter Lr1C.n (fun lr1 ->
        match Lr0.incoming (Lr1C.to_lr0 lr1) with
        | None -> ()
        | Some (T t) -> array_set_add states_of_terminals (t :> int) lr1
        | Some (N n) -> array_set_add states_of_nonterminals (n :> int) lr1
      )

  let states_of_symbol = function
    | T t -> states_of_terminals.((t :> int))
    | N n -> states_of_nonterminals.((n :> int))

  (* Map symbol names to actual symbols *)

  let linearize_symbol =
    let buffer = Buffer.create 32 in
    function
    | Syntax.Name s -> s
    | sym ->
      Buffer.reset buffer;
      let rec aux = function
        | Syntax.Name s -> Buffer.add_string buffer s
        | Syntax.Apply (s, args) ->
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

  let find_symbol =
    let table = Hashtbl.create 7 in
    let add_symbol s = Hashtbl.add table (symbol_name ~mangled:false s) s in
    Terminal.iter (fun t -> add_symbol (T t));
    Nonterminal.iter (fun n -> add_symbol (N n));
    fun name -> Hashtbl.find_opt table (linearize_symbol name)
end

(* ---------------------------------------------------------------------- *)

(* Transitions are represented as finite sets with auxiliary functions
   to get the predecessors, successors and labels. *)
module Transition : sig
  (* Abstract types used as index to represent the different sets of
     transitions.
     For instance, [goto] represents the finite set of goto transition:
     - the value [goto : goto cardinal] is the cardinal of this set
     - any value of type [goto index] is a member of this set
       (representing a goto transition)
  *)
  type goto and shift and any

  (* The set of goto transitions *)
  val goto : goto cardinal
  (* The set of all transitions = goto U shift *)
  val any : any cardinal
  (* The set of shift transitions *)
  val shift : shift cardinal

  (* Building the isomorphism between any and goto U shift *)

  (* Inject goto into any *)
  val of_goto : goto index -> any index

  (* Inject shift into any *)
  val of_shift : shift index -> any index

  (* Project a transition into a goto or a shift transition *)
  val split : any index -> (goto index, shift index) either

  (* [find_goto s nt] finds the goto transition originating from [s] and
     labelled by [nt], or raise [Not_found].  *)
  val find_goto : Lr1C.t -> Nonterminal.t -> goto index

  (* Get the source state of a transition *)
  val source : any index -> Lr1C.t

  (* Get the target state of a transition *)
  val target : any index -> Lr1C.t

  (* Symbol that labels a transition *)
  val symbol : any index -> symbol

  (* Symbol that labels a goto transition *)
  val goto_symbol : goto index -> Nonterminal.t

  (* Symbol that labels a shift transition *)
  val shift_symbol : shift index -> Terminal.t

  (* [successors s] returns all the transitions [tr] such that
     [source tr = s] *)
  val successors : Lr1C.t -> any index list

  (* [predecessors s] returns all the transitions [tr] such that
     [target tr = s] *)
  val predecessors : Lr1C.t -> any index list
end =
struct

  (* Pre-compute all information, such that functions of this module
     always operate in O(1) *)

  (* Create two fresh finite sets that will be populated with goto and shift
     transitions *)
  module Goto = Gensym()
  module Shift = Gensym()

  let () =
    (* Count goto and shift transitions by iterating on all states and
       transitions *)
    Lr1.iter begin fun lr1 ->
      List.iter begin fun (sym, _) ->
        match sym with
        | T _t ->
          (*if Terminal.real t then*)
            ignore (Shift.fresh ())
        | N _ ->
          ignore (Goto.fresh ())
      end (Lr1.transitions lr1)
    end

  type goto = Goto.n
  let goto = Goto.n

  type shift = Shift.n
  let shift = Shift.n

  (* Any is the disjoint sum of goto and shift transitions *)
  module Any = (val sum goto shift)
  type any = Any.n
  let any = Any.n

  let of_goto = Any.inj_l
  let of_shift = Any.inj_r
  let split = Any.prj

  (* Vectors to store information on states and transitions.

     We allocate a bunch of data structures (sources, targets, t_symbols,
     nt_symbols and predecessors vectors, t_table and nt_table hash tables),
     and then populate them by iterating over all transitions.
  *)

  let sources = Vector.make' any (fun () -> Index.of_int Lr1C.n 0)
  let targets = Vector.make' any (fun () -> Index.of_int Lr1C.n 0)

  let t_symbols = Vector.make' shift (fun () -> Terminal.of_int 0)
  let nt_symbols = Vector.make' goto (fun () -> Nonterminal.of_int 0)

  (* Hash tables to associate information to the pair of
     a transition and a symbol.
  *)

  let nt_table = Hashtbl.create 7

  let nt_pack lr1 goto =
    (* Custom function to key into nt_table: compute a unique integer from
       an lr1 state and a non-terminal. *)
    Index.to_int lr1 * Nonterminal.count + Nonterminal.to_int goto

  let t_table = Hashtbl.create 7

  let t_pack lr1 t =
    (* Custom function to key into t_table: compute a unique integer from
       an lr1 state and a terminal. *)
    Index.to_int lr1 * Terminal.count + Terminal.to_int t

  (* A vector to store the predecessors of an lr1 state.
     We cannot compute them directly, we discover them by exploring the
     successor relation below. *)
  let predecessors = Vector.make Lr1C.n []

  let successors =
    (* We populate all the data structures allocated above, i.e.
       the vectors t_sources, t_symbols, t_targets, nt_sources, nt_symbols,
       nt_targets and predecessors, as well as the tables t_table and
       nt_table, by iterating over all successors. *)
    let next_goto = Index.enumerate goto in
    let next_shift = Index.enumerate shift in
    Vector.init Lr1C.n begin fun source ->
      List.fold_left begin fun acc (sym, target) ->
        match sym with
        (*| T t when not (Terminal.real t) ->
          (* Ignore pseudo-terminals *)
          acc*)
        | _ ->
          let target = Lr1C.of_g target in
          let index = match sym with
            | T t ->
              let index = next_shift () in
              Vector.set t_symbols index t;
              Hashtbl.add t_table (t_pack source t) index;
              of_shift index
            | N nt ->
              let index = next_goto () in
              Vector.set nt_symbols index nt;
              Hashtbl.add nt_table (nt_pack source nt) index;
              of_goto index
          in
          Vector.set sources index source;
          Vector.set targets index target;
          Vector.set_cons predecessors target index;
          index :: acc
      end [] (Lr1.transitions (Lr1C.to_g source))
    end

  let successors lr1 = Vector.get successors lr1
  let predecessors lr1 = Vector.get predecessors lr1

  let find_goto source nt = Hashtbl.find nt_table (nt_pack source nt)

  let source i = Vector.get sources i

  let symbol i =
    match split i with
    | L i -> N (Vector.get nt_symbols i)
    | R i -> T (Vector.get t_symbols i)

  let goto_symbol i = Vector.get nt_symbols i
  let shift_symbol i = Vector.get t_symbols i

  let target i = Vector.get targets i
end

let lr1_predecessors = Vector.init Lr1C.n (fun lr1 ->
    List.fold_left
      (fun acc tr -> IndexSet.add (Transition.source tr) acc)
      IndexSet.empty
      (Transition.predecessors lr1)
  )

let lr1set_predecessors lr1s =
  indexset_bind lr1s (Vector.get lr1_predecessors)

module Redgraph = struct
  let reductions = Vector.init Lr1C.n (fun lr1 ->
      let prepare_goto p =
        (Array.length (Production.rhs p), Production.lhs p)
      in
      let order (d1, n1) (d2, n2) =
        let c = Int.compare d1 d2 in
        if c = 0
        then Int.compare (n1 : nonterminal :> int) (n2 : nonterminal :> int)
        else c
      in
      let productions =
        Lr1.reductions (Lr1C.to_g lr1)
        |> List.map (fun (_, ps) -> prepare_goto (List.hd ps))
        |> List.sort_uniq order
      in
      let depth = List.fold_left (fun x (d, _) -> max x d) 0 productions in
      let vector = Array.make (depth + 1) [] in
      List.iter (fun (d,n) -> array_cons vector d n) productions;
      vector
    )

  type abstract_stack = {
    states: Lr1C.set;
    mutable goto: Lr1C.set ref Lr1Map.t;
    parent: abstract_parent;
  }

  and abstract_parent = abstract_stack option ref

  type concrete_stack = {
    prefix: Lr1C.t list;
    base: Lr1C.t;
    suffix: abstract_parent;
  }

  type stack =
    | Concrete of concrete_stack
    | Abstract of abstract_stack

  let get_parent states suffix =
    match !suffix with
    | Some parent -> parent
    | None ->
      let states = lr1set_predecessors states in
      let result = {states; goto = Lr1Map.empty; parent = ref None} in
      suffix := Some result;
      result

  let get_goto stack state =
    match Lr1Map.find_opt state stack.goto with
    | Some set -> set
    | None ->
      let set = ref IndexSet.empty in
      stack.goto <- Lr1Map.add state set stack.goto;
      set

  let pop = function
    | Concrete {prefix = _ :: prefix; base; suffix} ->
      Concrete {prefix; base; suffix}
    | Concrete {prefix = []; base; suffix} ->
      Abstract (get_parent (IndexSet.singleton base) suffix)
    | Abstract t ->
      Abstract (get_parent t.states t.parent)

  let goto_target lr1 nt =
    match Transition.(target (of_goto (find_goto lr1 nt))) with
    | exception Not_found -> None
    | result -> Some result

  let goto stack acc nts =
    match stack with
    | Concrete {prefix; base; suffix} ->
      let lr1 = match prefix with lr1 :: _ -> lr1 | [] -> base in
      List.iter begin fun nt ->
        match goto_target lr1 nt with
        | None -> ()
        | Some lr1 -> acc := {prefix = lr1 :: prefix; base; suffix} :: !acc
      end nts
    | Abstract t ->
      IndexSet.iter begin fun src ->
        match List.filter_map (goto_target src) nts with
        | [] -> ()
        | targets ->
          let set = get_goto t src in
          set := IndexSet.union (IndexSet.of_list targets) !set
      end t.states

  let follow_transitions stack =
    let lr1 = match stack.prefix with
      | lr1 :: _ -> lr1
      | [] -> stack.base
    in
    let reductions = Vector.get reductions lr1 in
    let acc = ref [] in
    let stack = ref (Concrete stack) in
    Array.iteri begin fun i nts ->
      if i <> 0 then stack := pop !stack;
      goto !stack acc nts;
    end reductions;
    !acc

  let rec close_transitions acc state =
    let new_transitions = follow_transitions state in
    let acc = new_transitions @ acc in
    List.fold_left close_transitions acc new_transitions

  module Closed_derivation = struct
    type node = {
      mutable children: node Lr1Map.t;
      mutable sources: Lr1C.set;
    }

    let fresh () = {children = Lr1Map.empty; sources = IndexSet.empty}

    let root_node = fresh ()

    let delta node lr1 =
      match Lr1Map.find_opt lr1 node.children with
      | Some node' -> node'
      | None ->
        let node' = fresh () in
        node.children <- Lr1Map.add lr1 node' node.children;
        node'

    let register source state =
      let node = List.fold_left delta root_node state.prefix in
      let node = delta node state.base in
      node.sources <- IndexSet.add source node.sources

    let derive ~root ~step ~join =
      let rmap = ref Lr1Map.empty in
      let register derivation source =
        match Lr1Map.find_opt source !rmap with
        | None -> rmap := Lr1Map.add source (ref derivation) !rmap
        | Some rderiv -> rderiv := join !rderiv derivation
      in
      let rec visit derivation node =
        IndexSet.iter (register derivation) node.sources;
        Lr1Map.iter (fun lr1 node' -> visit (step derivation lr1) node')
          node.children
      in
      visit root root_node;
      Lr1Map.map (!) !rmap
  end

  type goto_transition = {
    sources: Lr1C.set;
    targets: Lr1C.set;
    mutable targets_closure: Lr1C.set;
  }

  let equal_goto_transition a b =
    IndexSet.equal a.sources b.sources &&
    IndexSet.equal a.targets b.targets

  let equal_goto_transition_list l =
    List.equal equal_goto_transition l

  type node = {
    states: Lr1C.set;
    goto_transitions: goto_transition list;
  }

  type root = {
    stack: node array;
  }

  let reconstruct_root suffix =
    let nodes =
      let rec aux = function
        | None -> []
        | Some state -> state :: aux !(state.parent)
      in
      aux !suffix
    in
    let prepare_node state =
      let goto_transitions =
        Lr1Map.bindings state.goto
        |> List.map (fun (state, targets) -> (!targets, state))
        |> IndexRefine.annotated_partition
        |> List.map (fun (targets, sources) -> {
              sources = IndexSet.of_list sources;
              targets;
              targets_closure = IndexSet.empty;
            })
      in
      { states = state.states; goto_transitions; }
    in
    { stack = Array.of_list (List.map prepare_node nodes) }

  let roots = Vector.init Lr1C.n (fun lr1 ->
      let root = {prefix = []; base = lr1; suffix = ref None} in
      let vs = close_transitions [] root  in
      List.iter (Closed_derivation.register lr1) vs;
      reconstruct_root root.suffix
    )

  let reverse_deps = Vector.make Lr1C.n []

  let () =
    let register_dep node {sources; targets; _} =
      let origin = (node, sources) in
      IndexSet.iter
        (fun target -> Vector.set_cons reverse_deps target origin)
        targets
    in
    Index.iter Lr1C.n begin fun lr1 ->
      let root = Vector.get roots lr1 in
      Array.iteri (fun offset node ->
          List.iter (register_dep (lr1, offset)) node.goto_transitions;
        ) root.stack;
    end

  let goto_closure =
    let module T = struct
      type top_goto_closure = {
        mutable mark: bool;
        mutable transitions: goto_transition list;
      }
    end in
    let open T in
    let top_deps = Vector.map (fun deps ->
        List.filter_map (function
            | ((root, 0), _) -> Some root
            | ((_, _), _) -> None
          ) deps
      ) reverse_deps
    in
    let top_goto_closure = Vector.map (fun node ->
        { mark = false;
          transitions =
            match node.stack with
            | [||] -> []
            | stack -> stack.(0).goto_transitions
        }
      ) roots
    in
    let close_one acc t0 =
      IndexSet.fold begin fun target acc ->
        List.filter_map begin fun t' ->
          let sources = IndexSet.inter t0.sources t'.sources in
          if IndexSet.is_empty sources
          then None
          else Some {sources; targets = t'.targets; targets_closure = IndexSet.empty}
        end (Vector.get top_goto_closure target).transitions @ acc
      end t0.targets acc
    in
    let close_list transitions =
      List.fold_left close_one transitions transitions
    in
    let close lr1 =
      match (Vector.get roots lr1).stack with
      | [||] -> []
      | stack -> close_list stack.(0).goto_transitions
    in
    let todo = ref [] in
    let update lr1 =
      let top_goto = Vector.get top_goto_closure lr1 in
      top_goto.mark <- false;
      let transitions = close lr1 in
      if not (equal_goto_transition_list transitions top_goto.transitions)
      then (
        top_goto.transitions <- transitions;
        List.iter (fun dep ->
            let top_goto' = Vector.get top_goto_closure dep in
            if not top_goto'.mark then
              (top_goto'.mark <- true; todo := dep :: !todo)
          ) (Vector.get top_deps lr1)
      )
    in
    let rec loop () =
      match List.rev !todo with
      | [] -> ()
      | xs ->
        todo := [];
        List.iter update xs;
        loop ()
    in
    Index.iter Lr1C.n update;
    loop ();
    let final_list node =
      node.goto_transitions
      |> close_list
      |> List.map (fun {sources; targets; _} -> sources, targets)
      |> IndexRefine.annotated_partition
      |> List.map (fun (sources, targetss) ->
          {
            sources;
            targets = List.fold_left IndexSet.union IndexSet.empty targetss;
            targets_closure = IndexSet.empty;
          }
        )
    in
    Vector.map (fun root -> Array.map final_list root.stack) roots

  let reachable_goto_closure =
    (* Compute reachable goto closure *)
    let module X =
      Fix.Fix.ForType
        (struct type t = Lr1C.t * int end)
        (struct
          type property = Lr1C.set
          let bottom = IndexSet.empty
          let equal = IndexSet.equal
          let is_maximal _ = false
        end)
    in
    let equations (index, step) valuation =
      let len = Array.length (Vector.get roots index).stack in
      assert (step <= len);
      if step >= len then
        IndexSet.empty
      else
        List.fold_left (fun acc {targets; _} ->
            IndexSet.fold (fun target acc ->
                IndexSet.union (valuation (target, 0)) acc
              ) targets (IndexSet.union targets acc)
          )
          (valuation (index, step + 1))
          (Vector.get goto_closure index).(step)
    in
    let fix = X.lfp equations in
    Vector.init Lr1C.n (fun root ->
        Array.init
          (Array.length (Vector.get roots root).stack)
          (fun i -> fix (root, i))
      )

  let () =
    Index.iter Lr1C.n begin fun lr1 ->
      Array.iter begin fun transitions ->
        List.iter (fun tr ->
            tr.targets_closure <-
              IndexSet.union
                tr.targets
                (indexset_bind tr.targets (fun lr1 ->
                     match Vector.get reachable_goto_closure lr1 with
                     | [||] -> IndexSet.empty
                     | stack -> stack.(0)
                   ))
          ) transitions
      end (Vector.get goto_closure lr1)
    end
end

module Sigma : sig
  (** The set of states is represented either as positive occurrences (all
      states that are contained) or negative occurrences (all states that are
      not contained).

      This makes complement a cheap operation.  *)
  type t =
    | Pos of Lr1C.set
    | Neg of Lr1C.set

  val singleton : Lr1C.t -> t
  val is_singleton : t -> Lr1C.t option
  val to_lr1set : t -> Lr1C.set

  include Mulet.SIGMA with type t := t

  val union : t -> t -> t
  (** Compute union of two sets *)

  val intersect : t -> t -> bool
  (** Check if two sets intersect *)

  val mem : Lr1C.t -> t -> bool
  (** [mem lr1 t] checks if the state [lr1] is an element of a sigma set [t] *)

  val cmon : t -> Cmon.t

  val quick_subset : t -> t -> bool

  val annotated_partition : (t * 'a) list -> (t * 'a list) list
end = struct
  type t =
    | Pos of Lr1C.set
    | Neg of Lr1C.set

  let empty = Pos IndexSet.empty
  let full = Neg IndexSet.empty
  let compl = function Pos x -> Neg x | Neg x -> Pos x
  let is_empty = function Pos x -> IndexSet.is_empty x | Neg _ -> false
  let is_full = function Neg x -> IndexSet.is_empty x | Pos _ -> false

  let singleton lr1 = Pos (IndexSet.singleton lr1)

  let is_singleton = function
    | Neg _ -> None
    | Pos s ->
      if IndexSet.is_singleton s
      then Some (IndexSet.choose s)
      else None

  let to_lr1set = function
    | Pos xs -> xs
    | Neg xs -> IndexSet.diff all_states xs

  let is_subset_of x1 x2 =
    match x1, x2 with
    | Pos x1, Pos x2 -> IndexSet.subset x1 x2
    | Neg x1, Neg x2 -> IndexSet.subset x2 x1
    | Pos x1, Neg x2 -> IndexSet.disjoint x1 x2
    | Neg _ , Pos _ -> false

  let inter x1 x2 =
    match x1, x2 with
    | Pos x1, Pos x2 -> Pos (IndexSet.inter x1 x2)
    | Neg x1, Neg x2 -> Neg (IndexSet.union x1 x2)
    | (Pos x1, Neg x2) | (Neg x2, Pos x1) ->
      Pos (IndexSet.diff x1 x2)

  let intersect x1 x2 =
    match x1, x2 with
    | Pos x1, Pos x2 -> not (IndexSet.disjoint x1 x2)
    | Neg _, Neg _ -> true
    | Pos x1, Neg x2 | Neg x2, Pos x1 ->
      not (IndexSet.is_empty (IndexSet.diff x1 x2))

  let compare x1 x2 =
    match x1, x2 with
    | Pos x1, Pos x2 -> IndexSet.compare x1 x2
    | Neg x1, Neg x2 -> IndexSet.compare x2 x1
    | Pos _ , Neg _ -> -1
    | Neg _ , Pos _ -> 1

  let union x1 x2 =
    match x1, x2 with
    | Neg x1, Neg x2 -> Neg (IndexSet.inter x1 x2)
    | Pos x1, Pos x2 -> Pos (IndexSet.union x1 x2)
    | Pos x1, Neg x2 | Neg x2, Pos x1 ->
      Neg (IndexSet.diff x2 x1)

  let partition l =
    let only_pos = ref true in
    let project = function Pos x -> x | Neg x -> only_pos := false; x in
    let l = List.map project l in
    let pos x = Pos x in
    try
      if !only_pos
      then List.map pos (IndexRefine.partition l)
      else
        let parts, total = IndexRefine.partition_and_total l in
        Neg total :: List.map pos parts
    with exn ->
      Printf.eprintf
        "Partition failed with %d inputs (strictly positive: %b):\n"
        (List.length l) !only_pos;
      List.iter (fun set ->
          Printf.eprintf "- cardinal=%d, set={" (IndexSet.cardinal set);
          IndexSet.iter (fun elt -> Printf.eprintf "%d," (elt : _ index :> int)) set;
        ) l;
      raise exn

  let annotated_partition l =
    let negative = ref [] in
    let project (sg, tag) =
      match sg with
      | Pos x -> (x, `P tag)
      | Neg x ->
        let tag = (ref (-1), tag) in
        negative := tag :: !negative; (x, `N tag)
    in
    let l = List.map project l in
    let negative = !negative in
    try
      let parts, total = IndexRefine.annotated_partition_and_total l in
      let parts =
        List.mapi (fun i (part, tags) ->
            let tags =
              List.filter_map (function
                  | `P tag -> Some tag
                  | `N (r, _) -> r := i; None
                ) tags
            in
            let tags = List.fold_left
                (fun tags (r, tag) ->
                   if !r <> i then tag :: tags else tags)
                tags negative
            in
            Pos part, tags
          ) parts
      in
      match negative with
      | [] -> parts
      | neg -> (Neg total, List.map snd neg) :: parts
    with _ -> assert false

  let mem x = function
    | Pos xs -> IndexSet.mem x xs
    | Neg xs -> not (IndexSet.mem x xs)

  let cmon = function
    | Pos xs ->
      if IndexSet.is_empty xs
      then Cmon.constant "Empty"
      else Cmon.construct "Pos" [cmon_indexset xs]
    | Neg xs ->
      if IndexSet.is_empty xs
      then Cmon.constant "Full"
      else Cmon.construct "Neg" [cmon_indexset xs]

  let quick_subset sg1 sg2 =
    match sg1, sg2 with
    | Pos ls1, Pos ls2 -> IndexSet.quick_subset ls1 ls2
    | Neg ls1, Pos ls2 -> not (IndexSet.quick_subset ls1 ls2)
    | Pos ls1, Neg ls2 -> not (IndexSet.quick_subset ls2 ls1)
    | Neg ls1, Neg ls2 -> IndexSet.quick_subset ls2 ls1
end

module Clause = struct
  type capture = {
    id: int;
    for_clause: t;
    name: string;
    mutable var: int option;
  }

  and t = {
    priority: int;
    action: Syntax.ocaml_code option;
    mutable captures: capture list;
    mutable arity: int;
  }

  let alloc_capture c =
    match c.var with
    | Some _ -> ()
    | None ->
      c.var <- Some c.for_clause.arity;
      c.for_clause.arity <- c.for_clause.arity + 1

  let fresh_capture =
    let k = ref 0 in
    fun for_clause name ->
      let capture = {id = -(incr k; !k); for_clause; name; var = None} in
      for_clause.captures <- capture :: for_clause.captures;
      capture

  let compare_capture c1 c2 =
    Int.compare c1.id c2.id

  let make priority action =
    { priority; captures = []; arity = 0; action }

  let gencode clauses =
    let arities =
      List.mapi (fun i clause ->
          assert (clause.priority = i);
          string_of_int clause.arity)
        clauses
    in
    Printf.printf
      "let arities = [|%s|]" (String.concat ";" arities);
    Printf.printf
      "let execute : int * %s.MenhirInterpreter.element Analyser_def.Registers.t -> _ = function\n"
      (String.capitalize_ascii (Filename.basename Input.Grammar.basename));
    List.iter (fun clause ->
        let variables = Array.make clause.arity "" in
        List.iter (fun capture ->
            match capture.var with
            | None -> ()
            | Some i ->
              variables.(i) <- "Some " ^ capture.name
          ) clause.captures;
        let variables = String.concat "; " (Array.to_list variables) in
        Printf.printf " | %d, [|%s|] -> begin\n%s\nend\n"
          clause.priority
          variables
          (match clause.action with
           | None -> "failwith \"Should be unreachable\""
           | Some (_, str) -> str)
      ) clauses;
    Printf.printf
      "  | _ -> failwith \"Invalid action\""
end

module Label = struct
  module Action = struct
    type t =
      | Nothing
      | Action of Clause.t

    let compare : t -> t -> int = compare

    let append t1 t2 =
      match t1, t2 with
      | Nothing, x | x, Nothing -> x
      | Action {priority = p1; _}, Action {priority = p2; _} ->
        if p1 <= p2 then t1 else t2

    let priority = function
      | Nothing -> None
      | Action clause -> Some clause.priority
  end

  type t = {
    action: Action.t;
    captures: Clause.capture list;
  }

  let empty = {action = Nothing; captures = []}

  let is_empty = function
    | {action = Nothing; captures = []} -> true
    | _ -> false

  let compare t1 t2 =
    let c = Action.compare t1.action t2.action in
    if c = 0
    then List.compare Clause.compare_capture t1.captures t2.captures
    else c

  let append t1 t2 =
    match t1, t2 with
    | t, {action=Nothing; captures=[]} | {action=Nothing; captures=[]}, t -> t
    | _ -> {
        action = Action.append t1.action t2.action;
        captures = merge_uniq Clause.compare_capture t1.captures t2.captures
      }

  let make_action clause =
    { action = Action clause; captures=[] }

  let make_capture clause name =
    { action = Nothing; captures=[Clause.fresh_capture clause name] }
end

let do_log = ref false

module rec Reg : Mulet.S with type sigma = Sigma.t
                          and type label = Label.t
                          and type abstract = Redgraph_derivation.t
  = Mulet.Make(Sigma)(Label)(Redgraph_derivation)

and Redgraph_derivation : sig
  include Mulet.DERIVABLE
    with type sigma := Sigma.t
     and type label := Label.t

  type compiled
  val compile : Reg.Expr.t -> compiled

  val make : compiled -> Lr1C.set -> Reg.Expr.t

  val cmon : t -> Cmon.t
end =
struct
  type derivation =
    | Step of { re: Reg.Expr.t; mutable transitions: transition list }
    | Void

  and transition = {
    sigma: Sigma.t;
    mutable target: (Label.t * derivation) option;
  }

  let lift re =
    if Reg.Expr.is_empty re then
      Void
    else
      Step { re; transitions = [] }

  let unlift = function
    | Void -> Reg.Expr.empty
    | Step { re; _} -> re

  let get_transitions = function
    | Void -> []
    | Step d ->
      match d.transitions with
      | [] ->
        let mk_transition sigma = {sigma; target=None} in
        let result = List.map mk_transition (Reg.Expr.left_classes d.re) in
        d.transitions <- result;
        result
      | transitions -> transitions

  let derive d sg =
    match d with
    | Void -> Label.empty, Void
    | Step {re; _} ->
      match
        List.filter_map begin fun t ->
          if Sigma.is_subset_of sg t.sigma then (
            let d' = match t.target with
              | Some x -> x
              | None ->
                let lbl, re = Reg.Expr.left_delta re t.sigma in
                let x = lbl, lift re in
                t.target <- Some x;
                x
            in
            Some d'
          ) else None
        end (get_transitions d)
      with
      | [] -> Label.empty, Void
      | [d] -> d
      | ds ->
        prerr_endline "MULTIPLE DERIVATIONS, FIX ME";
        if true then exit 1;
        let lbl = ref Label.empty in
        let res = List.filter_map (fun (lbl', d) ->
            lbl := Label.append lbl' !lbl;
            match d with
            | Void -> None
            | Step { re; _} -> Some re
          ) ds
        in
        (!lbl, lift (Reg.Expr.disjunction res))

  let join_derivation d1 d2 = match d1, d2 with
    | Void, x | x, Void -> x
    | Step d1, Step d2 -> lift (Reg.Expr.(|.) d1.re d2.re)

  type compiled = {
    source: Reg.Expr.t;
    derivations: (Label.t * derivation) Lr1Map.t;
    domain: Lr1C.set;
  }

  let compare_compilation t1 t2 =
    Reg.Expr.compare t1.source t2.source

  let compile source =
    let source = Reg.Expr.(source ^. star (set Sigma.full)) in
    let root = Step { re = source; transitions = [] } in
    let closed_derivations =
      Redgraph.Closed_derivation.derive
        ~root
        ~step:(fun d lr1 -> snd (derive d (Sigma.singleton lr1)))
        ~join:join_derivation
    in
    let derivations = ref Lr1Map.empty in
    let domain = ref IndexSet.empty in
    let visit lr1 =
      let lbl, re = Reg.Expr.left_delta source (Sigma.singleton lr1) in
      let re = match Lr1Map.find_opt lr1 closed_derivations with
        | Some Void | None -> re
        | Some (Step d) -> Reg.Expr.(|.) re d.re
      in
      if not (Reg.Expr.is_empty re) then (
        derivations := Lr1Map.add lr1 (lbl, lift re) !derivations;
        domain := IndexSet.add lr1 !domain;
      )
    in
    Index.iter Lr1C.n visit;
    let derivations = !derivations in
    let domain = !domain in
    Printf.printf "%d non empty derivations\n%!"
      (IndexSet.cardinal domain);
    { source; derivations; domain }

  type t =
    | Node of { root: Lr1C.t; step: int; compiled: compiled; }
    | Root of { states: Lr1C.set; compiled: compiled }

  let compare t1 t2 =
    match t1, t2 with
    | Root t1, Root t2 ->
      let c = IndexSet.compare t1.states t2.states in
      if c <> 0 then c else
        compare_compilation t1.compiled t2.compiled
    | Node _, Root _ -> +1
    | Root _, Node _ -> -1
    | Node t1, Node t2 ->
      let c = compare_index t1.root t2.root in
      if c <> 0 then c else
        let c = Int.compare t1.step t2.step in
        if c <> 0 then c else
          compare_compilation t1.compiled t2.compiled

  let is_empty _ = false

  let nullable _ = false

  let get_label _ = Label.empty

  let make compiled states =
    Reg.Expr.abstract (Root { compiled; states })

  let fold_left_classes t f acc =
    match t with
    | Root t ->
      IndexSet.fold begin fun lr1 acc ->
        match Vector.get Redgraph.reachable_goto_closure lr1 with
        | [||] -> acc
        | stack ->
          if IndexSet.disjoint stack.(0) t.compiled.domain
          then acc
          else f (Sigma.singleton lr1) acc
      end t.states acc
    | Node t ->
      let root = Vector.get Redgraph.roots t.root in
      (*let cell = root.stack.(t.step) in*)
      let acc =
        (* If closure in domain *)
        if t.step < Array.length root.stack
        then f Sigma.full acc
        else acc
      in
      let visit acc {Redgraph. sources; targets; targets_closure} =
        if IndexSet.disjoint t.compiled.domain targets_closure then acc else (
          let acc = f (Sigma.Pos sources) acc in
          let acc = IndexSet.fold (fun lr1 acc ->
              match Lr1Map.find lr1 t.compiled.derivations with
              | exception Not_found -> acc
              | _, d ->
                List.fold_left
                  (fun acc tr -> f tr.sigma acc) acc (get_transitions d)
            ) targets acc
          in
          acc
        )
      in
      List.fold_left visit acc
        (Vector.get Redgraph.goto_closure t.root).(t.step)

  let empty_delta = (Label.empty, Reg.Expr.empty)

  let make_one compiled root step =
    let node = Vector.get Redgraph.roots root in
    if step >= Array.length node.stack ||
       IndexSet.disjoint compiled.domain
         (Vector.get Redgraph.reachable_goto_closure root).(step)
    then Reg.Expr.empty
    else Reg.Expr.abstract (Node { root; step; compiled })

  let cmon = function
    | Root _ -> Cmon.constant "Root"
    | Node {root; step; _} ->
      Cmon.crecord "Node" [
        "root", cmon_index root;
        (*"root_items",
          Cmon.constant (Format.asprintf "%a" Print.itemset
                         (Lr0.items (Lr1C.to_lr0 root)));*)
        "step", Cmon.int step;
      ]

  let cmon_re re =
    Mulet.cmon_re re
      ~set:Sigma.cmon ~label:(fun _ -> Cmon.unit)
      ~abstract:Redgraph_derivation.cmon

  let left_delta (t : t) sigma =
    match t with
    | Root t ->
      begin match Sigma.is_singleton sigma with
        | None -> empty_delta
        | Some lr1 ->
          let re = make_one t.compiled lr1 0 in
          match Lr1Map.find lr1 t.compiled.derivations with
          | exception Not_found -> Label.empty, re
          | lbl, d -> lbl, Reg.Expr.(|.) re (unlift d)
      end
    | Node t ->
      let res = ref [make_one t.compiled t.root (t.step + 1)] in
      let lbls = ref Label.empty in
      List.iter begin fun {Redgraph. sources; targets; _} ->
        if Sigma.is_subset_of sigma (Sigma.Pos sources) then
          IndexSet.iter begin fun lr1 ->
            if !do_log then Printf.eprintf "goto %d\n" (lr1 : _ index :> int);
            res := make_one t.compiled lr1 1 :: !res;
            match Lr1Map.find lr1 t.compiled.derivations with
            | exception Not_found ->
              if !do_log then
                Format.eprintf "no continuation\n%!";
            | _, d ->
              let lbl, d = derive d sigma in
              lbls := Label.append lbl !lbls;
              let re = unlift d in
              if !do_log then
                Format.eprintf "continuation @[%a@]\n%!" Cmon.format (cmon_re re);
              res := unlift d :: !res
          end targets
      end (Vector.get Redgraph.goto_closure t.root).(t.step);
      !lbls, Reg.Expr.disjunction !res

end

module Match_item = struct
  let maybe_has_lhs prod = function
    | None -> true
    | Some lhs -> lhs = Production.lhs prod

  let maybe_match_sym (sym, _, _) = function
    | None -> true
    | Some sym' -> sym = sym'

  let forall_i f l =
    match List.iteri (fun i x -> if not (f i x) then raise Exit) l with
    | () -> true
    | exception Exit -> false

  let item_match lhs (lp, prefix) (ls, suffix) (prod, pos) =
    maybe_has_lhs prod lhs &&
    pos >= lp &&
    let rhs = Production.rhs prod in
    Array.length rhs >= pos + ls &&
    forall_i (fun i sym -> maybe_match_sym rhs.(pos - i - 1) sym) prefix &&
    forall_i (fun i sym -> maybe_match_sym rhs.(pos + i) sym) suffix

  let states_by_items ~lhs ~prefix ~suffix =
    let prefix' = List.length prefix, List.rev prefix in
    let suffix' = List.length suffix, suffix in
    let result =
      Lr1.fold (fun lr1 acc ->
          if List.exists
              (item_match lhs prefix' suffix')
              (Lr0.items (Lr1.lr0 lr1))
          then IndexSet.add (Lr1C.of_g lr1) acc
          else acc
        ) IndexSet.empty
    in
    (*let lhs = match lhs with
      | None -> ""
      | Some nt -> Nonterminal.name nt ^ ": "
      in
      let sym_list l = String.concat " " (List.map (function
        | Some sym -> symbol_name sym
        | None -> "_"
      ) l)
      in
      Printf.eprintf "[%s%s . %s] = %d states\n"
      lhs (sym_list prefix) (sym_list suffix) (Set.cardinal result);*)
    result
end

let translate_symbol name =
  match State_indices.find_symbol name with
  | None ->
    prerr_endline
      ("Unknown symbol " ^ State_indices.linearize_symbol name);
    exit 1
  | Some symbol -> symbol

let translate_nonterminal sym =
  match translate_symbol sym with
  | N n -> n
  | T t ->
    Printf.eprintf "Expecting a non-terminal but %s is a terminal\n%!"
      (Terminal.name t);
    exit 1

let translate_producers list =
  List.map (Option.map translate_symbol) list

let reduce expr =
  let compiled = Redgraph_derivation.compile expr in
  let expr' = Redgraph_derivation.make compiled all_states in
  Reg.Expr.(expr |. expr')

let no_pos = {Syntax. line = -1; col = -1}

let translate_expr clause =
  let translate_capture term = function
    | None -> term
    | Some var ->
      Reg.Expr.(^.)
        (Reg.Expr.label (Label.make_capture clause var)) term
  in
  let rec translate_term = function
    | Syntax.Symbol (name, capture) ->
      let symbol = translate_symbol name in
      let states = State_indices.states_of_symbol symbol in
      translate_capture (Reg.Expr.set (Sigma.Pos states)) capture
    | Syntax.Item {lhs; prefix; suffix; capture} ->
      let lhs = Option.map translate_nonterminal lhs in
      let prefix = translate_producers prefix in
      let suffix = translate_producers suffix in
      let states = Match_item.states_by_items ~lhs ~prefix ~suffix in
      translate_capture (Reg.Expr.set (Sigma.Pos states)) capture
    | Syntax.Wildcard capture ->
      translate_capture (Reg.Expr.set Sigma.full) capture
    | Syntax.Alternative (e1, e2) ->
      Reg.Expr.(translate_expr e1 |. translate_expr e2)
    | Syntax.Repetition (e1, _) ->
      Reg.Expr.star (translate_expr e1)
    | Syntax.Reduce (expr, _) ->
      reduce (translate_expr expr)

  and translate_expr terms =
    let terms = List.rev_map (fun (term, _) -> translate_term term) terms in
    Reg.Expr.concatenation terms
  in
  translate_expr

let translate_clause priority {Syntax. pattern; action} =
  let clause = Clause.make priority action in
  let pattern, wrap =
    match pattern with
    | [Syntax.Reduce (re, _), _] -> re, Either.right
    | _ -> pattern, Either.left
  in
  let expr = translate_expr clause pattern in
  let label = Reg.Expr.label (Label.make_action clause) in
  clause, wrap (Reg.Expr.(^.) expr label)

let translate_entry {Syntax. startsymbols; error; name; args; clauses} =
  (* TODO *)
  ignore (startsymbols, error, name, args);
  let clauses, terms = List.split (List.mapi translate_clause clauses) in
  let terms, right = List.partition_map Fun.id terms in
  let terms = match right with
    | [] -> terms
    | terms' -> reduce (Reg.Expr.disjunction terms') :: terms
  in
  clauses, Reg.Expr.disjunction terms

let cmon_re re =
  Mulet.cmon_re re
    ~set:Sigma.cmon ~label:(fun _ -> Cmon.unit)
    ~abstract:Redgraph_derivation.cmon

(* Derive DFA with naive intersection *)

module DFA = struct
  let sigma_predecessors sg =
    Sigma.Pos (lr1set_predecessors (Sigma.to_lr1set sg))

  type 'set state = {
    expr: Reg.Expr.t;
    label: Label.t;
    mutable visited: Sigma.t;
    mutable scheduled: Sigma.t;
    mutable unvisited: Sigma.t list;
    mutable transitions: (Sigma.t * Clause.capture list * 'set index) list;
  }

  type large_dfa =
      Large_dfa : {
        set: (module CARDINAL with type n = 'set);
        states: ('set, 'set state) vector;
        dfa: ('set index * 'set state) Reg.Map.t;
        initial: 'set index * 'set state;
      } -> large_dfa

  let translate expr =
    let module States = IndexBuffer.Gen(struct type 'n t = 'n state end)() in
    let dfa = ref Reg.Map.empty in
    let todo = ref [] in
    let make_state expr =
      let state = {
        expr;
        label = Reg.Expr.get_label expr;
        unvisited = Reg.Expr.left_classes expr;
        visited = Sigma.empty;
        scheduled = Sigma.empty;
        transitions = [];
      } in
      let result = (States.add state, state) in
      dfa := Reg.Map.add expr result !dfa;
      result
    in
    let schedule (index, state) sigma =
      let unvisited = Sigma.inter sigma (Sigma.compl state.visited) in
      if not (Sigma.is_empty unvisited) then (
        if Sigma.is_empty state.scheduled then
          todo := index :: !todo;
        state.scheduled <- Sigma.union state.scheduled unvisited
      )
    in
    let update_transition sigma (sigma', _, state') =
      let inter = Sigma.inter sigma sigma' in
      if not (Sigma.is_empty inter) then
        schedule (state', States.get state') (sigma_predecessors inter)
    in
    let discover_transition sigma state sigma' =
      let inter = Sigma.inter sigma sigma' in
      (*Format.printf "discover %a\n%!"
        Cmon.format (Cmon.tuple [Sigma.cmon sigma; Sigma.cmon sigma']);*)
      if Sigma.is_empty inter
      then Either.Left sigma'
      else (
        let label, expr' = Reg.Expr.left_delta state.expr sigma' in
        let (index, _) as state' = match Reg.Map.find_opt expr' !dfa with
          | None -> make_state expr'
          | Some state' -> state'
        in
        schedule state' (sigma_predecessors inter);
        Either.Right (sigma', label.captures, index)
      )
    in
    let process index =
      let state = States.get index in
      let sigma = state.scheduled in
      state.scheduled <- Sigma.empty;
      state.visited <- Sigma.union state.visited sigma;
      List.iter (update_transition sigma) state.transitions;
      let unvisited, new_transitions =
        List.partition_map (discover_transition sigma state) state.unvisited
      in
      state.unvisited <- unvisited;
      state.transitions <- new_transitions @ state.transitions
    in
    let rec loop () =
      match List.rev !todo with
      | [] -> ()
      | todo' ->
        todo := [];
        List.iter process todo';
        loop ()
    in
    let initial = make_state expr in
    schedule initial Sigma.full;
    loop ();
    Large_dfa {
      set = (module States);
      states = States.freeze ();
      dfa = !dfa;
      initial;
    }

  let minimize (Large_dfa {set = (module States); states; dfa=_; initial}) =
    (* Removing unreachable actions *)
    let module Scc = Tarjan.Run(struct
        type node = States.n index
        let n = cardinal States.n
        let index = Index.to_int
        let successors f i =
          List.iter (fun (_sg, _, j) -> f j)
            (Vector.get states i).transitions
        let iter f = Index.iter States.n f
      end)
    in
    let reachable_action = Vector.make States.n BitSet.IntSet.empty in
    Scc.iter (fun _repr nodes ->
        List.iter (fun node ->
            List.iter
              (fun (_, cs, _) -> List.iter Clause.alloc_capture cs)
              (Vector.get states node).transitions
          ) nodes
      );
    Scc.rev_topological_iter (fun _repr nodes ->
        let get_min_action acc src =
          let state = Vector.get states src in
          List.fold_left
            (fun acc (_sg, _, tgt) ->
               BitSet.IntSet.union acc (Vector.get reachable_action tgt))
            (match Label.Action.priority state.label.action with
             | Some priority ->
               BitSet.IntSet.add priority acc
             | None -> acc
            )
            state.transitions
        in
        let set = List.fold_left get_min_action BitSet.IntSet.empty nodes in
        List.iter (fun node -> Vector.set reachable_action node set) nodes
      );
    (* Minimize automaton *)
    let module Class = struct
      type t = Lr1C.set * Lr1C.set * (Clause.capture list * Sigma.t) list

      let compare_capture_set (cs1,sg1) (cs2,sg2) =
        let c = List.compare Clause.compare_capture cs1 cs2 in
        if c <> 0 then c else
          Sigma.compare sg1 sg2

      let compare (p1,n1,c1) (p2,n2,c2) =
        let c = IndexSet.compare p1 p2 in
        if c <> 0 then c else
          let c = IndexSet.compare n1 n2 in
          if c <> 0 then c else
            List.compare compare_capture_set c1 c2

      let empty = IndexSet.(empty,empty,[])

      let add_transition (p,n,cs) (sg, c, _) =
        match c with
        | [] ->
          begin match sg with
            | Sigma.Pos p' -> (IndexSet.union p p', n, cs)
            | Sigma.Neg n' -> (p, IndexSet.union n n', cs)
          end
        | c ->
          (p, n, merge_uniq compare_capture_set [c, sg] cs)
    end in
    let module ClassMap = Map.Make(Class) in
    let class_of state =
      List.fold_left Class.add_transition Class.empty state.transitions
    in
    let classes = ref ClassMap.empty in
    Index.iter States.n (fun index ->
        let state = Vector.get states index in
        let clss = class_of state in
        match ClassMap.find_opt clss !classes with
        | None ->
          let states = IndexSet.singleton index in
          classes := ClassMap.add clss (ref states) !classes
        | Some states -> states := IndexSet.add index !states
      );
    let module Transitions = struct
      module Label = struct
        type t = Sigma.t * Clause.capture list
        let compare (sg1, cs1) (sg2, cs2) =
          let c = Sigma.compare sg1 sg2 in
          if c <> 0 then c else
            List.compare Clause.compare_capture cs1 cs2

        let partial_union (sg1, cs1) (sg2, cs2) =
          assert (List.compare Clause.compare_capture cs1 cs2 = 0);
          (Sigma.union sg1 sg2, cs1)
      end

      type t = {
        source: States.n index;
        label: Label.t;
        target: States.n index;
      }
      include IndexBuffer.Gen(struct type nonrec _ t = t end)()
    end in
    let partition_transition set =
      let sigmas =
        IndexSet.fold begin fun src acc ->
          let state = Vector.get states src in
          let src_action = match Label.Action.priority state.label.action with
            | None -> max_int
            | Some priority -> priority
          in
          List.fold_left begin fun acc (sg, cs, tgt) ->
            match BitSet.IntSet.minimum (Vector.get reachable_action tgt) with
            | Some tgt_action when tgt_action < src_action ->
              ((sg, (src, cs, tgt)) :: acc)
            | _ -> acc
          end acc state.transitions
        end set []
      in
      Sigma.annotated_partition sigmas
    in
    ClassMap.iter begin fun _cl st ->
      List.iter begin fun (sg, pairs) ->
        List.iter begin fun (source, cs, target) ->
          let label = (sg, cs) in
          ignore (Transitions.add {label; source; target} : _ index)
        end pairs;
      end (partition_transition !st);
    end !classes;
    let module ActionMap = Map.Make(Label.Action) in
    let action_classes = ref ActionMap.empty in
    Index.iter States.n (fun i ->
        let state = Vector.get states i in
        match state.label.action with
        | Label.Action.Nothing -> ()
        | Label.Action.Action _ as action ->
          match ActionMap.find_opt action !action_classes with
          | None ->
            let si = IndexSet.singleton i in
            action_classes := ActionMap.add action (ref si) !action_classes
          | Some set ->
            set := IndexSet.add i !set
      );
    let transition_table = Transitions.freeze () in
    let module DFA = struct
      let label tr = (Vector.get transition_table tr).label
      let source tr = (Vector.get transition_table tr).source
      let target tr = (Vector.get transition_table tr).target

      let finals f =
        ActionMap.iter (fun _ si -> IndexSet.iter f !si) !action_classes

      let initials f = f (fst initial)

      let refinements ~refine =
        ActionMap.iter
          (fun _ si -> refine ~iter:(fun f -> IndexSet.iter f !si))
          !action_classes

      type states = States.n
      let states = States.n

      type transitions = Transitions.n
      let transitions = Transitions.n
    end in
    let module Min = Valmari.Minimize(Transitions.Label)(DFA) in
    (* Print statistics on minimzed automaton *)
    begin
      let unique_transitions = Hashtbl.create 7 in
      Index.iter Min.transitions (fun tr ->
          let src = Min.source tr in
          let tgt = Min.target tr in
          Hashtbl.replace unique_transitions (src, tgt) ()
        );
      Printf.eprintf "minimized: states:%d transitions:%d (%d unique)\n%!"
        (cardinal Min.states) (cardinal Min.transitions) (Hashtbl.length unique_transitions);
    end;
    let get_label st =
      let large_st = Min.represent_state st in
      (Vector.get states large_st).label
    in
    (* Index transitions by source states *)
    let transitions_from = Vector.make Min.states IndexMap.empty in
    Index.iter Min.transitions (fun tr ->
        let source = Min.source tr in
        let target = Min.target tr in
        let label = Min.label tr in
        let map = Vector.get transitions_from source in
        match IndexMap.find_opt target map with
        | None ->
          Vector.set transitions_from source
            (IndexMap.add target (ref label) map)
        | Some rlabel ->
          rlabel := Transitions.Label.partial_union label !rlabel
      );
    (* Estimate benefits of a layered transitions encoding *)
    begin
      let forest = Transition_tree.build_tree Min.states (fun index ->
          let transitions = Vector.get transitions_from index in
          let pos, neg = IndexMap.fold (fun st lbl (pos, neg) ->
              let (sg, cs) = !lbl in
              match sg with
              | Sigma.Pos lr1s ->
                assert (not (IndexMap.mem st pos));
                (IndexMap.add st (lr1s (*FIXME , cs*)) pos, neg)
              | Sigma.Neg lr1s -> (pos, (lr1s, cs, st) :: neg)
            ) transitions (IndexMap.empty, [])
          in
          let coverage =
            IndexMap.fold (fun _ sg acc -> IndexSet.union sg acc)
              pos IndexSet.empty
          in
          let neg = match neg with
            | [] -> None
            | [_, _, st] -> Some st
            | _ :: _ :: _ -> assert false
          in
          (coverage, pos, neg)
        )
      in
      let naive_transitions = ref 0 in
      let optimized_transitions = ref 0 in
      let depth_max = ref 0 in
      let depth_sum = ref 0 in
      let count = ref 0 in
      let (+=) r x = r := !r + x in
      let rec add depth {Transition_tree. entry; cost; forest} =
        naive_transitions += entry.covered;
        optimized_transitions += cost;
        depth_max := max depth !depth_max;
        if depth > 1 then (
          depth_sum += depth;
          incr count;
        );
        add_forest depth !forest
      and add_forest depth forest =
        List.iter (add (depth + 1)) forest
      in
      add_forest 0 forest;
      Printf.eprintf "(* transitions without inheritance: %d\n\
                     \   transitions with    inheritance: %d\n\
                     \   max_depth: %d, avg_depth: %f *)\n"
        !naive_transitions
        !optimized_transitions
        !depth_max
        (float !depth_sum /. float !count)
    end;
    (* Print matching functions *)
    Printf.printf
      "module Table : sig\n\
      \  open Analyser_def\n\
      \  type state\n\
      \  val initial : state\n\
      \  val step : state -> int -> \n\
      \    register list * clause option * state option\n\
       end = struct\n\
      \  type state = int\n\
      \  let table = [|\n\
      ";
    let visit (index : Min.states index) =
      Printf.printf "    (fun st ->";
      let action = match Label.Action.priority (get_label index).action with
        | None -> "None"
        | Some priority ->
          Printf.sprintf "Some %d" priority
      in
      let pos, neg = IndexMap.fold (fun st lbl (pos, neg) ->
          let sg, cs = !lbl in
          match sg with
          | Sigma.Pos lr1s -> ((lr1s, cs, st) :: pos, neg)
          | Sigma.Neg lr1s -> (pos, (lr1s, cs, st) :: neg)
        ) (Vector.get transitions_from index) ([], [])
      in
      let print_captures cs =
        let print_capture c =
          match c.Clause.var with
          | None -> assert false
          | Some var ->
            Printf.sprintf "(%d,%d)" c.Clause.for_clause.priority var
        in
        "[" ^ String.concat ";" (List.map print_capture cs) ^ "]"
      in
      let print_states lr1s =
        IndexSet.elements lr1s
        |> List.map (string_of_int : int -> string :> _ index -> string)
        |> String.concat "|"
      in
      match pos, neg with
      | [], [] ->
        Printf.printf " ([], %s, None));\n" action
      | [], [_, cs, st] ->
        Printf.printf " (%s, %s, Some %d));\n"
          (print_captures cs) action (st :> int)
      | _ ->
        Printf.printf "\n    match st with\n";
        List.iter begin fun (lr1s, cs, st) ->
          Printf.printf "    | %s -> (%s, %s, Some %d)\n"
            (print_states lr1s)
            (print_captures cs)
            action
            (st : _ index :> int)
        end pos;
        begin match neg with
          | [] ->
            Printf.printf "    | _ -> ([], %s, None)"
              action
          | [_, cs, st] ->
            Printf.printf "    | _ -> (%s, %s, Some %d)"
              (print_captures cs)
              action
              (st :> int)
          | _ :: _ :: _ -> assert false
        end;
        Printf.printf ");\n";
    in
    Index.iter Min.states visit;
    Printf.printf
      "  |]\n\
      \  let initial = %d\n\
      \  let step st lr1 = table.(st) lr1\n\
       end\n"
      (Min.initials.(0) :> int)
end

let test_stack =
  List.map (Index.of_int Lr1C.n)
    (*[509;617;585;1124;1123;1122;618;812;802;617;585;1124;1123;1122;618;0]*)
    [509;617;585;1124;1123;1122;618;812;802;617;585;1124;1123;1122;1643;1642;0]

let () =
  prerr_endline (
    "Test stack: " ^
    String.concat " "
      (List.rev_map symbol_name
         (List.filter_map (fun x -> Lr0.incoming (Lr1C.to_lr0 x)) test_stack))
  )

let step re state =
  let _lbl, re = Reg.Expr.left_delta re (Sigma.singleton state) in
  let itemset = Lr0.items (Lr1C.to_lr0 state) in
  let action = match (Reg.Expr.get_label re).action with
    | Label.Action.Nothing ->
      "\x1b[31mNo action\x1b[m"
    | Label.Action.Action {priority; _} ->
      "\x1b[32mAction\x1b[m " ^ string_of_int priority
  in
  Format.eprintf "Matching %s at state %d:\n%a%s%!\n%a%!\n-------------\n%!"
    (match Lr0.incoming (Lr1C.to_lr0 state) with
     | None -> "()"
     | Some sym -> symbol_name sym)
    (state :> int) Print.itemset itemset action Cmon.format (cmon_re re);
  re

let rec interpret re = function
  | [] -> re
  | [last] ->
    (*do_log := true;*)
    step re last
  | x :: xs ->
    interpret (step re x) xs

let () = (
  let entry = List.hd lexer_definition.entrypoints in
  Format.eprintf "%a\n%!" Cmon.format (Syntax.print_entrypoints entry);
  let clauses, re = translate_entry entry in
  (*Format.eprintf "Starting from @[%a@]\n%!" Cmon.format (cmon_re re);*)
  ignore (interpret re test_stack : Reg.Expr.t);
  (*Format.printf "%a\n%!" Cmon.format (cmon_re re);*)
  let (Large_dfa {set=(module Set); _} as dfa) = DFA.translate re in
  prerr_endline (string_of_int (cardinal Set.n) ^ " states");
  DFA.minimize dfa;
  Clause.gencode clauses
)
