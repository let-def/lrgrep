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

(* The lexer generator. Command-line parsing. *)

let source_name = ref None
let output_name = ref None
let grammar_file = ref None
let interpret = ref false

let usage = "usage: menhirlex [options] sourcefile"

let print_version_string () =
  print_string "The Menhir parser lexer generator :-], version ";
  print_string Sys.ocaml_version;
  print_newline ();
  exit 0

let print_version_num () =
  print_endline Sys.ocaml_version;
  exit 0

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

let array_set_add arr index value =
  arr.(index) <- IndexSet.add value arr.(index)

let array_cons arr index value =
  arr.(index) <- value :: arr.(index)

let vector_iter v f =
  Index.iter (Vector.length v) (fun i -> f (Vector.get v i ))

let compare_index =
  (Int.compare : int -> int -> int :> _ index -> _ index -> int)

let cmon_index =
  (Cmon.int : int -> Cmon.t :> _ index -> Cmon.t)

let cmon_indexset xs =
  Cmon.list_map cmon_index (IndexSet.elements xs)

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

let lr1set_bind : Lr1C.set -> (Lr1C.t -> Lr1C.set) -> Lr1C.set =
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
  lr1set_bind lr1s (Vector.get lr1_predecessors)

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
      let vector = Array.make depth [] in
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
    let lr1 = List.hd stack.prefix in
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
  }

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
        |> List.map (fun (targets, sources) ->
            {sources = IndexSet.of_list sources; targets})
      in
      {states = state.states; goto_transitions}
    in
    { stack = Array.of_list (List.map prepare_node nodes) }

  let roots = Vector.init Lr1C.n (fun lr1 ->
      let root = {prefix = []; base = lr1; suffix = ref None} in
      let vs = close_transitions [] root  in
      List.iter (Closed_derivation.register lr1) vs;
      root
    )

  let () =
    (* Compute the "goto_from" relation *)
    let register_goto ~from target =
      let target = Vector.get roots target in
      match target.ancestor with
      | Node_next_of _ -> assert false
      | Node_goto_from t -> t.nodes <- from :: t.nodes
    in
    let rec visit node =
      IndexSet.iter (register_goto ~from:node) node.goto;
      List.iter visit node.next
    in
    vector_iter roots visit

  let nodes =
    let dummy () = Vector.get roots (Index.of_int Lr1C.n 0) in
    Vector.make' Node_id.n dummy

  let () =
    (* Initialize nodes and compute "next_goto_closure" relation *)
    let module Property = struct
      type property = Lr1C.set
      let leq_join = IndexSet.union
    end in
    let module Graph = struct
      type variable = Node_id.n index

      let foreach_root f =
        let rec populate node =
          Vector.set nodes node.id node;
          let acc = List.fold_left visit_next IndexSet.empty node.next in
          f node.id acc;
          acc
        and visit_next acc node =
          IndexSet.union (IndexSet.union node.goto (populate node)) acc
        in
        let populate' node = ignore (populate node) in
        vector_iter roots populate'

      let foreach_successor id reachable f =
        let self = Vector.get nodes id in
        let reachable = IndexSet.union reachable self.goto in
        match self.ancestor with
        | Node_next_of node -> f node.id reachable
        | Node_goto_from {nodes} ->
          List.iter (fun node -> f node.id reachable) nodes
    end in
    let module Store = struct
      let get id = (Vector.get nodes id).next_goto_closure
      let set id closure = (Vector.get nodes id).next_goto_closure <- closure
    end in
    let module Marks = struct
      let vector = Vector.make Node_id.n false
      let get = Vector.get vector
      let set = Vector.set vector
    end in
    let module _ =
      Fix.DataFlow.ForCustomMaps(Property)(Graph)(Store)(Marks)
    in
    ()

  let hashtbl_find_or_ref table key value =
    match Hashtbl.find table key with
    | ref -> ref
    | exception Not_found ->
      let r = ref value in
      Hashtbl.add table key r;
      r

  let node_reverse_deps =
    let vector = Vector.make Lr1C.n IndexSet.empty in
    Index.iter Lr1C.n (fun lr1 ->
        let rec visit ps =
          IndexSet.iter (fun lr1' ->
              let states = Vector.get vector lr1' in
              Vector.set vector lr1' (IndexSet.add lr1 states)
            ) ps.goto;
          List.iter visit ps.next
        in
        List.iter visit (Vector.get roots lr1).next
      );
    vector

  let () = if false then (
    print_endline "digraph G {";
    print_endline "  overlap=false";
    let visited = Vector.make Lr1C.n false in
    let should_visit lr1 =
      let result = not (Vector.get visited lr1) in
      Vector.set visited lr1 true;
      result
    in
    let target_states =
      Syntax.Name "let_binding_body"
      |> State_indices.find_symbol
      |> Option.get
      |> State_indices.states_of_symbol
    in
    let target_closure =
      let closure = ref IndexSet.empty in
      let rec expand states =
        if not (IndexSet.is_empty states) then
          expand (
            IndexSet.fold (fun lr1 acc ->
                if IndexSet.mem lr1 !closure then
                  acc
                else (
                  closure := IndexSet.add lr1 !closure;
                  IndexSet.union (Vector.get node_reverse_deps lr1) acc
                )
              ) states IndexSet.empty
          )
      in
      expand target_states;
      !closure
    in
    let rec visit lr1 =
      if not (IndexSet.mem lr1 target_closure) then
        false
      else (
        if should_visit lr1 then (
          Printf.printf "  ST%d[label=%S]\n"
            (lr1 : _ index :> int)
            (Format.asprintf "%d: %a" (lr1 :> int) Print.itemset
               (Lr0.items (Lr1C.to_lr0 lr1)));
          let tgt_table = Hashtbl.create 7 in
          let rec traverse path node =
            let path = (node.lr1 : _ index :> int) :: path in
            List.iter (traverse path) node.next;
            IndexSet.iter (fun lr1' ->
                if visit lr1' then
                  let r = hashtbl_find_or_ref tgt_table lr1' [] in
                  r := path :: !r
              ) node.goto
          in
          List.iter (traverse []) (Vector.get roots lr1).next;
          Hashtbl.iter (fun lr1' paths ->
              Printf.printf "  ST%d -> ST%d [label=%S]\n"
                (lr1 : _ index :> int)
                (lr1' : _ index :> int)
                (String.concat "\n"
                   (List.map (fun path -> String.concat " -> " (List.rev_map string_of_int path))
                      !paths));
            ) tgt_table;
        );
        true
      )
    in
    ignore (visit (Index.of_int Lr1C.n 509) : bool);
    print_endline "}";
  )

  (*let () =
    print_endline "digraph G {";
    let reachable = Vector.make Lr1C.n false in
    let rec reach lr1 =
      if not (Vector.get reachable lr1) then (
        Vector.set reachable lr1 true;
        let rec visit node =
          IndexSet.iter reach node.goto;
          List.iter visit node.parents
        in
        visit (Vector.get roots lr1).node
      )
    in
    reach (Index.of_int Lr1C.n 509);
    Index.iter Lr1C.n (fun src ->
        if Vector.get reachable src then (
          let stt = Vector.get roots src in
          Printf.printf "  ST%d[fontname=Mono,shape=box,label=%S]\n"
            (src :> int)
            (Format.asprintf "%d\n%a" (src :> int)
               Print.itemset (Lr0.items (Lr1.lr0 (Lr1C.to_g src))));
          let tgt_table = Hashtbl.create 7 in
          let rec visit_target pst =
            IndexSet.iter (fun tgt ->
                let lbl = string_of_int (pst.lr1 :> int) in
                match Hashtbl.find tgt_table tgt with
                | exception Not_found ->
                  Hashtbl.add tgt_table tgt (ref [lbl])
                | lst -> lst := lbl :: !lst
              ) pst.goto;
            List.iter visit_target pst.parents
          in
          List.iter visit_target stt.node.parents;
          Hashtbl.iter (fun tgt srcs ->
              Printf.printf "  ST%d -> ST%d [label=%S]\n"
                (src :> int) (tgt : _ index :> int)
                (String.concat "|" !srcs)
            ) tgt_table
        )
      );
    print_endline "}";*)
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
  val to_lr1set : t -> Lr1C.set

  include Mulet.SIGMA with type t := t

  val union : t -> t -> t
  (** Compute union of two sets *)

  val intersect : t -> t -> bool
  (** Check if two sets intersect *)

  val mem : Lr1C.t -> t -> bool
  (** [mem lr1 t] checks if the state [lr1] is an element of a sigma set [t] *)

  val cmon : t -> Cmon.t
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
end

module Label = struct
  type t =
    | Nothing
    | Action of { priority: int; action_desc: action_desc }

  and action_desc =
    | Unreachable
    | Code of string

  let empty = Nothing
  let compare : t -> t -> int = compare
  let append t1 t2 =
    match t1, t2 with
    | Nothing, x | x, Nothing -> x
    | Action {priority=p1; _}, Action {priority=p2; _} ->
      if p1 <= p2
      then t1
      else t2
end

module rec
  Reg
  : Mulet.S with type sigma = Sigma.t
             and type label = Label.t
             and type abstract = Redgraph_derivation.t
  = Mulet.Make(Sigma)(Label)(Redgraph_derivation)

and Redgraph_derivation :
sig
  include Mulet.DERIVABLE
    with type sigma := Sigma.t
     and type label := Label.t

  type compiled
  val compile : Reg.Expr.t -> compiled

  val make : compiled -> Lr1C.set -> Reg.Expr.t

  val cmon : t -> Cmon.t
end =
struct
  type compiled = {
    source: Reg.Expr.t;
    derivations: (Label.t * Reg.Expr.t) Lr1Map.t;
    domain: Lr1C.set;
  }

  let compare_compilation t1 t2 =
    Reg.Expr.compare t1.source t2.source

  let compile source =
    let source = Reg.Expr.(source ^. star (set Sigma.full)) in
    let closed_derivations =
      Redgraph.Closed_derivation.derive
        ~root:source
        ~step:(fun re lr1 ->
            let _label, re' = Reg.Expr.left_delta re (Sigma.singleton lr1) in
            (*TODO: what to do with the label?*)
            re')
        ~join:Reg.Expr.(|.)
    in
    let derivations = ref Lr1Map.empty in
    let domain = ref IndexSet.empty in
    let visit lr1 =
      (* Can do a bit better: group states that are "classes of interest" for
         source re, so that derivation is shared *)
      let re = match Lr1Map.find_opt lr1 closed_derivations with
        | None -> source
        | Some re -> Reg.Expr.(|.) re source
      in
      let lbl, re = Reg.Expr.left_delta re (Sigma.singleton lr1) in
      if not (Reg.Expr.is_empty re) then (
        derivations := Lr1Map.add lr1 (lbl, re) !derivations;
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
    | Node of { node: Redgraph.node; compiled: compiled; }
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
      let c = compare_index t1.node.id t2.node.id in
      if c <> 0 then c else
        compare_compilation t1.compiled t2.compiled

  let is_empty _ = false

  let nullable _ = false

  let get_label _ = Label.empty

  let make compiled states =
    Reg.Expr.abstract (Root { compiled; states })

  let left_classes t f acc =
    match t with
    | Root t ->
      IndexSet.fold (fun lr1 acc -> f (Sigma.singleton lr1) acc) t.states acc
    | Node _t ->
      (*let rec visit acc node =
        List.fold_left visit
          (List.fold_left
             (fun acc (parent : Redgraph.node) ->
                f (Sigma.singleton parent.lr1) acc)
             acc node.Redgraph.next)
          node.Redgraph.goto_next
      in
        visit acc t.node*)
      acc

  let empty_delta = (Label.empty, Reg.Expr.empty)

  let make_one compiled node =
    (*if IndexSet.disjoint compiled.domain node.Redgraph.next_goto_closure
    then Reg.Expr.empty
    else*) Reg.Expr.abstract (Node { node; compiled })

  let make_initial compiled lr1 =
    let expr = make_one compiled (Vector.get Redgraph.roots lr1) in
    match Lr1Map.find_opt lr1 compiled.derivations with
    | None -> (Label.empty, expr)
    | Some (lbl, expr') -> (lbl, Reg.Expr.(expr |. expr'))

  let node_with_state lr1 node =
    lr1 = node.Redgraph.lr1

  let next_at node lr1 =
    List.find_opt (node_with_state lr1) node.Redgraph.next

  let rec make_delta compiled (node : Redgraph.node) =
    let nodes =
      IndexSet.fold (fun lr1 nodes ->
          match next_at (Vector.get Redgraph.roots lr1) node.lr1 with
          | None -> nodes
          | Some node -> node :: nodes
        ) node.goto []
    in
    let lbls, res = List.split (List.map (make_delta compiled) nodes) in
    let lbls, res =
      IndexSet.fold begin fun lr1 (lbls, res) ->
        match Lr1Map.find_opt lr1 compiled.derivations with
        | None -> (lbls, res)
        | Some (_lbl, re) ->
          let lbl, re = Reg.Expr.left_delta re (Sigma.singleton node.lr1) in
          (lbl :: lbls, re :: res)
      end node.goto (lbls, res)
    in
    (List.fold_left Label.append Label.empty lbls,
     Reg.Expr.disjunction (make_one compiled node :: res))

  let left_delta (t : t) = function
    | Sigma.Pos s when IndexSet.is_singleton s ->
      let lr1 = IndexSet.choose s in
      begin match t with
        | Root t ->
          if IndexSet.mem lr1 t.states
          then make_initial t.compiled lr1
          else empty_delta
        | Node t ->
          begin match next_at t.node lr1 with
            | None -> empty_delta
            | Some node' -> make_delta t.compiled node'
          end
      end
    | _ -> empty_delta

  let cmon _ = Cmon.unit
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

let rec translate_term = function
  | Syntax.Symbol name ->
    let symbol = translate_symbol name in
    let states = State_indices.states_of_symbol symbol in
    Reg.Expr.set (Sigma.Pos states)
  | Syntax.Item {lhs; prefix; suffix} ->
    let lhs = Option.map translate_nonterminal lhs in
    let prefix = translate_producers prefix in
    let suffix = translate_producers suffix in
    let states = Match_item.states_by_items ~lhs ~prefix ~suffix in
    Reg.Expr.set (Sigma.Pos states)
  | Syntax.Wildcard ->
    Reg.Expr.set Sigma.full
  | Syntax.Alternative (e1, e2) ->
    Reg.Expr.(translate_expr e1 |. translate_expr e2)
  | Syntax.Repetition (e1, _) ->
    Reg.Expr.star (translate_expr e1)
  | Syntax.Reduce (expr, _) ->
    let expr = translate_expr expr in
    let compiled = Redgraph_derivation.compile expr in
    let expr' = Redgraph_derivation.make compiled all_states in
    Reg.Expr.(expr |. expr')

and translate_expr terms =
  let terms = List.rev_map (fun (term, _) -> translate_term term) terms in
  Reg.Expr.concatenation terms

let translate_clause priority {Syntax. pattern; action} =
  let action_desc = match action with
    | None -> Label.Unreachable
    | Some (_, code) -> Label.Code code
  in
  let pattern = translate_expr pattern in
  let label = Reg.Expr.label (Action {priority; action_desc}) in
  Reg.Expr.(^.) pattern label

let translate_entry {Syntax. startsymbols; error; name; args; clauses} =
  (* TODO *)
  ignore (startsymbols, error, name, args);
  let clauses = List.mapi translate_clause clauses in
  clauses

(* Derive DFA with naive intersection *)

module DFA = struct
  let sigma_predecessors sg =
    Sigma.Pos (lr1set_predecessors (Sigma.to_lr1set sg))

  type state = {
    index: int;
    expr: Reg.Expr.t;
    mutable visited: Sigma.t;
    mutable scheduled: Sigma.t;
    mutable unvisited: Sigma.t list;
    mutable transitions: (Sigma.t * state) list;
  }

  let translate expr =
    let count = ref 0 in
    let dfa = ref Reg.Map.empty in
    let todo = ref [] in
    let make_state expr =
      let index = !count in
      incr count;
      let classes = Reg.Expr.left_classes expr (fun sg sgs -> sg :: sgs) [] in
      let classes = Sigma.partition classes in
      let state = {
        index; expr;
        unvisited = classes;
        visited = Sigma.empty;
        scheduled = Sigma.empty;
        transitions = []
      } in
      dfa := Reg.Map.add expr state !dfa;
      state
    in
    let schedule state sigma =
      let unvisited = Sigma.inter sigma (Sigma.compl state.visited) in
      if not (Sigma.is_empty unvisited) then (
        if Sigma.is_empty state.scheduled then
          todo := state :: !todo;
        state.scheduled <- Sigma.union state.scheduled sigma
      )
    in
    let update_transition sigma (sigma', state') =
      let inter = Sigma.inter sigma sigma' in
      if not (Sigma.is_empty inter) then
        schedule state' (sigma_predecessors inter)
    in
    let discover_transition sigma state sigma' =
      let inter = Sigma.inter sigma sigma' in
      (*Format.printf "discover %a\n%!"
        Cmon.format (Cmon.tuple [Sigma.cmon sigma; Sigma.cmon sigma']);*)
      if Sigma.is_empty inter then
        Either.Left sigma'
      else (
        let _, expr' = Reg.Expr.left_delta state.expr sigma' in
        let state' = match Reg.Map.find_opt expr' !dfa with
          | None -> make_state expr'
          | Some state' -> state'
        in
        schedule state' (sigma_predecessors inter);
        Either.Right (sigma', state')
      )
    in
    let process state =
      state.visited <- Sigma.union state.visited state.scheduled;
      let sigma = state.scheduled in
      state.scheduled <- Sigma.empty;
      List.iter (update_transition sigma) state.transitions;
      let unvisited, new_transitions =
        List.partition_map (discover_transition sigma state) state.unvisited
      in
      (*Printf.printf "%d unvisited, %d new_transitions\n"
        (List.length unvisited) (List.length new_transitions);*)
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
    (!dfa, initial)

  let compile (dfa, initial : state Reg.Map.t * state) =
    let first = ref true in
    Printf.printf "let analyse stack =\n";
    let interesting = ref BitSet.IntSet.empty in
    Reg.Map.iter (fun _ state ->
        if state.transitions = []
        && Reg.Expr.get_label state.expr = Label.Nothing
        then ()
        else interesting := BitSet.IntSet.add state.index !interesting
      ) dfa;
    let interesting = !interesting in
    let visit (state : state) =
      if BitSet.IntSet.mem state.index interesting then (
        Printf.printf "  %s st_%d stack =\n"
          (if !first then "let rec" else "and")
          state.index;
        first := false;
        let action = Reg.Expr.get_label state.expr in
        begin match action with
          | Label.Action { priority ; _ } -> Printf.printf "    %d ::\n" priority
          | Label.Nothing -> ()
        end;
        Printf.printf "    match state stack with\n";
        List.iter (fun (sg, st) ->
            if BitSet.IntSet.mem st.index interesting then (
              Printf.printf
                "    | %s -> st_%d (next stack)\n"
                (Sigma.to_lr1set sg
                 |> IndexSet.elements
                 |> List.map (string_of_int : int -> string :> _ index -> string)
                 |> String.concat "|")
                st.index
            )
          ) state.transitions;
        Printf.printf "    | _ -> []\n";
      )
    in
    Reg.Map.iter (fun _ state -> visit state) dfa;
    Printf.printf "  in st_%d stack" initial.index
end

let cmon_re re =
  Mulet.cmon_re re
    ~set:Sigma.cmon ~label:(fun _ -> Cmon.unit)
    ~abstract:Redgraph_derivation.cmon

let test_stack =
  List.map (Index.of_int Lr1C.n)
    [509;617;585;1124;1123;1122;618;812;802;617;585;1124;1123;1122;618;0]
    (*[509;617;585;1124;1123;1122;618;812;802;617;585;1124;1123;1122;1643;1642;0]*)

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
  let action = match Reg.Expr.get_label re with
    | Label.Nothing -> "\x1b[31mNo action\x1b[m"
    | Label.Action {priority; _} -> "\x1b[32mAction\x1b[m " ^ string_of_int priority
  in
  Format.eprintf "Matching %s at state %d:\n%a%s%!\n%a%!\n-------------\n%!"
    (match Lr0.incoming (Lr1C.to_lr0 state) with
     | None -> "()"
     | Some sym -> symbol_name sym)
    (state :> int) Print.itemset itemset action Cmon.format (cmon_re re);
  re

let interpret re stack =
  List.fold_left step re stack

let () = (
  let entry = List.hd lexer_definition.entrypoints in
  (*Format.printf "%a\n%!" Cmon.format (Syntax.print_entrypoints entry);*)
  let clauses = translate_entry entry in
  let re = Reg.Expr.disjunction clauses in
  ignore (interpret re test_stack : Reg.Expr.t);
  (*Format.printf "%a\n%!" Cmon.format (cmon_re re);*)
  (*let dfa, initial = DFA.translate re in
    let count = Reg.Map.cardinal dfa in
    prerr_endline (string_of_int count ^ " states");
    DFA.compile (dfa, initial);*)
)
