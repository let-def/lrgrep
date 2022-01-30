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

let vector_iter v f =
  Index.iter (Vector.length v) (fun i -> f (Vector.get v i ))

let compare_index =
  (Int.compare : int -> int -> int :> _ index -> _ index -> int)

let initial_states : (nonterminal * lr1) list =
  Lr1.fold begin fun lr1 acc ->
    let lr0 = Lr1.lr0 lr1 in
    match Lr0.incoming lr0 with
    | Some _ -> acc
    | None ->
      (* Initial state *)
      Format.eprintf "Initial state %d\n%a\n"
        (Lr1.to_int lr1)
        Print.itemset (Lr0.items lr0);
      let (prod, _) = List.hd (Lr0.items lr0) in
      let nt = match (Production.rhs prod).(0) with
        | N nt, _, _ -> nt
        | _ -> assert false
      in
      (nt, lr1) :: acc
  end []

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
  let of_g lr1 = Index.of_int n (Lr1.to_int lr1)
  let to_g lr1 = Lr1.of_int (Index.to_int lr1)
end

module Lr1Map = Map.Make(struct
    type t = Lr1C.n index
    let compare = compare_index
  end)
module IndexRefine = Refine.Make(Utils.BitSet.IndexSet)

let all_states =
  let acc = ref IndexSet.empty in
  for i = (cardinal Lr1C.n) - 1 downto 0
  do acc := IndexSet.add (Index.of_int Lr1C.n i) !acc done;
  !acc

let lr1set_bind : Lr1C.n IndexSet.t -> (Lr1C.n index -> Lr1C.n IndexSet.t) -> Lr1C.n IndexSet.t =
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

  let array_set_add arr index value =
    arr.(index) <- IndexSet.add value arr.(index)

  let states_of_terminals =
    Array.make Terminal.count IndexSet.empty

  let states_of_nonterminals =
    Array.make Nonterminal.count IndexSet.empty

  let () =
    Index.iter Lr1C.n (fun lr1 ->
        match Lr0.incoming (Lr1.lr0 (Lr1C.to_g lr1)) with
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
  val find_goto : Lr1C.n index -> Nonterminal.t -> goto index

  (* Get the source state of a transition *)
  val source : any index -> Lr1C.n index

  (* Get the target state of a transition *)
  val target : any index -> Lr1C.n index

  (* Symbol that labels a transition *)
  val symbol : any index -> symbol

  (* Symbol that labels a goto transition *)
  val goto_symbol : goto index -> Nonterminal.t

  (* Symbol that labels a shift transition *)
  val shift_symbol : shift index -> Terminal.t

  (* [successors s] returns all the transitions [tr] such that
     [source tr = s] *)
  val successors : Lr1C.n index -> any index list

  (* [predecessors s] returns all the transitions [tr] such that
     [target tr = s] *)
  val predecessors : Lr1C.n index -> any index list
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

let set_of_predecessors =
  Vector.init Lr1C.n (fun lr1 ->
      let transitions = Transition.predecessors lr1 in
      List.fold_left
        (fun set tr -> IndexSet.add (Transition.source tr) set)
        IndexSet.empty transitions
    )

module Redgraph =
struct
  module Node_id = Gensym()

  type node = {
    id: Node_id.n index;
    lr1: Lr1C.n index;
    ancestor : ancestor;
    mutable next: node list;
    mutable goto: Lr1C.n IndexSet.t;
    mutable next_goto_closure: Lr1C.n IndexSet.t;
  }

  and ancestor =
    | Node_next_of of node
    | Node_goto_from of {mutable nodes: node list}

  type stack =
    | Goto of {lr1: Lr1C.n index; parent: stack}
    | Lr1 of node

  let lr1 (Goto {lr1; _} | Lr1 {lr1; _}) = lr1

  let productions =
    Vector.init Lr1C.n (fun lr1 ->
        let order p1 p2 =
          let c =
            Int.compare
              (Array.length (Production.rhs p1))
              (Array.length (Production.rhs p2))
          in
          if c = 0
          then Int.compare (p1 :> int) (p2 :> int)
          else c
        in
        Lr1.reductions (Lr1C.to_g lr1)
        |> List.map (fun (_, ps) -> List.hd ps)
        |> List.sort_uniq order
      )

  type states =
    | Concrete of stack
    | Abstract of node list

  let make_node lr1 ancestor = {
    id = Node_id.fresh ();
    lr1;
    ancestor;
    goto = IndexSet.empty;
    next = [];
    next_goto_closure = IndexSet.empty;
  }

  let get_parent_nodes node =
    match node.next with
    | [] ->
      let ancestor = Node_next_of node in
      let next = List.map
          (fun tr -> make_node (Transition.source tr) ancestor)
          (Transition.predecessors node.lr1)
      in
      node.next <- next;
      next
    | next -> next

  let pop = function
    | Concrete (Goto {parent; _}) -> Concrete parent
    | Concrete (Lr1 t) ->
      Abstract (get_parent_nodes t)
    | Abstract states ->
      Abstract (
        List.fold_left
          (fun states state -> get_parent_nodes state @ states) [] states
      )

  let rec pop_many states = function
    | 0 -> states
    | n ->
      assert (n > 0);
      pop_many (pop states) (n - 1)

  let goto_target lr1 nt =
    match Transition.(target (of_goto (find_goto lr1 nt))) with
    | exception Not_found -> None
    | result -> Some result

  let goto nt acc = function
    | Abstract states ->
      List.iter (fun state ->
          match goto_target state.lr1 nt with
          | None -> ()
          | Some st -> state.goto <- IndexSet.add st state.goto
        ) states;
      acc
    | Concrete st ->
      begin match goto_target (lr1 st) nt with
        | None -> acc
        | Some tgt -> Goto {lr1=tgt; parent=st} :: acc
      end

  let follow_transitions state =
    let rec follow states depth acc = function
      | [] -> acc
      | p :: ps ->
        let depth' = Array.length (Production.rhs p) in
        let states = pop_many states (depth' - depth) in
        let acc = goto (Production.lhs p) acc states in
        follow states depth' acc ps
    in
    follow (Concrete state) 0 [] (Vector.get productions (lr1 state))

  let rec close_transitions acc state =
    let new_transitions = follow_transitions state in
    let acc = new_transitions @ acc in
    List.fold_left close_transitions acc new_transitions

  module Closed_derivation = struct
    type node = {
      mutable children: node Lr1Map.t;
      mutable sources: Lr1C.n IndexSet.t;
    }

    let fresh () = {children = Lr1Map.empty; sources = IndexSet.empty}

    let root_node = fresh ()

    let delta lr1 node =
      match Lr1Map.find_opt lr1 node.children with
      | Some node' -> node'
      | None ->
        let node' = fresh () in
        node.children <- Lr1Map.add lr1 node' node.children;
        node'

    let register source state =
      let rec loop node state =
        let (Lr1 {lr1; _} | Goto {lr1; _}) = state in
        let node' = delta lr1 node in
        match state with
        | Lr1 _ -> node'
        | Goto {parent; _} -> loop node' parent
      in
      let node = loop root_node state in
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

  let roots =
    Vector.init Lr1C.n @@ fun lr1 ->
    let root = make_node lr1 (Node_goto_from {nodes=[]}) in
    let vs = close_transitions [] (Lr1 root)  in
    List.iter (Closed_derivation.register lr1) vs;
    root

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
    (* Initialize ndoes and compute "next_goto_closure" relation *)
    let module Property = struct
      type property = Lr1C.n IndexSet.t
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

  (*let () = (
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
               (Lr0.items (Lr1.lr0 (Lr1C.to_g lr1))));
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
          List.iter (traverse []) (Vector.get roots lr1).node.next;
          Hashtbl.iter (fun lr1' paths ->
              let path = List.hd !paths in
              Printf.printf "  ST%d -> ST%d //[label=%S]\n"
                (lr1 : _ index :> int)
                (lr1' : _ index :> int)
                (String.concat " -> " (List.rev_map string_of_int path));
            ) tgt_table;
        );
        true
      )
    in
    ignore (visit (Index.of_int Lr1C.n 509) : bool);
    print_endline "}";
  )*)

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
    | Pos of Lr1C.n IndexSet.t
    | Neg of Lr1C.n IndexSet.t

  val singleton : Lr1C.n index -> t
  val to_lr1set : t -> Lr1C.n IndexSet.t

  include Mulet.SIGMA with type t := t

  val union : t -> t -> t
  (** Compute union of two sets *)

  val intersect : t -> t -> bool
  (** Check if two sets intersect *)

  val mem : Lr1C.n index -> t -> bool
  (** [mem lr1 t] checks if the state [lr1] is an element of a sigma set [t] *)

  val cmon : t -> Cmon.t
end = struct
  type t =
    | Pos of Lr1C.n IndexSet.t
    | Neg of Lr1C.n IndexSet.t

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
    | Pos _ -> Cmon.construct "Pos" [Cmon.constant "_"]
    | Neg _ -> Cmon.construct "Neg" [Cmon.constant "_"]
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

  val make : compiled -> Lr1C.n IndexSet.t -> Reg.Expr.t
end =
struct
  type compiled = {
    source: Reg.Expr.t;
    derivations: (Label.t * Reg.Expr.t) Lr1Map.t;
    domain: Lr1C.n IndexSet.t;
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

  type t = {
    node: Redgraph.node;
    compiled: compiled;
  }

  let compare t1 t2 =
    let c = compare_index t1.node.id t2.node.id in
    if c <> 0 then c else
      compare_compilation t1.compiled t2.compiled

  let is_empty _ = false

  let nullable _ = false

  let get_label _ = Label.empty

  let make_one compiled node =
    if IndexSet.disjoint compiled.domain node.Redgraph.next_goto_closure
    then Reg.Expr.empty
    else Reg.Expr.abstract { node; compiled }

  let make compiled states =
    Reg.Expr.disjunction (
      IndexSet.fold (fun lr1 acc ->
          make_one compiled (Vector.get Redgraph.roots lr1) :: acc
        ) states []
    )

  let left_classes t f acc =
    List.fold_left
      (fun acc (parent : Redgraph.node) ->
         f (Sigma.Pos (IndexSet.singleton parent.lr1)) acc)
      acc t.node.next

  let empty_delta = (Label.empty, Reg.Expr.empty)

  let left_delta (t : t) = function
    | Sigma.Pos s when IndexSet.is_singleton s ->
      let lr1 = IndexSet.choose s in
      let node_with_state lr1 node = lr1 = node.Redgraph.lr1 in
      begin match List.find_opt (node_with_state lr1) t.node.next with
        | None -> empty_delta
        | Some node' ->
          let lbls, res =
            IndexSet.fold (fun lr1 (lbls, res as acc) ->
                match Lr1Map.find_opt lr1 t.compiled.derivations with
                | None -> acc
                | Some (lbl, re) -> (lbl :: lbls, re :: res)
              ) node'.goto ([], [])
          in
          (List.fold_left Label.append Label.empty lbls,
           Reg.Expr.disjunction (make_one t.compiled node' :: res))
      end
    | _ -> empty_delta

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
    Reg.Expr.(|.) expr (Redgraph_derivation.make compiled all_states)

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
  Reg.Expr.(^.) pattern label (*(pattern ^. label)*)

let translate_entry {Syntax. startsymbols; error; name; args; clauses} =
  (* TODO *)
  ignore (startsymbols, error, name, args);
  let clauses = List.mapi translate_clause clauses in
  clauses

(* Derive DFA with naive intersection *)

module DFA = struct
  let lr1_predecessors = Vector.init Lr1C.n (fun lr1 ->
      List.fold_left
        (fun acc tr -> IndexSet.add (Transition.source tr) acc)
        (IndexSet.empty)
        (Transition.predecessors lr1)
    )

  let lr1set_predecessors lr1s =
    lr1set_bind lr1s (fun lr1 -> Vector.get lr1_predecessors lr1)

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
      let state = {
        index; expr;
        unvisited = Reg.Expr.left_classes expr (fun sg sgs -> sg :: sgs) [];
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
        schedule state' inter
    in
    let discover_transition sigma state sigma' =
      let inter = Sigma.inter sigma sigma' in
      if Sigma.is_empty inter then
        Either.Left sigma'
      else (
        let _, expr' = Reg.Expr.left_delta state.expr sigma' in
        let state' = match Reg.Map.find_opt expr' !dfa with
          | None -> make_state expr'
          | Some state' -> state'
        in
        schedule state' inter;
        Either.Right (sigma', state')
      )
    in
    let process state =
      state.visited <- Sigma.union state.visited state.scheduled;
      let sigma = sigma_predecessors state.scheduled in
      state.scheduled <- Sigma.empty;
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
    (!dfa, initial)
end

let cmon_re re =
  Mulet.cmon_re re
    ~set:Sigma.cmon ~label:(fun _ -> Cmon.unit)
    ~abstract:(fun _ -> Cmon.unit)

let () =
  let entry = List.hd lexer_definition.entrypoints in
  Format.printf "%a\n%!" Cmon.format (Syntax.print_entrypoints entry);
  let clauses = translate_entry entry in
  let re = Reg.Expr.disjunction clauses in
  let cmon = Cmon.list_map cmon_re clauses in
  Format.printf "%a\n%!" Cmon.format cmon;
  let dfa, _initial = DFA.translate re in
  let count = Reg.Map.cardinal dfa in
  prerr_endline (string_of_int count ^ " states")
