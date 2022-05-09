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

(* The lexer generator. Command-line parsing. *)

let run_test = false
let verbose = false

let source_name = ref None
let output_name = ref None
let grammar_file = ref None

let usage =
  Printf.sprintf
    "lrgrep, a menhir lexer\n\
     usage: %s [options] <source>"
    Sys.argv.(0)

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

let source_file = match !source_name with
  | None ->
    Format.eprintf "No source provided, stopping now.\n";
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

(* General purpose definitions *)

open Fix.Indexing

module IndexSet = BitSet.IndexSet

type 'a indexset = 'a IndexSet.t
type ('n, 'a) indexmap = ('n, 'a) IndexMap.t

let array_cons arr index value =
  arr.(index) <- value :: arr.(index)

let index_fold v a f =
  let a = ref a in
  Index.iter v (fun i -> a := f i !a);
  !a

let indexset_bind : 'a indexset -> ('a index -> 'b indexset) -> 'b indexset =
  fun s f ->
  IndexSet.fold (fun lr1 acc -> IndexSet.union acc (f lr1)) s IndexSet.empty

let vector_set_add vec index value =
  Vector.set vec index (IndexSet.add value (Vector.get vec index))

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

(* Grammar representation *)

module Grammar = MenhirSdk.Cmly_read.Read (struct
    let filename = match !grammar_file with
      | Some filename -> filename
      | None ->
        Format.eprintf "No grammar provided (-g), stopping now.\n";
        Arg.usage specs usage;
        exit 1
  end)

module Terminal = struct
  include Grammar.Terminal
  type raw = t

  include (val const count)
  type t = n index
  type set = n indexset
  let of_g x = Index.of_int n (to_int x)
  let to_g x = of_int (Index.to_int x)

  let all =
    let acc = ref IndexSet.empty in
    for i = cardinal n - 1 downto 0
    do acc := IndexSet.add (Index.of_int n i) !acc done;
    !acc
  let name t = name (to_g t)
end

module Nonterminal = struct
  include Grammar.Nonterminal
  type raw = t

  include (val const count)
  type t = n index
  type set = n indexset
  let of_g x = Index.of_int n (to_int x)
  let to_g x = of_int (Index.to_int x)

  let all =
    let acc = ref IndexSet.empty in
    for i = cardinal n - 1 downto 0
    do acc := IndexSet.add (Index.of_int n i) !acc done;
    !acc
end

module Symbol = struct
  type t =
    | T of Terminal.t
    | N of Nonterminal.t

  let of_g = function
    | Grammar.T t -> T (Terminal.of_g t)
    | Grammar.N n -> N (Nonterminal.of_g n)

  let to_g = function
    | T t -> Grammar.T (Terminal.to_g t)
    | N n -> Grammar.N (Nonterminal.to_g n)

  let name ?mangled = function
    | T t -> Grammar.symbol_name ?mangled (T (Terminal.to_g t))
    | N n -> Grammar.symbol_name ?mangled (N (Nonterminal.to_g n))
end

module Production = struct
  include Grammar.Production
  type raw = t

  include (val const count)
  type t = n index
  type set = n indexset
  let of_g x = Index.of_int n (to_int x)
  let to_g x = of_int (Index.to_int x)

  let lhs p = Nonterminal.of_g (lhs (to_g p))
  let rhs =
    let import prod =
      Array.map (fun (sym,_,_) -> Symbol.of_g sym) (rhs (to_g prod))
    in
    Vector.get (Vector.init n import)
  let kind p = kind (to_g p)
end


module Lr1 = struct
  include Grammar.Lr1
  type raw = t

  include (val const count)
  type t = n index
  type set = n indexset
  type 'a map = (n, 'a) indexmap
  let of_g lr1 = Index.of_int n (to_int lr1)
  let to_g lr1 = of_int (Index.to_int lr1)

  let all =
    let acc = ref IndexSet.empty in
    for i = cardinal n - 1 downto 0
    do acc := IndexSet.add (Index.of_int n i) !acc done;
    !acc

  let to_lr0 lr1 = lr0 (to_g lr1)

  let incoming lr1 =
    Option.map Symbol.of_g (Grammar.Lr0.incoming (to_lr0 lr1))

  let items lr1 =
    List.map
      (fun (p,pos) -> (Production.of_g p, pos))
      (Grammar.Lr0.items (to_lr0 lr1))

  let reductions =
    let import_red reds =
      reds
      |> List.map
        (fun (t, ps) -> (Production.of_g (List.hd ps), Terminal.of_g t))
      |> Misc.group_by
        ~compare:(fun (p1,_) (p2,_) -> compare_index p1 p2)
        ~group:(fun (p,t) ps -> p, IndexSet.of_list (t :: List.map snd ps))
    in
    let import_lr1 lr1 = import_red (reductions (to_g lr1)) in
    Vector.get (Vector.init n import_lr1)
end

(*module LRijkstra =
  LRijkstraFast.Make(Grammar)(TerminalSet)(Lr1)
    (struct let all_terminals = all_terminals end)
    ()*)

(* State indices *)

module State_indices = struct

  (* Precompute states associated to symbols *)

  let states_of_terminals =
    Vector.make Terminal.n IndexSet.empty

  let states_of_nonterminals =
    Vector.make Nonterminal.n IndexSet.empty

  let () =
    Index.iter Lr1.n (fun lr1 ->
        match Lr1.incoming lr1 with
        | None -> ()
        | Some (Symbol.T t) -> vector_set_add states_of_terminals t lr1
        | Some (Symbol.N n) -> vector_set_add states_of_nonterminals n lr1
      )

  let states_of_symbol = function
    | Symbol.T t -> Vector.get states_of_terminals t
    | Symbol.N n -> Vector.get states_of_nonterminals n

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
    let add_symbol s = Hashtbl.add table (Symbol.name ~mangled:false s) s in
    Index.iter Terminal.n (fun t -> add_symbol (Symbol.T t));
    Index.iter Nonterminal.n (fun n -> add_symbol (Symbol.N n));
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
  val find_goto : Lr1.t -> Nonterminal.t -> goto index
  val find_goto_target : Lr1.t -> Nonterminal.t -> Lr1.t

  (* Get the source state of a transition *)
  val source : any index -> Lr1.t

  (* Get the target state of a transition *)
  val target : any index -> Lr1.t

  (* Symbol that labels a transition *)
  val symbol : any index -> Symbol.t

  (* Symbol that labels a goto transition *)
  val goto_symbol : goto index -> Nonterminal.t

  (* Symbol that labels a shift transition *)
  val shift_symbol : shift index -> Terminal.t

  (* [successors s] returns all the transitions [tr] such that
     [source tr = s] *)
  val successors : Lr1.t -> any index list

  (* [predecessors s] returns all the transitions [tr] such that
     [target tr = s] *)
  val predecessors : Lr1.t -> any index list
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
        | Grammar.T _t ->
          (*if Terminal.real t then*)
          ignore (Shift.fresh ())
        | Grammar.N _ ->
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

  let sources = Vector.make' any (fun () -> Index.of_int Lr1.n 0)
  let targets = Vector.make' any (fun () -> Index.of_int Lr1.n 0)

  let t_symbols = Vector.make' shift (fun () -> Index.of_int Terminal.n 0)
  let nt_symbols = Vector.make' goto (fun () -> Index.of_int Nonterminal.n 0)

  (* Hash tables to associate information to the pair of
     a transition and a symbol.
  *)

  let nt_table = Hashtbl.create 7

  let nt_pack lr1 goto =
    (* Custom function to key into nt_table: compute a unique integer from
       an lr1 state and a non-terminal. *)
    Index.to_int lr1 * Nonterminal.count + Index.to_int goto

  let t_table = Hashtbl.create 7

  let t_pack lr1 t =
    (* Custom function to key into t_table: compute a unique integer from
       an lr1 state and a terminal. *)
    Index.to_int lr1 * Terminal.count + Index.to_int t

  (* A vector to store the predecessors of an lr1 state.
     We cannot compute them directly, we discover them by exploring the
     successor relation below. *)
  let predecessors = Vector.make Lr1.n []

  let successors =
    (* We populate all the data structures allocated above, i.e.
       the vectors t_sources, t_symbols, t_targets, nt_sources, nt_symbols,
       nt_targets and predecessors, as well as the tables t_table and
       nt_table, by iterating over all successors. *)
    let next_goto = Index.enumerate goto in
    let next_shift = Index.enumerate shift in
    Vector.init Lr1.n begin fun source ->
      List.fold_left begin fun acc (sym, target) ->
        let target = Lr1.of_g target in
        let index = match sym with
          | Grammar.T t ->
            let t = Terminal.of_g t in
            let index = next_shift () in
            Vector.set t_symbols index t;
            Hashtbl.add t_table (t_pack source t) index;
            of_shift index
          | Grammar.N nt ->
            let nt = Nonterminal.of_g nt in
            let index = next_goto () in
            Vector.set nt_symbols index nt;
            Hashtbl.add nt_table (nt_pack source nt) index;
            of_goto index
        in
        Vector.set sources index source;
        Vector.set targets index target;
        Vector.set_cons predecessors target index;
        index :: acc
      end [] (Lr1.transitions (Lr1.to_g source))
    end

  let successors lr1 = Vector.get successors lr1
  let predecessors lr1 = Vector.get predecessors lr1

  let find_goto source nt = Hashtbl.find nt_table (nt_pack source nt)

  let source i = Vector.get sources i

  let symbol i =
    match split i with
    | L i -> Symbol.N (Vector.get nt_symbols i)
    | R i -> Symbol.T (Vector.get t_symbols i)

  let goto_symbol i = Vector.get nt_symbols i
  let shift_symbol i = Vector.get t_symbols i

  let target i = Vector.get targets i

  let find_goto_target source nt =
    target (of_goto (find_goto source nt))
end

let lr1_predecessors = Vector.init Lr1.n (fun lr1 ->
    List.fold_left
      (fun acc tr -> IndexSet.add (Transition.source tr) acc)
      IndexSet.empty
      (Transition.predecessors lr1)
  )

let lr1set_predecessors lr1s =
  indexset_bind lr1s (Vector.get lr1_predecessors)

module Match_item = struct
  let maybe_has_lhs prod = function
    | None -> true
    | Some lhs -> lhs = Production.lhs prod

  let maybe_match_sym sym = function
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
    index_fold Lr1.n IndexSet.empty (fun lr1 acc ->
        if List.exists
            (item_match lhs prefix' suffix')
            (Lr1.items lr1)
        then IndexSet.add lr1 acc
        else acc
      )
end

(* Implementation of simulation reduction *)

module Redgraph = struct
  let reductions = Vector.init Lr1.n (fun lr1 ->
      let prepare_goto p =
        (Array.length (Production.rhs p), Production.lhs p)
      in
      let order (d1, n1) (d2, n2) =
        match Int.compare d1 d2 with
        | 0 -> compare_index n1 n2
        | c -> c
      in
      let productions =
        Lr1.reductions lr1
        |> List.filter_map (fun (p, _) ->
            match Production.kind p with
            | `REGULAR -> Some (prepare_goto p)
            | `START -> None
          )
        |> List.sort_uniq order
      in
      let depth = List.fold_left (fun x (d, _) -> max x d) (-1) productions in
      let vector = Array.make (depth + 1) [] in
      List.iter (fun (d,n) -> array_cons vector d n) productions;
      assert (depth = -1 || vector.(depth) <> []);
      vector
    )

  type abstract_stack = {
    states: Lr1.set;
    mutable goto_nt: Nonterminal.set;
    mutable goto_closure: Lr1.set Lr1.map;
    parent: abstract_stack lazy_t;
  }

  type concrete_stack = {
    state: Lr1.t;
    mutable goto: concrete_stack Lr1.map;
    parent: stack;
  }

  and stack =
    | Concrete of concrete_stack
    | Abstract of abstract_stack lazy_t

  let strong_pop = function
    | Concrete t -> t.parent
    | Abstract (lazy t) -> Abstract t.parent

  let pop_abstract (t : abstract_stack) =
      if Lazy.is_val t.parent
      then Some (Lazy.force_val t.parent)
      else None

  let rec goto stack nt =
    match stack with
    | Abstract (lazy t) ->
      t.goto_nt <- IndexSet.add nt t.goto_nt
    | Concrete t ->
      let state = Transition.find_goto_target t.state nt in
      if not (IndexMap.mem state t.goto) then (
        let target = {state; goto=IndexMap.empty; parent=stack} in
        t.goto <- IndexMap.add state target t.goto;
        populate target
      )

  and populate state =
    let _ : stack =
      Array.fold_left (fun stack nts ->
          List.iter (goto stack) nts;
          strong_pop stack
        ) (Concrete state) (Vector.get reductions state.state)
    in
    ()

  let rec make_abstract states = lazy (
    let states = lr1set_predecessors states in
    { states;
      goto_nt = IndexSet.empty;
      goto_closure = IndexMap.empty;
      parent = make_abstract states }
  )

  let get_root =
    let make_root state =
      let state = {
        state;
        goto = IndexMap.empty;
        parent = Abstract (make_abstract (IndexSet.singleton state))
      } in
      populate state;
      state
    in
    Vector.get (Vector.init Lr1.n make_root)

  let get_root_parent lr1 =
    match (get_root lr1).parent with
    | Concrete _ -> assert false
    | Abstract t ->
      if Lazy.is_val t
      then Some (Lazy.force_val t)
      else None

  let goto_nts =
    let goto_nts lr1 =
      match get_root_parent lr1 with
      | None -> IndexSet.empty
      | Some t -> t.goto_nt
    in
    let vec = Vector.init Lr1.n goto_nts in
    Vector.get vec

  let close abs =
    let close_st st =
      let visited = ref IndexSet.empty in
      let rec visit_nt nt =
        let st' =
          try Transition.find_goto_target st nt
          with Not_found -> assert false
        in
        if not (IndexSet.mem st' !visited) then (
          visited := IndexSet.add st' !visited;
          visit_nts (goto_nts st')
        )
      and visit_nts nts =
        IndexSet.iter visit_nt nts
      in
      visit_nts abs.goto_nt;
      !visited
    in
    let add_st st acc = IndexMap.add st (close_st st) acc in
    if not (IndexSet.is_empty abs.goto_nt) then
      abs.goto_closure <- IndexSet.fold add_st abs.states IndexMap.empty

  let rec close_all abs =
      close abs;
      if Lazy.is_val abs.parent then
        close_all (Lazy.force abs.parent)

  let () =
    Index.iter Lr1.n
      (fun lr1 -> Option.iter close_all (get_root_parent lr1))
end

module Kreduce = struct
  let uid = ref 0

  type 'a compiled = {

  }

  type 'a t = {
    uid: int;

  }


end

(* KRE, first intermediate language *)

module KRE = struct
  type uid = int

  type desc =
    | KDone of {priority: int; clause: Syntax.clause}
    | KAtom of Lr1.set * Syntax.atom * t
    | KOr of t * t
    | KStar of t lazy_t * t
    | KReduce of t

  and t = {
    desc: desc;
    mutable hash: int;
  }

  let rec compare_until d1 d2 k1 k2 =
    if (k1 == k2) || (k1 == d1 && k2 == d2) then 0
    else
      let c = Int.compare k1.hash k2.hash in
      if c <> 0 then c else
        match k1.desc, k2.desc with
        | KDone c1, KDone c2 ->
          Int.compare c1.priority c2.priority
        | KAtom (s1, a1, k1), KAtom (s2, a2, k2) ->
          let c = IndexSet.compare s1 s2 in
          if c <> 0 then c else
            let c = compare a1 a2 in
            if c <> 0 then c else
              compare_until d1 d2 k1 k2
        | KOr (k1a, k1b), KOr (k2a, k2b) ->
          let c = compare_until d1 d2 k1a k2a in
          if c <> 0 then c else
            compare_until d1 d2 k1b k2b
        | KStar (lazy k1', d1'), KStar (lazy k2', d2') ->
          let c = compare_until d1' d2' k1' k2' in
          if c <> 0 then c else
            compare_until d1 d2 d1' d2'
        | KReduce k1, KReduce k2 ->
          compare_until d1 d2 k1 k2
        | KDone _   , (KAtom _ | KOr _ | KStar _ | KReduce _)
        | KAtom _   , (KOr _ | KStar _ | KReduce _)
        | KOr _     , (KStar _ | KReduce _)
        | KStar _   , (KReduce _)
          -> -1
        | KReduce _ , (KStar _ | KOr _ | KAtom _ | KDone _)
        | KStar _   , (KOr _ | KAtom _ | KDone _)
        | KOr _     , (KAtom _ | KDone _)
        | KAtom _   , (KDone _)
          -> 1

  let make desc =
    let hash = match desc with
      | KDone _         -> Hashtbl.hash desc
      | KAtom (s, a, k) -> Hashtbl.seeded_hash k.hash (s, a)
      | KOr (k1, k2)    -> Hashtbl.seeded_hash k1.hash k2.hash
      | KStar (_, k')   -> Hashtbl.seeded_hash 42 k'.hash
      | KReduce k'      -> Hashtbl.seeded_hash 51 k'.hash
    in
    {desc; hash}

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

  let translate_expr =
    let translate_atom_desc = function
      | Syntax.Symbol name ->
        let symbol = translate_symbol name in
        State_indices.states_of_symbol symbol
      | Syntax.Item {lhs; prefix; suffix} ->
        let lhs = Option.map translate_nonterminal lhs in
        let prefix = translate_producers prefix in
        let suffix = translate_producers suffix in
        Match_item.states_by_items ~lhs ~prefix ~suffix
      | Syntax.Wildcard ->
        Lr1.all
    in
    let translate_atom k (atom: Syntax.atom) =
      KAtom (translate_atom_desc atom.desc, atom, k)
    in
    let rec translate_term k = function
      | Syntax.Atom atom ->
        make (translate_atom k atom)
      | Syntax.Alternative (e1, e2) ->
        make (KOr (translate_expr k e1, translate_expr k e2))
      | Syntax.Repetition (e1, _) ->
        let rec result = lazy (make (KStar (jump, k)))
        and jump = lazy (translate_expr (Lazy.force result) e1) in
        let (lazy result, lazy jump) = result, jump in
        result.hash <- Hashtbl.seeded_hash 69 jump.hash;
        result
      | Syntax.Reduce (expr, _) ->
        let k = translate_expr k expr in
        make (KOr (k, make (KReduce k)))

      and translate_expr k = function
        | [] -> k
        | (x, _) :: xs ->
          let k = translate_expr k xs in
          translate_term k x
    in
    translate_expr

  let translate_clause priority clause =
    translate_expr (make (KDone {priority; clause})) clause.Syntax.pattern

  let translate_entry {Syntax. startsymbols; error; name; args; clauses} =
    ignore (startsymbols, error, name, args);
    List.mapi translate_clause clauses
end

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
      type t = Lr1.set * Lr1.set * (Clause.capture list * Sigma.t) list

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
    fun oc ->
    (* Print matching functions *)
    let print fmt = Printf.fprintf oc fmt in
    let sprint fmt = Printf.sprintf fmt in
    print
      "module Table : sig\n\
      \  open Analyser_def\n\
      \  type state\n\
      \  val initial : state\n\
      \  val step : state -> int -> \n\
      \    register list * clause option * state option\n\
       end = struct\n\
      \  [@@@ocaml.warning \"-27\"]\n\
      \  type state = int\n\
      \  let table = [|\n\
      ";
    let visit (index : Min.states index) =
      print "    (fun st ->";
      let action = match Label.Action.priority (get_label index).action with
        | None -> "None"
        | Some priority -> sprint "Some %d" priority
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
            sprint "(%d,%d)" c.Clause.for_clause.priority var
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
        print " ([], %s, None));\n" action
      | [], [_, cs, st] ->
        print " (%s, %s, Some %d));\n"
          (print_captures cs) action (st :> int)
      | _ ->
        print "\n    match st with\n";
        List.iter begin fun (lr1s, cs, st) ->
          print "    | %s -> (%s, %s, Some %d)\n"
            (print_states lr1s)
            (print_captures cs)
            action
            (st : _ index :> int)
        end pos;
        begin match neg with
          | [] -> print "    | _ -> ([], %s, None)" action
          | [_, cs, st] ->
            print "    | _ -> (%s, %s, Some %d)"
              (print_captures cs) action (st :> int)
          | _ :: _ :: _ -> assert false
        end;
        print ");\n";
    in
    Index.iter Min.states visit;
    print
      "  |]\n\
      \  let initial = %d\n\
      \  let step st lr1 = table.(st) lr1\n\
       end\n"
      (Min.initials.(0) :> int)
end

let () = (
  let entry = List.hd lexer_definition.entrypoints in
  if verbose then
    Format.eprintf "%a\n%!" Cmon.format (Syntax.print_entrypoints entry);
  let clauses, re = translate_entry entry in
  (*Format.eprintf "Starting from @[%a@]\n%!" Cmon.format (cmon_re re);*)
  if run_test then ignore (interpret re test_stack : Reg.Expr.t);
  (*Format.printf "%a\n%!" Cmon.format (cmon_re re);*)
  let (Large_dfa {set=(module Set); _} as dfa) = DFA.translate re in
  prerr_endline (string_of_int (cardinal Set.n) ^ " states");
  begin match !output_name with
    | None ->
      prerr_endline "No output file provided (option -o). Giving up.";
      exit 1
    | Some path ->
      let oc = open_out_bin path in
      output_string oc (snd lexer_definition.header);
      output_char oc '\n';
      let gen_table = DFA.minimize dfa in
      Clause.gencode oc clauses;
      output_char oc '\n';
      output_string oc (snd lexer_definition.trailer);
      output_char oc '\n';
      gen_table oc;
      close_out oc
  end
)
