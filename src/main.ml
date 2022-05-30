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
      | Syntax.Reduce _ ->
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
