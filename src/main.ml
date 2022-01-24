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

module Grammar = MenhirSdk.Cmly_read.Read (struct
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

let initial_states : (Grammar.nonterminal * Grammar.lr1) list =
  Grammar.Lr1.fold begin fun lr1 acc ->
    let lr0 = Grammar.Lr1.lr0 lr1 in
    match Grammar.Lr0.incoming lr0 with
    | Some _ -> acc
    | None ->
      (* Initial state *)
      Format.eprintf "Initial state %d\n%a\n"
        (Grammar.Lr1.to_int lr1)
        Grammar.Print.itemset (Grammar.Lr0.items lr0);
      let (prod, _) = List.hd (Grammar.Lr0.items lr0) in
      let nt = match (Grammar.Production.rhs prod).(0) with
        | Grammar.N nt, _, _ -> nt
        | _ -> assert false
      in
      (nt, lr1) :: acc
  end []

open Fix.Indexing
open Grammar

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

module Lr1Set = BitSet.Make(struct
    type t = Lr1C.n index
    let of_int i = Index.of_int Lr1C.n i
  end)

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
        (fun set tr -> Lr1Set.add (Transition.source tr) set)
        Lr1Set.empty transitions
    )

module Redgraph =
struct
  type parent_state = {
    lr1: Lr1C.n index;
    mutable goto: Lr1C.n index list;
    mutable parents: parent_state list;
  }

  type state =
    | Goto of {lr1: Lr1C.n index; parent: state}
    | Lr1 of {lr1: Lr1C.n index; parents: parent_state list ref}

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
    | Concrete of state
    | Abstract of parent_state list

  let parent_states lr1 =
    List.map
      (fun tr -> {lr1 = Transition.source tr; goto = []; parents = []})
      (Transition.predecessors lr1)

  let pop = function
    | Concrete (Goto {parent; _}) -> Concrete parent
    | Concrete (Lr1 {lr1; parents}) ->
      if !parents = [] then
        parents := parent_states lr1;
      Abstract !parents
    | Abstract states ->
      Abstract (
        List.fold_left (fun states state ->
            if state.parents = [] then
              state.parents <- parent_states state.lr1;
            state.parents @ states
          ) [] states
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
          | Some st -> state.goto <- st :: state.goto
        ) states;
      acc
    | Concrete st ->
      begin match goto_target (lr1 st) nt with
        | None -> acc
        | Some tgt -> Goto {lr1=tgt; parent=st} :: acc
      end

  let transitions state =
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
    let new_transitions = transitions state in
    let acc = new_transitions @ acc in
    List.fold_left close_transitions acc new_transitions

  module Derivations = struct
    include Gensym()

    type node = {
      index: n index;
      state: Lr1C.n index;
      mutable derivations: node list;
    }

    let table = Hashtbl.create 7

    let roots = ref []

    let rec register state =
      match Hashtbl.find_opt table state with
      | Some node -> node
      | None ->
        let node =
          match state with
          | Lr1 {lr1=state; _} ->
            let node = { index = fresh (); state; derivations = [] } in
            roots := node :: !roots;
            node
          | Goto {lr1=state; parent} ->
            let node' = register parent in
            let node = { index = fresh (); state; derivations = [] } in
            node'.derivations <- node :: node'.derivations;
            node
        in
        Hashtbl.add table state node;
        node

    let register state = (register state).index

    let freeze () =
      let vector = Vector.make' n (fun () -> List.hd !roots) in
      let set_node _ node = Vector.set vector node.index node in
      Hashtbl.iter set_node table;
      vector

    let derive
        ~(root : 'a)
        ~(step : 'a -> Lr1C.n index -> 'a)
      =
      let vector = Vector.make n root in
      let rec init_node derived node =
        let derived = step derived node.state in
        Vector.set vector node.index derived;
        List.iter (init_node derived) node.derivations
      in
      List.iter (init_node root) !roots;
      vector
  end

  let transitions =
    Vector.init Lr1C.n (fun lr1 ->
        let rs = ref [] in
        let vs = close_transitions [] (Lr1 {lr1; parents=rs})  in
        let compare_index =
          (Int.compare
           : int -> int -> int
           :> _ index -> _ index -> int)
        in
        let vs = List.map Derivations.register vs in
        (!rs, List.sort_uniq compare_index vs)
      )

  let derivations = Derivations.freeze ()

  let derive = Derivations.derive

  let () =
    print_endline "digraph G {";
    Index.iter Lr1C.n (fun src ->
        let targets, _derivations = Vector.get transitions src in
        Printf.printf "  ST%d[fontname=Mono,shape=box,label=%S]\n"
          (src :> int)
          (Format.asprintf "%d\n%a" (src :> int)
             Print.itemset (Lr0.items (Lr1.lr0 (Lr1C.to_g src))));
        let tgt_table = Hashtbl.create 7 in
        let rec visit_target pst =
          List.iter (fun tgt ->
              let lbl = string_of_int (pst.lr1 :> int) in
              match Hashtbl.find tgt_table tgt with
              | exception Not_found ->
                Hashtbl.add tgt_table tgt (ref [lbl])
              | lst -> lst := lbl :: !lst
            ) pst.goto;
          List.iter visit_target pst.parents
        in
        List.iter visit_target targets;
        Hashtbl.iter (fun tgt srcs ->
            Printf.printf "  ST%d -> ST%d [label=%S]\n"
              (src :> int) (tgt : _ index :> int)
              (String.concat "|" !srcs)
          ) tgt_table
      );
    print_endline "}";

end

module Sigma : sig
  (** The set of states is represented either as positive occurrences (all
      states that are contained) or negative occurrences (all states that are
      not contained).

      This makes complement a cheap operation.  *)
  type t =
    | Pos of Lr1Set.t
    | Neg of Lr1Set.t

  val singleton : Lr1.t -> t
  val to_lr1set : t -> Lr1Set.t

  include Mulet.SIGMA with type t := t

  val union : t -> t -> t
  (** Compute union of two sets *)

  val intersect : t -> t -> bool
  (** Check if two sets intersect *)

  val mem : Lr1.t -> t -> bool
  (** [mem lr1 t] checks if the state [lr1] is an element of a sigma set [t] *)
end = struct
  type t =
    | Pos of Lr1Set.t
    | Neg of Lr1Set.t

  let empty = Pos Lr1Set.empty
  let full = Neg Lr1Set.empty
  let compl = function Pos x -> Neg x | Neg x -> Pos x
  let is_empty = function Pos x -> Lr1Set.is_empty x | Neg _ -> false
  let is_full = function Neg x -> Lr1Set.is_empty x | Pos _ -> false

  let singleton lr1 = Pos (Lr1Set.singleton lr1)

  let to_lr1set = function
    | Pos xs -> xs
    | Neg xs -> Lr1Set.diff Lr1.all_states xs

  let is_subset_of x1 x2 =
    match x1, x2 with
    | Pos x1, Pos x2 -> Lr1.Set.subset x1 x2
    | Neg x1, Neg x2 -> Lr1.Set.subset x2 x1
    | Pos x1, Neg x2 -> Lr1.Set.disjoint x1 x2
    | Neg _ , Pos _ -> false

  let inter x1 x2 =
    match x1, x2 with
    | Pos x1, Pos x2 -> Pos (Lr1.Set.inter x1 x2)
    | Neg x1, Neg x2 -> Neg (Lr1.Set.union x1 x2)
    | (Pos x1, Neg x2) | (Neg x2, Pos x1) ->
      Pos (Lr1.Set.diff x1 x2)

  let intersect x1 x2 =
    match x1, x2 with
    | Pos x1, Pos x2 -> not (Lr1.Set.disjoint x1 x2)
    | Neg _, Neg _ -> true
    | Pos x1, Neg x2 | Neg x2, Pos x1 ->
      not (Lr1.Set.is_empty (Lr1.Set.diff x1 x2))

  let compare x1 x2 =
    match x1, x2 with
    | Pos x1, Pos x2 -> Lr1.Set.compare x1 x2
    | Neg x1, Neg x2 -> Lr1.Set.compare x2 x1
    | Pos _ , Neg _ -> -1
    | Neg _ , Pos _ -> 1

  let union x1 x2 =
    match x1, x2 with
    | Neg x1, Neg x2 -> Neg (Lr1.Set.inter x1 x2)
    | Pos x1, Pos x2 -> Pos (Lr1.Set.union x1 x2)
    | Pos x1, Neg x2 | Neg x2, Pos x1 ->
      Neg (Lr1.Set.diff x2 x1)

  let partition l =
    let only_pos = ref true in
    let project = function Pos x -> x | Neg x -> only_pos := false; x in
    let l = List.map project l in
    let pos x = Pos x in
    try
      if !only_pos
      then List.map pos (Lr1.partition l)
      else
        let parts, total = Lr1.partition_and_total l in
        Neg total :: List.map pos parts
    with exn ->
      Printf.eprintf
        "Partition failed with %d inputs (strictly positive: %b):\n"
        (List.length l) !only_pos;
      List.iter (fun set ->
          Printf.eprintf "- cardinal=%d, set={" (Lr1.Set.cardinal set);
          Lr1.Set.iter (fun elt -> Printf.eprintf "%d," (elt :> int)) set;
        ) l;
      raise exn

  let mem x = function
    | Pos xs -> Lr1.Set.mem x xs
    | Neg xs -> not (Lr1.Set.mem x xs)
end


module Re :
module Derivable_red = struct
  let from_re re =
    Redgraph.derive
      ~root:re
      ~step(fun re state -> Mulet.Make


end

(*module Unreduce : sig

  type t = {
    (* The production that is being reduced *)
    production: Production.t;

    (* The set of lookahead terminals that allow this reduction to happen *)
    lookahead: TerminalSet.t;

    (* The shape of the stack, all the transitions that are replaced by the
       goto transition when the reduction is performed *)
    steps: Transition.any index list;

    (* The lr1 state at the top of the stack before reducing.
       That is [state] can reduce [production] when the lookahead terminal
       is in [lookahead]. *)
    state: Lr1C.n index;
  }

  (* [goto_transition tr] lists all the reductions that ends up
     following [tr]. *)
  val goto_transition: Transition.goto index -> t list
end = struct

  type t = {
    production: Production.t;
    lookahead: TerminalSet.t;
    steps: Transition.any index list;
    state: Lr1C.n index;
  }

  let table = Vector.make Transition.goto []

  (* [add_reduction lr1 (production, lookahead)] populates [table] by
     simulating the reduction [production], starting from [lr1] when
     lookahead is in [lookahead] *)
  let add_reduction lr1 (production, lookahead) =
    if not (Production.kind production = `START) then begin
      let lhs = Production.lhs production in
      let rhs = Production.rhs production in
      let states =
        Array.fold_right (fun _ states ->
            let expand acc (state, steps) =
              List.fold_left (fun acc tr ->
                  (Transition.source tr, tr :: steps) :: acc
                ) acc (Transition.predecessors state)
            in
            List.fold_left expand [] states
          ) rhs [lr1, []]
      in
      List.iter (fun (source, steps) ->
          Vector.set_cons table (Transition.find_goto source lhs)
            { production; lookahead; steps; state=lr1 }
        ) states
    end

  let has_default_reduction lr1 =
    match Lr1.transitions lr1 with
    | _ :: _ -> None
    | [] ->
      match Lr1.reductions lr1 with
      | [] -> None
      | (_, [p]) :: ps when List.for_all (fun (_, p') -> p' = [p]) ps ->
        Some p
      | _ -> None

  (* [get_reductions lr1] returns the list of productions and the lookahead
     sets that allow reducing them from state [lr1] *)
  let get_reductions lr1 =
    let lr1 = Lr1C.to_g lr1 in
    match has_default_reduction lr1 with
    | Some prod ->
      (* State has a default reduction, the lookahead can be any terminal *)
      [prod, all_terminals]
    | None ->
      let raw =
        let add acc (t, ps) = (t, List.hd ps) :: acc in
        List.fold_left add [] (Lr1.reductions lr1)
      in
      (* Regroup lookahead tokens by production *)
      Utils.Misc.group_by raw
        ~compare:(fun (_, p1) (_, p2) ->
            Int.compare (Production.to_int p1) (Production.to_int p2)
          )
        ~group:(fun (t, p) tps ->
            let set = List.fold_left
                (fun set (t, _) -> TerminalSet.add t set)
                (TerminalSet.singleton t) tps
            in
            (p, set)
          )

  let () =
    (* Populate [table] with the reductions of all state *)
    Index.iter Lr1C.n
      (fun lr1 -> List.iter (add_reduction lr1) (get_reductions lr1))

  let goto_transition tr = Vector.get table tr
end

module Sigma = Middle.Sigma.Make(Lr1)
module Regex = Mulet.Make(Sigma)
    (struct
      type t = unit
      let compare () () = 0
      let empty = ()
      let plus () () = ()
    end)*)
