open Utils
module IndexSet = BitSet.IndexSet
type 'a indexset = 'a IndexSet.t
type ('n, 'a) indexmap = ('n, 'a) IndexMap.t

(* The lexer generator. Command-line parsing. *)

let opt_infile = ref None
let opt_parse_intf = ref false
let opt_stack_items = ref false
let opt_no_reductions = ref false
let opt_no_reductions_items = ref false

let usage =
  Printf.sprintf
    "Parser interpreter\n\
     Prints detailed information to help working out error patterns.\n\
     \n\
     Usage: %s [-intf] [-no-items] [-no-reductions] [-all-items] <-|foo.ml|bar.mli>"
    Sys.argv.(0)

let print_version_num () =
  print_endline "0.1";
  exit 0

let print_version_string () =
  print_string "The Menhir parser lexer generator :-], version ";
  print_version_num ()

let specs = [
  "-", Arg.Unit (fun () -> opt_infile := Some "-"),
  " Read input from stdin";
  "-intf", Arg.Set opt_parse_intf,
  " Parse an interface (by default: use extension or parse an implementation)";
  "-no-reductions", Arg.Set opt_no_reductions,
  " Do not simulate reductions";
  "-no-reduction-items", Arg.Set opt_no_reductions_items,
  " Do not print items when simulating reductions";
  "-stack-items", Arg.Set opt_stack_items,
  " Print items of all states on stack";
  "-v",  Arg.Unit print_version_string,
  " Print version and exit";
  "-version",  Arg.Unit print_version_string,
  " Print version and exit";
  "-vnum",  Arg.Unit print_version_num,
  " Print version number and exit";
]

let () = Arg.parse specs (fun name -> opt_infile := Some name) usage

let grammar : MenhirSdk.Cmly_format.grammar =
  let magic_number = "CMLY" ^ MenhirSdk.Version.version in
  let magic_length = String.length magic_number in
  if String.sub Interpreter_data.grammar 0 magic_length <> magic_number then
    failwith "Internal error: grammar has invalid magic number";
  Marshal.from_string Interpreter_data.grammar magic_length

open Fix.Indexing

let cons_option x xs =
  match x with
  | None -> xs
  | Some x -> x :: xs

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

module Terminal = struct
  include (val const (Array.length grammar.g_terminals))
  type t = n index
  let of_int = Index.of_int n
  let count = cardinal n
  let print t = grammar.g_terminals.((t : t :> int)).t_name
end

module Nonterminal = struct
  include (val const (Array.length grammar.g_nonterminals))
  type t = n index
  type set = n indexset
  let of_int = Index.of_int n
  let count = cardinal n
  let print n = grammar.g_nonterminals.((n : t :> int)).n_name
end

type terminal = Terminal.t
type nonterminal = Nonterminal.t

type symbol =
  | T of terminal
  | N of nonterminal

let import_symbol = function
  | MenhirSdk.Cmly_format.N n -> N (Nonterminal.of_int n)
  | MenhirSdk.Cmly_format.T t -> T (Terminal.of_int t)

let symbol_name = function
  | T t -> Terminal.print t
  | N n -> Nonterminal.print n

module TerminalSet = BitSet.Make(Terminal)

let all_terminals =
  let acc = ref TerminalSet.empty in
  for i = Terminal.count - 1 downto 0
  do acc := TerminalSet.add (Terminal.of_int i) !acc done;
  !acc

module Production = struct
  include (val const (Array.length grammar.g_productions))

  let get_prod p = grammar.g_productions.((p : n index :> int))

  let rhs =
    Vector.init n (fun prod ->
        Array.map
          (fun (sym, name, attrs) -> import_symbol sym, name, attrs)
          (get_prod prod).p_rhs
      )
    |> Vector.get

  let lhs prod = Index.of_int Nonterminal.n (get_prod prod).p_lhs

  let kind prod = (get_prod prod).p_kind
end

(* ---------------------------------------------------------------------- *)

module Lr0 = struct
  include (val const (Array.length grammar.g_lr0_states))
  type t = n index
  type set = n indexset
  let incoming lr0 =
    Option.map import_symbol
      grammar.g_lr0_states.((lr0 : t :> int)).lr0_incoming
end

module Lr1 = struct
  include (val const (Array.length grammar.g_lr1_states))
  type t = n index
  type set = n indexset
  let to_lr0 lr1 =
    Index.of_int Lr0.n grammar.g_lr1_states.((lr1 : t :> int)).lr1_lr0

  let transitions =
    Vector.init n (fun lr1 ->
        List.map (fun (sym, target) -> import_symbol sym, Index.of_int n target)
          grammar.g_lr1_states.((lr1 :> int)).lr1_transitions
      )
    |> Vector.get

  let reductions =
    Vector.init n (fun lr1 ->
        List.map (fun (t, prods) ->
            Terminal.of_int t, List.map (Index.of_int Production.n) prods)
          grammar.g_lr1_states.((lr1 :> int)).lr1_reductions
      )
    |> Vector.get

  let items lr1 =
    List.map
      (fun (prod, pos) -> (Index.of_int Production.n prod, pos))
      grammar.g_lr0_states.((to_lr0 lr1 :> int)).lr0_items
end

module Lr1Map = Map.Make(struct
    type t = Lr1.t
    let compare = compare_index
  end)
module IndexRefine = Refine.Make(Utils.BitSet.IndexSet)

let all_states =
  let acc = ref IndexSet.empty in
  for i = (cardinal Lr1.n) - 1 downto 0
  do acc := IndexSet.add (Index.of_int Lr1.n i) !acc done;
  !acc

let indexset_bind : 'a indexset -> ('a index -> 'b indexset) -> 'b indexset =
  fun s f ->
  IndexSet.fold (fun lr1 acc -> IndexSet.union acc (f lr1)) s IndexSet.empty

let print_loc ((loc_start : Lexing.position), (loc_end : Lexing.position)) =
    let sprintf = Printf.sprintf in
    let sline = loc_start.pos_lnum in
    let scol  = loc_start.pos_cnum - loc_start.pos_bol in
    let eline = loc_end.pos_lnum in
    let ecol  = loc_end.pos_cnum - loc_end.pos_bol in
    if sline = eline then
      sprintf "line %d:%d-%d\t" sline scol ecol
    else
      sprintf "from %d:%d to %d:%d\t" sline scol eline ecol

let print_item (prod, pos) =
  let rhs = Production.rhs prod in
  let path = ref [] in
  let add_dot i = if pos = i then path := "." :: !path in
  add_dot (Array.length rhs);
  for i = Array.length rhs - 1 downto 0 do
    let sym, _, _ = rhs.(i) in
    path := symbol_name sym :: !path;
    add_dot i;
  done;
  path := symbol_name (N (Production.lhs prod)) :: "::=" :: !path;
  String.concat " " !path

let print_items lr1 =
  List.map (fun item -> "\t\t  " ^ print_item item) (Lr1.items lr1)

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
  val symbol : any index -> symbol

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
    Index.iter Lr1.n begin fun lr1 ->
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

  let sources = Vector.make' any (fun () -> Index.of_int Lr1.n 0)
  let targets = Vector.make' any (fun () -> Index.of_int Lr1.n 0)

  let t_symbols = Vector.make' shift (fun () -> Terminal.of_int 0)
  let nt_symbols = Vector.make' goto (fun () -> Nonterminal.of_int 0)

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
        match sym with
        (*| T t when not (Terminal.real t) ->
          (* Ignore pseudo-terminals *)
          acc*)
        | _ ->
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
      end [] (Lr1.transitions source)
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
        |> List.filter_map (fun (_, ps) ->
            let prod = List.hd ps in
            match Production.kind prod with
            | `REGULAR -> Some (prepare_goto prod)
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
    mutable goto_closure: Lr1.set Lr1Map.t;
    parent: abstract_stack lazy_t;
  }

  type concrete_stack = {
    state: Lr1.t;
    mutable goto: concrete_stack Lr1Map.t;
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
      if not (Lr1Map.mem state t.goto) then (
        let target = {state; goto=Lr1Map.empty; parent=stack} in
        t.goto <- Lr1Map.add state target t.goto;
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
      goto_closure = Lr1Map.empty;
      parent = make_abstract states }
  )

  let get_root =
    let make_root state =
      let state = {
        state;
        goto = Lr1Map.empty;
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
          with Not_found ->
            Printf.eprintf "goto(#%d,%s): error\nitems:\n%!"
              (st :> int) (Nonterminal.print nt);
            List.iter prerr_endline (print_items st);
            assert false
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
    let add_st st acc = Lr1Map.add st (close_st st) acc in
    if not (IndexSet.is_empty abs.goto_nt) then
      abs.goto_closure <- IndexSet.fold add_st abs.states Lr1Map.empty

  let rec close_all abs =
      close abs;
      if Lazy.is_val abs.parent then
        close_all (Lazy.force abs.parent)

  let () =
    Index.iter Lr1.n
      (fun lr1 -> Option.iter close_all (get_root_parent lr1))
end

let lexbuf =
  let lexbuf = Lexing.from_channel ~with_positions:true stdin in
  Lexing.set_filename lexbuf "<stdin>";
  lexbuf

let get_token =
  let state = Lexer_raw.make Lexer_raw.keyword_table in
  let rec extract_token = function
    | Lexer_raw.Return tok -> tok
    | Lexer_raw.Refill k -> extract_token (k ())
    | Lexer_raw.Fail (err, loc) ->
      Format.eprintf "%a\n%!"
        Location.print_report (Lexer_raw.prepare_error loc err);
      exit 1
  in
  fun lexbuf -> extract_token (Lexer_raw.token_without_comments state lexbuf)

let do_parse
    (type a)
    (checkpoint : Lexing.position -> a Parser_raw.MenhirInterpreter.checkpoint)
  =
  let module I = Parser_raw.MenhirInterpreter in
  let rec loop : _ I.env -> _ I.checkpoint -> _ = fun env -> function
    | I.Shifting (_, _, _) | I.AboutToReduce (_, _) as cp ->
      loop env (I.resume cp)
    | I.Accepted _ -> None
    | I.Rejected -> assert false
    | I.HandlingError _ ->
      Some env
    | I.InputNeeded env' as cp ->
      match get_token lexbuf with
      | Parser_raw.EOF -> Some env'
      | token ->
        loop env' (I.offer cp (token, lexbuf.lex_start_p, lexbuf.lex_curr_p))
  in
  match checkpoint lexbuf.lex_curr_p with
  | I.InputNeeded env as cp -> loop env cp
  | _ -> assert false


let rec get_states acc env =
  let module I = Parser_raw.MenhirInterpreter in
  let loc =
    match I.top env with
    | Some (I.Element (_,_,start,stop)) -> Some (start, stop)
    | None -> None
  in
  let acc = (I.current_state_number env, loc) :: acc in
  match I.pop env with
  | None -> acc
  | Some env' -> get_states acc env'

let get_states env =
  List.rev (get_states [] env)

let print_lr1 state =
  match grammar.g_lr0_states.((Lr1.to_lr0 state :> int)).lr0_incoming with
  | None -> None
  | Some sym -> Some (symbol_name (import_symbol sym))

let rec print_concrete_children n node =
  Lr1Map.iter (fun _ node -> print_concrete n node) node.Redgraph.goto

and print_concrete n node =
  print_concrete_children (n + 1) node;
  Printf.printf "\x1b[1;33m\t\t%s↱ %s\n"
    (String.make n ' ') (Option.get (print_lr1 node.state));
  if not !opt_no_reductions_items then (
    print_string "\x1b[0;36m";
    List.iter print_endline (print_items node.state);
  )

let get_root_parent root =
  match root.Redgraph.parent with
  | Concrete _ -> assert false
  | Abstract t -> if Lazy.is_val t then Some (Lazy.force t) else None

let process_result lexbuf = function
  | None -> print_endline "Successful parse"
  | Some env ->
    let stack = get_states env in
    Format.printf "%a, parser stack (most recent first):\n%!"
      Location.print_loc (Location.curr lexbuf);
    let reds = ref [] in
    List.iteri begin fun i (state, loc) ->
      let state = Index.of_int Lr1.n state in
      if not !opt_no_reductions then (
        if i = 0 then (
          let root = Redgraph.get_root state in
          print_concrete_children 0 root;
          reds := Option.to_list (get_root_parent root);
        ) else (
          let expand abs =
            match Lr1Map.find_opt state abs.Redgraph.goto_closure with
            | None -> [abs]
            | Some sts ->
              IndexSet.fold (fun st' acc ->
                  let root = Redgraph.get_root st' in
                  print_concrete 0 root;
                  cons_option (get_root_parent root) acc
                ) sts [abs]
          in
          reds :=
            List.filter_map Redgraph.pop_abstract
              (List.concat_map expand !reds)
        );
      );
      print_string "\x1b[0m- ";
      print_string (
        match loc with
        | None -> "entrypoint\t"
        | Some loc -> print_loc loc
      );
      print_string "\x1b[1m";
      begin match print_lr1 state with
        | None ->
          let find_state (_,_,state') = state' = (state :> int) in
          let nt, _prod, _ = List.find find_state grammar.g_entry_points in
          print_endline grammar.g_nonterminals.(nt).n_name
        | Some sym -> print_endline sym
      end;
      if i = 0 || !opt_stack_items then (
        print_string "\x1b[0;36m";
        List.iter print_endline (print_items state);
      );
      print_string "\x1b[0m";
    end stack;
    print_newline ()

let () =
  match !opt_infile with
  | None | Some "" ->
    Format.eprintf "No input provided, stopping now.\n";
    Arg.usage specs usage;
  | Some file ->
    let is_intf = !opt_parse_intf || Filename.check_suffix file "i" in
    let ic = if file = "-" then stdin else open_in_bin file in
    if is_intf then
      process_result lexbuf (do_parse Parser_raw.Incremental.interface)
    else
      process_result lexbuf (do_parse Parser_raw.Incremental.implementation);
    if file <> "-" then
      close_in_noerr ic
