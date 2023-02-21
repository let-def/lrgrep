open Utils
open Fix.Indexing

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

module Grammar = MenhirSdk.Cmly_read.Lift(struct
    let grammar : MenhirSdk.Cmly_format.grammar =
      let magic_number = "CMLY" ^ MenhirSdk.Version.version in
      let magic_length = String.length magic_number in
      if String.sub Interpreter_data.grammar 0 magic_length <> magic_number then
        failwith "Internal error: grammar has invalid magic number";
      Marshal.from_string Interpreter_data.grammar magic_length
  end)

module Info = Mid.Info.Make(Grammar)
module Regexp = Mid.Regexp.Make(Info)()

(*
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

  let all_terminals =
    let acc = ref IndexSet.empty in
    for i = Terminal.count - 1 downto 0
    do acc := IndexSet.add (Terminal.of_int i) !acc done;
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
          List.map (fun (t, p) ->
              Terminal.of_int t, Index.of_int Production.n p)
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
*)

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
  let open Info in
  let rhs = Production.rhs prod in
  let path = ref [] in
  let add_dot i = if pos = i then path := "." :: !path in
  add_dot (Array.length rhs);
  for i = Array.length rhs - 1 downto 0 do
    path := Info.Symbol.name rhs.(i) :: !path;
    add_dot i;
  done;
  path := (Nonterminal.to_string (Production.lhs prod) ^ ":") :: !path;
  String.concat " " !path

let print_items lr1 =
  List.map (fun item -> "\t\t  [" ^ print_item item ^ "]") (Info.Lr1.items lr1)

(*module Redgraph = struct
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
        |> List.filter_map (fun (_, prod) ->
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
      let rec visit_nt nt acc =
        let st' =
          try Transition.find_goto_target st nt
          with Not_found ->
            Printf.eprintf "goto(#%d,%s): error\nitems:\n%!"
              (st :> int) (Nonterminal.print nt);
            List.iter prerr_endline (print_items st);
            assert false
        in
        visit_nts (goto_nts st') (IndexSet.add st' acc)
      and visit_nts nts acc =
        IndexSet.fold visit_nt nts acc
      in
      visit_nts abs.goto_nt IndexSet.empty
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
  end*)

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
    lexbuf
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
  let lr1 = Index.of_int Info.Lr1.n (I.current_state_number env) in
  let acc = (lr1, loc) :: acc in
  match I.pop env with
  | None -> acc
  | Some env' -> get_states acc env'

let get_states env =
  List.rev (get_states [] env)

let print_lr1 state =
  Option.map Info.Symbol.name (Info.Lr1.incoming state)

(*let rec print_inner_children n node =
  IndexMap.iter (fun _ node -> print_inner n node) node.Redgraph.goto

and print_inner n node =
  print_inner_children (n + 1) node;
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
*)

module RR = Regexp.Redgraph

let cons_not_empty x xs =
  match x with
  | [] -> xs
  | _ -> x :: xs

let rec display_steps n acc = function
  | [] -> acc
  | {RR. reachable=_; candidates} :: rest ->
    let acc = List.fold_left (display_candidate n) acc candidates in
    display_steps (n - 1) acc rest

and display_candidate : type a . int -> _ -> a RR.goto_candidate -> _ =
  fun n acc {RR. target; lookahead=_; filter=_} ->
  let {RR. inner; outer} = RR.get_transitions target in
  let acc = cons_not_empty outer acc in
  let acc = display_steps (n + 1) acc inner in
  let top, _ = RR.get_stack target in
  Printf.printf "\x1b[1;33m\t\t%s↱ %s\n"
    (String.make n ' ') (Option.get (print_lr1 top));
  acc

let rec merge_outer yys (xxs : _ RR.reduction_step list)=
  match xxs, yys with
  | [], l -> l
  | l, [] -> List.map (fun x -> x.RR.candidates) l
  | (x :: xs), (y :: ys) ->
    (x.RR.candidates @ y) :: merge_outer ys xs

let flatten_outer ?(outer=[]) outers =
  List.fold_left merge_outer outer outers

let process_result lexbuf = function
  | None -> print_endline "Successful parse"
  | Some env ->
    let stack = get_states env in
    (*Format.printf "let stack = [%s]\n"
      (String.concat ";" (List.map string_of_int (List.map fst stack)));*)
    Format.printf "%a, parser stack (most recent first):\n%!"
      Location.print_loc (Location.curr lexbuf);
    let outer = ref [] in
    List.iteri (fun i (state, loc) ->
        if i = 0 then (
          outer := flatten_outer (
              let top, _ = List.hd stack in
              let transitions = Vector.get Regexp.Redgraph.initial top in
              display_steps 0 [transitions.outer] transitions.inner
            )
        ) else (
          let rec process_outer = function
            | [] -> []
            | [] :: next -> next
            | candidates :: next ->
              let candidates =
                List.filter
                  (fun c -> IndexSet.mem state c.RR.filter)
                  candidates
              in
              let outer = List.fold_left (display_candidate 1) [] candidates in
              process_outer (flatten_outer ~outer:([] :: next) outer)
          in
          outer := process_outer !outer
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
            let find_state (_,_,state') = state' = Info.Lr1.to_g state in
            let nt, _prod, _ = List.find find_state Grammar.Grammar.entry_points in
            print_endline (Grammar.Nonterminal.name nt)
          | Some sym -> print_endline sym
        end;
        if i = 0 || !opt_stack_items then (
          print_string "\x1b[0;36m";
          List.iter print_endline (print_items state);
        );
        print_string "\x1b[0m";
      ) stack

    (*
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
    *)


let () =
  match !opt_infile with
  | None | Some "" ->
    Format.eprintf "No input provided, stopping now.\n";
    Arg.usage specs usage;
  | Some file ->
    let is_intf = !opt_parse_intf || Filename.check_suffix file "i" in
    let ic, filename, close_ic =
      if file = "-" then
        (stdin, "<stdin>", false)
      else
        (open_in_bin file, file, true)
    in
    let lexbuf =
      let lexbuf = Lexing.from_channel ~with_positions:true ic in
      Lexing.set_filename lexbuf filename;
      lexbuf
    in
    if is_intf then
      process_result lexbuf
        (do_parse Parser_raw.Incremental.interface lexbuf)
    else
      process_result lexbuf
        (do_parse Parser_raw.Incremental.implementation lexbuf);
    if close_ic then
      close_in_noerr ic
