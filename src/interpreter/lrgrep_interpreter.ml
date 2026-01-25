open Utils
open Misc
open Fix.Indexing

type 'terminal config = {
  print_reduce_filter : bool;
  print_stack_items : bool;
  (*pretty_printer : ('terminal list -> string) option;*)
}

let default_config = {
  print_reduce_filter = true;
  print_stack_items = false;
  (*pretty_printer = None;*)
}

let config
    ?(print_reduce_filter = default_config.print_reduce_filter)
    ?(print_stack_items = default_config.print_stack_items)
    (*?pretty_printer*)
    ()
  =
  {print_reduce_filter; print_stack_items; (*pretty_printer*)}

type ('a, 'p) with_position = 'a * 'p * 'p

type ('lr1, 'terminal, 'p) parser_output = {
  stack: ('lr1, 'p) with_position list;
  remainder: ('terminal, 'p) with_position Seq.node;
}

open Kernel.Info

type 'g sentence = {
  entrypoint: ('g lr1 index, Lexing.position) with_position option;
  symbols: ('g terminal index, Lexing.position) with_position list;
}

let lift_sentence g sentence =
  (* Step 1: extract optional entrypoint and symbols *)
  let lexbuf = Lexing.from_string ~with_positions:true sentence in
  Lexing.set_filename lexbuf "input";
  let symbol tok =
    let text = match tok with
      | `IDENT x -> x
      | _ ->
        Syntax.nonfatal_error lexbuf.lex_start_p "expecting a symbol";
        raise Exit
    in
    (text, lexbuf.lex_start_p, lexbuf.lex_curr_p)
  in
  let rec symbols acc =
    match Front.Lexer.sentence_interpreter lexbuf with
    | `EOF -> List.rev acc
    | other -> symbols (symbol other :: acc)
  in
  let entrypoint, symbols =
    let candidate = symbol (Front.Lexer.sentence_interpreter lexbuf) in
    match Front.Lexer.sentence_interpreter lexbuf with
    | `EOF   -> None, [candidate]
    | `COLON -> (Some candidate, symbols [])
    | other  -> (None, symbols [symbol other; candidate])
  in
  (* Step 2: lift to grammatical entities *)
  let lift_entrypoint (sym, startp, endp) =
    let entrypoints = Lr1.entrypoint_table g in
    match Hashtbl.find_opt entrypoints sym  with
    | None ->
      Syntax.nonfatal_error
        startp
        "unknown entrypoint %S%a\n"
        sym
        (print_dym (fun (_,s,_) -> s))
        (Damerau_levenshtein.filter_approx ~dist:10 sym
           (Hashtbl.to_seq entrypoints));
      raise Exit
    | Some sym -> (sym, startp, endp)
  in
  let lift_terminal (sym, startp, endp) =
    match Terminal.find g sym with
    | Result.Ok t -> (t, startp, endp)
    | Result.Error dym ->
      Syntax.nonfatal_error startp
        "unknown terminal %S%a\n" sym
        (print_dym (fun (_,s,_) -> s)) dym;
      raise Exit
  in
  let entrypoint = Option.map lift_entrypoint entrypoint in
  let symbols = List.map lift_terminal symbols in
  { entrypoint; symbols }

let print_loc ((loc_start : Lexing.position), (loc_end : Lexing.position)) =
  if loc_start = Lexing.dummy_pos then
    "             \t"
  else
    let sprintf = Printf.sprintf in
    let sline = loc_start.pos_lnum in
    let scol  = loc_start.pos_cnum - loc_start.pos_bol in
    let eline = loc_end.pos_lnum in
    let ecol  = loc_end.pos_cnum - loc_end.pos_bol in
    if sline = eline then
      sprintf "line %d:%d-%d\t" sline scol ecol
    else
      sprintf "from %d:%d to %d:%d\t" sline scol eline ecol

let print_items grammar indent suffix items =
  Printf.printf "\t\t%s\x1b[0;32m[%s" (String.make indent ' ') suffix;
  let pad = String.make (indent + 1 + String.length suffix) ' ' in
  let first = ref true in
  IndexSet.iter (fun item ->
      if !first then first := false else
        Printf.printf "\n\t\t%s" pad;
      Printf.printf " / %s" (Item.to_string grammar item);
    ) items;
  Printf.printf "]\n"

let print_lr1 grammar state =
  match Lr1.incoming grammar state with
  | None -> None
  | Some sym -> Some (Symbol.to_string grammar sym)

let print_stack grammar config ~is_goto stack =
  let top = List.hd stack in
  let stack = List.rev stack in
  let stack = if is_goto then stack else List.tl stack in
  let stack = List.filter_map (Lr1.incoming grammar) stack in
  if config.print_reduce_filter then
    print_items grammar
      (if is_goto then 2 else 0)
      "_*" (Lr1.items grammar top);
  if is_goto then
    Printf.printf "\t\t\x1b[1;33m↱ %s\n"
      (string_concat_map " " (Symbol.to_string grammar) stack)

let rec filter_reductions la = function
  | [] -> []
  | x :: xs ->
    let y = IndexMap.filter_map (fun _ la' ->
        match IndexSet.inter la la' with
        | la when IndexSet.is_empty la -> None
        | la -> Some la
      ) x
    in
    match filter_reductions la xs with
    | [] when IndexMap.is_empty y -> []
    | ys -> y :: ys

let rec merge_reductions = function
  | [], xs | xs, [] -> xs
  | x :: xs, y :: ys ->
    let xy = IndexMap.union (fun _ la la' -> Some (IndexSet.union la la')) x y in
    let xys = merge_reductions (xs, ys) in
    xy :: xys

let analyze_stack grammar (rcs : (_, _ Kernel.Redgraph.reduction_closure) vector)
    config ~stack ~remainder
  =
  Format.printf "Parser stack (most recent first):\n%!";
  let failing = ref IndexSet.empty in
  let outer = ref [] in
  let reached_state ~is_goto lookaheads state =
    let rc = rcs.:(state) in
    outer := merge_reductions (!outer, filter_reductions lookaheads rc.reductions);
    failing := IndexSet.fused_inter_union rc.failing lookaheads ~acc:!failing;
    let rec visit_stacks (stack, lookaheads', sub') =
      if not (IndexSet.disjoint lookaheads lookaheads') then (
        print_stack grammar config ~is_goto stack;
        List.iter visit_stacks sub'.Kernel.Redgraph.subs
      )
    in
    visit_stacks ([state], lookaheads, rc.stacks)
  in
  List.iteri begin fun i (state, start, stop) ->
    let simulate_gotos nts =
      IndexMap.iter begin fun nt lookaheads ->
        reached_state ~is_goto:true lookaheads
          (Transition.find_goto_target grammar state nt)
      end nts
    in
    let rec simulate_reductions () =
      match !outer with
      | [] -> ()
      | x :: xs when IndexMap.is_empty x ->
        outer := xs
      | x :: xs ->
        outer := IndexMap.empty :: xs;
        simulate_gotos x;
        simulate_reductions ()
    in
    if i = 0 then
      reached_state ~is_goto:false (Terminal.regular grammar) state
    else
      simulate_reductions ();
    let items = Lr1.items grammar state in
    if config.print_stack_items then (
      print_string "\x1b[0;36m";
      IndexSet.iter
        (fun item -> print_endline ("\t\t[" ^ Item.to_string grammar item ^ "]"))
        items;
    );
    print_string "\x1b[0m- ";
    print_string (print_loc (start, stop));
    print_string "\x1b[1m";
    begin match print_lr1 grammar state with
      | None ->
        let prod = Option.get (Lr1.is_entrypoint grammar state) in
        print_endline (Symbol.to_string grammar (Production.rhs grammar prod).(0))
      | Some sym -> print_endline sym
    end;
    print_string "\x1b[0m";
  end stack;
  if not (list_is_empty remainder) then
    Printf.printf "Remaining input:\n  %s\n"
      (string_concat_map " " (Terminal.to_string grammar) remainder);
  if IndexSet.is_not_empty !failing then
    Printf.printf "Rejected lookahead symbols:\n  %s\n"
      (string_concat_map ", " (Terminal.to_string grammar)
         (List.rev (IndexSet.elements !failing)))

type 'g parser = {
  grammar: 'g grammar;
  table: ('g lr1 index * 'g terminal index,
          [ `Reduce of 'g production index
          | `Reject
          | `Shift of 'g lr1 index ]) Hashtbl.t
}

(* A parser for [grammar] with a lazily populated action table *)
let make_parser (type g) (grammar : g grammar) : g parser =
  { grammar; table = Hashtbl.create 7 }

(* Lookup and memoize parser actions *)
let get_action parser state terminal =
  let g = parser.grammar in
  match Lr1.default_reduction g state with
  | Some prod -> `Reduce prod
  | None ->
    let key = (state, terminal) in
    match Hashtbl.find_opt parser.table key with
    | Some action -> action
    | None ->
      let action =
        match
          IndexSet.find
            (fun red ->
               let la = Reduction.lookaheads g red in
               IndexSet.mem terminal la)
            (Reduction.from_lr1 g state)
        with
        | red -> `Reduce (Reduction.production g red)
        | exception Not_found ->
          let sym = Symbol.inj_t g terminal in
          match
            IndexSet.find
              (fun tr -> Index.equal sym (Transition.symbol g tr))
              (Transition.successors g state)
          with
          | tr -> `Shift (Transition.target g tr)
          | exception Not_found ->
            `Reject
      in
      Hashtbl.add parser.table key action;
      action

let parse_sentence (type g p)
    (parser : g parser)
    (entrypoint : (g lr1 index, p) with_position)
    (symbols : (g terminal index, p) with_position Seq.t)
  =
  (* Process a sentence *)
    let rec consume_terminal stack (t, startp, endp as token) =
      let (state, _, currp) = List.hd stack in
      match get_action parser state t with
      | `Reject -> Result.Error stack
      | `Shift state -> Result.Ok ((state, startp, endp) :: stack)
      | `Reduce prod ->
        let (stack, startp', endp') =
          match Production.length parser.grammar prod with
          | 0 -> (stack, currp, currp)
          | n ->
            let (_, _, endp) = List.hd stack in
            let stack = list_drop (n - 1) stack in
            let (_, startp, _) = List.hd stack in
            let stack = List.tl stack in
            (stack, startp, endp)
        in
        let (state, _, _) = List.hd stack in
        let state' =
          Transition.find_goto_target parser.grammar state
            (Production.lhs parser.grammar prod)
        in
        let stack = (state', startp', endp') :: stack in
        consume_terminal stack token
    in
    let rec loop stack ts =
      match ts () with
      | Seq.Nil -> (stack, stack, Seq.empty)
      | Seq.Cons (t, ts') as ts0 ->
        match consume_terminal stack t with
        | Result.Ok stack' -> loop stack' ts'
        | Result.Error stack' -> (stack, stack', fun () -> ts0)
    in
    loop [entrypoint] symbols
