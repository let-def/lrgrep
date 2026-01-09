open MenhirSdk
open Fix.Indexing
open Utils
open Misc

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

type 'a with_position = 'a * Lexing.position * Lexing.position

type ('lr1, 'terminal) parser_output = {
  stack: 'lr1 with_position list;
  remainder: 'terminal with_position Seq.node;
}

module Make(Grammar : Cmly_api.GRAMMAR)() : sig
  open Grammar

  val parse_sentence
    :  entrypoint:lr1
    -> sentence:terminal with_position Seq.t
    -> (lr1, terminal) parser_output

  val analyze_stack
    :  terminal config
    -> stack:lr1 with_position Seq.t
    -> unit

  val analyze_sentence
    :  terminal config
    -> entrypoint:lr1
    -> sentence:terminal with_position Seq.t ->
    unit
end =
struct
  open Kernel.Info
  include Lift()
  include Load_grammar(Grammar)

  let reductions = Kernel.Redgraph.close_lr1_reductions grammar

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

  let print_items indent suffix items =
    Printf.printf "\t\t%s\x1b[0;32m  [%s" (String.make indent ' ') suffix;
    let pad = String.make (indent + 3 + String.length suffix) ' ' in
    let first = ref true in
    IndexSet.iter (fun item ->
        if !first then first := false else
          Printf.printf "\n\t\t%s" pad;
        Printf.printf " / %s" (Item.to_string grammar item);
      ) items;
    Printf.printf "]\n"

  let print_lr1 state =
    match Lr1.incoming grammar state with
    | None -> None
    | Some sym -> Some (Symbol.name grammar sym)

  (*let display config =
    let rec display_steps la n acc = function
      | [] -> acc
      | {Viable. reachable=_; goto_transitions} :: rest ->
         let acc = List.fold_left (display_goto_transition la n) acc goto_transitions in
         display_steps la (n - 1) acc rest

    and display_goto_transition la n acc = let la = IndexSet.inter la lookahead
    in if IndexSet.is_empty la then acc else let {Viable. inner; outer} =
    Viable.get_transitions target in let acc = if outer <> [] then (la, outer)
    :: acc else acc in let acc = display_steps la (n + 1) acc inner in let
    target = Viable.get_config target in Printf.printf "\t\t%s\x1b[1;33m↱ %s\n"
    (String.make n ' ') (Option.get (print_lr1 target.top)); if
    config.print_reduce_filter then ( let suffix = match target.rest with | [] |
    [_] -> [target.top] | _ :: rest -> target.top :: rest in let suffix =
    Utils.Misc.string_concat_map "; " (fun lr1 -> Option.get (print_lr1 lr1))
    suffix in print_items n suffix (Lr1.items target.top); ); acc in
    display_goto_transition*)

  let analyze_stack config ~stack =
    (*Format.printf "let stack = [%s]\n"
      (String.concat ";" (List.map string_of_int (List.map fst stack)));*)
    Format.printf "Parser stack (most recent first):\n%!";
    let outer = ref [] in
    List.iteri begin fun i (state, start, stop) ->
      let state = Index.of_int (Lr1.cardinal grammar) (Grammar.Lr1.to_int state) in
      let display_stacks

      in
      let rec simulate_reductions pending lookaheads = function
        | [] -> pending
        | nts :: remainder ->
          let pending = cons_if (not (List.is_empty remainder)) remainder pending in
          IndexMap.fold begin fun nt lookaheads' pending ->
            let lookaheads = IndexSet.inter lookaheads lookaheads' in
            if IndexSet.is_not_empty lookaheads then
              let state' = Transition.find_goto_target grammar state nt in
              simulate_reductions pending lookaheads reductions.:(state').reductions
            else
              pending
          end nts pending
      in
      if i = 0 then
        outer := [Terminal.all, reductions.:(state)];
      outer := process_threads [] !outer;
      let items = Lr1.items state in
      if (i = 0 && config.print_reduce_filter) then (
        print_items 0 "" items;
      ) else if config.print_stack_items then (
        print_string "\x1b[0;36m";
        IndexSet.iter
          (fun item -> print_endline ("\t\t  [" ^ Item.to_string item ^ "]"))
          items;
      );
      print_string "\x1b[0m- ";
      print_string (print_loc (start, stop));
      print_string "\x1b[1m";
      begin match print_lr1 state with
      | None ->
         let find_state (_,_,state') = state' = Lr1.to_g state in
         let nt, _prod, _ = List.find find_state Grammar.Grammar.entry_points in
         print_endline (Grammar.Nonterminal.name nt)
      | Some sym -> print_endline sym
      end;
      print_string "\x1b[0m";
    end stack
end
