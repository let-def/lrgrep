(*
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

  val analyze_stack
    :  terminal config
    -> stack:lr1 with_position Seq.t
    -> unit

  val parse_sentence
    :  entrypoint:lr1
    -> sentence:terminal with_position Seq.t
    -> (lr1, terminal) parser_output

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

  let rcs = Kernel.Redgraph.close_lr1_reductions grammar

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

  let print_stack config ~is_goto stack =
    let top = List.hd stack in
    let stack = List.rev stack in
    let stack = if is_goto then stack else List.tl stack in
    let stack = List.filter_map (Lr1.incoming grammar) stack in
    Printf.printf "\t\t\x1b[1;33m↱ %s\n"
      (string_concat_map " " (Symbol.name grammar) stack);
    if config.print_reduce_filter then begin
      print_items 0 "_*" (Lr1.items grammar top);
    end

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

  let analyze_stack config ~stack =
    Format.printf "Parser stack (most recent first):\n%!";
    let failing = ref IndexSet.empty in
    let outer = ref [] in
    List.iteri begin fun i (state, start, stop) ->
      let state = Index.of_int (Lr1.cardinal grammar) (Grammar.Lr1.to_int state) in
      let rec reached_state ~is_goto lookaheads state =
        let rc = rcs.:(state) in
        outer := merge_reductions (!outer, filter_reductions lookaheads rc.reductions);
        failing := IndexSet.fused_inter_union rc.failing lookaheads ~acc:!failing;
        List.iter begin fun (stack, lookaheads') ->
          if not (IndexSet.disjoint lookaheads lookaheads') then
            print_stack config ~is_goto stack
        end rc.stacks
      in
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
      if (i = 0 && config.print_reduce_filter) then (
        print_items 0 "" items;
      ) else if config.print_stack_items then (
        print_string "\x1b[0;36m";
        IndexSet.iter
          (fun item -> print_endline ("\t\t  [" ^ Item.to_string grammar item ^ "]"))
          items;
      );
      print_string "\x1b[0m- ";
      print_string (print_loc (start, stop));
      print_string "\x1b[1m";
      begin match print_lr1 state with
      | None ->
        let prod = Option.get (Lr1.is_entrypoint grammar state) in
        print_endline (Symbol.name grammar (Production.rhs grammar prod).(0))
      | Some sym -> print_endline sym
      end;
      print_string "\x1b[0m";
    end stack

  let parse_sentence ~entrypoint:_ ~sentence:_ =


  val analyze_sentence
    :  terminal config
    -> entrypoint:lr1
    -> sentence:terminal with_position Seq.t ->
    unit
end
*)
