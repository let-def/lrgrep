(* MIT License

   Copyright (c) 2025 Frédéric Bour

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell

   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
 *)

open Utils
open Misc
open Syntax
open Fix.Indexing
open Regexp
open Info

let printf_debug = false

(* Index LR(1) states by incoming symbol, goto transitions, items, ... *)
module Indices = struct
  open Info

  let string_of_symbol =
    let buffer = Buffer.create 32 in
    function
    | Name s -> s
    | sym ->
      Buffer.reset buffer;
      let rec aux = function
        | Name s -> Buffer.add_string buffer s
        | Apply (s, args) ->
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

  type 'g t = {
    all_symbols: 'g symbol indexset;
    by_incoming_symbol: ('g symbol, 'g lr1 indexset) vector;
    prod_by_lhs: ('g nonterminal, 'g production indexset) vector;
    by_items: ('g item, 'g lr1 indexset) vector;
  }

  let make (type g) (g : g grammar) =
    (* by_incoming_symbol *)
    let by_incoming_symbol = Vector.make (Symbol.cardinal g) IndexSet.empty in
    Index.iter (Lr1.cardinal g) (fun lr1 ->
        match Lr1.incoming g lr1 with
        | None -> ()
        | Some sym -> by_incoming_symbol.@(sym) <- IndexSet.add lr1
      );
    (* prod_by_lhs *)
    let prod_by_lhs = Vector.make (Nonterminal.cardinal g) IndexSet.empty in
    Index.rev_iter (Production.cardinal g)
      (fun prod -> prod_by_lhs.@(Production.lhs g prod) <- IndexSet.add prod);
    (* Closure of nonterminals with epsilon-rules *)
    let left_rec_nt_reflexive_closure =
      let table = Vector.make (Nonterminal.cardinal g) IndexSet.empty in
      let rec close acc nt =
        if IndexSet.mem nt acc then acc else
          let acc' = Vector.get table nt in
          if IndexSet.is_not_empty acc' then
            IndexSet.union acc acc'
          else
            let acc = IndexSet.add nt acc in
            IndexSet.fold
              (fun prod acc -> close_rhs (Production.rhs g prod) 0 acc)
              prod_by_lhs.:(nt) acc
      and close_rhs rhs pos acc =
        if pos >= Array.length rhs then acc
        else match Symbol.desc g rhs.(pos) with
          | T _ -> acc
          | N nt ->
            let acc = close acc nt in
            if Nonterminal.nullable g nt then
              close_rhs rhs (pos + 1) acc
            else
              acc
      in
      fun nt ->
        let result = Vector.get table nt in
        if IndexSet.is_empty result then
          let result = close IndexSet.empty nt in
          Vector.set table nt result;
          result
        else
          result
    in
    (* by_items *)
    let by_items = Vector.make (Item.cardinal g) IndexSet.empty in
    Index.rev_iter (Lr1.cardinal g) (fun lr1 ->
        let register item = by_items.@(item) <- IndexSet.add lr1 in
        let closure_items nt =
          IndexSet.iter
            (fun prod -> register (Item.make g prod 0))
            prod_by_lhs.:(nt)
        in
        let kernel_item item =
          register item;
          let (prod, pos) = Item.desc g item in
          let rhs = Production.rhs g prod in
          if pos < Array.length rhs then
            match Symbol.desc g rhs.(pos) with
            | T _ -> ()
            | N nt ->
              IndexSet.iter closure_items
                (left_rec_nt_reflexive_closure nt)
        in
        IndexSet.iter kernel_item (Lr1.items g lr1)
      );
    {all_symbols = Symbol.all g; by_incoming_symbol;
     prod_by_lhs; by_items}

  let find_symbols g indices = function
    | None -> Result.Ok indices.all_symbols
    | Some name ->
      Result.map IndexSet.singleton
        (Symbol.find g (string_of_symbol name))

  let get_symbol g pos sym =
    let sym = string_of_symbol sym in
    match Symbol.find g sym with
    | Result.Error (`Mangled nt) ->
      warn pos "symbol %s is mangled, please write %s"
        sym (Nonterminal.to_string g nt);
      Symbol.inj_n g nt
    | Result.Error (`Dym dym) ->
      error pos "Unknown symbol %s%a" sym (print_dym (fun (_,s,_) -> s)) dym
    | Result.Ok sym -> sym
end

let string_of_goto g gt =
  let tr = Transition.of_goto g gt in
  let src = Transition.source g tr in
  let tgt = Transition.target g tr in
  Printf.sprintf "%s -> %s" (Lr1.to_string g src) (Lr1.to_string g tgt)

module Globbing = struct

  type 'g glob_skip = {
    dot: bool;
    exact: 'g glob_exact;
  }

  and 'g glob_exact = {
    dots: IntSet.t;
    syms: 'g Info.symbol indexset array;
    length: int;
    skip: 'g glob_skip option;
  }

  let parse_component comp =
    let rec loop dots syms pos = function
      | [] -> (dots, Array.of_list (List.rev syms))
      | `Dot :: rest -> loop (IntSet.add pos dots) syms pos rest
      | `Find sym :: rest -> loop dots (sym :: syms) (pos + 1) rest
    in
    loop IntSet.empty [] 0 comp

  let rec structure_filter g indices = function
    | [] -> ([], [])
    | (Skip, _pos) :: rest ->
      let last, tail = structure_filter g indices rest in
      ([], parse_component last :: tail)
    | (Dot, _pos) :: rest ->
      let last, tail = structure_filter g indices rest in
      (`Dot :: last, tail)
    | (Find sym, pos) :: rest ->
      let last, tail = structure_filter g indices rest in
      match Indices.find_symbols g indices sym with
      | Result.Error (`Mangled n) ->
        warn pos "symbol %s is mangled, please write %s"
          (Indices.string_of_symbol (Option.get sym))
          (Nonterminal.to_string g n);
        (`Find (IndexSet.singleton (Symbol.inj_n g n)) :: last, tail)
      | Result.Error (`Dym dym) ->
        error pos "Unknown symbol %s%a"
          (Indices.string_of_symbol (Option.get sym))
          (print_dym (fun (_,s,_) -> s)) dym
      | Result.Ok set ->
        (`Find set :: last, tail)

  let normalize_filter = function
    | [] -> {dots=IntSet.empty; syms=[||]; skip=None; length=0}
    | [dots, syms] -> {dots; syms; skip=None; length = Array.length syms}
    | (dots, syms) :: rest ->
      let rec loop = function
        | [] -> assert false
        | [dots, syms] ->
          {dot = false; exact={dots; syms; skip=None; length=Array.length syms}}
        | (dots, [||]) :: xs ->
          if IntSet.is_empty dots
          then loop xs
          else {(loop xs) with dot = true}
        | (dots, syms) :: rest ->
          {dot = false; exact = loop_skip dots syms rest}
      and loop_skip dots syms rest =
        let skip = loop rest in
        let length = Array.length syms + skip.exact.length in
        {dots; syms; length; skip = Some skip}
      in
      loop_skip dots syms rest

  let parse g indices filter =
    let last, tail = structure_filter g indices filter in
    let comp = parse_component last in
    normalize_filter (comp :: tail)

  let sub_match rhs pos exact =
    (pos + exact.length <= Array.length rhs) &&
    let rec loop i =
      (i = Array.length exact.syms) || (
        IndexSet.mem rhs.(pos + i) exact.syms.(i) &&
        loop (i + 1)
      )
    in
    loop 0

  let rec match_skip rhs pos {dot=_;exact} =
    match exact.skip with
    | None ->
      (* Match suffix *)
      let pos' = Array.length rhs - Array.length exact.syms in
      (pos' >= pos) && sub_match rhs pos' exact
    | Some skip' ->
      (* Match substring *)
      let rec loop pos =
        (pos + exact.length <= Array.length rhs) &&
        if not (sub_match rhs pos exact)
        then loop (pos + 1)
        else match_skip rhs (pos + Array.length exact.syms) skip'
      in
      loop pos

  let add_dots pos exact dots =
    IntSet.fold (fun i acc -> IntSet.add (pos + i) acc) exact.dots dots

  let rec extract_skip rhs pos {dot; exact} =
    match exact.skip with
    | None ->
      (* Match suffix *)
      let pos' = Array.length rhs - exact.length in
      if (pos' >= pos) && sub_match rhs pos' exact then
        let dots =
          if dot
          then IntSet.init_interval pos pos'
          else IntSet.empty
        in
        Some (add_dots pos' exact dots)
      else
        None

    | Some skip' ->
      (* Match substring *)
      let rec downmatch i j =
        if j <= i then i
        else if sub_match rhs j exact &&
                match_skip rhs (j + Array.length exact.syms) skip' then
          j
        else
          downmatch i (j - 1)
      in
      let rec upmatch pos =
        if pos + exact.length > Array.length rhs then None
        else if not (sub_match rhs pos exact)
        then upmatch (pos + 1)
        else
          match extract_skip rhs (pos + Array.length exact.syms) skip' with
          | None -> None
          | Some dots ->
            let dots = add_dots pos exact dots in
            let dots =
              if dot then
                IntSet.union dots
                  (IntSet.init_interval pos
                     (downmatch pos (Array.length rhs - skip'.exact.length)))
              else
                dots
            in
            Some dots
      in
      upmatch pos

  let extract rhs exact =
    if not (sub_match rhs 0 exact) then
      IntSet.empty
    else
      let pos' = Array.length exact.syms in
      match exact.skip with
      | None ->
        if Array.length rhs = pos'
        then exact.dots
        else IntSet.empty
      | Some skip ->
        match extract_skip rhs pos' skip with
        | None ->
          assert (not (match_skip rhs pos' skip));
          IntSet.empty
        | Some dots -> IntSet.union dots exact.dots
end

let transl_filter (type g) (g : g grammar) indices position ~lhs ~rhs =
  let transl_sym = Option.map (Indices.get_symbol g position) in
  let lhs = transl_sym lhs in
  let prods = match lhs with
    | None -> Production.all g
    | Some lhs ->
      match Symbol.desc g lhs with
      | T _ -> error position "left-handside of a filter should be a non-terminal"
      | N n -> indices.Indices.prod_by_lhs.:(n)
  in
  let filter = Globbing.parse g indices rhs in
  let matching_dots prod = Globbing.extract (Production.rhs g prod) filter in
  let matching_states prod =
    IntSet.fold (fun pos acc ->
        IndexSet.union indices.by_items.:(Item.make g prod pos) acc)
      (matching_dots prod) IndexSet.empty
  in
  IndexSet.bind prods matching_states

let compile_reduce_expr (type g) (g : g grammar) rg trie re =
  let open Info in
  let targets = ref IndexSet.empty in
  let immediate = ref IndexSet.empty in
  let rec follow path (node : g Redgraph.target_trie) (label, k : _ Label.t * _ K.t) =
    match k with
    | K.Accept ->
      if false then
        print_endline (Lr1.list_to_string g (List.rev path));
      immediate := IndexSet.union (IndexSet.inter label.filter node.immediates) !immediate;
      targets := IndexMap.fold (fun lr1 target acc ->
          if IndexSet.mem lr1 label.filter
          then IndexSet.add target acc
          else acc
        ) node.targets !targets
    | k ->
      IndexMap.iter begin fun lr1 node' ->
        if IndexSet.mem lr1 label.Label.filter then
          derive (lr1 :: path) node' k
      end node.sub
  and derive path node k =
    List.iter (follow path node) (K.derive g rg (Lr1.all g) k)
  in
  derive [] trie (K.More (re, K.Done));
  (* if printf_debug then
    Printf.printf "pattern:\n\
                   - goto: %s\n\
                   - immediate: %s\n"
      (string_of_indexset ~index:(string_of_goto g) !goto)
      (Lr1.set_to_string g !immediate); *)
  (!targets, !immediate)

let transl (type g) (g : g grammar) rg indices trie ~capture re =
  let all_cap = ref IndexSet.empty in
  let mk_capture kind name =
    let index = capture kind name in
    all_cap := IndexSet.add index !all_cap;
    IndexSet.singleton index
  in
  let rec transl ~for_reduction re =
    Expr.make re.position @@
    match re.desc with
    | Atom (capture, symbol, mark) ->
      if for_reduction && Option.is_some capture then
        error re.position "Captures are not allowed inside reductions";
      let set = match symbol with
        | None -> Lr1.all g
        | Some sym ->
          let sym = Indices.get_symbol g re.position sym in
          if for_reduction && Symbol.is_terminal g sym then
            warn re.position "A reduction can only match non-terminals";
          indices.Indices.by_incoming_symbol.:(sym)
      in
      let cap = match capture with
        | None -> IndexSet.empty
        | Some name -> mk_capture Value name
      in
      Expr.Seq [
        Expr.make re.position (Expr.Set (set, cap));
        Expr.make re.position (Expr.Usage (Usage.singleton mark));
      ]
    | Alternative res ->
      Expr.Alt (List.map (transl ~for_reduction) res)
    | Repetition {expr; policy} ->
      Expr.Star (transl ~for_reduction expr, policy)
    | Reduce {capture; policy; expr; mark} ->
      if for_reduction then
        error re.position "Reductions cannot be nested";
      (* print_cmon stderr (Front.Syntax.cmon_regular_expression expr);*)
      let re = transl ~for_reduction:true expr in
      let pattern, immediate = compile_reduce_expr g rg trie re in
      if false then
        warn re.position
          "Reduce pattern is matching %d cases (and matches immediately for %d states)"
          (IndexSet.cardinal pattern)
          (IndexSet.cardinal immediate);
      let capture, capture_end = match capture with
        | None -> IndexSet.empty, IndexSet.empty
        | Some name ->
          let capture_start = mk_capture Start_loc name in
          let capture_end = mk_capture End_loc name in
          (capture_start, capture_end)
      in
      let r = Expr.Reduce (capture_end, {capture; pattern; policy}) in
      let core =
        if IndexSet.is_empty immediate then
          r
        else if immediate == Lr1.all g then
          Expr.Alt [Expr.make re.position r; Expr.make re.position (Expr.Seq [])]
        else
          Expr.Alt [Expr.make re.position r; Expr.make re.position (Expr.Filter immediate)]
      in
      Expr.Seq [
        Expr.make re.position core;
        Expr.make re.position (Expr.Usage (Usage.singleton mark));
      ]
    | Concat res ->
      Expr.Seq (List.rev_map (transl ~for_reduction) res)
    | Filter {lhs; rhs} ->
      let lhs = Option.join lhs in
      let states = transl_filter g indices re.position ~lhs ~rhs in
      if IndexSet.is_empty states then
        warn re.position "No items match this filter";
      Expr.Filter states
  in
  let result = transl ~for_reduction:false re in
  (!all_cap, result)
