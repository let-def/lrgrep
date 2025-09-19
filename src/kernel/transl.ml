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
    linearized_symbols: (string, 'g symbol index) Hashtbl.t;
    prod_by_lhs: ('g nonterminal, 'g production indexset) vector;
    by_items: ('g item, 'g lr1 indexset) vector;
  }

  let make (type g) (g : g grammar) =
    (* linearized_symbols *)
    let linearized_symbols = Hashtbl.create 7 in
    let name s = Symbol.name g ~mangled:false s in
    let add_symbol s = Hashtbl.add linearized_symbols (name s) s in
    Index.iter (Symbol.cardinal g) add_symbol;
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
          if not (IndexSet.is_empty acc') then
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
    {all_symbols = Symbol.all g; by_incoming_symbol; linearized_symbols;
     prod_by_lhs; by_items}

  let find_linearized_symbol indices name =
    Hashtbl.find_opt indices.linearized_symbols name

  let find_symbol indices name =
    find_linearized_symbol indices (string_of_symbol name)

  let find_symbols indices = function
    | None -> indices.all_symbols
    | Some name ->
      match find_linearized_symbol indices (string_of_symbol name) with
      | None -> IndexSet.empty
      | Some sym -> IndexSet.singleton sym

  let get_symbol indices pos sym =
    match find_symbol indices sym with
    | None -> error pos "Unknown symbol %s" (string_of_symbol sym)
    | Some sym -> sym
end

let string_of_goto g gt =
  let tr = Transition.of_goto g gt in
  let src = Transition.source g tr in
  let tgt = Transition.target g tr in
  Printf.sprintf "%s -> %s" (Lr1.to_string g src) (Lr1.to_string g tgt)

module Reductum_trie = struct
  type 'g t = {
    mutable sub : ('g lr1, 'g t) indexmap;
    mutable goto : 'g goto_transition indexset;
    mutable immediate: 'g lr1 indexset;
  }

  let make
    (type g)
    (g : g grammar)
    (rc : (g lr1, g Redgraph.reduction_closure) vector)
    (grc : (g goto_transition, g Redgraph.reduction_closure) vector)
    =
    let empty () = {
      sub = IndexMap.empty;
      goto = IndexSet.empty;
      immediate = IndexSet.empty;
    } in
    let root = empty () in
    root.immediate <- Lr1.all g;
    let get_child node lr1 =
      match IndexMap.find_opt lr1 node.sub with
      | Some node' -> node'
      | None ->
        let node' = empty () in
        node.sub <- IndexMap.add lr1 node' node.sub;
        node'
    in
    let rec mark_immediate node = function
      | [] -> assert false
      | [lr1] -> node.immediate <- IndexSet.add lr1 node.immediate
      | lr1 :: rest -> mark_immediate (get_child node lr1) rest
    in
    Vector.iteri begin fun _ cl ->
      List.iter (fun (stack, _) -> mark_immediate root stack)
        cl.Redgraph.stacks;
    end rc;
    Vector.iteri begin fun gt cl ->
      let register node = node.goto <- IndexSet.add gt node.goto in
      List.iter
        (fun (stack, _) -> register (List.fold_left get_child root stack))
        cl.Redgraph.stacks
      (*register (get_child root (Transition.target g (Transition.of_goto g gt)))*)
    end grc;
    root

  let output_spaces =
    let buf = ref "" in
    fun oc n ->
      if n > String.length !buf then
        buf := String.make n ' ';
      output_substring oc !buf 0 n

  let rec dump_trie g indent node =
    let immediate =
      if IndexSet.equal node.immediate (Lr1.all g) then
        "<any>"
      else
        Lr1.set_to_string g node.immediate
    in
    let goto =
      string_of_indexset ~index:(string_of_goto g) node.goto
    in
    Printf.fprintf stdout
      "%a- immediate: %s\n\
       %a- goto: %s\n"
      output_spaces indent immediate
      output_spaces indent goto;
    IndexMap.iter (fun lr1 node' ->
        Printf.fprintf stdout "%a-> %s\n" output_spaces indent (Lr1.to_string g lr1);
        dump_trie g (indent + 2) node'
      ) node.sub

  let dump_trie g node =
    Printf.fprintf stdout "root:\n";
    dump_trie g 2 node;
    flush stdout

end

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

  let rec structure_filter indices = function
    | [] -> ([], [])
    | (Skip, _pos) :: rest ->
      let last, tail = structure_filter indices rest in
      ([], parse_component last :: tail)
    | (Dot, _pos) :: rest ->
      let last, tail = structure_filter indices rest in
      (`Dot :: last, tail)
    | (Find sym, pos) :: rest ->
      let last, tail = structure_filter indices rest in
      let set = Indices.find_symbols indices sym in
      if IndexSet.is_empty set then
        error pos "Unknown symbol %s" (Indices.string_of_symbol (Option.get sym));
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

  let parse indices filter =
    let last, tail = structure_filter indices filter in
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
  let transl_sym = Option.map (Indices.get_symbol indices position) in
  let lhs = transl_sym lhs in
  let prods = match lhs with
    | None -> Production.all g
    | Some lhs ->
      match Symbol.desc g lhs with
      | T _ -> error position "left-handside of a filter should be a non-terminal"
      | N n -> indices.prod_by_lhs.:(n)
  in
  let filter = Globbing.parse indices rhs in
  let matching_dots prod = Globbing.extract (Production.rhs g prod) filter in
  let matching_states prod =
    IntSet.fold (fun pos acc ->
        IndexSet.union indices.by_items.:(Item.make g prod pos) acc)
      (matching_dots prod) IndexSet.empty
  in
  IndexSet.bind prods matching_states

let compile_reduce_expr (type g) (g : g grammar) rg trie re =
  let open Info in
  let goto = ref IndexSet.empty in
  let immediate = ref IndexSet.empty in
  let rec follow path (node : _ Reductum_trie.t) (label, k : _ Label.t * _ K.t) =
    match k with
    | K.Accept ->
      if false then
        print_endline (Lr1.list_to_string g (List.rev path));
      let immediate_filter = IndexSet.inter label.filter node.immediate in
      immediate := IndexSet.union immediate_filter !immediate;
      let goto_filter =
        IndexSet.filter (fun gt ->
            let src = Transition.source g (Transition.of_goto g gt) in
            IndexSet.mem src label.filter
          ) node.goto
      in
      goto := IndexSet.union goto_filter !goto
    | k ->
      IndexMap.iter begin fun lr1 node' ->
        if IndexSet.mem lr1 label.Label.filter then
          derive (lr1 :: path) node' k
      end node.sub
  and derive path node k =
    List.iter (follow path node) (K.derive g rg (Lr1.all g) k)
  in
  derive [] trie (K.More (re, K.Done));
  if printf_debug then
    Printf.printf "pattern:\n\
                   - goto: %s\n\
                   - immediate: %s\n"
      (string_of_indexset ~index:(string_of_goto g) !goto)
      (Lr1.set_to_string g !immediate);
  (!goto, !immediate)

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
          let sym = Indices.get_symbol indices re.position sym in
          if for_reduction && Symbol.is_terminal g sym then
            warn re.position "A reduction can only match non-terminals";
          indices.by_incoming_symbol.:(sym)
      in
      let cap = match capture with
        | None -> IndexSet.empty
        | Some name -> mk_capture Value name
      in
      Expr.Set (set, cap, Usage.singleton mark)
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
      let usage = Usage.singleton mark in
      let r = Expr.Reduce (capture_end, {capture; pattern; policy; usage}) in
      if IndexSet.is_empty immediate then
        r
      else if immediate == Lr1.all g then
        Expr.Alt [Expr.make re.position r; Expr.make re.position (Expr.Seq [])]
      else
        Expr.Alt [Expr.make re.position r; Expr.make re.position (Expr.Filter immediate)]
    | Concat res ->
      Expr.Seq (List.rev_map (transl ~for_reduction) res)
    | Filter {lhs; rhs} ->
      let states = transl_filter g indices re.position ~lhs ~rhs in
      if IndexSet.is_empty states then
        warn re.position "No items match this filter";
      Expr.Filter states
  in
  let result = transl ~for_reduction:false re in
  (!all_cap, result)
