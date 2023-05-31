open Utils
open Misc
open Front.Syntax
open Fix.Indexing
open Regexp

let error {line; col} fmt =
  Printf.eprintf "Error line %d, column %d: " line col;
  Printf.kfprintf (fun oc -> output_char oc '\n'; flush oc; exit 1) stderr fmt

let warn {line; col} fmt =
  Printf.eprintf "Warning line %d, column %d: " line col;
  Printf.kfprintf (fun oc -> output_char oc '\n'; flush oc) stderr fmt

module type S = sig
  module Regexp : Regexp.S
  open Regexp
  open Info

  module Indices :
  sig
    val states_of_symbol : Symbol.n index -> Lr1.n indexset
    val string_of_symbol : symbol -> string
    val find_linearized_symbol : string -> Symbol.t option
    val find_symbol : symbol -> Symbol.t option
    val get_symbol : position -> symbol -> Symbol.t
  end
  val match_sym : 'a index -> 'a index option -> bool
  type capture_kind = Start_loc | End_loc | Value
  type lr1_trie = {
    mutable sub : lr1_trie Lr1.map;
    mutable reached : Redgraph.state indexset;
  }
  val lr1_trie_root : lr1_trie
  val compile_reduce_expr :
    RE.t ->
    Redgraph.state indexset * Lr1.n indexset
  val transl :
    capture:(capture_kind -> string -> Capture.n index) ->
    for_reduction:bool ->
    regular_expr -> Capture.n indexset * RE.t
end

module Make(Regexp : Regexp.S) : S with module Regexp = Regexp =
struct
  module Regexp = Regexp
  open Regexp
  open Info

  (* Index LR(1) states by incoming symbol, goto transitions, items, ... *)

  module Indices =
  struct
    (* Group by incoming symbol *)

    let states_of_symbol =
      let table = Vector.make Symbol.n IndexSet.empty in
      Index.iter Lr1.n (fun lr1 ->
          match Lr1.incoming lr1 with
          | None -> ()
          | Some sym -> vector_set_add table sym lr1
        );
      Vector.get table

    let symbol_of_string str =
      let rec parse_symbol str i0 i j =
        if i = j then
          (Name (String.sub str i0 (i - i0)), i)
        else
          match str.[i] with
          | ',' | ')' -> (Name (String.sub str i0 (i - i0)), i)
          | '(' ->
            let name = String.sub str i0 (i - i0) in
            let args, i = parse_args str (i + 1) j in
            Apply (name, args), i
          | _ -> parse_symbol str i0 (i + 1) j

      and parse_args str i j =
        let arg, i = parse_symbol str i i j in
        if i = j then
          failwith ("Malformed symbol " ^ str)
        else
          match str.[i] with
          | ',' ->
            let args, i = parse_args str (i + 1) j in
            (arg :: args, i)
          | ')' -> ([arg], (i + 1))
          | _ ->
            failwith ("Malformed symbol " ^ str)
      in
      fst (parse_symbol str 0 0 (String.length str))

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

    let find_linearized_symbol =
      let table = Hashtbl.create 7 in
      let add_symbol s = Hashtbl.add table (Symbol.name ~mangled:false s) s in
      Index.iter Symbol.n add_symbol;
      Hashtbl.find_opt table

    let _find_instances =
      let table = Hashtbl.create 7 in
      let get name = Option.value (Hashtbl.find_opt table name) ~default:[] in
      let add_symbol sym =
        match symbol_of_string (Symbol.name ~mangled:false sym) with
        | Name _ -> ()
        | Apply (name, args) ->
          Hashtbl.replace table name ((args, sym) :: get name)
      in
      Index.iter Symbol.n add_symbol;
      get

    let find_symbol name =
      find_linearized_symbol (string_of_symbol name)

    let find_symbols = function
      | None -> Symbol.all
      | Some name ->
        match find_linearized_symbol (string_of_symbol name) with
        | None -> IndexSet.empty
        | Some sym -> IndexSet.singleton sym

    let get_symbol pos sym =
      match find_symbol sym with
      | None -> error pos "Unknown symbol %s" (string_of_symbol sym)
      | Some sym -> sym

    let prod_by_lhs =
      let table = Vector.make Nonterminal.n IndexSet.empty in
      Index.rev_iter Production.n (fun prod ->
          vector_set_add table (Production.lhs prod) prod);
      Vector.get table

    let left_rec_nt_reflexive_closure =
      let table = Vector.make Nonterminal.n IndexSet.empty in
      let rec close acc nt =
        if IndexSet.mem nt acc then acc else
          let acc' = Vector.get table nt in
          if not (IndexSet.is_empty acc') then
            IndexSet.union acc acc'
          else
            let acc = IndexSet.add nt acc in
            IndexSet.fold
              (fun prod acc -> close_rhs (Production.rhs prod) 0 acc)
              (prod_by_lhs nt) acc
      and close_rhs rhs pos acc =
        if pos >= Array.length rhs then acc
        else match Symbol.prj rhs.(pos) with
          | L _ -> acc
          | R nt ->
            let acc = close acc nt in
            if Nonterminal.nullable nt then
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

    let states_by_items =
      let table = Vector.init Production.n
          (fun prod -> Array.make (1 + Production.length prod) IndexSet.empty)
      in
      Index.rev_iter Lr1.n (fun lr1 ->
          let register (prod, pos) =
            let prod = Vector.get table prod in
            prod.(pos) <- IndexSet.add lr1 prod.(pos)
          in
          let closure_items nt =
            (* TODO: Should we traverse nullable nt?! *)
            IndexSet.iter
              (fun prod -> register (prod, 0))
              (prod_by_lhs nt)
          in
          let kernel_item (prod, pos) =
            register (prod, pos);
            let rhs = Production.rhs prod in
            if pos < Array.length rhs then
              match Symbol.prj rhs.(pos) with
              | L _ -> ()
              | R nt ->
                IndexSet.iter closure_items
                  (left_rec_nt_reflexive_closure nt)
          in
          List.iter kernel_item (Lr1.items lr1)
      );
      Vector.get table
  end

  module Globbing = struct

    type glob_skip = {
      dot: bool;
      exact: glob_exact;
    }

    and glob_exact = {
      dots: IntSet.t;
      syms: Symbol.set array;
      skip: glob_skip option;
    }

    let parse_component comp =
      let rec loop dots syms pos = function
        | [] -> (dots, Array.of_list (List.rev syms))
        | `Dot :: rest -> loop (IntSet.add pos dots) syms pos rest
        | `Find sym :: rest -> loop dots (sym :: syms) (pos + 1) rest
      in
      loop IntSet.empty [] 0 comp

    let rec structure_filter = function
      | [] -> ([], [])
      | (Skip, _pos) :: rest ->
        let last, tail = structure_filter rest in
        let dots, syms = parse_component last in
        ([], {dots; syms; skip=None} :: tail)
      | (Dot, _pos) :: rest ->
        let last, tail = structure_filter rest in
        (`Dot :: last, tail)
      | (Find sym, pos) :: rest ->
        let last, tail = structure_filter rest in
        let set = Indices.find_symbols sym in
        if IndexSet.is_empty set then
          error pos "Unknown symbol %s" (Indices.string_of_symbol (Option.get sym));
        (`Find set :: last, tail)

    let normalize_filter = function
      | [] -> {dots=IntSet.empty; syms=[||]; skip=None}
      | [exact] -> exact
      | prefix :: rest ->
        let rec loop = function
          | [] -> assert false
          | [exact] ->
            {dot = false; exact}
          | {dots; syms=[||]; _} :: xs ->
            if IntSet.is_empty dots
            then loop xs
            else {(loop xs) with dot = true}
          | exact :: rest ->
            {dot = false; exact = {exact with skip = Some (loop rest)}}
        in
        {prefix with skip = Some (loop rest)}

    let sub_match rhs pos syms =
      let lsyms = Array.length syms in
      (pos + lsyms <= Array.length rhs) &&
      let rec loop pos i =
        (i >= lsyms) || (
          IndexSet.mem rhs.(pos) syms.(i) &&
          loop (pos + 1) (i + 1)
        )
      in
      loop pos 0

    let rec match_skip rhs pos skip =
      match skip.exact.skip with
      | None ->
        (* Match suffix *)
        let pos' = Array.length rhs - Array.length skip.exact.syms in
        (pos' >= pos) && sub_match rhs pos' skip.exact.syms
      | Some skip' ->
        (* Match substring *)
        let rec loop pos =
          (pos <= Array.length rhs) &&
          if not (sub_match rhs pos skip.exact.syms)
          then loop (pos + 1)
          else match_skip rhs (pos + Array.length skip.exact.syms) skip'
        in
        loop pos

    let add_dots pos exact dots =
      IntSet.fold (fun i acc -> IntSet.add (pos + i) acc) exact.dots dots

    let rec extract_skip rhs pos skip =
      match skip.exact.skip with
      | None ->
        (* Match suffix *)
        let pos' = Array.length rhs - Array.length skip.exact.syms in
        if (pos' >= pos) && sub_match rhs pos' skip.exact.syms then
          let dots =
            if skip.dot
            then IntSet.init_interval pos pos'
            else IntSet.empty
          in
          Some (add_dots pos' skip.exact dots)
        else
          None

      | Some skip' ->
        (* Match substring *)
        let rec loop_match i j =
          if j <= i then i
          else if sub_match rhs j skip.exact.syms &&
                  match_skip rhs (j + Array.length skip.exact.syms) skip' then
            j
          else
            loop_match i (j - 1)
        in
        let rec loop pos' =
          if pos' > Array.length rhs then None
          else if not (sub_match rhs pos' skip.exact.syms)
          then loop (pos' + 1)
          else
            match extract_skip rhs (pos' + Array.length skip.exact.syms) skip' with
            | None -> None
            | Some dots ->
              let dots =
                if skip.dot then
                  IntSet.union dots
                    (IntSet.init_interval pos' (loop_match pos' (Array.length rhs - 1)))
                else
                  dots
              in
              Some dots
        in
        loop pos

    let match_exact rhs pos exact =
      sub_match rhs pos exact.syms &&
      let pos' = pos + Array.length exact.syms in
      match exact.skip with
      | None -> Array.length rhs = pos'
      | Some skip -> match_skip rhs pos' skip

    let extract rhs exact =
      if not (sub_match rhs 0 exact.syms) then
        None
      else
        let pos' = Array.length exact.syms in
        match exact.skip with
        | None ->
          if Array.length rhs = pos'
          then Some exact.dots
          else None
        | Some skip ->
          match extract_skip rhs pos' skip with
          | None -> None
          | Some dots -> Some (IntSet.union dots exact.dots)
  end

  let match_sym sym = function
    | None -> true
    | Some sym' -> Misc.equal_index sym sym'

  type normalized_filter =
    | NDot of position
    | NSym of Symbol.set

  let normalize_rhs rhs =
    let rec norm_dup = function
      | [] -> []
      | (Skip, pos1) :: (Skip, pos2) :: rest ->
        warn pos2 "Redundant '...'";
        norm_dup ((Skip, pos1) :: rest)
      | (Dot, pos1) :: (Dot, pos2) :: rest ->
        warn pos2 "Redundant '.'";
        norm_dup ((Dot, pos1) :: rest)
      | (Find str, pos) :: rest ->
        let sym = Indices.find_symbols str in
        if IndexSet.is_empty sym then
          error pos "Unknown symbol %s" (Indices.string_of_symbol (Option.get str));
        begin match norm_dup rest with
          | x :: xs -> (NSym sym :: x) :: xs
          | [] -> [[NSym sym]]
        end
      | (Dot, pos1) :: rest ->
        begin match norm_dup rest with
          | (NDot pos2 :: x) :: xs ->
            warn pos2 "Redundant '.'";
            (NDot pos1 :: x) :: xs
          | x :: xs ->
            (NDot pos1 :: x) :: xs
          | [] ->
            [[NDot pos1]]
        end
      | (Skip, pos1) :: rest ->
        begin match norm_dup rest with
          | [] :: _ as result ->
            warn pos1 "Redundant '...'";
            result
          | [NDot pos1] :: [NDot pos2] :: xs ->
            warn pos2 "Redundant '...'";
            [NDot pos1] :: xs
          | [] -> [[]; []]
          | xs -> [] :: xs
        end
    in
    norm_dup rhs

  let glob_filter rhs filter =
    let rec match_test rhs pos = function
      | NDot _ :: seek ->
        match_test rhs pos seek
      | NSym syms :: seek ->
        if (pos < Array.length rhs) && IndexSet.mem rhs.(pos) syms
        then match_test rhs (pos + 1) seek
        else None
      | [] -> Some pos
    in
    let rec match_seek rhs pos test =
      match match_test rhs pos test with
      | None when pos < Array.length rhs ->
        match_seek rhs (pos + 1) test
      | x -> x
    in
    let rec test_length = function
      | NDot _ :: rest -> test_length rest
      | NSym _ :: rest -> 1 + test_length rest
      | [] -> 0
    in
    let rec match_rest_only rhs pos = function
      | [] -> pos = Array.length rhs
      | seek :: rest ->
        let length = test_length seek in
        let pos' = Array.length rhs - length in
        (pos' >= pos) &&
        match rest with
        | [] -> length = 0 || Option.is_some (match_test rhs pos' seek)
        | rest ->
          match match_seek rhs pos seek with
          | None -> false
          | Some pos -> match_rest_only rhs pos rest
    in
    let rec place_dots acc pos = function
      | [] -> acc
      | NDot _ :: rest -> place_dots (IntSet.add pos acc) pos rest
      | NSym _ :: rest -> place_dots acc (pos + 1) rest
    in
    let place_dots pos test acc = place_dots acc pos test in
    let rec match_rest_dot rhs pos = function
      | [] ->
        if pos = Array.length rhs
        then Some IntSet.empty
        else None
      | [[]] -> Some IntSet.empty
      | [] :: rest ->
        match_rest_dot rhs pos rest
      | [NDot _] :: rest ->
        begin match match_rest_dot rhs pos rest with
          | None -> None
          | Some dots ->
            let dots = ref (IntSet.add pos dots) in
            let pos = ref (pos + 1) in
            while !pos <= Array.length rhs &&
                  match_rest_only rhs !pos rest
            do
              dots := IntSet.add !pos !dots;
              incr pos
            done;
            Some !dots
        end
      | seek :: rest ->
        let length = test_length seek in
        let pos' = Array.length rhs - length in
        if pos' < pos then
          None
        else (match rest with
          | [] ->
            if length = 0 || Option.is_some (match_test rhs pos' seek)
            then Some (place_dots pos seek IntSet.empty)
            else None
          | rest ->
            match match_seek rhs pos seek with
            | None -> None
            | Some pos ->
              Option.map (place_dots (pos - length) seek)
                (match_rest_dot rhs pos rest)
          )
    in
    let match_dot rhs = function
      | [test] ->
        let length = test_length test in
        if Array.length rhs = length then
          Some (place_dots 0 test IntSet.empty)
        else
          None
      | [] :: rest -> match_rest_dot rhs 0 rest
      | prefix :: rest ->
        Option.map (place_dots 0 prefix) (match_rest_dot rhs 0 rest)
      | [] -> None
    in
    Option.value (match_dot rhs filter) ~default:IntSet.empty

  let transl_filter position ~lhs ~rhs =
    let transl_sym = Option.map (Indices.get_symbol position) in
    let lhs = transl_sym lhs in
    let prods = match lhs with
      | None -> Production.all
      | Some lhs ->
        match Symbol.prj lhs with
        | L _ -> error position "left-handside of a filter should be a non-terminal"
        | R n -> Indices.prod_by_lhs n
    in
    let filter = normalize_rhs rhs in
    let matching_dots prod =
      let result = glob_filter (Production.rhs prod) filter in
      begin match IntSet.elements result with
        | [] -> ()
        | dots ->
          let rhs = Array.to_list (Production.rhs prod) in
          let rec annotate pos = function
            | (pos' :: dots), rhs when pos = pos' ->
              "." :: annotate pos (dots, rhs)
            | [], [] -> []
            | dots, [] -> "WEIRD @" :: List.map string_of_int dots
            | dots, (x :: rhs) ->
              Symbol.name x :: annotate (pos + 1) (dots, rhs)
          in
          let txt = annotate 0 (dots, rhs) in
          warn position "filter matches %s: %s"
            (Nonterminal.to_string (Production.lhs prod))
            (String.concat " " txt)
      end;
      result
    in
    let matching_states prod =
      IntSet.fold
        (fun pos acc -> IndexSet.union (Indices.states_by_items prod).(pos) acc)
        (matching_dots prod) IndexSet.empty
    in
    indexset_bind prods matching_states

  type capture_kind =
    | Start_loc
    | End_loc
    | Value

  type lr1_trie = {
    mutable sub: lr1_trie Lr1.map;
    mutable reached: Redgraph.state indexset;
  }

  let lr1_trie_root =
    let root = {sub = IndexMap.empty; reached = IndexSet.empty} in
    let rec visit_trie node = function
      | [] -> node
      | x :: xs ->
        let node' = match IndexMap.find_opt x node.sub with
          | Some node' -> node'
          | None ->
            let node' = {sub = IndexMap.empty; reached = IndexSet.empty} in
            node.sub <- IndexMap.add x node' node.sub;
            node'
        in
        visit_trie node' xs
    in
    Index.iter Redgraph.state (fun state ->
        let top, rest = Redgraph.get_stack state in
        let node = visit_trie root (top :: rest) in
        node.reached <- IndexSet.add state node.reached
      );
    root

  let compile_reduce_expr re =
    let reached = ref IndexSet.empty in
    let immediate = ref IndexSet.empty in
    let rec step node k =
      let process_next = function
        | (label, None) ->
          if node == lr1_trie_root then
            immediate := IndexSet.union !immediate label.K.filter
          else
            reached := (
              if IndexSet.equal Lr1.all label.filter then
                IndexSet.union node.reached !reached
              else
                IndexMap.fold (fun lr1 node' acc ->
                    if IndexSet.mem lr1 label.filter
                    then IndexSet.union acc node'.reached
                    else acc
                  ) node.sub !reached
            )
        | (label, Some k') ->
          IndexMap.iter (fun lr1 node' ->
              if IndexSet.mem lr1 label.K.filter then
                step node' k'
            ) node.sub
      in
      List.iter process_next (K.derive k)
    in
    step lr1_trie_root (K.More (re, K.Done));
    (!reached, !immediate)

  let transl ~capture ~for_reduction re =
    let all_cap = ref IndexSet.empty in
    let mk_capture kind name =
      let index = capture kind name in
      all_cap := IndexSet.add index !all_cap;
      IndexSet.singleton index
    in
    let rec transl ~for_reduction re =
      RE.make re.position @@
      match re.desc with
      | Atom (capture, symbol) ->
        if for_reduction && Option.is_some capture then
          error re.position "Captures are not allowed inside reductions";
        let set = match symbol with
          | None -> Lr1.all
          | Some sym ->
            let sym = Indices.get_symbol re.position sym in
            if for_reduction && Symbol.is_terminal sym then
              warn re.position "A reduction can only match non-terminals";
            Indices.states_of_symbol sym
        in
        let cap = match capture with
          | None -> IndexSet.empty
          | Some name -> mk_capture Value name
        in
        RE.Set (set, cap)
      | Alternative res ->
        RE.Alt (List.map (transl ~for_reduction) res)
      | Repetition {expr; policy} ->
        RE.Star (transl ~for_reduction expr, policy)
      | Reduce {capture; policy; expr} ->
        if for_reduction then
          error re.position "Reductions cannot be nested";
        (* print_cmon stderr (Front.Syntax.cmon_regular_expression expr);*)
        let re = transl ~for_reduction:true expr in
        let pattern, immediate = compile_reduce_expr re in
        (*warn re.position
            "Reduce pattern is matching %d/%d cases (and matches immediately for %d states)"
            (IndexSet.cardinal pattern) (cardinal Redgraph.state)
            (IndexSet.cardinal immediate);*)
        let capture0, capture = match capture with
          | None -> IndexSet.empty, IndexSet.empty
          | Some name ->
            let capture0 = mk_capture Start_loc name in
            let capture = mk_capture End_loc name in
            (capture0, capture)
        in
        let r = RE.Reduce (capture0, {capture; pattern; policy}) in
        if IndexSet.is_empty immediate then
          r
        else if immediate == Lr1.all then
          RE.Alt [RE.make re.position r; RE.make re.position (RE.Seq [])]
        else
          RE.Alt [RE.make re.position r; RE.make re.position (RE.Filter immediate)]
      | Concat res ->
        RE.Seq (List.map (transl ~for_reduction) res)
      | Filter {lhs; rhs} ->
        let states =
          transl_filter re.position ~lhs ~rhs
        in
        if IndexSet.is_empty states then
          warn re.position "No items match this filter";
        RE.Filter states
    in
    let result = transl ~for_reduction re in
    (!all_cap, result)
end
