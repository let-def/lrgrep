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
    mutable reached : Redgraph.n indexset;
  }
  val lr1_trie_root : lr1_trie
  val compile_reduce_expr :
    RE.t ->
    Redgraph.n indexset * Lr1.n indexset
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
      length: int;
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
        ([], parse_component last :: tail)
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

    let parse filter =
      let last, tail = structure_filter filter in
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

  let match_sym sym = function
    | None -> true
    | Some sym' -> Misc.equal_index sym sym'

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
    let filter = Globbing.parse rhs in
    let matching_dots prod =
      let result = Globbing.extract (Production.rhs prod) filter in
      (*begin match IntSet.elements result with
        | [] -> ()
        | dots ->
          let rhs = Array.to_list (Production.rhs prod) in
          let rec annotate pos = function
            | (pos' :: dots), rhs when pos = pos' ->
              "." :: annotate pos (dots, rhs)
            | [], [] -> []
            | dots, [] -> assert (dots = []); []
            | dots, (x :: rhs) ->
              Symbol.name x :: annotate (pos + 1) (dots, rhs)
          in
          let txt = annotate 0 (dots, rhs) in
          warn position "filter matches %s: %s"
            (Nonterminal.to_string (Production.lhs prod))
            (String.concat " " txt)
        end;*)
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
    mutable reached: Redgraph.n indexset;
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
    Index.iter Redgraph.n (fun state ->
        let config = Redgraph.get_config state in
        let node = visit_trie root (config.top :: config.rest) in
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
      List.iter process_next (K.derive Lr1.all k)
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
      | Atom (capture, symbol, mark) ->
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
        RE.Set (set, cap, Usage.singleton mark)
      | Alternative res ->
        RE.Alt (List.map (transl ~for_reduction) res)
      | Repetition {expr; policy} ->
        RE.Star (transl ~for_reduction expr, policy)
      | Reduce {capture; policy; expr; mark} ->
        if for_reduction then
          error re.position "Reductions cannot be nested";
        (* print_cmon stderr (Front.Syntax.cmon_regular_expression expr);*)
        let re = transl ~for_reduction:true expr in
        let pattern, immediate = compile_reduce_expr re in
        (*warn re.position
            "Reduce pattern is matching %d/%d cases (and matches immediately for %d states)"
            (IndexSet.cardinal pattern) (cardinal Redgraph.state)
            (IndexSet.cardinal immediate);*)
        let capture, capture_end = match capture with
          | None -> IndexSet.empty, IndexSet.empty
          | Some name ->
            let capture_start = mk_capture Start_loc name in
            let capture_end = mk_capture End_loc name in
            (capture_start, capture_end)
        in
        let usage = Usage.singleton mark in
        let r = RE.Reduce (capture_end, {capture; pattern; policy; usage}) in
        if IndexSet.is_empty immediate then
          r
        else if immediate == Lr1.all then
          RE.Alt [RE.make re.position r; RE.make re.position (RE.Seq [])]
        else
          RE.Alt [RE.make re.position r; RE.make re.position (RE.Filter immediate)]
      | Concat res ->
        RE.Seq (List.rev_map (transl ~for_reduction) res)
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
