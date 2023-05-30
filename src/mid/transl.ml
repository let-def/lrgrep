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
    val states_by_item_suffix : Symbol.n index -> Lr1.n indexset
    val linearize_symbol : symbol -> string
    val find_linearized_symbol : string -> Symbol.t option
    val find_symbol : symbol -> Symbol.t option
    val get_symbol : position -> symbol -> Symbol.t
  end
  val match_sym : 'a index -> 'a index option -> bool
  val transl_filter :
    position ->
    lhs:symbol option ->
    pre_anchored:bool ->
    prefix:symbol option list ->
    suffix:symbol option list ->
    post_anchored:bool -> Lr1.n indexset
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

    let states_of_symbol = Vector.make Symbol.n IndexSet.empty

    let () =
      Index.iter Lr1.n (fun lr1 ->
          match Lr1.incoming lr1 with
          | None -> ()
          | Some sym -> vector_set_add states_of_symbol sym lr1
        )

    let states_of_symbol = Vector.get states_of_symbol

    let states_by_item_suffix = Vector.make Symbol.n IndexSet.empty

    let () =
      Index.iter Lr1.n (fun state ->
          List.iter (fun (prod, dot) ->
              if dot < Production.length prod then (
                vector_set_add states_by_item_suffix
                  (Production.rhs prod).(dot) state
              )
            ) (Lr1.items state)
        )

    let states_by_item_suffix = Vector.get states_by_item_suffix

    (* Map symbol names to actual symbols *)

    let linearize_symbol =
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

    let find_symbol name =
      find_linearized_symbol (linearize_symbol name)

    let get_symbol pos sym =
      match find_symbol sym with
      | None -> error pos "Unknown symbol %s" (linearize_symbol sym)
      | Some sym -> sym
  end

  let match_sym sym = function
    | None -> true
    | Some sym' -> Misc.equal_index sym sym'

  let transl_filter position ~lhs ~pre_anchored ~prefix ~suffix ~post_anchored =
    let transl_sym = Option.map (Indices.get_symbol position) in
    let lhs = transl_sym lhs in
    let prefix = List.rev_map transl_sym prefix in
    let suffix = List.map transl_sym suffix in
    let check_len anchored pat len =
      if anchored
      then pat = len
      else pat <= len
    in
    let len_pre = List.length prefix in
    let len_suf = List.length suffix in
    let check_item (prod, pos) =
      let rhs = Production.rhs prod in
      check_len pre_anchored len_pre pos &&
      check_len post_anchored len_suf (Production.length prod - pos) &&
      match_sym (Symbol.inj_r (Production.lhs prod)) lhs &&
      list_foralli (fun i pat -> match_sym rhs.(pos + i) pat) suffix &&
      list_foralli (fun i pat -> match_sym rhs.(pos - i - 1) pat) prefix
    in
    let check_state state =
      List.exists check_item (Lr1.items state)
    in
    let candidates = Lr1.all in
    let candidates = match suffix with
      | Some x :: _ ->
        Lr1.intersect candidates (Indices.states_by_item_suffix x)
      | _ -> candidates
    in
    let candidates = match prefix with
      | Some x :: _ ->
        Lr1.intersect candidates (Indices.states_of_symbol x)
      | _ -> candidates
    in
    IndexSet.filter check_state candidates

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
      | Filter {lhs; pre_anchored; prefix; suffix; post_anchored} ->
        let states =
          transl_filter re.position ~lhs ~pre_anchored ~prefix ~suffix ~post_anchored
        in
        if IndexSet.is_empty states then
          warn re.position "No items match this filter";
        RE.Filter states
    in
    let result = transl ~for_reduction re in
    (!all_cap, result)
end
