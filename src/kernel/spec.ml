open Utils
open Misc
open Fix.Indexing
open Info
open Regexp

module Clause = Unsafe_cardinal()
type ('g, 'r) clause = ('g * 'r) Clause.t

type ('g, 'r) clauses = {
  syntax : (('g, 'r) clause, Syntax.clause) vector;
  captures : (('g, 'r) clause, (Capture.n, Syntax.capture_kind * string) indexmap) vector;
}

module Branch = Unsafe_cardinal()
type ('g, 'r) branch = ('g * 'r) Branch.t

type ('g, 'r) branches = {
  clause: (('g, 'r) branch, ('g, 'r) clause index) vector;
  pattern: (('g, 'r) branch, Syntax.pattern) vector;
  expr: (('g, 'r) branch, 'g Expr.t) vector;
  of_clause : (('g, 'r) clause, ('g, 'r) branch indexset) vector;
  lookaheads : (('g, 'r) branch, 'g terminal indexset option) vector;
  br_captures : (('g, 'r) branch, Capture.n indexset) vector;
  total: ('g, 'r) branch Boolvector.t;
}

type 'g _rule = Rule : ('g, 'r) clauses * ('g, 'r) branches -> 'g _rule

let import_rule (type g) (info : g info)
    (viable : g Viable_reductions.t)
    (indices : g Transl.Indices.t)
    (trie : g Transl.Reductum_trie.t)
    (rule : Syntax.rule) : g _rule
  =
  let open (val info) in
  let open struct type r end in
  let module Clauses = Vector.Of_array(struct
      type a = Syntax.clause
      let array = Array.of_list rule.clauses
    end)
  in
  let module Branches = Vector.Of_array(struct
      type a = Clauses.n index * Syntax.pattern
      let array =
        Vector.mapi (fun clause syntax ->
            List.map (fun pattern -> (clause, pattern))
              syntax.Syntax.patterns
          ) Clauses.vector
        |> Vector.to_list
        |> List.flatten
        |> Array.of_list
    end)
  in
  (* Branch definitions *)
  let branch_count = Vector.length Branches.vector in
  let clause = Vector.map fst Branches.vector in
  let pattern = Vector.map snd Branches.vector in
  let of_clause =
    let index = ref 0 in
    let import clause =
      let count = List.length clause.Syntax.patterns in
      let first = Index.of_int branch_count !index in
      index := !index + count;
      let last = Index.of_int branch_count (!index - 1) in
      IndexSet.init_interval first last
    in
    Vector.map import Clauses.vector
  in
  let lookaheads =
    Vector.map (fun pattern ->
        match pattern.Syntax.lookaheads with
        | [] -> None
        | symbols ->
          let lookahead_msg =
            "Lookahead can either be a terminal or `first(nonterminal)'"
          in
          let sym_pattern (sym, pos) =
            match sym with
            | Syntax.Apply ("first", [sym]) ->
              begin match Symbol.prj (Transl.Indices.get_symbol indices pos sym) with
                | L t ->
                  let t = Terminal.to_string t in
                  failwith (lookahead_msg ^ "; in first(" ^ t ^ "), " ^
                            t ^ " is a terminal")
                | R n ->
                  Nonterminal.to_g n
                  |> Grammar.Nonterminal.first
                  |> List.map Terminal.of_g
              end
            | Syntax.Name _ ->
              begin match Symbol.prj (Transl.Indices.get_symbol indices pos sym) with
                | R n ->
                  failwith (lookahead_msg ^ "; " ^
                            Nonterminal.to_string n ^ " is a nonterminal")
                | L t -> [t]
              end
            | _ ->
              failwith lookahead_msg
          in
          Some (IndexSet.of_list (List.concat_map sym_pattern symbols))
      ) pattern
  in
  let total =
    Boolvector.init branch_count @@ fun br ->
    Option.is_none lookaheads.:(br) &&
    match Clauses.vector.:(clause.:(br)).action with
    | Syntax.Total _ -> true
    | Syntax.Partial _ -> false
    | Syntax.Unreachable -> true
  in
  let br_captures = Vector.make branch_count IndexSet.empty in
  let expr = Vector.make branch_count Expr.empty in
  (* Clause definitions *)
  let clause_count = Vector.length Clauses.vector in
  let captures =
    let gensym = Capture.gensym () in
    Vector.init clause_count @@ fun clause ->
    let capture_tbl = Hashtbl.create 7 in
    let capture_def = ref IndexMap.empty in
    let capture kind name =
      let key = (kind, name) in
      match Hashtbl.find_opt capture_tbl key with
      | Some index -> index
      | None ->
        let index = gensym () in
        Hashtbl.add capture_tbl key index;
        capture_def := IndexMap.add index key !capture_def;
        index
    in
    let translate_branch (br : Branches.n index) =
      let cap, exp =
        Transl.transl info viable indices trie ~capture pattern.:(br).expr
      in
      br_captures.:(br) <- cap;
      expr.:(br) <- exp;
    in
    IndexSet.iter translate_branch of_clause.:(clause);
    !capture_def
  in
  (* Packing *)
  let module Clauses_def = Clause.Eq(struct
      type t = g * r
      type n = Clauses.n
      let n = Vector.length Clauses.vector
    end) in
  let Refl = Clauses_def.eq in
  let module Branches_def = Branch.Eq(struct
      type t = g * r
      type n = Branches.n
      let n = Vector.length Branches.vector
    end) in
  let Refl = Branches_def.eq in
  let clauses = {syntax = Clauses.vector; captures} in
  let branches = {clause; pattern; expr; of_clause; lookaheads; br_captures; total} in
  Rule (clauses, branches)
