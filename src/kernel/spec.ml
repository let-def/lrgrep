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
open Fix.Indexing
open Info
open Regexp

module Clause = Unsafe_cardinal()
type ('g, 'r) clause = ('g * 'r) Clause.t

type clause_def = {
  new_group: bool;
  shortest: bool;
  syntax: Syntax.clause;
}

type ('g, 'r) clauses = {
  definitions : (('g, 'r) clause, clause_def) vector;
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
  is_total: ('g, 'r) branch Boolvector.t;
  is_partial: ('g, 'r) branch Boolvector.t;
  priority: (('g, 'r) branch, ('g, 'r) branch opt index) vector;
}

type 'g _rule = Rule : ('g, 'r) clauses * ('g, 'r) branches -> 'g _rule

let import_rule (type g) (g : g grammar)
    (rg : g Redgraph.graph)
    (indices : g Transl.Indices.t)
    (trie : g Redgraph.target_trie)
    (rule : Syntax.rule) : g _rule
  =
  let open struct type r end in
  let module Clauses = Vector.Of_array(struct
      type a = clause_def
      let array =
        let index = function
          | [] -> []
          | [clause] -> [{new_group = true; shortest = false; syntax=clause}]
          | clauses ->
            List.mapi begin fun i (clause : Syntax.clause) ->
              begin match clause.action with
                | Syntax.Total _ -> ()
                | Syntax.Partial (pos, _) ->
                  Syntax.error pos
                    "%%partial clauses are not supported in a %%shortest group"
                | Syntax.Unreachable pos ->
                  Syntax.error pos
                    "unreachable \".\" clauses are not supported in a %%shortest group"
              end;
              List.iter begin fun pat ->
                match pat.Syntax.lookaheads with
                | [] -> ()
                | (_, pos) :: _ ->
                  Syntax.error pos
                    "lookahead constraints are not supported in a %%shortest group"
              end clause.patterns;
              {new_group = (i = 0); shortest = true; syntax=clause}
            end clauses
        in
        Array.of_list (List.concat_map index rule.clauses)
    end)
  in
  let module Branches = Vector.Of_array(struct
      type a = Clauses.n index * Syntax.pattern * bool * bool
      let array =
        Vector.mapi begin fun clause def ->
          List.mapi (fun i pattern -> (clause, pattern, i = 0 && def.new_group, def.shortest))
            def.syntax.patterns
        end Clauses.vector
        |> Vector.to_list
        |> List.flatten
        |> Array.of_list
    end)
  in
  (* Branch definitions *)
  let branch_count = Vector.length Branches.vector in
  let clause = Vector.map (fun (c,_,_,_) -> c) Branches.vector in
  let pattern = Vector.map (fun (_,p,_,_) -> p) Branches.vector in
  let priority =
    let last = ref Opt.none in
    Vector.mapi begin fun index (_,_,ng,sh) ->
      if ng then
        last := Opt.some index;
      if sh
      then Option.get (Index.pred !last)
      else !last
    end Branches.vector
  in
  let of_clause =
    let index = ref 0 in
    let import clause =
      let count = List.length clause.syntax.patterns in
      let first = Index.of_int branch_count !index in
      index := !index + count;
      let last = Index.of_int branch_count (!index - 1) in
      IndexSet.init_interval first last
    in
    Vector.map import Clauses.vector
  in
  let lookaheads =
    Vector.map begin fun pattern ->
      match pattern.Syntax.lookaheads with
      | [] -> None
      | symbols ->
        let lookahead_msg =
          "Lookahead can either be a terminal or `first(nonterminal)'"
        in
        let sym_pattern (sym, pos) =
          match sym with
          | Syntax.Apply ("first", [sym]) ->
            begin match Symbol.desc g (Transl.Indices.get_symbol g pos sym) with
              | T t ->
                let t = Terminal.to_string g t in
                Syntax.error pos "%s; in first(%s), %s is a terminal"
                  lookahead_msg t t
              | N n -> Nonterminal.first g n
            end
          | Syntax.Name _ ->
            begin match Symbol.desc g (Transl.Indices.get_symbol g pos sym) with
              | N n ->
                Syntax.error pos "%s; %s is a nonterminal"
                  lookahead_msg (Nonterminal.to_string g n)
              | T t -> IndexSet.singleton t
            end
          | _ ->
            Syntax.error pos "%s" lookahead_msg
        in
        Some (List.fold_left IndexSet.union IndexSet.empty (List.map sym_pattern symbols))
    end pattern
  in
  let is_partial =
    Boolvector.init branch_count begin fun br ->
      match Clauses.vector.:(clause.:(br)).syntax.action with
      | Syntax.Partial _ -> true
      | Syntax.Total _ -> false
      | Syntax.Unreachable pos ->
        Syntax.warn pos "unreachable clauses \"{.}\" are not yet supported";
        false
    end
  in
  let is_total =
    Boolvector.init branch_count begin fun br ->
      Option.is_none lookaheads.:(br) &&
      not (Boolvector.test is_partial br)
    end
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
      let cap, exp = Transl.transl g rg indices trie ~capture pattern.:(br).expr in
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
  let clauses = {definitions = Clauses.vector; captures} in
  let branches = {clause; pattern; expr; of_clause; lookaheads;
                  br_captures; is_total; is_partial; priority} in
  Rule (clauses, branches)

let branch_count branches = Vector.length branches.expr
