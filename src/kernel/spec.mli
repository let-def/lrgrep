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

open Fix.Indexing
open Utils
open Misc
open Regexp
open Info

type ('g, 'r) clause
type ('g, 'r) branch

type ('g, 'r) clauses = {
  syntax : (('g, 'r) clause, Syntax.clause) vector;
  captures : (('g, 'r) clause, (Capture.n, Syntax.capture_kind * string) indexmap) vector;
}

type ('g, 'r) branches = {
  clause: (('g, 'r) branch, ('g, 'r) clause index) vector;
  pattern: (('g, 'r) branch, Syntax.pattern) vector;
  expr: (('g, 'r) branch, 'g Expr.t) vector;
  of_clause : (('g, 'r) clause, ('g, 'r) branch indexset) vector;
  lookaheads : (('g, 'r) branch, 'g terminal indexset option) vector;
  br_captures : (('g, 'r) branch, Capture.n indexset) vector;
  is_total: ('g, 'r) branch Boolvector.t;
  is_partial: ('g, 'r) branch Boolvector.t;
}

val branch_count : ('g, 'r) branches -> ('g, 'r) branch cardinal

type 'g _rule = Rule : ('g, 'r) clauses * ('g, 'r) branches -> 'g _rule

val import_rule : 'g grammar ->
  'g Redgraph.graph ->
  'g Transl.Indices.t ->
  'g Redgraph.target_trie ->
  Syntax.rule -> 'g _rule
