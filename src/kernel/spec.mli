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
}

val branch_count : ('g, 'r) branches -> ('g, 'r) branch cardinal

type 'g _rule = Rule : ('g, 'r) clauses * ('g, 'r) branches -> 'g _rule

val import_rule : 'g grammar ->
  ('g, 'g lr1) Redgraph.graph ->
  'g Transl.Indices.t ->
  'g Redgraph.trie ->
  Syntax.rule -> 'g _rule
