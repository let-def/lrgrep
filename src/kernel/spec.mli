(* MIT License
 *
 * Copyright (c) 2025 Frédéric Bour
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 *
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *)

(** Specification interface

    This module exports the data structures that represent the user's
    error specification after parsing and resolution.

    Type definitions:

    - ('g, 'r) clause: A group of patterns sharing the same action (one clause in the grammar)
    - ('g, 'r) branch: A single pattern within a clause

    - clause_def: Additional metadata about a clause, mainly for handling %shortest groups:
      - [shortest]: Is the clause in a %shortest group
      - [new_group]: Is the clause the first of the group?
        If true, it starts a new priority group.

    - ('g, 'r) clauses: All clauses in a rule:
      - [definitions]: The clause definition for each clause
      - [captures]: The captured variables for each clause

    - ('g, 'r) branches: All branches across all clauses:
      - [clause], [pattern], [expr]: Branch properties.
        Pattern is the abstract syntax of the pattern being recognized, while
        expr is a lower-level representation of the pattern as a regular
        expression.
      - [of_clause]: Mapping from clauses to their branches
      - [lookaheads]: Optional lookahead constraints per branch
      - [br_captures]: Captured variables per branch
      - [is_total], [is_partial]: Whether branches are total/partial
      - [priority]: Priorities determine which branch should match when multiple
        branches succeed. Text order is used most of the time (first branch
        matches first), except for a %shortest group, in which case the branch
        that matches the first has the priority.

    - 'g _rule: A rule with all its clauses and branches

    Main functions:

    - [branch_count]: Return the number of branches
    - [import_rule]: Compile a Syntax.rule into the internal specification

    Implementation details:

    - The priority system allows specification of multiple clauses with
      different precedence. Later clauses have lower priority.

    - The [is_total] / [is_partial] distinction matters for coverage analysis:
      partial clauses only accept specific lookaheads, total clauses accept
      any lookahead.

    - The [import_rule] function handles various clause types:
      - Normal clauses (total)
      - Partial clauses with specific lookahead constraints
      - %shortest groups where all clauses share priority

    TODO: "Unreachable" clauses are not implemented at the moment.
*)

open Fix.Indexing
open Utils
open Misc
open Regexp
open Info

type ('g, 'r) clause
type ('g, 'r) branch

type clause_def = {
  new_group: bool;
  shortest: bool;
  syntax: Syntax.clause;
}

type ('g, 'r) clauses = {
  definitions : (('g, 'r) clause, clause_def) vector;
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
  priority: (('g, 'r) branch, ('g, 'r) branch opt index) vector;
}

val branch_count : ('g, 'r) branches -> ('g, 'r) branch cardinal

type 'g _rule = Rule : ('g, 'r) clauses * ('g, 'r) branches -> 'g _rule

val import_rule : 'g grammar ->
  'g Redgraph.graph ->
  'g Transl.Indices.t ->
  'g Redgraph.target_trie ->
  Syntax.rule -> 'g _rule
