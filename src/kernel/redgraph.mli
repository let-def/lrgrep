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

(** Reduction graph interface

    This module exports the reduction graph data structures and functions for
    analyzing viable reductions in LR(1) parsers.

    Proceeds in two steps:
    - Compute the closure ϵ-reductions (reductions that do not consume any
      input token) for each LR(1) state.
      This analysis is local (it does not depend on the stack, only on the
      LR(1) state), and form a tree of possible sequence of ϵ-reductions,
      ending with optionl "pending", non-ϵ, reductions the need to consume
      states from the stack to proceed.
      This closure is represented by stack_tree's and reduction_closure's, and
      simplifies and speedups latter analyses.

    - Construct a graph whose edges are labelled by LR(1) states and which map
      an LR(1) stack suffix to the (sequences of) reductions applicable to
      this configuration.
      The paths of this graph enumerate all the stack suffixes that can be
      consumed by reducing. The process is repeated as long as a reduction is
      applicable, thus a right recursion [A → α A] translates to a cycle.
      The process also keeps track of lookahead symbols permitting each
      reduction to strictly simulate the behavior of an LR(1) automaton that
      possible went through conflict resolution.

    But to recognize a reduction pattern, we have to do the reverse mapping:
    the user provides the target of a reduction (e.g. I want to reduce an
    expression), and we need to find the paths that can reach this target.
    So we introduce a "target" abstraction to which a reduction pattern
    translate to, a reverse index [target_trie] to go from a pattern to a
    set of targets, and we associate to each node of the graph the reachable
    targets.

    Type definitions:

    - 'g stack_tree: Tree structure of possible ϵ-reduction stacks for an LR state
      - [next]: Subtrees reachable after reductions
      - [reductions]: Pending non ϵ-reductions at each node, grouped by depth

    - 'g reduction_closure: Complete ϵ-reductions information for an LR state
      - [accepting], [failing]: Lookaheads that cause acceptance/failure
      - [stacks]: Stack trees of ϵ-reductions
      - [all_stacks], [all_reductions]: flattened ϵ-stacks and ϵ-reductions

    - 'g target_trie: Trie for indexing reduction targets reached by sequences of LR(1) states.
      E.g. if there is a goto transition `s0 -> s1` labelled `expression`, there will be
        a path `s0 -> s1` labelled `expression target` in the trie.
      - [sub]: Child nodes for each LR state
      - [immediates]: States from which the reductions are immediate (ϵ-reductions by definition)
      - [targets]: Targets reached by the current prefix.

    - 'g graph: Minimized reduction graph
      - Contains cells with reduction information at each position
      - Each step represents a position in the reduction computation

    - 'g action: What to do at a given step of a reduction sequence
      - [Advance]: Move to next step (simulate a "pop" action on the stack, it
        does not depend on the actual stack contents)
      - [Switch]: Transition to different goto targets (simulate a "goto" action
        on the stack, which depends on the state at the top of the stack)

    Key functions:

    - [close_lr1_reductions]: Compute reduction closures for all LR1 states
    - [index_targets]: Build target trie and map targets to goto transitions
    - [make]: Construct the minimized reduction graph
    - [initial], [follow]: Navigate the reduction graph

    Tricky implementation details:

    - The reduction graph is used to compute lookahead-dependent reduction
      sequences. Each cell represents either a (state, lookahead) configuration,
      or an intermediate step in a reduction sequence given by a triple (state,
      depth, lookahead) (a non-deterministic transition which applies if
      `state` is `depth` states deep in the stack).

    - The target trie enables efficient lookup of which reductions are
      reachable.

    - The minimization via Valmari's algorithm preserves the reachability
      structure needed for cost computation while reducing state space.

    - The [filter_reductions] function updates reduction lookahead sets when
      the lookahead domain is restricted to preserve LR(1) behaviors.
*)

open Fix.Indexing
open Utils
open Misc
open Info

type 'g stack_tree = {
  subs: ('g lr1 index list * 'g terminal indexset * 'g stack_tree) list;
} [@@ocaml.unboxed]

type 'g reduction_closure = {
  accepting: 'g terminal indexset;
  failing: 'g terminal indexset;
  reductions: ('g nonterminal, 'g terminal indexset) indexmap list;
  stacks: 'g stack_tree;
}

type ('g, 'n) reduction_closures = ('n, 'g reduction_closure) vector

val close_lr1_reductions : 'g grammar -> ('g, 'g lr1) reduction_closures

(*val close_goto_reductions
  :  'g grammar
  -> ('g, 'g lr1) reduction_closures
  -> ('g, 'g goto_transition) reduction_closures*)

val dump_closure
  :  ?failing:bool
  -> 'g grammar
  -> ('n index -> string)
  -> ('n, 'g reduction_closure) vector
  -> unit

type 'g target

type 'g targets = ('g target, 'g terminal indexset) indexmap

type 'g target_trie = private {
  mutable sub: ('g lr1, 'g target_trie) indexmap;
  mutable immediates: 'g lr1 indexset;
  mutable targets: ('g lr1, 'g target index) indexmap;
}

val index_targets
  :  'g grammar
  -> ('g, 'g lr1) reduction_closures
  -> 'g target_trie * ('g goto_transition, 'g targets) vector

type 'g graph

val make
  :  'g grammar
  -> ('g, 'g lr1) reduction_closures
  -> ('g goto_transition, 'g targets) vector
  -> 'g graph

type 'g step

type 'g transition = {
  reached: 'g target indexset;
  reachable: 'g target indexset;
  step: 'g step index;
}

type 'g action =
  | Advance of 'g step index
  | Switch of ('g lr1, 'g transition list) indexmap

val initial : 'g graph -> 'g lr1 index -> 'g transition list

val follow : 'g graph -> 'g step index -> 'g action
