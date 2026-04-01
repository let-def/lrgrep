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

(** LRC (LR with classes)

    This module computes a refinement of LR(1) states by reachable "lookahead-classes".
    When an LR(1) automaton is pruned to resolve conflicts, some sequence of
    transitions might become unreachable, while the individual transitions
    themselves stay reachable.
    For instance a sequence of states s0 -> s1 -> s2 on the stack might be
    impossible to reach, while s0 -> s1 and s1 -> s2 are individually reachable
    because there is lookahead that permit to reach both consecutively.
    Admittedly, this only happens for severely pruned automaton and not matter
    much in practice. Most of the time, LRC after minimization coincide with
    LR(1) states.
    TODO: Maybe I could drop LRC to simplify things and accept the rare
    over-approximations of reachable stacks? I should quantify the problem
    by measuring how often LRC diverge from LR(1).

    Type definitions:

    - 'g n: type-level index for LRC states
    - ('g, 'n) t: LRC structure containing:
      - [lr1_of]: Mapping from LRC states to their underlying LR1 states
      - [lrcs_of]: Mapping from LR1 states to sets of LRC states
      - [all_wait], [all_leaf]: Sets of wait and leaf LRC states
      - [all_successors], [reachable_from]: Transition relations

    - 'n entrypoints: Reachability information starting from specific states:
      - [reachable]: States reachable from entrypoints
      - [wait]: Wait states among reachable states
      - [entrypoints]: The initial entrypoint states
      - [successors], [predecessors]: Transition relations
      - [some_prefix]: Computing prefixes to reach states

    - 'g mlrc: Minimal LRC states (minimized via Valmari's algorithm)

    Main functions:

    - [make]: Build LRC structure from grammar and reachability analysis
    - [make_minimal]: Build minimal LRC structure with state minimization
    - [from_entrypoints]: Compute reachability from specific entrypoints
    - [check_deterministic]: Verify the LRC automaton is deterministic (for debugging)
    - [check_equivalence]: Check equivalence of two LRC structures (for debugging)

    Tricky details:

    - The LRC abstraction allows reasoning about which (LR1 state, lookahead
      class) pairs are reachable after conflict resolution.

    - The [make_minimal] function uses Valmari's algorithm to quotient
      equivalent LRC states, significantly reducing memory usage.

    - The [some_prefix] function computes minimal-length paths to each state,
      and coverage analysis.
    FIXME someday: Prefixes are minimal in number of symbols, not in number of
    terminals, which is ultimately what we care about when generating
    counter-examples. A short prefix can expend to a long sentence.
*)

open Fix.Indexing
open Utils
open Misc
open Info

type 'g n

type ('g, 'n) t = {
  lr1_of: ('n, 'g lr1 index) vector;
  lrcs_of: ('g lr1, 'n indexset) vector;
  all_wait: 'n indexset;
  all_leaf: 'n indexset;
  all_successors: ('n, 'n indexset) vector;
  reachable_from: ('n, 'n indexset) vector;
}

val make : 'g grammar -> 'g Reachability.t -> ('g, 'g n) t
val to_string : 'g grammar -> ('g, 'g n) t -> 'g n index -> string
val set_to_string : 'g grammar -> ('g, 'g n) t -> 'g n indexset -> string

type 'n entrypoints = {
  reachable: 'n indexset;
  wait: 'n indexset;
  entrypoints: 'n indexset;
  successors: ('n, 'n indexset) vector;
  predecessors: ('n, 'n indexset) vector;
  some_prefix: 'n index -> 'n index list;
}

val from_entrypoints : 'g grammar -> ('g, 'n) t -> 'n indexset -> 'n entrypoints

val check_deterministic : 'g grammar -> 'g Reachability.t -> unit

type 'g mlrc
val make_minimal : 'g grammar -> 'g Reachability.t -> ('g, 'g mlrc) t

val check_equivalence : 'g grammar -> ('g, 'n1) t -> ('g, 'n2) t -> 'n1 indexset -> 'n2 indexset -> unit
