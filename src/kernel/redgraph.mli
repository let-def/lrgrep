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
open Info

type 'g reduction_closure = {
  failing: 'g terminal indexset;
  reductions: ('g nonterminal, 'g terminal indexset) indexmap list;
  stacks: ('g lr1 index list * 'g terminal indexset) list;
}

type ('g, 'n) reduction_closures = ('n, 'g reduction_closure) vector

val close_lr1_reductions : 'g grammar -> ('g, 'g lr1) reduction_closures

val close_goto_reductions
  :  'g grammar
  -> ('g, 'g lr1) reduction_closures
  -> ('g, 'g goto_transition) reduction_closures

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
