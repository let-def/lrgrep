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

(** Compact representation of reduction positions

    This module provides an efficient compact representation for positions in
    reductions of the form (n, A) where:
    - n is the number of stack elements to pop
    - A is the nonterminal symbol for the goto transition

    The position is interpreted as: "pop n elements from the stack before
    following the goto transition labeled A".

    Design and implementation:

    - The table structure uses a contiguous memory layout where positions for
      each nonterminal are allocated consecutively. This enables O(1) access
      and efficient cache usage.

    - [inj] converts a (nonterminal, position) pair to an index into the
      table.

    - [prj] converts an index back to (nonterminal, position).

    - [previous] returns whether the position is at the start of a reduction
      (Left nt means we're at the start and need to follow goto nt) or in the
      middle of a reduction (Right pos gives the previous position).

    - [is_zero] checks if we're at the start of a reduction (position 0).

    Tricky implementation details:

    - The zero position for each nonterminal is stored separately to enable
      efficient lookups when we need to start a reduction.

    - Index arithmetic is used for compactness: positions are offsets from
      the zero position of the associated nonterminal.

    - The [previous'] function handles None (for Optional type) in addition
      to the regular case, enabling use with optional positions.
*)

open Utils
open Misc
open Fix.Indexing
open Info

(*
  Compact representation of a position in a reduction
  (a pair `(n, A)` interpreted as `pop n elements before
  following the goto transition labelled `A`).
*)

include Unsafe_cardinal()

(** A position as a (nonterminal, offset) pair.
    The offset is the number of stack elements to pop before
    following the goto transition labeled by the nonterminal. *)
type 'g desc = 'g nonterminal index * int

(** Table mapping compact indices to (nonterminal, offset) positions.
    [desc] stores positions contiguously per nonterminal for O(1) access.
    [zero] maps each nonterminal to the index of its position-0 entry,
    enabling fast injection of (nt, 0) pairs. *)
type 'g table = {
  desc: ('g t, 'g desc) vector;
  zero: ('g nonterminal, 'g t index) vector;
}

(** Build a position table from a grammar.
    For each nonterminal, allocates contiguous slots equal to the maximum
    RHS length of its productions, plus a sentinel at index 0.
    Total cardinality = 1 + sum of max production lengths per nonterminal. *)
let make (type g) (g : g grammar) : g table =
  let length = Vector.make (Nonterminal.cardinal g) 0 in
  Index.iter (Production.cardinal g) (fun prod ->
      length.@(Production.lhs g prod) <- Int.max (Production.length g prod)
    );
  let open Const(struct
      type t = g
      let cardinal =
        Vector.fold_left (+) (1 + Vector.length_as_int length) length
    end)
  in
  let desc = Vector.make' n (fun () -> Index.of_int (Nonterminal.cardinal g) 0, 0) in
  let enum = Index.enumerate n in
  let zero = Vector.mapi (fun nt count ->
      let zero = enum () in
      desc.:(zero) <- (nt, 0);
      for i = 1 to count do
        desc.:(enum ()) <- (nt, i);
      done;
      zero
    ) length
  in
  {desc; zero}

(** Inject a (nonterminal, offset) pair into a compact table index.
    Raises [Assert_failure] if the offset is out of range. *)
let inj (type g) (p : g table) nt pos =
  assert (pos >= 0);
  let p0 = p.zero.:(nt) in
  let pn = Index.of_int (Vector.length p.desc) ((p0 :> int) + pos) in
  let (nt', pos') = p.desc.:(pn)  in
  assert (Index.equal nt nt');
  assert (pos = pos');
  pn

(** Project a compact table index back to its (nonterminal, offset) pair. *)
let prj (type g) (p : g table) pos =
  p.desc.:(pos)

(** Navigate to the predecessor position in a reduction.
    Returns [Left nt] if at position 0 for nonterminal [nt] (start of
    a reduction — follow the goto transition labeled [nt]).
    Returns [Right pos'] if at position > 0 (middle of a reduction —
    [pos'] is the previous position, obtained via O(1) [Index.pred]). *)
let previous (type g) (p : g table) pos =
  match p.desc.:(pos) with
  | (nt, 0) -> Either.Left nt
  | _ -> Either.Right (Option.get (Index.pred pos))

(** Like [previous] but accepts an optional index.
    Returns [Right Opt.none] when the input is [None].
    Used in reduction graphs where positions may be absent. *)
let previous' (type g) (p : g table) pos =
  match Opt.prj pos with
  | None -> Either.Right Opt.none
  | Some pos' ->
    match p.desc.:(pos') with
    | (nt, 0) -> Either.Left nt
    | _ -> Either.Right (Option.get (Index.pred pos))

(** Check whether the position is at offset 0 (start of a reduction). *)
let is_zero (type g) (p : g table) pos =
  let _, pos = p.desc.:(pos) in
  (pos = 0)
