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

(** Internally, an order chain is represented as a mutable singly-linked list. *)

type chain = {
  mutable value: int;  (* Stores the value of the chain element; -1 if not frozen *)
  mutable next: chain; (* Points to the next element in the chain *)
}

(* The same type is seen as a chain and as an element of the chain.
   The first element of the chain is used as representative of the whole
   chain. *)
type t = chain
type element = chain

(* Sentinel element that marks the end of all chains. It is self-referential. *)
let rec sentinel = {value = -2; next = sentinel}

(* Creates a new chain with a single element
   (itself, and [next] points to the sentinel). *)
let make () = {value = -1; next = sentinel}

(* Returns the chain seen as an element. *)
let root (x : t) : element = x

(* Extends the chain by adding a new element to the end.
   Fails if the chain has already been frozen. *)
let extend c =
  if c.value <> -1 then
    invalid_arg "Order_chain.extend: chain has already been frozen";
  let next = {value = -1; next = c.next} in
  c.next <- next;
  next

(* Returns the next element in the chain, extending it if necessary.
   Fails if the chain has already been frozen. *)
let next c =
  if c.value <> -1 then
    invalid_arg "Order_chain.next: chain has already been frozen";
  if c.next != sentinel then
    c.next
  else
    extend c

(* Recursively freezes the chain by assigning increasing integer values to each
   element.
   Fails if the chain has already been frozen. *)
let rec freeze_rec t i =
  if t.value <> -1 then
    invalid_arg "Order_chain.freeze: chain has already been frozen";
  t.value <- i;
  let i = i + 1 in
  if t.next != sentinel then
    freeze_rec t.next i
  else
    i

(* Initiates the freezing process of the chain starting with value 0. *)
let freeze t = freeze_rec t 0

(* Evaluates the value of a chain element.
   Fails if the chain has not been frozen. *)
let evaluate elt =
  if elt.value = -1 then
    invalid_arg "Order_chain.evaluate: chain has not been frozen";
  elt.value
