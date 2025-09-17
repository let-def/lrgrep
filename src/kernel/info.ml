(* MIT License
 *
 * Copyright (c) 2025 Frédéric Bour
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
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

(** This module defines data structures and operations for handling grammar
  information in a structured way. It includes representations for terminals,
  non-terminals, productions, and LR states, along with their transitions and
  reductions. The module is designed to work with Menhir's grammar
  representation and extends it with additional functionality for
  convenience. *)

open Utils
open Misc
open Fix.Indexing

module type GRAMMAR = MenhirSdk.Cmly_api.GRAMMAR

module UC_terminal = Unsafe_cardinal()
module UC_nonterminal = Unsafe_cardinal()
module UC_production = Unsafe_cardinal()
module UC_lr0  = Unsafe_cardinal()
module UC_lr1  = Unsafe_cardinal()
module UC_item = Unsafe_cardinal()
module UC_goto_transition = Unsafe_cardinal()
module UC_shift_transition = Unsafe_cardinal()
module UC_reduction = Unsafe_cardinal()

type 'g terminal = 'g UC_terminal.t
type 'g nonterminal = 'g UC_nonterminal.t
type 'g symbol = ('g terminal, 'g nonterminal) Sum.n
type 'g production = 'g UC_production.t
type 'g item = 'g UC_item.t
type 'g lr0 = 'g UC_lr0.t
type 'g lr1 = 'g UC_lr1.t
type 'g goto_transition = 'g UC_goto_transition.t
type 'g shift_transition = 'g UC_shift_transition.t
type 'g transition = ('g goto_transition, 'g shift_transition) Sum.n
type 'g reduction = 'g UC_reduction.t

type 'g grammar = {
  raw: (module MenhirSdk.Cmly_api.GRAMMAR);
  terminal_n : 'g terminal cardinal;
  terminal_all: 'g terminal indexset;
  terminal_regular: 'g terminal indexset;
  nonterminal_n : 'g nonterminal cardinal;
  nonterminal_all: 'g nonterminal indexset;
  symbol_all : 'g symbol indexset;
  production_lhs : ('g production, 'g nonterminal index) vector;
  production_rhs : ('g production, 'g symbol index array) vector;
  production_all : 'g production indexset;
  item_productions : ('g item, 'g production index) vector;
  item_offsets : ('g production, int) vector;
  lr0_items : ('g lr0, 'g item indexset) vector;
  lr0_incoming : ('g lr0, 'g symbol index option) vector;
  lr0_is_entrypoint : ('g lr0, 'g production index option) vector;
  transition_source : ('g transition, 'g lr1 index) vector;
  transition_target : ('g transition, 'g lr1 index) vector;
  transition_shift_sym : ('g shift_transition, 'g terminal index) vector;
  (*transition_shift_table: ('g lr1, ('g terminal, 'g shift_transition index) indexmap) vector;*)
  transition_goto_sym : ('g goto_transition, 'g nonterminal index) vector;
  transition_goto_table: ('g lr1, ('g nonterminal, 'g goto_transition index) indexmap) vector;
  transition_predecessors: ('g lr1, 'g transition indexset) vector;
  transition_successors: ('g lr1, 'g transition indexset) vector;
  transition_accepting : 'g goto_transition indexset;
  lr1_all : 'g lr1 indexset;
  lr1_lr0 : ('g lr1, 'g lr0 index) vector;
  lr1_wait : 'g lr1 indexset;
  lr1_accepting : 'g lr1 indexset;
  lr1_reduce_on : ('g lr1, 'g terminal indexset) vector;
  lr1_shift_on : ('g lr1, 'g terminal indexset) vector;
  lr1_reject : ('g lr1, 'g terminal indexset) vector;
  lr1_entrypoints : 'g lr1 indexset;
  lr1_entrypoint_table : (string, 'g lr1 index) Hashtbl.t;
  lr1_predecessors : ('g lr1, 'g lr1 indexset lazy_stream) vector;
  reduction_state : ('g reduction, 'g lr1 index) vector;
  reduction_production : ('g reduction, 'g production index) vector;
  reduction_lookaheads : ('g reduction, 'g terminal indexset) vector;
  reduction_from_lr1 : ('g lr1, 'g reduction indexset) vector;
}

let raw g = g.raw

module Lift(G : MenhirSdk.Cmly_api.GRAMMAR) = struct
  type g

  module Import
      (UC : UNSAFE_CARDINAL)
      (M : sig type t  val count : int val of_int : int -> t val to_int : t -> int end) =
  struct
    include UC.Const(struct type t = g let cardinal = M.count end)
    let of_g i = Index.of_int n (M.to_int i)
    let to_g i = M.of_int (Index.to_int i)
    let all = IndexSet.all n
  end


  module Terminal = struct
    include Import(UC_terminal)(G.Terminal)
    let regular = IndexSet.init_from_set n (fun t ->
        match G.Terminal.kind (G.Terminal.of_int (t : _ index :> int)) with
        | `EOF | `REGULAR -> true
        | `PSEUDO | `ERROR -> false
      )
  end

  module Nonterminal = Import(UC_nonterminal)(G.Nonterminal)

  module Symbol = struct
    let n = Sum.cardinal Terminal.n Nonterminal.n
    let all = IndexSet.all n

    let of_g = function
      | G.T t -> Sum.inj_l (Terminal.of_g t)
      | G.N n -> Sum.inj_r Terminal.n (Nonterminal.of_g n)

    (*let to_g t = match Sum.prj Terminal.n t with
      | L t -> G.T (Terminal.to_g t)
      | R n -> G.N (Nonterminal.to_g n)*)
  end

  module Production = struct
    include Import(UC_production)(G.Production)

    let lhs = Vector.init n (fun p -> Nonterminal.of_g (G.Production.lhs (to_g p)))

    let rhs =
      Vector.init n @@ fun p ->
      Array.map
        (fun (sym,_,_) -> Symbol.of_g sym)
        (G.Production.rhs (to_g p))
  end

  module Item = struct
    let count = ref 0

    let offsets =
      Vector.init Production.n (fun prod ->
          let position = !count in
          count := !count + Array.length Production.rhs.:(prod) + 1;
          position
        )

    include UC_item.Const(struct type t = g let cardinal = !count end)

    let productions =
      Vector.make' n (fun () -> Index.of_int Production.n 0)

    let () =
      let enum = Index.enumerate n in
      Index.iter Production.n @@ fun prod ->
      for _ = 0 to Array.length Production.rhs.:(prod) do
        productions.:(enum ()) <- prod
      done
  end

  module Lr0 = struct
    include Import(UC_lr0)(G.Lr0)

    let items = Vector.init n @@ fun lr0 ->
      to_g lr0
      |> G.Lr0.items
      |> List.map (fun (p,pos) -> Index.of_int Item.n (Item.offsets.:(Production.of_g p) + pos))
      |> IndexSet.of_list

    let incoming = Vector.init n @@ fun lr0 ->
      to_g lr0
      |> G.Lr0.incoming
      |> Option.map Symbol.of_g

    let is_entrypoint =
      Vector.map (fun items ->
          if not (IndexSet.is_singleton items) then
            None
          else
            let item = IndexSet.choose items in
            let prod = Item.productions.:(item) in
            if Index.to_int item = Item.offsets.:(prod) then
              Some prod
            else
              None
        ) items
  end

  module Lr1 = struct
    include Import(UC_lr1)(G.Lr1)

    let lr0 = Vector.init n @@ fun lr1 ->
      Lr0.of_g (G.Lr1.lr0 (to_g lr1))
  end

  module Transition =
  struct
    let shift_count, goto_count =
      let shift_count = ref 0 in
      let goto_count = ref 0 in
      (* Count goto and shift transitions by iterating on all states and
         transitions *)
      G.Lr1.iter begin fun lr1 ->
        List.iter begin fun (sym, _) ->
          match sym with
          | G.T _ -> incr shift_count
          | G.N _ -> incr goto_count
        end (G.Lr1.transitions lr1)
      end;
      (!shift_count, !goto_count)

    module Goto = UC_goto_transition.Const(struct type t = g let cardinal = goto_count end)
    module Shift = UC_shift_transition.Const(struct type t = g let cardinal = shift_count end)

    let any = Sum.cardinal Goto.n Shift.n

    let of_goto = Sum.inj_l
    let of_shift = Sum.inj_r Goto.n

    (* Vectors to store information on states and transitions.

       We allocate a bunch of data structures (sources, targets, t_symbols,
       nt_symbols and predecessors vectors, t_table and nt_table hash tables),
       and then populate them by iterating over all transitions.
    *)

    let sources = Vector.make' any (fun () -> Index.of_int Lr1.n 0)
    let targets = Vector.make' any (fun () -> Index.of_int Lr1.n 0)

    let shift_sym = Vector.make' Shift.n (fun () -> Index.of_int Terminal.n 0)
    let goto_sym = Vector.make' Goto.n (fun () -> Index.of_int Nonterminal.n 0)

    (* Tables to associate a pair of a state and a symbol to a transition. *)

    let goto_table = Vector.make Lr1.n IndexMap.empty

    (*let shift_table = Vector.make Lr1.n IndexMap.empty*)

    (* A vector to store the predecessors of an lr1 state.
       We cannot compute them directly, we discover them by exploring the
       successor relation below. *)
    let predecessors = Vector.make Lr1.n IndexSet.empty

    let successors =
      (* We populate all the data structures allocated above, i.e.
         the vectors t_sources, t_symbols, t_targets, nt_sources, nt_symbols,
         nt_targets and predecessors, as well as the tables t_table and
         nt_table, by iterating over all successors. *)
      let next_goto = Index.enumerate Goto.n in
      let next_shift = Index.enumerate Shift.n in
      Vector.init Lr1.n begin fun source ->
        List.fold_right begin fun (sym, target) acc ->
          let target = Lr1.of_g target in
          let index = match sym with
            | G.T t ->
              let t = Terminal.of_g t in
              let index = next_shift () in
              shift_sym.:(index) <- t;
              (*shift_table.@(source) <- IndexMap.add t index;*)
              of_shift index
            | G.N nt ->
              let nt = Nonterminal.of_g nt in
              let index = next_goto () in
              goto_sym.:(index) <- nt;
              goto_table.@(source) <- IndexMap.add nt index;
              of_goto index
          in
          sources.:(index) <- source;
          targets.:(index) <- target;
          predecessors.@(target) <- IndexSet.add index;
          IndexSet.add index acc
        end (G.Lr1.transitions (Lr1.to_g source)) IndexSet.empty
      end

    let accepting =
      let acc = ref IndexSet.empty in
      Index.rev_iter Lr1.n begin fun lr1 ->
        match Lr0.is_entrypoint.:(Lr1.lr0.:(lr1)) with
        | None -> ()
        | Some prod ->
          let sym =
            match Sum.prj Terminal.n Production.rhs.:(prod).(0) with
            | L _ -> assert false
            | R nt -> nt
          in
          acc := IndexSet.fold_right (fun acc tr ->
              match Sum.prj Goto.n tr with
              | L gt when goto_sym.:(gt) = sym -> IndexSet.add gt acc
              | _ -> acc
            ) !acc successors.:(lr1)
      end;
      !acc
  end

  module Lr1_extra = struct
    open Lr1

    let accepting = ref IndexSet.empty

    (** The set of terminals that will trigger a reduction *)
    let reduce_on = Vector.init n @@ fun lr1 ->
      List.fold_left
        (fun acc (t, _) ->
           if G.Terminal.kind t = `PSEUDO then
             accepting := IndexSet.add lr1 !accepting;
           IndexSet.add (Terminal.of_g t) acc)
        IndexSet.empty (G.Lr1.get_reductions (to_g lr1))

    let accepting = !accepting

    (** The set of terminals that will trigger a shift transition *)
    let shift_on = Vector.init n @@ fun lr1 ->
      List.fold_left
        (fun acc (sym, _raw) ->
           match sym with
           | G.T t -> IndexSet.add (Terminal.of_g t) acc
           | G.N _ -> acc)
        IndexSet.empty (G.Lr1.transitions (to_g lr1))

    (** The set of terminals the state has no transition for *)
    let reject = Vector.init n @@ fun lr1 ->
      let result = Terminal.all in
      let result = IndexSet.diff result reduce_on.:(lr1) in
      let result = IndexSet.diff result shift_on.:(lr1) in
      result

    let wait = IndexSet.init_from_set n (fun lr1 ->
        match G.Lr0.incoming (Lr0.to_g lr0.:(lr1)) with
        | Some (G.N _) -> false
        | Some (G.T t) -> G.Terminal.kind t = `REGULAR && not (IndexSet.mem lr1 accepting)
        | None -> true
      )

    let predecessors = Vector.init n @@ fun lr1 ->
      IndexSet.map (fun tr -> Transition.sources.:(tr)) Transition.predecessors.:(lr1)

    let entrypoint_table =
      let table = Hashtbl.create 7 in
      Index.iter n (fun lr1 ->
        match Lr0.is_entrypoint.:(lr0.:(lr1)) with
        | None -> ()
        | Some prod ->
          let sym, _, _ = (G.Production.rhs (Production.to_g prod)).(0) in
          Hashtbl.add table (G.Symbol.name sym) lr1
      );
      table

    let entrypoints =
      Hashtbl.fold
        (fun _ lr1 acc -> IndexSet.add lr1 acc)
        entrypoint_table IndexSet.empty
  end

  module Reduction = struct
    let n = ref 0
    let raw =
      let import_red reds =
        reds
        |> List.filter_map (fun (t, p) ->
            match G.Production.kind p with
            | `START -> None
            | `REGULAR -> Some (Production.of_g p, Terminal.of_g t)
          )
        |> Misc.group_by
          ~compare:(fun (p1,_) (p2,_) -> compare_index p1 p2)
          ~group:(fun (p,t) ps -> p, IndexSet.of_list (t :: List.map snd ps))
        |> List.sort (fun (p1,_) (p2,_) ->
            let l1 = Array.length Production.rhs.:(p1) in
            let l2 = Array.length Production.rhs.:(p2) in
            let c = Int.compare l1 l2 in
            if c <> 0 then c else
              compare_index Production.lhs.:(p1) Production.lhs.:(p2)
          )
      in
      let import_lr1 lr1 =
        let reds = import_red (G.Lr1.get_reductions (Lr1.to_g lr1)) in
        n := !n + List.length reds;
        reds
      in
      Vector.init Lr1.n import_lr1

    include UC_reduction.Const(struct type t = g let cardinal = !n end)

    let state = Vector.make' n (fun () -> Index.of_int Lr1.n 0)
    let production = Vector.make' n (fun () -> Index.of_int Production.n 0)
    let lookaheads = Vector.make n IndexSet.empty
    let from_lr1 =
      let enum = Index.enumerate n in
      Vector.mapi (fun lr1 reds ->
          List.fold_left (fun set (prod, la) ->
              let i = enum () in
              state.:(i) <- lr1;
              production.:(i) <- prod;
              lookaheads.:(i) <- la;
              IndexSet.add i set
            ) IndexSet.empty reds
        ) raw
  end

  let grammar = {
    raw = (module G);
    terminal_n              = Terminal.n;
    terminal_all            = Terminal.all;
    terminal_regular        = Terminal.regular;
    nonterminal_n           = Nonterminal.n;
    nonterminal_all         = Nonterminal.all;
    symbol_all              = Symbol.all;
    production_lhs          = Production.lhs;
    production_rhs          = Production.rhs;
    production_all          = Production.all;
    item_productions        = Item.productions;
    item_offsets            = Item.offsets;
    lr0_items               = Lr0.items;
    lr0_incoming            = Lr0.incoming;
    lr0_is_entrypoint       = Lr0.is_entrypoint;
    transition_source       = Transition.sources;
    transition_target       = Transition.targets;
    transition_shift_sym    = Transition.shift_sym;
    (*transition_shift_table  = Transition.shift_table;*)
    transition_goto_sym     = Transition.goto_sym;
    transition_goto_table   = Transition.goto_table;
    transition_predecessors = Transition.predecessors;
    transition_successors   = Transition.successors;
    transition_accepting    = Transition.accepting;
    lr1_all                 = Lr1.all;
    lr1_lr0                 = Lr1.lr0;
    lr1_wait                = Lr1_extra.wait;
    lr1_accepting           = Lr1_extra.accepting;
    lr1_reduce_on           = Lr1_extra.reduce_on;
    lr1_shift_on            = Lr1_extra.shift_on;
    lr1_reject              = Lr1_extra.reject;
    lr1_entrypoints         = Lr1_extra.entrypoints;
    lr1_entrypoint_table    = Lr1_extra.entrypoint_table;
    lr1_predecessors        = Vector.init Lr1.n (fun x -> iterate (IndexSet.singleton x)
                                                    (fun x -> IndexSet.bind x (Vector.get Lr1_extra.predecessors)) );
    reduction_state         = Reduction.state;
    reduction_production    = Reduction.production;
    reduction_lookaheads    = Reduction.lookaheads;
    reduction_from_lr1      = Reduction.from_lr1;
  }
end

module type INDEXED = sig
  type 'g n

  val cardinal : 'g grammar -> 'g n cardinal
  val of_int : 'g grammar -> int -> 'g n index
end

module Terminal = struct
  type 'g n = 'g terminal

  let cardinal g = g.terminal_n
  let of_int g i = Index.of_int (cardinal g) i

  let to_string g i =
    let open (val g.raw) in
    Terminal.name (Terminal.of_int (Index.to_int i))

  let all g = g.terminal_all

  let regular g = g.terminal_regular

  let semantic_value g i =
    let open (val g.raw) in
    Terminal.typ (Terminal.of_int (Index.to_int i))

  let intersect g a b =
    if a == g.terminal_all then b
    else if b == g.terminal_all then a
    else IndexSet.inter a b

  let is_error g i =
    let open (val g.raw) in
    match Terminal.kind (Terminal.of_int (i : _ index :> int)) with
    | `ERROR -> true
    | _ -> false
end

module Nonterminal = struct
  type 'g n = 'g nonterminal

  let cardinal g = g.nonterminal_n
  let of_int g i = Index.of_int (cardinal g) i

  let all g = g.nonterminal_all

  let to_string g i =
    let open (val g.raw) in
    Nonterminal.name (Nonterminal.of_int (Index.to_int i))

  let kind g i =
    let open (val g.raw) in
    Nonterminal.kind (Nonterminal.of_int (Index.to_int i))

  let semantic_value g i =
    let open (val g.raw) in
    Nonterminal.typ (Nonterminal.of_int (Index.to_int i))

  let nullable g i =
    let open (val g.raw) in
    Nonterminal.nullable (Nonterminal.of_int (Index.to_int i))

  let first g i =
    let open (val g.raw) in
    Nonterminal.of_int (Index.to_int i)
    |> Nonterminal.first
    |> List.map (fun t -> Index.of_int g.terminal_n (Terminal.to_int t))
    |> IndexSet.of_list
end

module Symbol = struct
  type 'g n = 'g symbol

  let cardinal g = Sum.cardinal g.terminal_n g.nonterminal_n
  let of_int g i = Index.of_int (cardinal g) i

  type 'g desc =
    | T of 'g terminal index
    | N of 'g nonterminal index

  let prj g i = Sum.prj g.terminal_n i

  let desc g i =
    match prj g i with
    | L t -> T t
    | R n -> N n

  let is_terminal g t = match prj g t with
    | L _ -> true
    | R _ -> false

  let is_nonterminal g t = match prj g t with
    | L _ -> false
    | R _ -> true

  let name g ?mangled t =
    let open (val g.raw) in
    match prj g t with
    | L t -> symbol_name ?mangled (T (Terminal.of_int (Index.to_int t)))
    | R n -> symbol_name ?mangled (N (Nonterminal.of_int (Index.to_int n)))

  let semantic_value g t = match prj g t with
    | L t -> Some (Option.value (Terminal.semantic_value g t) ~default:"unit")
    | R n -> Nonterminal.semantic_value g n

  let all g = g.symbol_all

  let inj_t _ t = Sum.inj_l t
  let inj_n g n = Sum.inj_r g.terminal_n n
end

module Production = struct
  type 'g n = 'g production

  let cardinal g = Vector.length g.production_lhs
  let of_int g i = Index.of_int (cardinal g) i

  let lhs g i = g.production_lhs.:(i)
  let rhs g i = g.production_rhs.:(i)
  let length g i = Array.length (rhs g i)
  let kind g i =
    let open (val g.raw) in
    Production.kind (Production.of_int (Index.to_int i))
  let all g = g.production_all
end

(* Explicit representation of LR(0) items *)
module Item = struct
  type 'g n = 'g item

  let cardinal g = Vector.length g.item_productions
  let of_int g i = Index.of_int (cardinal g) i

  let make g prod pos =
    if pos < 0 || pos > Production.length g prod then
      invalid_arg "Info.Item.make: pos out of bounds";
    Index.of_int (cardinal g) (g.item_offsets.:(prod) + pos)

  let last g prod =
    make g prod (Production.length g prod)

  let production g i = g.item_productions.:(i)

  let position g i =
    ((i : _ index :> int) - g.item_offsets.:(production g i))

  let desc g i =
    let prod = production g i in
    (prod, (i : _ index :> int) - g.item_offsets.:(prod))

  let prev g (i : 'g n index) =
    match Index.pred i with
    | Some j when not (Index.equal (production g i) (production g j)) -> None
    | result -> result

  let is_reducible g i =
    let prod = production g i in
    ((i : _ index :> int) - g.item_offsets.:(prod)) =
    Production.length g prod

  let to_string g i =
    let prod, pos = desc g i in
    let b = Buffer.create 63 in
    Buffer.add_string b (Nonterminal.to_string g (Production.lhs g prod));
    Buffer.add_char b ':';
    let rhs = Production.rhs g prod in
    let add_sym sym =
      Buffer.add_char b ' ';
      Buffer.add_string b (Symbol.name g sym);
    in
    for i = 0 to pos - 1
    do add_sym rhs.(i) done;
    Buffer.add_string b " .";
    for i = pos to Array.length rhs - 1
    do add_sym rhs.(i) done;
    Buffer.contents b
end

module Lr0 = struct
  type 'g n = 'g lr0

  let cardinal g = Vector.length g.lr0_items
  let of_int g i = Index.of_int (cardinal g) i

  (* See [Lr1.incoming]. *)
  let incoming g i = g.lr0_incoming.:(i)

  (* See [Lr1.items]. *)
  let items g i = g.lr0_items.:(i)

  (* If the state is an initial state, returns the pseudo (start)
     production that recognizes this entrypoint. *)
  let is_entrypoint g i = g.lr0_is_entrypoint.:(i)
end

module Lr1 = struct
  type 'g n = 'g lr1

  let cardinal g = Vector.length g.lr1_reduce_on
  let of_int g i = Index.of_int (cardinal g) i

  let all g = g.lr1_all
  let accepting g = g.lr1_accepting

  (* A ``wait'' state is an LR(1) state in which the parser needs to look at
     more input before knowing how to proceed.
     Wait states are the initial states and the targets of SHIFT transitions
     (states with a terminal as incoming symbol), except the accepting ones
     (after reading EOF, the only valid action is to reduce). *)
  let wait g = g.lr1_wait

  (* Get the LR(0) "core" state *)
  let to_lr0 g i = g.lr1_lr0.:(i)

  (* The symbol annotating the incoming transitions of a state.
     There is none for initial states, and at most one for others. *)
  let incoming g i = Lr0.incoming g (to_lr0 g i)

  (* Get the items in the kernel of a state (before closure). *)
  let items g i = Lr0.items g (to_lr0 g i)

  let is_entrypoint g i = Lr0.is_entrypoint g (to_lr0 g i)
  let entrypoint_table g = g.lr1_entrypoint_table
  let entrypoints g = g.lr1_entrypoints

  (* Printing functions, for debug purposes.
     Not nice for the end-user (FIXME). *)

  let symbol_to_string g lr1 =
    match incoming g lr1 with
    | Some sym -> Symbol.name g sym
    | None ->
      let entrypoint = Option.get (is_entrypoint g lr1) in
      (Symbol.name g (Production.rhs g entrypoint).(0) ^ ":")

  let to_string g lr1 =
    string_of_index lr1 ^ ":" ^ symbol_to_string g lr1

  let list_to_string g lr1s =
    string_concat_map ~wrap:("[","]") "; " (to_string g) lr1s

  let set_to_string g lr1s =
    string_concat_map ~wrap:("{","}") ", " (to_string g) (IndexSet.elements lr1s)

  (** [shift_on t] is the set of lookaheads that state [t] can shift *)
  let shift_on g i = g.lr1_shift_on.:(i)

  (** [reduce_on t] is the set of lookaheads that trigger a reduction in state
      [t] *)
  let reduce_on g i = g.lr1_reduce_on.:(i)

  (** [reject t] is set of lookaheads that cause the automaton to fail when in
      state [t] *)
  let reject g i = g.lr1_reject.:(i)

  (** [predecessors t] is the set of LR(1) states that have transition going
      to [t]. *)
  let predecessors g i = g.lr1_predecessors.:(i)

  (** Wrapper around [IndexSet.inter] speeding-up intersection with [all] *)
  let intersect g a b =
    if a == g.lr1_all then b
    else if b == g.lr1_all then a
    else IndexSet.inter a b

  let default_reduction g i =
    let open (val g.raw) in
    match Lr1.default_reduction (Lr1.of_int (i : _ index :> int)) with
    | None -> None
    | Some p -> Some (Index.of_int (Vector.length g.production_rhs) (Production.to_int p))
end

module Reduction = struct
  type 'g n = 'g reduction

  let cardinal g = Vector.length g.reduction_production
  let of_int g i = Index.of_int (cardinal g) i

  (* A reduction is a triple [(lr1, prod, lookaheads)], meaning that:
     in state [lr1], when looking ahead at a terminal in [lookaheads], the
     action is to reduce [prod]. *)

  let state g i = g.reduction_state.:(i)
  let production g i = g.reduction_production.:(i)
  let lookaheads g i = g.reduction_lookaheads.:(i)

  (* All reductions applicable to an lr1 state. *)
  let from_lr1 g lr1 =
    g.reduction_from_lr1.:(lr1)
end

module Transition = struct
  (* The set of goto transitions *)
  let goto g = Vector.length g.transition_goto_sym
  (* The set of all transitions = goto U shift *)
  let any g = Vector.length g.transition_source
  (* The set of shift transitions *)
  let shift g = Vector.length g.transition_shift_sym

  (* Inject goto into any *)
  let of_goto _g i = Sum.inj_l i

  (* Inject shift into any *)
  let of_shift g i = Sum.inj_r (goto g) i

  (* Project a transition into a goto or a shift transition *)
  let split g i =
    Sum.prj (goto g) i

  (* [find_goto s nt] finds the goto transition originating from [s] and
     labelled by [nt], or raise [Not_found].  *)
  let find_goto g lr1 nt =
    match IndexMap.find_opt nt g.transition_goto_table.:(lr1) with
    | Some gt -> gt
    | None ->
      Printf.ksprintf invalid_arg "find_goto(%s, %s)"
        (Lr1.to_string g lr1) (Nonterminal.to_string g lr1)

  let find_goto_target g lr1 nt =
    g.transition_target.:(of_goto g (find_goto g lr1 nt))

  (* Get the source state of a transition *)
  let source g i = g.transition_source.:(i)

  (* Get the target state of a transition *)
  let target g i = g.transition_target.:(i)

  (* Symbol that labels a transition *)
  let symbol g i =
    match split g i with
    | L i -> Sum.inj_r g.terminal_n g.transition_goto_sym.:(i)
    | R i -> Sum.inj_l g.transition_shift_sym.:(i)

  (* Symbol that labels a goto transition *)
  let goto_symbol g i = g.transition_goto_sym.:(i)

  (* Symbol that labels a shift transition *)
  let shift_symbol g i = g.transition_shift_sym.:(i)

  (* [successors s] returns all the transitions [tr] such that
     [source tr = s] *)
  let successors g i = g.transition_successors.:(i)

  (* [predecessors s] returns all the transitions [tr] such that
     [target tr = s] *)
  let predecessors g i = g.transition_predecessors.:(i)

  (* Accepting transitions are goto transitions from an initial state to an
     accepting state, recognizing one of the grammar entrypoint. *)
  let accepting g = g.transition_accepting
end
