open Fix.Indexing
open Utils
open BitSet
open Misc

module type INDEXED = sig
  type raw
  include Fix.Indexing.CARDINAL
  type t = n index
  type set = n indexset
  type 'a map = (n, 'a) IndexMap.t
  val of_g : raw -> t
  val to_g : t -> raw
end

module Indexed(Raw : sig
    type t
    val count : int
    val to_int : t -> int
    val of_int : int -> t
  end)
  : INDEXED with type raw = Raw.t =
struct
  include Const(struct let cardinal = Raw.count end)
  type raw = Raw.t
  type t = n index
  type set = n indexset
  type 'a map = (n, 'a) indexmap
  let of_g x = Index.of_int n (Raw.to_int x)
  let to_g x = Raw.of_int (Index.to_int x)
end

let all n =
  let acc = ref IndexSet.empty in
  for i = cardinal n - 1 downto 0
  do acc := IndexSet.add (Index.of_int n i) !acc done;
  !acc

module Make(Grammar : Sigs.GRAMMAR) : Sigs.INFO with module Grammar = Grammar =
struct
  module Grammar = Grammar

  module Terminal = struct
    include Indexed(Grammar.Terminal)
    let to_string i = Grammar.Terminal.name (to_g i)
    let all = all n
  end

  module Nonterminal = struct
    include Indexed(Grammar.Nonterminal)
    let to_string i = Grammar.Nonterminal.name (to_g i)
    let all = all n
  end

  module Symbol = struct
    type t =
      | T of Terminal.t
      | N of Nonterminal.t

    let of_g = function
      | Grammar.T t -> T (Terminal.of_g t)
      | Grammar.N n -> N (Nonterminal.of_g n)

    let to_g = function
      | T t -> Grammar.T (Terminal.to_g t)
      | N n -> Grammar.N (Nonterminal.to_g n)

    let name ?mangled = function
      | T t -> Grammar.symbol_name ?mangled (T (Terminal.to_g t))
      | N n -> Grammar.symbol_name ?mangled (N (Nonterminal.to_g n))
  end

  module Production = struct
    include Indexed(Grammar.Production)

    let lhs p = Nonterminal.of_g (Grammar.Production.lhs (to_g p))

    let rhs =
      let import prod =
        Array.map
          (fun (sym,_,_) -> Symbol.of_g sym)
          (Grammar.Production.rhs (to_g prod))
      in
      Vector.get (Vector.init n import)

    let kind p = Grammar.Production.kind (to_g p)
  end

  module Lr1 = struct
    include Indexed(Grammar.Lr1)
    let all = all n

    let to_lr0 lr1 = Grammar.Lr1.lr0 (to_g lr1)

    let incoming lr1 =
      Option.map Symbol.of_g (Grammar.Lr0.incoming (to_lr0 lr1))

    let items lr1 =
      List.map
        (fun (p,pos) -> (Production.of_g p, pos))
        (Grammar.Lr0.items (to_lr0 lr1))

    let reductions =
      let import_red reds =
        reds
        |> List.map
          (fun (t, ps) -> (Production.of_g (List.hd ps), Terminal.of_g t))
        |> Misc.group_by
          ~compare:(fun (p1,_) (p2,_) -> compare_index p1 p2)
          ~group:(fun (p,t) ps -> p, IndexSet.of_list (t :: List.map snd ps))
      in
      let import_lr1 lr1 = import_red (Grammar.Lr1.reductions (to_g lr1)) in
      Vector.get (Vector.init n import_lr1)

    let to_string lr1 =
      string_of_index lr1 ^ ":" ^
      match incoming lr1 with
      | None -> "<initial state>"
      | Some sym -> Symbol.name sym

    let reduce_on =
      vector_tabulate n (fun lr1 ->
          List.fold_left
            (fun acc (t, _) -> IndexSet.add (Terminal.of_g t) acc)
            IndexSet.empty (Grammar.Lr1.reductions (to_g lr1))
        )

    let shift_on =
      vector_tabulate n (fun lr1 ->
          List.fold_left
            (fun acc (sym, _raw) ->
               match sym with
               | Grammar.T t -> IndexSet.add (Terminal.of_g t) acc
               | Grammar.N _ -> acc)
            IndexSet.empty (Grammar.Lr1.transitions (to_g lr1))
        )

    let fail_on =
      vector_tabulate n (fun lr1 ->
          let result = Terminal.all in
          let result = IndexSet.diff result (reduce_on lr1) in
          let result = IndexSet.diff result (shift_on lr1) in
          result
        )
  end

  (* Transitions are represented as finite sets with auxiliary functions
     to get the predecessors, successors and labels. *)
  module Transition =
  struct

    (* Pre-compute all information, such that functions of this module
       always operate in O(1) *)

    (* Create two fresh finite sets that will be populated with goto and shift
       transitions *)
    let shift_count, goto_count =
      let shift_count = ref 0 in
      let goto_count = ref 0 in
      (* Count goto and shift transitions by iterating on all states and
         transitions *)
      Grammar.Lr1.iter begin fun lr1 ->
        List.iter begin fun (sym, _) ->
          match sym with
          | Grammar.T _ -> incr shift_count
          | Grammar.N _ -> incr goto_count
        end (Grammar.Lr1.transitions lr1)
      end;
      (!shift_count, !goto_count)

    module Goto = Const(struct let cardinal = goto_count end)
    module Shift = Const(struct let cardinal = shift_count end)

    type goto = Goto.n
    let goto = Goto.n

    type shift = Shift.n
    let shift = Shift.n

    (* Any is the disjoint sum of goto and shift transitions *)
    module Any = Sum(Goto)(Shift)
    type any = Any.n
    let any = Any.n

    let of_goto = Any.inj_l
    let of_shift = Any.inj_r
    let split = Any.prj

    (* Vectors to store information on states and transitions.

       We allocate a bunch of data structures (sources, targets, t_symbols,
       nt_symbols and predecessors vectors, t_table and nt_table hash tables),
       and then populate them by iterating over all transitions.
    *)

    let sources = Vector.make' any (fun () -> Index.of_int Lr1.n 0)
    let targets = Vector.make' any (fun () -> Index.of_int Lr1.n 0)

    let t_symbols = Vector.make' shift (fun () -> Index.of_int Terminal.n 0)
    let nt_symbols = Vector.make' goto (fun () -> Index.of_int Nonterminal.n 0)

    (* Hash tables to associate information to the pair of
       a transition and a symbol.
    *)

    let nt_table = Hashtbl.create 7

    let nt_count = cardinal Nonterminal.n

    let nt_pack lr1 goto =
      (* Custom function to key into nt_table: compute a unique integer from
         an lr1 state and a non-terminal. *)
      Index.to_int lr1 * nt_count + Index.to_int goto

    let t_table = Hashtbl.create 7

    let t_count = cardinal Terminal.n

    let t_pack lr1 t =
      (* Custom function to key into t_table: compute a unique integer from
         an lr1 state and a terminal. *)
      Index.to_int lr1 * t_count + Index.to_int t

    (* A vector to store the predecessors of an lr1 state.
       We cannot compute them directly, we discover them by exploring the
       successor relation below. *)
    let predecessors = Vector.make Lr1.n []

    let successors =
      (* We populate all the data structures allocated above, i.e.
         the vectors t_sources, t_symbols, t_targets, nt_sources, nt_symbols,
         nt_targets and predecessors, as well as the tables t_table and
         nt_table, by iterating over all successors. *)
      let next_goto = Index.enumerate goto in
      let next_shift = Index.enumerate shift in
      Vector.init Lr1.n begin fun source ->
        List.fold_left begin fun acc (sym, target) ->
          let target = Lr1.of_g target in
          let index = match sym with
            | Grammar.T t ->
              let t = Terminal.of_g t in
              let index = next_shift () in
              Vector.set t_symbols index t;
              Hashtbl.add t_table (t_pack source t) index;
              of_shift index
            | Grammar.N nt ->
              let nt = Nonterminal.of_g nt in
              let index = next_goto () in
              Vector.set nt_symbols index nt;
              Hashtbl.add nt_table (nt_pack source nt) index;
              of_goto index
          in
          Vector.set sources index source;
          Vector.set targets index target;
          Vector.set_cons predecessors target index;
          index :: acc
        end [] (Grammar.Lr1.transitions (Lr1.to_g source))
      end

    let successors lr1 = Vector.get successors lr1
    let predecessors lr1 = Vector.get predecessors lr1

    let find_goto source nt = Hashtbl.find nt_table (nt_pack source nt)

    let source i = Vector.get sources i

    let symbol i =
      match split i with
      | L i -> Symbol.N (Vector.get nt_symbols i)
      | R i -> Symbol.T (Vector.get t_symbols i)

    let goto_symbol i = Vector.get nt_symbols i
    let shift_symbol i = Vector.get t_symbols i

    let target i = Vector.get targets i

    let find_goto_target source nt =
      target (of_goto (find_goto source nt))
  end

  let lr1_predecessors = Vector.init Lr1.n (fun lr1 ->
      List.fold_left
        (fun acc tr -> IndexSet.add (Transition.source tr) acc)
        IndexSet.empty
        (Transition.predecessors lr1)
    )

  let lr1set_predecessors lr1s =
    indexset_bind lr1s (Vector.get lr1_predecessors)

  type 'a dfa_transition = Lr1.set * 'a

  let dfa_normalize_transitions
      cmp (tr : 'a dfa_transition list)
    : 'a list dfa_transition list
    =
    IndexRefine.annotated_partition tr
    |> List.map (fun (sg, a) -> sg, List.sort_uniq cmp a)
    |> Misc.group_by
      ~compare:(fun (_, a1) (_, a2) -> List.compare cmp a1 a2)
      ~group:(fun (sg0, a) sgs ->
          (List.fold_left
             (fun sg (sg', _) -> IndexSet.union sg sg')
             sg0 sgs, a)
        )

  let dfa_normalize_and_merge ~compare ~merge ts =
    dfa_normalize_transitions compare ts
    |> List.map (fun (k, v) -> (k, merge v))
end
