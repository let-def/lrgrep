open Fix.Indexing
open Utils
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

    let intersect a b =
      if a == all then b
      else if b == all then a
      else IndexSet.inter a b

    let semantic_value i =
      Grammar.Terminal.typ (to_g i)
  end

  module Nonterminal = struct
    include Indexed(Grammar.Nonterminal)
    let to_string i = Grammar.Nonterminal.name (to_g i)
    let all = all n
    let kind i = Grammar.Nonterminal.kind (to_g i)
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

  (** [Transitions] module, defined below, depends on the set of Lr1 states
      but [Lr1] module depends on [Transitions].
      So [Lr1I] is defined first and is the raw set of Lr1 states. *)
  module Lr1I = Indexed(Grammar.Lr1)

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

    let sources = Vector.make' any (fun () -> Index.of_int Lr1I.n 0)
    let targets = Vector.make' any (fun () -> Index.of_int Lr1I.n 0)

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
    let predecessors = Vector.make Lr1I.n []

    let successors =
      (* We populate all the data structures allocated above, i.e.
         the vectors t_sources, t_symbols, t_targets, nt_sources, nt_symbols,
         nt_targets and predecessors, as well as the tables t_table and
         nt_table, by iterating over all successors. *)
      let next_goto = Index.enumerate goto in
      let next_shift = Index.enumerate shift in
      Vector.init Lr1I.n begin fun source ->
        List.fold_left begin fun acc (sym, target) ->
          let target = Lr1I.of_g target in
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
        end [] (Grammar.Lr1.transitions (Lr1I.to_g source))
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

  module Lr1 = struct
    include Lr1I
    let all = all n

    let to_lr0 lr1 = Grammar.Lr1.lr0 (to_g lr1)

    let incoming lr1 =
      Option.map Symbol.of_g (Grammar.Lr0.incoming (to_lr0 lr1))

    let items lr1 =
      List.map
        (fun (p,pos) -> (Production.of_g p, pos))
        (Grammar.Lr0.items (to_lr0 lr1))

    (** A more convenient presentation of reductions than the one from Cmly.
        Each reduction is exposed as a pair of a Production.t and the lookahead
        set.
        Start productions are not included. *)
    let reductions =
      let import_red reds =
        reds
        |> List.filter_map (fun (t, p) ->
             let p = Production.of_g p in
             match Production.kind p with
             | `START -> None
             | `REGULAR -> Some (p, Terminal.of_g t)
          )
        |> Misc.group_by
          ~compare:(fun (p1,_) (p2,_) -> compare_index p1 p2)
          ~group:(fun (p,t) ps -> p, IndexSet.of_list (t :: List.map snd ps))
      in
      let import_lr1 lr1 = import_red (Grammar.Lr1.reductions (to_g lr1)) in
      Vector.get (Vector.init n import_lr1)

    (** A somewhat informative string description of the Lr1 state, for debug
        purposes. *)
    let to_string lr1 =
      match incoming lr1 with
      | Some sym -> string_of_index lr1 ^ ":" ^ Symbol.name sym
      | None -> (
          match items lr1 with
          | [p, 0] ->
            let p = Production.to_g p in
            assert (Grammar.Production.kind p = `START);
            let name = Grammar.Nonterminal.name (Grammar.Production.lhs p) in
            let name = Bytes.of_string name in
            Bytes.set name (Bytes.length name - 1) ':';
            Bytes.unsafe_to_string name
          | _ -> assert false
        )

    let list_to_string lr1s =
      string_concat_map ~wrap:("[","]") "; " to_string lr1s

    let set_to_string lr1s =
      string_concat_map ~wrap:("{","}") ", " to_string (IndexSet.elements lr1s)

    (** The set of terminals that will trigger a reduction *)
    let reduce_on = tabulate_finset n (fun lr1 ->
        List.fold_left
          (fun acc (t, _) -> IndexSet.add (Terminal.of_g t) acc)
          IndexSet.empty (Grammar.Lr1.reductions (to_g lr1))
      )

    (** The set of terminals that will trigger a shift transition *)
    let shift_on = tabulate_finset n (fun lr1 ->
        List.fold_left
          (fun acc (sym, _raw) ->
             match sym with
             | Grammar.T t -> IndexSet.add (Terminal.of_g t) acc
             | Grammar.N _ -> acc)
          IndexSet.empty (Grammar.Lr1.transitions (to_g lr1))
      )

    (** The set of terminals the state has no transition for *)
    let reject = tabulate_finset n (fun lr1 ->
        let result = Terminal.all in
        let result = IndexSet.diff result (reduce_on lr1) in
        let result = IndexSet.diff result (shift_on lr1) in
        result
      )

    (** Compute the ϵ-closure of lr1 reductions *)
    let closed_reject = Vector.make n IndexSet.empty
    let closed_shift_on = Vector.make n IndexSet.empty

    let internal_stacks = Vector.make n []

    let t0 = Sys.time ()

    type closed_reduction = {
      pop: int;
      prod: Production.t;
      prods: Production.t list;
      lookahead: Terminal.set;
    }

    let close_reductions lr1 =
      let rresult = ref [] in
      let rreject = ref IndexSet.empty in
      let rshift = ref IndexSet.empty in
      let rinternal = ref [] in
      let rec close_prod prod prods ts n stack =
        match stack with
        | [] ->
          push rresult {pop = n + 1; prod; prods; lookahead = ts}
        | top :: _ when n = 0 ->
          let target = Transition.find_goto_target top (Production.lhs prod) in
          let stack = target :: stack in
          close_stack (prod :: prods) ts stack
        | _ :: stack ->
          close_prod prod prods ts (n - 1) stack

      and close_stack prods ts = function
        | [] -> ()
        | (lr1 :: _) as stack ->
          rreject := IndexSet.union !rreject (Terminal.intersect (reject lr1) ts);
          rshift := IndexSet.union !rshift (Terminal.intersect (shift_on lr1) ts);
          push rinternal stack;
          let visit_reduction (prod, ts') =
            let ts = Terminal.intersect ts ts' in
            if not (IndexSet.is_empty ts) then
              let n = Array.length (Production.rhs prod) in
              close_prod prod prods ts n stack
          in
          List.iter visit_reduction (reductions lr1)
      in
      close_stack [] Terminal.all [lr1];
      (!rreject, !rshift, !rinternal, !rresult)


    let closed_reductions = tabulate_finset n (fun lr1 ->
        let reject', shift_on', internal, reductions = close_reductions lr1 in
        Vector.set closed_reject lr1 reject';
        Vector.set closed_shift_on lr1 shift_on';
        Vector.set internal_stacks lr1 internal;
        (* In theory, reject and closed_reject can differ if a transition
           that follows a nullable-reduction has been removed because of a
           conflict. That should not be a problem in practice.
           Haven't seen this in the wild yet, I will leave this code to check
           that for now...
        *)
        if not (IndexSet.equal (reject lr1) reject') then (
          Printf.eprintf
            "reject and closed_reject differ for state %s:\n%s\n%s\n"
            (to_string lr1)
            (string_of_indexset ~index:Terminal.to_string (reject lr1))
            (string_of_indexset ~index:Terminal.to_string reject');
          exit 1
        );
        let order_by_length c1 c2 = Int.compare c1.pop c2.pop in
        List.sort order_by_length reductions
      )

    let () =
      Printf.eprintf "%.02fms spent to close reductions\n"
        ((Sys.time () -. t0) *. 1000.0)

    let closed_reject = Vector.get closed_reject
    let closed_shift_on = Vector.get closed_shift_on
    let internal_stacks = Vector.get internal_stacks

    let predecessors = tabulate_finset n (fun lr1 ->
        List.fold_left
          (fun acc tr -> IndexSet.add (Transition.source tr) acc)
          IndexSet.empty
          (Transition.predecessors lr1)
      )

    let set_predecessors lr1s =
      indexset_bind lr1s predecessors
  end

  type 'a partial_derivative = Lr1.set * 'a

  let determinize_derivatives
      ~compare ~merge (tr : 'a partial_derivative list)
    : 'b partial_derivative list
    =
    IndexRefine.annotated_partition tr
    |> List.map (fun (sg, a) -> sg, List.sort_uniq compare a)
    |> Misc.group_by
      ~compare:(fun (_, a1) (_, a2) -> List.compare compare a1 a2)
      ~group:(fun (sg0, a) sgs ->
          let states =
            List.fold_left
              (fun sg (sg', _) -> IndexSet.union sg sg')
              sg0 sgs
          in
          states, merge a
        )
end
