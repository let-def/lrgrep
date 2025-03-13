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

(** This module is responsible for generating a deterministic finite automaton
    (DFA) from a given grammar and lookahead set. The DFA is used to perform
    pattern matching on input tokens according to the grammar rules.
    The module includes several stages:
    - Construction of a big DFA from the grammar and lookahead set.
    - Minimization of the DFA.
    - Generation of output code for the minimized DFA.
    The module uses various data structures and algorithms to ensure efficient
    construction and minimization of the DFA, as well as to generate the
    corresponding OCaml code.
*)

open Utils
open Misc
open Fix.Indexing
open Regexp
open Lrgrep_support

type priority = int

module Make
    (Transl : Transl.S)
    (R : sig val parser_name : string val rule : Syntax.rule end) =
struct
  open Transl
  open Regexp
  open Info
  open R

  module Branch : sig
    include CARDINAL
    type t = n index

    type state = {
      index: t;
      accept: bool;
    }

    type clause
    val clause : clause cardinal
    val clause_syntax : clause index -> Syntax.clause
    val clause_branches : clause index -> n indexset
    val clause_captures : clause index -> (Capture.n, Syntax.capture_kind * string) indexmap

    val branch_clause : n index -> clause index
    val branch_pattern : n index -> Syntax.pattern
    val branch_lookaheads : n index -> Terminal.set option
    val branch_captures : n index -> Capture.n indexset
    val branch_expr : n index -> Regexp.RE.t
    val branch_is_total : n index -> bool
  end =
  struct
    module Clause = Vector.Of_array(struct
        type a = Syntax.clause
        let array = Array.of_list rule.clauses
      end)

    type clause = Clause.n
    let clause = Vector.length Clause.vector
    let clause_syntax = Vector.get Clause.vector

    type desc = {
      clause: clause index;
      pattern: Syntax.pattern;
    }

    include Vector.Of_array(struct
        type a = desc
        let array =
          Vector.mapi (fun clause syntax ->
              List.map
                (fun pattern -> {clause; pattern})
                syntax.Syntax.patterns
            ) Clause.vector
          |> Vector.to_list
          |> List.flatten
          |> Array.of_list
      end)

    let n = Vector.length vector
    type t = n index

    type state = {
      index: t;
      accept: bool;
    }

    let branch_clause b = vector.:(b).clause

    let branch_pattern b = vector.:(b).pattern

    let clause_branches =
      let index = ref 0 in
      let import clause =
        let count = List.length clause.Syntax.patterns in
        let first = Index.of_int n !index in
        index := !index + count;
        let last = Index.of_int n (!index - 1) in
        IndexSet.init_interval first last
      in
      Vector.get (Vector.map import Clause.vector)

    let branch_lookaheads =
      tabulate_finset n @@ fun branch ->
      match vector.:(branch).pattern.lookaheads with
      | [] -> None
      | symbols ->
        let lookahead_msg =
          "Lookahead can either be a terminal or `first(nonterminal)'"
        in
        let sym_pattern (sym, pos) =
          match sym with
          | Syntax.Apply ("first", [sym]) ->
            begin match Symbol.prj (Transl.Indices.get_symbol pos sym) with
              | L t ->
                let t = Terminal.to_string t in
                failwith (lookahead_msg ^ "; in first(" ^ t ^ "), " ^
                          t ^ " is a terminal")
              | R n ->
                Nonterminal.to_g n
                |> Grammar.Nonterminal.first
                |> List.map Terminal.of_g
            end
          | Syntax.Name _ ->
            begin match Symbol.prj (Transl.Indices.get_symbol pos sym) with
              | R n ->
                failwith (lookahead_msg ^ "; " ^
                          Nonterminal.to_string n ^ " is a nonterminal")
              | L t -> [t]
            end
          | _ ->
            failwith lookahead_msg
        in
        Some (IndexSet.of_list (List.concat_map sym_pattern symbols))

    let branch_is_total br =
      Option.is_none (branch_lookaheads br) &&
      match (clause_syntax (branch_clause br)).action with
      | Syntax.Total _ -> true
      | Syntax.Partial _ -> false
      | Syntax.Unreachable -> true

    let branch_transl = Vector.make n (IndexSet.empty, Transl.Regexp.RE.empty)

    let clause_captures =
      let gensym = Capture.gensym () in
      tabulate_finset clause @@ fun clause ->
      let capture_tbl = Hashtbl.create 7 in
      let capture_def = ref IndexMap.empty in
      let capture kind name =
        let key = (kind, name) in
        match Hashtbl.find_opt capture_tbl key with
        | Some index -> index
        | None ->
          let index = gensym () in
          Hashtbl.add capture_tbl key index;
          capture_def := IndexMap.add index key !capture_def;
          index
      in
      let translate_branch (br : n index) =
        let pattern = branch_pattern br in
        branch_transl.:(br) <- Transl.transl ~capture pattern.expr
      in
      IndexSet.iter translate_branch (clause_branches clause);
      !capture_def

    let branch_captures b = fst branch_transl.:(b)

    let branch_expr b = snd branch_transl.:(b)
  end

  module type STACKS = sig
    include CARDINAL
    val tops : n indexset
    val prev : n index -> n indexset
    val label : n index -> Lr1.set
  end

  type ('src, 'tgt) mapping = ('tgt, 'src index * (Capture.set * Usage.set)) vector

  module type DFA = sig
    include CARDINAL

    type 'n t = private {
      index: n index;
      branches: ('n, Branch.state) vector;
      mutable transitions : 'n transition list;
    }

    and 'src transition = Transition : {
        label: Lr1.set;
        target: 'tgt t;
        mapping: ('src, 'tgt) mapping;
      } -> 'src transition

    and packed = Packed : 'n t -> packed [@@ocaml.unboxed]

    val initial : n index

    val states : (n, packed) vector

    val transitions : 'n t -> 'n transition list

    val domain : n index -> Lr1.set
  end

  module Determinize (Stacks: STACKS) () : DFA = struct

    module NFA = struct
      type t = {
        uid: int;
        k: K.t;
        transitions: (K.label * t lazy_t) list;
        branch: Branch.state;
        mutable mark: unit ref;
      }

      let compare t1 t2 =
        Int.compare t1.uid t2.uid

      let uid =
        let k = ref 0 in
        fun () -> incr k; !k

      let default_mark = ref ()

      module KMap = Map.Make(Regexp.K)

      let make index =
        let nfa = ref KMap.empty in
        let branch = {Branch. index; accept = false} in
        let rec aux k =
          match KMap.find_opt k !nfa with
          | Some t -> t
          | None ->
            let accept = ref false in
            let rec process_transitions = function
              | [] -> []
              | (label, target) :: rest ->
                begin match target with
                  | None when K.is_immediate_label label ->
                    accept := true;
                    []
                  | None ->
                    (label, accepting) :: process_transitions rest
                  | Some k' ->
                    (label, lazy (aux k')) :: process_transitions rest
                end
            in
            let inj ({K. filter; usage; captures}, t) = (filter, (usage, captures, t)) in
            let prj filter (usage, captures, t) = ({K. filter; usage; captures}, t) in
            let transitions =
              K.derive Lr1.all k
              |> process_transitions
              |> List.map inj
              |> IndexRefine.annotated_partition
              |> List.concat_map (fun (filter, l) -> List.map (prj filter) l)
            in
            let uid = uid () in
            let branch = if !accept then {branch with accept = true} else branch in
            let t = {uid; k; transitions; branch; mark=default_mark} in
            nfa := KMap.add k t !nfa;
            t
        and accepting = lazy (aux K.Done)
        in
        aux

      let branches =
        Vector.init Branch.n (fun br ->
            let re = Branch.branch_expr br in
            make br (Regexp.K.More (re, Regexp.K.Done))
          )
    end

    module Construction = struct
      include IndexBuffer.Gen.Make()

      type 'n prestate = {
        index: n index;
        kernel: ('n, NFA.t) vector;
        accept: Branch.t option;
        mutable raw_transitions: (Lr1.set * 'n fwd_mapping lazy_t) list;
      }

      and 'src fwd_mapping =
          Fwd_mapping : ('src, 'tgt) mapping * 'tgt prestate -> 'src fwd_mapping

      type prepacked = Prepacked : 'n prestate -> prepacked [@@ocaml.unboxed]

      let prestates = get_generator ()

      module KernelMap = Map.Make(struct
          type t = NFA.t array
          let compare g1 g2 = array_compare NFA.compare g1 g2
        end)

      let kernel_make (type a) (prj : a -> NFA.t) (ts : a list) : a array =
        let mark = ref () in
        let ts = List.filter (fun a ->
            let th = prj a in
            if th.mark != mark then (
              th.mark <- mark;
              true
            ) else false
          ) ts
        in
        Array.of_list ts

      let kernel_fold f x acc =
        let acc = ref acc in
        Vector.iteri (fun i x -> acc := f i x !acc) x;
        !acc

      let dfa = ref KernelMap.empty

      let initial =
        let rec determinize : type n . (n, NFA.t) vector -> n prestate =
          fun kernel ->
            match KernelMap.find_opt (Vector.as_array kernel) !dfa with
            | Some (Prepacked t') ->
              let Refl = assert_equal_length kernel t'.kernel in
              t'
            | None ->
              let accept = ref None in
              let rev_transitions =
                let make i ({K. filter; captures; usage}, t) =
                  (filter, (i, (captures, usage), t))
                in
                kernel_fold
                  (fun i (nfa : NFA.t) acc ->
                     if nfa.branch.accept &&
                        Branch.branch_is_total nfa.branch.index then
                       accept := Some nfa.branch.index;
                     List.rev_map (make i) nfa.transitions @ acc)
                  kernel []
              in
              let prepare_target_kernel (index, captures, lazy nfa) =
                nfa, (index, captures)
              in
              let process_class label rev_targets =
                label, lazy (
                  let Packed result =
                    rev_targets
                    |> List.rev_map prepare_target_kernel
                    |> kernel_make fst
                    |> Vector.of_array
                  in
                  Fwd_mapping ((Vector.map snd result),
                               determinize (Vector.map fst result))
                )
              in
              let raw_transitions = ref [] in
              IndexRefine.iter_merged_decomposition rev_transitions
                (fun label targets -> push raw_transitions (process_class label targets));
              let raw_transitions = !raw_transitions in
              let reservation = IndexBuffer.Gen.reserve prestates in
              let state = {
                index = IndexBuffer.Gen.index reservation;
                kernel; accept = !accept;
                raw_transitions;
              } in
              IndexBuffer.Gen.commit prestates reservation (Prepacked state);
              dfa := KernelMap.add (Vector.as_array kernel) (Prepacked state) !dfa;
              state
        in
        let Vector.Packed kernel =
          Vector.of_array (kernel_make Fun.id (Vector.to_list NFA.branches))
        in
        (determinize kernel).index

      let () = stopwatch 3 "Processed initial states"

      let visited: (n, Stacks.n indexset) IndexBuffer.Dyn.t =
        IndexBuffer.Dyn.make IndexSet.empty

      let scheduled: (n, Stacks.n indexset) IndexBuffer.Dyn.t =
        IndexBuffer.Dyn.make IndexSet.empty

      let (.*()) = IndexBuffer.Dyn.get
      let (.*()<-) = IndexBuffer.Dyn.set

      let () =
        let accepting = Vector.make Branch.n [] in
        let todo = ref [] in
        let min_clause t = (Vector.as_array t.kernel).(0).branch.index in
        let schedule bound i set =
          let Prepacked t as packed = IndexBuffer.Gen.get prestates i in
          if min_clause t <= bound then
            let set = IndexSet.diff set visited.*(i) in
            if not (IndexSet.is_empty set) then (
              if IndexSet.is_empty scheduled.*(i) then (
                scheduled.*(i) <- set;
                match t.accept with
                | Some c when c < bound ->
                  accepting.@(c) <- List.cons packed
                | Some _ | None -> push todo packed
              ) else
                scheduled.*(i) <- IndexSet.union scheduled.*(i) set
            )
        in
        let update bound (Prepacked t) =
          let todo = scheduled.*(t.index) in
          visited.*(t.index) <- IndexSet.union visited.*(t.index) todo;
          scheduled.*(t.index) <- IndexSet.empty;
          List.iter begin fun (label, target) ->
            let really_empty = ref true in
            let expand_stack stack =
              if IndexSet.disjoint (Stacks.label stack) label
              then IndexSet.empty
              else (really_empty := false; Stacks.prev stack)
            in
            let stacks = indexset_bind todo expand_stack in
            if not !really_empty then
              let lazy (Fwd_mapping (_, t')) = target in
              if not (IndexSet.is_empty stacks) then
                schedule bound t'.index stacks
          end t.raw_transitions
        in
        let next_bound = Index.rev_enumerate Branch.n in
        let rec loop bound =
          match !todo with
          | [] ->
            let bound = next_bound () in
            todo := accepting.:(bound);
            accepting.:(bound) <- [];
            loop bound
          | todo' ->
            todo := [];
            List.iter (update bound) todo';
            loop bound
        in
        try
          let bound = next_bound () in
          schedule bound initial Stacks.tops;
          loop bound
        with Index.End_of_set -> ()

      let prestates = IndexBuffer.Gen.freeze prestates

      let domain =
        Vector.init n (fun i -> indexset_bind visited.*(i) Stacks.label)
    end

    type n = Construction.n
    let n = Construction.n

    let domain = Vector.get Construction.domain

    type 'n t = {
      index: n index;
      branches: ('n, Branch.state) vector;
      mutable transitions : 'n transition list;
    }

    and 'src transition =
        Transition : {
          label: Lr1.set;
          target: 'tgt t;
          mapping: ('src, 'tgt) mapping;
        } -> 'src transition

    and packed = Packed : 'n t -> packed [@@ocaml.unboxed]

    let initial = Construction.initial

    let states =
      let make (Construction.Prepacked {index; kernel; _}) =
        let branches = Vector.map (fun t -> t.NFA.branch) kernel in
        Packed {index; branches; transitions = []}
      in
      Vector.map make Construction.prestates

    let from_prestate (type n) (p : n Construction.prestate) : n t =
      let Packed t = states.:(p.index) in
      let Refl = assert_equal_length t.branches p.kernel in
      t

    let () =
      Vector.iteri (fun i (Construction.Prepacked p) ->
          let t = from_prestate p in
          let domain = domain i in
          t.transitions <-
            List.filter_map (fun (label, target) ->
                if Lazy.is_val target then
                  let label = IndexSet.inter label domain  in
                  if not (IndexSet.is_empty label) then
                    let Construction.Fwd_mapping (mapping, target) =
                      Lazy.force target in
                    let target = from_prestate target in
                    Some (Transition {label; mapping; target})
                  else
                    None
                else
                  None
              ) p.raw_transitions;
        ) Construction.prestates

    let transitions t = t.transitions

    let () = stopwatch 3 "Determinized DFA (%d states)" (cardinal n)
  end

  module type DATAFLOW = sig
    module DFA : DFA
    val pairings : (DFA.n, (Branch.t * (Order_chain.element * Order_chain.element) list) list list) vector
    val accepts : (DFA.n, (Branch.t * priority) list) vector
    val get_liveness : 'n DFA.t -> ('n, Capture.set) vector
    val get_registers : 'n DFA.t -> ('n, Register.t Capture.map) vector
    val register_count : int
  end

  module Dataflow (DFA : DFA) : DATAFLOW with module DFA := DFA =
  struct
    type 'tgt rev_mapping = Rev_mapping : 'src DFA.t * ('src, 'tgt) mapping -> 'tgt rev_mapping
    type packed_rev_mapping = Rev_packed : 'n rev_mapping list -> packed_rev_mapping [@@ocaml.unboxed]

    let reverse_transitions =
      let table = Vector.make DFA.n (Rev_packed []) in
      Vector.iter begin fun (DFA.Packed src) ->
        let process (DFA.Transition {target; mapping; _}) =
          match table.:(target.index) with
          | Rev_packed [] ->
            table.:(target.index) <- Rev_packed [Rev_mapping (src, mapping)]
          | Rev_packed (Rev_mapping (_, mapping0) :: _ as xs) ->
            let Refl = assert_equal_length mapping mapping0 in
            table.:(target.index) <- Rev_packed (Rev_mapping (src, mapping) :: xs)
        in
        List.iter process (DFA.transitions src)
      end DFA.states;
      table

    let iter_reverse_transitions (type n) (t : n DFA.t) (f : n rev_mapping -> unit) =
      match reverse_transitions.:(t.index) with
      | Rev_packed [] -> ()
      | Rev_packed (Rev_mapping (_, mapping0) :: _ as xs) ->
        let Refl = assert_equal_length mapping0 t.branches in
        List.iter f xs

    type 'n data = {
      state: 'n DFA.t;
      mutable reachable: 'n indexset;
      mutable splits: 'n indexset;
      mutable new_splits: 'n indexset;
      mutable chain: ('n index * Order_chain.element) list;
    }

    type packed = Packed : 'n data -> packed [@@ocaml.unboxed]

    let data =
      Vector.map (fun (DFA.Packed t) ->
          let n = Vector.length t.branches in
          let reachable =
            IndexSet.init_from_set n (fun i -> t.branches.:(i).accept)
          in
          let splits = IndexSet.empty in
          let new_splits = IndexSet.empty in
          Packed {state=t; reachable; splits; new_splits; chain=[]}
        ) DFA.states

    let get_data (type n) (st : n DFA.t) : n data =
      let Packed split = data.:(st.index) in
      let Refl = assert_equal_length st.branches split.state.branches in
      split

    let () =
      let todo = ref [] in
      let propagate (Packed t) =
        let reach = t.reachable in
        iter_reverse_transitions t.state @@ fun (Rev_mapping (src, mapping)) ->
        let s = get_data src in
        let changed = ref false in
        IndexSet.iter (fun i ->
            let j, _ = mapping.:(i) in
            let reach' = s.reachable in
            let reach'' = IndexSet.add j reach' in
            if not (IndexSet.equal reach' reach'') then (
              s.reachable <- reach'';
              changed := true;
            )
          ) reach;
        if !changed then
          push todo (Packed s)
      in
      Vector.iter propagate data;
      fixpoint ~propagate todo

    let () = stopwatch 3 "Computed reachability"

    let () =
      let process (Packed t) =
        let reach = t.reachable in
        iter_reverse_transitions t.state @@ fun (Rev_mapping (_, mapping)) ->
        IndexSet.iter (fun i ->
            let _, (_, usage) = mapping.:(i) in
            Usage.mark_used usage
          ) reach
      in
      Vector.iter process data

    let () =
      let reachable_branches =
        let Packed t = data.:(DFA.initial) in
        let branches = t.state.branches in
        IndexSet.map (fun i -> branches.:(i).index) t.reachable
      in
      let iter_re f (re : Syntax.regular_expr) =
        match re.desc with
        | Atom _ -> ()
        | Filter _ -> ()
        | Repetition {expr; policy = _} ->
          f expr
        | Reduce {capture = _; mark = _; expr; policy = _} ->
          f expr
        | Alternative res ->
          List.iter f res
        | Concat res ->
          List.iter f res
      in
      let rec check (re : Syntax.regular_expr) =
        match re.desc with
        | Atom (_, _, mark) | Reduce {mark; _} ->
          if Usage.is_unused mark then (
            Printf.eprintf "Warning: expression line %d, column %d is unreachable\n"
              re.position.line re.position.col
          )
        | _ -> iter_re check re
      in
      Index.iter Branch.n @@ fun branch ->
      let pattern = Branch.branch_pattern branch in
      if IndexSet.mem branch reachable_branches then
        check pattern.expr
      else
        Printf.eprintf "Warning: clause line %d, column %d is unreachable\n"
          pattern.expr.position.line pattern.expr.position.col

    let () = stopwatch 3 "Dead-code analysis"

    let () =
      let count = ref 0 in
      let todo = ref [] in
      Vector.iter (fun (Packed t) ->
          let branches = t.state.branches in
          t.new_splits <-
            IndexSet.init_from_set
              (Vector.length branches)
              (fun i -> branches.:(i).accept);
          if not (IndexSet.is_empty t.new_splits) then
            push todo (Packed t);
        ) data;
      let schedule (type n) (t : n data) (splits : n indexset) =
        let splits = IndexSet.diff splits t.splits in
        if IndexSet.is_empty splits then
          ()
        else if IndexSet.is_empty t.new_splits then (
          incr count;
          push todo (Packed t);
          t.new_splits <- splits;
        ) else
          t.new_splits <- IndexSet.union t.new_splits splits
      in
      let rec schedule_one : type n. n data -> n indexset -> unit =
        fun (type n) (t : n data) (splits : n indexset) ->
          let splits = IndexSet.diff splits t.splits in
          if IndexSet.is_empty splits then
            ()
          else if IndexSet.is_empty t.new_splits then (
            t.new_splits <- splits;
            propagate (Packed t)
          ) else
            t.new_splits <- IndexSet.union t.new_splits splits
      and propagate (Packed src) =
        let new_splits = src.new_splits in
        src.new_splits <- IndexSet.empty;
        src.splits <- IndexSet.union src.splits new_splits;
        let new_splits = IndexSet.elements new_splits in
        let rec map_one mapping tgt i x xs =
          let n = Array.length mapping in
          if i >= n then
            IndexSet.empty
          else
            let x', _ = mapping.(i) in
            if x' < x then
              map_one mapping tgt (i + 1) x xs
            else
              let branch = src.state.branches.:(x) in
              let acc = map_splits mapping tgt (i + 1) xs in
              if Index.equal src.state.branches.:(x').index branch.index then
                IndexSet.add (Index.of_int (Vector.length tgt.DFA.branches) i) acc
              else
                acc
        and map_splits mapping tgt i = function
          | [] -> IndexSet.empty
          | x :: xs -> map_one mapping tgt i x xs
        in
        match src.state.transitions with
        | [] -> ()
        | [DFA.Transition {mapping; target; _}] ->
          schedule_one
            (get_data target)
            (map_splits (Vector.as_array mapping) target 0 new_splits)
        | xs ->
          List.iter begin fun (DFA.Transition {mapping; target; _}) ->
            schedule
              (get_data target)
              (map_splits (Vector.as_array mapping) target 0 new_splits)
          end xs
      in
      fixpoint ~propagate todo;
      stopwatch 3 "computed priority splits (%d refinements)" !count

    let chain = Order_chain.make ()

    let pairings = Vector.make DFA.n []

    let group_by_branch t = function
      | [] -> []
      | (i, _) as x :: xs ->
        let rec loop branch acc accs = function
          | [] -> List.rev ((branch, List.rev acc) :: accs)
          | (i, _) as x :: xs ->
            let branch' = t.DFA.branches.:(i).index in
            if branch = branch' then
              loop branch (x :: acc) accs xs
            else
              loop branch' [x] ((branch, List.rev acc) :: accs) xs
        in
        let branch = t.branches.:(i).index in
        loop branch [x] [] xs

    let rec chain_next_split i element = function
      | (i', element') :: rest ->
        let c = Index.compare i' i in
        if c < 0  then
          chain_next_split i element' rest
        else if c = 0 then
          (element', rest)
        else
          (Order_chain.extend element, rest)
      | [] -> (Order_chain.next element, [])

    let () =
      let chain_processed = Boolvector.make DFA.n false in
      let root = Order_chain.root chain in
      let Packed initial = data.:(DFA.initial) in
      initial.chain <- (
        match IndexSet.elements initial.splits with
        | [] -> []
        | splits ->
          let branches = initial.state.branches in
          let rec fresh_chain branch element = function
            | [] -> []
            | m :: ms ->
              let branch' = branches.:(m).index in
              let element =
                if Index.equal branch branch'
                then Order_chain.next element
                else root
              in
              (m, element) :: fresh_chain branch' element ms
          in
          fresh_chain (Index.of_int Branch.n 0) root splits
      );
      Boolvector.set chain_processed DFA.initial;
      let direct_transitions = ref 0 in
      let shared_transitions = ref 0 in
      let trivial_pairing = ref 0 in
      let nontrivial_pairing = ref 0 in
      let transitions_with_pairing = ref 0 in
      let process_direct_transition src mapping tgt =
        assert (not (Boolvector.test chain_processed tgt.state.DFA.index));
        incr direct_transitions;
        let sbranches = src.state.branches in
        let tbranches = tgt.state.branches in
        let rec extract_branch branch acc = function
          | (n, _) as x :: xs when Index.equal sbranches.:(n).index branch ->
            extract_branch branch (x :: acc) xs
          | rest -> List.rev acc, rest
        in
        let rec seek_branch branch = function
          | [] -> [], []
          | ((n, _) as x :: xs) as xxs ->
            let c = Index.compare sbranches.:(n).index branch in
            if c < 0 then
              seek_branch branch xs
            else if c = 0 then
              extract_branch branch [x] xs
            else
              ([], xxs)
        in
        let rec process_splits chain = function
          | [] -> []
          | m :: ms ->
            let branch = tbranches.:(m).index in
            let chain, rest = seek_branch branch chain in
            process_branch branch chain rest m ms
        and process_branch branch chain rest m ms =
          let i, _ = mapping.:(m) in
          let split, chain = chain_next_split i root chain in
          (m, split) :: process_continue_branch branch chain rest ms
        and process_continue_branch branch chain rest = function
          | m :: ms when Index.equal tbranches.:(m).index branch ->
            process_branch branch chain rest m ms
          | ms -> process_splits rest ms
        in
        tgt.chain <- process_splits src.chain (IndexSet.elements tgt.splits);
        Boolvector.set chain_processed tgt.state.index
      in
      let process_shared_transition src mapping tgt =
        incr shared_transitions;
        assert (Boolvector.test chain_processed src.state.index);
        assert (Boolvector.test chain_processed tgt.state.index);
        let src_chain = group_by_branch src.state src.chain in
        let tgt_chain = group_by_branch tgt.state tgt.chain in
        let rec find_element i element = function
          | [] -> element, []
          | (i', element') :: xs as xxs ->
            if (i' : _ index) > i
            then element, xxs
            else find_element i element' xs
        in
        let rec pair_elements src_elements = function
          | [] -> []
          | (i, tgt_element) :: rest ->
            let src_element, src_elements =
              find_element (fst mapping.:(i)) root src_elements
            in
            let tl = pair_elements src_elements rest in
            if src_element == tgt_element then (
              incr trivial_pairing;
              tl
            ) else (
              incr nontrivial_pairing;
              (src_element, tgt_element) :: tl
            )
        in
        let rec process_tgt clause elements next = function
          | (clause', _) :: rest when compare_index clause' clause < 0 ->
            process_tgt clause elements next rest
          | (clause', elements') :: rest when equal_index clause clause' ->
            let tl = process_next rest next in
            begin match pair_elements elements' elements with
              | [] -> tl
              | hd -> (clause, hd) :: tl
            end
          | src_chain -> process_next src_chain next
        and process_next src_chain = function
          | [] -> []
          | (clause, elements) :: next ->
            process_tgt clause elements next src_chain
        in
        process_next src_chain tgt_chain
      in
      let visit acc (_, Packed src) =
        assert (Boolvector.test chain_processed src.state.index);
        let acc = ref acc in
        let process_transition (DFA.Transition {label; target; mapping; _}) =
          let tgt = get_data target in
          let pairing =
            if Boolvector.test chain_processed target.index then
              process_shared_transition src mapping tgt
            else (
              process_direct_transition src mapping tgt;
              push acc (label, Packed tgt);
              []
            )
          in
          if not (List.is_empty pairing) then
            incr transitions_with_pairing;
          pairing
        in
        let pairings' = List.map process_transition src.state.transitions in
        pairings.:(src.state.index) <- pairings';
        !acc
      in
      let rec loop = function
        | [] -> ()
        | xs ->
          loop (List.fold_left visit []
                  (List.sort (fun (l1, _) (l2, _) -> IndexSet.compare l1 l2) xs))
      in
      loop (visit [] ((), Packed initial));
      stopwatch 3
        "constructed order chain with %d elements \
         (%d direct transitions, %d shared, %d trivial pairings, \
         %d non-trivial pairings, %d transitions with pairings)"
        (Order_chain.freeze chain)
        !direct_transitions
        !shared_transitions
        !trivial_pairing
        !nontrivial_pairing
        !transitions_with_pairing
    (*let count = ref 0 in
      Vector.iter (fun (Packed t) ->
        DFA.iter_transitions t (fun tr ->
            match tr.pairings with
            | [] -> ()
            | pairings ->
              Printf.eprintf "Transition with pairing #%d:\n" !count;
              incr count;
              List.iter (fun (clause, pairings) ->
                  Printf.eprintf "- clause %d:" (Index.to_int clause);
                  List.iter (fun (a, b) ->
                      Printf.eprintf " %d->%d"
                        (Order_chain.evaluate a)
                        (Order_chain.evaluate b))
                    pairings;
                  Printf.eprintf "\n"
                ) pairings
          )
      ) states*)

    let accepts = Vector.map begin fun (Packed t) ->
        let remainder = ref t.chain in
        let branches = t.state.branches in
        let rec loop i element = function
          | (i', element') :: rest
            when Index.compare i' i <= 0 &&
                 Index.equal branches.:(i').index branches.:(i).index ->
            loop i element' rest
          | rest ->
            remainder := rest;
            element
        in
        let get_element i = loop i (Order_chain.root chain) !remainder in
        let acc = ref [] in
        let test_branch i {Branch. accept; index} =
          if accept then
            push acc (index, Order_chain.evaluate (get_element i))
        in
        Vector.iteri test_branch branches;
        List.rev !acc
      end data

    let liveness =
      Vector.map
        (fun (DFA.Packed st) ->
           Array.make (Array.length (Vector.as_array st.branches)) IndexSet.empty)
        DFA.states

    let get_liveness (type n) (st : n DFA.t) : (n, Capture.set) vector =
      Vector.cast_array (Vector.length st.branches) liveness.:(st.index)

    let () =
      let todo = ref [] in
      let propagate (Packed src) =
        let live_src = get_liveness src.state in
        let process_transition (DFA.Transition {mapping; target; _}) =
          let changed = ref false in
          let tgt = get_data target in
          let live_tgt = get_liveness tgt.state in
          let process_mapping tgt_j (src_i, (captures, _usage)) =
            let live = IndexSet.union live_src.:(src_i) captures in
            let live' = live_tgt.:(tgt_j) in
            if not (IndexSet.equal live live') then (
              live_tgt.:(tgt_j) <- live;
              changed := true;
            )
          in
          Vector.iteri process_mapping mapping;
          if !changed then push todo (Packed tgt)
        in
        List.iter process_transition src.state.transitions
      in
      Vector.iter propagate data;
      fixpoint ~propagate todo

    let () = stopwatch 3 "Computed liveness"

    (* Naive allocator *)
    let registers : (DFA.n, Register.t Capture.map array) vector =
      Vector.map (fun live ->
          let last_reg = ref (-1) in
          let alloc_reg _ =
            incr last_reg;
            Register.of_int !last_reg
          in
          Array.map (IndexMap.inflate alloc_reg) live
        ) liveness

    let get_registers (type m) (st : m DFA.t) : (m, _) vector =
      Vector.cast_array (Vector.length st.branches) registers.:(st.index)

    let register_count =
      let max_live = ref 0 in
      let max_index = ref (-1) in
      let check_state (DFA.Packed state) =
        let regs = get_registers state in
        let max_live' =
          Vector.fold_left (fun sum map -> sum + IndexMap.cardinal map) 0 regs
        in
        max_live := max !max_live max_live';
        Vector.iter (IndexMap.iter (fun _ reg ->
            max_index := max !max_index (Index.to_int reg))) regs;
      in
      Vector.iter check_state DFA.states;
      stopwatch 3
        "allocated registers (max live registers: %d, register count: %d)"
        !max_live (!max_index + 1);
      !max_index + 1
  end

  module type MACHINE = sig
    type label = {
      filter: Lr1.set;
      (** The set of lr1 states that allow this transition to be taken. *)
      captures: (Capture.t * Register.t) list;
      (** The set of variables captured, and the register in which to store the
          variable, when the transition is taken. *)
      clear: Register.set;
      (** The set of registers to clear when the transition is taken. *)
      moves: Register.t Register.map;
      (** Registers to move when taking this transition.
          The source register is used as a key and the target as a value. *)
      priority: (Branch.t * priority * priority) list;
      (** Dynamic priority levels to remap.
          An element (c, p1, p2) means that a match of clause [c] at priority
          [p1] in the source state corresponds to a match at priority [p2] in
          the target state. *)
    }

    type states
    val states : states cardinal
    val initial : states index option

    type transitions
    val transitions : transitions cardinal

    val source : transitions index -> states index
    val label : transitions index -> label
    val target : transitions index -> states index

    (* Transitions labelled by Lr1 states in [unhandled st] are reachable
       (there exists viable stacks that can reach them), but are not defined
       (there is no [transitions] for them).
       They should be rejected at runtime. *)
    val unhandled : states index -> Lr1.set

    (* [outgoing st] is the set of transitions leaving [st] *)
    val outgoing : states index -> transitions indexset

    (* [matching st] is the set of clauses accepted when reaching [st].  Each
       clause comes with a priority level and a mapping indicating in which
       register captured variables can be found. *)
    val matching : states index -> (Branch.t * priority * Register.t Capture.map) list

    (* [threads st] list the clauses being recognized in state [st].
       The boolean indicates if the clause is accepted in this state. *)
    val branches : states index -> (Branch.state * Register.t Capture.map) list

    val partial_captures : Capture.set
  end

  module Minimize (DFA : DFA) (Dataflow : DATAFLOW with module DFA := DFA) : MACHINE =
  struct
    (* Labels of DFA transitions *)
    type label = {
      filter: Lr1.set;
      captures: (Capture.t * Register.t) list;
      clear: Register.set;
      moves: Register.t Register.map;
      priority: (Branch.t * int * int) list;
    }

    let label_compare t1 t2 =
      let c = IndexSet.compare t1.filter t2.filter in
      if c <> 0 then c else
        let c =
          List.compare
            (compare_pair compare_index compare_index)
            t1.captures t2.captures
        in
        if c <> 0 then c else
          let c = IndexMap.compare compare_index t1.moves t2.moves in
          if c <> 0 then c else
            let c = IndexSet.compare t1.clear t1.clear in
            c

    module Transition = struct
      type t = {
        source: DFA.n index;
        target: DFA.n index;
        label: label;
      }

      let partial_captures = ref IndexSet.empty

      include Vector.Of_array(struct
          type a = t
          let array =
            let dyn = Dynarray.create () in
            let process_transition source src_regs
                (DFA.Transition {label=filter; mapping; target; _}) pairings =
              let tgt_regs = Dataflow.get_registers target in
              let captures = ref [] in
              let moves = ref IndexMap.empty in
              let clear = ref IndexSet.empty in
              let process_mapping (src_i, (captured, _usage)) tgt_bank =
                let src_bank = src_regs.:(src_i) in
                let process_tgt_reg capture tgt_reg =
                  if IndexSet.mem capture captured then
                    push captures (capture, tgt_reg)
                  else
                    match IndexMap.find_opt capture src_bank with
                    | Some src_reg ->
                      if src_reg <> tgt_reg then
                        moves := IndexMap.add src_reg tgt_reg !moves
                    | None ->
                      partial_captures := IndexSet.add capture !partial_captures;
                      clear := IndexSet.add tgt_reg !clear
                in
                IndexMap.iter process_tgt_reg tgt_bank
              in
              Vector.iter2 process_mapping mapping tgt_regs;
              let captures = !captures and moves = !moves and clear = !clear in
              let priority = List.concat_map (fun (clause, pairs) ->
                  List.map
                    (fun (p1, p2) -> clause, Order_chain.evaluate p1, Order_chain.evaluate p2)
                    pairs
                ) pairings
              in
              let label = {filter; captures; moves; clear; priority} in
              Dynarray.add_last dyn {source; target = target.index; label};
            in
            let process_state (DFA.Packed source) pairings =
              List.iter2
                (process_transition source.index (Dataflow.get_registers source))
                source.transitions pairings
            in
            Vector.iter2 process_state DFA.states Dataflow.pairings;
            Dynarray.to_array dyn
        end)

      let n = Vector.length vector

      let partial_captures = !partial_captures
    end

    let partial_captures =
      let acc = Transition.partial_captures in
      Vector.fold_left begin fun acc (DFA.Packed st) ->
        Vector.fold_left2 begin fun acc (br : Branch.state) regs ->
          if br.accept then
            let cap = Branch.branch_captures br.index in
            IndexSet.fold begin fun var acc ->
              if IndexMap.mem var regs
              then acc
              else IndexSet.add var acc
            end cap acc
          else acc
        end acc st.branches (Dataflow.get_registers st)
      end acc DFA.states

    module Min = Valmari.Minimize_with_custom_decomposition(struct
        type states = DFA.n
        let states = DFA.n

        type transitions = Transition.n
        let transitions = Transition.n

        type [@ocaml.warning "-34"] nonrec label = label
        let label i = Transition.vector.:(i).label
        let source i = Transition.vector.:(i).source
        let target i = Transition.vector.:(i).target

        let initials f = f DFA.initial
        let finals f =
          Vector.iteri (fun index accepts ->
              match accepts with
              | [] -> ()
              | _ :: _ -> f index
            ) Dataflow.accepts

        let [@ocaml.warning "-32"] refinements refine =
          (* Refine states by accepted actions *)
          let table = Hashtbl.create 7 in
          Vector.rev_iteri (fun index accepts ->
              match accepts with
              | [] -> ()
              | _ :: _ ->
                match Hashtbl.find_opt table accepts with
                | None -> Hashtbl.add table accepts (ref (IndexSet.singleton index))
                | Some r -> r := IndexSet.add index !r
            ) Dataflow.accepts;
          Hashtbl.iter
            (fun _ r -> refine (fun ~add -> IndexSet.iter add !r))
            table

        let [@ocaml.warning "-32"] decomposition refine =
          let acc = ref [] in
          let actions = ref [] in
          Index.iter transitions (fun tr ->
              let label = label tr in
              push acc (label.filter, tr);
              if label.captures <> [] ||
                 not (IndexSet.is_empty label.clear) ||
                 not (IndexMap.is_empty label.moves) then
                push actions ({label with filter = IndexSet.empty}, tr);
            );
          IndexRefine.iter_decomposition !acc
            (fun _set iter -> refine (fun ~add -> iter add));
          let actions = List.sort (fun (l1, _) (l2, _) -> label_compare l1 l2) !actions in
          let rec group_actions l ks = function
            | (l', k) :: rest when label_compare l l' = 0 ->
              group_actions l (k :: ks) rest
            | rest ->
              refine (fun ~add -> List.iter add ks);
              start rest
          and start = function
            | [] -> ()
            | (l, k) :: rest -> group_actions l [k] rest
          in
          start actions
      end)

    type states = Min.states
    let states = Min.states

    type transitions = Min.transitions
    let transitions = Min.transitions

    let source = Min.source
    let label  = Min.label
    let target = Min.target

    let initial =
      if Array.length Min.initials = 0
      then None
      else Some Min.initials.(0)

    let outgoing = Vector.make Min.states IndexSet.empty
    let unhandled = Vector.make Min.states IndexSet.empty

    let () =
      (* Initialize with all reachable labels *)
      Index.iter DFA.n begin fun big ->
        match Min.transport_state big with
        | None -> ()
        | Some min -> unhandled.@(min) <- IndexSet.union (DFA.domain big)
      end;
      (* Remove the ones for which transitions exist *)
      Index.rev_iter Min.transitions begin fun tr ->
        let index = Min.source tr in
        let label = Min.label tr in
        let visited = unhandled.:(index) in
        let visited = IndexSet.diff visited label.filter in
        unhandled.:(index) <- visited;
        outgoing.@(index) <- IndexSet.add tr
      end

    let outgoing = Vector.get outgoing
    let unhandled = Vector.get unhandled

    let matching state =
      let DFA.Packed source = DFA.states.:(Min.represent_state state) in
      let priorities = ref Dataflow.accepts.:(source.index) in
      let get_priority clause =
        match !priorities with
        | (clause', p) :: rest ->
          if not (Index.equal clause clause') then (
            Printf.eprintf "Accepting clause %d but got priority for clause %d?!\n"
              (Index.to_int clause) (Index.to_int clause');
            assert false
          ) else if false then
            Printf.eprintf "Accepting clause %d with priority %d\n"
              (Index.to_int clause) p;
          priorities := rest;
          p
        | [] -> assert false
      in
      let add_accepting acc {Branch. index; accept} regs =
        if accept
        then (index, get_priority index, regs) :: acc
        else acc
      in
      let registers = Dataflow.get_registers source in
      List.rev (Vector.fold_left2 add_accepting [] source.branches registers)

    let branches state =
      let DFA.Packed source = DFA.states.:(Min.represent_state state) in
      let add_accepting branch regs acc = (branch, regs) :: acc in
      let registers = Dataflow.get_registers source in
      Vector.fold_right2 add_accepting source.branches registers []

    let () = stopwatch 3 "OutDFA of %s" rule.name
  end

  module Codegen (M : MACHINE) = struct
    let captures_lr1 =
      let map = ref IndexMap.empty in
      Index.iter M.transitions (fun tr ->
          let label = M.label tr in
          map := List.fold_left (fun map (cap, _reg) ->
              IndexMap.update cap (function
                  | None -> Some label.filter
                  | Some set' -> Some (IndexSet.union set' label.filter)
                ) map
            ) !map label.captures
        );
      !map

    let recover_type index =
      try
        let lr1s = IndexMap.find index captures_lr1 in
        let symbols = IndexSet.map (fun lr1 ->
            match Lr1.incoming lr1 with
            | None -> raise Not_found
            | Some sym -> sym
          ) lr1s
        in
        let typ = IndexSet.fold (fun sym acc ->
            let typ = match Symbol.semantic_value sym with
              | None -> raise Not_found
              | Some typ -> String.trim typ
            in
            match acc with
            | None -> Some typ
            | Some typ' ->
              if typ <> typ' then raise Not_found;
              acc
          ) symbols None
        in
        match typ with
        | None -> None
        | Some typ -> Some (symbols, typ)
      with Not_found -> None

    let symbol_matcher s = match Info.Symbol.prj s with
      | L t -> "T T_" ^ Info.Terminal.to_string t
      | R n -> "N N_" ^ Grammar.Nonterminal.mangled_name (Info.Nonterminal.to_g n)

    let bytes_match b i str =
      Bytes.length b >= i + String.length str &&
      let exception Exit in
      match
        for j = 0 to String.length str - 1 do
          if Bytes.get b (i + j) <> String.get str j then
            raise Exit
        done
      with
      | () -> true
      | exception Exit -> false

    let rewrite_loc_keywords str =
      let b = Bytes.of_string str in
      let l = Bytes.length b in
      let i = ref 0 in
      while !i < l do
        if Bytes.get b !i = '$' &&
           (bytes_match b (!i + 1) "startloc(" ||
            bytes_match b (!i + 1) "endloc(")
        then (
          Bytes.set b !i '_';
          while Bytes.get b !i <> '(' do incr i; done;
          Bytes.set b !i '_';
          while !i < l  && Bytes.get b !i <> ')' do incr i; done;
          if !i < l then Bytes.set b !i '_'
        )
        else incr i
      done;
      Bytes.to_string b

    let bind_capture out ~roffset index (def, name) =
      let is_optional = IndexSet.mem index M.partial_captures in
      let none = if is_optional then "None" else "assert false" in
      let some x = if is_optional then "Some (" ^ x ^ ")" else x in
      let offset = !roffset in
      incr roffset;
      match def with
      | Syntax.Value ->
        let typ = recover_type index in
        Code_printer.fmt out
          "    let %s, _startloc_%s_, _endloc_%s_ = match __registers.(%d) with \n\
          \      | Empty -> %s\n\
          \      | Initial -> assert false\n\
          \      | Value (%s.MenhirInterpreter.Element (%s, %s, startp, endp)%s) ->\n"
          name name name offset
          (if is_optional then "(None, None, None)" else "assert false")
          parser_name
          (if Option.is_none typ then "_" else "st")
          (if Option.is_none typ then "_" else "x")
          (if Option.is_none typ then "as x" else "");
        begin match typ with
          | None -> ()
          | Some (symbols, typ) ->
            Code_printer.fmt out
              "        let x = match %s.MenhirInterpreter.incoming_symbol st with\n"
              parser_name;
            List.iter (fun symbol ->
                Code_printer.fmt out "          | %s -> (x : %s)\n"
                  (symbol_matcher symbol) typ) (IndexSet.elements symbols);
            Code_printer.fmt out
              "          | _ -> assert false\n\
              \        in\n"
        end;
        Code_printer.fmt out "        (%s, %s, %s)\n" (some "x") (some "startp") (some "endp");
        Code_printer.fmt out "    in\n";
        Code_printer.fmt out "    let _ = %s in\n" name
      | Start_loc ->
        Code_printer.fmt out
          "    let _startloc_%s_ = match __registers.(%d) with\n\
          \      | Empty -> %s\n\
          \      | Initial -> %s\n\
          \      | Value (%s.MenhirInterpreter.Element (_, _, p, _)) -> %s\n\
          \    in\n"
          name offset
          none
          (some "__initialpos")
          parser_name (some "p")
      | End_loc ->
        Code_printer.fmt out
          "    let _endloc_%s_ = match __registers.(%d) with\n\
          \      | Empty -> %s\n\
          \      | Initial -> %s\n\
          \      | Value (%s.MenhirInterpreter.Element (_, _, _, p)) -> %s\n\
          \    in\n"
          name offset
          none
          (some "__initialpos")
          parser_name (some "p")

    let lookahead_constraint branch =
      match Branch.branch_lookaheads branch with
      | None -> None
      | Some terms ->
        let term_pattern t =
          let name = Info.Terminal.to_string t in
          match Info.Terminal.semantic_value t with
          | None -> name
          | Some _ -> name ^ " _"
        in
        Some (string_concat_map ~wrap:("(",")") "|"
                term_pattern (IndexSet.elements terms))

    let output_code out =
      Code_printer.fmt out
        "let lrgrep_execute_%s %s\n\
        \  (__clause, (__registers : %s.MenhirInterpreter.element Lrgrep_runtime.register_values))\n\
        \  (__initialpos : Lexing.position)\n\
        \  ((token : %s.MenhirInterpreter.token), _startloc_token_, _endloc_token_)\n\
        \  : _ option = match __clause, token with\n"
        rule.name (String.concat " " rule.args)
        parser_name parser_name;
      let output_clause clause =
        let branches = Branch.clause_branches clause in
        let captures = Branch.clause_captures clause in
        Code_printer.fmt out " ";
        IndexSet.iter (fun branch ->
            Code_printer.fmt out
              " | %d, %s"
              (Index.to_int branch)
              (Option.value (lookahead_constraint branch) ~default:"_");
          ) branches;
        Code_printer.fmt out " ->\n";
        IndexMap.iter (bind_capture out ~roffset:(ref 0)) captures;
        begin match (Branch.clause_syntax clause).action with
          | Unreachable ->
            Code_printer.print out "    failwith \"Should be unreachable\"\n"
          | Partial (loc, str) ->
            Code_printer.print out "    (\n";
            Code_printer.fmt out ~loc "%s\n" (rewrite_loc_keywords str);
            Code_printer.print out "    )\n"
          | Total (loc, str) ->
            Code_printer.print out "    Some (\n";
            Code_printer.fmt out ~loc "%s\n" (rewrite_loc_keywords str);
            Code_printer.print out "    )\n"
        end;
        let constrained =
          IndexSet.filter
            (fun branch -> Option.is_some (Branch.branch_lookaheads branch))
            branches
        in
        if not (IndexSet.is_empty constrained) then
          Code_printer.fmt out "  | (%s), _ -> None\n"
            (string_concat_map "|" string_of_index (IndexSet.elements constrained))
      in
      Index.iter Branch.clause output_clause;
      Code_printer.print out "  | _ -> failwith \"Invalid action (internal error or API misuse)\"\n\n"
  end
end
