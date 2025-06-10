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
open Lrgrep_support
open Info
open Spec
open Regexp

type ('g, 'n) stacks = {
  tops: 'n indexset;
  prev: 'n index -> 'n indexset;
  label: 'n index -> 'g lr1 indexset;
}

type priority = int

module NFA = struct

  type ('g, 'r) t = {
    uid: int;
    k: 'g K.t;
    transitions: ('g Label.t * ('g, 'r) t lazy_t) list;
    branch: ('g, 'r) branch index;
    accept: bool;
    mutable mark: unit ref;
  }

  let compare t1 t2 =
    Int.compare t1.uid t2.uid

  let default_mark = ref ()

  let uid =
    let k = ref 0 in
    fun () -> incr k; !k

  let make (type g) (g : g grammar) viable branch =
    let module KMap = Map.Make(struct type t = g Regexp.K.t let compare = Regexp.K.compare end) in
    let nfa = ref KMap.empty in
    let rec aux k =
      match KMap.find_opt k !nfa with
      | Some t -> t
      | None ->
        let accept = ref false in
        let rec process_transitions = function
          | [] -> []
          | (label, None) :: rest -> (label, accepting) :: process_transitions rest
          | (label, Some k') :: rest -> (label, lazy (aux k')) :: process_transitions rest
        in
        let inj ({Label. filter; usage; captures}, t) = (filter, (usage, captures, t)) in
        let prj filter (usage, captures, t) = ({Label. filter; usage; captures}, t) in
        let transitions =
          K.derive viable (Lr1.all g) k
          |> process_transitions
          |> List.map inj
          |> IndexRefine.annotated_partition
          |> List.concat_map (fun (filter, l) -> List.map (prj filter) l)
        in
        let uid = uid () in
        let accept = !accept in
        let t = {uid; k; transitions; branch; accept; mark=default_mark} in
        nfa := KMap.add k t !nfa;
        t
    and accepting = lazy (aux K.Done)
    in
    aux

  let from_branches info viable branches =
    Vector.mapi (fun br re -> make info viable br (Regexp.K.More (re, Regexp.K.Done)))
      branches.expr
end

module DFA = struct
  type ('src, 'tgt) mapping = ('tgt, 'src index * (Capture.set * Usage.set)) vector

  type ('g, 'r, 'dfa, 'n) state = {
    index: 'dfa index;
    branches: ('n, ('g, 'r) branch index) vector;
    accepting: 'n Boolvector.t;
    mutable transitions : ('g, 'r, 'dfa, 'n) transition list;
  }

  and ('g, 'r, 'dfa, 'src) transition = Transition : {
      label: 'g lr1 indexset;
      target: ('g, 'r, 'dfa, 'tgt) state;
      mapping: ('src, 'tgt) mapping;
    } -> ('g, 'r, 'dfa, 'src) transition

  type ('g, 'r, 'dfa) packed = Packed : ('g, 'r, 'dfa, 'n) state -> ('g, 'r, 'dfa) packed [@@ocaml.unboxed]

  type ('g, 'r, 'dfa) t = {
    initial: 'dfa index;
    states: ('dfa, ('g, 'r, 'dfa) packed) vector;
    domain: ('dfa, 'g lr1 indexset) vector;
  }

  type ('g, 'r) _t = T : ('g, 'r, 'dfa) t -> ('g, 'r) _t

  let determinize (type g r s)
      (branches: (g, r) branches)
      (stacks: (g, s) stacks) initial : (g, r) _t
    =
    let module Construction = struct
      include IndexBuffer.Gen.Make()

      type 'n prestate = {
        index: n index;
        kernel: ('n, (g, r) NFA.t) vector;
        accept: (g, r) branch index option;
        mutable raw_transitions: (g lr1 indexset * 'n fwd_mapping lazy_t) list;
      }

      and 'src fwd_mapping =
          Fwd_mapping : ('src, 'tgt) mapping * 'tgt prestate -> 'src fwd_mapping

      type prepacked = Prepacked : 'n prestate -> prepacked [@@ocaml.unboxed]

      let prestates = get_generator ()

      module KernelMap = Map.Make(struct
          type t = (g, r) NFA.t array
          let compare g1 g2 = array_compare NFA.compare g1 g2
        end)

      let kernel_make (type a) (prj : a -> (g, r) NFA.t) (ts : a list) : a array =
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
        let rec determinize : type n . (n, (g, r) NFA.t) vector -> n prestate =
          fun kernel ->
            match KernelMap.find_opt (Vector.as_array kernel) !dfa with
            | Some (Prepacked t') ->
              let Refl = assert_equal_length kernel t'.kernel in
              t'
            | None ->
              let accept = ref None in
              let rev_transitions =
                let make i ({Label. filter; captures; usage}, t) =
                  (filter, (i, (captures, usage), t))
                in
                kernel_fold
                  (fun i (nfa : (g, r) NFA.t) acc ->
                     if nfa.accept && Boolvector.test branches.is_total nfa.branch then
                       accept := Some nfa.branch;
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
          Vector.of_array (kernel_make Fun.id (Vector.to_list initial))
        in
        (determinize kernel).index

      let () = stopwatch 3 "Processed initial states"

      let visited: (n, s indexset) IndexBuffer.Dyn.t =
        IndexBuffer.Dyn.make IndexSet.empty

      let scheduled: (n, s indexset) IndexBuffer.Dyn.t =
        IndexBuffer.Dyn.make IndexSet.empty

      let (.*()) = IndexBuffer.Dyn.get
      let (.*()<-) = IndexBuffer.Dyn.set

      let () =
        let accepting = Vector.make (branch_count branches) [] in
        let todo = ref [] in
        let min_clause t = (Vector.as_array t.kernel).(0).branch in
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
              if IndexSet.disjoint (stacks.label stack) label
              then IndexSet.empty
              else (really_empty := false; stacks.prev stack)
            in
            let stacks = indexset_bind todo expand_stack in
            if not !really_empty then
              let lazy (Fwd_mapping (_, t')) = target in
              if not (IndexSet.is_empty stacks) then
                schedule bound t'.index stacks
          end t.raw_transitions
        in
        let next_bound = Index.rev_enumerate (branch_count branches) in
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
          schedule bound initial stacks.tops;
          loop bound
        with Index.End_of_set -> ()

      let prestates = IndexBuffer.Gen.freeze prestates

      let domain =
        Vector.init n (fun i -> indexset_bind visited.*(i) stacks.label)
    end in
    let states =
      let make (Construction.Prepacked {index; kernel; _}) =
        let branches = Vector.map (fun t -> t.NFA.branch) kernel in
        let accepting = Boolvector.from_vector kernel (fun t -> t.NFA.accept) in
        Packed {index; branches; accepting; transitions = []}
      in
      Vector.map make Construction.prestates
    in
    let from_prestate (type n) (p : n Construction.prestate) : (g, r, _, n) state =
      let Packed t = states.:(p.index) in
      let Refl = assert_equal_length t.branches p.kernel in
      t
    in
    Vector.iteri (fun i (Construction.Prepacked p) ->
        let t = from_prestate p in
        let domain = Construction.domain.:(i) in
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
      ) Construction.prestates;
    stopwatch 3 "Determinized DFA (%d states)" (cardinal Construction.n);
    T {initial = Construction.initial; states; domain = Construction.domain}

  let state_count dfa = Vector.length dfa.states
end

module Dataflow = struct
  type chain = (Order_chain.element * Order_chain.element) list

  type ('g, 'r, 'dfa) t = {
    pairings : ('dfa, (('g, 'r) branch index * chain) list list) vector;
    accepts : ('dfa, (('g, 'r) branch index * priority) list) vector;
    liveness : 'n. ('g, 'r, 'dfa, 'n) DFA.state -> ('n, Capture.set) vector;
    registers : 'n. ('g, 'r, 'dfa, 'n) DFA.state -> ('n, Register.t Capture.map) vector;
    register_count : int;
  }

  type ('g, 'r, 'dfa, 'tgt) rev_mapping = Rev_mapping
      :  ('g, 'r, 'dfa, 'src) DFA.state * ('src, 'tgt) DFA.mapping
      -> ('g, 'r, 'dfa, 'tgt) rev_mapping

  type ('g, 'r, 'dfa) packed_rev_mapping = Rev_packed
      :  ('g, 'r, 'dfa, 'n) rev_mapping list
      -> ('g, 'r, 'dfa) packed_rev_mapping [@@ocaml.unboxed]

  let reverse_transitions dfa =
    let table = Vector.make (DFA.state_count dfa) (Rev_packed []) in
    Vector.iter begin fun (DFA.Packed src) ->
      let process (DFA.Transition {target; mapping; _}) =
        match table.:(target.index) with
        | Rev_packed [] ->
          table.:(target.index) <- Rev_packed [Rev_mapping (src, mapping)]
        | Rev_packed (Rev_mapping (_, mapping0) :: _ as xs) ->
          let Refl = assert_equal_length mapping mapping0 in
          table.:(target.index) <- Rev_packed (Rev_mapping (src, mapping) :: xs)
      in
      List.iter process src.transitions
    end dfa.states;
    table

  let make (type g r dfa) branches (dfa : (g, r, dfa) DFA.t) =
    let reverse_transitions = reverse_transitions dfa in
    let iter_reverse_transitions (type n)
        (t : (g, r, dfa, n) DFA.state)
        (f : (g, r, dfa, n) rev_mapping -> unit)
      =
      match reverse_transitions.:(t.index) with
      | Rev_packed [] -> ()
      | Rev_packed (Rev_mapping (_, mapping0) :: _ as xs) ->
        let Refl = assert_equal_length mapping0 t.branches in
        List.iter f xs
    in
    let module Dataflow = struct
      type 'n data = {
        state: (g, r, dfa, 'n) DFA.state;
        mutable reachable: 'n indexset;
        mutable splits: 'n indexset;
        mutable new_splits: 'n indexset;
        mutable chain: ('n index * Order_chain.element) list;
      }

      type packed = Packed : 'n data -> packed [@@ocaml.unboxed]

      let data =
        Vector.map (fun (DFA.Packed t) ->
            let n = Vector.length t.branches in
            let reachable = IndexSet.init_from_set n (Boolvector.test t.accepting) in
            let splits = IndexSet.empty in
            let new_splits = IndexSet.empty in
            Packed {state=t; reachable; splits; new_splits; chain=[]}
          ) dfa.states

      let get_data (type n) (st : (g, r, dfa, n) DFA.state) : n data =
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
        fixpoint ~propagate todo;
        stopwatch 3 "Computed reachability"

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
          let Packed t = data.:(dfa.initial) in
          IndexSet.map (Vector.get t.state.branches) t.reachable
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
        Vector.iteri (fun branch (pattern : Syntax.pattern) ->
            if IndexSet.mem branch reachable_branches then
              check pattern.expr
            else
              Printf.eprintf "Warning: clause line %d, column %d is unreachable\n"
                pattern.expr.position.line pattern.expr.position.col
          ) branches.pattern

      let () = stopwatch 3 "Dead-code analysis"

      let () =
        let count = ref 0 in
        let todo = ref [] in
        Vector.iter (fun (Packed t) ->
            t.new_splits <-
              IndexSet.init_from_set
                (Vector.length t.state.branches)
                (Boolvector.test t.state.accepting);
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
                if Index.equal src.state.branches.:(x') branch then
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

      let pairings = Vector.make (DFA.state_count dfa) []

      let group_by_branch t = function
        | [] -> []
        | (i, _) as x :: xs ->
          let rec loop branch acc accs = function
            | [] -> List.rev ((branch, List.rev acc) :: accs)
            | (i, _) as x :: xs ->
              let branch' = t.DFA.branches.:(i) in
              if branch = branch' then
                loop branch (x :: acc) accs xs
              else
                loop branch' [x] ((branch, List.rev acc) :: accs) xs
          in
          loop t.branches.:(i) [x] [] xs

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
        let chain_processed = Boolvector.make (DFA.state_count dfa) false in
        let root = Order_chain.root chain in
        let Packed initial = data.:(dfa.initial) in
        initial.chain <- (
          match IndexSet.elements initial.splits with
          | [] -> []
          | splits ->
            let rec fresh_chain branch element = function
              | [] -> []
              | m :: ms ->
                let branch' = initial.state.branches.:(m) in
                let element =
                  if Index.equal branch branch'
                  then Order_chain.next element
                  else root
                in
                (m, element) :: fresh_chain branch' element ms
            in
            fresh_chain (Index.of_int (branch_count branches) 0) root splits
        );
        Boolvector.set chain_processed dfa.initial;
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
            | (n, _) as x :: xs when Index.equal sbranches.:(n) branch ->
              extract_branch branch (x :: acc) xs
            | rest -> List.rev acc, rest
          in
          let rec seek_branch branch = function
            | [] -> [], []
            | ((n, _) as x :: xs) as xxs ->
              let c = Index.compare sbranches.:(n) branch in
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
              let branch = tbranches.:(m) in
              let chain, rest = seek_branch branch chain in
              process_branch branch chain rest m ms
          and process_branch branch chain rest m ms =
            let i, _ = mapping.:(m) in
            let split, chain = chain_next_split i root chain in
            (m, split) :: process_continue_branch branch chain rest ms
          and process_continue_branch branch chain rest = function
            | m :: ms when Index.equal tbranches.:(m) branch ->
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

      let accepts = Vector.map begin fun (Packed t) ->
          let remainder = ref t.chain in
          let accepting = t.state.accepting in
          let branches = t.state.branches in
          let rec loop i element = function
            | (i', element') :: rest
              when Index.compare i' i <= 0 &&
                   Index.equal branches.:(i') branches.:(i) ->
              loop i element' rest
            | rest ->
              remainder := rest;
              element
          in
          let get_element i = loop i (Order_chain.root chain) !remainder in
          let acc = ref [] in
          let test_branch i index =
            if Boolvector.test accepting i then
              push acc (index, Order_chain.evaluate (get_element i))
          in
          Vector.iteri test_branch branches;
          List.rev !acc
        end data

      let liveness =
        Vector.map
          (fun (DFA.Packed st) ->
             Array.make (Array.length (Vector.as_array st.branches)) IndexSet.empty)
          dfa.states

      let get_liveness (type n) (st : (_, _, _, n) DFA.state) : (n, Capture.set) vector =
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
      let registers : (dfa, Register.t Capture.map array) vector =
        Vector.map (fun live ->
            let last_reg = ref (-1) in
            let alloc_reg _ =
              incr last_reg;
              Register.of_int !last_reg
            in
            Array.map (IndexMap.inflate alloc_reg) live
          ) liveness

      let get_registers (type m) (st : (_, _, _, m) DFA.state) : (m, _) vector =
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
        Vector.iter check_state dfa.states;
        stopwatch 3
          "allocated registers (max live registers: %d, register count: %d)"
          !max_live (!max_index + 1);
        !max_index + 1
    end
    in
    let open Dataflow in
    {pairings; accepts; register_count;
     liveness = get_liveness; registers = get_registers}
end

module Machine = struct
  type ('g, 'r) label = {
    filter: 'g lr1 indexset;
    (** The set of lr1 states that allow this transition to be taken. *)
    captures: (Capture.t * Register.t) list;
    (** The set of variables captured, and the register in which to store the
        variable, when the transition is taken. *)
    clear: Register.set;
    (** The set of registers to clear when the transition is taken. *)
    moves: Register.t Register.map;
    (** Registers to move when taking this transition.
        The source register is used as a key and the target as a value. *)
    priority: (('g, 'r) branch index * priority * priority) list;
    (** Dynamic priority levels to remap.
        An element (c, p1, p2) means that a match of clause [c] at priority
        [p1] in the source state corresponds to a match at priority [p2] in
        the target state. *)
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

  type ('g, 'r, 'st, 'tr) t = {
    initial: 'st index option;
    source: ('tr, 'st index) vector;
    target: ('tr, 'st index) vector;

    label: ('tr, ('g, 'r) label) vector;

    (* Transitions labelled by Lr1 states in [unhandled st] are reachable
       (there exists viable stacks that can reach them), but are not defined
       (there is no [transitions] for them).
       They should be rejected at runtime. *)
    unhandled: ('st, 'g lr1 indexset) vector;

    (* [outgoing st] is the set of transitions leaving [st] *)
    outgoing: ('st, 'tr indexset) vector;

    (* [accepting.:(st)] lists the clauses accepted when reaching [st].  Each
       clause comes with a priority level and a mapping indicating in which
       register captured variables can be found. *)
    accepting: ('st, (('g, 'r) branch index * priority * Register.t Capture.map) list) vector;

    (* [branches.:(st)] lists the clauses being recognized in state [st].
       The boolean indicates if the clause is accepted in this state. *)
    branches: ('st, (('g, 'r) branch index * bool * Register.t Capture.map) list) vector;

    register_count : int;
    partial_captures : Capture.set;
  }

  type ('g, 'r) _t = T : ('g, 'r, 'st, 'tr) t -> ('g, 'r) _t


  let minimize (type g r dfa)
      (branches : (g, r) branches)
      (dfa : (g, r, dfa) DFA.t)
      (dataflow : (g, r, dfa) Dataflow.t)
    =
    let partial_captures = ref IndexSet.empty in
    let module Transition = struct
      type t = {
        source: dfa index;
        target: dfa index;
        label: (g, r) label;
      }
      include Vector.Of_array(struct
          type a = t
          let array =
            let dyn = Dynarray.create () in
            let process_transition source src_regs
                (DFA.Transition {label=filter; mapping; target; _}) pairings =
              let tgt_regs = dataflow.registers target in
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
                (process_transition source.index (dataflow.registers source))
                source.transitions pairings
            in
            Vector.iter2 process_state dfa.states dataflow.pairings;
            Dynarray.to_array dyn
        end)
      let n = Vector.length vector
    end in
    let partial_captures =
      let acc = !partial_captures in
      Vector.fold_left begin fun acc (DFA.Packed st) ->
        Vector.fold_lefti2 begin fun acc i index regs ->
          if Boolvector.test st.accepting i then
            let cap = branches.br_captures.:(index) in
            IndexSet.fold begin fun var acc ->
              if IndexMap.mem var regs
              then acc
              else IndexSet.add var acc
            end cap acc
          else acc
        end acc st.branches (dataflow.registers st)
      end acc dfa.states
    in
    let module Min = Valmari.Minimize_with_custom_decomposition(struct
      type states = dfa
      let states = DFA.state_count dfa

      type transitions = Transition.n
      let transitions = Transition.n

      type [@ocaml.warning "-34"] nonrec label = (g, r) label
      let label i = Transition.vector.:(i).label
      let source i = Transition.vector.:(i).source
      let target i = Transition.vector.:(i).target

      let initials f = f dfa.initial
      let finals f =
        Vector.iteri (fun index accepts ->
            match accepts with
            | [] -> ()
            | _ :: _ -> f index
          ) dataflow.accepts

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
          ) dataflow.accepts;
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
    in
    let initial =
      if Array.length Min.initials = 0
      then None
      else Some Min.initials.(0)
    in
    let source = Vector.init Min.transitions Min.source in
    let target = Vector.init Min.transitions Min.target in
    let label = Vector.init Min.transitions Min.label in
    let outgoing = Vector.make Min.states IndexSet.empty in
    let unhandled = Vector.make Min.states IndexSet.empty in
    let accepting =
      Vector.init Min.states @@ fun state ->
      let DFA.Packed source = dfa.states.:(Min.represent_state state) in
      let priorities = ref dataflow.accepts.:(source.index) in
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
      let add_accepting acc i index regs =
        if Boolvector.test source.accepting i
        then (index, get_priority index, regs) :: acc
        else acc
      in
      let registers = dataflow.registers source in
      List.rev (Vector.fold_lefti2 add_accepting [] source.branches registers)
    in
    let branches =
      Vector.init Min.states @@ fun state ->
      let DFA.Packed source = dfa.states.:(Min.represent_state state) in
      let add_branch i branch regs acc =
        (branch, Boolvector.test source.accepting i, regs) :: acc
      in
      let registers = dataflow.registers source in
      Vector.fold_righti2 add_branch source.branches registers []
    in
    stopwatch 3 "OutDFA";
    T {initial; source; target; label; unhandled; outgoing; partial_captures;
       register_count = dataflow.register_count; accepting; branches}

  let states t = Vector.length t.outgoing
end
