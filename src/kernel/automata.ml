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

module type STACKS = sig
  type lr1
  type n
  val n : n cardinal
  val initials : n indexset
  val next : n index -> n indexset
  val label : n index -> lr1 indexset
end

module Entry
    (Transl : Transl.S)
    (Stacks: STACKS with type lr1 := Transl.Regexp.Info.Lr1.n)
    (E : sig
       val parser_name : string
       val entry : Syntax.entry
     end)
    () :
sig
  open Transl.Regexp.Info

  (* Representation of a clause *)
  module Clause : sig
    include CARDINAL
    type t = n index

    (* Is the clause total? *)
    val total : t -> bool

    (* Lookahead constraints of this clause, if any *)
    val lookaheads : t -> Terminal.set option
  end

  (* Variables captured by a clause *)
  val captures : Clause.t -> Capture.set

  (* The first DFA produced. Exposed only to gather statistics.
     Big because it is before optimization/minimization. *)
  module BigDFA : sig
    type n
    val n : n cardinal
  end

  (* Labels annotating transitions *)
  module Label : sig
    type t = {
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
      priority: (Clause.t * priority * priority) list;
      (** Dynamic priority levels to remap.
          An element (c, p1, p2) means that a match of clause [c] at priority
          [p1] in the source state corresponds to a match at priority [p2] in
          the target state. *)
    }
    (* Maximum number of register used *)
    val register_count : int
    val compare : t -> t -> int
  end

  (* BigDFA after minimization.  Exposed only to gather statistics. *)
  module MinDFA : sig
    type states
    val states : states cardinal
  end

  (* Final DFA, produced after all optimizations. *)
  module OutDFA : sig
    type states
    val states : states cardinal
    val initial : states index

    type transitions
    val transitions : transitions cardinal

    val source : transitions index -> states index
    val label : transitions index -> Label.t
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
    val matching : states index -> (Clause.t * priority * Register.t Capture.map) list

    (* [threads st] list the clauses being recognized in state [st].
       The boolean indicates if the clause is accepted in this state. *)
    val threads : states index -> (bool * Clause.t * Register.t Capture.map) list
  end

  val output_code : Code_printer.t -> unit
end =
struct
  open Transl
  open Regexp
  open Info

  let time = Stopwatch.enter Stopwatch.main "Processing entry %s" E.entry.name

  module Clause = struct
    module Actions = Const(struct let cardinal = List.length E.entry.clauses end)

    type actions = Actions.n

    type desc = {
      action: actions index;
      pattern: Syntax.pattern;
      partial: bool;
      has_lookaheads: bool;
    }

    include Vector.Of_array(struct
        type a = desc
        let array = Array.of_list (
            E.entry.clauses
            |> List.mapi (fun index clause ->
                clause.Syntax.patterns
                |> List.map (fun pattern ->
                    {
                      action = Index.of_int Actions.n index;
                      pattern;
                      partial = (match clause.Syntax.action with
                          | Syntax.Unreachable -> false
                          | Syntax.Total _ -> false
                          | Syntax.Partial _ -> true);
                      has_lookaheads = (pattern.lookaheads <> []);
                    }
                  )
              )
            |> List.flatten
          )
      end)

    type t = n index

    let n = Vector.length vector

    let actions =
      let index = ref 0 in
      let list =
        List.map (fun clause ->
            let count = List.length clause.Syntax.patterns in
            let first = Index.of_int n !index in
            index := !index + count;
            let last = Index.of_int n (!index - 1) in
            clause, IndexSet.init_interval first last
          ) E.entry.clauses
      in
      Vector.cast_array Actions.n (Array.of_list list)

    let total c = not (Vector.get vector c).partial

    let lookaheads = tabulate_finset n (fun st ->
        match (Vector.get vector st).pattern.lookaheads with
        | [] -> None
        | symbols ->
          let lookahead_msg =
            "Lookahead can either be a terminal or `first(nonterminal)'"
          in
          let sym_pattern (sym, pos) =
            match sym with
            | Syntax.Apply ("first", [sym]) ->
              begin match Info.Symbol.prj (Indices.get_symbol pos sym) with
                | L t ->
                  let t = Info.Terminal.to_string t in
                  failwith (lookahead_msg ^ "; in first(" ^ t ^ "), " ^
                            t ^ " is a terminal")
                | R n ->
                  Nonterminal.to_g n
                  |> Grammar.Nonterminal.first
                  |> List.map Info.Terminal.of_g
              end
            | Syntax.Name _ ->
              begin match Symbol.prj (Indices.get_symbol pos sym) with
                | R n ->
                  failwith (lookahead_msg ^ "; " ^
                            Info.Nonterminal.to_string n ^ " is a nonterminal")
                | L t -> [t]
              end
            | _ ->
              failwith lookahead_msg
          in
          Some (IndexSet.of_list (List.concat_map sym_pattern symbols))
      )
  end

  module LazyNFA = struct
    type t = {
      (* State construction *)
      uid: int;
      k: K.t;
      transitions: (K.label * t lazy_t) list;
      accept: bool;
      clause: Clause.t;
      mutable mark: unit ref;
    }

    let compare t1 t2 =
      Int.compare t1.uid t2.uid

    let uid =
      let k = ref 0 in
      fun () -> incr k; !k

    let default_mark = ref ()

    module KMap = Map.Make(Regexp.K)

    let make clause =
      let nfa = ref KMap.empty in
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
          let accept = !accept in
          let t = {uid; k; transitions; accept; clause; mark=default_mark} in
          nfa := KMap.add k t !nfa;
          t
      and accepting = lazy (aux K.Done)
      in
      aux

    let captures = Vector.make Clause.n IndexSet.empty

    let process_clause ~capture (i : Clause.t) =
      let captures', re =
        Transl.transl ~capture ~for_reduction:false
          (Vector.get Clause.vector i).pattern.expr
      in
      Vector.set captures i captures';
      make i Regexp.K.(More (re, Done))

    let process_action ~capture (_source, clauses) =
      let capture_tbl = Hashtbl.create 7 in
      let capture_def = ref IndexMap.empty in
      let capture kind name =
        let key = (kind, name) in
        match Hashtbl.find_opt capture_tbl key with
        | Some index -> index
        | None ->
          let index = capture () in
          Hashtbl.add capture_tbl key index;
          capture_def := IndexMap.add index key !capture_def;
          index
      in
      let clauses = List.map (process_clause ~capture) (IndexSet.elements clauses) in
      !capture_def, clauses

    let actions =
      let capture = Capture.gensym () in
      Vector.map (process_action ~capture) Clause.actions

    let () = Stopwatch.step time "LazyNFA"

  end

  let captures clause = Vector.get LazyNFA.captures clause

  module BigDFA : sig
    include CARDINAL

    type ('src, 'tgt) mapping = ('tgt, 'src index * (Capture.set * Usage.set)) vector

    type 'n t = private {
      index: n index;
      group: ('n, LazyNFA.t) vector;
      mutable raw_transitions: (Lr1.set * 'n fwd_mapping lazy_t) list;
      mutable transitions: 'n transition list;
      accept: Clause.t option;
      mutable visited_labels: Lr1.set;
      mutable visited: Stacks.n indexset;
      mutable scheduled: Stacks.n indexset;
      mutable splits: 'n indexset;
      mutable new_splits: 'n indexset;
      mutable chain: ('n index * Order_chain.element) list;
    }

    and 'src fwd_mapping = Fwd_mapping : ('src, 'tgt) mapping * 'tgt t -> 'src fwd_mapping

    and 'src transition = {
      label: Lr1.set;
      mapping: 'src fwd_mapping;
      mutable pairings: (Clause.t * (Order_chain.element * Order_chain.element) list) list;
    }

    type packed = Packed : 'n t -> packed [@@unboxed]

    (*val determinize : ('a, LazyNFA.t) vector -> 'a t*)
    val initial : n index
    val states : (n, packed) vector
    val get_registers : 'm t -> ('m, Register.t Capture.map) vector
    val iter_transitions : 'n t -> ('n transition -> unit) -> unit
    val register_count : int
    val accepts : (n, (Clause.t * int) list) vector
  end =
  struct
    open IndexBuffer

    let time = Stopwatch.enter time "BigDFA"

    let group_make (type a) (prj : a -> LazyNFA.t) (ts : a list) : a array =
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

    let group_fold f x acc =
      let acc = ref acc in
      Vector.iteri (fun i x -> acc := f i x !acc) x;
      !acc

    (*let group_index_of (type n) th (t : (n, LazyNFA.t) vector) =
      let exception Found of n index in
      match
        Vector.iteri
          (fun i x -> if LazyNFA.compare x th = 0 then raise (Found i))
          t
      with
      | () -> raise Not_found
      | exception (Found n) -> n*)

    module State = Gen.Make()
    type n = State.n
    let n = State.n

    type ('src, 'tgt) mapping = ('tgt, 'src index * (Capture.set * Usage.set)) vector

    type 'n t = {
      index: n index;
      group: ('n, LazyNFA.t) vector;
      mutable raw_transitions: (Lr1.set * 'n fwd_mapping lazy_t) list;
      mutable transitions: 'n transition list;
      accept: Clause.t option;
      mutable visited_labels: Lr1.set;
      mutable visited: Stacks.n indexset;
      mutable scheduled: Stacks.n indexset;
      mutable splits: 'n indexset;
      mutable new_splits: 'n indexset;
      mutable chain: ('n index * Order_chain.element) list;
    }

    and 'src fwd_mapping = Fwd_mapping : ('src, 'tgt) mapping * 'tgt t -> 'src fwd_mapping

    and 'src transition = {
      label: Lr1.set;
      mapping: 'src fwd_mapping;
      mutable pairings: (Clause.t * (Order_chain.element * Order_chain.element) list) list;
    }

    type packed = Packed : 'n t -> packed [@@ocaml.unboxed]

    let states = State.get_generator ()

    module GroupMap = Map.Make(struct
        type t = LazyNFA.t array
        let compare g1 g2 = array_compare LazyNFA.compare g1 g2
      end)

    let determinize group =
      let map = ref GroupMap.empty in
      let rec aux : type n . (n, LazyNFA.t) vector -> n t =
        fun group ->
          match GroupMap.find_opt (Vector.as_array group) !map with
          | Some (Packed t') ->
            let Refl = assert_equal_length group t'.group in
            t'
          | None ->
            let accept = ref None in
            let rev_transitions =
              let make i ({K. filter; captures; usage}, t) =
                (filter, (i, (captures, usage), t))
              in
              group_fold
                (fun i (nfa : LazyNFA.t) acc ->
                   if nfa.accept && (
                       let desc = Vector.get Clause.vector nfa.clause in
                       not desc.partial && not desc.has_lookaheads
                     ) then
                     accept := Some nfa.clause;
                   List.rev_map (make i) nfa.transitions @ acc)
                group []
            in
            let process_class label rev_targets =
              label, lazy (
                let prepare_target (index, captures, lazy nfa) =
                  nfa, (index, captures)
                in
                let Packed result =
                  rev_targets
                  |> List.rev_map prepare_target
                  |> group_make fst
                  |> Vector.of_array
                in
                Fwd_mapping ((Vector.map snd result), aux (Vector.map fst result))
              )
            in
            let raw_transitions = ref [] in
            IndexRefine.iter_merged_decomposition rev_transitions
              (fun label targets -> push raw_transitions (process_class label targets));
            let raw_transitions = !raw_transitions in
            let reservation = Gen.reserve states in
            let state = {
              index = Gen.index reservation;
              accept = !accept;
              group; raw_transitions;
              transitions    = [];
              visited_labels = IndexSet.empty;
              scheduled      = IndexSet.empty;
              visited        = IndexSet.empty;
              splits         = IndexSet.empty;
              new_splits     = IndexSet.empty;
              chain          = [];
            } in
            Gen.commit states reservation (Packed state);
            map := GroupMap.add (Vector.as_array group) (Packed state) !map;
            state
      in
      aux group

    let initial =
      let Vector.Packed group = Vector.of_array (
          LazyNFA.actions
          |> Vector.to_list
          |> List.concat_map snd
          |> group_make Fun.id
        )
      in
      (determinize group).index

    let () = Stopwatch.step time "Processed initial states"

    let () =
      let accepting = Vector.make Clause.n [] in
      let todo = ref [] in
      let min_clause t = (Vector.as_array t.group).(0).clause in
      let schedule bound i set =
        let Packed t as packed = Gen.get states i in
        if min_clause t <= bound then (
          let set = IndexSet.diff set t.visited in
          if not (IndexSet.is_empty set) then (
            if IndexSet.is_empty t.scheduled then (
              begin match t.accept with
                | Some c when c < bound ->
                  Vector.set_cons accepting c packed
                | Some _ | None -> push todo packed
              end;
              t.scheduled <- set
            ) else (
              t.scheduled <- IndexSet.union t.scheduled set
            )
          )
        )
      in
      let update bound (Packed t) =
        let todo = t.scheduled in
        t.visited <- IndexSet.union t.visited todo;
        t.scheduled <- IndexSet.empty;
        List.iter begin fun (label, target) ->
          let really_empty = ref true in
          let expand_stack stack =
            if IndexSet.disjoint (Stacks.label stack) label
            then IndexSet.empty
            else (really_empty := false; Stacks.next stack)
          in
          let stacks = indexset_bind todo expand_stack in
          if not !really_empty then
            let lazy (Fwd_mapping (_, t')) = target in
            if not (IndexSet.is_empty stacks) then
              schedule bound t'.index stacks
        end t.raw_transitions
      in
      let rec loop bound =
        match !todo with
        | [] when Index.to_int bound > 0 ->
          let bound = Index.of_int Clause.n (Index.to_int bound - 1) in
          todo := Vector.get accepting bound;
          Vector.set accepting bound [];
          loop bound
        | [] -> ()
        | todo' ->
          todo := [];
          List.iter (update bound) todo';
          loop bound
      in
      match cardinal Clause.n with
      | 0 -> ()
      | n ->
        let bound = Index.of_int Clause.n (n - 1) in
        schedule bound initial Stacks.initials;
        loop bound

    let states = Gen.freeze states

    let () = Stopwatch.step time "Determinized DFA (%d states)" (cardinal n)

    let () =
      Vector.iter (fun (Packed t) ->
          let visited_labels = indexset_bind t.visited Stacks.label in
          t.visited_labels <- visited_labels;
          t.transitions <-
            List.filter_map (fun (label, target) ->
                if Lazy.is_val target then
                  let label = IndexSet.inter label visited_labels in
                  if not (IndexSet.is_empty label) then
                    Some {label; mapping = Lazy.force target; pairings = []}
                  else
                    None
                else
                  None
              ) t.raw_transitions;
          t.raw_transitions <- [];
        ) states

    let iter_transitions t f =
      List.iter f t.transitions

    type 'tgt rev_mapping = Rev_mapping : 'src t * ('src, 'tgt) mapping -> 'tgt rev_mapping
    type packed_rev_mapping = Rev_packed : 'n rev_mapping list -> packed_rev_mapping [@@ocaml.unboxed]

    let reverse_transitions =
      let table = Vector.make n (Rev_packed []) in
      Vector.iter (fun (Packed src) ->
          iter_transitions src (fun tr ->
              let Fwd_mapping (mapping, tgt) = tr.mapping in
              match Vector.get table tgt.index with
              | Rev_packed [] ->
                Vector.set table tgt.index (Rev_packed [Rev_mapping (src, mapping)])
              | Rev_packed (Rev_mapping (_, mapping0) :: _ as xs) ->
                let Refl = assert_equal_length mapping mapping0 in
                Vector.set table tgt.index (Rev_packed (Rev_mapping (src, mapping) :: xs))
            )
        ) states;
      table

    let iter_reverse_transitions (type n) (t : n t) (f : n rev_mapping -> unit) =
      match Vector.get reverse_transitions t.index with
      | Rev_packed [] -> ()
      | Rev_packed (Rev_mapping (_, mapping0) :: _ as xs) ->
        let Refl = assert_equal_length mapping0 t.group in
        List.iter f xs

    let reachable =
      Vector.map (fun (Packed tgt) ->
          (IndexSet.init_from_set
             (Vector.length tgt.group)
             (fun i -> (Vector.get tgt.group i).accept)
           :> IntSet.t)
        ) states

    let () =
      let todo = ref [] in
      let process (Packed tgt) =
        let reach = Vector.get reachable tgt.index in
        iter_reverse_transitions tgt (fun (Rev_mapping (src, mapping)) ->
            let mapping = Vector.as_array mapping in
            let changed = ref false in
            IntSet.iter (fun i ->
                let j, _ = mapping.(i) in
                let reach' = Vector.get reachable src.index in
                let reach'' = IntSet.add (j :> int) reach' in
                if not (IntSet.equal reach' reach'') then (
                  Vector.set reachable src.index reach'';
                  changed := true;
                )
              ) reach;
            if !changed then
              push todo (Packed src)
          )
      in
      Vector.iter process states;
      let rec loop () =
        match !todo with
        | [] -> ()
        | todo' ->
          todo := [];
          List.iter process todo';
          loop ()
      in
      loop ()

    let () = Stopwatch.step time "Computed reachability"

    let () =
      let process (Packed tgt) =
        let reach = Vector.get reachable tgt.index in
        iter_reverse_transitions tgt (fun (Rev_mapping (_, mapping)) ->
            let mapping = Vector.as_array mapping in
            IntSet.iter (fun i ->
                let _, (_, usage) = mapping.(i) in
                Usage.mark_used usage
              ) reach;
          )
      in
      Vector.iter process states

    let () =
      let reachable_clauses =
        let clauses = ref IndexSet.empty in
        let Packed t = Vector.get states initial in
        IntSet.iter (fun i ->
            let i = Index.of_int (Vector.length t.group) i in
            let thread = Vector.get t.group i in
            clauses := IndexSet.add thread.clause !clauses;
          ) (Vector.get reachable initial);
        !clauses
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
      Vector.iteri (fun index (clause : Clause.desc) ->
          let expr = clause.pattern.expr in
           if IndexSet.mem index reachable_clauses then
             check expr
           else
            Printf.eprintf "Warning: clause line %d, column %d is unreachable\n"
              expr.position.line expr.position.col
        )
        Clause.vector

    let () = Stopwatch.step time "Dead-code analysis"

    let () =
      let count = ref 0 in
      let todo = ref [] in
      let init_split (Packed t) =
        t.new_splits <-
          IndexSet.init_from_set
            (Vector.length t.group)
            (fun i -> (Vector.get t.group i).accept);
        if not (IndexSet.is_empty t.new_splits) then
          push todo (Packed t)
      in
      Vector.iter init_split states;
      let schedule (type n) (t : n t) (splits : n indexset) =
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
      let rec schedule_one : type n. n t -> n indexset -> unit =
        fun (type n) (t : n t) (splits : n indexset) ->
          let splits = IndexSet.diff splits t.splits in
          if IndexSet.is_empty splits then
            ()
          else if IndexSet.is_empty t.new_splits then (
            t.new_splits <- splits;
            process (Packed t)
          ) else
            t.new_splits <- IndexSet.union t.new_splits splits
      and process (Packed src) =
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
              let nfa = Vector.get src.group x in
              let acc = map_splits mapping tgt (i + 1) xs in
              if (Vector.get src.group x').clause = nfa.clause then
                IndexSet.add (Index.of_int (Vector.length tgt.group) i) acc
              else
                acc
        and map_splits mapping tgt i = function
          | [] -> IndexSet.empty
          | x :: xs -> map_one mapping tgt i x xs
        in
        match src.transitions with
        | [] -> ()
        | [{mapping = Fwd_mapping (mapping, tgt); _}] ->
          schedule_one tgt (map_splits (Vector.as_array mapping) tgt 0 new_splits)
        | xs ->
          List.iter (fun {mapping = Fwd_mapping (mapping, tgt); _} ->
              schedule tgt (map_splits (Vector.as_array mapping) tgt 0 new_splits)
            ) xs
      in
      let rec loop () =
        match !todo with
        | [] -> ()
        | todo' ->
          todo := [];
          List.iter process todo';
          loop ()
      in
      loop ();
      Stopwatch.step time "Compute priority splits (%d refinements)" !count

    let chain = Order_chain.make ()

    let group_by_clause t = function
      | [] -> []
      | (i, _) as x :: xs ->
        let rec loop clause acc accs = function
          | [] -> List.rev ((clause, List.rev acc) :: accs)
          | (i, _) as x :: xs ->
            let clause' = (Vector.get t.group i).clause in
            if clause = clause' then
              loop clause (x :: acc) accs xs
            else
              loop clause' [x] ((clause, List.rev acc) :: accs) xs
        in
        let clause = (Vector.get t.group i).clause in
        loop clause [x] [] xs

    let () =
      let chain_processed = Vector.make n false in
      let root = Order_chain.root chain in
      let Packed initial = Vector.get states initial in
      initial.chain <- (
        match cardinal Clause.n with
        | 0 -> []
        | _ ->
          let rec fresh_chain t clause element = function
            | [] -> []
            | m :: ms ->
              let clause' = (Vector.get t.group m).clause in
              let element =
                if clause = clause'
                then Order_chain.next element
                else root
              in
              (m, element) :: fresh_chain t clause' element ms
          in
          fresh_chain initial (Index.of_int Clause.n 0) root (IndexSet.elements initial.splits);
      );
      Vector.set chain_processed initial.index true;
      let direct_transitions = ref 0 in
      let shared_transitions = ref 0 in
      let trivial_pairing = ref 0 in
      let nontrivial_pairing = ref 0 in
      let transitions_with_pairing = ref 0 in
      let process_direct_transition src mapping tgt =
        assert (not (Vector.get chain_processed tgt.index));
        incr direct_transitions;
        let rec extract_clause clause acc = function
          | (n, _) as x :: xs when (Vector.get src.group n).clause = clause ->
            extract_clause clause (x :: acc) xs
          | rest -> List.rev acc, rest
        in
        let rec seek_clause clause = function
          | [] -> [], []
          | ((n, _) as x :: xs) as xxs ->
            let clause' = (Vector.get src.group n).clause in
            if clause' < clause then
              seek_clause clause xs
            else if clause = clause' then
              extract_clause clause [x] xs
            else
              ([], xxs)
        in
        let rec chain_next_split i element = function
          | (i', element') :: rest ->
            if i' < i  then
              chain_next_split i element' rest
            else if i' = i then
              (element', rest)
            else
              (Order_chain.extend element, rest)
          | [] -> (Order_chain.next element, [])
        in
        let rec process_splits chain = function
          | [] -> []
          | m :: ms ->
            let clause = (Vector.get tgt.group m).clause in
            let chain, rest = seek_clause clause chain in
            process_clause clause chain rest m ms
        and process_clause clause chain rest m ms =
          let i, _ = (Vector.get mapping m) in
          let split, chain = chain_next_split i root chain in
          (m, split) :: process_continue_clause clause chain rest ms
        and process_continue_clause clause chain rest = function
          | m :: ms when (Vector.get tgt.group m).clause = clause ->
            process_clause clause chain rest m ms
          | ms -> process_splits rest ms
        in
        tgt.chain <- process_splits src.chain (IndexSet.elements tgt.splits);
        Vector.set chain_processed tgt.index true;
      in
      let process_shared_transition src mapping tgt =
        incr shared_transitions;
        assert (Vector.get chain_processed src.index);
        assert (Vector.get chain_processed tgt.index);
        let src_chain = group_by_clause src src.chain in
        let tgt_chain = group_by_clause tgt tgt.chain in
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
              find_element (fst (Vector.get mapping i)) root src_elements
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
        assert (Vector.get chain_processed src.index);
        List.fold_left (fun acc ({label; mapping = Fwd_mapping (mapping, tgt); _} as tr) ->
            if not (Vector.get chain_processed tgt.index) then (
              process_direct_transition src mapping tgt;
              (label, Packed tgt) :: acc
            ) else (
              begin match process_shared_transition src mapping tgt with
                | [] -> ()
                | pairings ->
                  incr transitions_with_pairing;
                  tr.pairings <- pairings
              end;
              acc
            )
        ) acc src.transitions
      in
      let rec loop = function
        | [] -> ()
        | xs ->
          loop (List.fold_left visit []
                  (List.sort (fun (l1, _) (l2, _) -> IndexSet.compare l1 l2) xs))
      in
      loop (visit [] ((), Packed initial));
      Stopwatch.step time
        "Constructed order chain with %d elements (%d direct transitions, %d shared, %d trivial pairings, %d non-trivial pairings, %d transitions with pairings)"
        (Order_chain.freeze chain)
        !direct_transitions
        !shared_transitions
        !trivial_pairing
        !nontrivial_pairing
        !transitions_with_pairing;
      if false then (
        let count = ref 0 in
        Vector.iter (fun (Packed t) ->
            iter_transitions t (fun tr ->
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
          ) states
      )

    let accepts =
      Vector.map (fun (Packed t) ->
          let remainder = ref t.chain in
          let get_element i =
            let clause = (Vector.get t.group i).clause in
            let rec loop element = function
              | (i', element') :: rest
                when i' <= i && (Vector.get t.group i').clause = clause ->
                loop element' rest
              | rest ->
                remainder := rest;
                element
            in
            loop (Order_chain.root chain) !remainder
          in
          let acc = ref [] in
          Vector.iteri begin fun i (nfa : LazyNFA.t) ->
            if nfa.accept then
              push acc (nfa.clause, Order_chain.evaluate (get_element i))
          end t.group;
          List.rev !acc
        ) states

    let liveness =
      let reserve (Packed t) =
        Vector.Packed (Vector.make (Vector.length t.group) IndexSet.empty) in
      Vector.map reserve states

    let liveness (type m) (t : m t) : (m, Capture.set) vector =
      let Vector.Packed v = Vector.get liveness t.index in
      let Refl = assert_equal_length v t.group in
      v

    let () =
      let todo = ref [] in
      let process (Packed src) =
        let live_src = liveness src in
        let process_transition {mapping = Fwd_mapping (mapping, tgt); _} =
          let changed = ref false in
          let live_tgt = liveness tgt in
          (*let reachable = Vector.get reachable tgt.index in*)
          let process_mapping tgt_j (src_i, (captures, _usage)) =
            (*if IntSet.mem (tgt_j : _ index :> int) reachable then*)
              let live = IndexSet.union (Vector.get live_src src_i) captures in
              let live' = Vector.get live_tgt tgt_j in
              if not (IndexSet.equal live live') then (
                Vector.set live_tgt tgt_j live;
                changed := true;
              )
          in
          Vector.iteri process_mapping mapping;
          if !changed then
            push todo (Packed tgt)
        in
        iter_transitions src process_transition
      in
      Vector.iter process states;
      let rec loop () =
        match !todo with
        | [] -> ()
        | todo' ->
          todo := [];
          List.iter process todo';
          loop ()
      in
      loop ()

    let () = Stopwatch.step time "Computed liveness"

    let empty_registers = Vector.Packed Vector.empty

    let registers : (n, Register.t Capture.map Vector.packed) vector =
      Vector.make (Vector.length states) empty_registers

    let get_registers (type m) (st : m t) : (m, _) vector =
      let Vector.Packed regs = Vector.get registers st.index in
      let Refl = assert_equal_length regs st.group in
      regs

    (* Naive allocator *)
    let () =
      let init (Packed state) =
        let in_use = ref IntSet.empty in
        let alloc_reg _ = Register.of_int (IntSet.allocate in_use) in
        let alloc caps = IndexMap.inflate alloc_reg caps in
        Vector.set registers state.index
          (Vector.Packed (Vector.map alloc (liveness state)))
      in
      Vector.iter init states

    let register_count =
      let max_live = ref 0 in
      let max_index = ref (-1) in
      let check_state (Packed state) =
        let regs = get_registers state in
        let max_live' =
          Vector.fold_left (fun sum map -> sum + IndexMap.cardinal map) 0 regs
        in
        max_live := max !max_live max_live';
        Vector.iter (IndexMap.iter (fun _ reg ->
            max_index := max !max_index (Index.to_int reg))) regs;
      in
      Vector.iter check_state states;
      Stopwatch.step time
        "register allocation, max live registers: %d, register count: %d"
        !max_live (!max_index + 1);
      !max_index + 1

    let () = Stopwatch.leave time
  end

  (* Labels of DFA transitions *)
  module Label = struct
    type t = {
      filter: Lr1.set;
      captures: (Capture.t * Register.t) list;
      clear: Register.set;
      moves: Register.t Register.map;
      priority: (Clause.t * int * int) list;
    }

    let compare t1 t2 =
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

    let register_count = BigDFA.register_count
  end

  module RunDFA = struct
    type states = BigDFA.n
    let states = BigDFA.n

    let partial_captures = ref IndexSet.empty

    module Transition = struct
      open IndexBuffer

      type t = {
        source: states index;
        target: states index;
        label: Label.t;
      }
      include Gen.Make()

      let all = get_generator ()

      let () =
        let process_state (BigDFA.Packed source) =
          let src_regs = BigDFA.get_registers source in
          BigDFA.iter_transitions source @@
          fun {BigDFA. label=filter; mapping=Fwd_mapping (mapping, target); pairings} ->
          let tgt_regs = BigDFA.get_registers target in
          let captures = ref [] in
          let moves = ref IndexMap.empty in
          let clear = ref IndexSet.empty in
          let process_mapping (src_i, (captured, _usage)) tgt_bank =
            let src_bank = Vector.get src_regs src_i in
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
          let label = {Label.filter; captures; moves; clear; priority} in
          ignore (Gen.add all {source = source.index; target = target.index; label})
        in
        Vector.iter process_state BigDFA.states

      let all = Gen.freeze all
    end

    type transitions = Transition.n
    let transitions = Transition.n

    let partial_captures =
      let acc = !partial_captures in
      Vector.fold_left begin fun acc (BigDFA.Packed st) ->
        Vector.fold_left2 begin fun acc (nfa : LazyNFA.t) regs ->
          if nfa.accept then
            let cap = Vector.get LazyNFA.captures nfa.clause in
            IndexSet.fold begin fun var acc ->
              if IndexMap.mem var regs
              then acc
              else IndexSet.add var acc
            end cap acc
          else acc
        end acc st.group (BigDFA.get_registers st)
      end acc BigDFA.states

    type [@ocaml.warning "-34"] label = Label.t
    let label i = (Vector.get Transition.all i).label
    let source i = (Vector.get Transition.all i).source
    let target i = (Vector.get Transition.all i).target

    let initials f = f BigDFA.initial
    let finals f =
      Vector.iteri (fun index accepts ->
          match accepts with
          | [] -> ()
          | _ :: _ -> f index
        ) BigDFA.accepts

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
        ) BigDFA.accepts;
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
      let actions = List.sort (fun (l1, _) (l2, _) -> Label.compare l1 l2) !actions in
      let rec group_actions l ks = function
        | (l', k) :: rest when Label.compare l l' = 0 ->
          group_actions l (k :: ks) rest
        | rest ->
          refine (fun ~add -> List.iter add ks);
          start rest
      and start = function
        | [] -> ()
        | (l, k) :: rest -> group_actions l [k] rest
      in
      start actions

    let () = Stopwatch.step time "RunDFA"
  end

  (*module MinDFA = Valmari.Minimize (Label) (RunDFA)*)
  module MinDFA = Valmari.Minimize_with_custom_decomposition(RunDFA)

  let () = Stopwatch.step time "MinDFA"

  module type OUTDFA = sig
    type states
    val states : states cardinal
    val initial : states index

    type transitions
    val transitions : transitions cardinal

    val source : transitions index -> states index
    val label : transitions index -> Label.t
    val target : transitions index -> states index

    (* Labels which are reachable (there exists failing configurations (stack,
       lookahead) that reach these state) but for which the state defines no
       transition. They should be rejected at runtime. *)
    val unhandled : states index -> Lr1.set

    val outgoing : states index -> transitions indexset
    val matching : states index -> (Clause.t * priority * Register.t Capture.map) list
    val threads : states index -> (bool * Clause.t * Register.t Capture.map) list
  end

  module MakeOutDFA() = struct
    include MinDFA
    let initial = initials.(0)

    (*include RunDFA
    let initial = BigDFA.initial
    let transport_state state = Some state
    let represent_state state = state*)

    let outgoing = Vector.make states IndexSet.empty
    let unhandled = Vector.make states IndexSet.empty

    let () =
      (* Initialize with all reachable labels *)
      Vector.iter begin fun (BigDFA.Packed source) ->
        match transport_state source.index with
        | None -> ()
        | Some index ->
          unhandled.@(index) <- IndexSet.union source.visited_labels
      end BigDFA.states;
      (* Remove the ones for which transitions exist *)
      Index.rev_iter transitions begin fun tr ->
        let index = source tr in
        let label = label tr in
        let visited = Vector.get unhandled index in
        let visited = IndexSet.diff visited label.filter in
        Vector.set unhandled index visited;
        outgoing.@(index) <- IndexSet.add tr
      end

    let outgoing = Vector.get outgoing
    let unhandled = Vector.get unhandled

    let matching state =
      let BigDFA.Packed source =
        Vector.get BigDFA.states (represent_state state)
      in
      let priorities = ref (Vector.get BigDFA.accepts source.index) in
      let get_priority clause =
        match !priorities with
        | (clause', p) :: rest ->
          if clause <> clause' then (
            Printf.eprintf "Accepting clause %d but got priority for clause %d?!\n"
              (Index.to_int clause)
              (Index.to_int clause');
            assert false
          ) else if false then
            Printf.eprintf "Accepting clause %d with priority %d\n"
              (Index.to_int clause) p;
          priorities := rest;
          p
        | [] -> assert false
      in
      let add_accepting acc {LazyNFA. accept; clause; _} regs =
        if not accept then acc else
          (clause, get_priority clause, regs) :: acc
      in
      let registers = BigDFA.get_registers source in
      List.rev (Vector.fold_left2 add_accepting [] source.group registers)

    let threads state =
      let BigDFA.Packed source =
        Vector.get BigDFA.states (represent_state state)
      in
      let add_accepting {LazyNFA. accept; clause; _} regs acc =
        (accept, clause, regs) :: acc
      in
      let registers = BigDFA.get_registers source in
      Vector.fold_right2 add_accepting source.group registers []

    let () = Stopwatch.step time "OutDFA"
  end

  module OutDFA = (val (
      if Array.length MinDFA.initials = 0 then
        (module struct
          module States = Const(struct let cardinal = 1 end)
          type states = States.n
          let states = States.n
          let initial = Index.of_int states 0
          type transitions = Empty.n
          let transitions = Empty.n
          let source _ = assert false
          let label _ = assert false
          let target _ = assert false
          let unhandled _ = Lr1.wait
          let outgoing _ = IndexSet.empty
          let matching _ = []
          let threads _ = []
        end : OUTDFA)
      else
        (module MakeOutDFA() : OUTDFA)
    ))

  let captures_lr1 =
    let map = ref IndexMap.empty in
    Index.iter OutDFA.transitions (fun tr ->
        let label = OutDFA.label tr in
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
    let is_optional = IndexSet.mem index RunDFA.partial_captures in
    let none = if is_optional then "None" else "assert false" in
    let some x = if is_optional then "Some (" ^ x ^ ")" else x in
    let offset = !roffset in
    incr roffset;
    match def with
    | Value ->
      let typ = recover_type index in
      Code_printer.fmt out
        "    let %s, _startloc_%s_, _endloc_%s_ = match __registers.(%d) with \n\
        \      | Empty -> %s\n\
        \      | Initial -> assert false\n\
        \      | Value (%s.MenhirInterpreter.Element (%s, %s, startp, endp)%s) ->\n"
        name name name offset
        (if is_optional then "(None, None, None)" else "assert false")
        E.parser_name
        (if Option.is_none typ then "_" else "st")
        (if Option.is_none typ then "_" else "x")
        (if Option.is_none typ then "as x" else "");
      begin match typ with
        | None -> ()
        | Some (symbols, typ) ->
          Code_printer.fmt out
            "        let x = match %s.MenhirInterpreter.incoming_symbol st with\n"
            E.parser_name;
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
        E.parser_name (some "p")
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
        E.parser_name (some "p")

  let lookahead_constraint index =
    match Clause.lookaheads index with
    | None -> None
    | Some terms ->
      let term_pattern t =
        let name = Info.Terminal.to_string t in
        match Info.Terminal.semantic_value t with
        | None -> name
        | Some _ -> name ^ " _"
      in
      Some (string_concat_map ~wrap:("(",")")
              "|" term_pattern (IndexSet.elements terms))

  let output_code out =
    Code_printer.fmt out
      "let lrgrep_execute_%s %s\n\
      \  (__clause, (__registers : %s.MenhirInterpreter.element Lrgrep_runtime.register_values))\n\
      \  (__initialpos : Lexing.position)\n\
      \  ((token : %s.MenhirInterpreter.token), _startloc_token_, _endloc_token_)\n\
      \  : _ option = match __clause, token with\n"
      E.entry.name (String.concat " " E.entry.args)
      E.parser_name E.parser_name;
    let output_action (source, clauses) (captures, _states) =
      Code_printer.fmt out " ";
      IndexSet.iter (fun index ->
          Code_printer.fmt out
            " | %d, %s"
            (Index.to_int index)
            (Option.value (lookahead_constraint index)
               ~default:"_");
        ) clauses;
      Code_printer.fmt out " ->\n";
      IndexMap.iter (bind_capture out ~roffset:(ref 0)) captures;
      begin match source.Syntax.action with
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
          (fun clause -> Option.is_some (Clause.lookaheads clause))
          clauses
      in
      if not (IndexSet.is_empty constrained) then
        Code_printer.fmt out "  | (%s), _ -> None\n"
          (string_concat_map "|" string_of_index (IndexSet.elements constrained))
    in
    Vector.iter2 output_action Clause.actions LazyNFA.actions;
    Code_printer.print out "  | _ -> failwith \"Invalid action (internal error or API misuse)\"\n\n"

  let () = Stopwatch.leave time
end
