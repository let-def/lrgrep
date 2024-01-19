open Utils
open Misc
open Fix.Indexing

module type GRAMMAR = MenhirSdk.Cmly_api.GRAMMAR

(** [INDEXED] represents a finite set, essentially extending
    [Fix.Indexing.CARDINAL] with convenient definitions.

    The grammar defines many fixed sets (for terminals, non-terminals,
    productions, LR states, ...). For convenience, we represent each of those
    sets using [Fix.Indexing]. *)
module type INDEXED = sig
  (** This module defines a finite set *)
  include CARDINAL

  (** An element of the set *)
  type t = n index

  (** A subset of elements *)
  type set = n indexset

  (** A partial map from elements to values of type ['a] *)
  type 'a map = (n, 'a) indexmap
end

(** [GRAMMAR_INDEXED] extends [INDEXED] with bijection with
    Menhir's representation. *)
module type GRAMMAR_INDEXED = sig
  include INDEXED

  (** The type of Menhir's representation for elements of this finite set *)
  type raw

  (** Import an element from the grammar representation *)
  val of_g : raw -> t

  (** Export an element to the grammar representation *)
  val to_g : t -> raw
end

module type S = sig
  module Grammar : GRAMMAR

  module Terminal : sig
    include GRAMMAR_INDEXED with type raw = Grammar.terminal
    val to_string : t -> string
    val all : set

    (** [semantic_value term] is [Some typ] if terminal [term] has a semantic
        value of type [typ], or [None] for unparameterized terminals. *)
    val semantic_value : t -> string option

    (** Wrapper around [IndexSet.inter] speeding-up intersection with [all] *)
    val intersect : set -> set -> set
  end

  module Nonterminal : sig
    include GRAMMAR_INDEXED with type raw = Grammar.nonterminal
    val to_string : t -> string
    val all : set
    val kind : t -> [`REGULAR | `START]
    val semantic_value : t -> string option
    val nullable : t -> bool
  end

  module Symbol : sig
    include SUM with type l := Terminal.n
                 and type r := Nonterminal.n

    type t = n index
    type set = n indexset

    type desc =
      | T of Terminal.t
      | N of Nonterminal.t

    val desc : t -> desc

    val is_terminal : n index -> bool
    val is_nonterminal : n index -> bool

    val of_g : Grammar.symbol -> t
    val to_g : t -> Grammar.symbol

    val name : ?mangled:bool -> t -> string
    val semantic_value : t -> string option

    val all : set
  end

  module Production : sig
    include GRAMMAR_INDEXED with type raw = Grammar.production
    val lhs : t -> Nonterminal.t
    val rhs : t -> Symbol.t array
    val length : t -> int
    val kind : t -> [ `REGULAR | `START ]
    val all : set
  end

  module Lr0 : sig
    include GRAMMAR_INDEXED with type raw = Grammar.lr0
    val incoming : t -> Symbol.t option
    val items : t -> (Production.t * int) list
    val entrypoint : t -> Nonterminal.t option
  end

  module Lr1 : sig
    include GRAMMAR_INDEXED with type raw = Grammar.lr1
    val all : set
    val idle : set
    val to_lr0 : t -> Lr0.t
    val incoming : t -> Symbol.t option
    val items : t -> (Production.t * int) list

    (* Printing functions, for debug purposes.
       Not nice for the end-user (FIXME). *)

    val to_string : t -> string
    val list_to_string : t list -> string
    val set_to_string : set -> string

    val symbol_to_string : t -> string

    (** [shift_on t] is the set of lookaheads that state [t] can shift *)
    val shift_on : t -> Terminal.set

    (** [reduce_on t] is the set of lookaheads that trigger a reduction in state
        [t] *)
    val reduce_on : t -> Terminal.set

    (** [reject t] is set of lookaheads that cause the automaton to fail when in
        state [t] *)
    val reject : t -> Terminal.set

    (** [predecessors t] is the set of LR(1) states that have transition going
        to [t]. *)
    val predecessors : t -> set

    (** [predecessors] but lifted to operate on a set of LR(1) states. *)
    val set_predecessors : set -> set

    (** Wrapper around [IndexSet.inter] speeding-up intersection with [all] *)
    val intersect : set -> set -> set
  end

  module Transition : sig
    (* Abstract types used as index to represent the different sets of
       transitions.
       For instance, [goto] represents the finite set of goto transition:
       - the value [goto : goto cardinal] is the cardinal of this set
       - any value of type [goto index] is a member of this set
         (representing a goto transition)
    *)
    type goto and shift and any

    (* The set of goto transitions *)
    val goto : goto cardinal
    (* The set of all transitions = goto U shift *)
    val any : any cardinal
    (* The set of shift transitions *)
    val shift : shift cardinal

    (* Building the isomorphism between any and goto U shift *)

    (* Inject goto into any *)
    val of_goto : goto index -> any index

    (* Inject shift into any *)
    val of_shift : shift index -> any index

    (* Project a transition into a goto or a shift transition *)
    val split : any index -> (goto index, shift index) either

    (* [find_goto s nt] finds the goto transition originating from [s] and
       labelled by [nt], or raise [Not_found].  *)
    val find_goto : Lr1.t -> Nonterminal.t -> goto index
    val find_goto_target : Lr1.t -> Nonterminal.t -> Lr1.t

    (* Get the source state of a transition *)
    val source : any index -> Lr1.t

    (* Get the target state of a transition *)
    val target : any index -> Lr1.t

    (* Symbol that labels a transition *)
    val symbol : any index -> Symbol.t

    (* Symbol that labels a goto transition *)
    val goto_symbol : goto index -> Nonterminal.t

    (* Symbol that labels a shift transition *)
    val shift_symbol : shift index -> Terminal.t

    (* [successors s] returns all the transitions [tr] such that
       [source tr = s] *)
    val successors : Lr1.t -> any index list

    (* [predecessors s] returns all the transitions [tr] such that
       [target tr = s] *)
    val predecessors : Lr1.t -> any index list
  end

  module Reduction : sig
    include INDEXED

    val state: t -> Lr1.t
    val production: t -> Production.t
    val lookaheads: t -> Terminal.set

    val from_lr1: Lr1.t -> set
  end
end

module Indexed(Raw : sig
    type t
    val count : int
    val to_int : t -> int
    val of_int : int -> t
  end)
  : GRAMMAR_INDEXED with type raw = Raw.t =
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

module Make(Grammar : GRAMMAR) : S with module Grammar = Grammar =
struct
  let time = Stopwatch.enter Stopwatch.main "Info.Make"

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
    let semantic_value i =
      Grammar.Nonterminal.typ (to_g i)
    let nullable i =
      Grammar.Nonterminal.nullable (to_g i)
  end

  module Symbol = struct
    include Sum(Terminal)(Nonterminal)
    type t = n index
    type set = n indexset

    type desc =
      | T of Terminal.t
      | N of Nonterminal.t

    let desc t =
      match prj t with
      | L t -> T t
      | R n -> N n

    let of_g = function
      | Grammar.T t -> inj_l (Terminal.of_g t)
      | Grammar.N n -> inj_r (Nonterminal.of_g n)

    let to_g t = match prj t with
      | L t -> Grammar.T (Terminal.to_g t)
      | R n -> Grammar.N (Nonterminal.to_g n)

    let name ?mangled t = match prj t with
      | L t -> Grammar.symbol_name ?mangled (T (Terminal.to_g t))
      | R n -> Grammar.symbol_name ?mangled (N (Nonterminal.to_g n))

    let is_terminal t = match prj t with
      | L _ -> true
      | R _ -> false

    let is_nonterminal t = match prj t with
      | L _ -> false
      | R _ -> true

    let semantic_value t = match prj t with
      | L t -> Some (Option.value (Terminal.semantic_value t) ~default:"unit")
      | R n -> Nonterminal.semantic_value n

    let all = all n
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

    let length p = Array.length (rhs p)

    let kind p = Grammar.Production.kind (to_g p)

    let all = all n
  end

  module Lr0 = struct
    include Indexed(Grammar.Lr0)

    let incoming lr0 =
      Option.map Symbol.of_g (Grammar.Lr0.incoming (to_g lr0))

    let items lr0 =
      List.map
        (fun (p,pos) -> (Production.of_g p, pos))
        (Grammar.Lr0.items (to_g lr0))

    let entrypoint lr0 =
      match incoming lr0 with
      | Some _ -> None
      | None -> (
          match items lr0 with
          | [p, 0] ->
            let p = Production.to_g p in
            assert (Grammar.Production.kind p = `START);
            Some (Nonterminal.of_g (Grammar.Production.lhs p))
          | _ -> assert false
        )
  end

  (** [Transitions] module, defined below, depends on the set of Lr1 states
      but [Lr1] module depends on [Transitions].
      So [Prelr1] is defined first and is the raw set of Lr1 states. *)
  module Prelr1 = Indexed(Grammar.Lr1)

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

    let sources = Vector.make' any (fun () -> Index.of_int Prelr1.n 0)
    let targets = Vector.make' any (fun () -> Index.of_int Prelr1.n 0)

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
    let predecessors = Vector.make Prelr1.n []

    let successors =
      (* We populate all the data structures allocated above, i.e.
         the vectors t_sources, t_symbols, t_targets, nt_sources, nt_symbols,
         nt_targets and predecessors, as well as the tables t_table and
         nt_table, by iterating over all successors. *)
      let next_goto = Index.enumerate goto in
      let next_shift = Index.enumerate shift in
      Vector.init Prelr1.n begin fun source ->
        List.fold_left begin fun acc (sym, target) ->
          let target = Prelr1.of_g target in
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
        end [] (Grammar.Lr1.transitions (Prelr1.to_g source))
      end

    let successors lr1 = Vector.get successors lr1
    let predecessors lr1 = Vector.get predecessors lr1

    let find_goto source nt = Hashtbl.find nt_table (nt_pack source nt)

    let source i = Vector.get sources i

    let symbol i =
      match split i with
      | L i -> Symbol.inj_r (Vector.get nt_symbols i)
      | R i -> Symbol.inj_l (Vector.get t_symbols i)

    let goto_symbol i = Vector.get nt_symbols i
    let shift_symbol i = Vector.get t_symbols i

    let target i = Vector.get targets i

    let find_goto_target source nt =
      target (of_goto (find_goto source nt))

    let () = Stopwatch.step time "Transition"
  end


  module Lr1 = struct
    include Prelr1
    let all = all n

    let to_lr0 lr1 = Lr0.of_g (Grammar.Lr1.lr0 (to_g lr1))

    let incoming lr1 = Lr0.incoming (to_lr0 lr1)

    let idle = IndexSet.init_from_set n (fun lr1 ->
        match Option.map Symbol.desc (incoming lr1) with
        | Some (N _) -> false
        | Some (T t) -> Grammar.Terminal.kind (Terminal.to_g t) = `REGULAR
        | None -> true
      )

    let items lr1 = Lr0.items (to_lr0 lr1)

    (** A somewhat informative string description of the Lr1 state, for debug
        purposes. *)
    let to_string lr1 =
      string_of_index lr1 ^ ":" ^
      let lr0 = to_lr0 lr1 in
      match Lr0.incoming lr0 with
      | Some sym -> Symbol.name sym
      | None ->
        let entrypoint = Option.get (Lr0.entrypoint lr0) in
        let name = Bytes.of_string (Nonterminal.to_string entrypoint) in
        Bytes.set name (Bytes.length name - 1) ':';
        Bytes.unsafe_to_string name

    let symbol_to_string lr1 =
      match incoming lr1 with
      | Some sym -> Symbol.name sym
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

    let predecessors = tabulate_finset n (fun lr1 ->
        List.fold_left
          (fun acc tr -> IndexSet.add (Transition.source tr) acc)
          IndexSet.empty
          (Transition.predecessors lr1)
      )

    let set_predecessors lr1s =
      indexset_bind lr1s predecessors

    let intersect a b =
      if a == all then b
      else if b == all then a
      else IndexSet.inter a b

    let () = Stopwatch.step time "Lr1"
  end

  (** A more convenient presentation of reductions than the one from Cmly.
      Reductions are represented as a distinct set. For a reduction, one can
      query the production, the lookaheads and the state it starts from.
      Conversely, one can go from a state to its reductions.
      Start productions are ignored. *)
  module Reduction = struct
    let n = ref 0
    let raw =
      let prepare_red (t, ps) = (t, List.hd ps) in
      let import_red reds =
        reds
        |> List.map prepare_red
        |> List.filter_map (fun (t, p) ->
            let p = Production.of_g p in
            match Production.kind p with
            | `START -> None
            | `REGULAR -> Some (p, Terminal.of_g t)
          )
        |> Misc.group_by
          ~compare:(fun (p1,_) (p2,_) -> compare_index p1 p2)
          ~group:(fun (p,t) ps -> p, IndexSet.of_list (t :: List.map snd ps))
        |> List.sort (fun (p1,_) (p2,_) ->
            let l1 = Array.length (Production.rhs p1) in
            let l2 = Array.length (Production.rhs p2) in
            let c = Int.compare l1 l2 in
            if c <> 0 then c else
              compare_index (Production.lhs p1) (Production.lhs p2)
          )
      in
      let import_lr1 lr1 =
        let reds = import_red (Grammar.Lr1.reductions (Lr1.to_g lr1)) in
        n := !n + List.length reds;
        reds
      in
      Vector.init Lr1.n import_lr1

    include Const(struct let cardinal = !n end)

    type t = n index
    type set = n indexset
    type 'a map = (n, 'a) indexmap

    let state = Vector.make' n (fun () -> Index.of_int Lr1.n 0)
    let production = Vector.make' n (fun () -> Index.of_int Production.n 0)
    let lookaheads = Vector.make n IndexSet.empty
    let from_lr1 =
      let enum = Index.enumerate n in
      Vector.mapi (fun lr1 reds ->
          List.fold_left (fun set (prod, la) ->
              let i = enum () in
              Vector.set state i lr1;
              Vector.set production i prod;
              Vector.set lookaheads i la;
              IndexSet.add i set
            ) IndexSet.empty reds
        ) raw

    let state = Vector.get state
    let production = Vector.get production
    let lookaheads = Vector.get lookaheads
    let from_lr1 = Vector.get from_lr1
  end

  let () = Stopwatch.leave time
end
