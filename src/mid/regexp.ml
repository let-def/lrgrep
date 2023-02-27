open Front
open Fix.Indexing
open Utils
open Misc

module Positive = Const(struct let cardinal = max_int end)

let cmon_pair f g (x, y) = Cmon.tuple [f x; g y]

let cmon_option f = function
  | None -> Cmon.constant "None"
  | Some x -> Cmon.constructor "Some" (f x)

let cmon_set_cardinal set =
  Cmon.constant ("{" ^ string_of_int (IndexSet.cardinal set) ^ " elements}")

type var = string

module type REDGRAPH = sig
  module Info : Info.S
  open Info

  type state
  val state : state cardinal

  type 'a goto_candidate = {
    target: state index;
    lookahead: Terminal.set;
    filter: 'a;
  }

  type 'a reduction_step = {
    reachable: state indexset;
    candidates: 'a goto_candidate list;
  }

  type inner_transitions = unit reduction_step list
  type outer_transitions = Lr1.set reduction_step list
  type transitions = {
    inner: inner_transitions;
    outer: outer_transitions;
  }

  type stack = Lr1.t * Lr1.t list

  val initial : (Lr1.n, transitions) vector

  val get_stack : state index -> stack
  val get_transitions : state index -> transitions

  val reachable : state index -> state indexset

  val to_string : state index -> string
end

module type RE = sig
  module Info : Info.S
  open Info
  type redstate

  (** Integers that serves has unique id to identify sub-terms.
      Thanks to properties of Antimirov's derivatives, no new term is
      introduced during derivation. All terms are produced during initial
      parsing. *)
  type uid = private int

  type quantifier_ordering = {
    kind: Syntax.quantifier_kind;
    index: int;
  }

  type reduction = {
    pattern: redstate indexset;
    capture: var option;
    ordering: quantifier_ordering;
  }

  val compare_reduction : reduction -> reduction -> int

  val cmon_reduction : reduction -> Cmon.t

  (** A regular expression term with its unique ID, its description and its
      position. *)
  type t = {
    uid : uid;
    desc : desc;
    position : Syntax.position;
  }

  (** The different constructors of regular expressions*)
  and desc =
    | Set of Lr1.set * var option
    (** Recognise a set of states, and optionally bind the matching state to
        a variable. *)
    | Alt of t list
    (** [Alt ts] is the disjunction of sub-terms [ts] (length >= 2).
        [Alt []] represents the empty language. *)
    | Seq of t list
    (** [Seq ts] is the concatenation of sub-terms [ts] (length >= 2).
        [Seq []] represents the {ε}. *)
    | Star of t * quantifier_ordering
    (** [Star t] is represents the Kleene star of [t] *)
    | Filter of Lr1.set
    | Reduce of reduction
    (** The reduction operator *)

  (** Introduce a new term, allocating a unique ID *)
  val make : Syntax.position -> desc -> t

  (** Compare two terms *)
  val compare : t -> t -> int

  (** Print a term to a [Cmon] document. [var] arguments allow to customize
      printing of variables. *)
  val cmon : t -> Cmon.t
end

module type S = sig
  module Info : Info.S
  open Info

  module Redgraph : REDGRAPH
    with module Info := Info

  module RE : RE
    with module Info := Info
     and type redstate := Redgraph.state

  module K : sig
    type label = {
      filter: Lr1.set;
      vars: var list;
      ordering: RE.quantifier_ordering list
    }

    type 'a t =
      | Done of 'a
      | More of RE.t * 'a t
      | Reducing of {
          reduction: RE.reduction;
          lookahead: Terminal.set;
          transitions: Redgraph.outer_transitions;
          next: 'a t;
        }
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val cmon : ?accept:('a -> Cmon.t) -> 'a t -> Cmon.t
    val derive :
      accept:(label -> 'a -> unit) ->
      direct:(label -> 'a t -> unit) ->
      'a t -> unit
  end
end

module Make (Info : Info.S)() : S with module Info = Info =
struct
  module Info = Info
  open Info

  module Redgraph : REDGRAPH with module Info := Info =
  struct
    include Gensym()
    type state = n
    let state = n

    type stack = Lr1.t * Lr1.t list

    type 'a goto_candidate = {
      target: state index;
      lookahead: Terminal.set;
      filter: 'a;
    }

    type 'a reduction_step = {
      reachable: state indexset;
      candidates: 'a goto_candidate list;
    }

    type inner_transitions = unit reduction_step list
    type outer_transitions = Lr1.set reduction_step list
    type transitions = {
      inner: inner_transitions;
      outer: outer_transitions;
    }

    let rec group_reductions depth
      : (Production.t * Terminal.set) list -> (Nonterminal.t * Terminal.set) list list =
      function
      | [] -> []
      | (prod, la) :: rest when depth = Production.length prod ->
        let lhs = Production.lhs prod in
        begin match group_reductions depth rest with
          | ((lhs', la') :: other) :: tl when lhs = lhs' ->
            ((lhs, IndexSet.union la la') :: other) :: tl
          | [] -> [[lhs, la]]
          | other :: tl ->
            ((lhs, la) :: other) :: tl
        end
      | otherwise ->
        [] :: group_reductions (depth + 1) otherwise

    let states =
      IndexBuffer.make ((Index.of_int Lr1.n 0, []), ([], []))

    let nodes = Hashtbl.create 7

    let rec visit_stack stack =
      match Hashtbl.find_opt nodes stack with
      | Some state -> state
      | None ->
        let state = fresh () in
        Hashtbl.add nodes stack state;
        IndexBuffer.set states state
          (stack, visit_transitions stack);
        state

    and visit_transitions (top, _ as stack) =
      Lr1.reductions top
      |> group_reductions 0
      |> visit_inner stack

    and visit_inner (top, rest) = function
      | [] -> ([], [])
      | gotos :: next ->
        let (inner, outer) = match rest with
          | top' :: rest' -> visit_inner (top', rest') next
          | [] -> ([], visit_outer (IndexSet.singleton top) next)
        in
        let process_goto (lhs, lookahead) =
          let target_lhs = Transition.find_goto_target top lhs in
          let target = visit_stack (target_lhs, top :: rest) in
          {target; lookahead; filter=()}
        in
        (List.map process_goto gotos :: inner, outer)

    and visit_outer successors = function
      | [] -> []
      | gotos :: next ->
        let lr1_states = Lr1.set_predecessors successors in
        let next = visit_outer lr1_states next in
        let process_goto acc (lhs, lookahead) =
          let process_target source acc =
            let target_lhs = Transition.find_goto_target source lhs in
            IndexMap.update target_lhs (function
                | Some (sources, target) ->
                  Some (IndexSet.add source sources, target)
                | None ->
                  Some (IndexSet.singleton source, visit_stack (target_lhs, []))
              ) acc
          in
          let by_target = IndexSet.fold process_target lr1_states IndexMap.empty in
          let add_target _ (filter, target) acc = {filter; target; lookahead} :: acc in
          IndexMap.fold add_target by_target acc
        in
        let gotos = List.fold_left process_goto [] gotos in
        gotos :: next

    let initial =
      Vector.init Lr1.n (fun lr1 -> visit_transitions (lr1, []))

    let states_def = IndexBuffer.contents states state

    let reachable =
      let reachable =
        states_def |> Vector.mapi begin fun self (_stack, (inner, outer)) ->
          let add_target acc step = IndexSet.add step.target acc in
          let acc = IndexSet.singleton self in
          let acc = List.fold_left (List.fold_left add_target) acc inner in
          let acc = List.fold_left (List.fold_left add_target) acc outer in
          acc
        end
      in
      let preds = Vector.make state IndexSet.empty in
      Vector.iteri (fun source reachable ->
          IndexSet.iter (fun target ->
              if target <> source then
                vector_set_add preds target source
            ) reachable
        ) reachable;
      (*begin
        let oc = open_out "reach.dot" in
        let fp fmt = Printf.fprintf oc fmt in
        fp "digraph G {\n";
        fp "  rankdir=\"LR\";\n";
        fp "  node[fontname=%S nojustify=true shape=box];\n" "Monospace";
        fp "  edge[fontname=%S nojustify=true shape=box];\n" "Monospace";
        let todo = Vector.make state true in
        let rec visit st =
          if Vector.get todo st then (
            Vector.set todo st false;
            let (top, stack), _ = Vector.get states_def st in
            fp "  st_%d[label=%S];\n"
              (Index.to_int st)
              (string_of_index st ^ ": " ^
               Lr1.list_to_string (List.rev (top :: stack)));
            IndexSet.iter (fun st' ->
                fp "  st_%d -> st_%d\n"
                  (Index.to_int st) (Index.to_int st') ;
                visit st'
              ) (Vector.get preds st)
          )
        in
        visit (Index.of_int state 668);
        visit (Index.of_int state 704);
        fp "}\n";
        close_out oc;
        end;*)
      let todo = ref [] in
      let update reach' state =
        let reach = Vector.get reachable state in
        if not (IndexSet.subset reach' reach) then (
          Vector.set reachable state (IndexSet.union reach reach');
          push todo state
        )
      in
      let process state =
        let reach = Vector.get reachable state in
        IndexSet.iter (update reach) (Vector.get preds state)
      in
      Index.iter state process;
      let rec loop () =
        match !todo with
        | [] -> ()
        | some ->
          todo := [];
          List.iter process some;
          loop ()
      in
      loop ();
      reachable

    (*let () =
      if false then
        Vector.iteri (fun st reachable ->
            Printf.eprintf "#%d: %d reachable\n"
              (Index.to_int st) (IndexSet.cardinal reachable)
          ) reachable*)

    let reachable = Vector.get reachable

    let make_reduction_step (inner, outer) =
      let rec process_steps = function
        | [] -> []
        | step :: steps ->
          let steps = process_steps steps in
          let acc = match steps with
            | [] -> IndexSet.empty
            | x :: _ -> x.reachable
          in
          let reachable =
            List.fold_left (fun acc candidate ->
                IndexSet.union acc (reachable candidate.target))
              acc step
          in
          {reachable; candidates=step} :: steps
      in
      {inner = process_steps inner; outer = process_steps outer}

    let states_def =
      Vector.map
        (fun (stack, steps) -> (stack, make_reduction_step steps))
        states_def

    let initial = Vector.map make_reduction_step initial

    let get_def = Vector.get states_def
    let get_stack st = fst (get_def st)
    let get_transitions st = snd (get_def st)

    let to_string state =
      let top, rest = get_stack state in
      let states = List.rev (top :: rest) in
      string_concat_map " " Lr1.to_string states
  end

  (* [RE]: Syntax for regular expression extended with reduction operator *)
  module RE : RE with module Info := Info
                  and type redstate := Redgraph.state =
  struct
    type uid = int

    let uid =
      let k = ref 0 in
      fun () -> incr k; !k

    type quantifier_ordering = {
      kind: Syntax.quantifier_kind;
      index: int;
    }

    type reduction = {
      pattern: Redgraph.state indexset;
      capture: string option;
      ordering: quantifier_ordering;
    }

    let compare_reduction r1 r2 =
      if r1 == r2 then 0 else
        let c = IndexSet.compare r1.pattern r2.pattern in
        if c <> 0 then c else
          let c = Option.compare String.compare r1.capture r2.capture in
          if c <> 0 then c else
            compare r1.ordering r2.ordering

    type t = {
      uid : uid;
      desc : desc;
      position : Syntax.position;
    }
    and desc =
      | Set of Lr1.set * string option
      | Alt of t list
      | Seq of t list
      | Star of t * quantifier_ordering
      | Filter of Lr1.set
      | Reduce of reduction

    let make position desc = {uid = uid (); desc; position}

    let compare t1 t2 =
      Int.compare t1.uid t2.uid

    let cmon_quantifier_ordering {kind; index} =
      Cmon.record [
        "kind",  Syntax.cmon_quantifier_kind kind;
        "index", Cmon.int index;
      ]

    let cmon_reduction {capture; pattern; ordering} =
      Cmon.record [
        "capture", cmon_option Cmon.string capture;
        "pattern", cmon_set_cardinal (*cmon_indexset*) pattern;
        "ordering", cmon_quantifier_ordering ordering;
      ]

    let cmon t =
      let rec aux t =
        match t.desc with
        | Set (lr1s, var) ->
          Cmon.construct "Set" [
            cmon_set_cardinal lr1s;
            cmon_option Cmon.string var;
          ]
        | Alt ts -> Cmon.constructor "Alt" (Cmon.list_map aux ts)
        | Seq ts -> Cmon.constructor "Seq" (Cmon.list_map aux ts)
        | Star (t, o) -> Cmon.construct "Star" [aux t; cmon_quantifier_ordering o]
        | Filter lr1s ->
          Cmon.constructor "Filter" (cmon_set_cardinal lr1s)
        | Reduce r ->
          Cmon.constructor "Reduce" (cmon_reduction r)
      in
      aux t
  end

  module K = struct
    type label = {
      filter: Lr1.set;
      vars: var list;
      ordering: RE.quantifier_ordering list
    }

    type 'a t =
      | Done of 'a
      | More of RE.t * 'a t
      | Reducing of {
          reduction: RE.reduction;
          lookahead: Terminal.set;
          transitions: Redgraph.outer_transitions;
          next: 'a t;
        }

    let rec list_compare f xxs yys =
      if xxs == yys then 0 else
        match xxs, yys with
        | [], _  -> -1
        | _ , [] -> +1
        | (x :: xs), (y :: ys) ->
          let c = f x y in
          if c <> 0 then c else
            list_compare f xs ys

    let compare_outer_candidate c1 c2 =
      let c = compare_index c1.Redgraph.target c2.Redgraph.target in
      if c <> 0 then c else
        let c = IndexSet.compare c1.filter c2.filter in
        if c <> 0 then c else
          IndexSet.compare c1.lookahead c2.lookahead

    let compare_reduction_step r1 r2 =
      let c = IndexSet.compare r1.Redgraph.reachable r2.Redgraph.reachable in
      if c <> 0 then c else
        list_compare compare_outer_candidate r1.candidates r2.candidates

    let rec compare cmp_a t1 t2 =
      if t1 == t2 then 0 else
        match t1, t2 with
        | Done a1, Done a2 -> cmp_a a1 a2
        | More (e1, t1'), More (e2, t2') ->
          let c = RE.compare e1 e2 in
          if c <> 0 then c else
            compare cmp_a t1' t2'
        | Reducing r1, Reducing r2 ->
          let c = RE.compare_reduction r1.reduction r2.reduction in
          if c <> 0 then c else
            let c = list_compare compare_reduction_step r1.transitions r2.transitions in
            if c <> 0 then c else
              let c = IndexSet.compare r1.lookahead r2.lookahead in
              if c <> 0 then c else
                compare cmp_a r1.next r2.next
        | Done _, (More _ | Reducing _) -> -1
        | (More _ | Reducing _), Done _ -> +1
        | More _, Reducing _ -> -1
        | Reducing _, More _ -> +1

    let cmon_candidate ?lookahead:lookahead' ~filter:cmon_filter {Redgraph. target; lookahead; filter} =
      Cmon.record [
        "target"  , cmon_index target;
        "lookahead" , (
          match lookahead' with
          | None -> cmon_set_cardinal lookahead
          | Some lookahead' ->
            Cmon.constant (
              Printf.sprintf "{%d elements} (%d matching current reduction)"
                (IndexSet.cardinal lookahead)
                (IndexSet.cardinal (IndexSet.inter lookahead lookahead'))
            )
        );
        "filter"     , cmon_filter filter;
      ]

    let cmon_outer_candidate ?lookahead c =
      cmon_candidate ?lookahead c
        ~filter:(fun filter ->
            if IndexSet.cardinal filter >= 10 then cmon_set_cardinal filter else (
              (*Cmon.record [
                "filter",*)
              cmon_indexset filter
                ~index:(fun lr1 -> Cmon.constant (Lr1.to_string lr1));
              (*"predecessors",
                cmon_indexset (Lr1.set_predecessors filter)
                ~index:(fun lr1 -> Cmon.constant (Lr1.to_string lr1));
                ]*)
            )
          )

    let cmon_transitions ~candidate trs =
      trs |> Cmon.list_map @@ fun {Redgraph. reachable; candidates} ->
      Cmon.record [
        "reachable", cmon_set_cardinal reachable;
        "candidates", Cmon.list_map candidate candidates;
      ]

    (*let cmon_all_transitions {Redgraph. inner; outer} =
      Cmon.record [
        "inner", cmon_transitions ~candidate:(cmon_candidate ~filter:(fun () -> Cmon.unit)) inner;
        "outer", cmon_transitions ~candidate:cmon_outer_candidate outer;
      ]*)

    let cmon ?(accept=fun _ -> Cmon.constant "_") =
      let rec self = function
        | Done a ->
          Cmon.constructor "Done" (accept a)
        | More (re, next) ->
          Cmon.construct "More" [RE.cmon re; self next]
        | Reducing {reduction; lookahead; transitions; next} ->
          Cmon.crecord "Reducing" [
            "reduction"   , RE.cmon_reduction reduction;
            "lookahead"   , cmon_set_cardinal (*cmon_indexset*) lookahead;
            "transitions" , cmon_transitions ~candidate:cmon_outer_candidate transitions;
            "next"        , self next;
          ]
      in
      self

    let not_empty s1 =
      not (IndexSet.is_empty s1)

    let intersecting s1 s2 =
      not (IndexSet.disjoint s1 s2)

    let label_filter label filter =
      let filter = Lr1.intersect label.filter filter in
      if not_empty filter then
        Some {label with filter}
      else
        None

    let live_redstep (red : RE.reduction) (step : _ Redgraph.reduction_step) =
      intersecting red.pattern step.reachable

    let live_redstate (red : RE.reduction) (state : Redgraph.state index) =
      intersecting red.pattern (Redgraph.reachable state)

    let visited_target = ref IndexSet.empty

    let rec reduce_target ~on_outer r ~lookahead target =
      not_empty lookahead && (
        (live_redstate r target &&
         reduce_transitions ~on_outer r ~lookahead
           (Redgraph.get_transitions target))
        || IndexSet.mem target r.pattern
      )

    and reduce_transitions ~on_outer r ~lookahead {Redgraph. inner; outer} =
      assert (not_empty lookahead);
      let matched = ref false in
      let visit_candidate (c : unit Redgraph.goto_candidate) =
        if reduce_target ~on_outer r c.target
            ~lookahead:(Terminal.intersect lookahead c.lookahead)
        then matched := true
      in
      let rec loop = function
        | step :: xs when live_redstep r step ->
          List.iter visit_candidate step.candidates;
          loop xs
        | _ -> ()
      in
      loop inner;
      if outer <> [] then
        on_outer r lookahead outer;
      !matched

    let derive ~accept ~direct k =
      let rec reduce_outer matching next label reduction lookahead = function
        | step :: transitions when live_redstep reduction step ->
          let visit_candidate (candidate : Lr1.set Redgraph.goto_candidate) =
            match label_filter label candidate.filter with
            | Some label when
                reduce_target reduction candidate.target
                  ~lookahead:(Terminal.intersect lookahead candidate.lookahead)
                  ~on_outer:(reduce_outer matching next label) ->
              matching := IndexSet.union label.filter !matching
            | _ -> ()
          in
          List.iter visit_candidate step.candidates;
          begin match transitions with
            | step' :: _ when live_redstep reduction step' ->
              direct label (Reducing {reduction; transitions; lookahead; next});
            | _ -> ()
          end
        | _ -> ()
      in
      let reduce_outer next label reduction lookahead transitions =
        let matching = ref IndexSet.empty in
        reduce_outer matching next label reduction lookahead transitions;
        if not_empty !matching then
          Some {label with filter = !matching}
        else
          None
      in
      let rec process_k label k =
        match k with
        | Done a ->
          accept label a

        | More (re, next) as self ->
          process_re label self next re.desc

        | Reducing {reduction; transitions; lookahead; next} ->
          visited_target := IndexSet.empty;
          let l' = reduce_outer next label reduction lookahead transitions in
          begin match l' with
            | None -> ()
            | Some label -> process_k label next
          end

      and process_re label self next = function
        | Set (s, var) ->
          begin match label_filter label s with
            | None -> ()
            | Some label ->
              let label = match var with
                | None -> label
                | Some var -> {label with vars = var :: label.vars}
              in
              direct label next
          end

        | Alt es ->
          List.iter (fun e -> process_k label (More (e, next))) es

        | Star (r, _todo) ->
          process_k label next;
          process_k label (More (r, self))

        | Seq es ->
          process_k label (List.fold_right (fun e k -> More (e, k)) es next)

        | Filter filter ->
          begin match label_filter label filter with
            | None -> ()
            | Some label' -> process_k label' next
          end

        | Reduce r ->
          let matching = IndexSet.filter (fun lr1 ->
              let steps = Vector.get Redgraph.initial lr1 in
              let on_outer reduction lookahead = function
                | (step :: _) as transitions when live_redstep reduction step ->
                  direct {label with filter = IndexSet.singleton lr1}
                    (Reducing {reduction; lookahead; transitions; next})
                | _ -> ()
              in
              reduce_transitions ~on_outer r ~lookahead:Terminal.all steps
            ) label.filter
          in
          if not_empty matching then
            process_k {label with filter = matching} next
      in
      let label = {
        filter = Lr1.all;
        vars = [];
        ordering = [];
      } in
      process_k label k

  end
end
