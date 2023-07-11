open Front
open Fix.Indexing
open Utils
open Misc

module Capture : sig
  type n
  type t = n index
  type set = n indexset
  type 'a map = (n, 'a) indexmap
  val gensym : unit -> unit -> n index
end = struct
  include Positive
  type t = n index
  type set = n indexset
  type 'a map = (n, 'a) indexmap
  let gensym () =
    let r = ref (-1) in
    fun () -> incr r; Index.of_int n !r
end

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

  type config = {
    top: Lr1.t;
    rest: Lr1.t list;
    lookahead: Terminal.set;
  }

  val initial : (Lr1.n, transitions) vector

  val get_config : state index -> config
  val get_stack : state index -> Lr1.t list
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

  type reduction = {
    pattern: redstate indexset;
    capture: Capture.set;
    policy: Syntax.quantifier_kind;
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
    | Set of Lr1.set * Capture.set
    (** Recognise a set of states, and optionally bind the matching state to
        a variable. *)
    | Alt of t list
    (** [Alt ts] is the disjunction of sub-terms [ts] (length >= 2).
        [Alt []] represents the empty language. *)
    | Seq of t list
    (** [Seq ts] is the concatenation of sub-terms [ts] (length >= 2).
        [Seq []] represents the {ε}. *)
    | Star of t * Syntax.quantifier_kind
    (** [Star t] is represents the Kleene star of [t] *)
    | Filter of Lr1.set
    | Reduce of Capture.set * reduction
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
      captures: Capture.set;
    }

    val is_immediate_label : label -> bool

    val compare_label : label -> label -> int

    type t =
      | Done
      | More of RE.t * t
      | Reducing of {
          reduction: RE.reduction;
          transitions: Redgraph.outer_transitions;
          next: t;
        }
    val compare : t -> t -> int
    val cmon : t -> Cmon.t

    (* TODO
       There is no way to detect immediate match (empty (sub-)regex leading
       immediately to Done).
       The case is handled by looking for [(empty_label, Done)] in the result
       list, where [empty_label] is a label capturing and filtering nothing
       (vars and ordering are empty, filter = Lr1.all).

       However, this does not allow to distinguish between the regular
       expressions ϵ and _ which will both lead to "Done" while matching no
       transitions.

       TODO not sure.
       To simplify the NFA a bit, we could normalize by splitting disjunctions
       before derivation.
    *)
    val derive : t -> (label * t option) list
  end
end

module Make (Info : Info.S)() : S with module Info = Info =
struct
  module Info = Info
  open Info

  module Redgraph : REDGRAPH with module Info := Info =
  struct
    let time = Stopwatch.enter Stopwatch.main "Regexp.Make.Redgraph"

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

    open IndexBuffer

    module State = Gen.Make()
    type state = State.n
    let state = State.n

    type config = {
      top: Lr1.t;
      rest: Lr1.t list;
      lookahead: Terminal.set;
    }

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

    let states = State.get_generator ()

    let nodes = Hashtbl.create 7

    let reductions =
      Vector.init Lr1.n (fun lr1 -> lr1 |> Lr1.reductions |> group_reductions 0)

    let rec visit_config config =
      match Hashtbl.find_opt nodes config with
      | Some state -> state
      | None ->
        let reservation = Gen.reserve states in
        let index = Gen.index reservation in
        Hashtbl.add nodes config index;
        Gen.commit states reservation (config, visit_transitions config);
        index

    and visit_transitions config =
      visit_inner config (Vector.get reductions config.top)

    and visit_inner config = function
      | [] -> ([], [])
      | gotos :: next ->
        let (inner, outer) = match config.rest with
          | top :: rest ->
            visit_inner {config with top; rest} next
          | [] ->
            ([], visit_outer config.lookahead (IndexSet.singleton config.top) next)
        in
        let process_goto (lhs, lookahead) =
          let target_lhs = Transition.find_goto_target config.top lhs in
          let lookahead = Terminal.intersect lookahead config.lookahead in
          if IndexSet.is_empty lookahead then
            None
          else
            let target =
              visit_config {
                top = target_lhs;
                rest = config.top :: config.rest;
                lookahead;
              }
            in
            Some {target; lookahead; filter=()}
        in
        (List.filter_map process_goto gotos :: inner, outer)

    and visit_outer lookahead successors = function
      | [] -> []
      | gotos :: next ->
        let lr1_states = Lr1.set_predecessors successors in
        let next = visit_outer lookahead lr1_states next in
        let process_goto acc (lhs, lookahead) =
          let process_target source acc =
            let target_lhs = Transition.find_goto_target source lhs in
            IndexMap.update target_lhs (function
                | Some (sources, target) ->
                  Some (IndexSet.add source sources, target)
                | None ->
                  let config = {
                    top = target_lhs;
                    rest = [];
                    lookahead;
                  } in
                  Some (IndexSet.singleton source, visit_config config)
              ) acc
          in
          let by_target = IndexSet.fold process_target lr1_states IndexMap.empty in
          let add_target _ (filter, target) acc = {filter; target; lookahead} :: acc in
          IndexMap.fold add_target by_target acc
        in
        let gotos = List.fold_left process_goto [] gotos in
        gotos :: next

    let initial =
      Vector.init Lr1.n (fun lr1 ->
          let config = {
            top = lr1;
            rest = [];
            lookahead = Terminal.all;
          } in
          visit_transitions config
        )

    let states = Gen.freeze states

    let reachable =
      let reachable =
        states |> Vector.mapi begin fun self (_stack, (inner, outer)) ->
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
                IndexSet.union acc
                  (Vector.get reachable candidate.target))
              acc step
          in
          {reachable; candidates=step} :: steps
      in
      {inner = process_steps inner; outer = process_steps outer}

    let states = Vector.map (fun (stack, steps) -> (stack, make_reduction_step steps)) states

    let initial = Vector.map make_reduction_step initial
    let reachable = Vector.get reachable

    let get_def = Vector.get states
    let get_config st = fst (get_def st)
    let get_transitions st = snd (get_def st)
    let get_stack st =
      let config = get_config st in
      config.top :: config.rest

    let to_string state =
      let {top; rest; _} = get_config state in
      let states = List.rev (top :: rest) in
      string_concat_map " " Lr1.to_string states

    let () = Stopwatch.leave time
  end

  (* [RE]: Syntax for regular expression extended with reduction operator *)
  module RE : RE with module Info := Info
                  and type redstate := Redgraph.state =
  struct
    type uid = int

    let uid =
      let k = ref 0 in
      fun () -> incr k; !k

    type reduction = {
      pattern: Redgraph.state indexset;
      capture: Capture.set;
      policy: Syntax.quantifier_kind;
    }

    let compare_reduction r1 r2 =
      if r1 == r2 then 0 else
        let c = IndexSet.compare r1.pattern r2.pattern in
        if c <> 0 then c else
          IndexSet.compare r1.capture r2.capture

    type t = {
      uid : uid;
      desc : desc;
      position : Syntax.position;
    }
    and desc =
      | Set of Lr1.set * Capture.set
      | Alt of t list
      | Seq of t list
      | Star of t * Syntax.quantifier_kind
      | Filter of Lr1.set
      | Reduce of Capture.set * reduction

    let make position desc = {uid = uid (); desc; position}

    let compare t1 t2 =
      Int.compare t1.uid t2.uid

    let cmon_reduction {capture; pattern; policy} =
      Cmon.record [
        "capture", cmon_indexset capture;
        "pattern", cmon_set_cardinal (*cmon_indexset*) pattern;
        "policy", Syntax.cmon_quantifier_kind policy;
      ]

    let cmon t =
      let rec aux t =
        match t.desc with
        | Set (lr1s, var) ->
          Cmon.construct "Set" [
            cmon_set_cardinal lr1s;
            cmon_indexset var;
          ]
        | Alt ts -> Cmon.constructor "Alt" (Cmon.list_map aux ts)
        | Seq ts -> Cmon.constructor "Seq" (Cmon.list_map aux ts)
        | Star (t, qk) -> Cmon.construct "Star" [aux t; Syntax.cmon_quantifier_kind qk]
        | Filter lr1s ->
          Cmon.constructor "Filter" (cmon_set_cardinal lr1s)
        | Reduce (var, r) ->
          Cmon.construct "Reduce" [cmon_indexset var; cmon_reduction r]
      in
      aux t
  end

  module K = struct
    type label = {
      filter: Lr1.set;
      captures: Capture.set;
    }

    let is_immediate_label {filter; captures} =
      IndexSet.equal filter Lr1.all &&
      IndexSet.is_empty captures

    let compare_label l1 l2 =
      if l1 == l2 then 0 else
        let c = IndexSet.compare l1.filter l2.filter in
        if c <> 0 then c else
          IndexSet.compare l1.captures l2.captures

    type t =
      | Done
      | More of RE.t * t
      | Reducing of {
          reduction: RE.reduction;
          transitions: Redgraph.outer_transitions;
          next: t;
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

    let rec compare t1 t2 =
      if t1 == t2 then 0 else
        match t1, t2 with
        | Done, Done -> 0
        | More (e1, t1'), More (e2, t2') ->
          let c = RE.compare e1 e2 in
          if c <> 0 then c else
            compare t1' t2'
        | Reducing r1, Reducing r2 ->
          let c = RE.compare_reduction r1.reduction r2.reduction in
          if c <> 0 then c else
            let c = list_compare compare_reduction_step r1.transitions r2.transitions in
            if c <> 0 then c else
              compare r1.next r2.next
        | Done, (More _ | Reducing _) -> -1
        | (More _ | Reducing _), Done -> +1
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

    let rec cmon = function
      | Done ->
        Cmon.constant "Done"
      | More (re, next) ->
        Cmon.construct "More" [RE.cmon re; cmon next]
      | Reducing {reduction; transitions; next} ->
        Cmon.crecord "Reducing" [
          "reduction"   , RE.cmon_reduction reduction;
          "transitions" , cmon_transitions ~candidate:cmon_outer_candidate transitions;
          "next"        , cmon next;
        ]

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

    let label_capture label vars =
      if not_empty vars then
        {label with captures = IndexSet.union label.captures vars}
      else
        label

    let live_redstep (red : RE.reduction) (step : _ Redgraph.reduction_step) =
      intersecting red.pattern step.reachable

    let live_redstate (red : RE.reduction) (state : Redgraph.state index) =
      intersecting red.pattern (Redgraph.reachable state)

    let rec reduce_target ~on_outer r target =
      (live_redstate r target &&
       reduce_transitions ~on_outer r
         (Redgraph.get_transitions target))
      || IndexSet.mem target r.pattern

    and reduce_transitions ~on_outer r {Redgraph. inner; outer} =
      let matched = ref false in
      let visit_candidate (c : unit Redgraph.goto_candidate) =
        if reduce_target ~on_outer r c.target
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
        on_outer r outer;
      !matched

    let derive k =
      let reduce_outer next label reduction transitions =
        let ks = ref [] in
        let matching = ref IndexSet.empty in
        let rec visit_transitions label reduction = function
          | step :: transitions when live_redstep reduction step ->
            List.iter visit_candidate step.candidates;
            begin match transitions with
              | step' :: _ when live_redstep reduction step' ->
                let reducing = Reducing {reduction; transitions; next} in
                push ks (label, Some reducing)
              | _ -> ()
            end
          | _ -> ()
        and visit_candidate (candidate : Lr1.set Redgraph.goto_candidate) =
          match label_filter label candidate.filter with
          | Some label
            when reduce_target reduction candidate.target
                ~on_outer:(visit_transitions label) ->
            matching := IndexSet.union label.filter !matching
          | _ -> ()
        in
        visit_transitions label reduction transitions;
        let matching =
          if not_empty !matching then
            Some {label with filter = !matching}
          else
            None
        in
        (matching, !ks)
      in
      let ks = ref [] in
      let rec process_k label = function
        | Done ->
          push ks (label, None)

        | More (re, next) as self ->
          process_re label self next re.desc

        | Reducing {reduction; transitions; next} ->
          let l', ks' = reduce_outer next label reduction transitions in
          match l', reduction.policy with
          | None, _ ->
            ks := ks' @ !ks
          | Some label, Longest ->
            ks := ks' @ !ks;
            process_k (label_capture label reduction.capture) next
          | Some label, Shortest ->
            process_k (label_capture label reduction.capture) next;
            ks := ks' @ !ks

      and process_re label self next = function
        | Set (s, var) ->
          begin match label_filter label s with
            | None -> ()
            | Some label ->
              push ks (label_capture label var, Some next)
          end

        | Alt es ->
          List.iter (fun e -> process_k label (More (e, next))) es

        | Star (r, Shortest) ->
          process_k label next;
          process_k label (More (r, self))

        | Star (r, Longest) ->
          process_k label (More (r, self));
          process_k label next

        | Seq es ->
          process_k label (List.fold_right (fun e k -> More (e, k)) es next)

        | Filter filter ->
          begin match label_filter label filter with
            | None -> ()
            | Some label' -> process_k label' next
          end

        | Reduce (cap, r) ->
          let label = label_capture label cap in
          let ks' = ref [] in
          let matching =
            IndexSet.filter (fun lr1 ->
                let steps = Vector.get Redgraph.initial lr1 in
                let on_outer reduction = function
                  | (step :: _) as transitions when live_redstep reduction step ->
                    let label = {label with filter = IndexSet.singleton lr1} in
                    let next = Reducing {reduction; transitions; next} in
                    push ks' (label, Some next)
                  | _ -> ()
                in
                reduce_transitions ~on_outer r steps
              ) label.filter
          in
          let label = label_capture label r.capture in
          begin match r.RE.policy with
            | Shortest ->
              if not_empty matching then
                process_k {label with filter = matching} next;
              ks := !ks' @ !ks
            | Longest ->
              ks := !ks' @ !ks;
              if not_empty matching then
                process_k {label with filter = matching} next;
          end
      in
      let label = {
        filter = Lr1.all;
        captures = IndexSet.empty;
      } in
      process_k label k;
      List.rev !ks
  end
end
