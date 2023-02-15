open Front
open Fix.Indexing
open Utils
open Misc

module Positive = Const(struct let cardinal = max_int end)

include (struct
  type var = Positive.n
  let var i = Index.of_int Positive.n i
end : sig
  type var
  val var : int -> var index
end)

let cmon_pair f g (x, y) = Cmon.tuple [f x; g y]

let cmon_option f = function
  | None -> Cmon.constant "None"
  | Some x -> Cmon.constructor "Some" (f x)

let cmon_var x = Cmon.constructor "Var" (cmon_index x)

module type REDGRAPH = sig
  module Info : Info.S
  open Info

  val simulate_reductions :
    (Production.t * 'a indexset) list ->
    [> `Goto of Nonterminal.t * 'a indexset | `Pop ] Seq.t

  type state
  val state : state cardinal

  type transition = {
    pop : (int * Lr1.set) option;
    lookahead : Terminal.set;
    target : state index;
  }
  type stack = private Lr1.t list
  type state_def = { stack : stack; transitions : transition list; }
  val initial : (Lr1.t * transition list) list
  val states : state index -> state_def
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

  (** A regular expression term with its unique ID, its description and its
      position. *)
  type t = {
    uid : uid;
    desc : desc;
    position : Syntax.position;
  }

  (** The different constructors of regular expressions*)
  and desc =
    | Set of Lr1.set * var index option
    (** Recognise a set of states, and optionally bind the matching state to
        a variable. *)
    | Alt of t list
    (** [Alt ts] is the disjunction of sub-terms [ts] (length >= 2).
        [Alt []] represents the empty language. *)
    | Seq of t list
    (** [Seq ts] is the concatenation of sub-terms [ts] (length >= 2).
        [Seq []] represents the {ε}. *)
    | Star of t
    (** [Star t] is represents the Kleene star of [t] *)
    | Filter of Lr1.set
    | Reduce of {
        capture: (var index * var index) option;
        pattern: redstate indexset;
      }
    (** The reduction operator *)

  (** Introduce a new term, allocating a unique ID *)
  val make : Syntax.position -> desc -> t

  (** Compare two terms *)
  val compare : t -> t -> int

  (** Print a term to a [Cmon] document. [var] arguments allow to customize
      printing of variables. *)
  val cmon : ?var:(var index -> Cmon.t) -> t -> Cmon.t
end

module type K = sig
  module Info : Info.S
  open Info

  type re
  type redstate

  type reduction = {
    target      : redstate index;
    capture_end : var index option;
    pattern     : redstate indexset;
    candidates  : Lr1.set;
    lookahead   : Terminal.set;
  }

  type 'a t =
    | Done of 'a
    | More of re * 'a t
    | Reducing of {pop: int; reduction: reduction; next: 'a t}

  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

  val cmon : ?var:(var index -> Cmon.t) -> ?accept:('a -> Cmon.t) -> 'a t -> Cmon.t

  val derive :
    accept:('a -> unit) ->
    direct:(Info.Lr1.set -> var index option -> 'a t -> unit) ->
    'a t -> unit
end

module type S = sig
  module Info : Info.S
  module Redgraph : REDGRAPH
    with module Info := Info
  module RE : RE
    with module Info := Info
     and type redstate := Redgraph.state
  module K : K
    with module Info := Info
     and type re := RE.t
     and type redstate := Redgraph.state
end

module Make (Info : Info.S)() : S with module Info = Info =
struct
  module Info = Info
  open Info

  module Redgraph : REDGRAPH with module Info := Info =
  struct
    let simulate_reductions reductions : _ Seq.t =
      let rec consume_lhs n lhs lookahead = function
        | (prod, lookahead') :: rest
          when Production.length prod = n && Production.lhs prod = lhs ->
          let lookahead = IndexSet.union lookahead lookahead' in
          consume_lhs n lhs lookahead rest

        | rest ->
          return (`Goto (lhs, lookahead)) n rest

      and step n = function
        | [] -> Seq.Nil

        | (prod, lookahead) :: rest when Production.length prod = n ->
          consume_lhs n (Production.lhs prod) lookahead rest

        | rest -> return `Pop (n + 1) rest

      and return r n next =
        Seq.Cons (r, fun () -> step n next)

      in
      fun () -> step 0 reductions

    include Gensym()
    type state = n
    let state = n

    type stack = Lr1.t list
    type lookahead = Terminal.set

    type transition = {
      pop: (int * Lr1.set) option;
      lookahead: lookahead;
      target: state index;
    }

    type state_def = {
      stack: stack;
      transitions: transition list;
    }

    let states = IndexBuffer.make
        {stack=[IndexSet.choose Lr1.all]; transitions=[]}

    let nodes = Hashtbl.create 7

    let rec visit_stack stack =
      match Hashtbl.find_opt nodes stack with
      | Some state -> state
      | None ->
        let state = fresh () in
        Hashtbl.add nodes stack state;
        let transitions = visit_transitions stack in
        IndexBuffer.set states state {stack; transitions};
        state

    and visit_transitions stack =
      let reductions = Lr1.reductions (List.hd stack) in
      visit_inner stack (simulate_reductions reductions)

    and visit_inner stack actions =
      match actions () with
      | Seq.Nil -> []
      | Seq.Cons (`Pop, next) ->
        begin match stack with
          | [] -> assert false
          | [top] -> visit_outer (Lr1.predecessors top) 0 next
          | _ :: stack -> visit_inner stack next
        end
      | Seq.Cons (`Goto (lhs, lookahead), next) ->
        let transition =
          let goto = Transition.find_goto_target (List.hd stack) lhs in
          let target = visit_stack (goto :: stack) in
          {pop = None; lookahead; target}
        in
        transition :: visit_inner stack next

    and visit_outer states depth actions =
      match actions () with
      | Seq.Nil -> []
      | Seq.Cons (`Pop, next) ->
        visit_outer (Lr1.set_predecessors states) (depth + 1) next
      | Seq.Cons (`Goto (lhs, lookahead), next) ->
        let result = visit_outer states depth next in
        let by_target = IndexSet.fold (fun state acc ->
            let goto = Transition.find_goto_target state lhs in
            IndexMap.update goto (function
                | Some (sources, stack) ->
                  Some (IndexSet.add state sources, stack)
                | None ->
                  Some (IndexSet.singleton state, visit_stack [goto])
              ) acc
          ) states IndexMap.empty
        in
        IndexMap.fold
          (fun _target_state (sources, target) acc ->
             {pop = Some (depth, sources); lookahead; target} :: acc)
          by_target result

    let initial =
      IndexSet.fold
        (fun lr1 acc -> (lr1, visit_transitions [lr1]) :: acc)
        Lr1.idle []

    let states = IndexBuffer.contents states state

    let reachable =
      let reachable =
        Vector.map (fun state ->
            state.transitions
            |> List.map (fun tr -> tr.target)
            |> IndexSet.of_list
          )  states
      in
      let preds = Vector.make state IndexSet.empty in
      Vector.iteri (fun source reachable ->
          IndexSet.iter (fun target -> vector_set_add preds target source)
            reachable
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

    let states = Vector.get states
    let reachable = Vector.get reachable

    let to_string state =
      let states = List.rev ((states state).stack : stack :> Lr1.t list) in
      string_concat_map " " (fun lr1 ->
          match Lr1.incoming lr1 with
          | Some sym -> Symbol.name sym
          | None -> "<initial>"
        ) states
  end

  (* [RE]: Syntax for regular expression extended with reduction operator *)
  module RE : RE with module Info := Info
                  and type redstate := Redgraph.state =
  struct
    type uid = int

    let uid =
      let k = ref 0 in
      fun () -> incr k; !k

    type t = {
      uid : uid;
      desc : desc;
      position : Syntax.position;
    }
    and desc =
      | Set of Lr1.set * var index option
      | Alt of t list
      | Seq of t list
      | Star of t
      | Filter of Lr1.set
      | Reduce of {
          capture: (var index * var index) option;
          pattern: Redgraph.state indexset;
        }

    let make position desc = {uid = uid (); desc; position}

    let compare t1 t2 =
      Int.compare t1.uid t2.uid

    let cmon_set_cardinal set =
      Cmon.constant ("{" ^ string_of_int (IndexSet.cardinal set) ^ " elements}")

    let cmon ?(var=cmon_var) t =
      let rec aux t =
        match t.desc with
        | Set (lr1s, v) ->
          Cmon.construct "Set" [
            cmon_set_cardinal lr1s;
            match v with
            | None -> Cmon.constant "None"
            | Some x -> Cmon.constructor "Some" (var x)
          ]
        | Alt ts -> Cmon.constructor "Alt" (Cmon.list_map aux ts)
        | Seq ts -> Cmon.constructor "Seq" (Cmon.list_map aux ts)
        | Star t -> Cmon.constructor "Star" (aux t)
        | Filter lr1s ->
          Cmon.constructor "Filter" (cmon_set_cardinal lr1s)
        | Reduce {capture; pattern} ->
          Cmon.crecord "Reduce" [
            "capture", cmon_option (cmon_pair var var) capture;
            "pattern", cmon_indexset pattern;
          ]
      in
      aux t
  end

  module K : K with module Info := Info
                and type re := RE.t
                and type redstate := Redgraph.state =
  struct
    type reduction = {
      target: Redgraph.state index;
      capture_end: var index option;
      pattern: Redgraph.state indexset;
      candidates: Lr1.set;
      lookahead: Terminal.set;
    }

    let compare_reduction r1 r2 =
      if r1 == r2 then 0 else
        let c = compare_index r1.target r2.target in
        if c <> 0 then c else
          let c =
            Option.compare compare_index r1.capture_end r2.capture_end
          in
          if c <> 0 then c else
            let c = IndexSet.compare r1.pattern r2.pattern in
            if c <> 0 then c else
              let c = IndexSet.compare r1.candidates r2.candidates in
              if c <> 0 then c else
                IndexSet.compare r1.lookahead r2.lookahead

    type 'a t =
      | Done of 'a
      | More of RE.t * 'a t
      | Reducing of {pop: int; reduction: reduction; next: 'a t}

    let rec compare cmp_a t1 t2 =
      if t1 == t2 then 0 else
        match t1, t2 with
        | Done a1, Done a2 -> cmp_a a1 a2
        | More (e1, t1'), More (e2, t2') ->
          let c = RE.compare e1 e2 in
          if c <> 0 then c else
            compare cmp_a t1' t2'
        | Reducing r1, Reducing r2 ->
          let c = Int.compare r1.pop r2.pop in
          if c <> 0 then c else
            let c = compare_reduction r1.reduction r2.reduction in
            if c <> 0 then c else
              compare cmp_a r1.next r2.next
        | Done _, (More _ | Reducing _) -> -1
        | (More _ | Reducing _), Done _ -> +1
        | More _, Reducing _ -> -1
        | Reducing _, More _ -> +1

    let cmon_reduction ?(var=cmon_var)
        {target; capture_end; pattern; candidates; lookahead} =
      Cmon.record [
        "target"     , cmon_index target;
        "capture_end", cmon_option var capture_end;
        "pattern"    , cmon_indexset pattern;
        "candidates" , cmon_indexset candidates;
        "lookahead"  , cmon_indexset lookahead;
      ]

    let rec cmon ?var ?(accept=fun _ -> Cmon.constant "_") = function
      | Done a ->
        Cmon.constructor "Done" (accept a)
      | More (re, next) ->
        Cmon.construct "More" [RE.cmon ?var re; cmon ?var next]
      | Reducing {pop; reduction; next} ->
        Cmon.crecord "Reducing" [
          "pop"       , Cmon.int pop;
          "reduction" , cmon_reduction ?var reduction;
          "next"      , cmon ?var next;
        ]

    let pop_next label = function
      | None -> (label, None)
      | Some (0, label') -> (Lr1.intersect label label', None)
      | Some (n, label') -> (label, Some (n - 1, label'))

    let reachable state pattern =
      IndexSet.mem state pattern ||
      not (IndexSet.disjoint pattern (Redgraph.reachable state))

    let derive ~accept ~direct k =
      let rec loop filter = function
        | Done a as k ->
          if IndexSet.equal filter Lr1.all then
            accept a
          else
            direct filter None k
        | Reducing {pop; reduction; next} ->
          if pop > 0 then
            direct filter None (Reducing {pop = pop - 1; reduction; next})
          else
            process_transition
              ~capture_start:None
              ~capture_end:reduction.capture_end
              ~label:(Lr1.intersect filter reduction.candidates)
              ~k
              ~pattern:reduction.pattern
              ~pop:None
              ~target:reduction.target
              ~lookahead:reduction.lookahead
        | More (re, k') as k ->
          match re.desc with
          | Set (s, var) ->
            direct (Lr1.intersect filter s) var k'
          | Alt es ->
            List.iter (fun e -> loop filter (More (e, k'))) es
          | Star r ->
            loop filter k';
            loop filter (More (r, k))
          | Seq es ->
            loop filter (List.fold_right (fun e k -> More (e,k)) es k')
          | Reduce {capture; pattern} ->
            Redgraph.initial |>
            List.iter begin fun (lr1, trs) ->
              if (IndexSet.mem lr1 filter) then
                List.iter (fun {Redgraph. pop; target; lookahead} ->
                    process_transition
                      ~capture_start:(Option.map fst capture)
                      ~capture_end:(Option.map snd capture)
                      ~label:(IndexSet.singleton lr1)
                      ~k ~pattern ~pop ~target ~lookahead
                  ) trs
            end
          | Filter filter' ->
            let filter = Lr1.intersect filter' filter in
            if not (IndexSet.is_empty filter) then
              loop filter k'

      and process_transition
          ~capture_start ~capture_end ~label ~k ~pattern
          ~pop ~target ~lookahead =
        if not (IndexSet.is_empty lookahead) &&
           not (IndexSet.is_empty label) &&
           (reachable target pattern)
        then
          match pop with
          | Some (pop, candidates) ->
            let reduction = {capture_end; pattern; candidates; lookahead; target} in
            direct label capture_start (Reducing {pop; reduction; next=k})
          | None ->
            if IndexSet.mem target pattern then
              loop label k;
            (Redgraph.states target).transitions |>
            List.iter begin fun (tr : Redgraph.transition) ->
              if Option.is_some tr.pop then
                let label, pop = pop_next label tr.pop in
                process_transition ~capture_start ~capture_end ~label ~k ~pattern
                  ~pop ~target:tr.target
                  ~lookahead:(Terminal.intersect lookahead tr.lookahead)
            end
      in
      loop Lr1.all k

  end
end
