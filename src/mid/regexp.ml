open Front
open Fix.Indexing
open Utils
open Misc

module Positive = Const(struct let cardinal = max_int end)

type var = Positive.n

let fresh_var =
  let k = ref (-1) in
  fun () -> incr k; Index.of_int Positive.n !k

module type REDGRAPH = sig
  module Info : Info.S
  open Info

  val simulate_reductions :
    (Production.t * 'a indexset) list ->
    [> `Goto of Nonterminal.t * 'a indexset | `Pop ] Seq.t

  module State : CARDINAL

  type state = State.n index
  type stateset = State.n indexset
  type lookahead = Terminal.set

  type transition = {
    pop : (int * Lr1.set) option;
    lookahead : lookahead;
    target : State.n index;
  }

  type stack = private Lr1.t list
  type state_def = { stack : stack; transitions : transition list; }
  val initial : (Lr1.t * transition list) list
  val states : (State.n, state_def) vector
  val reachable : (State.n, State.n indexset) vector
  val by_symbols : Symbol.t list -> State.n indexset
end

module type S = sig
  module Info : Info.S

  module Redgraph : REDGRAPH with module Info := Info
end

module Make (Info : Info.S)() =
struct
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

        | rest -> return `Pop n rest

      and return r n next =
        Seq.Cons (r, fun () -> step (n + 1) next)

      in
      fun () -> step 0 reductions

    module State = Gensym()

    module Stack : sig
      type t = private Lr1.t list

      val init : Lr1.t -> t
      val push : Lr1.t -> t -> t
      val top : t -> Lr1.t
      val pop : t -> t option
    end = struct
      type t = Lr1.t list
      let init lr1 = [lr1]
      let push = List.cons
      let top = List.hd
      let pop = function
        | [] -> assert false
        | [_] -> None
        | _ :: xs -> Some xs
    end
    type stack = Stack.t
    type lookahead = Terminal.set

    type transition = {
      pop: (int * Lr1.set) option;
      lookahead: lookahead;
      target: State.n index;
    }

    type state_def = {
      stack: Stack.t;
      transitions: transition list;
    }

    let states = IndexBuffer.make
        {stack=Stack.init (IndexSet.choose Lr1.all); transitions=[]}

    let nodes = Hashtbl.create 7

    let rec visit_stack stack =
      match Hashtbl.find_opt nodes stack with
      | Some state -> state
      | None ->
        let state = State.fresh () in
        Hashtbl.add nodes stack state;
        let transitions = visit_transitions stack in
        IndexBuffer.set states state {stack; transitions};
        state

    and visit_transitions stack =
      let reductions = Lr1.reductions (Stack.top stack) in
      visit_inner stack (simulate_reductions reductions)

    and visit_inner stack actions =
      match actions () with
      | Seq.Nil -> []
      | Seq.Cons (`Pop, next) ->
        begin match Stack.pop stack with
          | Some stack -> visit_inner stack next
          | None -> visit_outer (Lr1.predecessors (Stack.top stack)) 0 next
        end
      | Seq.Cons (`Goto (lhs, lookahead), next) ->
        let transition =
          let goto = Transition.find_goto_target (Stack.top stack) lhs in
          let target = visit_stack (Stack.push goto stack) in
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
                  Some (IndexSet.singleton state, visit_stack (Stack.init goto))
              ) acc
          ) states IndexMap.empty
        in
        IndexMap.fold
          (fun _target_state (sources, target) acc ->
             {pop = Some (depth, sources); lookahead; target} :: acc)
          by_target result

    let initial =
      let acc = ref [] in
      Index.iter Lr1.n (fun lr1 ->
          match Lr1.incoming lr1 with
          | None | Some (Symbol.T _) ->
            push acc (lr1, visit_transitions (Stack.init lr1))
          | Some (Symbol.N _) -> ()
        );
      !acc

    let states = IndexBuffer.contents states State.n

    let reachable =
      let reachable =
        Vector.map (fun state ->
            state.transitions
            |> List.map (fun tr -> tr.target)
            |> IndexSet.of_list
          )  states
      in
      let preds = Vector.make State.n IndexSet.empty in
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
      Index.iter State.n process;
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

    let by_symbols =
      let table = Hashtbl.create 7 in
      Vector.iteri (fun index state ->
          let syms =
            List.rev_map (fun lr1 -> Option.get (Lr1.incoming lr1))
              (state.stack : Stack.t :> _ list)
          in
          match Hashtbl.find_opt table syms with
          | None -> Hashtbl.add table syms (ref (IndexSet.singleton index))
          | Some indices -> indices := IndexSet.add index !indices
        ) states;
      fun syms ->
        match Hashtbl.find_opt table syms with
        | None -> IndexSet.empty
        | Some indices -> !indices

    let initial_by_reachability states =
      List.filter_map (fun (lr1, tr) ->
          match
            List.filter (fun tr ->
                let reachable = Vector.get reachable tr.target in
                not (IndexSet.disjoint reachable states)
              ) tr
          with
          | [] -> None
          | tr -> Some (lr1, tr)
        ) initial

    type state = State.n index
    type stateset = State.n indexset
  end

  (* [RE]: Syntax for regular expression extended with reduction operator *)
  module RE : sig
    (** Integers that serves has unique id to identify sub-terms.
        Thanks to properties of Antimirov's derivatives, no new term is
        introduced during derivation. All terms are produced during initial
        parsing. *)
    type uid = private int

    type reduction = {
      target: Redgraph.state;
      capture: var index option;
      pattern: Redgraph.stateset;
      candidates: Lr1.set;
      lookahead: Redgraph.lookahead;
    }

    (** A regular expression term with its unique ID, its description and its
        position. *)
    type t =
      | Normal of { uid : uid; desc : desc; position : Syntax.position }
      | Reduction of { pop: int; reduction: reduction }

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
          pattern: Redgraph.stateset;
        }
      (** The reduction operator *)

    (** Introduce a new term, allocating a unique ID *)
    val make : Syntax.position -> desc -> t

    (** Compare two terms *)
    val compare : t -> t -> int

    (** Print a term to a [Cmon] document. [var] arguments allow to customize
        printing of variables. *)
    val cmon : ?var:(var index -> Cmon.t) -> t -> Cmon.t

  end = struct
    type uid = int

    let uid =
      let k = ref 0 in
      fun () -> incr k; !k

    type reduction = {
      target: Redgraph.state;
      capture: var index option;
      pattern: Redgraph.stateset;
      candidates: Lr1.set;
      lookahead: Redgraph.lookahead;
    }

    type t =
      | Normal of { uid : uid; desc : desc; position : Syntax.position }
      | Reduction of { pop: int; reduction: reduction }
    and desc =
      | Set of Lr1.set * var index option
      | Alt of t list
      | Seq of t list
      | Star of t
      | Filter of Lr1.set
      | Reduce of {
          capture: (var index * var index) option;
          pattern: Redgraph.stateset;
        }

    let make position desc = Normal {uid = uid (); desc; position}

    let compare t1 t2 =
      match t1, t2 with
      | Normal t1, Normal t2 -> Int.compare t1.uid t2.uid
      | Reduction r1, Reduction r2 ->
        let c = Int.compare r1.pop r2.pop in
        if c <> 0 then c else
          compare r1.reduction r2.reduction
      | Normal _, Reduction _ -> -1
      | Reduction _, Normal _ -> +1

    let cmon_set_cardinal set =
      Cmon.constant ("{" ^ string_of_int (IndexSet.cardinal set) ^ " elements}")

    let cmon_pair f g (x, y) = Cmon.tuple [f x; g y]

    let cmon_option f = function
      | None -> Cmon.constant "None"
      | Some x -> Cmon.constructor "Some" (f x)

    let cmon_var x = Cmon.constructor "Var" (cmon_index x)

    let cmon_reduction ?(var=cmon_var) {target; capture; pattern; candidates; lookahead} =
      Cmon.record [
        "target"     , cmon_index target;
        "capture"    , cmon_option var capture;
        "pattern"    , cmon_indexset pattern;
        "candidates" , cmon_indexset candidates;
        "lookahead"  , cmon_indexset lookahead;
      ]

    let cmon ?(var=cmon_var) t =
      let rec aux = function
        | Reduction {pop; reduction} ->
          Cmon.crecord "Reduction" [
            "pop", Cmon.int pop;
            "reduction", cmon_reduction ~var reduction;
          ]
        | Normal t ->
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


  module K : sig
    type clause = Positive.n

    val clause : int -> clause index

    type t =
      | Done of {clause: clause index}
      | More of RE.t * t

    val cmon : ?var:(var index -> Cmon.t) -> t -> Cmon.t

    val derive :
      accept:(clause index -> unit) ->
      direct:(Lr1.set -> clause index option -> t -> unit) ->
      t -> unit
  end = struct
    type clause = Positive.n

    let clause n =
      Index.of_int Positive.n n

    type t =
      | Done of {clause: clause index}
      | More of RE.t * t

    let more re k = More (re, k)

    let rec cmon ?var = function
      | Done {clause} ->
        Cmon.crecord "Done" ["clause", cmon_index clause]
      | More (re, k) ->
        Cmon.construct "More" [RE.cmon ?var re; cmon ?var k]

    let derive ~accept ~direct k =
      let rec loop filter = function
        | Done {clause} -> accept clause
        | More (Reduction {pop; reduction}, k) ->
          if pop > 0 then
            direct filter None (more (Reduction {pop = pop - 1; reduction}) k)
          else
            process_transition
              ~capture0:None
              ~capture:reduction.capture
              ~label:(Lr1.intersect filter reduction.candidates)
              ~k
              ~pattern:reduction.pattern
              ~pop:None
              ~target:reduction.target
              ~lookahead:reduction.lookahead
        | More (Normal re, k') as k ->
          match re.desc with
          | Set (s, var) ->
            direct (Lr1.intersect filter s) var k'
          | Alt es ->
            List.iter (fun e -> loop filter (more e k')) es
          | Star r ->
            loop filter k';
            loop filter (more r k)
          | Seq es ->
            loop filter (List.fold_right more es k')
          | Reduce {capture; pattern} ->
            List.iter (fun (lr1, trs) ->
                if (IndexSet.mem lr1 filter) then
                  List.iter (fun {Redgraph. pop; target; lookahead} ->
                      process_transition
                        ~capture0:(Option.map fst capture)
                        ~capture:(Option.map snd capture)
                        ~label:(IndexSet.singleton lr1)
                        ~k ~pattern ~pop ~target ~lookahead
                    ) trs
              ) Redgraph.initial
          | Filter filter' ->
            let filter = Lr1.intersect filter' filter in
            if not (IndexSet.is_empty filter) then
              loop filter k'

      and process_transition
          ~capture0 ~capture ~label ~k ~pattern
          ~pop ~target ~lookahead =
        if (IndexSet.mem target pattern ||
            not (IndexSet.disjoint pattern (Vector.get Redgraph.reachable target))) &&
           not (IndexSet.is_empty lookahead)
        then match pop with
          | Some (pop, candidates) ->
            let reduction = {RE. capture; pattern; candidates; lookahead; target} in
            direct label capture0 (more (Reduction {pop; reduction}) k)
          | None ->
            if IndexSet.mem target pattern then
              loop label k;
            let state = Vector.get Redgraph.states target in
            List.iter (fun (tr : Redgraph.transition) ->
                let label, pop = match tr.pop with
                  | None -> (label, None)
                  | Some (0, label') -> (Lr1.intersect label label', None)
                  | Some (n, label') -> (label, Some (n - 1, label'))
                in
                process_transition ~capture0 ~capture ~label ~k ~pattern
                  ~pop ~target:tr.target
                  ~lookahead:(Terminal.intersect lookahead tr.lookahead)
              ) state.transitions
      in
      loop Lr1.all k

  end
end
