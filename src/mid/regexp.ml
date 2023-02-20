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

let cmon_set_cardinal set =
  Cmon.constant ("{" ^ string_of_int (IndexSet.cardinal set) ^ " elements}")

module type REDGRAPH = sig
  module Info : Info.S
  open Info

  type state
  val state : state cardinal

  type 'a goto_sequence = (state indexset * ('a * Terminal.set) list) list
  type inner_transitions = (state index) goto_sequence
  type outer_transitions = (Lr1.set * state index) goto_sequence
  type transitions = {
    inner : inner_transitions;
    outer : outer_transitions;
  }

  type stack = Lr1.t * Lr1.t list

  val initial : (Lr1.t * transitions) list

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

  type reduction = {
    pattern: redstate indexset;
    capture: (var index * var index) option;
  }

  val compare_reduction : reduction -> reduction -> int

  val cmon_reduction : ?var:(var index -> Cmon.t) -> reduction -> Cmon.t

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
    | Reduce of reduction
    (** The reduction operator *)

  (** Introduce a new term, allocating a unique ID *)
  val make : Syntax.position -> desc -> t

  (** Compare two terms *)
  val compare : t -> t -> int

  (** Print a term to a [Cmon] document. [var] arguments allow to customize
      printing of variables. *)
  val cmon : ?var:(var index -> Cmon.t) -> t -> Cmon.t
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
    val cmon : ?var:(var index -> Cmon.t) -> ?accept:('a -> Cmon.t) -> 'a t -> Cmon.t
    val derive :
      accept:(?set:Lr1.set -> 'a -> unit) ->
      direct:(Lr1.set -> var index option -> 'a t -> unit) ->
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
    type 'a goto_sequence = (state indexset * ('a * Terminal.set) list) list
    type inner_transitions = (state index) goto_sequence
    type outer_transitions = (Lr1.set * state index) goto_sequence
    type transitions = {
      inner: inner_transitions;
      outer: outer_transitions;
    }

    let rec group_reductions depth
      : (Production.t * Terminal.set) list -> Nonterminal.t goto_sequence =
      function
      | [] -> []
      | (prod, la) :: rest when depth = Production.length prod ->
        let lhs = Production.lhs prod in
        begin match group_reductions depth rest with
          | (_, ((lhs', la') :: other)) :: tl when lhs = lhs' ->
            (IndexSet.empty, (lhs, IndexSet.union la la') :: other) :: tl
          | [] -> [IndexSet.empty, [lhs, la]]
          | (_, other) :: tl ->
            (IndexSet.empty, (lhs, la) :: other) :: tl
        end
      | otherwise ->
        (IndexSet.empty, []) :: group_reductions (depth + 1) otherwise

    let states =
      IndexBuffer.make ((Index.of_int Lr1.n 0, []), {inner=[]; outer=[]})

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
      | [] ->
        {inner = []; outer = []}
      | (_, goto) :: next ->
        let {inner; outer} = match rest with
          | top' :: rest' ->
            visit_inner (top', rest') next
          | [] ->
            {inner = []; outer = visit_outer (IndexSet.singleton top) next}
        in
        let process_goto (lhs, la) =
          let target = Transition.find_goto_target top lhs in
          (visit_stack (target, top :: rest), la)
        in
        {inner = (IndexSet.empty, List.map process_goto goto) :: inner; outer}

    and visit_outer states = function
      | [] -> []
      | (_, goto) :: next ->
        let states = Lr1.set_predecessors states in
        let next = visit_outer states next in
        let process_goto acc (lhs, la) =
          let process_target source acc =
            let target = Transition.find_goto_target source lhs in
            IndexMap.update target (function
                | Some (sources, stack) ->
                  Some (IndexSet.add source sources, stack)
                | None ->
                  Some (IndexSet.singleton source, visit_stack (target, []))
              ) acc
          in
          let by_target = IndexSet.fold process_target states IndexMap.empty in
          let add_target _ transition acc = (transition, la) :: acc in
          IndexMap.fold add_target by_target acc
        in
        let goto = List.fold_left process_goto [] goto in
        (IndexSet.empty, goto) :: next

    let initial =
      IndexSet.fold
        (fun lr1 acc -> (lr1, visit_transitions (lr1, [])) :: acc)
        Lr1.idle []

    let states_def = IndexBuffer.contents states state

    let reachable =
      let reachable =
        states_def |> Vector.mapi begin fun self (_stack, {inner; outer}) ->
          let ix = ref (IndexSet.singleton self) in
          let add st = ix := IndexSet.add st !ix in
          List.iter (fun (_, trs) ->
            List.iter (fun (st, _) -> add st) trs
            ) inner;
          List.iter (fun (_reach, trs) ->
              List.iter (fun ((_, st), _) -> add st) trs
            ) outer;
          !ix
        end
      in
      let preds = Vector.make state IndexSet.empty in
      Vector.iteri (fun source reachable ->
          IndexSet.iter (fun target ->
              if target <> source then
                vector_set_add preds target source
            ) reachable
        ) reachable;
      begin
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
      end;
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

    let () =
      Vector.iteri (fun st reachable ->
          Printf.eprintf "#%d: %d reachable\n"
            (Index.to_int st) (IndexSet.cardinal reachable)
        ) reachable

    let reachable = Vector.get reachable

    let populate_trs (x, {inner; outer}) =
      let reached = ref IndexSet.empty in
      let rec process f = function
        | [] -> []
        | (_, transitions) :: xs ->
          let xs = process f xs in
          List.iter (fun (x, _) ->
              reached := IndexSet.union !reached (reachable (f x))
            ) transitions;
          (!reached, transitions) :: xs
      in
      let inner = process Fun.id inner in
      reached := IndexSet.empty;
      let outer = process snd outer in
      (x, {inner; outer})

    let () =
      Vector.iteri (fun ix trs ->
          Vector.set states_def ix (populate_trs trs)
        ) states_def

    let initial = List.map populate_trs initial

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

    type reduction = {
      pattern: Redgraph.state indexset;
      capture: (var index * var index) option;
    }

    let compare_reduction r1 r2 =
      if r1 == r2 then 0 else
        let c = IndexSet.compare r1.pattern r2.pattern in
        if c <> 0 then c else
          Option.compare (compare_pair compare_index compare_index) r1.capture r2.capture

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
      | Reduce of reduction

    let make position desc = {uid = uid (); desc; position}

    let compare t1 t2 =
      Int.compare t1.uid t2.uid

    let cmon_reduction ?(var=cmon_var) {capture; pattern} =
      Cmon.record [
        "capture", cmon_option (cmon_pair var var) capture;
        "pattern", cmon_set_cardinal (*cmon_indexset*) pattern;
      ]

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
        | Reduce r ->
          Cmon.constructor "Reduce" (cmon_reduction ~var r)
      in
      aux t
  end

  module K = struct
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

    let compare_outer_transition ((s1, t1), la1) ((s2, t2), la2) =
      let c = compare_index t1 t2 in
      if c <> 0 then c else
        let c = IndexSet.compare s1 s2 in
        if c <> 0 then c else
          IndexSet.compare la1 la2

    let compare_outer_transitions (r1, l1) (r2, l2) =
      let c = IndexSet.compare r1 r2 in
      if c <> 0 then c else
        list_compare compare_outer_transition l1 l2

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
            let c = list_compare compare_outer_transitions r1.transitions r2.transitions in
            if c <> 0 then c else
              let c = IndexSet.compare r1.lookahead r2.lookahead in
              if c <> 0 then c else
                compare cmp_a r1.next r2.next
        | Done _, (More _ | Reducing _) -> -1
        | (More _ | Reducing _), Done _ -> +1
        | More _, Reducing _ -> -1
        | Reducing _, More _ -> +1

    let cmon_outer_transitions ((s, t), la) =
      Cmon.record [
        "label"     , (
          if IndexSet.cardinal s < 10
          then cmon_indexset ~index:(fun lr1 -> Cmon.constant (Lr1.to_string lr1)) s
          else cmon_set_cardinal (*cmon_indexset*) s
        );
        "redstate"  , cmon_index t;
        "lookahead" , cmon_set_cardinal (*cmon_indexset*) la;
      ]

    let rec cmon ?var ?(accept=fun _ -> Cmon.constant "_") = function
      | Done a ->
        Cmon.constructor "Done" (accept a)
      | More (re, next) ->
        Cmon.construct "More" [RE.cmon ?var re; cmon ?var next]
      | Reducing {reduction; lookahead; transitions; next} ->
        Cmon.crecord "Reducing" [
          "reduction"   , RE.cmon_reduction ?var reduction;
          "lookahead"   , cmon_set_cardinal (*cmon_indexset*) lookahead;
          "transitions" , Cmon.list_map (function
              | (reachable, []) ->
                Cmon.record [
                  "reachable", cmon_set_cardinal reachable;
                  "action", Cmon.constant "Pop";
                ]
              | (reachable, xs) ->
                Cmon.record [
                  "reachable", cmon_set_cardinal reachable;
                  "action", Cmon.constructor "Goto" (Cmon.list_map cmon_outer_transitions xs)
                ]
            )
            transitions;
          "next"        , cmon ?var next;
        ]

    let pattern_reachable ~from:state pattern =
      not (IndexSet.disjoint pattern (Redgraph.reachable state))

    let rec reduce_target (r : RE.reduction) la target process_outer =
      let matched = ref false in
      if not (IndexSet.is_empty la) then (
        if IndexSet.mem target r.pattern then
          matched := true;
        if pattern_reachable r.pattern ~from:target &&
           reduce_transitions r process_outer la (Redgraph.get_transitions target)
        then
          matched := true;
      );
      !matched

    and reduce_transitions (r : RE.reduction) process_outer la {Redgraph. inner; outer} =
      assert (not (IndexSet.is_empty la));
      let matched = ref false in
      let visit_tr (target', la') =
        if reduce_target r (Terminal.intersect la la') target' process_outer
        then matched := true
      in
      let rec loop = function
        | [] -> ()
        | (reachable, trs) :: xs ->
          if not (IndexSet.disjoint reachable r.pattern) then (
            List.iter visit_tr trs;
            loop xs
          )
      in
      loop inner;
      if outer <> [] then
        process_outer r la outer;
      !matched

    let derive ~accept ~direct k =
      let rec reduce_outer matching filter next reduction lookahead = function
        | [] -> ()
        | (reachable, goto) :: transitions ->
          if not (IndexSet.disjoint reachable reduction.RE.pattern) then (
            begin match transitions with
              | (reachable, _) :: _
                when not (IndexSet.disjoint reachable reduction.RE.pattern) ->
                direct filter None
                  (Reducing {reduction; transitions; lookahead; next});
              | _ -> ()
            end;
            List.iter (fun ((filter', target), lookahead') ->
                let filter = Lr1.intersect filter filter' in
                let lookahead = Terminal.intersect lookahead lookahead' in
                if not (IndexSet.is_empty filter) then (
                  if reduce_target reduction lookahead target
                      (reduce_outer matching filter next)
                  then matching := IndexSet.union filter !matching;
                )
              ) goto
          )
      in
      let rec process_k filter k =
        if not (IndexSet.is_empty filter) then
          match k with
          | Done a ->
            accept
              ?set:(if IndexSet.equal filter Lr1.all then None else Some filter)
              a

          | More (re, k) as self ->
            process_re filter self k re.desc

          | Reducing {reduction; transitions; lookahead; next} ->
            let matching = ref IndexSet.empty in
            reduce_outer matching filter next reduction lookahead transitions;
            process_k !matching next

      and process_re filter self next = function
        | Set (s, var) ->
          let s = Lr1.intersect filter s in
          if not (IndexSet.is_empty s) then
            direct s var next

        | Alt es ->
          List.iter (fun e -> process_k filter (More (e, next))) es

        | Star r ->
          process_k filter next;
          process_k filter (More (r, self))

        | Seq es ->
          process_k filter (List.fold_right (fun e k -> More (e,k)) es next)

        | Filter filter' ->
          process_k (Lr1.intersect filter' filter) next

        | Reduce r ->
          let matching = ref IndexSet.empty in
          List.iter (fun (lr1, trs) ->
              if IndexSet.mem lr1 filter then (
                let process_outer reduction lookahead = function
                  | (reachable, _) :: _ as transitions
                    when not (IndexSet.disjoint reachable reduction.RE.pattern) ->
                    direct (IndexSet.singleton lr1) None
                      (Reducing {reduction; lookahead; transitions; next})
                  | _ -> ()
                in
                if reduce_transitions r process_outer Terminal.all trs then
                  matching := IndexSet.add lr1 !matching
              )
            ) Redgraph.initial;
          process_k !matching next
      in
      process_k Lr1.all k

  end
end
