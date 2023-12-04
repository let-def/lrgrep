open Fix.Indexing
open Utils
open Misc

module type S = sig
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

module Make(Info : Info.S)() : S with module Info := Info =
struct
  open Info

  let time = Stopwatch.enter Stopwatch.main "Viable_reductions.Make"

  module State = IndexBuffer.Gen.Make()
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

  let reductions =
    let rec group depth
      : (Production.t * Terminal.set) list -> (Nonterminal.t * Terminal.set) list list =
      function
      | [] -> []
      | (prod, la) :: rest when depth = Production.length prod ->
        let lhs = Production.lhs prod in
        begin match group depth rest with
          | ((lhs', la') :: other) :: tl when lhs = lhs' ->
            ((lhs, IndexSet.union la la') :: other) :: tl
          | [] -> [[lhs, la]]
          | other :: tl ->
            ((lhs, la) :: other) :: tl
          end
      | otherwise ->
        [] :: group (depth + 1) otherwise
    in
    Vector.init Lr1.n (fun lr1 -> group 0 (Lr1.reductions lr1))

  let states = State.get_generator ()

  let nodes = Hashtbl.create 7

  let rec visit_config config =
    match Hashtbl.find_opt nodes config with
    | Some state -> state
    | None ->
      let reservation = IndexBuffer.Gen.reserve states in
      let index = IndexBuffer.Gen.index reservation in
      Hashtbl.add nodes config index;
      IndexBuffer.Gen.commit states reservation (config, visit_transitions config);
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
      let process_goto acc (lhs, lookahead') =
        let lookahead = Terminal.intersect lookahead lookahead' in
        if IndexSet.is_empty lookahead then acc
        else
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

  let states = IndexBuffer.Gen.freeze states

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

  let () =
    Stopwatch.step time "Viable reductions graph has %d nodes" (cardinal state);
    Stopwatch.leave time
end
