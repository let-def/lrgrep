open Fix.Indexing
open Utils
open Misc

module type S = sig
  module Info : Info.S
  open Info

  include CARDINAL

  type 'a goto_candidate = {
    target: n index;
    lookahead: Terminal.set;
    filter: 'a;
    reduction: Reduction.t;
  }

  type 'a reduction_step = {
    reachable: n indexset;
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

  val initial : (Lr1.n, n index) vector

  val get_config : n index -> config
  val get_stack : n index -> Lr1.t list
  val get_transitions : n index -> transitions

  val reachable : n index -> n indexset

  val to_string : n index -> string
end

module Make(Info : Info.S)() : S with module Info := Info =
struct
  open Info

  let time = Stopwatch.enter Stopwatch.main "Viable_reductions.Make"

  include IndexBuffer.Gen.Make()

  type config = {
    top: Lr1.t;
    rest: Lr1.t list;
    lookahead: Terminal.set;
  }

  type 'a goto_candidate = {
    target: n index;
    lookahead: Terminal.set;
    filter: 'a;
    reduction: Reduction.t;
  }

  type 'a reduction_step = {
    reachable: n indexset;
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
      : Reduction.t list -> Reduction.set list =
      function
      | [] -> []
      | red :: rest when depth = Production.length (Reduction.production red) ->
        begin match group depth rest with
          | [] -> [IndexSet.singleton red]
          | reds :: tail ->
            IndexSet.add red reds :: tail
          end
      | otherwise ->
        IndexSet.empty :: group (depth + 1) otherwise
    in
    Vector.init Lr1.n
      (fun lr1 -> group 0 (IndexSet.elements (Reduction.from_lr1 lr1)))

  let states = get_generator ()

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
      let process_goto red =
        let prod = Reduction.production red in
        let top = Transition.find_goto_target config.top (Production.lhs prod) in
        let lookahead = Terminal.intersect (Reduction.lookaheads red) config.lookahead in
        if IndexSet.is_empty lookahead then
          None
        else
          let rest = config.top :: config.rest in
          let target = visit_config {top; rest; lookahead} in
          Some {target; lookahead; filter=(); reduction=red}
      in
      (List.filter_map process_goto (IndexSet.elements gotos) :: inner, outer)

  and visit_outer lookahead successors = function
    | [] -> []
    | gotos :: next ->
      let lr1_states = Lr1.set_predecessors successors in
      let next = visit_outer lookahead lr1_states next in
      let process_goto red acc =
        let lookahead =
          Terminal.intersect lookahead (Reduction.lookaheads red)
        in
        if IndexSet.is_empty lookahead then acc
        else
          let lhs = Production.lhs (Reduction.production red) in
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
          let add_target _ (filter, target) acc =
            {filter; target; lookahead; reduction=red} :: acc
          in
          IndexMap.fold add_target by_target acc
      in
      let gotos = IndexSet.fold process_goto gotos [] in
      gotos :: next

  let initial =
    Vector.init Lr1.n (fun lr1 ->
      let config = {
        top = lr1;
        rest = [];
        lookahead = Terminal.all;
      } in
      visit_config config
    )

  let states = IndexBuffer.Gen.freeze states

  let reachable =
    let reachable =
      let add_target acc step = IndexSet.add step.target acc in
      let add_targets acc l =
        List.fold_left (List.fold_left add_target) acc l
      in
      Vector.mapi
        (fun self (_stack, (inner, outer)) ->
          add_targets (add_targets (IndexSet.singleton self) inner) outer)
        states
    in
    close_relation reachable;
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

  (*let initial = Vector.map make_reduction_step initial*)
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
    Stopwatch.step time "Viable reductions graph has %d nodes" (cardinal n);
    Stopwatch.leave time
end
