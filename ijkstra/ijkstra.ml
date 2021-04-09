let grammar_file = ref None

let usage = "usage: menhirlex [options] sourcefile"

let print_version_string () =
  print_string "The Menhir parser lexer generator :-], version ";
  print_string Sys.ocaml_version;
  print_newline ();
  exit 0

let print_version_num () =
  print_endline Sys.ocaml_version;
  exit 0

let specs = [
  "-g", Arg.String (fun x -> grammar_file := Some x),
  " <file.cmly>  Path of the Menhir compiled grammar to analyse (*.cmly)";
  "-v",  Arg.Unit print_version_string,
  " Print version and exit";
  "-version",  Arg.Unit print_version_string,
  " Print version and exit";
  "-vnum",  Arg.Unit print_version_num,
  " Print version number and exit";
]

let () = Arg.parse specs invalid_arg usage

module Analysis (X : sig val filename : string end)() =
struct
  module Grammar = MenhirSdk.Cmly_read.Read(X)
  module Lr1 = Middle.Lr1.Make(Grammar)
  module Sigma = Middle.Sigma.Make(Lr1)
  module Red = Reduction_graph.Make(Sigma)()

  type state = {
    repr: Red.Graph.state;
    mutable successors: (Red.LookaheadSet.t * state) list;
    mutable predecessors: (Red.LookaheadSet.t * state) list;
    mutable tick: int;
  }

  let table = Hashtbl.create 107
  let by_lr1s = Array.make Grammar.Lr1.count []

  let build () =
    let clock = let k = ref 0 in fun () -> incr k; !k in
    (* Enumerate interesting states, that forces shifting a terminal *)
    let strong_states =
      Grammar.Lr1.fold (fun lr1 acc ->
          match Grammar.Lr0.incoming (Grammar.Lr1.lr0 lr1) with
          | Some (Grammar.N _) -> acc
          | None | Some (Grammar.T _) -> lr1 :: acc
        ) []
    in
    (* Visit them *)
    let rec visit repr =
      match Hashtbl.find table repr with
      | state -> state
      | exception Not_found ->
        let state = {
          repr;
          successors = [];
          predecessors = [];
          tick = 0;
        } in
        Hashtbl.add table repr state;
        let top = Grammar.Lr1.to_int (Red.Graph.stack_top repr) in
        by_lr1s.(top) <- state :: by_lr1s.(top);
        let add_state lookahead repr' =
          let state' = visit repr' in
          state.successors <- (lookahead, state') :: state.successors;
          state'.predecessors <- (lookahead, state) :: state'.predecessors;
        in
        List.iter (fun (lookahead, tr) ->
            match tr with
            | Red.Graph.Epsilon repr' -> add_state lookahead repr'
            | Red.Graph.Targets {dispatch; _} ->
              List.iter (fun (_,repr') -> add_state lookahead repr') dispatch
          ) (Red.Graph.transitions repr);
        state
    in
    List.iter (fun lr1 -> ignore (visit (Red.Graph.from_lr1 lr1)))
      strong_states;
    (* Now reverse them *)
    List.map (fun lr1 ->
        lr1,
        let lr0 = Grammar.Lr1.lr0 lr1 in
        match Grammar.Lr0.incoming lr0 with
        | Some (Grammar.N _) -> assert false
        | None -> Lr1.Set.empty
        | Some (Grammar.T lookahead) ->
          let predecessors = Lr1.predecessors_of_state lr1 in
          let tick = clock () in
          let rec reachable acc state =
            match state.repr with
            | Red.Graph.Lr1 lr1' -> Lr1.Set.add lr1' acc
            | _ ->
              List.fold_left (fun acc (lookahead_set, state') ->
                  if Red.LookaheadSet.mem lookahead lookahead_set &&
                     state'.tick < tick
                  then (
                    state'.tick <- tick;
                    reachable acc state'
                  ) else acc
                ) acc state.predecessors
          in
          Lr1.Set.fold (fun lr1 acc ->
              List.fold_left reachable acc by_lr1s.(Grammar.Lr1.to_int lr1)
            ) predecessors Lr1.Set.empty
      ) strong_states

  module Dijkstra_graph = struct

    type frame = {
      skip: int;
      states: Lr1.Set.t;
    }

    type t = {
      top: Lr1.t;
      stack: frame list;
    }

    let initial top = { top; stack = [] }

    let apply_epsilon source t =
      match t.stack with
      | [] -> Some { top = source; stack = [] }
      | { skip = 0; states } :: stack ->
        if Lr1.Set.mem source states
        then Some { top = source; stack }
        else None
      | frame :: stack' ->
        let stack = {frame with skip = frame.skip - 1} :: stack' in
        Some { top = source; stack }

    let apply_target pop dispatch =





    let transitions t =
      let candidates = by_lr1s t.top in
      let rec update_stack stack =
      in



  end

end

let () = match !grammar_file with
  | None ->
    Format.eprintf "No grammar provided (-g), stopping now.\n"
  | Some path ->
    let module Analysis = Analysis(struct let filename = path end)() in
    let t0 = Sys.time () in
    let g = Analysis.build () in
    let t1 = Sys.time () in
    Format.eprintf "Built graph in %.02f\n" ((t1 -. t0) *. 1000.0);
    let states, transitions =
      List.fold_left (fun (states, transitions) (_lr1, lr1s) ->
          states + 1, transitions + Analysis.Lr1.Set.cardinal lr1s
        ) (0, 0) g
    in
    Format.eprintf "%d states, %d transitions\n" states transitions
