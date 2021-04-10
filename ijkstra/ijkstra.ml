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

  let () =
    Printf.printf "predecessors stats\n%!";
    Grammar.Lr1.iter (fun lr1 ->
        match Grammar.Lr0.incoming (Grammar.Lr1.lr0 lr1) with
        | None | Some (Grammar.T _) -> ()
        | Some (Grammar.N _) ->
          Printf.printf "%d predecessors\n%!"
            (Red.Lr1.Set.cardinal (Red.Lr1.predecessors_of_state lr1));
    )


  let table = Hashtbl.create 107
  let by_lr1s = Array.make Grammar.Lr1.count []

  let clock = let k = ref 0 in fun () -> incr k; !k

  let g =
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

  module Lookaheads = Utils.Refine.Make(Red.LookaheadSet)

  let coarse_lookaheads =
    Array.mapi (fun lr1 sources ->
        Format.printf "coarse lookahead %d\n%!" lr1;
        let tick = clock () in
        let rec all_lookaheads acc state =
          if state.tick = tick then acc else (
            state.tick <- tick;
            List.fold_left (fun acc (la, state') ->
                all_lookaheads (la :: acc) state'
              ) acc state.predecessors
          )
        in
        let classes = List.fold_left all_lookaheads [] sources in
        Lookaheads.partition classes
      ) by_lr1s
end

let () = match !grammar_file with
  | None ->
    Format.eprintf "No grammar provided (-g), stopping now.\n"
  | Some path ->
    let module Analysis = Analysis(struct let filename = path end)() in
    let t0 = Sys.time () in
    let t1 = Sys.time () in
    Format.eprintf "Built graph in %.02f\n" ((t1 -. t0) *. 1000.0);
    let states, transitions =
      List.fold_left (fun (states, transitions) (_lr1, lr1s) ->
          states + 1, transitions + Analysis.Lr1.Set.cardinal lr1s
        ) (0, 0) Analysis.g
    in
    Format.eprintf "%d states, %d transitions\n" states transitions;
    let total, by_class =
      Array.fold_left (fun (total, by_class) classes ->
          let count = List.fold_left
              (fun sum class' ->
                 sum + Analysis.Red.LookaheadSet.cardinal class')
              0 classes
          in
          total + count, by_class + List.length classes
        ) (0, 0) Analysis.coarse_lookaheads
    in
    let all_classes = List.sort_uniq Analysis.Red.LookaheadSet.compare
        (List.flatten (Array.to_list Analysis.coarse_lookaheads))
    in
    Format.eprintf "%d transition based on lookaheads\n%d transitions based on lookahead classes,%d unique classes\n" total by_class (List.length all_classes)
