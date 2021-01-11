module Fin = Strong.Finite

module Make (G : sig
    type states
    val states : states Fin.set
    val successors : states Fin.elt -> (states Fin.elt -> 'a -> 'a) -> 'a -> 'a
  end) :
sig
  type sccs
  val scc : (G.states, sccs Fin.elt) Fin.Array.t
  val sccs : (sccs, G.states Fin.elt list) Fin.Array.t
end = struct

  type status = {
    elt: G.states Fin.elt;
    mutable index: int;
    mutable lowlink: int;
    mutable on_stack: bool;
  }

  let table =
    Fin.Array.init G.states
      (fun elt -> { elt; index = -1; lowlink = -1 ; on_stack = false })

  let push (count, stack) state =
    state.on_stack <- true;
    state.index <- count;
    state.lowlink <- count;
    (count + 1, state :: stack)

  let pop until (count, xs) =
    let rec aux acc = function
      | x :: xs ->
        x.on_stack <- false;
        let acc = x.elt :: acc in
        if x == until then acc, xs else aux acc xs
      | [] -> assert false
    in
    let scc, xs' = aux [] xs in
    (scc, (count, xs'))

  let rec strong_connect (sccs, stack) state =
    let stack = push stack state in
    let (sccs, stack) =
      G.successors state.elt begin fun elt' acc ->
        let state' = Fin.Array.get table elt' in
        if state'.index = -1 then begin
          let acc = strong_connect acc state' in
          state.lowlink <- min state.lowlink state'.lowlink;
          acc
        end
        else begin
          if state'.on_stack then
            state.lowlink <- min state.lowlink state'.index;
          acc
        end
      end (sccs, stack)
    in
    if state.lowlink = state.index then
      let scc, stack = pop state stack in
      (scc :: sccs, stack)
    else
      (sccs, stack)

  let sccs, _ = Fin.Set.fold_left G.states begin fun acc elt ->
      let state = Fin.Array.get table elt in
      if state.index = -1
      then strong_connect acc state
      else acc
    end ([], (0, []))

  module Scc = Strong.Finite.Array.Of_array (struct
      type a = G.states Fin.elt list
      let table = Array.of_list sccs
    end)

  type sccs = Scc.n
  let sccs = Scc.table
  let scc : (G.states, sccs Fin.elt) Fin.Array.t =
    match Strong.Natural.order Strong.Natural.zero G.states with
    | Eq -> Fin.Array.empty
    | _ -> Fin.Array.make G.states (Fin.Elt.of_int Scc.n 0)

  let () = Fin.Array.iteri (fun index elements ->
      List.iter (fun elt -> Fin.Array.set scc elt index) elements
    ) sccs
end
