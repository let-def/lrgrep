open Fix.Indexing

module type DFA = sig
  type states
  val states : states cardinal
  type transitions
  val transitions : transitions cardinal

  type label
  val label  : transitions index -> label
  val source : transitions index -> states index
  val target : transitions index -> states index

end

module type INPUT = sig
  include DFA

  val initials : (states index -> unit) -> unit
  val finals : (states index -> unit) -> unit
  val refinements : ((add:(states index -> unit) -> unit) -> unit) -> unit
end

let index_transitions (type state) (type transition)
    (states : state cardinal)
    (transitions : transition cardinal)
    (target : transition index -> state index)
  : state index -> (transition index -> unit) -> unit
  =
  let f = Array.make (cardinal states + 1) 0 in
  Index.iter transitions (fun t ->
      let state = (target t :> int) in
      f.(state) <- f.(state) + 1
    );
  for i = 0 to cardinal states - 1 do
    f.(i + 1) <- f.(i + 1) + f.(i)
  done;
  let a = Array.make (cardinal transitions) (Index.of_int transitions 0) in
  Index.rev_iter transitions (fun t ->
    let state = (target t :> int) in
    let index = f.(state) - 1 in
    f.(state) <- index;
    a.(index) <- t
    );
  (fun st fn ->
     let st = (st : state index :> int) in
     for i = f.(st) to f.(st + 1) - 1 do fn a.(i) done
  )

let discard_unreachable
    (type state) (type transition)
    (blocks : state Partition.t)
    (transitions_of : state index -> (transition index -> unit) -> unit)
    (target : transition index -> state index)
  =
  Partition.iter_marked_elements blocks 0 (fun state ->
      transitions_of state
        (fun transition -> Partition.mark blocks (target transition))
    );
  Partition.discard_unmarked blocks

module Minimize
    (Label : Map.OrderedType)
    (In: INPUT with type label := Label.t) :
sig
  include DFA with type label = Label.t

  val initials : states index array
  val finals : states index array

  val transport_state :
    In.states index -> states index option
  val transport_transition :
    In.transitions index -> transitions index option

  val represent_state :
    states index -> In.states index
  val represent_transition :
    transitions index -> In.transitions index
end = struct

  let () =
    if cardinal In.transitions = 0 then
      invalid_arg "Valmari: degenerate input, no transition"

  (* State partition *)
  let blocks = Partition.create In.states

  (* Remove states unreachable from initial state *)
  let () =
    In.initials (Partition.mark blocks);
    let transitions_source =
      index_transitions In.states In.transitions In.source in
    discard_unreachable blocks transitions_source In.target

  (* Index the set of transitions targeting a state *)
  let transitions_targeting =
    index_transitions In.states In.transitions In.target

  (* Remove states which cannot reach any final state *)
  let () =
    In.finals (Partition.mark blocks);
    discard_unreachable blocks transitions_targeting In.source

  (* Split final states *)
  let () =
    In.finals (Partition.mark blocks);
    Partition.split blocks

  (* Transition partition *)
  let cords =
    let partition t1 t2 = Label.compare (In.label t1) (In.label t2) in
    Partition.create In.transitions ~partition

  let () =
    Partition.discard cords (fun t ->
        Partition.set_of blocks (In.source t) = -1 ||
        Partition.set_of blocks (In.target t) = -1
      )

  (* Main loop, split the sets *)
  let () =
    let block_set = ref 1 in
    let cord_set = ref 0 in
    let refine_blocks () =
      Partition.iter_elements cords !cord_set
        (fun transition -> Partition.mark blocks (In.source transition));
      Partition.split blocks;
    in
    let refine_cords () =
      while !block_set < Partition.set_count blocks do
        Partition.iter_elements blocks !block_set (fun state ->
            transitions_targeting state (Partition.mark cords)
          );
        Partition.split cords;
        incr block_set;
      done;
    in
    let stabilize () =
      while !cord_set < Partition.set_count cords do
        refine_blocks ();
        refine_cords ();
        incr cord_set;
      done;
    in
    stabilize ();
    (* Finally, split explicitely refined states *)
    let refine_explicit f =
      f ~add:(Partition.mark blocks);
      Partition.split blocks;
      refine_cords ();
      stabilize ();
    in
    In.refinements refine_explicit

  module States = Const(struct let cardinal = Partition.set_count blocks end)
  type states = States.n
  let states = States.n

  module Transitions = Vector.Of_array(struct
      type a = In.transitions index
      let array =
        let count = ref 0 in
        Index.iter In.transitions (fun tr ->
            if Partition.is_first blocks (In.source tr) &&
               Partition.set_of blocks (In.target tr) > -1
            then incr count
          );
        match !count with
        | 0 -> [||]
        | n -> Array.make n (Index.of_int In.transitions 0)

      let () =
        let count = ref 0 in
        Index.iter In.transitions (fun tr ->
            if Partition.is_first blocks (In.source tr) &&
               Partition.set_of blocks (In.target tr) > -1
            then (
              let index = !count in
              incr count;
              array.(index) <- tr
            )
          );
    end)
  type transitions = Transitions.n
  let transitions = Vector.length Transitions.vector

  type label = Label.t

  let transport_state_unsafe =
    let table =
      Vector.init In.states (Partition.set_of blocks)
    in
    Vector.get table

  let represent_state =
    let table =
      Vector.init states
        (fun st -> Partition.choose blocks (st : states index :> int))
    in
    Vector.get table

  let represent_transition transition =
    (Vector.get Transitions.vector transition)

  let label transition : Label.t =
    In.label (represent_transition transition)

  let source transition =
    Index.of_int states
      (transport_state_unsafe (In.source (represent_transition transition)))

  let target transition =
    Index.of_int states
      (transport_state_unsafe (In.target (represent_transition transition)))

  let initials =
    In.initials (Partition.mark blocks);
    let sets = Partition.marked_sets blocks in
    Partition.clear_marks blocks;
    Array.map (Index.of_int states) (Array.of_list sets)

  let finals =
    In.finals (Partition.mark blocks);
    let sets = Partition.marked_sets blocks in
    Partition.clear_marks blocks;
    Array.map (Index.of_int states) (Array.of_list sets)

  let transport_state state =
    match transport_state_unsafe state with
    | -1 -> None
    | n -> Some (Index.of_int states n)

  let transport_transition =
    let table = Vector.make In.transitions None in
    Vector.iteri (fun tr trin ->
        assert (Vector.get table trin = None);
        Vector.set table trin (Some tr);
      ) Transitions.vector;
    Vector.get table

end
