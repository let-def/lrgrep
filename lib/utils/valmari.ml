module Fin = Strong.Finite
module Partition = Valmari_partition

module type DFA = sig
  type states
  val states : states Fin.set
  type transitions
  val transitions : transitions Fin.set

  type label
  val label  : transitions Fin.elt -> label
  val source : transitions Fin.elt -> states Fin.elt
  val target : transitions Fin.elt -> states Fin.elt
end

module type INPUT = sig
  include DFA

  val initials : (states Fin.elt -> unit) -> unit
  val finals : (states Fin.elt -> unit) -> unit

  val refinements :
    refine:(iter:((states Fin.elt -> unit) -> unit) -> unit) -> unit
end

let index_transitions (type state) (type transition)
    (states : state Fin.set)
    (transitions : transition Fin.set)
    (target : transition Fin.elt -> state Fin.elt)
  : state Fin.elt -> (transition Fin.elt -> unit) -> unit
  =
  let f = Array.make (Fin.Set.cardinal states + 1) 0 in
  Fin.Set.iter transitions (fun t ->
      let state = (target t :> int) in
      f.(state) <- f.(state) + 1
    );
  for i = 0 to Fin.Set.cardinal states - 1 do
    f.(i + 1) <- f.(i + 1) + f.(i)
  done;
  let a = Array.make (Fin.Set.cardinal transitions)
      (Fin.Elt.of_int transitions 0)
  in
  Fin.Set.rev_iter transitions (fun t ->
    let state = (target t :> int) in
    let index = f.(state) - 1 in
    f.(state) <- index;
    a.(index) <- t
    );
  (fun st fn ->
     let st = (st : state Fin.elt :> int) in
     for i = f.(st) to f.(st + 1) - 1 do fn a.(i) done
  )

let discard_unreachable
    (type state) (type transition)
    (blocks : state Partition.t)
    (transitions_of : state Fin.elt -> (transition Fin.elt -> unit) -> unit)
    (target : transition Fin.elt -> state Fin.elt)
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

  val initials : states Fin.elt array
  val finals : states Fin.elt array

  val transport_state :
    In.states Fin.elt -> states Fin.elt option
  val transport_transition :
    In.transitions Fin.elt -> transitions Fin.elt option

  val represent_state :
    states Fin.elt -> In.states Fin.elt
  val represent_transition :
    transitions Fin.elt -> In.transitions Fin.elt
end = struct

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

  (* Split explicitely refined states *)
  let () =
    let refine ~iter =
        iter (Partition.mark blocks);
        Partition.split blocks
    in
    In.refinements ~refine

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
    while !cord_set < Partition.set_count cords do
      Partition.iter_elements cords !cord_set
        (fun transition -> Partition.mark blocks (In.source transition));
      Partition.split blocks;
      while !block_set < Partition.set_count blocks do
        Partition.iter_elements blocks !block_set (fun state ->
            transitions_targeting state (Partition.mark cords)
          );
        Partition.split cords;
        incr block_set;
      done;
      incr cord_set;
    done

  module States =
    Strong.Natural.Nth(struct let n = Partition.set_count blocks end)
  type states = States.n
  let states = States.n

  let oc = open_out_bin "raw.dot"

  let () =
    (*let mo = ref 0 in*)
    Fin.Set.iter In.transitions
      (fun t ->
         if Partition.is_first blocks (In.source t) &&
            Partition.set_of blocks (In.target t) > -1
         then
           Printf.fprintf oc "  %d - #%s -> %d\n"
             (Partition.set_of blocks (In.source t))
             (Digest.to_hex (Digest.string (Marshal.to_string (In.label t) [])))
             (Partition.set_of blocks (In.target t))
      )
    (*let fo = ref 0 in
    for i = 0 to Partition.set_count blocks = 1 do
      if

    done*)

  let () = close_out oc

  let oc = open_out_bin "minimized.dot"

  module Transitions = Fin.Array.Of_array(struct
      type a = In.transitions Fin.elt
      let table =
        match Partition.set_count cords with
        | 0 -> [||]
        | count ->
          let count' = ref 0 in
          for i = 0 to count - 1 do
            match Partition.choose_opt cords i with
            | None -> ()
            | Some elt ->
              if Partition.set_of blocks (In.target elt) > -1 then
                incr count'
              else
                Partition.iter_elements cords i (fun elt' ->
                    assert (Partition.set_of blocks (In.target elt') = -1))
          done;
          let table = Array.make !count' (Partition.choose cords 0) in
          let count' = ref 0 in
          for i = 0 to count - 1 do
            match Partition.choose_opt cords i with
            | None -> ()
            | Some elt ->
              if Partition.set_of blocks (In.target elt) > -1 then (
                Printf.fprintf oc "cord %d mapped to out transition %d\n" i !count';
                if i = 610 then (
                  Printf.fprintf oc "representative is in transition in%d: (%d -> %d) = (%d -> %d)\n"
                    (elt :> int)
                    (In.source elt :> int)
                    (In.target elt :> int)
                    (Partition.set_of blocks (In.source elt))
                    (Partition.set_of blocks (In.target elt))
                );
                table.(!count') <- elt;
                incr count';
              )
          done;
          table
    end)
  type transitions = Transitions.n
  let transitions = Transitions.n

  type label = Label.t

  let transport_state_unsafe =
    let table =
      Fin.Array.init In.states (Partition.set_of blocks)
    in
    Fin.Array.get table

  let represent_state =
    let table =
      Fin.Array.init states
        (fun st -> Partition.choose blocks (st : states Fin.elt :> int))
    in
    Fin.Array.get table

  let represent_transition transition =
    Fin.(Transitions.table.(transition))

  let label transition : Label.t =
    In.label (represent_transition transition)

  let source transition =
    Fin.Elt.of_int states
      (transport_state_unsafe (In.source (represent_transition transition)))

  let target transition =
    Fin.Elt.of_int states
      (transport_state_unsafe (In.target (represent_transition transition)))

  let initials =
    In.initials (Partition.mark blocks);
    let sets = Partition.marked_sets blocks in
    Partition.clear_marks blocks;
    Array.map (Fin.Elt.of_int states) (Array.of_list sets)

  let finals =
    In.finals (Partition.mark blocks);
    let sets = Partition.marked_sets blocks in
    Partition.clear_marks blocks;
    Array.map (Fin.Elt.of_int states) (Array.of_list sets)

  let transport_state state =
    match transport_state_unsafe state with
    | -1 -> None
    | n -> Some (Fin.Elt.of_int states n)

  let transport_transition =
    let table = Fin.Array.make In.transitions None in
    Fin.Array.iteri (fun tr trin ->
        assert (Fin.Array.get table trin = None);
        Fin.Array.set table trin (Some tr);
      ) Transitions.table;
    Fin.Array.get table

  let () =
    output_string oc "digraph {\n";
    Fin.Set.iter In.transitions (fun tr ->
        match
          transport_state (In.source tr),
          transport_state (In.target tr)
        with
        | Some src, Some dst ->
          (*begin match transport_transition tr with
            | None ->
              Printf.fprintf oc
                "# ERROR: (%d -> %d) is missing transportation to (st%d -> st%d)\n"
                (In.source tr :> int) (In.target tr :> int)
                (src :> int) (dst :> int)
              ;
            | Some tr' ->
              let src' = source tr' and dst' = target tr' in
              if src' <> src || dst' <> dst then
              Printf.fprintf oc
                "# ERROR: (%d -> %d) incorrectly transported to \
                 (st%d -> st%d) rather than (st%d -> st%d)\n"
                (In.source tr :> int) (In.target tr :> int)
                (src' :> int) (dst' :> int)
                (src :> int) (dst :> int)
          end;*)
          if src = Fin.Elt.of_int states 345 then (
            let set = Partition.set_of cords tr in
            let repr = Partition.choose cords set in
            Printf.fprintf oc
              "# Transition in%d with source 345 belongs to cord %d, \
               with representative in%d targetting block %d\n"
              (tr :> int) set (repr :> int)
              (Partition.set_of blocks (In.target repr))
            ;
          );
          Printf.fprintf oc "  st%d -> st%d [label=\"%d -> %d\"]\n"
            (src :> int) (dst :> int)
            (In.source tr :> int) (In.target tr :> int);
        | None, _ | _, None -> ()
      );
    output_string oc "}\n";
    Printf.fprintf oc "# is 345 final? %b\n" (Array.exists ((=) (Fin.Elt.of_int states 345)) finals);
    close_out oc

end
