open Utils
module Fin = Strong.Finite

module Make (Sigma : Intf.SIGMA)()
: Intf.REDUCTION with module Lr1 = Sigma.Lr1
=
struct
  module Sigma = Sigma
  module Lr1 = Sigma.Lr1
  module G = Lr1.Grammar

  module Graph : sig
    type state =
      | Lr1 of Lr1.t
      | Goto of {
          lr1: Lr1.t;
          next: state;
        }

    type transition =
      | Targets of Lr1.Set.t list * (Lr1.t * state) list
      | Epsilon of state

    val transitions : state -> transition list
  end = struct

    type state =
      | Lr1 of Lr1.t
      | Goto of {
          lr1: Lr1.t;
          next: state;
        }

    type transition =
      | Targets of Lr1.Set.t list * (Lr1.t * state) list
      | Epsilon of state

    type builder =
      | Abstract of state
      | Concrete of Lr1.Set.t * Lr1.Set.t list

    let start_from state = Abstract state

    let pop = function
      | Abstract (Goto g) -> Abstract g.next
      | Abstract (Lr1 lr1) -> Concrete (Lr1.predecessors_of_state lr1, [])
      | Concrete (hd, tl) -> Concrete (Lr1.predecessors_of_states hd, hd :: tl)

    let lr1_state (Goto {lr1; _} | Lr1 lr1) = lr1

    let goto nt = function
      | Abstract next ->
        let lr1 = Lr1.goto (lr1_state next) nt in
        Epsilon (Goto {lr1; next})
      | Concrete (hd, tl) ->
        let targets =
          List.filter_map (fun lr1_parent ->
              match G.Lr0.incoming (G.Lr1.lr0 lr1_parent) with
              | None ->
                (* We reached an initial state: goto transition doesn't exist,
                   it is an accepting state. *)
                None
              | Some _ ->
                let lr1 = Lr1.goto lr1_parent nt in
                Some (lr1_parent, Goto {lr1; next = Lr1 lr1_parent})
            )
          (Lr1.Set.elements hd)
        in
        Targets (List.rev tl, targets)

    module ProdSet = BitSet.Make(G.Production)

    let transitions state =
      let reducible =
        List.fold_left (fun acc (_, prod) -> ProdSet.add (List.hd prod) acc)
          ProdSet.empty
          (G.Lr1.reductions (lr1_state state))
      in
      let reduce prod =
        let builder = start_from state in
        let builder =
          Array.fold_right
            (fun _ builder -> pop builder)
            (G.Production.rhs prod)
            builder
        in
        goto (G.Production.lhs prod) builder
      in
      ProdSet.fold (fun prod transitions -> reduce prod :: transitions)
        reducible []
  end

  module Concrete : sig
    module States : Fin.Set.T
    type state = States.n Fin.elt
    module Set : BitSet.S with type element = state

    type transition = Lr1.Set.t * state

    val from_lr1 : Lr1.t -> state
    val transitions : state -> transition list
    val represent : state -> Graph.state list
  end = struct
    module Genstate = Fin.Set.Gensym()

    type state = Genstate.n Fin.elt

    type transition = Lr1.Set.t * state

    let visited
      : (Graph.state, Genstate.n Fin.elt) Hashtbl.t
      = Hashtbl.create 7

    let number_and_todo state =
      match Hashtbl.find visited state with
      | id -> id, false
      | exception Not_found ->
        let id = Genstate.fresh () in
        Hashtbl.add visited state id;
        id, true

    let all_states = ref []

    let register_state id transitions repr =
      all_states := (id, transitions, repr) :: !all_states

    let register_intermediate transitions =
      let id = Genstate.fresh () in
      all_states := (id, transitions, []) :: !all_states;
      id

    let rec close_transitions direct =
      let reprs = ref [] and transitions = ref [] in
      let rec aux = function
        | Graph.Epsilon state ->
          reprs := state :: !reprs;
          List.iter aux (Graph.transitions state)
        | Graph.Targets (intermediates, targets) ->
          transitions :=
            visit_intermediates intermediates targets @ !transitions
      in
      List.iter aux direct;
      !reprs, !transitions

    and visit_intermediates intermediates targets =
      match intermediates with
      | [] ->
        let visit_target (lr1, target) = Lr1.Set.singleton lr1, visit target in
        List.map visit_target targets
      | lr1s :: lr1ss ->
        let id = register_intermediate (visit_intermediates lr1ss targets) in
        [lr1s, id]

    and visit state =
      let id, todo = number_and_todo state in
      if todo then begin
        let states, transitions =
          close_transitions (Graph.transitions state) in
        register_state id transitions (state :: states)
      end;
      id

    let from_lr1 = G.Lr1.tabulate (fun lr1 -> visit (Graph.Lr1 lr1))

    module States = struct
      type n = Genstate.n
      let n = Genstate.freeze ()
    end

    module Set = BitSet.Make(struct
        type t = States.n Fin.elt
        let of_int x = Fin.Elt.of_int States.n x
      end)

    let table = Fin.Array.make States.n ([], [])

    let () =
      List.iter
        (fun (id, transitions, state) ->
           Fin.(table.(id) <- (transitions, state)))
        !all_states;
      all_states := []

    let transitions id = fst Fin.(table.(id))
    let represent id = snd Fin.(table.(id))
  end

  module Derivation :
  sig
    type n
    type t = n Fin.elt

    type 'a derivations
    val derive :
      step:(G.lr1 -> 'a -> 'a) ->
      finish:(G.lr1 -> 'a -> 'b) -> 'a -> 'b derivations
    val get : 'a derivations -> t -> 'a

    module Set : BitSet.S with type element = t

    val filter : ('a -> bool) -> 'a derivations -> Set.t
    val reachable : Concrete.state -> Set.t
    val reached : Concrete.state -> Set.t
  end =
  struct
    include Fin.Set.Gensym()

    type t = n Fin.elt

    type trie = {
      mutable derivations: (G.lr1 * t) list;
      mutable next: (G.lr1 * trie) list;
    }

    let root = {derivations = []; next = []}

    let rec lookup trie = function
      | Graph.Lr1 lr1 ->
        begin
          try List.assoc lr1 trie.derivations
          with Not_found ->
            let id = fresh () in
            trie.derivations <- (lr1, id) :: trie.derivations;
            id
        end
      | Graph.Goto {lr1; next; _} ->
        let trie' =
          try List.assoc lr1 trie.next
          with Not_found ->
            let trie' = {derivations = []; next = []} in
            trie.next <- (lr1, trie') :: trie.next;
            trie'
        in
        lookup trie' next

    let register nts = ignore (lookup root nts : _ Fin.elt)
    let () =
      let process st = List.iter register (Concrete.represent st) in
      Fin.Set.iter Concrete.States.n process

    let n = freeze ()

    type 'a derivations = (n, 'a option) Fin.Array.t

    let derive ~step ~finish init =
      let result = Fin.Array.make n None in
      let rec traverse acc {derivations; next} =
        List.iter (fun (l, t) -> Fin.(result.(t) <- Some (finish l acc))) derivations;
        List.iter (fun (nt, trie) -> traverse (step nt acc) trie) next
      in
      traverse init root;
      result

    let get tbl t = match Fin.(tbl.(t)) with
      | None -> assert false
      | Some x -> x

    module Set = BitSet.Make(struct
        type t = n Fin.elt
        let of_int = Fin.Elt.of_int n
      end)

    let filter f ds =
      let set = ref Set.empty in
      Fin.Array.iteri
        (fun idx d -> if f (Option.get d) then set := Set.add idx !set) ds;
      !set

    let reached = Fin.Array.init Concrete.States.n (fun cst ->
        List.fold_left
          (fun acc gst -> Set.add (lookup root gst) acc)
          Set.empty (Concrete.represent cst)
      )

    let reachable = Fin.Array.copy reached

    module Scc = Scc.Make(struct
        type states = Concrete.States.n
        let states = Concrete.States.n
        let successors st f acc =
          List.fold_left begin fun acc (_, st) ->
            f st acc
          end acc (Concrete.transitions st)
      end)

    let () =
      Fin.Array.rev_iter begin fun scc ->
        let add_state acc st = Set.union acc Fin.(reachable.(st)) in
        let closure = List.fold_left begin fun acc st ->
            let acc = add_state acc st in
            let add_tr acc (_, st) = add_state acc st in
            let acc = List.fold_left add_tr acc (Concrete.transitions st) in
            acc
          end Set.empty scc
        in
        List.iter (fun st -> Fin.(reachable.(st) <- closure)) scc
      end Scc.sccs

    let reached st = Fin.(reached.(st))
    let reachable st = Fin.(reachable.(st))
  end

end
