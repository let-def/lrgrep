open Utils
module Fin = Strong.Finite

let consider_lookahead = false

module Make (Sigma : Intf.SIGMA)()
: Intf.REDUCTION with module Lr1 = Sigma.Lr1
=
struct
  module Sigma = Sigma
  module Lr1 = Sigma.Lr1
  module G = Lr1.Grammar
  module LookaheadSet = BitSet.Make(G.Terminal)

  module Graph : sig
    type state =
      | Lr1 of Lr1.t
      | Goto of {
          lr1: Lr1.t;
          lookahead: LookaheadSet.t;
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
          lookahead: LookaheadSet.t;
          next: state;
        }

    type transition =
      | Targets of Lr1.Set.t list * (Lr1.t * state) list
      | Epsilon of state

    type builder =
      | Abstract of LookaheadSet.t * state
      | Concrete of LookaheadSet.t * Lr1.Set.t * Lr1.Set.t list

    let start_from lookahead state =
      if consider_lookahead then
        begin match state with
          | Lr1 _ -> ()
          | Goto t -> assert (LookaheadSet.subset lookahead t.lookahead);
        end;
      Abstract (lookahead, state)

    let pop = function
      | Abstract (lookahead, Goto g) -> Abstract (lookahead, g.next)
      | Abstract (lookahead, Lr1 lr1) ->
        Concrete (lookahead, Lr1.predecessors_of_state lr1, [])
      | Concrete (lookahead, hd, tl) ->
        Concrete (lookahead, Lr1.predecessors_of_states hd, hd :: tl)

    let lr1_state (Goto {lr1; _} | Lr1 lr1) = lr1

    let goto nt = function
      | Abstract (lookahead, next) ->
        let lr1 = Lr1.goto (lr1_state next) nt in
        Epsilon (Goto {lookahead; lr1; next})
      | Concrete (lookahead, hd, tl) ->
        let targets =
          List.filter_map (fun lr1_parent ->
                let lr1 = Lr1.goto lr1_parent nt in
                Some (lr1_parent, Goto {lookahead; lr1; next = Lr1 lr1_parent})
            )
          (Lr1.Set.elements hd)
        in
        Targets (List.rev tl, targets)

    let transitions state =
      let reducible =
        G.Lr1.reductions (lr1_state state)
        |> List.filter_map (fun (lookahead, prod) ->
            match state with
            | Goto t
              when consider_lookahead &&
                   not (LookaheadSet.mem lookahead t.lookahead) ->
              None
            | Goto _ | Lr1 _ ->
              let prod = List.hd prod in
              match G.Nonterminal.kind (G.Production.lhs prod) with
              | `START -> None
              | `REGULAR -> Some (lookahead, prod)
          )
        |> List.sort (fun (_, p1) (_, p2) -> compare p1 p2)
        |> Misc.merge_group
          ~equal:(Int.equal :> G.production -> G.production -> bool)
          ~group:(fun prod lookahead ->
              let lookahead_set =
                if consider_lookahead
                then LookaheadSet.(List.fold_right add lookahead empty)
                else LookaheadSet.empty
              in
              (prod, lookahead_set))
      in
      let reduce (prod, lookahead) =
        let builder = start_from lookahead state in
        let builder =
          Array.fold_right
            (fun _ builder -> pop builder)
            (G.Production.rhs prod)
            builder
        in
        goto (G.Production.lhs prod) builder
      in
      List.map reduce reducible
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


  (*
  let () =
    Random.self_init ();
    G.Lr1.iter (fun start ->
        let oc = Printf.ksprintf open_out "st%04d.dot" (G.Lr1.to_int start) in
        output_string oc "digraph G {\n";
        let start = Concrete.from_lr1 start in
        let count = ref 0 in
        let visited = Hashtbl.create 7 in
        let rec visit st =
          try Hashtbl.find visited st
          with Not_found ->
            let id = !count in
            incr count;
            Hashtbl.add visited st id;
            let name =
              let rec to_strings = function
                | Graph.Lr1 lr1 -> ["LR1(" ^ string_of_int (G.Lr1.to_int lr1) ^ ")"]
                | Graph.Goto {lr1; next; _} ->
                  let sym = match G.Lr0.incoming (G.Lr1.lr0 lr1) with
                    | None -> assert false
                    | Some sym -> sym
                  in
                  G.symbol_name sym :: to_strings next
              in
              let to_string = function
                | Graph.Goto _ as st -> String.concat "; " (to_strings st)
                | Graph.Lr1 lr1 ->
                  Format.asprintf "LR1(%d)\n%a"
                    (G.Lr1.to_int lr1)
                    G.Print.itemset
                    (G.Lr0.items (G.Lr1.lr0 lr1))
              in
              String.concat "\n" (List.map to_string (Concrete.represent st))
            in
            Printf.fprintf oc "  S%d[label=%S];\n" id name;
            List.iter
              (fun (_, target) -> Printf.fprintf oc "  S%d -> S%d;\n" id (visit target))
              (Concrete.transitions st);
            id
        in
        ignore (visit start);
        output_string oc "}\n";
        close_out oc
      )
     *)
end
