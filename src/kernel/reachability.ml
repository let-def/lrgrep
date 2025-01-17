(******************************************************************************)
(*                                                                            *)
(*                                Reachability                                *)
(*                                                                            *)
(* Copyright (c) 2025 Frédéric Bour                                           *)
(*                                                                            *)
(* Permission is hereby granted, free of charge, to any person obtaining a    *)
(* copy of this software and associated documentation files (the "Software"), *)
(* to deal in the Software without restriction, including without limitation  *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,   *)
(* and/or sell copies of the Software, and to permit persons to whom the      *)
(* Software is furnished to do so, subject to the following conditions:       *)
(*                                                                            *)
(* The above copyright notice and this permission notice shall be included in   *)
(* all copies or substantial portions of the Software.                        *)
(*                                                                            *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *)
(* DEALINGS IN THE SOFTWARE.                                                  *)
(*                                                                            *)
(******************************************************************************)

(** This module computes the reachability of states in a parser automaton. It is
    used to reason about the behavior of an LR(1) automaton after conflict
    resolution (with some transitions removed).
    The module implements algorithms for partitioning lookahead symbols with
    identical behaviors, and uses these partitions to determine the cost of
    reaching each state with a given lookahead. *)

open Fix.Indexing
open Utils
open Misc

module type S = sig
  module Info : Info.S
  open Info

  module Classes : sig
    (* Returns the classes of terminals for a given goto transition *)
    val for_edge : Transition.goto index -> Terminal.set array

    (* Returns the classes of terminals for a given LR(1) state *)
    val for_lr1 : Lr1.t -> Terminal.set array

    (* Returns the classes of terminals before taking a transition *)
    val pre_transition : Transition.any index -> Terminal.set array

    (* Returns the classes of terminals after taking a transition *)
    val post_transition : Transition.any index -> Terminal.set array
  end

  module Tree : sig
    include CARDINAL

    (* Returns the leaf node corresponding to a given transition *)
    val leaf : Transition.any index -> n index

    (* Splits a node into its left and right children if it is an inner node *)
    val split : n index -> (Transition.any index, n index * n index) either

    (* Returns the nullable terminals and non-nullable equations for a given goto transition *)
    val goto_equations : Transition.goto index -> Terminal.set * (n index * Terminal.set) list

    (* Returns the pre-classes for a given node *)
    val pre_classes : n index -> Terminal.set array

    (* Returns the post-classes for a given node *)
    val post_classes : n index -> Terminal.set array
  end

  module Cells : sig
    type t = private int
    type offset = int
    type row = int
    type column = int

    (* The table that stores all compact cost matrices *)
    val table : (Tree.n, int array) vector

    (* Total number of cells *)
    val count : int

    (* Cost of a cell *)
    val cost : t -> int

    (* Compute an index in a table *)
    val table_index : post_classes:int -> pre:int -> post:int -> int

    (* Compute a linear offset from a node, a row, and a column *)
    val offset : Tree.n index -> row -> column -> offset

    (* Get the cell corresponding to a node and offset *)
    val encode_offset : Tree.n index -> offset -> t

    (* Get the node and offset corresponding to a cell *)
    val decode_offset : t -> Tree.n index * offset

    (* Get the cell corresponding to a node, a row, and a column *)
    val encode : Tree.n index -> row -> column -> t

    (* Get the node, row, and column corresponding to a cell *)
    val decode : t -> Tree.n index * row * column
  end

  module Coercion : sig
    type pre = Pre_identity | Pre_singleton of int

    (* Compute the pre coercion from a partition of the form P = first(cost(s, A))
       to a partition of the form Q = first(ccost(𝑠, 𝐴 → 𝜖•𝛼)))
    *)
    val pre : 'a indexset array -> 'a indexset array -> pre option

    type forward = int array array
    type backward = int array
    type infix = { forward : forward; backward : backward; }

    (* Compute the infix coercion from two partitions P Q such that Q <= P *)
    val infix : ?lookahead:'a indexset -> 'a indexset array -> 'a indexset array -> infix
  end
end

module Make (Info : Info.S)() : S with module Info := Info =
struct
  let time = Stopwatch.enter Stopwatch.main "Reachability"

  open Info

  (* ---------------------------------------------------------------------- *)

  (* Useful definitions *)

  (* Testing class inclusion *)
  let quick_subset = IndexSet.quick_subset

  (* ---------------------------------------------------------------------- *)

  (* Compute the inverse of the reduction relation.
     It lists the different reductions that lead to following a goto
     transition, reversing the effect of a single reduction.

     It serves the same purpose as the [reduce(𝑠, 𝐴 → 𝛼)] function from the
     paper but is more convenient for the rest of the implementation.
  *)
  module Unreduce : sig

    type t = {
      (* The production that is being reduced *)
      production: Production.t;

      (* The set of lookahead terminals that allow this reduction to happen *)
      lookahead: Terminal.set;

      (* The shape of the stack, all the transitions that are replaced by the
         goto transition when the reduction is performed *)
      steps: Transition.any index list;

      (* The lr1 state at the top of the stack before reducing.
         That is [state] can reduce [production] when the lookahead terminal
         is in [lookahead]. *)
      state: Lr1.t;
    }

    (* [goto_transition tr] lists all the reductions that ends up
       following [tr]. *)
    val goto_transition: Transition.goto index -> t list
  end = struct

    type t = {
      production: Production.t;
      lookahead: Terminal.set;
      steps: Transition.any index list;
      state: Lr1.t;
    }

    let table = Vector.make Transition.goto []

    (* [add_reduction lr1 (production, lookahead)] populates [table] by
       simulating the reduction [production], starting from [lr1] when
       lookahead is in [lookahead] *)
    let add_reduction lr1 (production, lookahead) =
      let production = Production.of_g production in
      if Production.kind production = `REGULAR then begin
        let lhs = Production.lhs production in
        let rhs = Production.rhs production in
        let states =
          Array.fold_right (fun _ states ->
              let expand acc (state, steps) =
                List.fold_left (fun acc tr ->
                    (Transition.source tr, tr :: steps) :: acc
                  ) acc (Transition.predecessors state)
              in
              List.fold_left expand [] states
            ) rhs [lr1, []]
        in
        List.iter (fun (source, steps) ->
            Vector.set_cons table (Transition.find_goto source lhs)
              { production; lookahead; steps; state=lr1 }
          ) states
      end

    let lr1_reductions = Grammar.Lr1.get_reductions

    let has_default_reduction lr1 =
      match Grammar.Lr1.transitions lr1 with
      | _ :: _ -> None
      | [] ->
        match lr1_reductions lr1 with
        | [] -> None
        | (_, p) :: ps when List.for_all (fun (_, p') -> p' = p) ps ->
          Some p
        | _ -> None

    (* [get_reductions lr1] returns the list of productions and the lookahead
       sets that allow reducing them from state [lr1] *)
    let get_reductions lr1 =
      let lr1 = Lr1.to_g lr1 in
      match has_default_reduction lr1 with
      | Some prod ->
        (* State has a default reduction, the lookahead can be any terminal *)
        [prod, Terminal.all]
      | None ->
        let raw =
          let add acc (t, p) =
            match Grammar.Terminal.kind t with
            | `ERROR -> acc
            | _ -> ((t, p) :: acc)
          in
          List.fold_left add [] (lr1_reductions lr1)
        in
        (* Regroup lookahead tokens by production *)
        Utils.Misc.group_by raw
          ~compare:(fun (_, p1) (_, p2) ->
              compare_index (Production.of_g p1) (Production.of_g p2)
            )
          ~group:(fun (t, p) tps ->
              let set = List.fold_left
                  (fun set (t, _) -> IndexSet.add (Terminal.of_g t) set)
                  (IndexSet.singleton (Terminal.of_g t)) tps
              in
              (p, set)
            )

    let () =
      (* Populate [table] with the reductions of all state *)
      Index.iter Lr1.n
        (fun lr1 -> List.iter (add_reduction lr1) (get_reductions lr1))

    let goto_transition tr = Vector.get table tr
  end

  (* ---------------------------------------------------------------------- *)

  (* Compute classes refinement.

     This implements section 6.2, Approximating first and follow Partitions.

     This algorithm computes a partition of tokens for each transition.
     The partition is a bit finer than necessary, but the approximation is
     still sound: merging the rows or columns of the matrices based on token
     classes gives a correct result.
  *)

  module Classes = struct

    (* A node of the graph is either an lr1 state or a goto transition *)
    module Node = (val sum Lr1.n Transition.goto)

    (* Represents the dependency graph of Equation 7-9, to compute the SCCs *)
    module Gr = struct
      type node = Node.n index
      let n = cardinal Node.n

      let index = Index.to_int

      let visit_lr1 f lr1 =
        match Lr1.incoming lr1 with
        | Some sym when Symbol.is_nonterminal sym ->
          List.iter (fun tr ->
              match Transition.split tr with
              | L nt -> f (Node.inj_r nt)
              | R _ -> assert false
            )
            (Transition.predecessors lr1)
        | _ -> ()

      let successors f i =
        match Node.prj i with
        | L lr1 -> visit_lr1 f lr1
        | R e -> List.iter
                   (fun {Unreduce. state; _} -> f (Node.inj_l state))
                   (Unreduce.goto_transition e)

      let iter f = Index.iter Node.n f
    end

    module Scc = Tarjan.Run(Gr)

    (* Associate a class to each node *)

    let classes = Vector.make Node.n []

    (* Evaluate classes for a node, directly computing equation 4-6.
       (compute follow for a goto node, first for an lr1 node)

       [classes] vector is used to approximate recursive occurrences.
    *)
    let classes_of acc node =
      let acc = ref acc in
      begin match Node.prj node with
        | L lr1 ->
          Gr.visit_lr1 (fun n -> acc := Vector.get classes n @ !acc) lr1
        | R edge ->
          List.iter (fun {Unreduce. lookahead; state; _} ->
              let base = Vector.get classes (Node.inj_l state) in
              (* Comment the code below to have a partial order on partitions
                 (remove the ↑Z in equation (6) *)
              let base =
                if lookahead != Terminal.all
                then List.map (IndexSet.inter lookahead) base
                else base
              in
              (* Stop commenting here *)
              acc := (lookahead :: base) @ !acc
            ) (Unreduce.goto_transition edge)
      end;
      !acc

    let visit_scc _ nodes =
      (* Compute approximation for an SCC, as described in section 6.2 *)
      let coarse_classes =
        IndexRefine.partition (List.fold_left classes_of [] nodes)
      in
      match nodes with
      | [node] -> Vector.set classes node coarse_classes
      | nodes ->
        List.iter begin fun node ->
          match Node.prj node with
          | L _ -> ()
          | R e ->
            let coarse = ref IndexSet.empty in
            List.iter
              (fun {Unreduce. lookahead; _} ->
                 coarse := IndexSet.union lookahead !coarse)
              (Unreduce.goto_transition e);
            Vector.set classes node (
              coarse_classes
              |> List.map (IndexSet.inter !coarse)
              |> IndexRefine.partition
            )
        end nodes;
        List.iter begin fun node ->
          match Node.prj node with
          | R _ -> ()
          | L lr1 ->
            let acc = ref [] in
            Gr.visit_lr1 (fun n -> acc := Vector.get classes n @ !acc) lr1;
            Vector.set classes node (IndexRefine.partition !acc)
        end nodes

    let () = Scc.rev_topological_iter visit_scc

    (* Initialize classes of initial states and of states whose incoming
       symbol is a terminal *)
    let () = Index.iter Lr1.n (fun lr1 ->
        match Lr1.incoming lr1 with
        | Some sym when Symbol.is_nonterminal sym -> ()
        | None | Some _ ->
          Vector.set classes (Node.inj_l lr1) [Terminal.all]
      )

    (* We now have the final approximation.
       Classes will be identified and accessed by their index,
       random access is important.
    *)
    let classes = Vector.map Array.of_list classes

    let for_edge nte =
      Vector.get classes (Node.inj_r nte)

    let for_lr1 st =
      Vector.get classes (Node.inj_l st)

    (* Precompute the singleton partitions, e.g. { {t}, T/{t} } for each t *)
    let t_singletons =
      Vector.init Terminal.n (fun t -> [|IndexSet.singleton t|])

    let all_terminals =
      [|Terminal.all|]

    (* Just before taking a transition [tr], the lookahead has to belong to
       one of the classes in [pre_transition tr].

       [pre_transition tr] indexes the rows of cost matrix for [tr].
    *)
    let pre_transition tr =
      match Symbol.desc (Transition.symbol tr) with
      | T t -> Vector.get t_singletons t
      | N _ -> for_lr1 (Transition.source tr)

    (* Just after taking a transition [tr], the lookahead has to belong to
       one of the classes in [post_transition tr].

       [post_transition tr] indexes the columns of cost matrix for [tr].
    *)
    let post_transition tr =
      match Transition.split tr with
      | L edge -> for_edge edge
      | R _ -> all_terminals
  end

  (* ---------------------------------------------------------------------- *)

  (* We now construct the DAG (as a tree with hash-consing) of all matrix
     products.

     Each occurrence of [ccost(s,x)] is mapped to a leaf.
     Occurrences of [(ccost(𝑠, 𝐴 → 𝛼•𝑥𝛽)] are mapped to inner nodes, except
     that the chain of multiplication are re-associated.
  *)
  module ConsedTree () : sig
    (* The finite set of nodes of the tree.
       The set is not frozen yet: as long as its cardinal has not been
       observed, new nodes can be added. *)
    include CARDINAL

    (* The set of inner nodes *)
    module Inner : CARDINAL

    (* [leaf tr] returns the node that corresponds [cost(s,x)]
       where [s = source tr] and [x = symbol tr]. *)
    val leaf : Transition.any index -> n index

    (* [node l r] returns the inner-node that corresponds to
       the matrix product [l * r]  *)
    val node : n index -> n index -> n index

    (* Get the tree node that corresponds to an inner node *)
    val inject : Inner.n index -> n index

    (* Determines whether a node is a leaf or an inner node *)
    val split : n index -> (Transition.any index, Inner.n index) either

    (* Once all nodes have been added, the DAG needs to be frozen *)
    module FreezeTree() : sig
      val define : Inner.n index -> n index * n index
    end
  end = struct
    (* The fresh finite set of all inner nodes *)
    module Inner = Gensym()
    (* The nodes of the trees is the disjoint sum of all transitions
       (the leaves) and the inner nodes. *)
    include (val sum Transition.any Inner.n)

    let leaf = inj_l
    let inject = inj_r
    let split = prj

    (* An inner node is made of the index of its left and right children *)
    type pack = n index * n index
    let pack t u = (t, u)
    let unpack x = x

    (* The node table is used to give a unique index to each inner node *)
    let node_table : (pack, Inner.n index) Hashtbl.t = Hashtbl.create 7

    (* Returns the index of an inner node, or allocate one for a new node *)
    let node l r =
      let p = pack l r in
      let node_index =
        try Hashtbl.find node_table p
        with Not_found ->
          let i = Inner.fresh () in
          Hashtbl.add node_table p i;
          i
      in
      inj_r node_index

    (* When all nodes have been created, the set of nodes can be frozen.
       A reverse index is created to get the children of an inner node. *)
    module FreezeTree() =
    struct
      let rev_index = Vector.make' Inner.n
          (fun () -> let dummy = Index.of_int n 0 in (dummy, dummy))

      let define ix = Vector.get rev_index ix

      let () =
        Hashtbl.iter
          (fun pair index -> Vector.set rev_index index (unpack pair))
          node_table
    end
  end

  (* ---------------------------------------------------------------------- *)

  (* The hash-consed tree of all matrix equations (products and minimums). *)
  module Tree = struct
    include ConsedTree()

    let goto_equations =
      (* Explicit representation of the rhs of equation (7).
         This equation defines ccost(𝑠, 𝐴) as the minimum of a set of
         sub-matrices.

         Matrices of the form [creduce(𝑠, 𝐴 → 𝛼)] are represented by a
         [TerminalSet.t], following section 6.5.

         [goto_equations] are represented as pair [(nullable, non_nullable)]
         such that, for each sub-equation [ccost(𝑠, 𝐴→𝜖•𝛼) · creduce(𝑠, 𝐴→𝛼)]:
         - if [𝛼 = 𝜖] (an empty production can reduce A),
           [nullable] contains the terminals [creduce(𝑠, 𝐴 → 𝛼)]
         - otherwise,
           [non_nullable] contains the pair [ccost(𝑠, 𝐴→𝜖•𝛼)], [creduce(𝑠, 𝐴→𝛼)}
      *)
      tabulate_finset Transition.goto @@ fun tr ->
      (* Number of rows in the compact cost matrix for tr *)
      let first_dim =
        Array.length (Classes.pre_transition (Transition.of_goto tr))
      in
      (* Number of columns in the compact cost matrix for a transition tr' *)
      let transition_size tr' =
        Array.length (Classes.post_transition tr')
      in
      (* Import the solution to a matrix-chain ordering problem as a sub-tree *)
      let rec import_mcop = function
        | Mcop.Matrix l -> leaf l
        | Mcop.Product (l, r) -> node (import_mcop l) (import_mcop r)
      in
      (* Compute the nullable terminal set and non_nullable list for a single
         reduction, optimizing the matrix-product chain.  *)
      let solve_ccost_path {Unreduce. steps; lookahead; _} =
        let dimensions = first_dim :: List.map transition_size steps in
        match Mcop.dynamic_solution (Array.of_list dimensions) with
        | exception Mcop.Empty -> Either.Left lookahead
        | solution ->
          let steps = Array.of_list steps in
          let solution = Mcop.map_solution (fun i -> steps.(i)) solution in
          Either.Right (import_mcop solution, lookahead)
      in
      let nullable, non_nullable =
        List.partition_map solve_ccost_path (Unreduce.goto_transition tr)
      in
      (List.fold_left IndexSet.union IndexSet.empty nullable, non_nullable)

    include FreezeTree()

    (* Pre-compute classes before (pre) and after (post) a node *)

    let table_pre = Vector.make Inner.n [||]
    let table_post = Vector.make Inner.n [||]

    let pre_classes t = match split t with
      | L tr -> Classes.pre_transition tr
      | R ix -> Vector.get table_pre ix

    let post_classes t = match split t with
      | L tr -> Classes.post_transition tr
      | R ix -> Vector.get table_post ix

    let () =
      (* Nodes are allocated in topological order.
         When iterating over all nodes, children are visited before parents. *)
      Index.iter Inner.n @@ fun node ->
      let l, r = define node in
      Vector.set table_pre node (pre_classes l);
      Vector.set table_post node (post_classes r)

    let split i = match split i with
      | L _ as result -> result
      | R n -> R (define n)

  end

  (* ---------------------------------------------------------------------- *)

  (* Representation of matrix cells, the variables of the data flow problem.
     There will be a lot of them. Actually, on large grammars, most of the
     memory is consumed by cost matrices.

     Therefore we want a rather compact encoding.
     We use a two-level encoding:
     - first the [table] vector maps a node index to a "compact cost matrix"
     - each "compact cost matrix" is represented as a 1-dimensional array of
       integers, of dimension |pre_classes n| * |post_classes n|

     This module defines conversion functions between three different
     representations of cells:

     [Cells.t] identify a cell as a single integer
     <=>
     [Tree.n index * Cells.offset] identify a cell as a pair of a node and an
     offset in the array of costs
     <=>
     [Tree.n index * Cells.row * Cells.column] identify a cell as a triple of
     a node, a row index and a column index of the compact cost matrix
  *)
  module Cells : sig

    (* The table that stores all compact cost matrices.
       [Vector.get table n] is the matrix of node [n], represented as a linear
       array of length |pre(n)| * |post(n)|.
    *)
    val table : (Tree.n, int array) vector

    (* A value of type t identifies a single cell. It is an integer that
       encodes the node index and the offset of the cell inside the matrix of
       that node.
    *)
    type t = private int

    (* A value of type offset represents an index of a compact cost matrix.
       An offset of node [n] belongs to the interval
         0 .. Array.length (Tree.pre_classes n) *
              Array.length (Tree.post_classes n) - 1
    *)
    type offset = int

    (* A value of type row represents the index of a row of a matrix.
       A row of node [n] belongs to the interval
         0 .. Array.length (Tree.pre_classes n) - 1
    *)
    type row = int

    (* A value of type column represents the index of a column of a matrix.
       A column of node [n] belongs to the interval
         0 .. Array.length (Tree.post_classes n) - 1
    *)
    type column = int

    (* Total number of cells *)
    val count : int

    (* Cost of a cell (initialized to max_int representing +inf, updated by the
       DataFlow solver). *)
    val cost : t -> int

    (* Compute an index in a table *)
    val table_index : post_classes:int -> pre:int -> post:int -> int

    (* Compute a linear offset from a node a row and a column *)
    val offset : Tree.n index -> row -> column -> offset

    (* Get the cell corresponding to a node and offset *)
    val encode_offset : Tree.n index -> offset -> t

    (* Get the node and offset corresponding to a cell *)
    val decode_offset : t -> Tree.n index * offset

    (* Get the cell corresponding to a node, a row and a column *)
    val encode : Tree.n index -> row -> column -> t

    (* Get the cell corresponding to a node, a row and a column *)
    val decode : t -> Tree.n index * row * column

  end = struct
    type t = int
    type offset = int
    type row = int
    type column = int

    let count = ref 0

    let table =
      let init_node node =
        let node_count =
          Array.length (Tree.pre_classes node) *
          Array.length (Tree.post_classes node)
        in
        count := !count + node_count;
        Array.make node_count max_int
      in
      Vector.init Tree.n init_node

    let count = !count

    (* Determine how many bits are needed to represent a node index.
       A cell is then represented as [(offset lsl shift) lor (node)]. *)
    let shift =
      let rec loop i =
        if cardinal Tree.n <= 1 lsl i
        then i
        else loop (i + 1)
      in
      loop 0

    let encode_offset node cell =
      Index.to_int node lor (cell lsl shift)

    let table_index ~post_classes ~pre ~post =
      pre * post_classes + post

    let offset node =
      let post_classes = Array.length (Tree.post_classes node) in
      fun pre post -> table_index ~post_classes ~pre ~post

    let encode node =
      let offset_of = offset node in
      fun i_pre i_post -> encode_offset node (offset_of i_pre i_post)

    let decode_offset i =
      let node = Index.of_int Tree.n (i land (1 lsl shift - 1)) in
      (node, i lsr shift)

    let decode i =
      let node, offset = decode_offset i in
      let sz = Array.length (Tree.post_classes node) in
      (node, offset / sz, offset mod sz)

    (* Sanity checks

      let decode i =
        let n, b, a as result = decode i in
        assert (i = encode n b a);
        result

      and encode node i_pre i_post =
        let result = encode node i_pre i_post in
        assert (decode result = (node, i_pre, i_post));
        result
    *)

    let cost t =
      let node, offset = decode_offset t in
      (Vector.get table node).(offset)
  end

  (* ---------------------------------------------------------------------- *)

  (* This module implements efficient representations of the coerce matrices,
     as mentioned in section 6.5.

     However, our implementation has one more optimization.
     In general, we omit the last block of a partition (it can still be deduced
     by removing the other blocks from the universe T, see section 6.1).  The
     block that is omitted is one that is guaranteed to have infinite cost in
     the compact cost matrix.
     Therefore, we never need to represent the rows and columns that correspond
     to the missing class; by construction we know they have infinite cost.
     For instance for shift transitions, it means we only have a 1x1 matrix:
     the two classes are the terminal being shifted, with a cost of one, and
     its complement, with an infinite cost, that is omitted.

     Our coercion functions are augmented to handle this special case.
  *)
  module Coercion = struct

    (* Pre coercions are used to handle the minimum in equation (7):
       ccost(𝑠, 𝐴 → 𝜖•𝛼) · creduce(𝑠, 𝐴 → 𝛼)

       If 𝛼 begins with a terminal, it will have only one class.
       This is handled by the [Pre_singleton x] constructor that indicates that
       this only class should be coerced to class [x].

       If 𝛼 begins with a non-terminal, [Pre_identity] is used: ccost(𝑠, 𝐴) and
       ccost(𝑠, 𝐴 → 𝜖•𝛼) are guaranteed to have the same "first" classes.
    *)
    type pre =
      | Pre_identity
      | Pre_singleton of int

    (* Compute the pre coercion from a partition of the form
         P = first(cost(s, A))
       to a partition of the form
         Q = first(ccost(𝑠, 𝐴 → 𝜖•𝛼)))

       If 𝛼 starts with a terminal, we look only for the
    *)
    let pre outer inner =
      if outer == inner then
        Some Pre_identity
      else (
        assert (Array.length inner = 1);
        assert (IndexSet.is_singleton inner.(0));
        let t = IndexSet.choose inner.(0) in
        match Utils.Misc.array_findi (fun _ ts -> IndexSet.mem t ts) 0 outer with
        | i -> Some (Pre_singleton i)
        | exception Not_found ->
          (* If the production that starts with the 'inner' partition cannot be
             reduced (because of conflict resolution), the transition becomes
             unreachable and the terminal `t` might belong to no classes.
          *)
          None
      )

    (* The type infix is the general representation for the coercion matrices
       coerce(𝑃, 𝑄) appearing in 𝑀1 · coerce(𝑃, 𝑄) · 𝑀2

       Since Q is finer than P, a class of P maps to multiple classes of Q.
       This is represented by the forward array: a class p in P maps to all
       classes q in array [forward.(p)].

       The other direction is an injection: a class q in Q maps to class
       [backward.(q)] in P.

       The special class [-1] represents a class that is not mapped in the
       partition (this occurs for instance when using creduce to filter a
       partition).
    *)
    type forward = int array array
    type backward = int array
    type infix = { forward: forward; backward: backward }

    (* Compute the infix coercion from two partitions P Q such that Q <= P.

       The optional [lookahead] argument is used to filter classes outside of a
       certain set of terminals, exactly like the ↓ operator on partitions.
       This is used to implement creduce operator.
    *)
    let infix ?lookahead pre_classes post_classes =
      let forward_size = Array.make (Array.length pre_classes) 0 in
      let backward =
        Array.map (fun ca ->
            let keep = match lookahead with
              | None -> true
              | Some la -> quick_subset ca la
            in
            if keep then (
              match
                Utils.Misc.array_findi
                  (fun _ cb -> quick_subset ca cb) 0 pre_classes
              with
              | exception Not_found -> -1
              | i -> forward_size.(i) <- 1 + forward_size.(i); i
            ) else (-1)
          ) post_classes
      in
      let forward = Array.map (fun sz -> Array.make sz 0) forward_size in
      Array.iteri (fun i_pre i_f ->
          if i_f <> -1 then (
            let pos = forward_size.(i_f) - 1 in
            forward_size.(i_f) <- pos;
            forward.(i_f).(pos) <- i_pre
          )
        ) backward;
      { forward; backward }
  end

  (* ---------------------------------------------------------------------- *)

  (* Represent the data flow problem to solve *)
  module Solver = struct
    let min_cost a b : int =
      if a < b then a else b

    (* Reverse dependencies record in which equations a node appears *)
    type reverse_dependency =
      (* Equation (7): this node appears in the RHS of the definition of a
         goto transition.
         The dependency is accompanied with pre-coercion (see [Coercion.pre])
         and the forward coercion that represents the creduce(...). *)
      | Leaf of Transition.goto index * Coercion.pre * Coercion.forward

      (* Equation (8): this node appears in some inner product.
         The dependency stores the index of the parent node as well as the
         coercion matrix. *)
      | Inner of Tree.Inner.n index * Coercion.infix

    let dependents : (Tree.n, reverse_dependency list) vector =
      (* Store enough information with each node of the tree to compute
         which cells are affected if a cell of this node changes.

         Because of sharing, a node can have multiple parents.
      *)
      Vector.make Tree.n []

    (* No need to record dependencies on a shift transition: its cost is
       constant.  However we initialize it to 1. *)
    let record_shift ~visit_root tr =
      let node = Tree.leaf (Transition.of_shift tr) in
      (*sanity*)assert (Array.length (Tree.pre_classes node) = 1);
      (*sanity*)assert (Array.length (Tree.post_classes node) = 1);
      visit_root (Cells.encode_offset node 0) 1

    (* Record dependencies on a goto transition.  *)
    let record_goto ~visit_root tr =
      let node = Tree.leaf (Transition.of_goto tr) in
      let pre = Tree.pre_classes node in
      let post = Tree.post_classes node in
      let nullable, non_nullable = Tree.goto_equations tr in
      (* Set matrix cells corresponding to nullable reductions to 0 *)
      if not (IndexSet.is_empty nullable) then (
        let offset_of = Cells.offset node in
        (* We use:
           - [c_pre] and [i_pre] for a class in the pre partition and its index
           - [c_post] and [i_post] for a class in the post partition and its
             index
        *)
        let update_cell i_post c_post i_pre c_pre =
          if not (IndexSet.disjoint c_pre c_post) then
            visit_root (Cells.encode_offset node (offset_of i_pre i_post)) 0
        in
        let update_col i_post c_post =
          if quick_subset c_post nullable then
            Array.iteri (update_cell i_post c_post) pre
        in
        Array.iteri update_col post
      );
      (* Register dependencies to other reductions *)
      List.iter begin fun (node', lookahead) ->
        match Coercion.pre pre (Tree.pre_classes node') with
        | None ->
          (* The goto transition is unreachable because of conflict resolution.
             Don't register any dependency. *)
          ()
        | Some coerce_pre ->
          let post' = Tree.post_classes node' in
          let coerce_post = Coercion.infix post' post ~lookahead in
          Vector.set_cons dependents node'
            (Leaf (tr, coerce_pre, coerce_post.Coercion.forward))
      end non_nullable

    (* Record dependencies on a inner node. *)
    let record_inner node =
      let (l, r) = Tree.define node in
      (*(*sanity*)assert (Tree.pre_classes l == Tree.pre_classes node);*)
      (*(*sanity*)assert (Tree.post_classes r == Tree.post_classes node);*)
      let c1 = Tree.post_classes l in
      let c2 = Tree.pre_classes r in
      let coercion = Coercion.infix c1 c2 in
      let dep = Inner (node, coercion) in
      assert (Array.length c2 = Array.length coercion.Coercion.backward);
      Vector.set_cons dependents l dep;
      Vector.set_cons dependents r dep

    (* A graph representation suitable for the DataFlow solver *)
    module Graph = struct
      type variable = Cells.t

      (* We cheat a bit. Normally a root is either the cell corresponding to a
         shift transition (initialized to 1) or the cells corresponding to the
         nullable reductions of a goto transitions (initialized to 0).

         Rather than duplicating the code for exactly computing those cells, we
         visit all transitions and consider every non-infinite cell a root.
      *)
      let foreach_root visit_root =
        (* Visit all nodes:
           - call [visit_root] on roots
           - populate the [dependents] vector for inner nodes.
        *)
        Index.iter Transition.shift (record_shift ~visit_root);
        Index.iter Transition.goto (record_goto ~visit_root);
        Index.iter Tree.Inner.n record_inner

      (* Visit all the successors of a cell.
         This amounts to:
         - finding the node the cell belongs to
         - looking at the reverse dependencies of this node
         - visiting all cells that are affected in the dependencies
      *)
      let foreach_successor index cost f =
        (* The cost has to be less than the maximum otherwise there is no point
           in relaxing the node.
           This guarantees that the additions below do not overflow. *)
        assert (cost < max_int);
        let node, i_pre, i_post = Cells.decode index in
        let update_dep = function
          | Leaf (parent, pre, post) ->
            (* This change might improve one of the cost(s,x) *)
            let parent = Tree.leaf (Transition.of_goto parent) in
            (* If the production begins with a terminal,
               we have to map the class *)
            let i_pre' = match pre with
              | Coercion.Pre_singleton i -> i
              | Coercion.Pre_identity -> i_pre
            in
            let parent_index = Cells.encode parent in
            Array.iter
              (fun i_post' -> f (parent_index i_pre' i_post') cost)
              post.(i_post)
          | Inner (parent, inner) ->
            (* This change updates the cost of an occurrence of equation 8,
               of the form l . coercion . r
               We have to find whether the change comes from the [l] or the [r]
               node to update the right-hand cells of the parent *)
            let l, r = Tree.define parent in
            let parent_index = Cells.encode (Tree.inject parent) in
            if l = node then (
              (* The left term has been updated *)
              let r_costs = Vector.get Cells.table r in
              let offset_of = Cells.offset r in
              for i_post' = 0 to Array.length (Tree.post_classes r) - 1 do
                let r_cost = Array.fold_left
                    (fun r_cost i_pre' ->
                       min_cost r_cost r_costs.(offset_of i_pre' i_post'))
                    max_int inner.Coercion.forward.(i_post)
                in
                if r_cost < max_int then (
                  f (parent_index i_pre i_post') (cost + r_cost)
                )
              done
            ) else (
              (* The right term has been updated *)
              (*sanity*)assert (r = node);
              match inner.Coercion.backward.(i_pre) with
              | -1 -> ()
              | l_post ->
                let l_costs = Vector.get Cells.table l in
                let offset_of = Cells.offset l in
                for i_pre = 0 to Array.length (Tree.pre_classes l) - 1 do
                  let l_cost = l_costs.(offset_of i_pre l_post) in
                  if l_cost < max_int then (
                    f (parent_index i_pre i_post) (l_cost + cost)
                  )
                done
            )
        in
        List.iter update_dep (Vector.get dependents node)
    end

    module Property = struct
      type property = int
      let leq_join = min_cost
    end

    (* Implement the interfaces required by DataFlow.ForCustomMaps *)

    module CostMap = struct
      (* Access the cost table using cells *)
      let get index =
        let node, offset = Cells.decode_offset index in
        (Vector.get Cells.table node).(offset)

      let set index v =
        let node, offset = Cells.decode_offset index in
        (Vector.get Cells.table node).(offset) <- v
    end

    module MarkMap = struct
      (* Associate a boolean value to each cell.
         For efficiency, we use a "bytes" for each node, each cell
         corresponding to a single byte.
         We could instead use bits, encoding 8 cells per byte, but the gain is
         negligible.
         Memory use is anyway dominated by the cost_table, so in the best case
         this optimization would reduce memory consumption by 1/9, and this
         does not translate to any observable performance improvement.
      *)
      let data = Vector.map
          (fun costs -> Bytes.make (Array.length costs) '\x00')
          Cells.table

      let get var =
        let node, cell = Cells.decode_offset var in
        Bytes.get (Vector.get data node) cell <> '\x00'

      let set var value =
        let node, cell = Cells.decode_offset var in
        Bytes.set (Vector.get data node) cell
          (if value then '\x01' else '\x00')
    end

    (* Run the solver *)
    include Fix.DataFlow.ForCustomMaps(Property)(Graph)(CostMap)(MarkMap)
  end

  let () = Stopwatch.leave time
end
