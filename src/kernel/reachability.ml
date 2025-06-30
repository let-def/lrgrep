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
open Info

module type S = sig
  type g

  type reduction = {
    (* The production that is being reduced *)
    production: g production index;

    (* The set of lookahead terminals that allow this reduction to happen *)
    lookahead: g terminal indexset;

    (* The shape of the stack, all the transitions that are replaced by the
       goto transition when the reduction is performed *)
    steps: g transition index list;

    (* The lr1 state at the top of the stack before reducing.
       That is [state] can reduce [production] when the lookahead terminal
       is in [lookahead]. *)
    state: g lr1 index;
  }

  (* [unreduce tr] lists all the reductions that ends up following [tr]. *)
  val unreduce : g goto_transition index -> reduction list

  module Classes : sig
    (* Returns the classes of terminals for a given goto transition *)
    val for_edge : g goto_transition index -> g terminal indexset array

    (* Returns the classes of terminals for a given LR(1) state *)
    val for_lr1 : g lr1 index -> g terminal indexset array

    (* Returns the classes of terminals before taking a transition *)
    val pre_transition : g transition index -> g terminal indexset array

    (* Returns the classes of terminals after taking a transition *)
    val post_transition : g transition index -> g terminal indexset array
  end

  module Coercion : sig
    type pre = Pre_identity | Pre_singleton of int

    (* Compute the pre coercion from a partition of the form
         P = first(cost(s, A))
       to a partition of the form
         Q = first(ccost(s, A → ϵ•α)))
    *)
    val pre : 'a indexset array -> 'a indexset array -> pre option

    type forward = int array array
    type backward = int array
    type infix = { forward : forward; backward : backward; }

    (* Compute the infix coercion from two partitions P Q such that Q <= P *)
    val infix : ?lookahead:'a indexset -> 'a indexset array -> 'a indexset array -> infix
  end

  module Tree : sig
    include CARDINAL

    (* Returns the leaf node corresponding to a given transition *)
    val leaf : g transition index -> n index

    (* Splits a node into its left and right children if it is an inner node *)
    val split : n index -> (g transition index, n index * n index) either

    (* Returns the nullable terminals and non-nullable equations for a given goto transition *)
    type equations = {
      nullable_lookaheads: g terminal indexset;
      nullable: reduction list;
      non_nullable: (reduction * n index) list;
    }
    val goto_equations : g goto_transition index -> equations

    (* Returns the pre-classes for a given node *)
    val pre_classes : n index -> g terminal indexset array

    (* Returns the post-classes for a given node *)
    val post_classes : n index -> g terminal indexset array
  end

  (* Identify each cell of compact cost matrices.
     A [Cell.n index] can be thought of as a triple made of a tree node and two indices
     (row, col) of the compact cost matrix associated to the node. *)
  module Cell : sig
    include CARDINAL

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

    (* Get the cell corresponding to a node, a row, and a column *)
    val encode : Tree.n index -> pre:row -> post:column -> n index

    (* Get the node, row, and column corresponding to a cell *)
    val decode : n index -> Tree.n index * row * column

    type goto
    val goto : goto cardinal
    val is_goto : n index -> goto index option
    val of_goto : goto index -> n index
    val goto_encode : g goto_transition index -> pre:row -> post:column -> goto index
    val goto_decode : goto index -> g goto_transition index * row * column
    val iter_goto : g goto_transition index -> (goto index -> unit) -> unit
  end

  module Analysis : sig
    val cost : Cell.n index -> int
    val finite : Cell.n index -> bool
  end
end

type 'g t = (module S with type g = 'g)

let make (type g) (g : g grammar) : g t = (module struct
  type nonrec g = g

  (* ---------------------------------------------------------------------- *)

  (* Useful definitions *)

  (* Testing class inclusion *)
  let quick_subset = IndexSet.quick_subset

  (* ---------------------------------------------------------------------- *)

  (* Compute the inverse of the reduction relation.
     It lists the different reductions that lead to following a goto
     transition, reversing the effect of a single reduction.

     It serves the same purpose as the [reduce(s, A → α)] function from the
     paper but is more convenient for the rest of the implementation.
  *)

  type reduction = {
    (* The production that is being reduced *)
    production: g production index;

    (* The set of lookahead terminals that allow this reduction to happen *)
    lookahead: g terminal indexset;

    (* The shape of the stack, all the transitions that are replaced by the
       goto transition when the reduction is performed *)
    steps: g transition index list;

    (* The lr1 state at the top of the stack before reducing.
       That is [state] can reduce [production] when the lookahead terminal
       is in [lookahead]. *)
    state: g lr1 index;
  }

  (* [unreduce tr] lists all the reductions that ends up following [tr]. *)
  let unreduce : g goto_transition index -> reduction list =
    let table = Vector.make (Transition.goto g) [] in
    (* [add_reduction lr1 (production, lookahead)] populates [table] by
       simulating the reduction [production], starting from [lr1] when
       lookahead is in [lookahead] *)
    let add_reduction lr1 (production, lookahead) =
      if Production.kind g production = `REGULAR then begin
        let lhs = Production.lhs g production in
        let rhs = Production.rhs g production in
        let states =
          Array.fold_right (fun _ states ->
              let expand acc (state, steps) =
                IndexSet.fold (fun tr acc ->
                    (Transition.source g tr, tr :: steps) :: acc
                  ) (Transition.predecessors g state) acc
              in
              List.fold_left expand [] states
            ) rhs [lr1, []]
        in
        List.iter (fun (source, steps) ->
            table.@(Transition.find_goto g source lhs) <-
              List.cons { production; lookahead; steps; state=lr1 }
          ) states
      end
    in
    (* [get_reductions lr1] returns the list of productions and the lookahead
       sets that allow reducing them from state [lr1] *)
    let get_reductions lr1 =
      match Lr1.default_reduction g lr1 with
      | Some prod ->
        (* State has a default reduction, the lookahead can be any terminal *)
        [prod, Terminal.all g]
      | None ->
        IndexSet.fold
          (fun red acc -> (Reduction.production g red, Reduction.lookaheads g red) :: acc)
          (Reduction.from_lr1 g lr1) []
    in
    (* Populate [table] with the reductions of all state *)
    Index.iter (Lr1.cardinal g)
      (fun lr1 -> List.iter (add_reduction lr1) (get_reductions lr1));
    Vector.get table

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
    module Node = (val Sum.make (Lr1.cardinal g) (Transition.goto g))

    (* Represents the dependency graph of Equation 7-9, to compute the SCCs *)
    module Gr = struct
      type node = Node.n index
      let n = cardinal Node.n

      let index = Index.to_int

      let visit_lr1 f lr1 =
        match Lr1.incoming g lr1 with
        | Some sym when Symbol.is_nonterminal g sym ->
          IndexSet.iter (fun tr ->
              match Transition.split g tr with
              | L nt -> f (Node.inj_r nt)
              | R _ -> assert false
            )
            (Transition.predecessors g lr1)
        | _ -> ()

      let successors f i =
        match Node.prj i with
        | L lr1 -> visit_lr1 f lr1
        | R e -> List.iter
                   (fun {state; _} -> f (Node.inj_l state))
                   (unreduce e)

      let iter f = Index.iter Node.n f
    end

    module Scc = Tarjan.Run(Gr)

    (* Associate a class to each node *)

    let classes = Vector.make Node.n IndexSet.Set.empty

    (* Evaluate classes for a node, directly computing equation 4-6.
       (compute follow for a goto node, first for an lr1 node)

       [classes] vector is used to approximate recursive occurrences.
    *)
    let classes_of acc node =
      let acc = ref acc in
      begin match Node.prj node with
        | L lr1 ->
          Gr.visit_lr1 (fun n -> acc := IndexSet.Set.union classes.:(n) !acc) lr1
        | R edge ->
          List.iter (fun {lookahead; state; _} ->
              let base = classes.:(Node.inj_l state) in
              (* Comment the code below to have a partial order on partitions
                 (remove the ↑Z in equation (6) *)
              let base =
                if lookahead != Terminal.all g
                then IndexSet.Set.map (IndexSet.inter lookahead) base
                else base
              in
              (* Stop commenting here *)
              acc := IndexSet.Set.union (IndexSet.Set.add lookahead base) !acc
            ) (unreduce edge)
      end;
      !acc

    let partition_sets sets =
      sets
      |> IndexSet.Set.elements
      |> IndexRefine.partition
      |> IndexSet.Set.of_list

    let visit_scc _ nodes =
      (* Compute approximation for an SCC, as described in section 6.2 *)
      let coarse_classes =
        partition_sets (List.fold_left classes_of IndexSet.Set.empty nodes)
      in
      match nodes with
      | [node] -> classes.:(node) <- coarse_classes
      | nodes ->
        List.iter begin fun node ->
          match Node.prj node with
          | L _ -> ()
          | R e ->
            let coarse = ref IndexSet.empty in
            List.iter
              (fun {lookahead; _} ->
                 coarse := IndexSet.union lookahead !coarse)
              (unreduce e);
            classes.:(node) <-
              partition_sets (IndexSet.Set.map (IndexSet.inter !coarse) coarse_classes)
        end nodes;
        List.iter begin fun node ->
          match Node.prj node with
          | R _ -> ()
          | L lr1 ->
            let acc = ref IndexSet.Set.empty in
            Gr.visit_lr1 (fun n -> acc := IndexSet.Set.union classes.:(n) !acc) lr1;
            classes.:(node) <- partition_sets !acc
        end nodes

    let () = Scc.rev_topological_iter visit_scc

    (* Initialize classes of initial states and of states whose incoming
       symbol is a terminal *)
    let () = Index.iter (Lr1.cardinal g) (fun lr1 ->
        match Lr1.incoming g lr1 with
        | Some sym when Symbol.is_nonterminal g sym -> ()
        | None | Some _ ->
          classes.:(Node.inj_l lr1) <- IndexSet.Set.singleton (Terminal.all g)
      )

    (* We now have the final approximation.
       Classes will be identified and accessed by their index,
       random access is important.
    *)
    let classes =
      let prepare l =
        let a = Array.of_seq (IndexSet.Set.to_seq l) in
        Array.sort IndexSet.compare_minimum a;
        a
      in
      Vector.map prepare classes

    let for_edge nte =
      classes.:(Node.inj_r nte)

    let for_lr1 st =
      classes.:(Node.inj_l st)

    (* Precompute the singleton partitions, e.g. { {t}, T/{t} } for each t *)
    let t_singletons =
      Vector.init (Terminal.cardinal g) (fun t -> [|IndexSet.singleton t|])

    let all_terminals =
      [|Terminal.all g|]

    (* Just before taking a transition [tr], the lookahead has to belong to
       one of the classes in [pre_transition tr].

       [pre_transition tr] indexes the rows of cost matrix for [tr].
    *)
    let pre_transition tr =
      match Transition.split g tr with
      | L _goto -> for_lr1 (Transition.source g tr)
      | R shift -> t_singletons.:(Transition.shift_symbol g shift)

    (* Just after taking a transition [tr], the lookahead has to belong to
       one of the classes in [post_transition tr].

       [post_transition tr] indexes the columns of cost matrix for [tr].
    *)
    let post_transition tr =
      match Transition.split g tr with
      | L edge -> for_edge edge
      | R _ -> all_terminals
  end

  let () = stopwatch 2 "reachability: computed classes"

  (* ---------------------------------------------------------------------- *)

  (* We now construct the DAG (as a tree with hash-consing) of all matrix
     products.

     Each occurrence of [ccost(s,x)] is mapped to a leaf.
     Occurrences of [(ccost(s, A → α•xβ)] are mapped to inner nodes, except
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
    val leaf : g transition index -> n index

    (* [node l r] returns the inner-node that corresponds to
       the matrix product [l * r]  *)
    val node : n index -> n index -> n index

    (* Get the tree node that corresponds to an inner node *)
    val inject : Inner.n index -> n index

    (* Determines whether a node is a leaf or an inner node *)
    val split : n index -> (g transition index, Inner.n index) either

    (* Once all nodes have been added, the DAG needs to be frozen *)
    module FreezeTree() : sig
      val define : Inner.n index -> n index * n index
    end
  end = struct
    (* The fresh finite set of all inner nodes *)
    module Inner = Gensym()
    (* The nodes of the trees is the disjoint sum of all transitions
       (the leaves) and the inner nodes. *)
    include (val Sum.make (Transition.any g) Inner.n)

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

      let define ix = rev_index.:(ix)

      let () =
        Hashtbl.iter
          (fun pair index -> rev_index.:(index) <- unpack pair)
          node_table
    end
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
       ccost(s, A → ϵ•α) · creduce(s, A → α)

       If α begins with a terminal, it will have only one class.
       This is handled by the [Pre_singleton x] constructor that indicates that
       this only class should be coerced to class [x].

       If α begins with a non-terminal, [Pre_identity] is used: ccost(s, A) and
       ccost(s, A → ϵ•α) are guaranteed to have the same "first" classes.
    *)
    type pre =
      | Pre_identity
      | Pre_singleton of int

    (* Compute the pre coercion from a partition of the form
         P = first(cost(s, A))
       to a partition of the form
         Q = first(ccost(s, A → ϵ•α)))

       If α starts with a terminal, we look only for the
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
       coerce(P, Q) appearing in M1 · coerce(P, Q) · M2

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

  (* The hash-consed tree of all matrix equations (products and minimums). *)
  module Tree = struct
    include ConsedTree()

    type equations = {
      nullable_lookaheads: g terminal indexset;
      nullable: reduction list;
      non_nullable: (reduction * n index) list;
    }

    let goto_equations =
      (* Explicit representation of the rhs of equation (7).
         This equation defines ccost(s, A) as the minimum of a set of
         sub-matrices.

         Matrices of the form [creduce(s, A → α)] are represented by a
         [TerminalSet.t], following section 6.5.

         [goto_equations] are represented as pair [(nullable, non_nullable)]
         such that, for each sub-equation [ccost(s, A→ϵ•α) · creduce(s, A→α)]:
         - if [α = ϵ] (an empty production can reduce A),
           [nullable] contains the terminals [creduce(s, A → α)]
         - otherwise,
           [non_nullable] contains the pair [ccost(s, A→ϵ•α)], [creduce(s, A→α)}
      *)
      tabulate_finset (Transition.goto g) @@ fun tr ->
      (* Number of rows in the compact cost matrix for tr *)
      let first_dim =
        Array.length (Classes.pre_transition (Transition.of_goto g tr))
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
      let solve_ccost_path red =
        let dimensions = first_dim :: List.map transition_size red.steps in
        match Mcop.dynamic_solution (Array.of_list dimensions) with
        | exception Mcop.Empty -> Either.Left red
        | solution ->
          let steps = Array.of_list red.steps in
          let solution = Mcop.map_solution (fun i -> steps.(i)) solution in
          Either.Right (red, import_mcop solution)
      in
      let nullable, non_nullable =
        List.partition_map solve_ccost_path (unreduce tr)
      in
      {
        nullable_lookaheads =
          List.fold_left (fun set red -> IndexSet.union red.lookahead set)
            IndexSet.empty nullable;
        nullable;
        non_nullable;
      }

    include FreezeTree()

    (* Pre-compute classes before (pre) and after (post) a node *)

    let table_pre = Vector.make Inner.n [||]
    let table_post = Vector.make Inner.n [||]

    let pre_classes t = match split t with
      | L tr -> Classes.pre_transition tr
      | R ix -> table_pre.:(ix)

    let post_classes t = match split t with
      | L tr -> Classes.post_transition tr
      | R ix -> table_post.:(ix)

    let pre_count t = Array.length (pre_classes t)
    let post_count t = Array.length (post_classes t)

    let () =
      (* Nodes are allocated in topological order.
         When iterating over all nodes, children are visited before parents. *)
      Index.iter Inner.n @@ fun node ->
      let l, r = define node in
      table_pre.:(node) <- pre_classes l;
      table_post.:(node) <- post_classes r

    let split i = match split i with
      | L _ as result -> result
      | R n -> R (define n)
  end

  let () = stopwatch 2 "reachability: constructed tree"

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

     [Cell.t] identify a cell as a single integer
     <=>
     [Tree.n index * Cells.offset] identify a cell as a pair of a node and an
     offset in the array of costs
     <=>
     [Tree.n index * Cells.row * Cells.column] identify a cell as a triple of
     a node, a row index and a column index of the compact cost matrix
  *)
  module Cell : sig
    include CARDINAL

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

    (* Get the cell corresponding to a node, a row, and a column *)
    val encode : Tree.n index -> pre:row -> post:column -> n index

    (* Get the node, row, and column corresponding to a cell *)
    val decode : n index -> Tree.n index * row * column

    (* Index of the first cell of matrix associated to a node *)
    val first_cell : Tree.n index -> n index

    type goto
    val goto : goto cardinal
    val is_goto : n index -> goto index option
    val of_goto : goto index -> n index
    val goto_encode : g goto_transition index -> pre:row -> post:column -> goto index
    val goto_decode : goto index -> g goto_transition index * row * column
    val iter_goto : g goto_transition index -> (goto index -> unit) -> unit
  end = struct
    type row = int
    type column = int

    let n, pre_bits, post_bits =
      let max_pre = ref 0 in
      let max_post = ref 0 in
      let n = ref 0 in
      let bits_needed n =
        let i = ref 0 in
        while 1 lsl !i <= n
        do incr i; done;
        !i
      in
      Index.iter Tree.n begin fun node ->
        let pre = Tree.pre_count node in
        let post = Tree.post_count node in
        n := !n + pre * post;
        max_pre := Int.max pre !max_pre;
        max_post := Int.max post !max_post;
      end;
      (!n, bits_needed !max_pre, bits_needed !max_post)

    include Const(struct let cardinal = n end)

    let mapping = Vector.make n 0

    let first_cell =
      let index = ref 0 in
      Vector.init Tree.n @@ fun node ->
      let first_index = !index in
      let base = (node :> int) lsl (pre_bits + post_bits) in
      let pre_count = Tree.pre_count node in
      let post_count = Tree.post_count node in
      for pre = 0 to pre_count - 1 do
        let base = base lor (pre lsl post_bits) in
        for post = 0 to post_count - 1 do
          mapping.:(Index.of_int n !index) <- base lor post;
          incr index
        done
      done;
      first_index

    let decode ix =
      let i = mapping.:(ix) in
      let post = i land (1 lsl post_bits - 1) in
      let i = i lsr post_bits in
      let pre = i land (1 lsl pre_bits - 1) in
      (Index.of_int Tree.n (i lsr pre_bits), pre, post)

    let encode i =
      let first = first_cell.:(i) in
      let post_count = Tree.post_count i in
      fun ~pre ~post ->
        Index.of_int n (first + pre * post_count + post)

    let first_goto_node, first_goto_cell, last_goto_cell =
      match cardinal (Transition.goto g) with
      | 0 -> (0, 0, -1)
      | n ->
        let tr i = Transition.of_goto g (Index.of_int (Transition.goto g) i) in
        let first_goto_node = Tree.leaf (tr 0) in
        let first = first_cell.:(first_goto_node) in
        let last = Tree.leaf (tr (n - 1)) in
        let next = Index.of_int Tree.n ((last :> int) + 1) in
        ((first_goto_node :> int), first, first_cell.:(next) - 1)

    module Goto = Const(struct
        let cardinal = last_goto_cell - first_goto_cell + 1
      end)

    type goto = Goto.n
    let goto = Goto.n

    let is_goto (i : n index) =
      let i = (i :> int) in
      if first_goto_cell <= i && i <= last_goto_cell
      then Some (Index.of_int goto (i - first_goto_cell))
      else None

    let of_goto (g : goto index) =
      Index.of_int n (first_goto_cell + (g :> int))

    let goto_decode (gt : goto index) =
      let n, pre, post = decode (of_goto gt) in
      let gt = Index.of_int (Transition.goto g) ((n :> int) - first_goto_cell) in
      (gt, pre, post)

    let goto_encode i =
      let node = Tree.leaf (Transition.of_goto g i) in
      let first = first_cell.:(node) - first_goto_cell in
      let post_count = Tree.post_count node in
      fun ~pre ~post ->
        Index.of_int goto (first + pre * post_count + post)

    let iter_goto (gt : g goto_transition index) f =
      let i = (gt :> int) in
      let index_of i = first_cell.:(Index.of_int Tree.n (first_goto_node + i)) in
      for j = index_of i to index_of (i + 1) - 1 do
        f (Index.of_int goto (j - first_goto_cell))
      done

    let first_cell i = Index.of_int n first_cell.:(i)
  end

  let () = stopwatch 2 "reachability: indexed matrix cells"

  module Reverse_dependencies = struct

    (* Reverse dependencies record in which equations a node appears *)
    type t =
      (* Equation (7): this node appears in the RHS of the definition of a
         goto transition.
         The dependency is accompanied with pre-coercion (see [Coercion.pre])
         and the forward coercion that represents the creduce(...). *)
      | Leaf of g goto_transition index * Coercion.pre * Coercion.forward

      (* Equation (8): this node appears in some inner product.
         The dependency stores the index of the parent node as well as the
         coercion matrix. *)
      | Inner of Tree.Inner.n index * Coercion.infix

    let occurrences : (Tree.n, t list) vector =
      (* Store enough information with each node of the tree to compute
         which cells are affected if a cell of this node changes.

         Because of sharing, a node can have multiple parents. *)
      Vector.make Tree.n []

    let () =
      Index.iter (Transition.goto g) begin fun tr ->
        (* Record dependencies of a goto transition.  *)
        let node = Tree.leaf (Transition.of_goto g tr) in
        let pre = Tree.pre_classes node in
        let post = Tree.post_classes node in
        (* Register dependencies to other reductions *)
        List.iter begin fun ({lookahead; _}, node') ->
          match Coercion.pre pre (Tree.pre_classes node') with
          | None ->
            (* The goto transition is unreachable because of conflict
               resolution.  Don't register any dependency. *)
            ()
          | Some coerce_pre ->
            let post' = Tree.post_classes node' in
            let coerce_post = Coercion.infix post' post ~lookahead in
            occurrences.@(node') <-
              List.cons (Leaf (tr, coerce_pre, coerce_post.Coercion.forward))
        end (Tree.goto_equations tr).non_nullable
      end;
      (* Record dependencies on a inner node. *)
      Index.iter Tree.Inner.n begin fun node ->
        let (l, r) = Tree.define node in
        (*(*sanity*)assert (Tree.pre_classes l == Tree.pre_classes node);*)
        (*(*sanity*)assert (Tree.post_classes r == Tree.post_classes node);*)
        let c1 = Tree.post_classes l in
        let c2 = Tree.pre_classes r in
        let coercion = Coercion.infix c1 c2 in
        let dep = Inner (node, coercion) in
        assert (Array.length c2 = Array.length coercion.Coercion.backward);
        occurrences.@(l) <- List.cons dep;
        occurrences.@(r) <- List.cons dep
      end

    let visit_occurrences index
        ~visit_goto
        ~from_left ~acc ~acc_right
        ~from_right
      =
      let node, i_pre, i_post = Cell.decode index in
      let update_dep = function
        | Leaf (parent, pre, post) ->
          (* If the production begins with a terminal,
             we have to map the class *)
          let i_pre' = match pre with
            | Coercion.Pre_singleton i -> i
            | Coercion.Pre_identity -> i_pre
          in
          let encode = Cell.goto_encode parent in
          Array.iter (fun i_post' -> visit_goto (encode ~pre:i_pre' ~post:i_post'))
            post.(i_post)
        | Inner (parent, inner) ->
          (* This change updates the cost of an occurrence of equation 8,
             of the form l . coercion . r
             We have to find whether the change comes from the [l] or the [r]
             node to update the right-hand cells of the parent *)
          let l, r = Tree.define parent in
          let encode_p = Cell.encode (Tree.inject parent) in
          if l = node then (
            (* The left term has been updated *)
            let encode_r = Cell.encode r in
            for i_post' = 0 to Array.length (Tree.post_classes r) - 1 do
              let acc =
                Array.fold_left
                  (fun acc i_pre' ->
                     acc_right acc (encode_r ~pre:i_pre' ~post:i_post'))
                  acc inner.Coercion.forward.(i_post)
              in
              from_left
                ~right:acc
                ~parent:(encode_p ~pre:i_pre ~post:i_post')
            done
          ) else (
            (* The right term has been updated *)
            (*sanity*)assert (r = node);
            match inner.Coercion.backward.(i_pre) with
            | -1 -> ()
            | l_post ->
              let encode_l = Cell.encode l in
              for i_pre = 0 to Array.length (Tree.pre_classes l) - 1 do
                from_right
                  ~left:(encode_l ~pre:i_pre ~post:l_post)
                  ~parent:(encode_p ~pre:i_pre ~post:i_post)
              done
          )
      in
      List.iter update_dep occurrences.:(node)
  end

  let () = stopwatch 2 "reachability: reversed matrix dependencies"

  (* ---------------------------------------------------------------------- *)

  (* Represent the data flow problem to solve *)
  module Solver = struct
    let min_cost a b : int =
      if a < b then a else b

    (* Initialize shift transitions to cost 1. *)
    let initialize_shift ~visit_root tr =
      let node = Tree.leaf (Transition.of_shift g tr) in
      (*sanity*)assert (Array.length (Tree.pre_classes node) = 1);
      (*sanity*)assert (Array.length (Tree.post_classes node) = 1);
      visit_root (Cell.first_cell node) 1

    (* Record dependencies on a goto transition.  *)
    let initialize_goto ~visit_root tr =
      let node = Tree.leaf (Transition.of_goto g tr) in
      let eqn = Tree.goto_equations tr in
      (* Set matrix cells corresponding to nullable reductions to 0 *)
      if not (IndexSet.is_empty eqn.nullable_lookaheads) then (
        let pre = Tree.pre_classes node in
        let post = Tree.post_classes node in
        (* We use:
           - [c_pre] and [i_pre] for a class in the pre partition and its index
           - [c_post] and [i_post] for a class in the post partition and its
             index
        *)
        let encode = Cell.encode node in
        let update_cell i_post c_post i_pre c_pre =
          if not (IndexSet.disjoint c_pre c_post) then
            visit_root (encode ~pre:i_pre ~post:i_post) 0
        in
        let update_col i_post c_post =
          if quick_subset c_post eqn.nullable_lookaheads then
            Array.iteri (update_cell i_post c_post) pre
        in
        Array.iteri update_col post
      )

    let costs = Vector.make Cell.n max_int

    (* A graph representation suitable for the DataFlow solver *)
    module Graph = struct
      type variable = Cell.n index

      (* We cheat a bit. Normally a root is either the cell corresponding to a
         shift transition (initialized to 1) or the cells corresponding to the
         nullable reductions of a goto transitions (initialized to 0).

         Rather than duplicating the code for exactly computing those cells, we
         visit all transitions and consider every non-infinite cell a root.
      *)
      let foreach_root visit_root =
        (* Populate roots:
           - shift transitions have cost 1 by definition
           - nullable goto transitions have cost 0
        *)
        Index.iter (Transition.shift g) (initialize_shift ~visit_root);
        Index.iter (Transition.goto g) (initialize_goto ~visit_root)

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
        Reverse_dependencies.visit_occurrences index
          ~visit_goto:(fun cell -> f (Cell.of_goto cell) cost)
          ~acc:max_int
          ~acc_right:(fun cost right -> min_cost cost costs.:(right))
          ~from_left:(fun ~right ~parent ->
              if right < max_int then
                f parent (cost + right))
          ~from_right:(fun ~left ~parent ->
              let left = costs.:(left) in
              if left < max_int then
                f parent (left + cost)
            )
    end

    module Property = struct
      type property = int
      let leq_join = min_cost
    end

    (* Implement the interfaces required by DataFlow.ForCustomMaps *)

    module BoolMap() = struct
      let table = Boolvector.make Cell.n false
      let get t = Boolvector.test table t
      let set t x =
        if x
        then Boolvector.set table t
        else Boolvector.clear table t
    end

    (* Run the solver for shortest paths *)
    include Fix.DataFlow.ForCustomMaps(Property)(Graph)(struct
        let get i = Vector.get costs i
        let set i x = Vector.set costs i x
      end)(BoolMap())

    (* Run the solver for finite languages *)
    module Bool_or = struct
      type property = bool
      let leq_join = (||)
    end

    module Finite = BoolMap()

    module FiniteGraph = struct
      type variable = Cell.n index

      let count = Vector.make Cell.goto 0

      let () =
        Index.iter Cell.n (fun cell ->
            Reverse_dependencies.visit_occurrences cell
              ~visit_goto:(fun goto -> count.@(goto) <- succ)
              ~acc:()
              ~acc_right:(fun () _ -> ())
              ~from_left:(fun ~right:() ~parent:_ -> ())
              ~from_right:(fun ~left:_ ~parent:_ -> ())
          )

      let foreach_root visit_root =
        Index.iter (Transition.shift g) (fun sh ->
            let node = Tree.leaf (Transition.of_shift g sh) in
            visit_root (Cell.first_cell node) true
          );
        Index.iter (Transition.goto g) (fun gt ->
            Cell.iter_goto gt (fun gt' ->
                let index = Cell.of_goto gt' in
                if costs.:(index) < max_int && count.:(gt') = 0 then
                  visit_root index true
              )
          )

      (* Visit all the successors of a cell.
         This amounts to:
         - finding the node the cell belongs to
         - looking at the reverse dependencies of this node
         - visiting all cells that are affected in the dependencies
      *)
      let foreach_successor index finite f =
        if finite then
          Reverse_dependencies.visit_occurrences index
            ~visit_goto:(fun gt ->
                let count' = count.:(gt) - 1  in
                count.:(gt) <- count';
                assert (count' >= 0);
                if count' = 0 then
                  f index true
              )
            ~acc:true ~acc_right:(fun acc right -> acc && Finite.get right)
            ~from_left:(fun ~right ~parent -> if right then f parent true)
            ~from_right:(fun ~left ~parent ->
                if Finite.get left then f parent true)
    end
    include Fix.DataFlow.ForCustomMaps(Bool_or)(FiniteGraph)(Finite)(BoolMap())
    let () = stopwatch 2 "solved minimal costs"
  end

  module Analysis = struct
    let cost = Vector.get Solver.costs
    let finite = Solver.Finite.get
  end

  (*let () =
    let string_of_cost i = if i = max_int then "∞" else string_of_int i in
    Index.iter (Transition.goto g)
      (fun gt ->
         let min = ref max_int in
         let count = ref 0 in
         Cell.iter_goto gt
           (fun gtc -> incr count; min := Int.min (Analysis.cost (Cell.of_goto gtc)) !min);
         if !min = max_int then (
           let tr = Transition.of_goto g gt in
           match
             List.filter_map begin fun (red, n) ->
               let pre_classes = Array.length (Tree.pre_classes n) in
               let post_classes = Array.length (Tree.post_classes n) in
               let encode = Cell.encode n in
               let candidates = ref [] in
               for pre = pre_classes - 1 downto 0 do
                 for post = post_classes - 1 downto 0 do
                   if Analysis.cost (encode ~pre ~post) <> max_int then
                     push candidates (pre, post)
                 done
               done;
               match !candidates with
               | candidates when
                   List.exists begin function
                     | Reverse_dependencies.Inner _ -> false
                     | Reverse_dependencies.Leaf (gt', _pre, post) ->
                       gt = gt' &&
                       List.exists (fun (_, post_index) -> Array.length post.(post_index) > 0) candidates
                   end Reverse_dependencies.occurrences.:(n) ->
                 Some (red, n, candidates)
               | _ -> None
             end (Tree.goto_equations gt).non_nullable
           with
           | [] -> ()
           | paths ->
             Printf.eprintf "unreachable goto transition (id:%d): %s -> %s (%dx%d=%d classes)\n"
               (gt :> int)
               (Lr1.to_string g (Transition.source g tr))
               (Lr1.to_string g (Transition.target g tr))
               (Array.length (Classes.for_lr1 (Transition.source g tr)))
               (Array.length (Classes.for_edge gt))
               !count;
             (*let terminals set =
                 string_concat_map ~wrap:("{","}") ", "
                   (Terminal.to_string g)
                   (List.rev (IndexSet.elements set))
               in*)
             let production_to_string g p =
               Nonterminal.to_string g (Production.lhs g p) ^ ": " ^
               string_concat_map " " (Symbol.name g)
                 (Array.to_list (Production.rhs g p))
             in
             List.iter begin fun (red, n, candidates) ->
               (*let pre_classes = Array.length (Tree.pre_classes n) in
                 let post_classes = Array.length (Tree.post_classes n) in*)
               Printf.eprintf "- reduction: %s" (production_to_string g red.production);
               let encode = Cell.encode n in
               Printf.eprintf " with candidates";
               List.iter begin fun (pre, post) ->
                 Printf.eprintf " (%d,%d, cell:%d)" pre post (encode ~pre ~post :> int)
               end candidates;
               Printf.eprintf "\n";
               List.iter begin function
                 | Reverse_dependencies.Leaf (gt', pre, post) when gt = gt' ->
                   let pre = match pre with
                     | Pre_identity -> Fun.id
                     | Pre_singleton i -> Fun.const i
                   in
                   Printf.eprintf "  found a reverse dependency with classes %s\n"
                     (string_concat_map ", "
                        (fun (pre_index, post_index) ->
                           Printf.sprintf "(%d,%s)"
                             (pre pre_index)
                             (string_concat_map ~wrap:("[","]") "," string_of_cost
                                (Array.to_list post.(post_index)))
                        ) candidates)
                 | _ -> ()
               end Reverse_dependencies.occurrences.:(n);
             end paths
         )
      )*)
end)
