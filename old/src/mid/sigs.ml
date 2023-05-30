open Front
open Utils
open Misc
open Front
open Fix.Indexing

(** The representation of (compiled) grammars exported by Menhir *)
module type GRAMMAR = MenhirSdk.Cmly_api.GRAMMAR

(** The [INFO] module type first exposes a convenient interface for accessing
    all components of the [GRAMMAR] (using [INDEXED] interface).

    It also computes a few information that proves useful for later passes.
    For each [Lr1] state, the set of predecessors, the reductions grouped by
    lookahead tokens, the ε-closure of reductions, etc.

    It defines a reification of automaton's [Transition]s.
    Each transition is classified (shift or goto), given a name, and various
    useful operations are defined on it.

    Finally, it defines a useful function for working with partial derivatives
    with respect to Lr1 states.

    Implemented by [Info] module.
*)
module type INFO = sig
  (** The grammar for which information were computed *)
  module Grammar : GRAMMAR

  module Terminal : sig
    include GRAMMAR_INDEXED with type raw = Grammar.terminal
    val to_string : t -> string
    val all : set

    (** [semantic_value term] is [Some typ] if terminal [term] has a semantic
        value of type [typ], or [None] for unparameterized terminals. *)
    val semantic_value : t -> string option

    (** Wrapper around [IndexSet.inter] speeding-up intersection with [all] *)
    val intersect : set -> set -> set
  end

  module Nonterminal : sig
    include GRAMMAR_INDEXED with type raw = Grammar.nonterminal
    val to_string : t -> string
    val all : set
    val kind : t -> [`REGULAR | `START]
  end

  module Symbol : sig
    type t = T of Terminal.t | N of Nonterminal.t
    val of_g : Grammar.symbol -> t
    val to_g : t -> Grammar.symbol
    val name : ?mangled:bool -> t -> string
  end

  module Production : sig
    include GRAMMAR_INDEXED with type raw = Grammar.production
    val lhs : t -> Nonterminal.t
    val rhs : t -> Symbol.t array
    val kind : t -> [ `REGULAR | `START ]
  end

  module Lr1 : sig
    include GRAMMAR_INDEXED with type raw = Grammar.lr1
    val all : set
    val incoming : t -> Symbol.t option
    val items : t -> (Production.t * int) list
    val reductions : t -> (Production.t * Terminal.set) list

    (* Printing functions, for debug purposes.
       Not nice for the end-user (FIXME). *)

    val to_string : t -> string
    val list_to_string : t list -> string
    val set_to_string : set -> string

    (** [shift_on t] is the set of terminals that the state [t] can shift *)
    val shift_on : t -> Terminal.set

    (** [reduce_on t] is the set of terminals that triggers a reduction in
        state [t] *)
    val reduce_on : t -> Terminal.set

    (** [reject t] is set of terminals that will immediately cause the
        automaton to fail when in state [t] *)
    val reject : t -> Terminal.set

    (** The list of reductions after ε-closure.
        They are represented by tuples [(pop, prod, prods, lookahead)]:
        - the reduction triggers when lookahead token is in [lookahead]
        - the action of the reduction is to pop [pop] states from the stack
        - [prod], is the last reduction of the sequence, so after popping,
          the goto transition followed is labelled by [Production.lhs prod]
        - [prods] is the list of internal reductions in this sequence, if any,
          in reverse order (the reduction that came immediately before [prod]
          is the first element of [prods])

        This list contains all the non-ε reduction and a bunch of "composite"
        reductions.
        A composite reduction is a sequence of reductions starting from an
        ε-one and finishing by a normal reduction that will
        pop enough states such that the current state is popped.
        Therefore, [pop >= 1].
    *)
    type closed_reduction = {
      pop: int;
      prod: Production.t;
      prods: Production.t list;
      lookahead: Terminal.set;
    }
    val closed_reductions : t -> closed_reduction list

    (** All the stacks that were visited during ϵ-closure. This is used
        to compute all the possible derivations for a given grammar.
        (See [derive] module in [Redgraph]). *)
    val internal_stacks : t -> t list list

    (** Like [reject], but also including all [reject] sets from the Lr1 state
        that where reached when following the sequences of reductions during
        ε-closure.
        If useful, [closed_reduce t] could be defined as the union of
        lookaheads in [closed_reductions t].
    *)
    val closed_reject : t -> Terminal.set
    val closed_shift_on : t -> Terminal.set

    (** [predecessors t] is the set of LR(1) states that have transition going
        to [t]. *)
    val predecessors : t -> set

    (** [predecessors] but lifted to operate on a set of LR(1) states. *)
    val set_predecessors : set -> set
  end

  module Transition : sig
    (* Abstract types used as index to represent the different sets of
       transitions.
       For instance, [goto] represents the finite set of goto transition:
       - the value [goto : goto cardinal] is the cardinal of this set
       - any value of type [goto index] is a member of this set
         (representing a goto transition)
    *)
    type goto and shift and any

    (* The set of goto transitions *)
    val goto : goto cardinal
    (* The set of all transitions = goto U shift *)
    val any : any cardinal
    (* The set of shift transitions *)
    val shift : shift cardinal

    (* Building the isomorphism between any and goto U shift *)

    (* Inject goto into any *)
    val of_goto : goto index -> any index

    (* Inject shift into any *)
    val of_shift : shift index -> any index

    (* Project a transition into a goto or a shift transition *)
    val split : any index -> (goto index, shift index) either

    (* [find_goto s nt] finds the goto transition originating from [s] and
       labelled by [nt], or raise [Not_found].  *)
    val find_goto : Lr1.t -> Nonterminal.t -> goto index
    val find_goto_target : Lr1.t -> Nonterminal.t -> Lr1.t

    (* Get the source state of a transition *)
    val source : any index -> Lr1.t

    (* Get the target state of a transition *)
    val target : any index -> Lr1.t

    (* Symbol that labels a transition *)
    val symbol : any index -> Symbol.t

    (* Symbol that labels a goto transition *)
    val goto_symbol : goto index -> Nonterminal.t

    (* Symbol that labels a shift transition *)
    val shift_symbol : shift index -> Terminal.t

    (* [successors s] returns all the transitions [tr] such that
       [source tr = s] *)
    val successors : Lr1.t -> any index list

    (* [predecessors s] returns all the transitions [tr] such that
       [target tr = s] *)
    val predecessors : Lr1.t -> any index list
  end

  (** A simple type to represent a derivative of type ['a] with respect to a
      set of states. *)
  type 'a partial_derivative = Lr1.set * 'a

  (** Given a list of non-deterministic derivatives (the same state can appear
      in different derivation), return a list of deterministic ones
      where derivations that sharing the same states are grouped together. *)
  val determinize_derivatives :
    compare:('a -> 'a -> int) -> merge:('a list -> 'b) ->
    'a partial_derivative list -> 'b partial_derivative list
end

(** Syntax and Antimirov's derivative for the different flavors of regular
    expressions used by LRgrep.
    Implemented by [Regexp] module. *)
module type REGEXP = sig
  (** The grammar and derived information on which regular expresssions are
      implemented. *)
  module Info : INFO
  open Info

  (* Syntax for regular expression extended with reduction operator *)
  module RE : sig
    (** Integers that serves has unique id to identify sub-terms.
        Thanks to properties of Antimirov's derivatives, no new term is
        introduced during derivation. All terms are produced during initial
        parsing. *)
    type uid = private int

    (** A variable is identified by an integer that is unique for a lexer,
        and sequentially allocated. *)
    type var
    val var : int -> var index

    (** A regular expression term with its unique ID, its description and its
        position. *)
    type t = { uid : uid; desc : desc; position : Syntax.position; }

    (** The different constructors of regular expressions*)
    and desc =
      | Set of Lr1.set * var index option
      (** Recognise a set of states, and optionally bind the matching state to
          a variable. *)
      | Alt of t list
      (** [Alt ts] is the disjunction of sub-terms [ts] (length >= 2).
          [Alt []] represents the empty language. *)
      | Seq of t list
      (** [Seq ts] is the concatenation of sub-terms [ts] (length >= 2).
          [Seq []] represents the {ε}. *)
      | Star of t
      (** [Star t] is represents the Kleene star of [t] *)
      | Reduce
      (** The reduction operator *)

    (** Introduce a new term, allocating a unique ID *)
    val make : Syntax.position -> desc -> t

    (** Compare two terms *)
    val compare : t -> t -> int

    (** Print a term to a [Cmon] document. [var] arguments allow to customize
        printing of variables. *)
    val cmon : ?var:(var index -> Cmon.t) -> t -> Cmon.t
  end

  (** Represent stacks of regular expression continuations (plain [RE.t]s are
      never used directly in the derivation process, they are always wrapped
      in [KRE.t]).
      Formally, the stack is interpreted as the concatenation of all stacked
      expressions.
  *)
  module KRE : sig

    type clause
    val clause : int -> clause index

    (** A stack of regular expression continuations *)
    type t =
      | Done of { clause : clause index }
      (** [Done {clause=i}] represents the bottom of the stack matching the
          clause number [i]. When reached, it means nothing more has to be
          matched and so clause [i] succeeded. *)
      | More of RE.t * t
      (** [More (re,t)] is the stack [t] with expression [re] added on top.
          [re] should match before trying to match [t]. *)

    (** Compare two stacks. *)
    val compare : t -> t -> int

    (** Print a stack to a Cmon document. *)
    val cmon : t -> Cmon.t
  end

  (** A set of RE stacks.
      The set usually denotes the union of the denotation of all KREs in it,
      though in [derive_kre] it is just used to mark already visited KREs. *)
  module KRESet : sig
    include Set.S with type elt = KRE.t

    (** [derive_kre ~visited ~reached ~direct ~reduce] computes the partial
        derivatives of [kre]. Output is done by mutating arguments.
        - [visited] is used to remember which [kre]'s have been visited.
          If [mem kre !visited] then it is skipped.
        - [accept] is the list of clauses that are accepted by [kre].
        - [direct] is the list of Antimirov derivatives.
          Each element has the form [(lr1s, vars, kre')], meaning that the
          derivative of [kre] with respect to [lr1s] contains [kre'] and that
          when matching, the member of [lr1s] that was matched should be saved
          to [vars]
        - [reduce] is the list of KREs that should be matched modulo reduction

        For instance, input:
          (st0 as var | ! st1)? { ... } (* clause 1 *)
        is represented by:
          kre = More(Alt [Set (st0, Some var);
                          Seq [Reduce; Set (st1, None)];
                          Seq []],
                     Done {clause = 1})
        And [derive_kre ~visited ~accept ~direct ~reduce kre] produces:
        - accept := [1], since the RE is optional the clause matches
          immediately
        - direct := [(st0, [var], Done {clause = 1})
                     (st1, [], Done {clause = 1})]
          * consuming [st0] from input let us reaches a new expression that
            will succeed immediately, and we want to capture the input consumed
            in variable [var]
          * consuming [st1] is also possible (by matching immediately rather
            than modulo reductions "! st1")
        - reduce := [More (Set (st1, None), Done {clause = 1})], because this
          one should now be matched modulo reductions
    *)
    val derive_kre :
      visited:t ref ->
      accept:(KRE.clause index * 'a) list ref ->
      direct:(Lr1.set * RE.var indexset * KRE.t * 'a) list ref ->
      reduce:(KRE.t * 'a) list ref -> KRE.t -> 'a -> unit

    (** When simulating reductions, we have a slightly simpler notion of
        derivation:
        - It is not possible to accept, since we are not matching actual input
          but just rewriting regular expresssions; so when simulating
          reductions, the derivation doesn't accept but remembers that the
          expression should succeed as soon as normal matching resumes.
        - As a simplification, LRgrep doesn't accept simulating new reductions
          when already simulating reductions.
          For instance, it means that in our implementation, [! ! re] = [! re]
          ([!] is idempotent). This diverges from the formal specification:
          [!] simulates sequence of reductions for all possible lookaheads, but
          within a sequence, the lookahead cannot change. [! !] introduces a
          new degree of liberty: the sequence of reduction can start from a
          first lookahead token, and then switch to another sequence using a
          second lookahead.
          This is difficult (and expensive) to compute, and the benefits are
          unclear. So we depart from the formalism and simplify [!] that
          appears inside a reduction ([! ! re] is one of such examples, but
          more generally [! re1 ! re2] can also be simplified if [re1] is
          consumed by one of the reduction simulated by the first [!]).
        [derive_in_reduction] implements this simpler notion.
    *)
    val derive_in_reduction : t -> (t * RE.var indexset) partial_derivative list

    (** Print a set of KREs to a cmon document. *)
    val cmon : t -> Cmon.t
  end

  (* Translate a syntactic symbol, raise an exception if symbol is invalid *)
  val transl_symbol : Syntax.symbol -> Symbol.t

  (* Translate a clause in shallow syntax (defined in [Front.Syntax]) to a
     [KRE.t]. [alloc] is called to allocate variables *)
  val transl :
    alloc:(string -> RE.var index) -> clause:KRE.clause index ->
    Syntax.regular_expr -> KRE.t
end

(** The reduction graph keeps track of all rewritings that apply or might
    apply based on what is known of the [Lr1 stack] prefix. *)
module type REDGRAPH = sig
  module Info : INFO
  open Info

  (** Each LR(1) state [st] has a node representing it in the graph, you can
      get it with [State.of_lr1 st]. This node is the initial state of a
      transducer that simulates all reductions that starts from state [st]. *)
  module State : sig
    include CARDINAL
    val of_lr1 : Lr1.t -> n index
  end

  type state = State.n index

  (** From each node, there are transitions to a parent node as long as a
      reduction is going on.

      For instance, if state [st] has an item (X ::= a b .) in its item set,
      then the node [n0 = State.of_lr1 st] will have at least two parents:
      - [Some n1 = state_parent n0], representing the intermediate state
          (X ::= a . b)
      - [Some n2 = state_parent n1], representing the state (X ::= . a b)

      The graph has a mostly linear shape, except that there also are
      edges for goto transitions.
      For instance from node [n2], we know that the goto transitions labelled by
      [X] can be followed.
      These transitions target some other LR(1) states, for which we can
      simulate reductions too. *)
  val state_parent : state -> state option

  (** We can also ask which LR(1) states can appear at
      the stack position represented by a node:
      - [state_lr1s n0] = {st0}, we started from state [st0], so it is the only
        state possible for this node
      - [ss1 = state_lr1s n1] = [Lr1.predecessors st0], the states we can find
        immediately before [st0] on the stack are its predecessors
      - [ss2 = state_lr1s n2] = [Lr1.set_predecessors ss1], as we go further
        down the stacks, we iterate the [predecessors] function *)
  val state_lr1s : state -> Lr1.set

  (** When we reach node [n2], it is possible to reduce the production
      (X ::= a b). We know by construction that all states in [ss2] have a goto
      transition labelled by [X].

      We can create a mapping {source -> target} whenever there is a goto
      transition [source -X-> target] for [source] in [ss2].

      Since in state [st0], the reduction (X ::= a b .) triggers only for
      certain lookahead tokens, we augment the mapping to:
        {source -> target | lookaheads}
      It can be read as follow:
        "when reaching state [source] and the lookahead
         token belongs to [lookaheads], the automaton follows a goto transition
         to [target]".

      A first part of this information can be obtained using [state_goto_nt].
      This functions returns a partial map that binds each nonterminal for
      which a goto transition can be followed in this state to the set of
      lookaheads that permit to follow this transition.

      With our example, [state_goto_nt st2] associates [X] to [lookaheads].
  *)
  val state_goto_nt : state -> Terminal.set Nonterminal.map


  (** Using one or more reductions, when the lookahead token belongs to
      [lookahead] and the automaton state is in [sources], we can follow
      transitions to the states in targets.

      Usually, [targets] is a singleton [target], representing the family of
      goto transitions [source -X-> target] for [source ∈ sources].

      But is not always the case when there are ϵ-transitions.
      For instance, if [Y ::= X .] is in the itemset of [target] and can be
      reduced with [lookahead], then there is a state [target'] such that
      [source -Y-> target'], and [targets] = {target, target'}.
  *)
  type goto_closure = {
    sources: Lr1.set;
    targets: Lr1.set;
    lookahead: Terminal.set;
  }

  (** (See [state_goto_nt] and [goto_closure] first).
      [state_goto_closure] retrieves all goto transitions that can be reached,
      directly or indirectly, from [state].
  *)
  val state_goto_closure : state -> goto_closure list

  (** [state_reachable st] returns the set of all target states of all goto
      transitions that are reachable from [st].
      These includes all the [targets] of [state_goto_closure], those of all
      [state_parent] reachable from [st].
      And finally, the closure of this set by adding all
        [fun lr1state -> state_reachable (State.of_lr1 lr1state)].
  *)
  val state_reachable : State.n index -> Lr1.set

  (** Compute all the derivations necessary to implement the "modulo reduction"
      rewriting. Starting from a regular expression [root], step will be
      applied for each possible rewriting.
      For rewriting sequences, [step] is applied multiple times.
      The implementation aims to be efficient:
      - intermediate values are reused when multiple sequences have a common
        prefix
      - [step] can return [None] to abort early, when a rewriting doesn't
        apply.

      All rewritings that apply to the same state are grouped together using
      [join]. A map indexed by states is returned. The map is partial:
      if a state has no rewriting it will not be bound.
  *)
  val derive :
    root:'a ->
    step:('a -> Lr1.t -> 'a option) ->
    join:('a list -> 'b) ->
    'b Lr1.map

end

(** Simulates the effect of reductions on derivable objects.

    A derivable object can be thought of as an abstract description of an NFA.
    Reduction simulation starts of a specific state and takes the form of an
    extension of the NFA.
    It has some internal transitions when a reduction is ongoing, and external
    transitions to the original NFA when a reduction succeeds.
*)
module type REDUCTION = sig
  module Info : INFO
  open Info

  (** Something is derivable if it accepts a [derivation] primitive.
      For an NFA state, the derivation is essentially the list of transitions.
      For a regular expression, this is Antimirov's notion of derivative.

      We parametrize our computation with respect to an abstraction notion of
      "DERIVABLE" objects:
      - it helps to cleanly separate the notion of reduction simulation and
        of regular expression
      - we can memoize the derivation function, see [Cache] functor, or
        interleave debugging features, easily
      - the abstract representation allows us to construct the NFA lazily
        without cluttering the interface (no one needs to know). This will be
        convenient when generating the final automaton, since we know only a
        subset of it is ever reachable (the subset that intersects _valid LR
        stacks_).
  *)
  module type DERIVABLE = sig

    (** The type of derivable objects *)
    type t

    (** Label that annotates a transition *)
    type label

    val merge_label : label list -> label

    (** The derivation primitive. The outcome is a list of (non-deterministic)
        "transitions": pairs [(ts, d)] of a set of labels [ts] (Lr1 states) and
        [d], the derivable object targetted by the transition. *)
    val derive : t -> (t * label) partial_derivative list

    (** [merge ts] return an object that represents the union (the disjunction)
        between many derivable objects. *)
    val merge : t list -> t

    (** A total comparison function, suitable to implement [Map.OrderedType] *)
    val compare : t -> t -> int

    (** Print a derivable object as a [Cmon.t] document *)
    val cmon : t -> Cmon.t

    val cmon_label : label -> Cmon.t
  end

  (** The cache functor: it memoizes the output of [derive] function such
      that future calls are instantaneous. *)
  module Cache (D : DERIVABLE) : sig
    include DERIVABLE with type label = D.label

    (** Memoize a derivable object *)
    val lift : D.t -> t

    (** Recover the original derivable object *)
    val unlift : t -> D.t
  end

  (** Simulate reduction of derivable objects represented by [D] *)
  module Make (D : DERIVABLE) :
  sig
    (** The type of states of the "reduction simulation" NFA. *)
    type t

    (** The transitions leaving a state are represented as a pair of:
        - external transitions, targeting [D.t]
        - internal transitions, staying inside [t]. *)
    type transitions =
      (D.t * D.label) partial_derivative list *
      (t   * D.label) partial_derivative list

    (** The NFA produced by reduction simulation is quite naive: it contains
        dead ends, subset of the automaton without any external transitions.
        Those subsets can be discarded, but it is difficult to tell ahead of
        time.
        We use a heuristic that is quick to compute and detects dead-ends quite
        accurately but not perfectly (sometime we enter such dead-end and give
        up a few states later).
        This heuristic needs to pre-compute a bunch of derivations of the
        original object, those are stored in a [compilation] object.
        This object also serves as an entrypoint to the extension of the NFA.
    *)
    type compilation

    (** The precomputations (see [compilation]) can be costly.
        Furthermore, it happens that we simulate the same reductions many times
        (because of a disjunction or a kleene star).
        Therefore we can cache those in a [compilation_cache].
    *)
    type compilation_cache

    (** Create a new cache. If the same cache is passed to the [compile]
        function, redundant compilation will be skipped.
        (See [compilation_cache]) *)
    val make_compilation_cache : unit -> compilation_cache

    (** Precompute derivations for a certain object to speed-up derivation *)
    val compile : compilation_cache -> D.t -> compilation

    (** Dump the informations pre-computed in a [compilation] object *)
    val cmon : compilation -> Cmon.t

    (** The initial transitions *)
    val initial : compilation -> transitions

    (** Get the transitions of an intermediate state of the extended NFA *)
    val derive : t -> transitions

    (** Compare two intermediate states *)
    val compare : t -> t -> int

    (* TODO:
       - find a better terminology than compilation ?
       - maybe merge the [compilation] and [t] types; this will simplify the
         interface, hopefully this will simplify the implementation too.
       - not urgent: some potential optimizations are noted in
         [../../FUTURE.md], check if they really improve the situation
    *)
  end
end
