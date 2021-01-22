(** [Mulet] module implements DFA generation by regular expression derivation.
*)

(** The alphabet is an abstract type in Mulet. A value of type [Sigma.t]
    represents a subset of the alphabet, where [Sigma.empty] is the empty
    subset and [Sigma.full] is the complete alphabet.
    Individual letters are never manipulated directly, so there is no need to
    represent them.
*)
module type SIGMA = sig
  include Map.OrderedType
  (* Subsets are totally ordered for using maps indexed by [Sigma.t] *)

  val empty : t
  (** The [empty] subset.
      It is the only subset [e] for which [is_subset_of e _ = true]. *)

  val full : t
  (** The [full] subset.
      It is the only subset [f] for which [is_subset_of _ f = true]. *)

  val is_empty : t -> bool
  (** Returns [true] iff the argument is the [empty] set.  *)

  val is_full : t -> bool
  (** Returns [true] iff the argument is the [full] set.  *)

  val is_subset_of : t -> t -> bool
  (** [is_subset_of s1 s2] is true iff all elements in [s1] are elements of
      [s2]. *)

  val compl : t -> t
  (** [compl s1] returns the complement of set [s1]: the set that contains all
      elements of [s2] that were not in [s1]. *)

  val inter : t -> t -> t
  (** [inter s1 s2] returns the subset [s] of element that are both in  [s1]
      and [s2]. *)

  val partition : t list -> t list
  (** [partition ss] returns a list of sets [ps] such that:
      - all sets in [ps] are disjoints
      - forall p in ps, s in ss, either they are disjoint or p is a subset of s
      - union ps = union ss
  *)
end

(** Signature of a monoid *)
module type MONOID = sig
  type t
  (** The support of the monoid *)

  val empty : t
  (** The empty element *)

  val append : t -> t -> t
  (** Operation of the monoid *)
end

(** Regular expressions have an expression label constructor, that behaves like
    an epsilon but can be queried.
Labels are elements of a monoid with a total order to index them in a map.
*)
module type LABEL = sig
  include Map.OrderedType
  include MONOID with type t := t
end

(** The syntax of regular expressions is introduced as a private type.
    Values are introduced using smart constructors that ensure wellformed-ness.

    The meaning of parameters:
    - ['s] is type of alphabet on which the regular expression is built
      (see [SIGMA.t])
    - ['l] is type of labels used to tag the regular expression (see [LABEL.t])
    - ['a] is the type of abstract objects (see [DERIVABLE] below)
*)
type ('s, 'l, 'a) re = private
  | Set of 's
  (** The expression that denotes the set of words with a single letter from a
      specific subset .
      denote (Set s) = { [x] | x \in s }
  *)
  | Epsilon
  (** The empty word.
      denote Epsilon = { [] } *)
  | Closure of ('s, 'l, 'a) re
  (** The Kleene star of another expression.
      denote (Closure re) =
        { [] } U { hd . tl | s1 \in denote re, tl \in denote (Closure re) }
  *)
  | Not of ('s, 'l, 'a) re
  (** Complement of a regular expression.
      denote (Not re) = Sigma* \ denote re
      where Sigma* is the complete language on alphabet Sigma
  *)
  | Concat of ('s, 'l, 'a) re list
  (** Concatenation of multiple expressions .
      denote (Concat (x :: xs)) =
        { x . y | x \in denote x, y \in denote (Concat xs) }
      denote (Concat []) =
        { [] }
  *)
  | Or of ('s, 'l, 'a) re list
  (** Disjunction of multiple expressions .
      denote (Or (x :: xs)) =
        denote x U denote (Or xs)
      denote (Or []) =
        { }
  *)
  | And of ('s, 'l, 'a) re list
  (** Conjunction of multiple expressions .
      denote (And (x :: xs)) =
        denote x \inter denote (And xs)
      denote (And []) =
        { }
  *)
  | Label   of 'l
  (** Annotate the expression with label [l].
      Labels act as an annotated version of Epsilon, some kind of identified
      empty word.
      They can be used in two ways but they do not affect the denoted language:
      denote (Label _) = denote Epsilon = { [] }.

      In the generated automaton, a transition is annotated with a label:
      - the empty label [Label.empty] by default,
      - the union (using [Label.append]) of all [Label] constructors from the
        source expression that were traversed to reach this target

      In the generated automaton, each state is identified by the regular
      expression recognized when starting from this state. The [get_label]
      function can be used to recover [labels] that are in terminal positions
      in this expression.
  *)
  | Abstract of 'a
  (** Regular expressions can be augmented by abstract objects provided they are
      derivable (see [DERIVABLE]).
      This allows injecting regular languages with a custom representation, for
      instance those that are not convenient to represent using regular
      expressions. *)

val cmon_re :
  ?compare:(module Map.S with type key = ('s, 'l, 'a) re) ->
  set:('s -> Cmon.t) ->
  label:('l -> Cmon.t) ->
  abstract:('a -> Cmon.t) ->
  ('s, 'l, 'a) re ->
  Cmon.t
(** A routine to print a regular expression using the [Cmon] toolkit.
    The [compare] constructor is optional, but when used it will be used to
    detect sharing.  *)

(** The definition of [DERIVABLE] objects, for using a custom representation of
    regular languages. *)
module type DERIVABLE = sig
  type sigma
  (** The type of alphabet on which the language is built. *)

  type label
  (** The type of labels. *)

  include Map.OrderedType
  (** Derivable objects need to be totally ordered for indexing. *)

  val is_empty : t -> bool
  (** Does this object denote the empty language? *)

  val nullable : t -> bool
  (** Does this object denote a language with the empty word? *)

  val get_label : t -> label
  (** Get the label of this object *)

  (** Derivation operations *)

  val left_classes : t -> (sigma -> 'a -> 'a) -> 'a -> 'a
  (** Fold over each subset of the alphabet with respect to which the object is
      "interesting" to derive. *)

  val left_delta : t -> sigma -> label * (sigma, label, t) re
  (** Compute the derivative of the object with respect to a specific subset
      of the alphabet.
      It returns the labels that were consumed in this process as well as the
      regular expression represents the regular language after derivation. *)
end

(** The signature of instantiated regular expression deriver. *)
module type S = sig

  type sigma
  (** The type of alphabet *)

  type label
  (** The type of labels *)

  type abstract
  (** The type of abstract objects *)

  module Expr : sig
    type t = (sigma, label, abstract) re
    (** The type of regular expression *)

    val empty : t
    (** The empty regular expression, {} *)

    val epsilon : t
    (** The regular expression recognizing the empty word, {[]} *)

    val star : t -> t
    (** Kleene star *)

    val set : sigma -> t
    (** Primitive regular expression recognizing the one letter words which are
        part of the sigma set *)

    val ( ^. ) : t -> t -> t
    (** Concatenate two expressions *)

    val ( &. ) : t -> t -> t
    (** Conjunction (intersection) of two expressions *)

    val ( |. ) : t -> t -> t
    (** Disjunction (union or alternative) of two expressions *)

    val disjunction : t list -> t
    (** Disjunction of a list of expressions *)

    val conjunction : t list -> t
    (** Conjunction of a list of expressions *)

    val concatenation : t list -> t
    (** Concatenation of a list of expressions *)

    val abstract : abstract -> t
    (** Inject a custom abstract object in the expression *)

    val compl : t -> t
    (** Complement of a regular expression *)

    val label : label -> t
    (** Tag the regular expression *)

    include Map.OrderedType with type t := t
    (** Regular expressions are totally ordered, for indexing purposes *)

    val is_empty : t -> bool
    (** Does this expression denote the empty language? *)

    val nullable : t -> bool
    (** Does this expression denote a language with the empty word? *)

    val get_label : t -> label
    (** Extract the labels of an expression in terminal position *)

    val left_classes : t -> (sigma -> 'a -> 'a) -> 'a -> 'a
    (** Fold over each subset of the alphabet with respect to which the object
        is "interesting" to derive. *)

    val left_delta : t -> sigma -> label * t
    (** Compute the derivative of the regular expression with respect to a
        specific subset of the alphabet.
        It returns the labels that were consumed in this process as well as the
        regular expression that represents continuation after derivation. *)
  end

  module Map : Map.S with type key = Expr.t
  (** Map whose domain ranges over regular expressions.  *)

  type transition = sigma * label * Expr.t
  (** A transition is represented as a triplet (sigma, label, expr):
      - [sigma] is the subset of the alphabet for which this transition should
        be followed
      - [label] is the label that apply to this transition (an annotation that
        has no meaning for Mulet itself)
      - [expr] is the destination of the transition, the new state that is
        reached after following it (states are indexed by the regular
        expression they recognize).
  *)

  type dfa = transition list Map.t
  (** A DFA is map whose keys are regular expressions and whose values are a
      list of transitions.
      A wellformed DFA is such that the language denoted by repeatedly
      following the transitions coincide with the language denoted by they
      regular expression key.
  *)

  val derive : Expr.t -> transition list
  (** Compute all transitions of an expression.
      A DFA can be constructed as the closure of this derivation (repeat
      derivation on all transitions, until no new expressions are produced).
  *)

  val add_to_dfa : dfa -> Expr.t list -> dfa
  (** [add_to_dfa dfa exprs] adds a few expressions to a DFA:
      - all exprs are bound in the resulting [dfa]
      - if [dfa] was wellformed, the result is wellformed too

      This interface allows to share work when deriving multiple expression
      that have common parts.
  *)

  val make_dfa : Expr.t -> dfa
  (** Converts the given expression to a DFA.
      [make_dfa expr = add_to_dfa Map.empty [expr] *)
end

module Make
    (Sigma : SIGMA)
    (Label : LABEL)
    (Abstract : DERIVABLE with type sigma := Sigma.t
                           and type label := Label.t) :
  S with type sigma = Sigma.t
     and type label = Label.t
     and type abstract = Abstract.t

(** A default implementation of [Sigma] using 8-bit characters. *)
module Chars : sig
  include SIGMA
  val of_list : char list -> t
  val to_list : t -> char list
end

(** A default implementation of [Abstract] when one does not need to use the
    feature. *)
module Null_derivable : sig
  type t = { void : 'a. 'a }
  val compare : t -> t -> int
  val is_empty : t -> bool
  val nullable : t -> bool
  val get_label : t -> _
  val left_classes : t -> _
  val left_delta : t -> _ -> _ * (_, _, t) re
end

