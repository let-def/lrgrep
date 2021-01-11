(** Some definitions that help using the typechecker to encode strong
    invariants. *)

(** {1 Type-level equality} *)

type (_, _) eq = Refl : ('a, 'a) eq
(** A value of type [(a, b) eq] witnesses that types [a] and [b] are equal. *)

val follow_eq : ('a, 'b) eq -> 'a -> 'b
(** If we know that two types are equal, we can turn value of one type into a
    value of the other type. *)

(* {1 Type-level ordering}

   Generalizes the type equality above to encode a total "ordering" on types:
   a value of type [(a, b) order] witnesses that types [a] and [b] can be
   ordered (for instance to store them in an heterogeneous data structure),
   and if they are equal, a constraint is introduced.
*)
module Order : sig type (_, _) t = Lt | Eq : ('a, 'a) t | Gt end

type ('a, 'b) order = ('a, 'b) Order.t

(** Generate an [order] value from the result of a [compare] function.
    The degenerate case [(a, a) order] just an order on values.  *)
val order_from_comparison : int -> ('a, 'a) order

(** {1 Uninhabitated type}

    A type with no inhabitant and its elimination function.
*)

type void
val void : void -> 'a

(** An encoding of natural numbers at the type-level using singleton types.

    [_ Natural.t] is a family of types such that:

    1) Foreach natural number [n],
         there is value [v_n] and an abstract type [t_n] such
         that [v_n : t_n Natural.t]
         and [v_n] is the only inhabitant of [t_n Natural.t]

    2) Given a value
         [v_n : t_n Natural.t]
       one can access the encoded number using [Natural.to_int v_n = n]

    3) Given two values
         [v_n : t_n Natural.t]
         [v_m : t_m Natural.t]
       one can compare the two numvers using
         [Natural.order v_n v_m : (t_n, t_m) order]
       If they happen to be equal, the constraint [t_n = t_m] is made
       available.

    Thus the [v_n] and [t_n] are both isomorphic to natural numbers and
    therefore to each other.
*)
module Natural : sig

  type 'a t
  (** The family of natural numbers where ['a] is used to identify a specific
      number. *)

  val order : 'a t -> 'b t -> ('a, 'b) order
  (** Order to natural numbers *)

  val lift_eq : ('a, 'b) eq -> ('a t, 'b t) eq
  (** If two types are known to be equal, then the natural number they
      represent (... if they happen to represent numbers) are equal to.

      That is, [_ Natural.t] is a function (and not an arbitrary relation).
  *)

  val to_int : 'a t -> int
  (** Access the natural number encoded in a natural value as an int *)


  (** The constant zero, at the type-level *)
  type zero

  (** The constant zero, at the value-level *)
  val zero : zero t

  (** The constant one, at the type-level *)
  type one

  (** The constant one, at the value-level *)
  val one : one t

  (** New naturals are introduced using modules to bind the abstract type that
      is used to index a given natural number. *)
  module type T = sig
    type n
    val n : n t
  end

  (** Introduce a natural using a functor *)
  module Nth (N : sig val n : int end) : T

  (* Introduce a natural as a first-class module *)
  val nth : int -> (module T)

  (* Some type-level arithmetic *)

  type ('a, 'b) sum
  val add : 'a t -> 'b t -> ('a, 'b) sum t
  val sum_comm : (('a, 'b) sum, ('b, 'a) sum) eq
  val sum_assoc : ((('a, 'b) sum, 'c) sum, ('a, ('b, 'c) sum) sum) eq

  type ('a, 'b) prod
  val mul : 'a t -> 'b t -> ('a, 'b) prod t
  val prod_comm : (('a, 'b) prod, ('b, 'a) prod) eq
  val prod_assoc : ((('a, 'b) prod, 'c) prod, ('a, ('b, 'c) prod) prod) eq
end

(** An encoding of finites sets at the type-level:
    - value of type ['n set] is a natural number.
    - value of type ['n elt] is guaranteed to be smaller than the natural
      number denoted by ['n set]

    That is we interpret a natural number [n] as the finite set with [n]
    elements and [x : n elt] as an element of this set.
*)
module Finite : sig
  type 'n set = 'n Natural.t
  type 'n elt = private int

  (** Various functions useful when considering the natural [n] as the type of
      finite sets with [n] elements. *)
  module Set : sig
    module type T = Natural.T

    val cardinal : 'n set -> int
    (** [cardinal n] = [to_int n] *)

    val iter : 'n set -> ('n elt -> unit) -> unit
    (** Iter on all elements of the set, from 0 to n - 1 *)

    val rev_iter : 'n set -> ('n elt -> unit) -> unit
    (** Iter elements in reverse order, from n - 1 down to 0 *)

    (** Fold elements from 0 to n - 1 *)
    val fold_left : 'n set -> ('b -> 'n elt -> 'b) -> 'b -> 'b

    (** Fold elements from n - 1 down to 0 *)
    val fold_right : 'n set -> ('n elt -> 'b -> 'b) -> 'b -> 'b

    (** Create a set for which you don't how many elements there will be in
        yet. *)
    module Gensym () : sig
      type n
      (** The natural number encoding the cardinal of the set. It is kept
          opaque until [freeze] is called. While opaque, [fresh] element can be
          generated. *)

      val freeze : unit -> n set
      (** Freeze the set: reveal it, allowing to observe its cardinal (the
          number of calls to [fresh]).  Since the cardinal is now fixed, calls
          to [fresh] are forbidden. *)

      val fresh : unit -> n elt
      (** Get a new element from a non frozen-set *)
    end
  end

  (** Various functions useful for working with elements. *)
  module Elt : sig
    val of_int : 'n set -> int -> 'n elt
    (** [of_int set n] returns [n] considered as an element of the finite set
        [set].
        It requires that [n >= 0 && n < cardinal set] otherwise it fails with
        [Invalid_argument]. *)

    val of_int_opt : 'n set -> int -> 'n elt option
    (** [of_int_opt set n] returns
        - [Some n'], where [n'] is the n'th element of the set if
            [n >= 0 && n < cardinal set]
        - [None]
    *)

    val to_int : 'n elt -> int
    (** Inject an element of a finite set to the set of integers. *)

    (** [compare n1 n2] is [Int.compare (to_int n1) (to_int n2) *)
    val compare : 'n elt -> 'n elt -> int
  end

  (** Finite arrays: arrays whose domain is a finite set.  The representation
      is exactly the one of ocaml arrays, but indexing operations are
      restricted to finite elements of the right set and thus are safe by
      construction.  *)
  module Array : sig
    type ('n, 'a) t = private 'a array
    type 'a _array = A : ('n, 'a) t -> 'a _array [@@ocaml.unboxed]
    val empty : (Natural.zero, _) t
    val is_empty : ('n, 'a) t -> (Natural.zero, 'n) eq option
    val length : ('n, 'a) t -> 'n set
    external get : ('n, 'a) t -> 'n elt -> 'a = "%array_unsafe_get"
    external set : ('n, 'a) t -> 'n elt -> 'a -> unit = "%array_unsafe_set"
    val make : 'n set -> 'a -> ('n, 'a) t
    val init : 'n set -> ('n elt -> 'a) -> ('n, 'a) t
    val make_matrix : 'i set -> 'j set -> 'a -> ('i, ('j, 'a) t) t
    val append : ('n, 'a) t -> ('m, 'a) t -> (('n, 'm) Natural.sum, 'a) t
    val of_array : 'a array -> 'a _array
    module type T = sig include Natural.T type a val table : (n, a) t end
    module Of_array (A : sig type a val table : a array end) : T with type a = A.a
    val module_of_array : 'a array -> (module T with type a = 'a)
    val to_array : ('n, 'a) t -> 'a array
    val all_elements : 'n set -> ('n, 'n elt) t

    val iter : ('a -> unit) -> (_, 'a) t -> unit
    val iteri : ('n elt -> 'a -> unit) -> ('n, 'a) t -> unit
    val rev_iter : ('a -> unit) -> (_, 'a) t -> unit
    val rev_iteri : ('n elt -> 'a -> unit) -> ('n, 'a) t -> unit
    val map : ('a -> 'b) -> ('n, 'a) t -> ('n, 'b) t
    val mapi : ('n elt -> 'a -> 'b) -> ('n, 'a) t -> ('n, 'b) t
    val fold_left : ('a -> 'b -> 'a) -> 'a -> ('n, 'b) t -> 'a
    val fold_right : ('b -> 'a -> 'a) -> ('n, 'b) t -> 'a -> 'a
    val iter2 : ('a -> 'b -> unit) -> ('n, 'a) t -> ('n, 'b) t -> unit
    val map2 : ('a -> 'b -> 'c) -> ('n, 'a) t -> ('n, 'b) t -> ('n, 'c)  t
    val copy : ('n, 'a) t -> ('n, 'a) t
  end
end
