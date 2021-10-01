(**
   "Caml Object Notation", a library for pretty-printing ocaml values
    with sharing.
*)

type id = private int
(** Unique identifiers to make sharing explicit.
    Variable names are automatically generated.
*)

(** The output is the syntax of OCaml values (with unique identifiers
    for structured values) extended with variables and let-bindings
    to represent sharing.
*)
type syntax =
  | Unit               (* () *)
  | Nil                (* [] *)
  | Bool of bool       (* true, false *)
  | Char of char       (* 'x' *)
  | Int of int         (* 0, 1, ... *)
  | Float of float     (* 0.0, 1.0, ... *)
  | Constant of string (* constant constructor, e.g None *)
  | Cons of {id: id; car: syntax; cdr: syntax} (* x :: xs *)
  | String of {id: id; data: string} (* "Foo" *)
  | Tuple of {id: id; data: syntax list} (* (a, b, c) ... *)
  | Record of {id: id; data: (string * syntax) list} (* {a: va; b: vb} *)
  | Constructor of {id: id; tag: string; data: syntax} (* Some foo *)
  | Var of id          (* x *)
  | Let of {id: id; bindings: (id * syntax) list; body: syntax}

type t = private syntax
(** A sub-type of [syntax] that is guaranteed to represent well-scoped
    values.  ([Var] nodes always refer to variables bound by an
    enclosing [Let]). *)

(** Primitive values *)

val unit: t
val bool: bool -> t
val char: char -> t
val int: int -> t
val float: float -> t
val string: string -> t
val constant: string -> t
val constructor: string -> t -> t
val tuple: t list -> t
val record: (string * t) list -> t

(** Shortcut for constructor with multiple arguments *)
val ctuple: string -> t list -> t

(** Shortcut for constructor with inline record *)
val crecord: string -> (string * t) list -> t

val nil: t
val list: t list -> t

(** Variants that prevent sharing these values *)

val unshared_string: string -> t
val unshared_constructor: string -> t -> t
val unshared_tuple: t list -> t
val unshared_record: (string * t) list -> t
val unshared_ctuple: string -> t list -> t
val unshared_crecord: string -> (string * t) list -> t
val unshared_list: t list -> t

val explicit_sharing: t -> t
(** Rewrite a value, introducing let-binders to make sharing
    explicit. *)

val print_as_is: t -> PPrint.document
(** Print the value as it is (without changing sharing) to a
    [PPrint.document] *)

val format_as_is: Format.formatter -> t -> unit
(** Format the value as it is (without changing sharing) to a
    [Format.formatter] *)

val print: t -> PPrint.document
(** Print the value with explicit sharing to a [PPrint.document].
    [print t == print_as_is (explicit_sharing t)]. *)

val format: Format.formatter -> t -> unit
(** Format the value with explicit sharing to a [Format.formatter].
    [format t == format_as_is (explicit_sharing t)]. *)

val list_map : ('a -> t) -> 'a list -> t
