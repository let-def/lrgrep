open Fix.Indexing

module type S = sig
  type 'n index
  type (+'n, !+'a) t
  val empty: ('n, 'a) t
  val is_empty: ('n, 'a) t -> bool
  val mem: 'n index -> ('n, 'a) t -> bool
  val add: 'n index -> 'a -> ('n, 'a) t -> ('n, 'a) t
  val update: 'n index -> ('a option -> 'a option) -> ('n, 'a) t -> ('n, 'a) t
  val singleton: 'n index -> 'a -> ('n, 'a) t
  val remove: 'n index -> ('n, 'a) t -> ('n, 'a) t
  val merge:
    ('n index -> 'a option -> 'b option -> 'c option) -> ('n, 'a) t -> ('n, 'b) t -> ('n, 'c) t
  val union: ('n index -> 'a -> 'a -> 'a option) -> ('n, 'a) t -> ('n, 'a) t -> ('n, 'a) t
  val compare: ('a -> 'a -> int) -> ('n, 'a) t -> ('n, 'a) t -> int
  val equal: ('a -> 'a -> bool) -> ('n, 'a) t -> ('n, 'a) t -> bool
  val iter: ('n index -> 'a -> unit) -> ('n, 'a) t -> unit
  val fold: ('n index -> 'a -> 'b -> 'b) -> ('n, 'a) t -> 'b -> 'b
  val for_all: ('n index -> 'a -> bool) -> ('n, 'a) t -> bool
  val exists: ('n index -> 'a -> bool) -> ('n, 'a) t -> bool
  val filter: ('n index -> 'a -> bool) -> ('n, 'a) t -> ('n, 'a) t
  val filter_map: ('n index -> 'a -> 'b option) -> ('n, 'a) t -> ('n, 'b) t
  val partition: ('n index -> 'a -> bool) -> ('n, 'a) t -> ('n, 'a) t * ('n, 'a) t
  val cardinal: ('n, 'a) t -> int
  val bindings: ('n, 'a) t -> ('n index * 'a) list
  val min_binding: ('n, 'a) t -> ('n index * 'a)
  val min_binding_opt: ('n, 'a) t -> ('n index * 'a) option
  val max_binding: ('n, 'a) t -> ('n index * 'a)
  val max_binding_opt: ('n, 'a) t -> ('n index * 'a) option
  val choose: ('n, 'a) t -> ('n index * 'a)
  val choose_opt: ('n, 'a) t -> ('n index * 'a) option
  val split: 'n index -> ('n, 'a) t -> ('n, 'a) t * 'a option * ('n, 'a) t
  val find: 'n index -> ('n, 'a) t -> 'a
  val find_opt: 'n index -> ('n, 'a) t -> 'a option
  val find_first: ('n index -> bool) -> ('n, 'a) t -> 'n index * 'a
  val find_first_opt: ('n index -> bool) -> ('n, 'a) t -> ('n index * 'a) option
  val find_last: ('n index -> bool) -> ('n, 'a) t -> 'n index * 'a
  val find_last_opt: ('n index -> bool) -> ('n, 'a) t -> ('n index * 'a) option
  val map: ('a -> 'b) -> ('n, 'a) t -> ('n, 'b) t
  val mapi: ('n index -> 'a -> 'b) -> ('n, 'a) t -> ('n, 'b) t
  val to_seq : ('n, 'a) t -> ('n index * 'a) Seq.t
  val to_rev_seq : ('n, 'a) t -> ('n index * 'a) Seq.t
  val to_seq_from : 'n index -> ('n, 'a) t -> ('n index * 'a) Seq.t
  val add_seq : ('n index * 'a) Seq.t -> ('n, 'a) t -> ('n, 'a) t
  val of_seq : ('n index * 'a) Seq.t -> ('n, 'a) t
end

module IntMap = struct
  type 'n index = int
  type (+'n, !'a) t = 'a IntMap.t

  include (IntMap : Map.S with type key := int and type 'a t := 'a IntMap.t)
end

module F(X : Index.Unsafe.T) = struct
  module type S = S with type 'a index = 'a X.t
end

include Index.Unsafe.Coerce(F)(IntMap)

let domain t =
  fold (fun n _ set -> IndexSet.add n set) t IndexSet.empty

let inflate (f : 'n index -> 'a) (set : 'n IndexSet.t) : ('n, 'a) t =
  IndexSet.fold (fun i map -> add i (f i) map) set empty

let filter_inflate (f : 'n index -> 'a option) (set : 'n IndexSet.t) : ('n, 'a) t =
  IndexSet.fold
    (fun i map -> match f i with
       | None -> map
       | Some x -> add i x map
    ) set empty
