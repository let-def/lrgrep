module type S =
  sig
    type 'n index
    type ('n, !'a) t
    val create: int -> ('n, 'a) t
    val clear : ('n, 'a) t -> unit
    val reset : ('n, 'a) t -> unit
    val copy: ('n, 'a) t -> ('n, 'a) t
    val add: ('n, 'a) t -> 'n index -> 'a -> unit
    val remove: ('n, 'a) t -> 'n index -> unit
    val find: ('n, 'a) t -> 'n index -> 'a
    val find_opt: ('n, 'a) t -> 'n index -> 'a option
    val find_all: ('n, 'a) t -> 'n index -> 'a list
    val replace : ('n, 'a) t -> 'n index -> 'a -> unit
    val mem : ('n, 'a) t -> 'n index -> bool
    val iter: ('n index -> 'a -> unit) -> ('n, 'a) t -> unit
    val filter_map_inplace: ('n index -> 'a -> 'a option) -> ('n, 'a) t -> unit
    val fold: ('n index -> 'a -> 'b -> 'b) -> ('n, 'a) t -> 'b -> 'b
    val length: ('n, 'a) t -> int
    val stats: ('n, 'a) t -> Hashtbl.statistics
    val to_seq : ('n, 'a) t -> ('n index * 'a) Seq.t
    val to_seq_keys : _ t -> 'n index Seq.t
    val to_seq_values : ('n, 'a) t -> 'a Seq.t
    val add_seq : ('n, 'a) t -> ('n index * 'a) Seq.t -> unit
    val replace_seq : ('n, 'a) t -> ('n index * 'a) Seq.t -> unit
    val of_seq : ('n index * 'a) Seq.t -> ('n, 'a) t
  end

include S with type 'n index := 'n Fix.Indexing.index
