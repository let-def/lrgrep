open Fix.Indexing
module IntMap = Map.Make(Int)

type ('n, !+'a) t = 'a IntMap.t
let empty: ('n, 'a) t = IntMap.empty
let is_empty: ('n, 'a) t -> bool = IntMap.is_empty
let mem: 'n index -> ('n, 'a) t -> bool =
  (IntMap.mem : int -> _ :> 'n index -> _)
let add: 'n index -> 'a -> ('n, 'a) t -> ('n, 'a) t =
  (IntMap.add : int -> _ :> 'n index -> _)

let update: 'n index -> ('a option -> 'a option) -> ('n, 'a) t -> ('n, 'a) t =
  (IntMap.update : int -> _ :> 'n index -> _)

let singleton: 'n index -> 'a -> ('n, 'a) t =
  (IntMap.singleton : int -> _ :> 'n index -> _)

let remove: 'n index -> ('n, 'a) t -> ('n, 'a) t =
  (IntMap.remove : int -> _ :> 'n index -> _)

let merge (type n) f ma mb =
  IntMap.merge
    (Obj.magic (f : n index -> 'a option -> 'b option -> 'c option)
     : int -> 'a option -> 'b option -> 'c option)
      ma mb

let union (type n) f ma mb =
  IntMap.union
    (Obj.magic (f : n index -> 'a -> 'a -> 'a option)
       : int -> 'a -> 'a -> 'a option)
    ma mb

let compare: ('a -> 'a -> int) -> ('n, 'a) t -> ('n, 'a) t -> int = IntMap.compare
let equal: ('a -> 'a -> bool) -> ('n, 'a) t -> ('n, 'a) t -> bool = IntMap.equal

let iter (type n) f ma =
  IntMap.iter
    (Obj.magic (f : n index -> 'a -> unit) : int -> 'a -> unit)
    ma

let fold (type n) f ma acc =
  IntMap.fold
    (Obj.magic (f : n index -> 'a -> 'b -> 'b) : int -> 'a -> 'b -> 'b)
    ma acc

(*let for_all: ('n index -> 'a -> bool) -> ('n, 'a) t -> bool = IntMap.for_all
let exists: ('n index -> 'a -> bool) -> ('n, 'a) t -> bool = IntMap.exists
let filter: ('n index -> 'a -> bool) -> ('n, 'a) t -> ('n, 'a) t = IntMap.filter
let filter_map: ('n index -> 'a -> 'b option) -> ('n, 'a) t -> ('n, 'b) t = IntMap.filter_map
let partition: ('n index -> 'a -> bool) -> ('n, 'a) t -> ('n, 'a) t * ('n, 'a) t = IntMap.partition*)
let cardinal: ('n, 'a) t -> int = IntMap.cardinal
(*let bindings: ('n, 'a) t -> ('n index * 'a) list = IntMap.bindings
let min_binding: ('n, 'a) t -> ('n index * 'a) = IntMap.min_binding
let min_binding_opt: ('n, 'a) t -> ('n index * 'a) option = IntMap.min_binding_opt
let max_binding: ('n, 'a) t -> ('n index * 'a) = IntMap.max_binding
let max_binding_opt: ('n, 'a) t -> ('n index * 'a) option = IntMap.max_binding_opt*)
(*let choose: ('n, 'a) t -> ('n index * 'a) = IntMap.choose
let choose_opt: ('n, 'a) t -> ('n index * 'a) option = IntMap.choose_opt*)

let split: 'n index -> ('n, 'a) t -> ('n, 'a) t * 'a option * ('n, 'a) t =
  (IntMap.split : int -> _ :> 'n index -> _)

let find: 'n index -> ('n, 'a) t -> 'a =
  (IntMap.find : int -> _ :> 'n index -> _)

let find_opt: 'n index -> ('n, 'a) t -> 'a option =
  (IntMap.find_opt : int -> _ :> 'n index -> _)

(*let find_first: ('n index -> bool) -> ('n, 'a) t -> 'n index * 'a = IntMap.find_first
let find_first_opt: ('n index -> bool) -> ('n, 'a) t -> ('n index * 'a) option = IntMap.find_first_opt
let find_last: ('n index -> bool) -> ('n, 'a) t -> 'n index * 'a = IntMap.find_last
let find_last_opt: ('n index -> bool) -> ('n, 'a) t -> ('n index * 'a) option = IntMap.find_last_opt*)

let map: ('a -> 'b) -> ('n, 'a) t -> ('n, 'b) t = IntMap.map
(*let mapi: ('n index -> 'a -> 'b) -> ('n, 'a) t -> ('n, 'b) t = IntMap.mapi*)

(*let to_seq : ('n, 'a) t -> ('n index * 'a) Seq.t = IntMap.to_seq
let to_rev_seq : ('n, 'a) t -> ('n index * 'a) Seq.t = IntMap.to_rev_seq
let to_seq_from : 'n index -> ('n, 'a) t -> ('n index * 'a) Seq.t = IntMap.to_seq_from
let add_seq : ('n index * 'a) Seq.t -> ('n, 'a) t -> ('n, 'a) t = IntMap.add_seq
let of_seq : ('n index * 'a) Seq.t -> ('n, 'a) t = IntMap.of_seq*)
