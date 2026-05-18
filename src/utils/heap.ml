module type S = sig
  type 'a elt
  type ('a, 'b) t = private
    | Leaf
    | Node of ('a, 'b) t * 'a elt * 'b * ('a, 'b) t * int

  val empty: ('a, 'b) t
  val merge: ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
  val singleton : 'a elt -> 'b -> ('a, 'b) t
  val insert : 'a elt -> 'b -> ('a, 'b) t -> ('a, 'b) t

  val pop : ('a, 'b) t -> ('a elt * 'b * ('a, 'b) t) option

  type ('a, 'b) pop2 =
    | Head of 'a elt * 'b * 'a elt * 'b * ('a, 'b) t
    | Tail of 'a elt * 'b
    | Done
  val pop2 : ('a, 'b) t -> ('a, 'b) pop2
end

module Make(T : sig
    type 'a t
    val compare : 'a t -> 'a t -> int
  end) : S with type 'a elt := 'a T.t =
struct
  type ('a, 'b) t =
    | Leaf
    | Node of ('a, 'b) t * 'a T.t * 'b * ('a, 'b) t * int

  let empty = Leaf
  let singleton k v = Node (Leaf, k, v, Leaf, 1)

  let rank = function Leaf -> 0 | Node (_,_,_,_,r) -> r

  let rec merge t1 t2 = match t1,t2 with
    | Leaf, t | t, Leaf -> t
    | Node (l1, k1, v1, r1, _), Node (l2, k2, v2, r2, _) ->
      if T.compare k1 k2 > 0 then
        merge_lt l2 k2 v2 r2 t1
      else
        merge_lt l1 k1 v1 r1 t2

  and merge_lt l k v r t2 =
    let merged = merge r t2 in (* always merge with right *)
    let rank_left = rank l and rank_right = rank merged in
    if rank_left >= rank_right
    then Node (l, k, v, merged, rank_right+1)
    else Node (merged, k, v, l, rank_left+1) (* left becomes right due to being shorter *)


  let insert k v t = merge (singleton k v) t

  let pop = function
    | Leaf -> None
    | Node (l, k, v, r, _) ->
      Some (k, v, merge l r)

  type ('a, 'b) pop2 =
    | Head of 'a T.t * 'b * 'a T.t * 'b * ('a, 'b) t
    | Tail of 'a T.t * 'b
    | Done

  let pop2 = function
    | Leaf -> Done
    | Node (Leaf, k, v, _, _) ->
      Tail (k, v)
    | Node (Node (ll, lk, lv, lr, _), k, v, Leaf, _) ->
      Head (k, v, lk, lv, merge ll lr)
    | Node (
        (Node (ll, lk, lv, lr, _) as l),
        k, v,
        (Node (rl, rk, rv, rr, _) as r),
        _
      ) ->
      if T.compare lk rk <= 0
      then Head (k, v, lk, lv, merge (merge ll lr) r)
      else Head (k, v, rk, rv, merge (merge rl rr) l)
end

module Int = Make(struct type _ t = int let compare = compare end)
