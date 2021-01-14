module Make (Lr1 : Intf.LR1) : Intf.SIGMA with module Lr1 = Lr1 =
struct
  module Lr1 = Lr1

  type t =
    | Pos of Lr1.Set.t
    | Neg of Lr1.Set.t

  let empty = Pos Lr1.Set.empty
  let full = Neg Lr1.Set.empty
  let compl = function Pos x -> Neg x | Neg x -> Pos x
  let is_empty = function Pos x -> Lr1.Set.is_empty x | Neg _ -> false
  let is_full = function Neg x -> Lr1.Set.is_empty x | Pos _ -> false

  let is_subset_of x1 x2 =
    match x1, x2 with
    | Pos x1, Pos x2 -> Lr1.Set.subset x1 x2
    | Neg x1, Neg x2 -> Lr1.Set.subset x2 x1
    | Pos x1, Neg x2 -> Lr1.Set.disjoint x1 x2
    | Neg _ , Pos _ -> false

  let inter x1 x2 =
    match x1, x2 with
    | Pos x1, Pos x2 -> Pos (Lr1.Set.inter x1 x2)
    | Neg x1, Neg x2 -> Neg (Lr1.Set.union x1 x2)
    | (Pos x1, Neg x2) | (Neg x2, Pos x1) ->
      Pos (Lr1.Set.diff x1 x2)

  let intersect x1 x2 =
    match x1, x2 with
    | Pos x1, Pos x2 -> not (Lr1.Set.disjoint x1 x2)
    | Neg _, Neg _ -> true
    | Pos x1, Neg x2 | Neg x2, Pos x1 ->
      not (Lr1.Set.is_empty (Lr1.Set.diff x1 x2))

  let compare x1 x2 =
    match x1, x2 with
    | Pos x1, Pos x2 -> Lr1.Set.compare x1 x2
    | Neg x1, Neg x2 -> Lr1.Set.compare x2 x1
    | Pos _ , Neg _ -> -1
    | Neg _ , Pos _ -> 1

  let union x1 x2 =
    match x1, x2 with
    | Neg x1, Neg x2 -> Neg (Lr1.Set.inter x1 x2)
    | Pos x1, Pos x2 -> Pos (Lr1.Set.union x1 x2)
    | Pos x1, Neg x2 | Neg x2, Pos x1 ->
      Neg (Lr1.Set.diff x2 x1)

  let partition l =
    let only_pos = ref true in
    let project = function Pos x -> x | Neg x -> only_pos := false; x in
    let l = List.map project l in
    let pos x = Pos x in
    try
      if !only_pos
      then List.map pos (Lr1.partition l)
      else
        let parts, total = Lr1.partition_and_total l in
        Neg total :: List.map pos parts
    with exn ->
      Printf.eprintf
        "Partition failed with %d inputs (strictly positive: %b):\n"
        (List.length l) !only_pos;
      List.iter (fun set ->
          Printf.eprintf "- cardinal=%d, set={" (Lr1.Set.cardinal set);
          Lr1.Set.iter (fun elt -> Printf.eprintf "%d," (elt :> int)) set;
        ) l;
      raise exn

  let mem x = function
    | Pos xs -> Lr1.Set.mem x xs
    | Neg xs -> not (Lr1.Set.mem x xs)
end
