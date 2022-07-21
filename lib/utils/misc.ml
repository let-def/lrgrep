let array_last arr = match Array.length arr with
  | 0 -> None
  | n -> Some (arr.(n - 1))

let rec array_findi f i arr =
  if i >= Array.length arr then
    raise Not_found
  else if f i arr.(i) then
    i
  else
    array_findi f (i + 1) arr

let group_by
    ~(compare:'a -> 'a -> int)
    ~(group:'a -> 'a list -> 'b)
    (list : 'a list) : 'b list
  =
  match List.sort compare list with
  | [] -> []
  | key :: rest ->
    let rec loop acc ks key = function
      | [] -> group key ks :: acc
      | key' :: rest ->
        if compare key key' = 0
        then loop acc (key' :: ks) key rest
        else loop (group key ks :: acc) [] key' rest
    in
    loop [] [] key rest

let merge_group
    ~(equal:'b -> 'b -> bool)
    ~(group:'b -> 'a list -> 'c)
  : ('a * 'b) list -> 'c list
  =
  function
  | [] -> []
  | (s1, k1) :: rest ->
    let rec loop acc ss key = function
      | [] -> group key ss :: acc
      | (s, key') :: rest ->
        if equal key key'
        then loop acc (s :: ss) key rest
        else loop (group key ss :: acc) [s] key' rest
    in
    loop [] [s1] k1 rest

open Fix.Indexing
open BitSet

type 'a indexset = 'a IndexSet.t

type ('n, 'a) indexmap = ('n, 'a) IndexMap.t

let (@) l1 l2 =
  match l1, l2 with
  | [], l | l, [] -> l
  | l1, l2 -> l1 @ l2

let array_cons arr index value =
  arr.(index) <- value :: arr.(index)

let option_cons x xs =
  match x with
  | None -> xs
  | Some x -> x :: xs

let index_fold v a f =
  let a = ref a in
  Index.iter v (fun i -> a := f i !a);
  !a

let indexset_bind : 'a indexset -> ('a index -> 'b indexset) -> 'b indexset =
  fun s f ->
  IndexSet.fold (fun lr1 acc -> IndexSet.union acc (f lr1)) s IndexSet.empty

let vector_set_add vec index value =
  Vector.set vec index (IndexSet.add value (Vector.get vec index))

let vector_iter v f =
  Index.iter (Vector.length v) (fun i -> f (Vector.get v i ))

let vector_tabulate n f =
  Vector.get (Vector.init n f)

let compare_index =
  (Int.compare : int -> int -> int :> _ index -> _ index -> int)

let string_concat_map sep f xs = String.concat sep (List.map f xs)

let string_of_index =
  (string_of_int : int -> string :> _ index -> string)

let string_of_indexset ?(string_of_index=string_of_index) xs =
  "[" ^ string_concat_map ";" string_of_index (IndexSet.elements xs) ^ "]"

let cmon_index =
  (Cmon.int : int -> Cmon.t :> _ index -> Cmon.t)

let cmon_indexset xs =
  Cmon.constant (
    "[" ^ string_concat_map ";" string_of_index (IndexSet.elements xs) ^ "]"
  )

let indexmap_update map k f =
  IndexMap.add k (f (IndexMap.find_opt k map)) map

let push xs x = xs := x :: !xs

let pushs xs = function
  | [] -> ()
  | x -> xs := x @ !xs

let rec hash_list f = function
  | [] -> 7
  | x :: xs -> Hashtbl.seeded_hash (hash_list f xs) (f x)

let rec merge_uniq cmp l1 l2 =
  match l1, l2 with
  | [], l2 -> l2
  | l1, [] -> l1
  | h1 :: t1, h2 :: t2 ->
    let c = cmp h1 h2 in
    if c = 0
    then h1 :: merge_uniq cmp t1 t2
    else if c < 0
    then h1 :: merge_uniq cmp t1 l2
    else h2 :: merge_uniq cmp l1 t2

let print_cmon oc cmon =
  PPrint.ToChannel.pretty 0.8 80 oc (Cmon.print cmon)
