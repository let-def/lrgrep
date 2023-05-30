open Fix.Indexing

let compare_ignore _ _ = 0

let compare_pair fst snd (x1, y1) (x2, y2) =
  match fst x1 x2 with
  | 0 -> snd y1 y2
  | n -> n

let compare_fst f = compare_pair f compare_ignore

let compare_snd f = compare_pair compare_ignore f

(** [array_last arr] returns the [Some x] where [x] is the last element
    of the array, or [None] if the array is empty *)
let array_last arr = match Array.length arr with
  | 0 -> None
  | n -> Some (arr.(n - 1))

(** [array_findi f i arr] finds the smallest [j >= i] such that
    [f j arr.(j) = true], or raises [Not_found] if there is no such [j] *)
let rec array_findi f i arr =
  if i >= Array.length arr then
    raise Not_found
  else if f i arr.(i) then
    i
  else
    array_findi f (i + 1) arr

let array_split a =
  (Array.map fst a, Array.map snd a)

let array_compare cmp a1 a2 =
  let len = Array.length a1 in
  let c = Int.compare len (Array.length a2) in
  if c <> 0 then c else
    let rec loop i len =
      if i = len then 0 else
        let c = cmp a1.(i) a2.(i) in
        if c <> 0 then c else
          loop (i + 1) len
    in
    loop 0 len

(** [group ~compare ~group list]
    Group togethers the elements of [list] that are equivalent according to
    [compare], using the [group] function *)
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

(** Merge consecutive elements [(a, b)] of list that have the same b, according
    to [equal]. *)
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

(** Convenient alias for [IndexSet] *)
type 'a indexset = 'a IndexSet.t

(** Convenient alias for [IndexMap] *)
type ('n, 'a) indexmap = ('n, 'a) IndexMap.t

(** Optimize stdlib's (@) to avoid copying the lhs when the rhs is empty *)
let (@) l1 l2 =
  match l1, l2 with
  | [], l | l, [] -> l
  | l1, l2 -> l1 @ l2

(** [array_cons a i v] cons the value [v] to the list [a.(i)], where a is an
    array of list. *)
let array_cons arr index value =
  arr.(index) <- value :: arr.(index)

(** [index_fold n a f] fold [f] with accumulator [a] over all indices in the
    finite set of cardinal [n] *)
let index_fold n a f =
  let a = ref a in
  Index.iter n (fun i -> a := f i !a);
  !a

(** [indexset_bind s f] returns the union of all sets [f i] for [i] in [s] *)
let indexset_bind : 'a indexset -> ('a index -> 'b indexset) -> 'b indexset =
  fun s f ->
  IndexSet.fold (fun lr1 acc -> IndexSet.union acc (f lr1)) s IndexSet.empty

(** [vector_set_add v i elt] adds element [elt] to [i]'th element of [v],
    a vector of sets. *)
let vector_set_add vec index value =
  Vector.set vec index (IndexSet.add value (Vector.get vec index))

(** [vector_iter v f] applies [f] to all elements of vector [v],
    in increasing order of indices *)
let vector_iter v f =
  Index.iter (Vector.length v) (fun i -> f (Vector.get v i ))

(** [tabulate_finset n f] tabulates function [f] over all indices in the
    finite set of cardinal [n] *)
let tabulate_finset n f =
  Vector.get (Vector.init n f)

(** Equality on indices *)
let equal_index =
  (Int.equal : int -> int -> bool :> 'a index -> 'a index -> bool)

(** Comparison of indices *)
let compare_index =
  (Int.compare : int -> int -> int :> 'a index -> 'a index -> int)

(** [string_concat_map ~wrap:(pre, post) sep f xs] returns a string made of the
    concatenation of the elements of [xs] printed by function [f] and separated
    by [sep].
    [wrap] is optional, but if provided, pre is prepended to the
    result and [post] is appended. *)
let string_concat_map ?wrap sep f xs =
  let result = String.concat sep (List.map f xs) in
  match wrap with
  | None -> result
  | Some (pre, post) -> pre ^ result ^ post

let string_of_index =
  (string_of_int : int -> string :> _ index -> string)

let string_of_indexset ?(index=string_of_index) xs =
  string_concat_map ~wrap:("[", "]") ";" index (IndexSet.elements xs)

(** Prepend an element to a list reference *)
let push xs x = xs := x :: !xs

(** Prepend elements to a list reference *)
let pushs xs = function
  | [] -> ()
  | x -> xs := x @ !xs

(* [hash_list h list] returns a hash of [list] where individual elements are
   hashed using [h] *)
let rec hash_list f = function
  | [] -> 7
  | x :: xs -> Hashtbl.seeded_hash (hash_list f xs) (f x)

(** Merge to ordered lists, keeping only one copy of elements occurring in both
    lists. (Duplicate elements in the same list are kept.) *)
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

(** List cons where the element to cons is optional *)
let cons_option x xs =
  match x with
  | None -> xs
  | Some x -> x :: xs

(** Turns a index into a cmon document *)
let cmon_index =
  (Cmon.int : int -> Cmon.t :> _ index -> Cmon.t)

(** Turns a set of indices into a cmon document *)
let cmon_indexset xs =
  Cmon.constant (
    string_concat_map ~wrap:("[","]") ";"
      string_of_index (IndexSet.elements xs)
  )

(** Print a cmon document to a channel with sensible defaults *)
let print_cmon oc cmon =
  PPrint.ToChannel.pretty 0.8 80 oc (Cmon.print cmon)

let sort_and_merge compare merge l =
  let rec loop x xs = function
    | [] -> [merge x xs]
    | y :: ys ->
      if compare x y = 0
      then loop x (y :: xs) ys
      else
        let xxs = merge x xs in
        xxs :: loop y [] ys
  in
  match List.sort compare l with
  | [] -> []
  | x :: xs -> loop x [] xs

let sort_and_merge_indexed compare l =
  let union_ix ix (_, ix') = IndexSet.union ix ix' in
  sort_and_merge
    (compare_fst compare)
    (fun (x, ix) rest -> (x, List.fold_left union_ix ix rest))
    l

let list_foralli f l =
  let rec loop i = function
    | [] -> true
    | x :: xs -> (f i x) && (loop (i + 1) xs)
  in
  loop 0 l
