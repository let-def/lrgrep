open Fix.Indexing

module Positive = Const(struct let cardinal = max_int end)

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
  let c = ref (Int.compare len (Array.length a2)) in
  let i = ref 0 in
  while !c = 0 && !i < len do
    c := cmp a1.(!i) a2.(!i);
    incr i
  done;
  !c

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

(** A function for consing a value when using Map.update *)
let cons_update x = function
  | None -> Some [x]
  | Some xs -> Some (x :: xs)

(** A function for adding an element to a set when using Map.update *)
let add_update x = function
  | None -> Some (IndexSet.singleton x)
  | Some y -> Some (IndexSet.add x y)

(** A function for unioning sets when using Map.update *)
let union_update x = function
  | None -> Some x
  | Some y -> Some (IndexSet.union x y)

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

(** Vector get *)
let (.:()) = Vector.get

(** Vector set *)
let (.:()<-) = Vector.set

(** Vector update *)
let (.@()<-) v i f = v.:(i) <- f v.:(i)

(** [tabulate_finset n f] tabulates function [f] over all indices in the
    finite set of cardinal [n] *)
let tabulate_finset n f =
  Vector.get (Vector.init n f)

let relation_reverse' n f =
  let rev = Vector.make n IndexSet.empty in
  Index.rev_iter n (fun src ->
      IndexSet.rev_iter (fun tgt -> rev.@(tgt) <- IndexSet.add src)
        (f src)
    );
  rev

let relation_reverse n rel =
  let rev = Vector.make n IndexSet.empty in
  Vector.rev_iteri (fun src tgts ->
      IndexSet.rev_iter (fun tgt -> rev.@(tgt) <- IndexSet.add src) tgts
    ) rel;
  rev

let fix_relation  (relation : ('n, 'n indexset) vector) (values : ('n, 'a) vector)
    ~(propagate : 'n index -> 'a -> 'n index -> 'a -> 'a)
  =
  let n = Vector.length values in
  let marked = ref true in
  let marks = Boolvector.make n true in
  let update i =
    if Boolvector.test marks i then (
      Boolvector.clear marks i;
      let value = values.:(i) in
      IndexSet.rev_iter (fun j ->
          let value' = values.:(j) in
          let value'' = propagate i value j value' in
          if value' != value'' then (
            values.:(j) <- value'';
            Boolvector.set marks j;
            marked := true
          )
        ) relation.:(i)
    )
  in
  while !marked do
    marked := false;
    Index.rev_iter n update
  done

let close_relation ?reverse rel =
  let rev = match reverse with
    | Some rev -> rev
    | None -> relation_reverse (Vector.length rel) rel
  in
  fix_relation rev rel
    ~propagate:(fun _ v _ v' -> IndexSet.union v v')

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

let cons_if c x xs =
  if c then x :: xs else xs

(** Turns a index into a cmon document *)
let cmon_index =
  (Cmon.int : int -> Cmon.t :> _ index -> Cmon.t)

(** Turns a set of indices into a cmon document *)
let cmon_indexset ?(index=cmon_index) xs =
  Cmon.list_map index (IndexSet.elements xs)

let cmon_set_cardinal set =
  Cmon.constant ("{" ^ string_of_int (IndexSet.cardinal set) ^ " elements}")

let cmon_pair f g (x, y) = Cmon.tuple [f x; g y]

let cmon_option f = function
  | None -> Cmon.constant "None"
  | Some x -> Cmon.constructor "Some" (f x)

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

let rec list_rev_iter f = function
  | x1 :: x2 :: x3 :: x4 :: xs ->
    list_rev_iter f xs;
    f x4;
    f x3;
    f x2;
    f x1
  | [x1; x2; x3] ->
    f x3;
    f x2;
    f x1
  | [x1; x2] ->
    f x2;
    f x1
  | [x1] ->
    f x1
  | [] -> ()

let rec list_drop n = function
  | _ :: xs when n > 0 -> list_drop (n - 1) xs
  | xs -> xs

let rec fixpoint ?counter ~propagate todo = match !todo with
  | [] -> ()
  | todo' ->
    Option.iter incr counter;
    todo := [];
    List.iter propagate todo';
    fixpoint ?counter ~propagate todo

let assert_equal_length v1 v2 =
  assert_equal_cardinal (Vector.length v1) (Vector.length v2)

let bytes_match b i str =
  Bytes.length b >= i + String.length str &&
  let exception Exit in
  match
    for j = 0 to String.length str - 1 do
      if Bytes.get b (i + j) <> String.get str j then
        raise Exit
    done
  with
  | () -> true
  | exception Exit -> false

let verbosity_level = ref 0

let stopwatch_delta =
  let last = ref [] in
  fun level ->
  let rec visit = function
    | (level', _) :: acc when level' > level ->
      visit acc
    | [] -> 0.0, []
    | (level', time) :: acc when level' = level ->
      (time, acc)
    | (_, time) :: _ as acc ->
      time, acc
  in
  let time = Sys.time () in
  let time', last' = visit !last in
  last := (level, time) :: last';
  (time -. time')

let stopwatch_counter = ref 0

let stopwatch_perfs =
  ref @@
  match Sys.getenv_opt "STOPWATCH_PERF" with
  | None -> []
  | Some list ->
    let steps = String.split_on_char ',' list in
    List.mapi (fun i step ->
        (i land 1 = 0),
        int_of_string step
      ) steps

let stopwatch_perf_step i =
  match !stopwatch_perfs with
  | (_, j) :: _ as steps when j <= i ->
    let result = ref None in
    let rec loop = function
      | [] -> []
      | (_, j) :: _ as rest when j > i ->
        rest
      | (_, j) :: rest when j < i ->
        loop rest
      | (result', _) :: rest ->
        result := Some result';
        loop rest
    in
    stopwatch_perfs := loop steps;
    !result
  | _ -> None

let stopwatch level fmt =
  if level <= !verbosity_level then (
    let delta = stopwatch_delta level in
    incr stopwatch_counter;
    let perf_status = stopwatch_perf_step !stopwatch_counter in
    if perf_status = Some false then Perfctl.disable ();
    if delta < 10.
    then Printf.eprintf "[%03d: % 5.0fms]" !stopwatch_counter (delta *. 1000.)
    else Printf.eprintf "[%03d: % 5.01fs]" !stopwatch_counter delta;
    Printf.fprintf stderr "%s-> " (String.make level ' ');
    Printf.kfprintf (fun _ ->
        prerr_newline ();
        if perf_status = Some true then Perfctl.enable ();
      ) stderr fmt
  ) else
    Printf.ifprintf stderr fmt

let rewrite_keywords f (pos : Lexing.position) str =
  let b = Bytes.of_string str in
  let l = Bytes.length b in
  let i = ref 0 in
  let pos_lnum = ref pos.pos_lnum in
  let pos_bol = ref pos.pos_bol in
  let nl () =
    pos_bol := pos.pos_cnum + !i;
    incr pos_lnum
  in
  let escape () =
    if !i < l && Bytes.get b !i = '\n' then
      (incr i; nl ())
    else
      incr i
  in
  while !i < l do
    match Bytes.get b !i with
    | '\n' -> incr i; nl ()
    | '\\' -> incr i; escape ()
    (* Look for $ident(ident) *)
    | '$' ->
      let dollar = !i in
      incr i;
      let in_range a b c = a <= b && b <= c in
      let is_ident c =
        in_range 'a' c 'z' || in_range 'A' c 'Z' ||
        in_range '0' c '9' || (c = '_') || (c = '\'')
      in
      while !i < l && is_ident (Bytes.get b !i)
      do incr i done;
      if !i < l && Bytes.get b !i = '(' then (
        let lpar = !i in
        incr i;
        while !i < l && is_ident (Bytes.get b !i) do
          incr i
        done;
        if !i < l && Bytes.get b !i = ')' then (
          let rpar = !i in
          let kw = Bytes.sub_string b dollar (lpar - dollar) in
          let arg = Bytes.sub_string b (lpar + 1) (rpar - lpar - 1) in
          let pos = {pos with pos_lnum = !pos_lnum; pos_bol = !pos_bol;
                              pos_cnum = pos.pos_cnum + dollar} in
          if f pos kw arg then (
            Bytes.set b dollar '_';
            Bytes.set b lpar '_';
            Bytes.set b rpar '_';
          )
        )
      )
    (* Skip strings *)
    | '"' ->
      incr i;
      while !i < l &&
            let c = Bytes.get b !i in
            incr i;
            match c with
            | '"' -> false
            | '\n' -> nl (); true
            | '\\' -> escape (); true
            | _ -> true
      do () done
    | _ -> incr i
  done;
  Bytes.to_string b
