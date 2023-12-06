module type S = SetSig.S0

(* A compressed (or should we say sparse?) bit set is a list of pairs
     of integers. The first component of every pair is an index, while
     the second component is a bit field. The list is sorted by order
     of increasing indices. *)

type t =
  | N
  | C of int * int * t

type element =
  int

let word_size =
  Sys.word_size - 1

let empty =
  N

let is_empty = function
  | N ->
    true
  | C _ ->
    false

let add i s =
  let ioffset = i mod word_size in
  let iaddr = i - ioffset
  and imask = 1 lsl ioffset in
  let rec add = function
    | N ->
      (* Insert at end. *)
      C (iaddr, imask, N)
    | C (addr, ss, qs) as s ->
      if iaddr < addr then
        (* Insert in front. *)
        C (iaddr, imask, s)
      else if iaddr = addr then
        (* Found appropriate cell, update bit field. *)
        let ss' = ss lor imask in
        if ss' = ss then
          s
        else
          C (addr, ss', qs)
      else
        (* Not there yet, continue. *)
        let qs' = add qs in
        if qs == qs' then
          s
        else
          C (addr, ss, qs')
  in
  add s

let singleton i =
  add i N

let remove i s =
  let ioffset = i mod word_size in
  let iaddr = i - ioffset
  and imask = 1 lsl ioffset in
  let rec remove = function
    | N ->
      N
    | C (addr, ss, qs) as s ->
      if iaddr < addr then
        s
      else if iaddr = addr then
        (* Found appropriate cell, update bit field. *)
        let ss' = ss land (lnot imask) in
        if ss' = 0 then
          qs
        else if ss' = ss then
          s
        else
          C (addr, ss', qs)
      else
        (* Not there yet, continue. *)
        let qs' = remove qs in
        if qs == qs' then
          s
        else
          C (addr, ss, qs')
  in
  remove s

let rec fold f s accu =
  match s with
  | N ->
    accu
  | C (base, ss, qs) ->
    loop f qs base ss accu

and loop f qs i ss accu =
  if ss = 0 then
    fold f qs accu
  else
    (* One could in principle check whether [ss land 0x3] is zero and if
       so move to [i + 2] and [ss lsr 2], and similarly for various sizes.
       In practice, this does not seem to make a measurable difference. *)
    loop f qs (i + 1) (ss lsr 1) (if ss land 1 = 1 then f i accu else accu)

let map f t =
  fold (fun x xs -> add (f x) xs) t empty

let filter_map f t =
  fold (fun x ys -> match f x with
      | None -> ys
      | Some y -> add y ys) t empty

let iter f s =
  fold (fun x () -> f x) s ()

let rec rev_iter f = function
  | N -> ()
  | C (base, ss, qs) ->
    rev_iter f qs;
    for i = word_size downto 0 do
      if ss land (1 lsl i) <> 0 then
        f (base + i)
    done

let rec fold_right f acc = function
  | N -> acc
  | C (base, ss, qs) ->
    let acc = ref (fold_right f acc qs) in
    for i = word_size downto 0 do
      if ss land (1 lsl i) <> 0 then
        acc := f !acc (base + i)
    done;
    !acc

let exists f t =
  let exception Found in
  match fold (fun elt () -> if f elt then raise Found) t () with
  | () -> false
  | exception Found -> true

let is_singleton s =
  match s with
  | C (_, ss, N) ->
    (* Test whether only one bit is set in [ss]. We do this by turning
       off the rightmost bit, then comparing to zero. *)
    ss land (ss - 1) = 0
  | C (_, _, C _)
  | N ->
    false

let cardinal s =
  fold (fun _ m -> m + 1) s 0

let elements s =
  List.rev (fold (fun tl hd -> tl :: hd) s [])

let rec subset s1 s2 =
  match s1, s2 with
  | N, _ ->
    true
  | _, N ->
    false
  | C (addr1, ss1, qs1), C (addr2, ss2, qs2) ->
    if addr1 < addr2 then
      false
    else if addr1 = addr2 then
      if (ss1 land ss2) <> ss1 then
        false
      else
        subset qs1 qs2
    else
      subset s1 qs2

let rec quick_subset a1 ss1 = function
  | N -> false
  | C (a2, ss2, qs2) ->
    if a1 = a2 then
      ss1 land ss2 <> 0
    else
      (a1 > a2 && quick_subset a1 ss1 qs2)

let quick_subset s1 s2 =
  match s1 with
  | N -> true
  | C (a1, ss1, _) ->
    (* We know that, by construction, ss1 is not empty.
       It suffices to test s2 also has elements in common with ss1 at address
       a1 to determine the quick_subset relation. *)
    quick_subset a1 ss1 s2

let mem i s =
  subset (singleton i) s

let rec union s1 s2 =
  match s1, s2 with
  | N, s
  | s, N ->
    s
  | C (addr1, ss1, qs1), C (addr2, ss2, qs2) ->
    if addr1 < addr2 then
      let qs = union qs1 s2 in
      if qs == qs1
      then s1
      else C (addr1, ss1, qs)
    else if addr1 > addr2 then
      let qs = union s1 qs2 in
      if qs == qs2
      then s2
      else C (addr2, ss2, qs)
    else
      let ss = ss1 lor ss2 in
      let qs = union qs1 qs2 in
      if ss = ss2 && qs == qs2
      then s2
      else if ss = ss1 && qs == qs1
      then s1
      else C (addr1, ss, qs)

let rec inter s1 s2 =
  match s1, s2 with
  | N, _
  | _, N ->
    N
  | C (addr1, ss1, qs1), C (addr2, ss2, qs2) ->
    if addr1 < addr2 then
      inter qs1 s2
    else if addr1 > addr2 then
      inter s1 qs2
    else
      let ss = ss1 land ss2 in
      let qs = inter qs1 qs2 in
      if ss = 0
      then qs
      else if ss = ss2 && qs == qs2
      then s2
      else if ss = ss1 && qs == qs1
      then s1
      else C (addr1, ss, qs)

exception Found of int

let choose s =
  try
    iter (fun x ->
        raise (Found x)
      ) s;
    raise Not_found
  with Found x ->
    x

let minimum s =
  try
    iter (fun x ->
        raise (Found x)
      ) s;
    None
  with Found x ->
    Some x

let rec maximum = function
  | N -> None
  | C (addr, ss, N) ->
    let i = ref 0 in
    let ss = ref (ss lsr 1) in
    while !ss > 0 do
      incr i;
      ss := !ss lsr 1
    done;
    Some (addr + !i)
  | C (_, _, rest) ->
    maximum rest

let rec compare s1 s2 =
  if s1 == s2 then 0 else
    match s1, s2 with
      N, N ->  0
    | _, N ->  1
    | N, _ -> -1
    | C (addr1, ss1, qs1), C (addr2, ss2, qs2) ->
      if addr1 < addr2 then -1
      else if addr1 > addr2 then 1
      else if ss1 < ss2 then -1
      else if ss1 > ss2 then 1
      else compare qs1 qs2

let equal s1 s2 =
  compare s1 s2 = 0

let rec disjoint s1 s2 =
  match s1, s2 with
  | N, _
  | _, N ->
    true
  | C (addr1, ss1, qs1), C (addr2, ss2, qs2) ->
    if addr1 = addr2 then
      if (ss1 land ss2) = 0 then
        disjoint qs1 qs2
      else
        false
    else if addr1 < addr2 then
      disjoint qs1 s2
    else
      disjoint s1 qs2

let rec diff s1 s2 =
  match s1, s2 with
  | N, _ | _, N -> s1
  | C (addr1, ss1, qs1), C (addr2, ss2, qs2) ->
    if addr1 < addr2
    then C (addr1, ss1, diff qs1 s2)
    else if addr1 > addr2
    then diff s1 qs2
    else
      let ss = ss1 land lnot ss2 in
      let d = diff qs1 qs2 in
      if ss = 0
      then d
      else C (addr1, ss, d)

let lsb x = (x land -x)
let compare_lsb x y = lsb x - lsb y

let compare_minimum s1 s2 =
  match s1, s2 with
  | N, N -> 0
  | N, _ -> -1
  | _, N -> 1
  | C (addr1, ss1, _), C (addr2, ss2, _) ->
    match Int.compare addr1 addr2 with
    | 0 ->
      begin match compare_lsb ss1 ss2 with
        | 0 ->
          let ss1' = ss1 land lnot ss2 in
          let ss2' = ss2 land lnot ss1 in
          compare_lsb ss1' ss2'
        | n -> n
      end
    | n -> n

let sorted_union xs = List.fold_right union xs empty

let rec extract_unique_prefix addr2 ss2 = function
  | N -> N, N
  | C (addr1, ss1, qs1) as self ->
    if addr1 < addr2 then
      let prefix, suffix = extract_unique_prefix addr2 ss2 qs1 in
      C (addr1, ss1, prefix), suffix
    else if addr1 > addr2 || ss1 = ss2 || compare_lsb ss1 ss2 >= 0 then
      N, self
    else
      (* l and r have the same address, and
         l has some prefix that is not part of r (lsb l < lsb r)*)
      let prefix_mask = (lsb ss2) - 1 in
      let ss0 = ss1 land prefix_mask in
      assert (ss0 <> 0);
      let ss1 = ss1 land lnot prefix_mask in
      if ss1 = 0 then
        (C (addr1, ss0, N), qs1)
      else
        (C (addr1, ss0, N), C (addr1, ss1, qs1))

let extract_unique_prefix l r =
  match l, r with
  | N, _ -> N, N
  | _, N -> invalid_arg "extract_unique_prefix: r < l"
  | l, C (addr2, ss2, _) -> extract_unique_prefix addr2 ss2 l

let rec extract_shared_prefix = function
  | C (addr1, ss1, qs1), C (addr2, ss2, qs2)
    when addr1 = addr2 ->
    if ss1 = ss2 then
      let common, rest = extract_shared_prefix (qs1, qs2) in
      (C (addr1, ss1, common), rest)
    else
      let ss1' = ss1 land lnot ss2 in
      let ss2' = ss2 land lnot ss1 in
      let common_mask = (lsb ss1' - 1) land (lsb ss2' - 1) in
      let rest_mask = lnot common_mask in
      let common = match ss1 land common_mask with
        | 0 -> N
        | n -> C (addr1, n, N)
      in
      let qs1' = match ss1 land rest_mask with
        | 0 -> qs1
        | ss1' -> C (addr1, ss1', qs1)
      in
      let qs2' = match ss2 land rest_mask with
        | 0 -> qs2
        | ss2' -> C (addr2, ss2', qs2)
      in
      common, (qs1', qs2')
  | (l, r) -> N, (l, r)

let extract_shared_prefix l r = extract_shared_prefix (l, r)

let of_list xs = List.fold_left (fun xs x -> add x xs) empty xs

let init_interval i j =
  let i, j = if i < j then i, j else j, i in
  let addr = j - j mod word_size in
  if addr <= i then
    let word = (1 lsl (j - i + 1) - 1) lsl (i - addr) in
    C (addr, word, N)
  else
    let rec loop acc addr =
      if addr <= i
      then C (addr, -1 lsl (i - addr), acc)
      else loop (C (addr, -1, acc)) (addr - word_size)
    in
    loop (C (addr, (-1) lsr (word_size - (j - addr + 1)), N)) (addr - word_size)

let init_subset i j f =
  let i, j = if i < j then i, j else j, i in
  let rec loop i addr =
    if addr > j then N else
      let addr' = addr + word_size in
      let k = if j < addr' then j else (addr' - 1) in
      let word = ref 0 in
      for i = i to k do
        if f i then word := !word lor (1 lsl (i - addr))
      done;
      let word = !word in
      if word = 0
      then loop addr' addr'
      else C (addr, word, loop addr' addr')
  in
  loop i (i - i mod word_size)

let rec filter f = function
  | N -> N
  | C (addr, word0, ss) ->
    let word = ref 0 in
    for i = 0 to word_size - 1 do
      if word0 land (1 lsl i) <> 0 && f (addr + i) then
        word := !word lor (1 lsl i)
    done;
    if !word = 0 then
      filter f ss
    else
      C (addr, !word, filter f ss)

let rec allocate result = function
  | N ->
    result := 0;
    C (0, 1, N)

  | C (addr, -1, N) ->
    let next = addr + word_size in
    result := next;
    C (addr, -1, C (next, 1, N))

  | C (addr, -1, qs) ->
    C (addr, -1, allocate result qs)

  | C (addr, word, qs) ->
    let i = ref 0 in
    while word land (1 lsl !i) <> 0
    do incr i done;
    result := addr + !i;
    C (addr, word lor (1 lsl !i), qs)

let allocate qs =
  let result = ref 0 in
  let qs' = allocate result !qs in
  qs := qs';
  !result
