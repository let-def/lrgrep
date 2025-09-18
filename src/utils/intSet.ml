module type S = SetSig.S0

(* A compressed (or should we say sparse?) bit set is a list of pairs
     of integers. The first component of every pair is an index, while
     the second component is a bit field. The list is sorted by order
     of increasing indices. *)

type t =
  | N
  | C of {
      addr: int;
      mask: int;
      mutable next: t;
    }


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

let c (addr, mask, next) = C {addr; mask; next}

let add i s =
  let ioffset = i mod word_size in
  let iaddr = i - ioffset
  and imask = 1 lsl ioffset in
  let rec add = function
    | N ->
      (* Insert at end. *)
      c (iaddr, imask, N)
    | C {addr; mask=ss; next= qs} as s ->
      if iaddr < addr then
        (* Insert in front. *)
        c (iaddr, imask, s)
      else if iaddr = addr then
        (* Found appropriate cell, update bit field. *)
        let ss' = ss lor imask in
        if ss' = ss then
          s
        else
          c (addr, ss', qs)
      else
        (* Not there yet, continue. *)
        let qs' = add qs in
        if qs == qs' then
          s
        else
          c (addr, ss, qs')
  in
  add s

let split i s =
  let ioffset = i mod word_size in
  let iaddr = i - ioffset
  and imask = 1 lsl ioffset in
  let rec split = function
    | N ->
      (N, false, N)
    | C {addr; mask=ss; next=qs} as s ->
      if iaddr < addr then
        (* Stop now. *)
        (N, false, s)
        
      else if iaddr = addr then
        (* Found appropriate cell, split bit field. *)
        let found = ss land imask <> 0 in
        let l_mask = imask - 1 in
        let l =
          match ss land l_mask with
          | 0 -> N
          | ss_l -> c (addr, ss_l, N)
        in
        let r =
          match ss land lnot (l_mask lor imask) with
          | 0 -> N
          | ss_r -> c (addr, ss_r, qs)
        in
        (l, found, r)
      else
        (* Not there yet, continue. *)
        let (l, f, r) = split qs in
        (c (addr, ss, l), f, r)
  in
  split s

let singleton i =
  add i N

let remove i s =
  let ioffset = i mod word_size in
  let iaddr = i - ioffset
  and imask = 1 lsl ioffset in
  let rec remove = function
    | N ->
      N
    | C {addr; mask=ss; next=qs} as s ->
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
          c (addr, ss', qs)
      else
        (* Not there yet, continue. *)
        let qs' = remove qs in
        if qs == qs' then
          s
        else
          c (addr, ss, qs')
  in
  remove s

let rec fold f s accu =
  match s with
  | N ->
    accu
  | C {addr=base; mask=ss; next=qs} ->
    let ss' = ref ss in
    let accu = ref accu in
    for _ = 0 to Bit_lib.pop_count ss - 1 do
      let bit = Bit_lib.lsb_index !ss' in
      accu := f (base + bit) !accu;
      ss' := !ss' lxor (1 lsl bit);
    done;
    fold f qs !accu

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
  | C {addr=base; mask=ss; next=qs} ->
    rev_iter f qs;
    let ss' = ref ss in
    for _ = 0 to Bit_lib.pop_count ss - 1 do
      let bit = Bit_lib.msb_index !ss' in
      f (base + bit);
      ss' := !ss' lxor (1 lsl bit);
    done

let rec fold_right f acc = function
  | N -> acc
  | C {addr=base; mask=ss; next=qs} ->
    let acc = ref (fold_right f acc qs) in
    let ss' = ref ss in
    for _ = 0 to Bit_lib.pop_count ss - 1 do
      let bit = Bit_lib.msb_index !ss' in
      acc := f !acc (base + bit);
      ss' := !ss' lxor (1 lsl bit);
    done;
    !acc

let exists f t =
  let exception Found in
  match fold (fun elt () -> if f elt then raise Found) t () with
  | () -> false
  | exception Found -> true

let is_singleton s =
  match s with
  | C {addr=_; mask=ss; next=N} ->
    (* Test whether only one bit is set in [ss]. We do this by turning
       off the rightmost bit, then comparing to zero. *)
    ss land (ss - 1) = 0
  | C {addr=_; mask=_; next=C _}
  | N ->
    false

let rec cardinal acc = function
  | N -> acc
  | C {addr=_; mask; next} ->
    cardinal (acc + Bit_lib.pop_count mask) next

let cardinal qs = cardinal 0 qs

let elements s =
  fold_right (fun tl hd -> hd :: tl) [] s

let rec subset s1 s2 =
  match s1, s2 with
  | N, _ ->
    true
  | _, N ->
    false
  | C c1, C c2 ->
    if c1.addr < c2.addr then
      false
    else if c1.addr = c2.addr then
      if (c1.mask land c2.mask) <> c1.mask then
        false
      else
        subset c1.next c2.next
    else
      subset s1 c2.next

let rec quick_subset a1 ss1 = function
  | N -> false
  | C c ->
    if a1 = c.addr then
      ss1 land c.mask <> 0
    else
      (a1 > c.addr && quick_subset a1 ss1 c.next)

let quick_subset s1 s2 =
  match s1 with
  | N -> true
  | C c ->
    (* We know that, by construction, ss1 is not empty.
       It suffices to test s2 also has elements in common with ss1 at address
       a1 to determine the quick_subset relation. *)
    quick_subset c.addr c.mask s2

let mem i s =
  let ioffset = i mod word_size in
  let iaddr = i - ioffset and imask = 1 lsl ioffset in
  let rec loop4 = function
    | C c when c.addr < iaddr -> loop4 c.next
    | C c when c.addr = iaddr -> c.mask land imask != 0
    | _ -> false
  in
  loop4 s

let rec union s1 s2 =
  match s1, s2 with
  | N, s
  | s, N ->
    s
  | C {addr=addr1; mask=ss1; next=qs1},
    C {addr=addr2; mask=ss2; next=qs2} ->
    if addr1 < addr2 then
      let qs = union qs1 s2 in
      if qs == qs1
      then s1
      else c (addr1, ss1, qs)
    else if addr1 > addr2 then
      let qs = union s1 qs2 in
      if qs == qs2
      then s2
      else c (addr2, ss2, qs)
    else
      let ss = ss1 lor ss2 in
      let qs = union qs1 qs2 in
      if ss = ss2 && qs == qs2
      then s2
      else if ss = ss1 && qs == qs1
      then s1
      else c (addr1, ss, qs)

let rec inter s1 s2 =
  match s1, s2 with
  | N, _
  | _, N ->
    N
  | C {addr=addr1; mask=ss1; next=qs1},
    C {addr=addr2; mask=ss2; next=qs2} ->
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
      else c (addr1, ss, qs)

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
  | C {addr; mask; next=N} ->
    let i = ref 0 in
    let ss = ref (mask lsr 1) in
    while !ss > 0 do
      incr i;
      ss := !ss lsr 1
    done;
    Some (addr + !i)
  | C c ->
    maximum c.next

let rec compare s1 s2 =
  if s1 == s2 then 0 else
    match s1, s2 with
      N, N ->  0
    | _, N ->  1
    | N, _ -> -1
    | C c1, C c2 ->
      if c1.addr < c2.addr then -1
      else if c1.addr > c2.addr then 1
      else if c1.mask < c2.mask then -1
      else if c1.mask > c2.mask then 1
      else compare c1.next c2.next

let equal s1 s2 =
  compare s1 s2 = 0

let rec disjoint s1 s2 =
  match s1, s2 with
  | N, _
  | _, N ->
    true
  | C {addr=addr1; mask=ss1; next=qs1},
    C {addr=addr2; mask=ss2; next=qs2} ->
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
  | C {addr=addr1; mask=ss1; next=qs1},
    C {addr=addr2; mask=ss2; next=qs2} ->
    if addr1 < addr2 then (
      let qs1' = diff qs1 s2 in
      if qs1' == qs1 then
        s1
      else
        c (addr1, ss1, qs1')
    )
    else if addr1 > addr2 then
      diff s1 qs2
    else
      let ss = ss1 land lnot ss2 in
      if ss = 0 then
        diff qs1 qs2
      else
        let qs1' = diff qs1 qs2 in
        if ss = ss1 && qs1' == qs1 then
          s1
        else
          c (addr1, ss, qs1')

let lsb x = (x land -x)

let compare_lsb x y = Int.compare (lsb x - 1) (lsb y - 1)

let compare_minimum s1 s2 =
  match s1, s2 with
  | N, N -> 0
  | N, _ -> -1
  | _, N -> 1
  | C c1, C c2 ->
    match Int.compare c1.addr c2.addr with
    | 0 -> compare_lsb c1.mask c2.mask
    | n -> n

let sorted_union xs = List.fold_right union xs empty

let rec extract_unique_prefix addr2 ss2 = function
  | N -> N, N
  | C c1 as self ->
    if c1.addr < addr2 then
      let prefix, suffix = extract_unique_prefix addr2 ss2 c1.next in
      C {c1 with next = prefix}, suffix
    else if c1.addr > addr2 || c1.mask = ss2 || compare_lsb c1.mask ss2 >= 0 then
      N, self
    else
      (* l and r have the same address, and
         l has some prefix that is not part of r (lsb l < lsb r)*)
      let prefix_mask = (lsb ss2) - 1 in
      let ss0 = c1.mask land prefix_mask in
      assert (ss0 <> 0);
      let ss1 = c1.mask land lnot prefix_mask in
      if ss1 = 0 then
        (c (c1.addr, ss0, N), c1.next)
      else
        (c (c1.addr, ss0, N), c (c1.addr, ss1, c1.next))

let extract_unique_prefix l r =
  match l, r with
  | N, _ -> N, N
  | _, N -> invalid_arg "extract_unique_prefix: r < l"
  | l, C c -> extract_unique_prefix c.addr c.mask l

let rec extract_shared_prefix = function
  | C c1, C c2 when c1.addr = c2.addr ->
    if c1.mask = c2.mask then
      let common, rest = extract_shared_prefix (c1.next, c2.next) in
      (C {c1 with next = common}, rest)
    else
      let ss1' = c1.mask land lnot c2.mask in
      let ss2' = c2.mask land lnot c1.mask in
      let common_mask = (lsb ss1' - 1) land (lsb ss2' - 1) in
      let rest_mask = lnot common_mask in
      let common = match c1.mask land common_mask with
        | 0 -> N
        | n -> C {c1 with mask = n; next = N}
      in
      let qs1' = match c1.mask land rest_mask with
        | 0 -> c1.next
        | ss1' -> C {c1 with mask = ss1'}
      in
      let qs2' = match c2.mask land rest_mask with
        | 0 -> c2.next
        | ss2' -> C {c2 with mask = ss2'}
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
    c (addr, word, N)
  else
    let rec loop2 acc addr =
      if addr <= i
      then c (addr, -1 lsl (i - addr), acc)
      else loop2 (c (addr, -1, acc)) (addr - word_size)
    in
    loop2 (c (addr, (-1) lsr (word_size - (j - addr + 1)), N)) (addr - word_size)

let init_subset i j f =
  let i, j = if i < j then i, j else j, i in
  let rec loop3 i addr =
    if addr > j then N else
      let addr' = addr + word_size in
      let k = if j < addr' then j else (addr' - 1) in
      let word = ref 0 in
      for i = i to k do
        if f i then word := !word lor (1 lsl (i - addr))
      done;
      let word = !word in
      if word = 0
      then loop3 addr' addr'
      else c (addr, word, loop3 addr' addr')
  in
  loop3 i (i - i mod word_size)

let rec filter f = function
  | N -> N
  | C {addr; mask; next} as ss0 ->
    let word = ref 0 in
    let word' = ref mask in
    for _ = 0 to Bit_lib.pop_count mask - 1 do
      let bit = Bit_lib.lsb_index !word' in
      if f (addr + bit) then
        word := !word lor (1 lsl bit);
      word' := !word' lxor (1 lsl bit);
    done;
    if !word = 0 then
      filter f next
    else
      let ss' = filter f next in
      if !word = mask && next == ss' then
        ss0
      else
        c (addr, !word, ss')

let rec find f = function
  | N -> raise Not_found
  | C c -> find_addr f c.addr c.mask c.next 0

and find_addr f a w ss i =
  if w land (1 lsl i) <> 0 && f (a + i) then
    (a + i)
  else if i = word_size - 1 then
    find f ss
  else
    find_addr f a w ss (i + 1)

let rec find_map f = function
  | N -> None
  | C c -> find_map_addr f c.addr c.mask c.next 0

and find_map_addr f a w ss i =
  match if w land (1 lsl i) = 0 then None else f (a + i) with
  | Some _ as result -> result
  | None when i = word_size - 1 -> find_map f ss
  | None -> find_map_addr f a w ss (i + 1)

let rec allocate result = function
  | N ->
    result := 0;
    c (0, 1, N)

  | C {addr; mask= -1; next=N} ->
    let next = addr + word_size in
    result := next;
    c (addr, -1, c (next, 1, N))

  | C {addr; mask= -1; next=qs} ->
    c (addr, -1, allocate result qs)

  | C {addr; mask; next} ->
    let i = Bit_lib.lsb_index (lnot mask) in
    result := addr + i;
    c (addr, mask lor (1 lsl i), next)

let allocate qs =
  let result = ref 0 in
  let qs' = allocate result !qs in
  qs := qs';
  !result

let rec to_seq q =
  match q with
  | N -> Seq.empty
  | C {addr; mask; next} ->
    c addr next mask

and c addr q' = function
  | 0 -> to_seq q'
  | mask ->
    let i = Bit_lib.lsb_index mask in
    fun () -> Seq.Cons (addr + i, c addr q' (mask lxor (1 lsl i)))

let bind m f = fold (fun elt acc -> union (f elt) acc) m empty
