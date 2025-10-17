type set =
  | Empty
  | Leaf of {mutable used: bool; id: int}
  | Join of {s1: set; s2: set; mutable used: bool; h: int}

type mark = set

let counter = ref 0

let id () =
  let r = !counter in
  counter := r + 1;
  r

let new_mark () = Leaf {used = false; id = id ()}

let is_used = function
  | Leaf l -> l.used
  | _ -> assert false

let is_unused t = not (is_used t)

let cmon_mark _ = Cmon.constant "<Usage.mark>"

let empty = Empty

let equal s1 s2 =
  s1 == s2

let rec compare s1 s2 =
  match s1, s2 with
  | Empty, Empty -> 0
  | Leaf l1, Leaf l2 -> Int.compare l1.id l2.id
  | Join j1, Join j2 ->
    let c = Int.compare j1.h j2.h in
    if c <> 0 then c else
      let c = compare j1.s1 j2.s1 in
      if c <> 0 then c else
        compare j1.s2 j2.s2
  | Empty, (Leaf _ | Join _) -> -1
  | Leaf _, Join _ -> -1
  | (Leaf _ | Join _), Empty -> +1
  | Join _, Leaf _ -> +1

let is_empty = function
  | Empty -> true
  | _ -> false

let singleton mark = mark

let h = function
  | Empty -> assert false
  | Leaf l -> l.id
  | Join j -> j.h

let join s1 s2 =
  if s1 == s2 then s1 else
    match s1, s2 with
    | Empty, s
    | s, Empty -> s
    | _ -> Join {s1; s2; used=false; h = Hashtbl.seeded_hash (h s1) (h s2)}

let rec mark_used = function
  | Empty -> ()
  | Leaf m -> m.used <- true
  | Join j when j.used -> ()
  | Join j ->
    j.used <- true;
    mark_used j.s1;
    mark_used j.s2

let cmon_set _ = Cmon.constant "<Usage.set>"
