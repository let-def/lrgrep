type mark = {
  mutable used: bool;
}

let new_mark () = { used = false }

let is_used t = t.used

let is_unused t = not (is_used t)

type set =
  | Empty
  | Leaf of mark
  | Join of {s1: set; s2: set; mutable used: bool}

let empty = Empty

let singleton mark = Leaf mark

let join s1 s2 =
  if s1 == s2 then s1 else
    match s1, s2 with
    | Empty, s
    | s, Empty -> s
    | _ -> Join {s1; s2; used=false}

let rec mark_used = function
  | Empty -> ()
  | Leaf m -> m.used <- true
  | Join j ->
    if not j.used then (
      j.used <- true;
      mark_used j.s1;
      mark_used j.s2;
    )
