open Fix.Indexing

type set = int
type 'a set_array = 'a array

type loc = int
type 'a loc_array = 'a array

type 'a t = {
  mutable set_count: set;
  element  : 'a index loc_array;
  location : loc array; (* L *)
  set_of   : set array; (* S *)
  first    : loc set_array; (* F *)
  past     : loc set_array; (* P *)
  marked   : int set_array; (* M *)
  mutable worklist: set list;
}

let create (type a) ?partition (set : a cardinal) =
  let id x = x in
  let undefined = 0 in
  let n = cardinal set in
  let t = {
    set_count = if n = 0 then 0 else 1;
    element  = Vector.as_array (Vector.init set (fun x -> x));
    location = Array.init n id;
    set_of = Array.make n 0;
    first = Array.make n undefined;
    past = Array.make n undefined;
    marked = Array.make (n+1) 0;
    worklist = []
  } in
  begin match partition with
    | None ->
      if n > 0 then (
        t.first.(0) <- 0;
        t.past.(0) <- n;
      );
    | Some cmp ->
      Array.sort cmp t.element;
      let part = ref t.element.(0) in
      t.first.(0) <- 0;
      let set_count = ref 0 in
      for i = 0 to n - 1 do
        let elt = t.element.(i) in
        if cmp !part elt <> 0 then (
          t.past.(!set_count) <- i;
          incr set_count;
          t.first.(!set_count) <- i;
          part := elt
        );
        t.set_of.((elt :> int)) <- !set_count;
        t.location.((elt :> int)) <- i
      done;
      t.past.(!set_count) <- n;
      t.set_count <- !set_count + 1;
  end;
  t

let mark (t : 'a t) element =
  let element' : 'a index :> int = element in
  let set = t.set_of.(element') in
  if set > -1 then (
    let loc_unmarked = t.first.(set) + t.marked.(set) in
    let loc = t.location.(element') in
    if loc >= loc_unmarked then (
      (*prerr_endline ("marking " ^ string_of_int element' ^
                     " (in set " ^ string_of_int set ^ ")");*)
      if loc > loc_unmarked then (
        let elt_unmarked = t.element.(loc_unmarked) in
        t.element.(loc) <- elt_unmarked;
        t.location.((elt_unmarked : _ index :> int)) <- loc;
        t.element.(loc_unmarked) <- element;
        t.location.(element') <- loc_unmarked;
      );
      if t.marked.(set) = 0 then
        t.worklist <- set :: t.worklist;
      t.marked.(set) <- t.marked.(set) + 1
    )
  )


let split t =
  let worklist = t.worklist in
  t.worklist <- [];
  List.iter (fun set ->
      let j = t.first.(set) + t.marked.(set) in
      if j = t.past.(set) then t.marked.(set) <- 0 else (
        if t.marked.(set) <= t.past.(set) - j then (
          t.first.(t.set_count) <- t.first.(set);
          t.past.(t.set_count) <- j;
          t.first.(set) <- j;
        ) else (
          t.past.(t.set_count) <- t.past.(set);
          t.first.(t.set_count) <- j;
          t.past.(set) <- j;
        );
        for i = t.first.(t.set_count) to t.past.(t.set_count) - 1 do
          t.set_of.((t.element.(i) : _ index :> int)) <- t.set_count
        done;
        t.marked.(set) <- 0;
        t.marked.(t.set_count) <- 0;
        t.set_count <- t.set_count + 1
      )
    ) worklist

let discard_unmarked t =
  t.worklist <- [];
  for set = 0 to t.set_count - 1 do
    let first_unmarked = t.first.(set) + t.marked.(set) in
    for i = first_unmarked to t.past.(set) - 1 do
      let elt = (t.element.(i) : _ index :> int) in
      (*prerr_endline ("discarding " ^ string_of_int elt);*)
      t.set_of.(elt) <- -1
    done;
    t.past.(set) <- first_unmarked;
    t.marked.(set) <- 0
  done

let discard t f =
  for set = 0 to t.set_count - 1 do
    for i = t.first.(set) to t.past.(set) - 1 do
      let elt = t.element.(i) in
      if not (f elt) then
        mark t elt
    done
  done;
  discard_unmarked t

let set_count t = t.set_count

let set_of (t : 'a t) elt = t.set_of.((elt : 'a index :> int))

let choose t set =
  assert (t.first.(set) < t.past.(set));
  t.element.(t.first.(set))

let choose_opt t set =
  if t.first.(set) < t.past.(set) then
    Some t.element.(t.first.(set))
  else
    None

let iter_elements t set f =
  for i = t.first.(set) to t.past.(set) - 1 do
    f t.element.(i)
  done

let iter_marked_elements t set f =
  let last = ref (t.first.(set)-1) in
  while !last < t.first.(set) + t.marked.(set) - 1 do
    let goal = t.first.(set) + t.marked.(set) - 1 in
    for i = !last + 1 to goal do
      f t.element.(i)
    done;
    last := goal
  done

let marked_sets t = t.worklist

let clear_marks t =
  let worklist = t.worklist in
  t.worklist <- [];
  List.iter (fun set -> t.marked.(set) <- 0) worklist

let is_first t n =
  let n = (n : 'n index :> int) in
  let s = t.set_of.(n) in
  let loc = t.location.(n) in
  (s > -1 && loc = t.first.(s) && loc < t.past.(s))
