type ('a, 'b) piece = ('a IndexSet.t * 'b IndexSet.t)
type ('a, 'b) t = ('a, 'b) piece list

let minimum = []

let is_minimum = function
  | [] -> true
  | _ -> false

let is_empty dom img = IndexSet.is_empty dom || IndexSet.is_empty img

let image f x =
  let rec loop x = function
    | [] -> IndexSet.empty
    | (dom, img) :: _ when IndexSet.mem x dom -> img
    | _ :: xs -> loop x xs
  in
  loop x f

let piece dom img = if is_empty dom img then [] else [dom, img]

let cons (dom, img) xs =
  let dom = ref dom in
  let rec find = function
    | [] -> raise Not_found
    | (dom', img') :: rest when IndexSet.equal img img'  ->
      dom := IndexSet.union !dom dom';
      rest
    | x :: xs ->
      x :: find xs
  in
  let xs =
    match find xs with
    | xs -> xs
    | exception Not_found -> xs
  in
  (!dom, img) :: xs

let piecewise pieces =
  let pieces = List.filter (fun (dom, img) -> not (is_empty dom img)) pieces in
  let pieces = IndexRefine.annotated_partition pieces in
  List.fold_right (fun (dom, imgs) acc ->
    (dom, List.fold_left IndexSet.union IndexSet.empty imgs) :: acc
  ) pieces []

let map f l = piecewise (List.concat_map f l)

let from dom f =
  piecewise (
    IndexSet.fold_right (fun acc point ->
      let img = f point in
      if IndexSet.is_empty img then acc else
        match acc with
        | (dom', img') :: rest when IndexSet.equal img img' ->
          (IndexSet.add point dom', img') :: rest
        | acc -> (IndexSet.singleton point, img) :: acc
    ) [] dom
  )

let to_list x = x

let count = ref 0

let insert dom img xs =
  let rec loop xs =
    match xs with
    | [] -> [dom, img]
    | (dom', _) :: _ when IndexSet.compare_minimum dom dom' < 0 ->
      (dom, img) :: xs
    | x :: rest ->
      x :: loop rest
  in
  if IndexSet.is_empty dom then
    xs
  else
    loop xs

let increase ?(ignore=IndexSet.empty) f g =
  let rec visit rf rg f g =
    match f, g with
    | [], _ -> (List.fold_right cons rf g, List.fold_right cons rg g)
    | _, [] -> (List.fold_right cons rf f, List.fold_right cons rg g)
    | (fd, fi) :: f', (gd, gi) :: g' ->
      let c = IndexSet.compare_minimum fd gd in
      if c < 0 then (
        if IndexSet.is_empty fd then
          visit rf rg f' g
        else
          let fd0, fd = IndexSet.extract_unique_prefix fd gd in
          let rf = (fd0, fi) :: rf in
          visit rf rg (insert fd fi f') g
      ) else if c > 0 then (
        if IndexSet.is_empty gd then
          visit rf rg f g'
        else
          let gd0, gd = IndexSet.extract_unique_prefix gd fd in
          let d = (gd0, gi) in
          let rf = d :: rf in
          let rg = d :: rg in
          visit rf rg f (insert gd gi g')
      ) else (
        if IndexSet.is_empty fd then
          visit rf rg f' g'
        else
          let d0, (fd, gd) = IndexSet.extract_shared_prefix fd gd in
          let rf = (d0, IndexSet.union gi fi) :: rf in
          let gi' = IndexSet.diff gi fi in
          let rg = if IndexSet.is_empty gi' then rg else (d0, gi') :: rg in
          visit rf rg (insert fd fi f') (insert gd gi g')
      )
  in
  let g =
    if IndexSet.is_empty ignore
    then g
    else List.filter_map (fun (dom, img) ->
      let img = IndexSet.diff img ignore in
      if IndexSet.is_empty img
      then None
      else Some (dom, img)
    ) g
  in
  let f', df' = visit [] [] f g in
  incr count;
  let p f =
    Misc.string_concat_map ~wrap:("{","}") ", "
      (fun (a, b) ->
        Printf.sprintf "(%s -> %s)"
          (Misc.string_of_indexset a)
          (Misc.string_of_indexset b))
      f
  in
  if (!count >= 20000) && not (is_minimum df') then (
    count := 0;
    Printf.eprintf "increase (ignore:%s) delta:\n%s\n/\\ %s\n=  %s\n/  %s\n"
      (Misc.string_of_indexset ignore)
      (p f)
      (p g)
      (p f')
      (p df')
  );
  if is_minimum df' && f <> f' then (
    Printf.eprintf "increase expecting noop:\n%s\n%s\n%s\n"
      (p f)
      (p g)
      (p f');
  );
  (f', df')
