module Increasing = struct
  type ('a, 'b) piece = ('a IndexSet.t * 'b IndexSet.t)
  type ('a, 'b) t = ('a, 'b) piece list

  let minimum = []

  let is_minimum = function
    | [] -> true
    | _ -> false

  let piece dom img =
    if IndexSet.is_empty dom || IndexSet.is_empty img then
      []
    else
      [dom, img]

  let rec insert dom img = function
    | [] -> [dom, img]
    | ((dom', img') as x :: xs) as xxs ->
      let c = IndexSet.compare img img' in
      if c < 0 then
        (dom, img) :: xxs
      else if c = 0 then
        (IndexSet.union dom dom', img') :: xs
      else
        x :: insert dom img xs

  let rec merge xs ys = match xs, ys with
    | [], result | result, [] -> result
    | ((xd, xi) as x) :: xs', ((yd, yi) as y) :: ys' ->
      let c = IndexSet.compare xi yi in
      if c < 0 then
        x :: merge xs' ys
      else if c = 0 then
        (IndexSet.union xd yd, xi) :: merge xs' ys'
      else
        y :: merge xs ys'

  let cons dom img xs =
    if IndexSet.is_empty dom then
      xs
    else
      (dom, img) :: xs

  let increase ?(ignore=IndexSet.empty) f g =
    let g = ref g in
    let pieces = ref [] in
    let rec visit_g dom0 img0 = function
      | g when IndexSet.is_empty !dom0 -> g
      | [] -> []
      | (dom', img') as x :: g' ->
        let dom = IndexSet.inter !dom0 dom' in
        if IndexSet.is_empty dom then
          x :: visit_g dom0 img0 g'
        else
          let img' = IndexSet.diff img' ignore in
          if not (IndexSet.is_empty img') then
            let img = IndexSet.union img0 img' in
            if not (IndexSet.equal img img0) then (
              pieces := insert dom img !pieces;
              dom0 := IndexSet.diff !dom0 dom;
            );
            cons (IndexSet.diff dom' dom) img' (visit_g dom0 img0 g')
          else
            visit_g dom0 img0 g'
    in
    let rec visit_f = function
      | f when is_minimum !g -> f
      | [] -> []
      | (dom, img as x) :: f' ->
        let dom0 = ref dom in
        g := visit_g dom0 img !g;
        if !dom0 == dom then
          x :: visit_f f'
        else
          cons !dom0 img (visit_f f')
    in
    let f = visit_f f in
    let g =
      if IndexSet.is_empty ignore
      then !g
      else List.map (fun (dom, img') -> dom, IndexSet.diff img' ignore) !g
    in
    let df = merge !pieces g in
    (merge f df, df)

  let image f x =
    let rec loop x = function
      | [] -> IndexSet.empty
      | (dom, img) :: _ when IndexSet.mem x dom -> img
      | _ :: xs -> loop x xs
    in
    loop x f

  let union_all = function
    | [] -> IndexSet.empty
    | hd :: tl -> List.fold_left IndexSet.union hd tl

  let compare_image (_, a) (_, b) =
    IndexSet.compare a b

  let piecewise pieces =
    let pieces = IndexRefine.annotated_partition pieces in
    let pieces =
      List.filter_map (fun (dom, imgs) ->
        let img = union_all imgs in
        if IndexSet.is_empty img
        then None
        else Some (dom, img)) pieces
    in
    let pieces = List.sort compare_image pieces in
    let rec merge = function
      | [] -> []
      | (dom1, img1) :: (dom2, img2) :: rest when IndexSet.equal img1 img2 ->
        merge ((IndexSet.union dom1 dom2, img1) :: rest)
      | (dom, img) :: xs when IndexSet.is_empty dom
        || IndexSet.is_empty img ->
        merge xs
      | x :: xs -> x :: merge xs
    in
    merge pieces

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

  let map f l = piecewise (List.map f l)
end

module Decreasing = struct
  type ('a, 'b) piece = ('a IndexSet.t * 'b IndexSet.t)
  type ('a, 'b) t = ('a, 'b) piece list

  let maximum = []

  let is_maximum = function
    | [] -> true
    | _ -> false

  let piece dom img =
    if IndexSet.is_empty dom then
      []
    else
      [dom, img]

  let rec insert dom img = function
    | [] -> [dom, img]
    | ((dom', img') as x :: xs) as xxs ->
      let c = IndexSet.compare img img' in
      if c < 0 then
        (dom, img) :: xxs
      else if c = 0 then
        (IndexSet.union dom dom', img') :: xs
      else
        x :: insert dom img xs

  let rec merge xs ys = match xs, ys with
    | [], result | result, [] -> result
    | ((xd, xi) as x) :: xs', ((yd, yi) as y) :: ys' ->
      let c = IndexSet.compare xi yi in
      if c < 0 then
        x :: merge xs' ys
      else if c = 0 then
        (IndexSet.union xd yd, xi) :: merge xs' ys'
      else
        y :: merge xs ys'

  let cons dom img xs =
    if IndexSet.is_empty dom then
      xs
    else
      (dom, img) :: xs

  let decrease f g =
    let g = ref g in
    let pieces = ref [] in
    let rec visit_g dom0 img0 = function
      | g when IndexSet.is_empty !dom0 -> g
      | [] -> []
      | (dom', img') as x :: g' ->
        let dom = IndexSet.inter !dom0 dom' in
        if IndexSet.is_empty dom then
          x :: visit_g dom0 img0 g'
        else
          let img = IndexSet.inter img0 img' in
          if not (IndexSet.equal img img0) then (
            pieces := insert dom img !pieces;
            dom0 := IndexSet.diff !dom0 dom;
          );
          cons (IndexSet.diff dom' dom) img' (visit_g dom0 img0 g')
    in
    let rec visit_f = function
      | f when is_maximum !g -> f
      | [] -> []
      | (dom, img as x) :: f' ->
        let dom0 = ref dom in
        g := visit_g dom0 img !g;
        if !dom0 == dom then
          x :: visit_f f'
        else
          cons !dom0 img (visit_f f')
    in
    let f = visit_f f in
    let df = merge !pieces !g in
    (merge f df, df)

  let image f x =
    let rec loop x = function
      | [] -> None
      | (dom, img) :: _ when IndexSet.mem x dom -> Some img
      | _ :: xs -> loop x xs
    in
    loop x f

  let compare_image (_, a) (_, b) =
    IndexSet.compare a b

  let piecewise pieces =
    let pieces = IndexRefine.annotated_partition pieces in
    let pieces =
      List.map
        (fun (dom, imgs) ->
          (dom, List.fold_left IndexSet.inter (List.hd imgs) (List.tl imgs)))
        pieces
    in
    let pieces = List.sort compare_image pieces in
    let rec merge = function
      | [] -> []
      | (dom1, img1) :: (dom2, img2) :: rest when IndexSet.equal img1 img2 ->
        merge ((IndexSet.union dom1 dom2, img1) :: rest)
      | (dom, _) :: xs when IndexSet.is_empty dom ->
        merge xs
      | x :: xs -> x :: merge xs
    in
    merge pieces

  let to_list x = x

  let map f l = piecewise (List.map f l)
end
