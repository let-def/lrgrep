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
