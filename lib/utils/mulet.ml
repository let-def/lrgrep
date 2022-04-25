module type SIGMA = sig
  include Map.OrderedType
  val empty : t
  val full : t
  val is_empty : t -> bool
  val is_full : t -> bool
  val is_subset_of : t -> t -> bool
  val compl : t -> t
  val inter : t -> t -> t

  val partition : t list -> t list
end

module type MONOID = sig
  type t
  val empty : t
  val append : t -> t -> t
end

module type LABEL = sig
  include Map.OrderedType
  include MONOID with type t := t
end

type ('s, 'l, 'a) re =
  | Set of 's
  | Epsilon
  | Closure of ('s, 'l, 'a) re
  | Not     of ('s, 'l, 'a) re
  | Concat  of ('s, 'l, 'a) re list
  | Or      of ('s, 'l, 'a) re list
  | And     of ('s, 'l, 'a) re list
  | Label   of 'l
  | Abstract of 'a

let cmon_re (type s l a)
    ?(compare : (module Map.S with type key = (s, l, a) re) option)
    ~(set : s -> Cmon.t)
    ~(label : l -> Cmon.t)
    ~(abstract : a -> Cmon.t)
    (expr : (s, l, a) re)
  : Cmon.t =
  let traverse sub = function
    | Set s   -> Cmon.constructor "Set" (set s)
    | Epsilon -> Cmon.constant "Epsilon"
    | Closure e -> Cmon.constructor "Closure" (sub e)
    | Not     e -> Cmon.constructor "Not" (sub e)
    | Concat  es -> Cmon.construct "Concat" [Cmon.list_map sub es]
    | Or      es -> Cmon.construct "Or" [Cmon.list_map sub es]
    | And     es -> Cmon.construct "And" [Cmon.list_map sub es]
    | Label   l -> Cmon.construct "Label" [label l]
    | Abstract a -> Cmon.construct "Abstract" [abstract a]
  in
  match compare with
  | None -> let rec fix expr = traverse fix expr in fix expr
  | Some (module Map) ->
    let seen = ref Map.empty in
    let rec fix expr =
      try Map.find expr !seen
      with Not_found ->
        let result = traverse fix expr in
        seen := Map.add expr result !seen;
        result
    in
    traverse fix expr

module type DERIVABLE = sig
  type sigma
  type label

  include Map.OrderedType
  val is_empty : t -> bool
  val nullable : t -> bool
  val get_label : t -> label
  val fold_left_classes : t -> (sigma -> 'a -> 'a) -> 'a -> 'a
  val left_delta : t -> sigma -> label * (sigma, label, t) re
end

module Null_derivable = struct
  type t = { void : 'a. 'a }
  let compare _ _ = 0
  let is_empty t = t.void
  let nullable t = t.void
  let get_label t = t.void
  let fold_left_classes t = t.void
  let left_delta t _ = t.void
end

module type S = sig
  type sigma
  type label
  type abstract

  module Expr : sig
    type t = (sigma, label, abstract) re
    val empty : t
    val epsilon : t
    val star : t -> t
    val set : sigma -> t
    val ( ^. ) : t -> t -> t
    val ( &. ) : t -> t -> t
    val ( |. ) : t -> t -> t
    val disjunction : t list -> t
    val conjunction : t list -> t
    val concatenation : t list -> t
    val abstract : abstract -> t
    val compl : t -> t
    val label : label -> t

    include Map.OrderedType with type t := t
    val is_empty : t -> bool
    val nullable : t -> bool
    val get_label : t -> label
    val fold_left_classes : t -> (sigma -> 'a -> 'a) -> 'a -> 'a
    val left_classes : t -> sigma list
    val left_delta : t -> sigma -> label * t
  end

  module Map : Map.S with type key = Expr.t
  type transition = sigma * label * Expr.t
  type dfa = transition list Map.t

  val derive : Expr.t -> transition list
  val add_to_dfa : dfa -> Expr.t list -> dfa
  val make_dfa : Expr.t -> dfa
end

module Make
    (Sigma : SIGMA)
    (Label : LABEL)
    (Abstract : DERIVABLE with type sigma := Sigma.t
                           and type label := Label.t) =
struct
  type sigma = Sigma.t
  type label = Label.t
  type abstract = Abstract.t

  module Expr : sig
    type t = (sigma, label, abstract) re

    val empty : t
    val epsilon : t
    val star : t -> t
    val set : sigma -> t
    val ( ^. ) : t -> t -> t
    val ( &. ) : t -> t -> t
    val ( |. ) : t -> t -> t
    val disjunction : t list -> t
    val conjunction : t list -> t
    val concatenation : t list -> t
    val abstract : abstract -> t
    val compl : t -> t
    val label : label -> t

    include Map.OrderedType with type t := t
    val is_empty : t -> bool
    val nullable : t -> bool
    val get_label : t -> label
    val fold_left_classes : t -> (sigma -> 'a -> 'a) -> 'a -> 'a
    val left_classes : t -> sigma list
    val left_delta : t -> sigma -> label * t
  end = struct
    type t = (sigma, label, abstract) re

    let empty = Set Sigma.empty

    let not_empty = Not empty

    let is_empty = function
      | Set x -> Sigma.is_empty x
      | _ -> false

    let is_full = function
      | Not (Set x) -> Sigma.is_empty x
      | Closure (Set x) -> Sigma.is_full x
      | _ -> false

    let compare_tags = compare

    let rec compare x y =
      if x == y then 0
      else match x, y with
        | Set xs, Set ys -> Sigma.compare xs ys
        | Epsilon, Epsilon -> 0
        | Concat xs , Concat ys
        | Or     xs , Or     ys
        | And    xs , And    ys ->
          compare_list (xs, ys)
        | Closure x, Closure y -> compare x y
        | Not x, Not y -> compare x y
        | Label x, Label y -> Label.compare x y
        | Abstract x, Abstract y -> Abstract.compare x y
        | ( Set _ | Epsilon | Concat _ | Closure _
          | Or _ | And _ | Not _ | Label _ | Abstract _), _ ->
          compare_tags x y

    and compare_list = function
      | [], [] -> 0
      | [], (_ :: _)  -> -1
      | (_ :: _), []  -> 1
      | x :: xs, y :: ys ->
        match compare x y with
        | 0 -> compare_list (xs, ys)
        | n -> n

    type ord = Lt | Eq | Gt

    let ord x y =
      match compare x y with
      | 0 -> Eq
      | n when n < 0 -> Lt
      | _ -> Gt

    let epsilon  = Epsilon
    let set s    = Set s

    let rec insert x = function
      | [] -> [x]
      | (y :: ys) as yys ->
        match ord x y with
        | Lt -> x :: yys
        | Eq -> yys
        | Gt -> y :: insert x ys

    let rec merge_uniq = function
      | [], x | x, [] -> x
      | ((x :: xs) as xxs), ((y :: ys) as yys) ->
        match ord x y with
        | Lt -> x :: merge_uniq (xs, yys)
        | Gt -> y :: merge_uniq (xxs, ys)
        | Eq -> x :: merge_uniq (xs, ys)

    let (&.) a b = match a, b with
      | And xs, And ys -> And (merge_uniq (xs, ys))
      | x, y when is_empty x || is_empty y -> empty
      | x, y when is_full x -> y
      | x, y when is_full y -> x
      | And xs, y | y, And xs -> And (insert y xs)
      | x, y ->
        match ord x y with
        | Lt -> And [x; y]
        | Eq -> x
        | Gt -> And [y; x]

    let (|.) a b = match a, b with
      | Or xs, Or ys -> Or (merge_uniq (xs, ys))
      | x, y when is_full x || is_full y -> not_empty
      | x, y when is_empty x -> y
      | x, y when is_empty y -> x
      | Or xs, y | y, Or xs -> Or (insert y xs)
      | x, y ->
        match ord x y with
        | Lt -> Or [x; y]
        | Eq -> x
        | Gt -> Or [y; x]

    let re_seq x xs =
      match x, xs with
      | Label la, Label lb :: tl -> Label (Label.append la lb) :: tl
      | x, xs -> x :: xs

    let (^.) a b = match a, b with
      | Epsilon, x | x, Epsilon -> x
      | Label la, Label lb -> Label (Label.append la lb)
      | x, y when is_empty x || is_empty y -> empty
      | Concat xs, Concat ys ->
        Concat (List.fold_right re_seq xs ys)
      | x, Concat ys -> Concat (re_seq x ys)
      | x, y -> Concat (re_seq x [y])

    (*let (^.) a b =
      let cmon_unit _ = Cmon.unit in
      let cmon_re ppf re =
        Cmon.format ppf (cmon_re
                           ~set:(fun sg -> if Sigma.is_empty sg
                                  then Cmon.constant "0"
                                  else Cmon.unit)
                           ~label:cmon_unit ~abstract:cmon_unit re)
      in
      let result = a ^. b in
      Format.eprintf "(@[%a@]) ^. (@[%a@]) = (@[%a@])\n%!"
        cmon_re a cmon_re b cmon_re result;
      result*)

    let disjunction ts =
      let ts = List.sort compare ts in
      List.fold_right (|.) ts empty

    let conjunction ts =
      let ts = List.sort compare ts in
      List.fold_right (&.) ts (Closure (Set Sigma.full))

    let concatenation ts =
      List.fold_right (^.) ts epsilon

    let abstract a =
      if Abstract.is_empty a then empty else Abstract a

    let compl = function
      | Not x -> x
      | a -> Not a

    let star = function
      | Closure _ | Epsilon | Label _ as r -> r
      | x -> if is_empty x then Epsilon else Closure x

    let rec nullable = function
      | Epsilon -> true
      | Set _ -> false
      | Concat xs | And xs -> List.for_all nullable xs
      | Or xs -> List.exists nullable xs
      | Not x -> not (nullable x)
      | Closure _ -> true
      | Label _ -> true
      | Abstract a -> Abstract.nullable a

    let concat_tl = function
      | [] -> assert false
      | [x] -> x
      | xs -> Concat xs

    let left_delta re x =
      let labels = ref Label.empty in
      let rec delta pos = function
        | Set xs when Sigma.is_subset_of x xs -> epsilon
        | Set _ | Epsilon -> empty
        | Concat [] | Or [] | And [] -> assert false
        | Concat (r :: s) when nullable r ->
          let r' = delta pos r in
          let s = concat_tl s in
          let s' = delta pos s in
          ((r' ^. s) |. s')
        | Concat (r :: s) ->
          let r' = delta pos r in
          let s = concat_tl s in
          (r' ^. s)
        | Closure r as rs ->
          let r' = delta pos r in
          (r' ^. rs)
        | Or (x :: xs) ->
          List.fold_left (fun acc y -> acc |. delta pos y) (delta pos x) xs
        | And (x :: xs) ->
          List.fold_left (fun acc y -> acc &. delta pos y) (delta pos x) xs
        | Not r ->
          let r' = delta false r in
          compl r'
        | Label label ->
          if pos then labels := Label.append label !labels;
          empty
        | Abstract a ->
          let label, re = Abstract.left_delta a x in
          if pos then labels := Label.append label !labels;
          re
      in
      let result = delta true re in
      !labels, result

    let rec get_label acc = function
      | Set _ | Epsilon -> acc
      | Concat xs ->
        let rec aux acc = function
          | [] -> acc
          | x :: xs ->
            let acc = get_label acc x in
            if nullable x
            then aux acc xs
            else acc
        in
        aux acc xs
      | Closure r -> get_label acc r
      | Or xs | And xs -> List.fold_left get_label acc xs
      | Not _ -> acc
      | Label label -> Label.append label acc
      | Abstract a -> Label.append (Abstract.get_label a) acc

    let get_label re = get_label Label.empty re

    let label l = Label l

    let rec fold_left_classes f f' acc = function
      | Epsilon | Label _ -> f Sigma.full acc
      | Set s -> f s acc
      | Concat xs ->
        let rec aux acc = function
          | [] -> acc
          | x :: xs ->
            let acc = fold_left_classes f f' acc x in
            if nullable x
            then aux acc xs
            else acc
        in
        aux acc xs
      | Or xs | And xs ->
        List.fold_left (fun acc x -> fold_left_classes f f' acc x) acc xs
      | Closure r -> fold_left_classes f f' acc r
      | Not r -> fold_left_classes f' f acc r
      | Abstract a -> Abstract.fold_left_classes a f acc

    let fold_left_classes re f acc =
      let f' set acc = f (Sigma.compl set) acc in
      fold_left_classes f f' acc re

    let left_classes re =
      Sigma.partition (fold_left_classes re (fun x xs -> x :: xs) [])
  end

  module Map = Map.Make(struct
      type t = Expr.t
      let compare = Expr.compare
    end)

  type transition = sigma * label * Expr.t
  type dfa = transition list Map.t

  let derive expr =
    let class_delta acc sigma =
      let labels, x' = Expr.left_delta expr sigma in
      (sigma, labels, x') :: acc
    in
    let add_non_empty s ss = if Sigma.is_empty s then ss else s :: ss in
    let classes = Expr.fold_left_classes expr add_non_empty [] in
    let partition = Sigma.partition classes in
    List.fold_left class_delta [] partition

  let rec add_to_dfa dfa = function
    | [] -> dfa
    | x :: todo when Map.mem x dfa -> add_to_dfa dfa todo
    | x :: todo ->
      (*Format.eprintf "new state: %a\n%!"
        Cmon.format
        (cmon_re
           ~set:(fun _ -> Cmon.unit)
           ~label:(fun _ -> Cmon.unit)
           ~abstract:(fun _ -> Cmon.unit)
           x);*)
      let transitions = derive x in
      let dfa = Map.add x transitions dfa in
      let add_todo todo (_, _, x') = x' :: todo in
      let todo = List.fold_left add_todo todo transitions in
      add_to_dfa dfa todo

  let make_dfa re = add_to_dfa Map.empty [re]
end

module Chars : sig
  include SIGMA
  val of_list : char list -> t
  val to_list : t -> char list
  val partition : t list -> t list
end = struct
  type t = string
  let empty = ""
  let full = String.make 32 '\xFF'
  let is_empty t = t = empty
  let is_full t = t = full
  let is_subset_of t1 t2 =
    let l1 = String.length t1 and l2 = String.length t2 in
    l1 <= l2 && (
      try
        for i = 0 to l1 - 1 do
          let c1 = Char.code t1.[i] and c2 = Char.code t2.[i] in
          if c1 land c2 <> c1 then raise Exit
        done;
        true
      with Exit -> false
    )

  let pack t =
    let len = String.length t in
    let last = ref len  in
    while !last > 0 && t.[!last - 1] = '\x00' do decr last done;
    if !last < len
    then String.sub t 0 !last
    else t

  let compl t =
    let s' =
      String.map (fun c -> Char.chr (0xFF land lnot (Char.code c))) t
    in
    let len = String.length s' in
    if len < 32 then
      s' ^ String.make (32 - len) '\xFF'
    else pack t

  let inter t1 t2 =
    let len = min (String.length t1) (String.length t2) in
    pack (String.init len (fun i ->
        let c1 = Char.code t1.[i] and c2 = Char.code t2.[i] in
        Char.chr (c1 land c2)
      ))

  let compare = String.compare

  let of_list = function
    | [] -> empty
    | cs ->
      let len = (Char.code (List.fold_left max '\000' cs) + 7) / 8 in
      let b = Bytes.make len '\000' in
      List.iter (fun c ->
          let c = Char.code c in
          Bytes.set b (c / 8)
            (Char.chr (Char.code (Bytes.get b (c / 8)) lor (c land 7)))
        ) cs;
      Bytes.unsafe_to_string b

  let to_list t =
    let r = ref [] in
    for i = String.length t - 1 downto 0 do
      let c = Char.code t.[i] in
      for j = 7 downto 0 do
        if c land (1 lsl j) <> 0 then
          r := Char.chr (i * 8 + j) :: !r
      done
    done ;
    !r

  let mini i1 i2 : int = if i1 < i2 then i1 else i2

  let intersect t1 t2 =
    match
      for i = 0 to mini (String.length t1) (String.length t2) - 1 do
        let c1 = Char.code t1.[i] in
        let c2 = Char.code t2.[i] in
        if c1 land c2 <> 0 then raise Exit
      done
    with
    | () -> false
    | exception Exit -> true

  let diff t1 t2 =
    let len = String.length t1 in
    let b = Bytes.of_string t1 in
    for i = 0 to mini len (String.length t2) - 1 do
      let c1 = Char.code t1.[i] in
      let c2 = Char.code t2.[i] in
      Bytes.set b i (Char.chr (c1 land lnot c2))
    done;
    let rec first_non_zero j =
      if (j = 0) || t1.[j - 1] <> '\000'
      then j
      else first_non_zero (j - 1)
    in
    Bytes.sub_string b 0 (first_non_zero len)

  (* Naive partitioning algorithm *)
  let partition = function
    | ([] | [_]) as l -> l
    | x :: xs ->
      let add_set acc set1 set2 =
        if intersect set1 set2 then
          let acc = inter set1 set2 :: acc in
          let set2_1 = diff set2 set1 in
          let acc = if is_empty set2_1 then acc else set2_1 :: acc in
          let set1_2 = diff set1 set2 in
          (set1_2, acc)
        else
          (set1, set2 :: acc)
      in
      let rec add_sets acc set = function
        | rest when is_empty set -> List.rev_append rest acc
        | [] -> acc
        | x :: xs ->
          let set', acc = add_set acc set x in
          add_sets acc set' xs
      in
      List.fold_left (fun acc set -> add_sets [] set acc) [x] xs
end
