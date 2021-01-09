type id = int let id_k = ref 0
let id = fun () -> incr id_k; !id_k
let unshared = 0

type syntax =
  | Unit
  | Nil
  | Bool of bool
  | Char of char
  | Int of int
  | Float of float
  | Constant of string
  | Cons of {id: id; car: syntax; cdr: syntax}
  | String of {id: id; data: string}
  | Tuple of {id: id; data: syntax list}
  | Record of {id: id; data: (string * syntax) list}
  | Constructor of {id: id; tag: string; data: syntax}
  | Var of id
  | Let of {id: id; bindings: (id * syntax) list; body: syntax}

type t = syntax

let nil  = Nil
let unit = Unit
let bool   data = Bool data
let char   data = Char data
let int    data = Int data
let float  data = Float data
let string data = String {id=id(); data}
let constant tag = Constant tag
let constructor tag data = Constructor {id=id(); tag; data}
let tuple  data = Tuple {id=id(); data}
let record data = Record {id=id(); data}
let cons   car cdr = Cons {id=id(); car; cdr}

let list xs = List.fold_right cons xs nil

let unshared_string data = String{id=unshared; data}
let unshared_constructor tag data = Constructor {id=unshared; tag; data}
let unshared_tuple  data = Tuple {id=unshared; data}
let unshared_record data = Record {id=unshared; data}
let unshared_cons car cdr = Cons {id=unshared; car; cdr}
let unshared_list xs = List.fold_right unshared_cons xs nil

let ctuple tag data = constructor tag (unshared_tuple data)
let crecord tag data = constructor tag (unshared_record data)
let unshared_ctuple tag data = unshared_constructor tag (unshared_tuple data)
let unshared_crecord tag data = unshared_constructor tag (unshared_record data)

let id_of = function
  | Bool _ | Char _ | Int _ | Float _ | Nil | Unit | Constant _ -> unshared
  | Tuple {id; _} | Record {id; _} | Constructor {id; _} | Cons {id; _}
  | String {id; _} | Var id | Let {id; _} -> id

let graph : t Fastdom.graph = {
  successors = begin fun f acc ->
    let rec aux acc = function
      | Unit | Nil | Bool _ | Char _ | Int _ | Float _ | Constant _
      | String _ | Var _ | Let _ -> acc
      | Tuple {data; _} -> List.fold_left f_ acc data
      | Record {data; _} -> List.fold_left f_field acc data
      | Constructor {data; _} -> f_ acc data
      | Cons {car; cdr; _} -> f_ (f_ acc car) cdr
    and f_field acc (_, v) =
      f_ acc v
    and f_ acc self =
      if id_of self <> unshared
      then f acc self
      else aux acc self
    in
    aux acc
  end;
  memoize = begin fun (type b) (f : _ -> b) ->
    let table : (id, b) Hashtbl.t = Hashtbl.create 7 in
    fun x ->
      let id = id_of x in
      if id = unshared then f x else
        try Hashtbl.find table id
        with Not_found ->
          let y = f x in
          Hashtbl.add table id y;
          y
  end;
}

let explicit_sharing t =
  let postorder, dominance = Fastdom.dominance graph t in
  let count = Array.length postorder in
  let bindings = Array.make count [] in
  let share tag = match Fastdom.predecessors tag with
    | [] | [_] -> false
    | _ :: _ :: _ -> true
  in
  for i = count - 1 downto 0 do
    let tag = postorder.(i) in
    if share tag then begin
      let dominator = Fastdom.dominator tag in
      let index = Fastdom.postorder_index dominator in
      bindings.(index) <- Fastdom.node tag :: bindings.(index);
    end
  done;
  let dominance t =
    if id_of t = unshared then None else Some (dominance t)
  in
  let rec traverse ~is_binding t =
    let bindings, t =
      match dominance t with
      | None -> ([], t)
      | Some tag ->
        let id = id_of t in
        let dominator = Fastdom.dominator tag in
        if not is_binding && share tag && dominator != tag
        then ([], Var id)
        else (bindings.(Fastdom.postorder_index tag), t)
    in
    let t = match t with
      | Unit | Nil | Bool _ | Char _ | Int _ | Float _ | Constant _
      | String _ | Var _ | Let _ -> t
      | Tuple t -> unshared_tuple (List.map traverse_child t.data)
      | Record t -> unshared_record (List.map (fun (k,v) -> k, traverse_child v) t.data)
      | Constructor t -> unshared_constructor t.tag (traverse_child t.data)
      | Cons t -> unshared_cons (traverse_child t.car) (traverse_child t.cdr)
    in
    match List.map traverse_binding bindings with
    | [] -> t
    | bindings -> Let {id=id(); bindings; body = t}
  and traverse_child t =
    traverse ~is_binding:false t
  and traverse_binding t =
    (id_of t, traverse ~is_binding:true t)
  in
  traverse_child t

let rec list_of_cons acc = function
  | Cons {id = _; car; cdr} -> list_of_cons (car :: acc) cdr
  | Nil -> List.rev acc, None
  | other -> List.rev acc, Some other

let rec sub_print_as_is =
  let open PPrint in
  function
  | Unit    -> true, string "()"
  | Nil     -> true, string "[]"
  | Constant tag -> true, string tag
  | Bool b  -> true, OCaml.bool b
  | Char c  -> true, OCaml.char c
  | Int  i  -> true, OCaml.int i
  | Float f -> true, OCaml.float f
  | Cons _ as self ->
    begin match list_of_cons [] self with
      | items, None ->
        true, OCaml.list print_as_is items
      | items, Some cdr ->
        false,
        List.fold_left
          (fun (prefix : PPrint.document) item ->
             prefix ^^ print_as_is item ^/^ string "::" ^^ break 1)
          empty items ^^ string "::" ^/^ print_as_is cdr
    end
  | String {id=_; data} -> true, OCaml.string data
  | Tuple {id=_; data} ->
    true, OCaml.tuple (List.map print_as_is data)
  | Record {id=_; data} ->
    true, OCaml.record "" (List.map (fun (k,v) -> k, print_as_is v) data)
  | Constructor {id=_; tag; data} ->
    let delimited, sub_doc = sub_print_as_is data in
    let doc =
      if delimited
      then sub_doc
      else OCaml.tuple [sub_doc]
    in
    false, group (string tag ^^ blank 1 ^^ doc)
  | Var id -> true, string ("v" ^ string_of_int id)
  | Let {id=_; bindings; body} ->
    let rec print_bindings prefix = function
      | [] -> string "in"
      | (id, value) :: values ->
        let id = string ("v" ^ string_of_int id) in
        let doc = print_as_is value in
        let need_break = match value with Let _ -> true | _ -> false in
        let doc =
          if need_break
          then group (group (string prefix ^/^ id ^/^ string "=") ^^ nest 2 (break 1 ^^ doc))
          else group (group (string prefix ^/^ id ^/^ string "= ") ^^ nest 2 doc)
        in
        doc ^/^ print_bindings "and" values
    in
    let bindings = print_bindings "let" bindings in
    false,
    bindings ^/^
    print_as_is body

and print_as_is doc =
  let _delim, doc = sub_print_as_is doc in
  doc

let format_as_is ppf t : unit =
  PPrint.ToFormatter.pretty 0.9 80 ppf (print_as_is t)

let print t : PPrint.document = print_as_is (explicit_sharing t)
let format ppf t : unit = format_as_is ppf (explicit_sharing t)
