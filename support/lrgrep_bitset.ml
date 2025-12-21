type t = string

let empty = ""

let is_empty = function
  | "" -> true
  | _ -> false

let interc c1 c2 =
  Char.unsafe_chr ((Char.code c1) land (Char.code c2))

let mem t s =
  assert (t >= 0);
  let i = t / 8 in
  let j = t mod 8 in
  (i < String.length s) &&
  (Char.code s.[i] land (1 lsl j) <> 0)

let inter s1 s2 =
  let l = ref (Int.min (String.length s1) (String.length s2)) in
  while !l > 0 && interc s1.[!l - 1] s2.[!l - 1] = '\000' do
    decr l
  done;
  String.init !l (fun i -> interc s1.[i] s2.[i])

let import = function
  | "" -> ""
  | x ->
    if x.[String.length x - 1] = '\000' then
      invalid_arg "Lrgrep_bitset.import: malformed bitset";
    x

let export s = s

let source_code t =
  Printf.sprintf "(Lrgrep_bitset.import %S)" t

let normalize = function
  | "" -> ""
  | s ->
    let l = ref (String.length s) in
    if s.[!l - 1] <> '\000' then
      s
    else (
      decr l;
      while !l > 0 && s.[!l - 1] = '\000' do
        decr l
      done;
      String.sub s 0 !l
    )

let init card f =
  normalize (String.init ((card + 7) / 8) (fun i ->
      let m = ref 0 in
      let j = i * 8 in
      for k = j to (if j + 7 >= card then card - 1 else j + 7) do
        if f k then
          m := !m lor (1 lsl (k land 7))
      done;
      Char.chr !m
    ))

(*module type T = sig
    type state
    type lr1
    type terminal
    type terminalset
    type item

    val all : terminalset

    type completion = {
      shift: (lr1 list * terminal) list;
      reduce: (int * state * terminalset) list;
    }

    type recovery = {
      cost: int;
      actions: item list;
      reached: lr1 list;
      next: (int * state) option;
    }

    val initial : state

    exception Malformed_stack
    val complete : state -> lr1 -> completion
    val recover : state -> lr1 -> recovery list
  end*)
