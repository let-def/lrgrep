open Fix.Indexing
open Utils
open Misc
open Kernel.Info

let classify_line txt =
  let is_whitespace = function ' ' | '\t' -> true | _ -> false in
  let l = String.length txt in
  let i = ref 0 in
  while !i < l && is_whitespace txt.[!i] do
    incr i
  done;
  if !i = l then
    `Whitespace
  else if txt.[!i] <> '#' then
    `Text
  else if !i + 1 < l && txt.[!i+1] = '#' then
    `Autocomment
  else
    `Comment (String.sub txt (!i + 1) (l - !i - 1))

type 'a line =
  | Comment of string
  | Text of 'a

type 'a file =
  | Comments of string list
  | Mixed of 'a

let rec extract_pre_block lines () =
  let prepare block =
    if List.for_all (function Comment _ -> true | Text _ -> false) block then
      let prj = function Comment cmt -> cmt | Text _ -> assert false in
      Comments (List.rev_map prj block)
    else
      Mixed (List.rev block)
  in
  let rec aux acc lines =
    match lines () with
    | Seq.Nil ->
      if list_is_empty acc then
        Seq.Nil
      else
        Seq.Cons (prepare acc, extract_pre_block lines)
    | Seq.Cons (line, lines) ->
      match classify_line line with
      | `Whitespace ->
        Seq.Cons (prepare acc, extract_pre_block lines)
      | `Autocomment ->
        aux acc lines
      | `Comment comment ->
        aux (Comment comment :: acc) lines
      | `Text ->
        aux (Text line :: acc) lines
  in
  aux [] lines

type 'sentence block = {
  sentences: 'sentence line list;
  comments: string list list;
  message: string line list;
}

(*let group_lines lines =
  let cons acc result =
    if List.is_empty acc
    then result
    else Text (List.rev acc) :: result
  in
  let rec aux acc = function
  | [] -> cons acc []
  | Comment _ as cmt :: rest ->
    cons acc (cmt :: aux [] rest)
  | Text x :: rest ->
    aux (x :: acc) rest
  in
  aux [] lines*)

let rec extract_block pblocks () =
  let rec extract_message sentences comments pblocks =
    match pblocks () with
    | Seq.Nil ->
      Printf.eprintf "error: last sentences without message in .messages file\n";
      exit 1
    | Seq.Cons (Comments comments', pblocks') ->
      extract_message sentences (comments' :: comments) pblocks'
    | Seq.Cons (Mixed message, pblocks') ->
      let comments = List.rev comments in
      (*let message = group_lines message in*)
      Seq.Cons (Mixed {sentences; comments; message}, extract_block pblocks')
  in
  match pblocks () with
  | Seq.Nil ->
    Seq.Nil
  | Seq.Cons (Comments _ as comm, pblocks') ->
    Seq.Cons (comm, extract_block pblocks')
  | Seq.Cons (Mixed sentences, pblocks') ->
    extract_message sentences [] pblocks'

let map_block f = function
  | Comments _ as cmts -> cmts
  | Mixed block ->
    Mixed (f block)

let map_line f = function
  | Comment _ as cmt -> cmt
  | Text txt -> Text (f txt)

type 'g sentence = {
  entrypoint: 'g lr1 index option;
  symbols: 'g terminal index list;
}

let lift_sentence g sentence =
  (* Step 1: extract optional entrypoint and symbols *)
  let entrypoint, symbols =
    match String.index_opt sentence ':' with
    | None -> None, sentence
    | Some colon ->
      let lhs = String.trim (String.sub sentence 0 colon) in
      let rhs =
        String.sub sentence
          (colon + 1)
          (String.length sentence - colon - 1)
      in
      (Some lhs, rhs)
  in
  let symbols = List.filter ((<>) "") (String.split_on_char ' ' symbols) in
  (* Step 2: lift to grammatical entities *)
  let lift_entrypoint sym =
    let entrypoints = Lr1.entrypoint_table g in
    match Hashtbl.find_opt entrypoints sym  with
    | None ->
      Printf.eprintf "Unknown entrypoint %S%a\n"
        sym
        (print_dym (fun (_,s,_) -> s))
        (Damerau_levenshtein.filter_approx ~dist:3 sym
           (Hashtbl.to_seq entrypoints));
      exit 1
    | Some sym -> sym
  in
  let lift_terminal sym =
    match Terminal.find g sym with
    | Result.Ok t -> t
    | Result.Error dym ->
      Printf.eprintf "Unknown terminal %S%a\n" sym
        (print_dym (fun (_,s,_) -> s)) dym;
      exit 1
  in
  let entrypoint = Option.map lift_entrypoint entrypoint in
  let symbols = List.map lift_terminal symbols in
  { entrypoint; symbols }

let parse_sentence (type g) (g : g grammar) =
  (* Memoize actions *)
  let action_table : (g lr1 index * g terminal index, _) Hashtbl.t = Hashtbl.create 7 in
  let get_action state terminal =
    match Lr1.default_reduction g state with
    | Some prod -> `Reduce prod
    | None ->
      let key = (state, terminal) in
      match Hashtbl.find_opt action_table key with
      | Some action -> action
      | None ->
        let action =
          match
            IndexSet.find
              (fun red -> IndexSet.mem terminal (Reduction.lookaheads g red))
              (Reduction.from_lr1 g state)
          with
          | red -> `Reduce (Reduction.production g red)
          | exception Not_found ->
            let sym = Symbol.inj_t g terminal in
            match
              IndexSet.find
                (fun tr -> Index.equal sym (Transition.symbol g tr))
                (Transition.successors g state)
            with
            | tr -> `Shift (Transition.target g tr)
            | exception Not_found ->
              `Reject
        in
        Hashtbl.add action_table key action;
        action
  in
  (* Process a sentence *)
  fun {entrypoint; symbols} ->
    let rec consume_terminal stack (t, startp, endp as token) =
      let (state, _, currp) = List.hd stack in
      match get_action state t with
      | `Reject -> Result.Error stack
      | `Shift state -> Result.Ok ((state, startp, endp) :: stack)
      | `Reduce prod ->
        let (stack, startp', endp') =
          match Production.length g prod with
          | 0 -> (stack, currp, currp)
          | n ->
            let (_, _, endp) = List.hd stack in
            let stack = list_drop (n - 1) stack in
            let (_, startp, _) = List.hd stack in
            let stack = List.tl stack in
            (stack, startp, endp)
        in
        let (state, _, _) = List.hd stack in
        let state' = Transition.find_goto_target g state (Production.lhs g prod) in
        let stack = (state', startp', endp') :: stack in
        consume_terminal stack token
    in
    let rec loop stack ts =
      match ts () with
      | Seq.Nil -> (stack, stack, Seq.empty)
      | Seq.Cons (t, ts') as ts0 ->
        match consume_terminal stack t with
        | Result.Ok stack' -> loop stack' ts'
        | Result.Error stack' -> (stack, stack', fun () -> ts0)
    in
    let entrypoint = match entrypoint with
      | None -> IndexSet.choose (Lr1.entrypoints g)
      | Some lhs -> lhs
    in
    let _canonical_stack, intermediate_stack, _remainder =
      let dummy_pos x = (x, Lexing.dummy_pos, Lexing.dummy_pos) in
      loop [dummy_pos entrypoint] (Seq.map dummy_pos (List.to_seq symbols))
    in
    let state, _, _ = List.hd intermediate_stack in
    state

let wrap_lines prefix newline mid_suffix suffix = function
  | [] -> []
  | first :: rest ->
    match List.rev rest with
    | [] -> [prefix ^ first ^ suffix]
    | last :: mid ->
      (prefix ^ first ^ mid_suffix) ::
      List.rev_map (fun mid -> newline ^ mid ^ mid_suffix) mid @
      [newline ^ last ^ suffix]

let state_to_pattern g lr1 =
  let items = Kernel.Coverage.string_of_items_for_filter g (Lr1.to_lr0 g lr1) in
  match Lr1.incoming g lr1 with
  | Some sym when Symbol.is_nonterminal g sym ->
    wrap_lines "| [_* /" "      /" "" "]" items
  | _ ->
    wrap_lines "| /" "  /" "" "" items

let fold_consecutive ~comment ~text lines acc =
  let rec comments acc lines = function
    | [] -> comment (List.rev lines) acc
    | Comment line :: rest ->
      comments acc (line :: lines) rest
    | Text line :: rest ->
      texts (comment (List.rev lines) acc) [line] rest
  and texts acc lines = function
    | [] -> text (List.rev lines) acc
    | Comment line :: rest ->
      comments (text (List.rev lines) acc) [line] rest
    | Text line :: rest ->
      texts acc (line :: lines) rest
  in
  match lines with
  | [] -> acc
  | Comment line :: rest ->
    comments acc [line] rest
  | Text line :: rest ->
    texts acc [line] rest

let block_to_lines g = function
  | Comments comments ->
    wrap_lines "(* " "   " "" " *)" comments
  | Mixed {sentences; comments; message} ->
    let sentences =
      fold_consecutive
        ~comment:(fun lines acc -> wrap_lines "  (* " "     " "" " *)" lines :: acc)
        ~text:(fun states acc -> List.concat_map (state_to_pattern g) states :: acc)
        sentences []
    in
    let comments =
      List.rev_map
        (fun lines -> [""] @ wrap_lines "  (* " "     " "" " *)" lines @ [""])
        comments
    in
    let message =
      fold_consecutive
        ~comment:(fun lines acc -> wrap_lines "  (* " "     " "" " *)" lines :: acc)
        ~text:(fun lines acc ->
            let lines = List.mapi (fun i line ->
                let line = String.escaped line in
                if i = 0
                then line
                else if line <> "" && line.[0] = ' '
                then "\\" ^ line
                else " " ^ line
              ) lines in
            wrap_lines "  { \"" "    " "\\n\\" "\" }" lines :: acc
          )
        message []
    in
    List.concat (List.rev_append sentences (List.rev_append comments (List.rev message)))

let blocks_to_file (type g) (g : g grammar) blocks () =
  let prepare i block =
    let lines = block_to_lines g block in
    let lines = if i = 0 then lines else "" :: lines in
    List.to_seq lines
  in
  Seq.Cons ("rule error_messages = parse error",
            Seq.concat (seq_mapi prepare blocks))
