open Fix.Indexing
open Utils
open Misc

let grammar_filename =
  let filename, oc = Filename.open_temp_file "lrgrep-interpreter" "cmly" in
  output_string oc Interpreter_data.grammar;
  close_out oc;
  filename

module Grammar = MenhirSdk.Cmly_read.Read(struct let filename = grammar_filename end)
let () = Sys.remove grammar_filename

module Info = Mid.Info.Make(Grammar)
open Info

module Item : sig
  type n
  val n : n cardinal
  type t = n index
  val inj : Production.t -> int -> t
  val prj : n index -> Production.t * int
  val production : n index -> Production.t
  val position : n index -> int
  val pred : t -> t option
end = struct
  let count = ref 0
  let first = Vector.init Production.n (fun p ->
      let index = ! count in
      count := !count + Production.length p + 1;
      index
    )
  let count = !count

  include Const(struct let cardinal = count end)
  type t = n index

  let production = Vector.make' n (fun () -> Index.of_int Production.n 0)

  let () =
    Vector.iteri (fun r index ->
        for i = index to index + Production.length r do
          Vector.set production (Index.of_int n i) r
        done
      ) first

  let inj r pos =
    assert (pos >= 0 && pos <= Production.length r);
    Index.of_int n (Vector.get first r + pos)

  let prj t =
    let r = Vector.get production t in
    (r, (t :> int) - Vector.get first r)

  let pred t =
    let r, p = prj t in
    if p > 0
    then Some (inj r (p - 1))
    else None

  let position t =
    let r = Vector.get production t in
    ((t :> int) - Vector.get first r)

  let production = Vector.get production

end

let effective_items : (Lr1.n, (Item.t * Terminal.set) list) vector =
  let table = Vector.make Lr1.n IndexMap.empty in
  let rec populate states items =
    let map = IndexMap.of_seq (List.to_seq items) in
    IndexSet.iter (fun lr1 ->
        let map' = Vector.get table lr1 in
        let union _k v v' = Some (IndexSet.union v v') in
        Vector.set table lr1 (IndexMap.union union map map')
      ) states;
    match List.filter_map (fun (it, la) -> match Item.pred it with
        | None -> None
        | Some it' -> Some (it', la)
      ) items
    with
    | [] -> ()
    | items' ->
      populate (indexset_bind states Lr1.predecessors) items'
  in
  Index.iter Lr1.n (fun lr1 ->
      let initial_items =
        IndexSet.fold (fun red acc ->
            let prod = Reduction.production red in
            let lookaheads = Reduction.lookaheads red in
            (Item.inj prod (Production.length prod), lookaheads) :: acc
          ) (Reduction.from_lr1 lr1) []
      in
      populate (IndexSet.singleton lr1) initial_items
    );
  let bindings_by_length map =
    List.sort (fun (it1, _) (it2, _) ->
        let _, n1 = Item.prj it1 in
        let _, n2 = Item.prj it2 in
        Int.compare n1 n2
      ) (IndexMap.bindings map)
  in
  Vector.map bindings_by_length table

let positive_items st =
  let rec loop = function
    | (it, _) :: rest when Item.position it = 0 -> loop rest
    | otherwise -> otherwise
  in
  loop (Vector.get effective_items st)

let time = Stopwatch.create ()

type config = {
  goto: Lr1.t;
  base: Lr1.t;
  lookaheads: Terminal.set;
}

let ignore_lookaheads = true

type state = {
  config: config;
  mutable epsilon: state list;
  mutable transitions: state list list;
}

let table = Hashtbl.create 7

let rec explore states la n = function
  | [] -> []
  | (it, la') :: xs when Item.position it = n ->
    let la' =
      if ignore_lookaheads then Terminal.all else IndexSet.inter la la'
    in
    if IndexSet.is_empty la' then
      explore states la n xs
    else
      let x', xs' = match explore states la n xs with
        | [] -> ([], [])
        | x' :: xs' -> (x', xs')
      in
      let p = Item.production it in
      let nt = Production.lhs p in
      IndexSet.fold (fun base acc ->
          let goto = Transition.find_goto_target base nt in
          let config = {base; goto; lookaheads = la'} in
          get_state config :: acc
        ) states x' :: xs'
  | xs ->
    [] :: explore (indexset_bind states Lr1.predecessors) la (n + 1) xs

and get_state config =
  match Hashtbl.find_opt table config with
  | Some st -> st
  | None ->
    let st = {config; epsilon=[]; transitions=[]} in
    Hashtbl.add table config st;
    begin match
        explore
          (IndexSet.singleton st.config.base)
          config.lookaheads 1
          (positive_items st.config.goto)
      with
      | [] -> ()
      | eps :: rest ->
        st.epsilon <- eps;
        st.transitions <- rest
    end;
    st

let initial =
  let acc = ref [] in
  Index.iter Lr1.n (fun lr1 ->
      match Lr1.incoming lr1 with
      | Some s when Symbol.is_nonterminal s -> ()
      | _ ->
        let states = IndexSet.singleton lr1 in
        let items = positive_items lr1 in
        push acc (lr1, explore states Terminal.all 0 items)
    );
  !acc

let () =
  Printf.eprintf "Non-deterministic full reduction graph: %d states\n"
    (Hashtbl.length table)
