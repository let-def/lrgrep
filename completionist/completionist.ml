let grammar_file = ref None

let usage = "usage: menhirlex [options] sourcefile"

let print_version_string () =
  print_string "The Menhir parser lexer generator :-], version ";
  print_string Sys.ocaml_version;
  print_newline ();
  exit 0

let print_version_num () =
  print_endline Sys.ocaml_version;
  exit 0

let specs = [
  "-g", Arg.String (fun x -> grammar_file := Some x),
  " <file.cmly>  Path of the Menhir compiled grammar to analyse (*.cmly)";
  "-v",  Arg.Unit print_version_string,
  " Print version and exit";
  "-version",  Arg.Unit print_version_string,
  " Print version and exit";
  "-vnum",  Arg.Unit print_version_num,
  " Print version number and exit";
]

let () = Arg.parse specs invalid_arg usage

module Analysis (X : sig val filename : string end)() =
struct
  module Grammar = MenhirSdk.Cmly_read.Read(X)
  module Lr1 = Middle.Lr1.Make(Grammar)
  module Sigma = Middle.Sigma.Make(Lr1)
  module Red = Reduction_graph.Make(Sigma)()

  (*module ItemSet = struct
    type t = Grammar.item list
    let empty = []
    let compare = compare
    let append t1 t2 = List.sort_uniq compare (t1 @ t2)
  end

  module rec Regular : Utils.Mulet.S
    with type sigma = Sigma.t
     and type label = ItemSet.t
     and type abstract = Reduction_operator.t
    = Utils.Mulet.Make(Sigma)(ItemSet)(Reduction_operator)

  and Reduction_operator : sig
    include Utils.Mulet.DERIVABLE
      with type sigma := Sigma.t
       and type label := ItemSet.t
       and type t = Red.Concrete.state
    val from_lr1 : Grammar.Lr1.t -> Regular.Expr.t
  end = struct
    type t = Red.Concrete.state
    let is_empty _ = false
    let nullable _ = false

    let get_label t =
      let rec close acc t =
        List.fold_left close (t :: acc) (Red.Concrete.epsilons t)
      in
      let ts = close [] t in
      let lr1s =
        List.map (fun t -> Red.Graph.stack_top (Red.Concrete.represent t)) ts
      in
      let items =
        lr1s
        |> List.map (fun lr1 -> Grammar.Lr0.items (Grammar.Lr1.lr0 lr1))
        |> List.flatten
        |> List.sort_uniq compare
      in
      items

    let from_lr1 lr1 = Regular.Expr.abstract (Red.Concrete.from_lr1 lr1)

    let left_classes t f acc =
      Red.Concrete.transitions t (fun ~pop ~dispatch acc ->
          if pop > 0
          then f Sigma.full acc
          else List.fold_left
              (fun acc (lr1, _) -> f (Sigma.Pos (Lr1.Set.singleton lr1)) acc)
              acc dispatch
        ) acc

    let left_delta t sigma =
      let make_re ~pop ~dispatch =
        if pop > 0 then
          let dispatch = List.map (fun (lr1, tgt) ->
              Regular.Expr.(
                set (Sigma.Pos (Lr1.Set.singleton lr1)) ^.
                Regular.Expr.abstract tgt
              )
            ) dispatch
          in
          let re = ref [Regular.Expr.disjunction dispatch] in
          for _i = 1 to pop - 1 do
            re := Regular.Expr.set Sigma.full :: !re
          done;
          Regular.Expr.concatenation !re
        else
          Regular.Expr.disjunction (
            List.filter_map (fun (lr1, tgt) ->
                if Sigma.mem lr1 sigma
                then Some (Regular.Expr.abstract tgt)
                else None
              ) dispatch
          )
      in
      let res =
        Red.Concrete.transitions t
          (fun ~pop ~dispatch acc -> make_re ~pop ~dispatch :: acc) []
      in
      ([], Regular.Expr.disjunction res)

    let compare t1 t2 = Int.compare (t1 : t :> int) (t2 : t :> int)
  end*)
end

let () = match !grammar_file with
  | None ->
    Format.eprintf "No grammar provided (-g), stopping now.\n"
  | Some path ->
    let module Analysis = Analysis(struct let filename = path end)() in
    ()
    (*Utils.Strong.Finite.Set.iter Analysis.Red.Concrete.states begin fun st ->
      let items =
        let lr1 = Analysis.Red.Graph.stack_top
            (Analysis.Red.Concrete.represent st)
        in
        let items = Analysis.Red.G.Lr1.lr0 lr1 in




      let eps =
        Analysis.Red.Concrete.epsilons st
        |> List.map (fun st' ->
            string_of_int (Utils.Strong.Finite.Elt.to_int st'))
        |> String.concat ";"
      in
      let disp =
        Analysis.Red.Concrete.immediate_transitions st
        |> List.map (fun (pop, disp) ->
            let disp = List.map (fun (lr1, st) ->
                Printf.sprintf "| %d -> st_%d"
                  (Analysis.Grammar.Lr1.to_int lr1)
                  (Utils.Strong.Finite.Elt.to_int st)
              ) disp
            in
            Printf.sprintf "(%d, function %s)"
              pop
              (String.concat "\n  " (disp @ ["| _ -> raise Not_found"]))
          )
        |> String.concat ";\n"
      in
      Printf.printf "and st_%d = State ([%s], [%s], [%s])\n" (st :> int) items eps disp;
    end*)

    (*let initial =
      Analysis.Regular.Expr.disjunction (
        Analysis.Grammar.Lr1.fold
        (fun lr1 acc -> Analysis.Reduction_operator.from_lr1 lr1 :: acc)
        []
      )
    in
    let dfa = Analysis.Regular.make_dfa initial in
    Printf.printf "completion DFA has %d states\n"
      (Analysis.Regular.Map.cardinal dfa)*)
