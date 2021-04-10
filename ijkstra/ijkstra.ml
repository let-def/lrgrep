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


  let targeted_by_reducing = Array.make Grammar.Lr1.count []

  let () =
    Grammar.Lr1.iter (fun lr1 ->
        Grammar.Lr1.reductions lr1
        |> (* Remove conflicting productions *)
        List.map (fun (t, ps) -> t, List.hd ps)
        |> (* Regroup lookahead tokens by production *)
        Utils.Misc.group_by
          ~compare:(fun (_, p1) (_, p2) ->
              Int.compare
                (p1 : Grammar.production :> int)
                (p2 : Grammar.production :> int)
            )
          ~group:(fun (t, p) tps ->
              let set =
                List.fold_left
                  (fun set (t, _) -> Red.LookaheadSet.add t set)
                  (Red.LookaheadSet.singleton t) tps
              in
              (p, set)
            )
        |> (* Simulate the action of each production *)
        List.iter (fun (prod, _ as reduction) ->
            Grammar.Production.rhs prod
            |> (* Compute states reachable by reduction *)
            Array.fold_left
              (fun states _ -> Lr1.predecessors_of_states states)
              (Lr1.Set.singleton lr1)
            |> (* Save information in target state *)
            Lr1.Set.iter (fun target ->
                targeted_by_reducing.((target :> int)) <-
                  reduction :: targeted_by_reducing.((target :> int))
              )
          )
      )



  type variable =
    | Cost_of_prod of Grammar.lr1 * Grammar.production * int
    | Cost_of_goto of Grammar.lr1 * Grammar.nonterminal

  module Cost : sig
    type t = private int
    val zero : t
    val one : t
    val infinite : t

    val equal : t -> t -> bool
    val compare : t -> t -> int
    val is_zero : t -> bool
    val is_infinite : t -> bool

    val sum : t -> t -> t
    val min : t -> t -> t
    val arg_min : ('a -> t) -> 'a -> 'a -> 'a
  end = struct
    type t = int
    let zero : t = 0
    let one : t = 1
    let infinite : t = max_int

    let equal = Int.equal
    let compare = Int.compare
    let is_zero x = equal x 0
    let is_infinite x = equal x infinite

    let sum (x : t) (y : t) : t =
      if is_infinite x || is_infinite y then infinite else x + y
    let min (x : t) (y : t) : t = if x < y then x else y
    let arg_min f a b =
      if f a < f b then a else b
  end

  let compute = function
    | Cost_of_prod (st, prod, pos) ->
      begin
        let rhs = Grammar.Production.rhs prod in
        if pos = Array.length rhs then
          fun _sol -> Cost.zero
        else
          let sym, _, _ = rhs.(pos) in
          match List.assoc sym (Grammar.Lr1.transitions st) with
          | exception Not_found -> fun _sol -> Cost.infinite
          | st' ->
            fun solution ->
            let tail = solution (Cost_of_prod (st', prod, pos + 1)) in
            if Cost.is_infinite tail then tail else
              let head = match sym with
                | Grammar.T _ -> Cost.one
                | Grammar.N nt -> solution (Cost_of_goto (st, nt))
              in
              Cost.sum head tail
      end
    | Cost_of_goto (st, nt) ->
      let reductions =
        targeted_by_reducing.((st :> int))
        |> List.filter (fun (prod, _) -> Grammar.Production.lhs prod = nt)
      in
      fun solution ->
      let candidates =
        List.map
          (fun (prod, _lookahead) -> solution (Cost_of_prod (st, prod, 0)))
          reductions
      in
      List.fold_left Cost.min Cost.infinite candidates

  module Fix = Fix.Fix.ForType(struct
      type t = variable
    end)(struct
      type property = Cost.t
      let bottom = Cost.infinite
      let equal = Cost.equal
      let is_maximal = Cost.is_zero
    end)

  let solve = Fix.lfp compute
end

let () = match !grammar_file with
  | None ->
    Format.eprintf "No grammar provided (-g), stopping now.\n"
  | Some path ->
    let module Analysis = Analysis(struct let filename = path end)() in
    Analysis.Grammar.Lr1.iter (fun lr1 ->
        let lr0 = Analysis.Grammar.Lr1.lr0 lr1 in
        List.iter (fun (prod, pos) ->
            if pos = 1 then
              Format.printf "cost of prod %d from state %d: %d\n"
                (Analysis.Grammar.Production.to_int prod)
                (Analysis.Grammar.Lr1.to_int lr1)
                (Analysis.solve (Analysis.Cost_of_prod (lr1, prod, 0)) :> int)
          ) (Analysis.Grammar.Lr0.items lr0)
      )
    (*let t0 = Sys.time () in
    let t1 = Sys.time () in
    Format.eprintf "Built graph in %.02f\n" ((t1 -. t0) *. 1000.0);
    let states, transitions =
      List.fold_left (fun (states, transitions) (_lr1, lr1s) ->
          states + 1, transitions + Analysis.Lr1.Set.cardinal lr1s
        ) (0, 0) Analysis.g
    in
    Format.eprintf "%d states, %d transitions\n" states transitions;
    let total, by_class =
      Array.fold_left (fun (total, by_class) classes ->
          let count = List.fold_left
              (fun sum class' ->
                 sum + Analysis.Red.LookaheadSet.cardinal class')
              0 classes
          in
          total + count, by_class + List.length classes
        ) (0, 0) Analysis.coarse_lookaheads
    in
    let all_classes = List.sort_uniq Analysis.Red.LookaheadSet.compare
        (List.flatten (Array.to_list Analysis.coarse_lookaheads))
    in
    Format.eprintf "%d transition based on lookaheads\n%d transitions based on lookahead classes,%d unique classes\n" total by_class (List.length all_classes)*)
