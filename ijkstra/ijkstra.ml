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

  module LookaheadPartition = Utils.Refine.Make(Red.LookaheadSet)

  (* Compute effect of reductions *)

  let reductions_by_lr1 =
    Grammar.Lr1.tabulate @@ fun lr1 ->
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

  let targeted_by_reducing = Array.make Grammar.Lr1.count []

  let () =
    Grammar.Lr1.iter (fun lr1 ->
        reductions_by_lr1 lr1
        |> (* Simulate the action of each production *)
        List.iter (fun (prod, lookahead) ->
            Grammar.Production.rhs prod
            |> (* Compute states reachable by reduction *)
            Array.fold_left
              (fun states _ -> Lr1.predecessors_of_states states)
              (Lr1.Set.singleton lr1)
            |> (* Save information in target state *)
            Lr1.Set.iter (fun target ->
                targeted_by_reducing.((target :> int)) <-
                  (prod, lookahead, lr1) ::
                  targeted_by_reducing.((target :> int))
              )
          )
      )

  (* Compute equivalence classes of lookaheads *)

  module Lookaheads = struct
    type variable =
      | Classes_of of Grammar.lr1 * Grammar.nonterminal
      | Lookaheads_of of Grammar.lr1
      | Subset_of_lookaheads of Grammar.lr1 * Red.LookaheadSet.t

    let classes_of = function
      | Subset_of_lookaheads (st, set) ->
        fun solve ->
          let sets = solve (Lookaheads_of st) in
          let sets = List.map (Red.LookaheadSet.inter set) sets in
          begin match List.sort_uniq Red.LookaheadSet.compare sets with
            | x :: xs when Red.LookaheadSet.is_empty x -> xs
            | xs -> xs
          end

      | Lookaheads_of st ->
        let sets = List.map snd (reductions_by_lr1 st) in
        begin match Grammar.Lr0.incoming (Grammar.Lr1.lr0 st) with
          | None | Some (Grammar.T _) -> fun _ -> sets
          | Some (Grammar.N nt) ->
            let states = Lr1.predecessors_of_state st in
            fun solve ->
              let sets =
                Lr1.Set.fold
                  (fun st' acc -> solve (Classes_of (st', nt)) @ acc)
                  states sets
              in
              LookaheadPartition.partition sets
        end

      | Classes_of (st, nt) ->
        let reductions =
          targeted_by_reducing.((st :> int))
          |> List.filter (fun (prod, _, _) ->
              Grammar.Production.lhs prod = nt)
        in
        fun solve ->
          LookaheadPartition.partition (
            List.fold_left (fun acc (_, sets, st) ->
                solve (Subset_of_lookaheads (st, sets)) @ acc
              ) [] reductions
          )


    module Fix = Fix.Fix.ForType(struct
        type t = variable
      end)(struct
        type property = Red.LookaheadSet.t list
        let bottom =
          let x = ref Red.LookaheadSet.empty in
          for i = Grammar.Terminal.count - 1 downto 0 do
            x := Red.LookaheadSet.add (Grammar.Terminal.of_int i) !x
          done;
          [!x]
        let equal = List.equal Red.LookaheadSet.equal
        let is_maximal _ = false
      end)

    let solve = Fix.lfp classes_of
  end

  (* Compute size of input needed to trigger a specific reduction *)

  module Size = struct
    type variable =
      | Cost_of_prod of Grammar.lr1 * Grammar.production * int * Red.LookaheadSet.t
      | Cost_of_goto of Grammar.lr1 * Grammar.nonterminal * Red.LookaheadSet.t

    module TokenMap = Map.Make(struct
        type t = Grammar.terminal
        let compare = (Int.compare :> t -> t -> int)
      end)

    let mini x y : int = if x < y then x else y

    module Cost : sig
      type t = private int TokenMap.t
      val zero : Red.LookaheadSet.t -> t
      val one : Grammar.terminal -> t -> t
      val bottom : t

      val equal : t -> t -> bool

      val min : t -> t -> t

      val compose : t -> Red.LookaheadSet.t -> t -> t
    end = struct
      type t = int TokenMap.t

      let zero ts =
        let add t acc = TokenMap.add t 0 acc in
        Red.LookaheadSet.fold add ts TokenMap.empty

      let one t ts =
        let offset =
          if TokenMap.is_empty ts
          then 0
          else TokenMap.fold (fun _ o o' -> mini o o') ts max_int
      in
      TokenMap.singleton t (1 + offset)

      let bottom = TokenMap.empty

      let equal = TokenMap.equal Int.equal


      let min xs ys = TokenMap.union (fun _ a b -> Some (mini a b)) xs ys

      let compose t1 ts t2 =
        let offset = Red.LookaheadSet.fold (fun t offset ->
            match TokenMap.find t t2 with
            | offset' -> mini offset offset'
            | exception Not_found -> offset
          ) ts max_int
        in
        if offset = max_int
        then bottom
        else TokenMap.map ((+) offset) t1
    end

    let count = ref 0

    let compute = function
      | Cost_of_prod (st, prod, pos, ts) ->
        incr count;
        Format.eprintf "#%d = Cost_of_prod (%d, %d, %d, _) : %a%!"
          (incr count; !count)
          (Grammar.Lr1.to_int st)
          (Grammar.Production.to_int prod)
          pos
          Grammar.Print.item (prod, pos)
        ;
        let rhs = Grammar.Production.rhs prod in
        if pos = Array.length rhs then
          let zero = Cost.zero ts in
          fun _sol -> zero
        else begin
          let sym, _, _ = rhs.(pos) in
          match List.assoc sym (Grammar.Lr1.transitions st) with
          | exception Not_found -> fun _sol -> Cost.bottom
          | st' ->
            match sym with
            | Grammar.T t -> fun solve ->
              (*Format.eprintf "Cost_of_prod (%d ,%d, %d, _)\n"
                (Grammar.Lr1.to_int st)
                (Grammar.Production.to_int prod)
                pos;*)
              Cost.one t (solve (Cost_of_prod (st', prod, pos + 1, ts)))
            | Grammar.N nt ->
              let classes = Lookaheads.solve (Classes_of (st, nt)) in
              fun solve ->
                (*Format.eprintf "Cost_of_prod (%d ,%d, %d, _)\n"
                  (Grammar.Lr1.to_int st)
                  (Grammar.Production.to_int prod)
                  pos;*)
                let tail = solve (Cost_of_prod (st', prod, pos + 1, ts)) in
                List.fold_left (fun cost ts' ->
                    let candidate =
                      Cost.compose (solve (Cost_of_goto (st, nt, ts'))) ts' tail
                    in
                    Cost.min cost candidate
                  ) Cost.bottom classes
        end
      | Cost_of_goto (st, nt, ts) ->
        let reductions =
          targeted_by_reducing.((st :> int))
          |> List.filter_map (fun (prod, ts', _) ->
              if Grammar.Production.lhs prod = nt then
                let ts' = Red.LookaheadSet.inter ts ts' in
                if Red.LookaheadSet.is_empty ts' then
                  None
                else
                  Some (prod, ts')
              else
                None
            )
        in
        fun solve ->
          let candidates =
            List.map (fun (prod, _lookahead) ->
                solve (Cost_of_prod (st, prod, 0, ts))
              ) reductions
          in
          List.fold_left Cost.min Cost.bottom candidates

    module Fix = Fix.Fix.ForType(struct
        type t = variable
      end)(struct
        type property = Cost.t
        let bottom = Cost.bottom
        let equal = Cost.equal
        let is_maximal _ = false
      end)

    let solve = Fix.lfp compute
  end
end

let () = match !grammar_file with
  | None ->
    Format.eprintf "No grammar provided (-g), stopping now.\n"
  | Some path ->
    let module Analysis = Analysis(struct let filename = path end)() in
    Analysis.Grammar.Lr1.iter (fun lr1 ->
        Format.printf "state %d has %d lookahead classes\n"
          (Analysis.Grammar.Lr1.to_int lr1)
          (List.length (Analysis.Lookaheads.solve (Lookaheads_of lr1)))
      );
    Array.iteri (fun lr1 prods ->
        List.iter (fun (prod, ts, _) ->
            let solution = Analysis.Size.solve (
                Cost_of_prod (
                  Analysis.Grammar.Lr1.of_int lr1,
                  prod,
                  0,
                  ts
                ))
            in
            Format.printf "cost of prod %d from state %d: %d\n%!"
              (Analysis.Grammar.Production.to_int prod)
              lr1
              (Analysis.Size.TokenMap.fold
                 (fun _ offset cost -> Analysis.Size.mini offset cost)
                 (solution :> int Analysis.Size.TokenMap.t) max_int)
          ) prods
      ) Analysis.targeted_by_reducing
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
