module Make(G : Middle.Intf.GRAMMAR) = struct
  let actions = G.Lr1.tabulate begin fun lr1 ->
      let reduce = G.Lr1.reductions lr1 in
      let shift =
        G.Lr1.transitions lr1 |> List.filter_map (function
            | (G.T t, target) -> Some (t, target)
            | (G.N _, _) -> None
          )
      in
      let table = Hashtbl.create (List.length reduce + List.length shift) in
      List.iter (fun (terminal, prods) ->
          Hashtbl.add table terminal (`Reduce (List.hd prods)))
        reduce;
      List.iter (fun (t, tgt) ->
          match Hashtbl.find table t with
          | _ ->
            Printf.eprintf "Conflict: state %d, symbol %s\n"
              (G.Lr1.to_int lr1) (G.Terminal.name t);
            failwith "Lr1 conflict"
          | exception Not_found ->
            Hashtbl.add table t (`Shift tgt)
        ) shift;
      fun t -> Hashtbl.find table t
    end

  let goto = G.Lr1.tabulate begin fun lr1 ->
      let targets =
        G.Lr1.transitions lr1 |> List.filter_map (function
            | (G.N n, target) -> Some (n, target)
            | (G.T _, _) -> None
          )
      in
      let table = Hashtbl.create (List.length targets) in
      List.iter (fun (n, tgt) ->
          match Hashtbl.find table n with
          | _ ->
            Printf.eprintf "Conflict: state %d, symbol %s\n"
              (G.Lr1.to_int lr1) (G.Nonterminal.name n);
            failwith "Lr1 conflict"
          | exception Not_found ->
            Hashtbl.add table n tgt
        ) targets;
      fun n -> Hashtbl.find table n
    end

  let rec pop n = function
    | _ :: xs when n > 0 -> pop (n - 1) xs
    | xs -> xs

  let rec interp (t : G.terminal) = function
    | [] -> failwith "Parser finished"
    | (hd :: _) as stack ->
      begin match actions hd t with
        | `Shift tgt ->
          begin match G.Lr1.transitions tgt, G.Lr1.reductions tgt with
            | [], [t', prod :: _] when G.Terminal.kind t' = `PSEUDO ->
              `Accept (G.Production.lhs prod)
            | _ -> `Continue (tgt :: stack)
          end
        | `Reduce prod ->
          let rhs = G.Production.rhs prod in
          let stack = pop (Array.length rhs) stack in
          let stack = match stack with
            | [] -> assert false
            | hd' :: _ -> goto hd' (G.Production.lhs prod) :: stack
          in
          interp t stack
        | exception Not_found -> `No_transition
      end

  let interp_nt (n : G.nonterminal) = function
    | [] -> failwith "Parser finished"
    | (hd :: _) as stack ->
      let tgt = goto hd n in
      (tgt :: stack)

  let rec loop stack = function
    | [] -> `Continue stack
    | tok :: ts ->
      begin
        match tok with
        | G.N n -> loop (interp_nt n stack) ts
        | G.T t ->
          match interp t stack with
          | `Continue stack' -> loop stack' ts
          | `Accept prod -> `Accept (prod, ts)
          | `No_transition -> `No_transition (stack, tok :: ts)
      end
end
