module Grammar = MenhirSdk.Cmly_read.Read (struct
    let filename = Sys.argv.(1)
  end)

let () = Grammar.Lr1.iter @@ fun lr1 ->
  let lr0 = Grammar.Lr1.lr0 lr1 in
  let itemset = Grammar.Lr0.items lr0 in
  Format.printf "==== %d ====\n%a\n\n%!"
    (Grammar.Lr1.to_int lr1)
    Grammar.Print.itemset itemset
