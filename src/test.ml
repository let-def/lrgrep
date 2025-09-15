open Bit_lib

let test1 =
  "pop_count", (fun x ->
      let y1 = pop_count x in
      let y2 = pop_count_slow x in
      (y1, y2)
    )

let test2 =
  "msb_index", (fun x ->
      let y1 = msb_index x in
      let y2 = msb_index_slow x in
      (y1, y2)
    )

let test3 =
  "lsb_index", (fun x ->
      let y1 = lsb_index x in
      let y2 = lsb_index_slow x in
      (y1, y2)
    )

let test4 =
  "extract_msb", (fun x ->
      let y1 = extract_msb x in
      let y2 = extract_msb_slow x in
      (y1, y2)
    )

let tests = [test1; test2; test3; test4]

let run_test always_print (name, f) x =
  let y1, y2 = f x in
  if always_print || y1 <> y2 then (
    Printf.printf "%s(fast) %x = %x\n" name x y1;
    Printf.printf "%s(slow) %x = %x\n" name x y2;
    if y1 <> y2 then
      Printf.printf "(FAILURE)\n"
  )

let run_tests x =
  List.iter (fun f -> run_test false f x) tests

let vec = [0; 1; 2; 3; min_int; max_int; -1; -2; -3]

let () =
  List.iter run_tests vec

let () =
  for i = 0 to Sys.word_size do
    run_tests i;
    run_tests (1 lsl i);
  done

let () =
  while true do
    run_tests (Int64.to_int (Random.bits64 ()))
  done
