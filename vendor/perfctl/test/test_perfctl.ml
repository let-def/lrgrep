
let not_in_profile n =
  let sieve = Array.make (n + 1) true in
  sieve.(0) <- false;
  sieve.(1) <- false;
  let i = ref 1 in
  while !i * !i <= n do
    if sieve.(!i) then (
      let j = ref (!i * !i) in
      while !j <= n do
        sieve.(!j) <- false;
        j := !j + !i
      done
    );
    incr i
  done

let in_profile n =
  let sieve = Array.make (n + 1) true in
  sieve.(0) <- false;
  sieve.(1) <- false;
  let i = ref 1 in
  while !i * !i <= n do
    if sieve.(!i) then (
      let j = ref (!i * !i) in
      while !j <= n do
        sieve.(!j) <- false;
        j := !j + !i
      done
    );
    incr i
  done

let () = not_in_profile 1_000_000

let () = Perfctl.enable ()

let () = in_profile 1_000_000

let () = Perfctl.disable ()

let () = not_in_profile 1_000_000
