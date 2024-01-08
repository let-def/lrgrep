open Fix.Indexing

type 'n t = bytes

let make c default =
  let n = cardinal c in
  Bytes.make ((n + 7) lsr 3) (if default then '\xFF' else '\x00')

let test b n =
  let n = (n : _ index :> int) in
  let cell = n lsr 3 in
  let bit = n land 7 in
  Char.code (Bytes.unsafe_get b cell) land (1 lsl bit) <> 0

let set b n =
  let n = (n : _ index :> int) in
  let cell = n lsr 3 in
  let bit = n land 7 in
  Bytes.unsafe_set b cell
    (Char.unsafe_chr (Char.code (Bytes.unsafe_get b cell) lor (1 lsl bit)))

let clear b n =
  let n = (n : _ index :> int) in
  let cell = n lsr 3 in
  let bit = n land 7 in
  Bytes.unsafe_set b cell
    (Char.unsafe_chr (Char.code (Bytes.unsafe_get b cell) land lnot (1 lsl bit)))
