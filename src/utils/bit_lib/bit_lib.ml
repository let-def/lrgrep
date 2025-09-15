external pop_count : (int [@untagged]) -> (int [@untagged]) =
  "bit_lib_pop_count_tagged" "bit_lib_pop_count" [@@noalloc]

external pop_count_slow : int -> int =
  "bit_lib_pop_count_tagged" [@@noalloc]

external msb_index : (int [@untagged]) -> (int [@untagged]) =
  "bit_lib_msb_index_tagged" "bit_lib_msb_index" [@@noalloc]

external msb_index_slow : int -> int =
  "bit_lib_msb_index_tagged" [@@noalloc]

external lsb_index : (int [@untagged]) -> (int [@untagged]) =
  "bit_lib_lsb_index_tagged" "bit_lib_lsb_index" [@@noalloc]

external lsb_index_slow : int -> int =
  "bit_lib_lsb_index_tagged" [@@noalloc]

external extract_msb : (int [@untagged]) -> (int [@untagged]) =
  "bit_lib_extract_msb_tagged" "bit_lib_extract_msb" [@@noalloc]

external extract_msb_slow : int -> int =
  "bit_lib_extract_msb_tagged" [@@noalloc]

let extract_lsb x = x land -x
