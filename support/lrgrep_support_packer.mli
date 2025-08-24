type 'a t
val make : unit -> 'a t

type 'a cells = (int * 'a) list

type row_mapping
type promise

val add_row : 'a t -> 'a cells -> promise

val resolve : row_mapping -> promise -> Lrgrep_runtime.Sparse_table.row

type table = {
  displacement: int array;
  keys: int array;
  values: int array;
}

val table_lookup
  :  table
  -> Lrgrep_runtime.Sparse_table.row
  -> Lrgrep_runtime.Sparse_table.col
  -> Lrgrep_runtime.Sparse_table.value option

val pack : 'a t -> ('a -> Lrgrep_runtime.Sparse_table.value) ->
  row_mapping * table

val encode : table -> string * Lrgrep_runtime.Sparse_table.t
