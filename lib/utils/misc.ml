let array_last arr = match Array.length arr with
  | 0 -> None
  | n -> Some (arr.(n - 1))
