(*
  Eliminate consecutive duplicates of list elements
*)
let rec compress = function
  | [] -> []
  | f :: s :: tl when f = s -> compress (s :: tl)
  | hd :: tl -> hd :: compress tl
