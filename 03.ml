(*
  Find the k'th element of a list.
*)
let rec pick ~pos = function
  | [] -> None
  | hd :: tl when pos = 0 -> Some hd
  | hd :: tl -> pick ~pos:(pos - 1) tl
