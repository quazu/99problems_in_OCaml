(*
  Insert an element at a given position into a list
*)
let rec insert x ~pos = function
  | [] -> [x]
  | hd :: tl when pos = 0 -> x :: tl
  | hd :: tl -> hd :: (insert x ~pos:(pos - 1) tl)
