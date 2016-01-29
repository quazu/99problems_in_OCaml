(*
  Remove the K'th element from a list
*)
let rec remove ~pos = function
  | [] -> []
  | hd :: tl when pos = 0 -> tl
  | hd :: tl -> hd :: (remove ~pos:(pos - 1) tl)
    
