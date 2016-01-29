(*
  Reverse a list
*)
let reverse xs =
  let rec reverse acc = function
    | [] -> acc
    | hd :: tl -> reverse (hd :: acc) tl
  in
    reverse [] xs
