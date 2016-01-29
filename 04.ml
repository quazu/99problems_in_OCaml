(*
  Find the number of elements of a list
*)
let size xs = 
  let rec size n = function
    | []       -> n
    | hd :: tl -> size (n + 1) tl
  in
    size 0 xs
