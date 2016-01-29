(*
  Flatten a nested list structure
*)
type 'a node = 
  | One of 'a
  | Many of 'a node list

let flatten xs =
  let rec loop acc = function
    | [] -> acc
    | One x :: tl -> loop (x :: acc) tl
    | Many x :: tl -> loop (loop acc x) tl
  in
    List.rev (loop [] xs)
