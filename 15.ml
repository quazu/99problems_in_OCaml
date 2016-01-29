(*
  Replicate the elements of a list a given number of times
*)
let replicate n xs =
  let rec replicate num n acc = function
    | [] -> List.rev acc
    | hd :: tl when n = 0 ->
      replicate num num acc tl
    | hd :: tl ->
      replicate num (n - 1) (hd :: acc) (hd :: tl)
  in
    replicate n n [] xs
