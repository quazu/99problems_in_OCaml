(*
  Drop every N'th element from a list
*)
let drill n xs =
  let rec drill num n acc = function
    | [] -> List.rev acc
    | hd :: tl when n = 1 ->
      drill num num acc tl
    | hd :: tl ->
      drill num (n - 1) (hd :: acc) tl
  in
    drill n n [] xs
