(*
  Split a list int two parts; the length of the first parts is given
*)
let split n xs =
  let rec split n acc = function
    | [] -> List.rev acc, []
    | hd ::tl when n = 1 -> List.rev (hd :: acc), tl
    | hd :: tl ->
      split (n - 1) (hd :: acc) tl
  in
    split n [] xs
