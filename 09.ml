(*
  Pack consecutive duplicates of list elements into sublists
*)
let pack xs =
  let rec loop sub acc = function
    | [] -> List.rev acc
    | f :: s :: tl when f = s -> loop (f :: sub) acc (s :: tl)
    | hd :: tl -> loop [] ( (hd :: sub) :: acc ) tl
  in
    loop [] [] xs
