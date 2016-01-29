(*
  Run-length encoding of a list
*)
let runlength_of xs =
  let rec loop n acc = function
    | [] -> List.rev acc
    | f :: s :: tl when f = s ->
      loop (n + 1) acc (s :: tl)
    | hd :: tl ->
      loop 0 ((n + 1, hd) :: acc) tl
  in
    loop 0 [] xs
