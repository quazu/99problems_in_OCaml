(*
  Modified run-length encoding
*)
type 'a rle = One of 'a | Many of int * 'a

let runlength_of xs =
  let rec loop n acc = function
    | [] -> List.rev acc
    | f :: s :: tl when f = s ->
      loop (n + 1) acc (s :: tl)
    | hd :: tl ->
      let elem =
        if (n = 0)
          then One hd
        else Many ((n + 1), hd)
      in
        loop 0 (elem :: acc) tl
  in
    loop 0 [] xs
