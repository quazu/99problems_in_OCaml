(*
  Generate the combinations of K distinct objects chosen from 
  the N elements of a list.
*)
let extract k xs =
  let add xs y =
    List.rev (y :: (List.rev xs))
  in
  let deliver xs ys = 
    List.map (add xs) ys
  in
  let concat xs ys = 
    List.rev (List.rev_append ys (List.rev xs))
  in
  let rec loop k0 k acc tmp xs0 xs =
    match xs with
      | [] -> acc
      | hd :: tl when k = 1 ->
        loop k0 k0 (concat (deliver tmp xs) acc) [] (List.tl xs0) (List.tl xs0)
      | hd :: tl ->
        loop k0 (k - 1) acc (add tmp hd) xs0 tl
  in
    loop k k [] [] xs xs
