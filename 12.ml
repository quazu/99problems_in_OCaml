(*
  Decode a run-length encoding list
*)
type 'a rle = One of 'a | Many of (int * 'a)

let plain_of rle =
(* concatenate two lists *)
  let concat xs ys =
    List.rev (List.rev_append ys (List.rev xs))
  in
  let rec loop acc = function
    | [] -> List.rev acc
    | (One hd) :: tl ->
      loop (hd :: acc) tl
    | (Many (n, hd)) :: tl ->
      let rec list_of n hd =
        if (n = 0) then [] else hd :: (list_of (n - 1) hd)
      in
        loop (concat (list_of n hd) acc) tl
  in
    loop [] rle
