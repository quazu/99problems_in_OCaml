(*
  Extract a given number of randomly selected elements from a list
*)
let pick n xs =
(* Remove the K'th element from a list *)
  let rec remove ~pos = function
    | [] -> []
    | hd :: tl when pos = 0 -> tl
    | hd :: tl -> hd :: (remove ~pos:(pos - 1) tl)
  in
  let rec loop n acc = function
    | [] -> List.rev acc
    | hd :: tl when n = 0 -> List.rev acc
    | (hd :: tl) as l ->
      let pos = Random.int (List.length l) in
      loop (n - 1) ((List.nth l pos) :: acc) (remove ~pos:pos l)
  in
    loop n [] xs
