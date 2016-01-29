(*
  Generate a random permutation of the elements of a list
*)
let permutate xs =
  (* Remove the K'th element from a list *)
  let rec remove ~pos = function
    | [] -> []
    | hd :: tl when pos = 0 -> tl
    | hd :: tl -> hd :: (remove ~pos:(pos - 1) tl)
  in
  let rec loop acc = function
    | [] -> List.rev acc
    | [x] -> x :: acc
    | (hd :: tl) as l ->
      let pos = Random.int (List.length l) in
      loop ((List.nth l pos) :: acc) (remove ~pos:pos l)
  in
    loop [] xs
