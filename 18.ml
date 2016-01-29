(*
  Extract a slice from a list
*)
let slice ~init ~last xs = 
(* Divides a list in front of pos(ition) *)
  let divide ~pos xs = 
    let rec divide n acc = function
      | [] -> List.rev acc, []
      | hd :: tl when n = 0 ->
        List.rev acc, (hd :: tl)
      | hd :: tl ->
        divide (n - 1) (hd :: acc) tl
    in
      divide pos [] xs
  in
  let (left, right) = divide init xs in
    fst (divide (last - init + 1) right)
