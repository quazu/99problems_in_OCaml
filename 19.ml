(*
  Rotate a list N places to the left
*)
let rotate i xs = 
(* Concatenate two lists *)
  let concat xs ys =
    List.rev (List.rev_append ys (List.rev xs))
  in
(* Divide a list in front of pos(ition) *)
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
(* make the number between zero and length of xs *)
  let len = List.length xs in
  let n = (i mod len + len) mod len in
  let (left, right) = divide ~pos:n xs in
    concat right left
