(*
  Generate the combinations of K distinct objects chosen from 
  the N elements of a list.
*)
let extract k xs =
  let rec aux k acc emit = function
    | [] -> acc
    | h :: t ->
      if k = 1
        then aux k (emit [h] acc) emit t
      else
        let new_emit x = emit (h :: x) in
        aux k (aux (k - 1) acc new_emit t) emit t
  in
  let emit x acc = x :: acc in
    aux k [] emit xs
