(*
  Sorting a list of lists according to length oth sublists.
*)

(*
  Sort The elements of the list according to their length.
*)
let rec length_qsort = function
  | [] -> []
  | x :: xs ->
    let smaller, larger =
      List.partition (fun y -> List.length y < List.length x) xs
    in
    length_qsort smaller @ (x::length_qsort larger)
