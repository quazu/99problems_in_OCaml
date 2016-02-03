(*
  Sorting a list of lists according to length oth sublists.
*)
(* Define quick sort not to use List.sort *)
let rec qsort f = function
  | [] -> []
  | x :: xs ->
    let smaller, larger = List.partition (fun y -> f y x) xs in
    qsort f smaller @ (x :: qsort f larger)

(*
  Sort The elements of the list according to their length.
*)
let length_sort list =
  qsort (fun x y -> List.length x < List.length y) list
