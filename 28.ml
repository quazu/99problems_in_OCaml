(*
  Sorting a list of lists according to length of sublists.
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

(*
  Sort The elements of the list according to their length frequency.
*)
let frequency_sort list =
  let rec freq_of e count = function
    | [] -> count
    | h :: tl when List.length e = List.length h ->
      freq_of e (count + 1) tl
    | h :: tl -> 
      freq_of e count tl
  in
  qsort (fun x y -> freq_of x 0 list < freq_of y 0 list) list 
