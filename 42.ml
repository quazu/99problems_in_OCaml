(*
  Gray code.
*)

(* Encompass two lists *)
let encompass f xs ys =
  let rec loop f xs acc = function
    | [] -> acc
    | h :: t -> loop f xs (List.map (f h) xs :: acc) t
  in
  let prepend xs ys =
    let rec loop xs ys acc = match xs, ys with
      | [], [] -> List.rev acc
      | h :: t, _ -> loop t ys (h :: acc)
      | [], h :: t -> loop [] t (h :: acc)
    in
    loop xs ys []
  in
  let flatten lst =
    let rec loop acc = function
      | [] -> List.rev acc
      | (h :: t) :: r -> loop (h :: acc) (prepend [t] r)
      | [] :: r -> loop acc r
    in
    loop [] lst
  in
  loop f xs [] ys
    |> flatten

let gray n =
  let bits = ["0"; "1"] in
  let rec loop acc n =
    if n = 1
      then acc
    else loop (encompass (^) bits acc) (n - 1)
  in
  loop bits n
