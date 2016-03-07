(*
  Gray code.
*)

(* Append character at the beginning of string *)
let append c str =
  (String.make 1 c) ^ str

let rec gray n =
  if n = 1
    then ["0"; "1"]
  else (List.map (append '0') (gray (n - 1))) @
       (List.map (append '1') (gray (n - 1)))


(*
  Another gray code.
*)

(* Encompass lists *)
let encompass xs ys =
  let rec loop xs acc = function
    | [] -> acc
    | h :: t -> loop xs (List.map ((^) h) xs :: acc) t
  in
  let rec flatten = function
    | [] -> []
    | l::r -> l @ flatten r
  in
  loop xs [] ys
    |> flatten

let gray n =
  let bits = ["0"; "1"] in
  let rec loop acc n =
    if n = 1
      then acc
    else loop (encompass bits acc) (n - 1)
  in
  loop bits n
