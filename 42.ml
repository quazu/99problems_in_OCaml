(*
  Gray code.
*)

(* Append character at the end of string *)
let append c str =
  (String.make 1 c) ^ str

let rec gray n =
  if n = 1
    then ["0"; "1"]
  else (List.map (append '0') (gray (n - 1))) @
       (List.map (append '1') (gray (n - 1)))
