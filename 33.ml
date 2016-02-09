(*
  Determine the prime factors of a given psitive integer.
*)
let factors n =
  let rec loop m n acc =
    if n = 1
      then acc
    else
      if (n mod m = 0)
        then loop m (n / m) (m :: acc)
      else 
        loop (m + 1) n acc
  in
  loop 2 n []

