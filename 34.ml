(*
  Determine the prime factors of a given positive integer.
*)
let factors n =
  let rec loop m ms n acc =
    if n = 1
      then (m, ms) :: acc
    else
      if (n mod m = 0)
        then loop m (ms + 1) (n / m) acc
      else
        let dist tp l = if (snd tp = 0) then l else tp :: l in
        loop (m + 1) 0 n (dist (m, ms) acc)
  in
  loop 2 0 n []
