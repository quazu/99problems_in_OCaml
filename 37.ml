(*
  A list of prime numbers.
*)
let primes n m =
  let rec range h l =
    if h = l
      then [l]
    else h :: (range (h + 1) l)
  in
  let rec filter f = function
    | [] -> []
    | h :: t when f h ->
      h :: (filter f t)
    | h :: t ->
      filter f t
  in
  let is_prime x =
    let rec loop n x =
      if (float_of_int n) > (float_of_int x) ** 0.5
        then true
      else
        if x mod n = 0
          then false
        else loop (n + 1) x
    in
    loop 2 x
  in
  filter is_prime (range n m)

