(*
  Determine whether a given integer number is prime.
*)
let is_prime x =
  if x = 1
    then false
  else
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
  let last = int_of_float ((float_of_int x) ** 0.5) + 1 in
  let rec sieve n last primes =
    if n = last
      then primes
    else sieve (n+1) last (filter (fun x -> x mod n != 0) primes)
  in
  let primes = sieve 2 last (range 2 x) in
  filter (fun y -> y = x ) primes != []
