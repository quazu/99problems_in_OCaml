(*
  Calculate Euler's totient function φ(m).
*)
let coprime x y =
  let rec gcd a b =
    if b = 0 then a else gcd b (a mod b)
  in
  gcd x y = 1

let phi n =
  let rec loop acc m =
    if m < n
      then loop (if coprime n m then acc+1 else acc) (m + 1)
    else acc
  in
  if n = 1 then 1 else loop 0 1
