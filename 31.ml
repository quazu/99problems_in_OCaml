(*
  Determine whether two positive integer numbers are coprime. 
*)
let coprime x y =
  let rec gcd a b =
    if b = 0 then a else gcd b (a mod b)
  in
  gcd x y = 1
