(*
  Calculate Euler's totient function phi(m).
*)

(* Determine the prime factors *)
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

let pow n p = 
  let rec loop n p acc =
    if p = 0
      then acc
    else loop n (p - 1) (n * acc)
  in
    loop n p 1

let phi m =
  let rec loop acc = function
    | [] -> acc
    | (p,m) :: ms ->
      loop (acc * (p - 1)  * (pow p (m - 1))) ms
  in
  loop 1 (factors m)
