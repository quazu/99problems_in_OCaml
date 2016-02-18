(*
  A list of Goldbach compositions.
*)

let rec filter f = function
  | [] -> []
  | h :: t when f h ->
    h :: (filter f t)
  | h :: t ->
    filter f t

let rec range h l =
  if h = l
    then [l]
  else h :: (range (h + 1) l)

(* List of primes *)
let primes n m =
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

let goldbach n =
  let numbers = primes 2 n in
  let rec loop = function 
    | h :: t ->
      if filter (fun x -> x = n - h) t = []
        then loop t
      else Some(h, n -h)
    | [] -> None
  in
  loop numbers

let goldbach_list n m =
  let is_even x = x mod 2 = 0 in
  let map f xs =
    let rec rev acc = function
      | [] -> acc
      | h :: t -> rev (h :: acc) t
    in
    let rec loop g acc = function
      | [] -> rev [] acc
      | h :: t -> loop g ((g h) :: acc) t
    in
    loop f [] xs
  in
  range n m
    |> filter is_even
    |> map (fun x -> x, goldbach x)
