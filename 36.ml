(*
  Compare the two methods of calculating Euler's totient function.
*)
let spec f x =
  let t0 = Unix.gettimeofday () in
  ignore (f x);
  let t1 = Unix.gettimeofday () in
    t1 -. t0
