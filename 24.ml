(*
  Lotto: Draw N different random numbers from the set 1..M
*)

(* Create a list containing all integers within a given range *)
let rec range init last =
  if init > last then failwith "Invalid range";
  if init = last
    then [init]
  else
    init :: (range (init + 1) last)

(* Extract a given number of randomly selected elements from a list *)
let pick n xs =
  (* Remove the K'th element from a list *)
  let rec remove ~pos = function
    | [] -> []
    | hd :: tl when pos = 0 -> tl
    | hd :: tl -> hd :: (remove ~pos:(pos - 1) tl)
  in
  let rec loop n acc = function
    | [] -> List.rev acc
    | hd :: tl when n = 0 -> List.rev acc
    | (hd :: tl) as l ->
      let pos = Random.int (List.length l) in
      loop (n - 1) ((List.nth l pos) :: acc) (remove ~pos:pos l)
  in
    loop n [] xs

let lotto num last =
  pick num (range 1 last)
