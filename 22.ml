(*
  Create a list containing all integers within a given range
*)
let rec range init last =
  if init > last then failwith "Invalid range";
  if init = last
    then [init]
  else
    init :: (range (init + 1) last)
