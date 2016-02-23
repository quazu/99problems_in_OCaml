(*
  Truth tables for logical expressions.
*)
type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr

let rec assess a val_a b val_b = function
  | Var x when x = a -> val_a
  | Var x when x = b -> val_b
  | Var _ -> failwith "Invalid_Expression"
  | Not e -> not (assess a val_a b val_b e)
  | And (e1, e2) -> assess a val_a b val_b e1 && assess a val_a b val_b e2
  | Or (e1, e2) -> assess a val_a b val_b e1 || assess a val_a b val_b e2

let table2 a b expr =
  [(true,  true,  assess a true  b true  expr);
   (true,  false, assess a true  b false expr);
   (false, true,  assess a false b true  expr);
   (false, false, assess a false b false expr)]

