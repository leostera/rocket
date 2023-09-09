open L_var

let rec inline (ast : expr) =
  match ast with
  | Prim (Add (Int x, Int y)) -> Int (x + y)
  | Prim (Neg (Int x)) -> Int (-x)
  | Prim (Neg x) -> Prim (Neg (inline x))
  | Prim (Add (x, y)) -> Prim (Add (inline x, inline y))
  | _ -> ast
