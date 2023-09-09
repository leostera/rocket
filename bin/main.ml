let () =
  let ast =
    Rocket_chapter2.L_var.(
      Let
        ( "x",
          Prim Read,
          Prim (Add (Var "x", Prim (Add (Int 1, Prim (Add (Int 2, Int 3)))))) ))
  in

  let result =
    ast |> Rocket_chapter2.Partial_evaluator.inline
    |> Rocket_chapter2.Interpreter.eval
  in

  match result with
  | Rocket_chapter2.L_var.Int x -> print_int x
  | _ -> print_string "(Error not-a-value)"
