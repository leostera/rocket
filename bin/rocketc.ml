let () =
  let ast =
    Rocket_chapter2.L_var.(Let ("a", Int 42, Let ("b", Var "a", Var "b")))
    |> Rocket_chapter2.Pass_uniquify.run
    |> Rocket_chapter2.Pass_remove_complex_operands.run
    |> Rocket_chapter2.Pass_explicate_control.run
    |> Rocket_chapter2.Pass_select_instructions.run
    |> Rocket_chapter2.Pass_assign_homes.run
    |> Rocket_chapter2.Pass_patch_instructions.run
    |> Rocket_chapter2.Pass_prelude_and_conclusion.run
  in

  let oc = open_out "test.s" in
  let fmt = Format.formatter_of_out_channel oc in
  Rocket_chapter2.Arm.pp fmt ast;
  Format.force_newline ();
  Format.force_newline ();
  close_out oc;

  ()
