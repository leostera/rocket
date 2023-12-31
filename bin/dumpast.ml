let () =
  (*
  let ast =
    Rocket_chapter2.L_var.(
      Let
        ( "x",
          Prim Read,
          Prim
            (Add
               ( Var "x",
                 Let
                   ( "x",
                     Prim (Neg (Int 1)),
                     Let
                       ( "y",
                         Int 2,
                         Prim (Add (Var "x", Prim (Add (Var "y", Int 3)))) ) )
               )) ))
  in
  *)
  let ast =
    Rocket_chapter2.L_var.(Let ("a", Int 5, Let ("b", Int 30, Let ("c", Var "a", Let ("b", Int 10, Prim (Add (Var "c", Var "b")))))))
  in

  let ast = ast |> Rocket_chapter2.Pass_uniquify.run in
  Sexplib.Sexp.pp_hum_indent 2 Format.std_formatter
    (Rocket_chapter2.L_var.sexp_of_expr ast);
  Format.force_newline ();
  Format.force_newline ();

  (* let ast =
       Rocket_chapter2.L_var.(
         Let
           ( "x",
             Prim (Add (Int 42, Prim (Neg (Int 10)))),
             Prim (Add (Var "x", Int 10)) ))
     in

     let ast =
       Rocket_chapter2.L_var.(
         Let
           ( "a", (Int 42),
           Let ("b", Prim (Add(Var "a", Prim(Neg(Int 2112)))),
             Var "b")))
     in
  *)
  let ast = ast |> Rocket_chapter2.Pass_remove_complex_operands.run in
  Sexplib.Sexp.pp_hum_indent 2 Format.std_formatter
    (Rocket_chapter2.L_mon_var.sexp_of_expr ast);
  Format.force_newline ();
  Format.force_newline ();

  (*
  let ast =
       Rocket_chapter2.L_mon_var.(
         Let
           ( "x",
            Let ("y", 
             Prim (Neg (Int 42)), Atom(Var "y")),
             Prim (Neg (Var "x"))))
     in
     *)
  let ast = ast |> Rocket_chapter2.Pass_explicate_control.run in
  Sexplib.Sexp.pp_hum_indent 2 Format.std_formatter
    (Rocket_chapter2.C_var.sexp_of_program ast);
  Format.force_newline ();
  Format.force_newline ();

  let ast = ast |> Rocket_chapter2.Pass_select_instructions.run in
  Sexplib.Sexp.pp_hum_indent 2 Format.std_formatter
    (Rocket_chapter2.X86_var.sexp_of_program ast);
  Format.force_newline ();
  Format.force_newline ();

  let ast = ast |> Rocket_chapter2.Pass_uncover_liveness.run in
  Sexplib.Sexp.pp_hum_indent 2 Format.std_formatter
    (Rocket_chapter2.X86_live.sexp_of_program ast);
  Format.force_newline ();
  Format.force_newline ();

  let ast = ast |> Rocket_chapter2.Pass_assign_homes.run in
  Sexplib.Sexp.pp_hum_indent 2 Format.std_formatter
    (Rocket_chapter2.X86_live.sexp_of_program ast);
  Format.force_newline ();
  Format.force_newline ();

  let ast = ast |> Rocket_chapter2.Pass_patch_instructions.run in
  Sexplib.Sexp.pp_hum_indent 2 Format.std_formatter
    (Rocket_chapter2.X86.sexp_of_program ast);
  Format.force_newline ();
  Format.force_newline ();

  let x86 = ast |> Rocket_chapter2.Pass_prelude_and_conclusion.run in
  Rocket_chapter2.X86.pp Format.std_formatter x86;
  Format.force_newline ();
  Format.force_newline ();

  ()
