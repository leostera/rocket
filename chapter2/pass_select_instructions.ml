open X86_var

let atom (a : C_var.atom) =
  match a with C_var.Int x -> Int x | C_var.Var v -> Var v

let handle_statement (Assign (var, value) : C_var.statement) =
  match value with
  | C_var.Atom (C_var.Int x) -> [ Movq (Int x, Var var) ]
  | C_var.Atom (C_var.Var v) -> [ Movq (Var v, Var var) ]
  | C_var.Read -> [ Callq "read_int"; Movq (Reg Rax, Var var) ]
  | C_var.Add (x, y) -> [ Movq (atom x, Var var); Addq (atom y, Var var) ]
  | C_var.Neg x -> [ Movq (atom x, Var var); Negq (Var var) ]

let rec handle_tail (tail : C_var.tail) : block =
  match tail with
  | C_var.Seq (st, next) -> handle_statement st @ handle_tail next
  | C_var.Return expr ->
      handle_statement (C_var.Assign ("%rax", expr)) @ [ Jmp "_conclusion" ]

let to_info C_var.{ locals } : X86_var.info = { locals }

let run : C_var.program -> X86_var.program =
 fun program ->
  {
    info =
      {
        locals =
          program
          |> List.map (fun (C_var.{ locals }, _) -> locals)
          |> List.concat |> List.sort_uniq compare;
      };
    labels = List.map (fun (_info, t) -> ("_start", handle_tail t)) program;
  }
