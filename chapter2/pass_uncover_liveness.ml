open X86_live

let get_live_op (arg : X86_var.arg) : live_op list =
  match arg with
  | X86_var.Reg r -> [ Reg r ]
  | X86_var.Deref (d, r) -> [ Deref (d, r) ]
  | X86_var.Var v -> [ Var v ]
  | _ -> []

let get_rw (instr : X86_var.instr) =
  match instr with
  | X86_var.Addq (x, y) -> (get_live_op x @ get_live_op y, get_live_op y)
  | X86_var.Subq (x, y) -> (get_live_op x @ get_live_op y, get_live_op y)
  | X86_var.Negq x -> (get_live_op x, get_live_op x)
  | X86_var.Movq (x, y) -> (get_live_op x, get_live_op y)
  | X86_var.Pushq _ -> ([], [])
  | X86_var.Popq _ -> ([], [])
  | X86_var.Callq _ -> ([], [])
  | X86_var.Reqt -> ([], [])
  | X86_var.Jmp _ -> ([Reg Rax], [Reg Rsp])

let uncover_live next_live (instr : X86_var.instr) =
  let reads, writes = get_rw instr in
  (next_live |> List.filter (fun l -> not (List.mem l writes))) @ reads

let block_liveness instrs =
  let (_, liveness) = 
  List.fold_right (fun instr (live, acc) ->
      let next_live = uncover_live live instr in
      (next_live, next_live :: acc)) instrs (empty_liveness, []) in

  List.combine instrs liveness

let run : X86_var.program -> X86_live.program =
 fun { info; labels } ->
  {
    info;
    labels = List.map (fun (lbl, block) -> (lbl, block_liveness block)) labels;
  }
