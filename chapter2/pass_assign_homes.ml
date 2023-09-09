open X86_var

let stack_size = 8

let remap_var ~locals (v : var) =
  match List.find_opt (fun (_, v') -> v = v') locals with
  | Some (reg_offset, _) -> Deref ((-(reg_offset+1)) * stack_size, Rbp)
  | None -> Var v

let remap_arg ~locals (arg : arg) =
  match arg with Var v -> remap_var ~locals v | _ -> arg

let remap_instr ~locals (instr : instr) =
  match instr with
  | Addq (x, y) -> Addq (remap_arg ~locals x, remap_arg ~locals y)
  | Subq (x, y) -> Subq (remap_arg ~locals x, remap_arg ~locals y)
  | Negq x -> Negq (remap_arg ~locals x)
  | Movq (x, y) -> Movq (remap_arg ~locals x, remap_arg ~locals y)
  | Pushq x -> Pushq (remap_arg ~locals x)
  | Popq x -> Popq (remap_arg ~locals x)
  | _ -> instr

let remap_block ~locals (lbl, block) =
  (lbl, List.map (remap_instr ~locals) block)

let with_index ls = List.mapi (fun idx el -> (idx, el)) ls

let run : X86_var.program -> X86_var.program =
 fun { info; labels } ->
  {
    info;
    labels = List.map (remap_block ~locals:(with_index info.locals)) labels;
  }
