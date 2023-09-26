open X86_var
open X86_live

let stack_size = 8

let remap_var ~locals (v : var) =
  match List.find_opt (fun (_, v') -> v = v') locals with
  | Some (reg_offset, _) -> Deref (-(reg_offset + 1) * stack_size, Rbp)
  | None -> Var v

let remap_arg ~locals (arg : arg) =
  match arg with Var v -> remap_var ~locals v | _ -> arg

let remap_instr ~locals (instr, live) =
  match instr with
  | Addq (x, y) -> Addq (remap_arg ~locals x, remap_arg ~locals y), live
  | Subq (x, y) -> Subq (remap_arg ~locals x, remap_arg ~locals y), live
  | Negq x -> Negq (remap_arg ~locals x), live
  | Movq (x, y) -> Movq (remap_arg ~locals x, remap_arg ~locals y), live
  | Pushq x -> Pushq (remap_arg ~locals x), live
  | Popq x -> Popq (remap_arg ~locals x), live
  | _ -> instr, live

let remap_block ~locals (lbl, block) =
  (lbl, List.map (remap_instr ~locals) block)

let with_index ls = List.mapi (fun idx el -> (idx, el)) ls

let run : X86_live.program -> X86_live.program =
 fun { info; labels } ->
  {
    info;
    labels = List.map (remap_block ~locals:(with_index info.locals)) labels;
  }
