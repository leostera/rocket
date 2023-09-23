open Arm_var

let stack_size = 8

let remap_var ~locals (v : var) : arg =
  match List.find_opt (fun (_, v') -> v = v') locals with
  | Some (reg_offset, _) -> Offset (R11_FP, Int (-(reg_offset + 1) * stack_size))
  | None -> Var v

let remap_arg ~locals (arg : arg) =
  match arg with Var v -> remap_var ~locals v | _ -> arg

let remap_instr ~locals (instr : instr) =
  match instr with
  | Add (x, y) -> Add (remap_arg ~locals x, remap_arg ~locals y)
  | Sub (x, y) -> Sub (remap_arg ~locals x, remap_arg ~locals y)
  | Mov (x, y) -> Mov (remap_arg ~locals x, remap_arg ~locals y)
  | Push x -> Push (remap_arg ~locals x)
  | Pop x -> Pop (remap_arg ~locals x)
  | _ -> instr

let remap_block ~locals (lbl, block) =
  (lbl, List.map (remap_instr ~locals) block)

let with_index ls = List.mapi (fun idx el -> (idx, el)) ls

let run : Arm_var.program -> Arm_var.program =
 fun { info; labels } ->
  {
    info;
    labels = List.map (remap_block ~locals:(with_index info.locals)) labels;
  }
