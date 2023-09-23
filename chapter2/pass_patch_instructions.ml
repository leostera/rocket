open Arm

exception Unpatchable_arg of Arm_var.arg

let is_offset (offset : Arm_var.arg) =
  match offset with Offset (_, _) -> true | _ -> false

let move offset1 offset2 = [ Mov (Reg R0, offset1); Mov (offset2, Reg R0) ]
let add offset1 offset2 = [ Mov (Reg R0, offset1); Add (offset2, Reg R0) ]
let sub offset1 offset2 = [ Mov (Reg R0, offset1); Sub (offset2, Reg R0) ]

let rec patch_arg (arg : Arm_var.arg) : Arm.arg =
  match arg with
  | Arm_var.Int x -> Int x
  | Arm_var.Reg reg -> Reg reg
  | Arm_var.Offset (reg, offset) -> Offset (reg, patch_arg offset)
  | Arm_var.Var "%R0" -> Reg R0
  | _ -> raise (Unpatchable_arg arg)

let patch_load (load : Arm_var.load) : Arm.load =
  match load with
  | Arm_var.Prefix (a, b) -> Prefix (patch_arg a, patch_arg b)
  | Arm_var.Postfix (a, b) -> Postfix (patch_arg a, patch_arg b)
  | Arm_var.Offset (a, b) -> Offset (patch_arg a, patch_arg b)

let patch_instr (instr : Arm_var.instr) : Arm.instr list =
  match instr with
  | Arm_var.Mov (offset1, offset2) when is_offset offset1 && is_offset offset2
    ->
      move (patch_arg offset1) (patch_arg offset2)
  | Arm_var.Add (offset1, offset2) when is_offset offset1 && is_offset offset2
    ->
      add (patch_arg offset1) (patch_arg offset2)
  | Arm_var.Sub (offset1, offset2) when is_offset offset1 && is_offset offset2
    ->
      sub (patch_arg offset1) (patch_arg offset2)
  | Arm_var.Add (x, y) -> [ Add (patch_arg x, patch_arg y) ]
  | Arm_var.Sub (x, y) -> [ Sub (patch_arg x, patch_arg y) ]
  | Arm_var.Mov (x, y) -> [ Mov (patch_arg x, patch_arg y) ]
  | Arm_var.Push arg -> [ Push (patch_arg arg) ]
  | Arm_var.Pop arg -> [ Pop (patch_arg arg) ]
  | Arm_var.Bl label -> [ Bl label ]
  | Arm_var.Bx reg -> [ Bx reg ]
  | Arm_var.Ldr arg -> [ Ldr (patch_load arg) ]
  | Arm_var.Rsbs (x, y) -> [ Rsbs (patch_arg x, patch_arg y) ]
  | Arm_var.Str (x, y) -> [ Str (patch_arg x, patch_arg y) ]

let patch_block (lbl, block) = (lbl, List.flatten @@ List.map patch_instr block)
let patch_info Arm_var.{ locals } = Arm.{ locals }

let run : Arm_var.program -> Arm.program =
 fun { info; labels } ->
  { info = patch_info info; labels = List.map patch_block labels }
