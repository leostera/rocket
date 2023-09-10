open X86

exception Unpatchable_arg of X86_var.arg

let is_deref (ref : X86_var.arg) =
  match ref with Deref (_, _) -> true | _ -> false

let move ref1 ref2 = [ Movq (ref1, Reg Rax); Movq (Reg Rax, ref2) ]
let add ref1 ref2 = [ Movq (ref1, Reg Rax); Addq (Reg Rax, ref2) ]
let sub ref1 ref2 = [ Movq (ref1, Reg Rax); Subq (Reg Rax, ref2) ]

let patch_arg (arg : X86_var.arg) : X86.arg =
  match arg with
  | X86_var.Int x -> Int x
  | X86_var.Reg reg -> Reg reg
  | X86_var.Deref (offset, reg) -> Deref (offset, reg)
  | X86_var.Var "%rax" -> Reg Rax
  | _ -> raise (Unpatchable_arg arg)

let patch_instr (instr : X86_var.instr) : X86.instr list =
  match instr with
  | X86_var.Movq (ref1, ref2) when is_deref ref1 && is_deref ref2 ->
      move (patch_arg ref1) (patch_arg ref2)
  | X86_var.Addq (ref1, ref2) when is_deref ref1 && is_deref ref2 ->
      add (patch_arg ref1) (patch_arg ref2)
  | X86_var.Subq (ref1, ref2) when is_deref ref1 && is_deref ref2 ->
      sub (patch_arg ref1) (patch_arg ref2)
  | X86_var.Addq (x, y) -> [ Addq (patch_arg x, patch_arg y) ]
  | X86_var.Subq (x, y) -> [ Subq (patch_arg x, patch_arg y) ]
  | X86_var.Movq (x, y) -> [ Movq (patch_arg x, patch_arg y) ]
  | X86_var.Negq arg -> [ Negq (patch_arg arg) ]
  | X86_var.Pushq arg -> [ Pushq (patch_arg arg) ]
  | X86_var.Popq arg -> [ Popq (patch_arg arg) ]
  | X86_var.Callq var -> [ Callq var ]
  | X86_var.Reqt -> [ Reqt ]
  | X86_var.Jmp label -> [ Jmp label ]

let patch_block (lbl, block) = (lbl, List.flatten @@ List.map patch_instr block)
let patch_info X86_var.{ locals } = X86.{ locals }

let run : X86_var.program -> X86.program =
 fun { info; labels } ->
  { info = patch_info info; labels = List.map patch_block labels }
