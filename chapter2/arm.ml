open Sexplib.Std

type label = string [@@deriving sexp]
type var = string [@@deriving sexp]

type reg =
  | R0
  | R1
  | R2
  | R3
  | R4
  | R5
  | R6
  | R7
  | R8
  | R9
  | R10
  | R11_FP
  | R12_IP
  | R13_SP
  | R14_LR
  | R15_PC
  | CPSR
[@@deriving sexp]

type arg = Int of int | Reg of reg | Offset of reg * arg [@@deriving sexp]

type load = Prefix of arg * arg | Postfix of arg * arg | Offset of arg * arg
[@@deriving sexp]

type instr =
  | Adr of arg * arg
  | Add of arg * arg
  | Bl of label
  | Bx of reg
  | Ldr of load
  | Mov of arg * arg
  | Pop of arg
  | Push of arg
  | Rsbs of arg * arg
  | Str of arg * arg
  | Sub of arg * arg
[@@deriving sexp]

type info = { locals : var list } [@@deriving sexp]
type block = instr list [@@deriving sexp]
type program = { info : info; labels : (label * block) list } [@@deriving sexp]

module Printer = struct
  let pp_reg fmt (reg : reg) =
    match reg with
    | R0 -> Format.fprintf fmt "r0"
    | R1 -> Format.fprintf fmt "r1"
    | R2 -> Format.fprintf fmt "r2"
    | R3 -> Format.fprintf fmt "r3"
    | R4 -> Format.fprintf fmt "r4"
    | R5 -> Format.fprintf fmt "r5"
    | R6 -> Format.fprintf fmt "r6"
    | R7 -> Format.fprintf fmt "r7"
    | R8 -> Format.fprintf fmt "r8"
    | R9 -> Format.fprintf fmt "r9"
    | R10 -> Format.fprintf fmt "r10"
    | R11_FP -> Format.fprintf fmt "fp"
    | R12_IP -> Format.fprintf fmt "ip"
    | R13_SP -> Format.fprintf fmt "sp"
    | R14_LR -> Format.fprintf fmt "lr"
    | R15_PC -> Format.fprintf fmt "pc"
    | CPSR -> Format.fprintf fmt "cpsr"

  let rec pp_arg fmt (x : arg) =
    match x with
    | Int x -> Format.fprintf fmt "#%d" x
    | Reg reg -> Format.fprintf fmt "%a" pp_reg reg
    | Offset (reg, offset) ->
        Format.fprintf fmt "[%a, %a]" pp_reg reg pp_arg offset

  let pp_label fmt (l : label) = Format.fprintf fmt "%s" l

  let pp_load fmt (l : load) =
    match l with
    | Prefix (x, y) -> Format.fprintf fmt "[%a %a]!" pp_arg x pp_arg y
    | Postfix (x, y) -> Format.fprintf fmt "[%a], %a" pp_arg x pp_arg y
    | Offset (x, y) -> Format.fprintf fmt "[%a %a]" pp_arg x pp_arg y

  let pp_instr fmt (instr : instr) =
    match instr with
    | Adr (x, y) -> Format.fprintf fmt "adr %a %a" pp_arg x pp_arg y
    | Add (x, y) -> Format.fprintf fmt "add %a %a" pp_arg x pp_arg y
    | Bl l -> Format.fprintf fmt "bl %a" pp_label l
    | Bx l -> Format.fprintf fmt "bx %a" pp_reg l
    | Ldr l -> Format.fprintf fmt "ldr %a" pp_load l
    | Mov (x, y) -> Format.fprintf fmt "mov %a %a" pp_arg y pp_arg x
    | Pop x -> Format.fprintf fmt "pop %a" pp_arg x
    | Push x -> Format.fprintf fmt "push %a" pp_arg x
    | Rsbs (x, y) -> Format.fprintf fmt "rsbs %a %a" pp_arg x pp_arg y
    | Str (x, y) -> Format.fprintf fmt "str %a %a" pp_arg x pp_arg y
    | Sub (x, y) -> Format.fprintf fmt "sub %a %a" pp_arg x pp_arg y

  let pp_label fmt (label, block) =
    Format.fprintf fmt "%s: \n" label;
    List.iter (fun instr -> Format.fprintf fmt "  %a\n" pp_instr instr) block;
    Format.fprintf fmt "\n"

  let pp fmt (program : program) = List.iter (pp_label fmt) program.labels
end

let pp = Printer.pp
