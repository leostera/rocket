open Sexplib.Std

type label = string [@@deriving sexp]
type var = string [@@deriving sexp]

type reg =
  | Rsp
  | Rbp
  | Rax
  | Rbx
  | Rcx
  | Rdx
  | Rsi
  | Rdi
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15
  | R16
[@@deriving sexp]

type arg = Int of int | Reg of reg | Deref of int * reg [@@deriving sexp]

type instr =
  | Addq of arg * arg
  | Subq of arg * arg
  | Negq of arg
  | Movq of arg * arg
  | Pushq of arg
  | Popq of arg
  | Callq of label
  | Reqt
  | Jmp of label
[@@deriving sexp]

type info = { locals : var list } [@@deriving sexp]
type block = instr list [@@deriving sexp]
type program = { info : info; labels : (label * block) list } [@@deriving sexp]

module Printer = struct
  let pp_reg fmt (reg : reg) =
    match reg with
    | Rsp -> Format.fprintf fmt "rsp"
    | Rbp -> Format.fprintf fmt "rbp"
    | Rax -> Format.fprintf fmt "rax"
    | Rbx -> Format.fprintf fmt "rbx"
    | Rcx -> Format.fprintf fmt "rcx"
    | Rdx -> Format.fprintf fmt "rdx"
    | Rsi -> Format.fprintf fmt "rsi"
    | Rdi -> Format.fprintf fmt "rdi"
    | R8 -> Format.fprintf fmt "r8"
    | R9 -> Format.fprintf fmt "r9"
    | R10 -> Format.fprintf fmt "r10"
    | R11 -> Format.fprintf fmt "r11"
    | R12 -> Format.fprintf fmt "r12"
    | R13 -> Format.fprintf fmt "r13"
    | R14 -> Format.fprintf fmt "r14"
    | R15 -> Format.fprintf fmt "r15"
    | R16 -> Format.fprintf fmt "r16"

  let pp_arg fmt (x : arg) =
    match x with
    | Int x -> Format.fprintf fmt "$%d" x
    | Reg reg -> Format.fprintf fmt "%%%a" pp_reg reg
    | Deref (offset, reg) -> Format.fprintf fmt "%d(%%%a)" offset pp_reg reg

  let pp_instr fmt (instr : instr) =
    match instr with
    | Addq (x, y) -> Format.fprintf fmt "addq %a, %a" pp_arg x pp_arg y
    | Subq (x, y) -> Format.fprintf fmt "subq %a, %a" pp_arg x pp_arg y
    | Negq x -> Format.fprintf fmt "negq %a" pp_arg x
    | Movq (x, y) -> Format.fprintf fmt "movq %a, %a" pp_arg x pp_arg y
    | Pushq arg -> Format.fprintf fmt "pushq %a" pp_arg arg
    | Popq arg -> Format.fprintf fmt "popq %a" pp_arg arg
    | Callq label -> Format.fprintf fmt "callq %s" label
    | Reqt -> Format.fprintf fmt "retq"
    | Jmp label -> Format.fprintf fmt "jmp %s" label

  let pp_label fmt (label, block) =
    Format.fprintf fmt "%s: \n" label;
    List.iter (fun instr -> Format.fprintf fmt "  %a\n" pp_instr instr) block;
    Format.fprintf fmt "\n"

  let pp fmt (program : program) =
    Format.fprintf fmt ".global _main\n";
    List.iter (pp_label fmt) program.labels
end

let pp = Printer.pp
