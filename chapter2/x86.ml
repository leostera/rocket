open Sexplib.Std

type label = string [@@deriving sexp]

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

type arg = Lit of int | Reg of reg | Deref of int * reg [@@deriving sexp]

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
  | Label of instr
[@@deriving sexp]

type info = string [@@deriving sexp]
type block = info * instr list [@@deriving sexp]
type program = { info : info; labels : (label * block) list } [@@deriving sexp]