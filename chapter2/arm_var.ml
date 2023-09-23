open Sexplib.Std

type label = string [@@deriving sexp]
type var = string [@@deriving sexp]
type reg = Arm.reg [@@deriving sexp]

type arg = Int of int | Reg of reg | Offset of reg * arg | Var of var
[@@deriving sexp]

type load = Prefix of arg * arg | Postfix of arg * arg | Offset of arg * arg
[@@deriving sexp]

type instr =
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
