open X86_var
open Sexplib.Std

type live_op = Var of var | Reg of reg [@@deriving sexp]
type liveness = {
  reads: live_op list;
  writes: live_op list;
}
[@@deriving sexp]

type block = (instr * liveness) list [@@deriving sexp]
type program = { info : info; labels : (label * block) list } [@@deriving sexp]
