open X86_var
open Sexplib.Std

type live_op = Reg of reg | Deref of int * reg | Var of var [@@deriving sexp]

let empty_liveness : live_op list = []

type block = (instr * live_op list) list [@@deriving sexp]
type program = { info : info; labels : (label * block) list } [@@deriving sexp]
