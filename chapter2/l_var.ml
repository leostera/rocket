open Sexplib.Std

type var = string [@@deriving sexp]

type expr = Int of int | Prim of op | Var of var | Let of var * expr * expr
[@@deriving sexp]

and op = Read | Add of expr * expr | Neg of expr [@@deriving sexp]
