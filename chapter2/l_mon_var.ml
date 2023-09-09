open Sexplib.Std

type var = string [@@deriving sexp]
type atom = Int of int | Var of var [@@deriving sexp]

type expr = Atom of atom | Prim of op | Let of var * expr * expr
[@@deriving sexp]

and op = Read | Add of atom * atom | Neg of atom [@@deriving sexp]
