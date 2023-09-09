open Sexplib.Std

type label = string [@@deriving sexp]
type var = string [@@deriving sexp]
type atom = Int of int | Var of var [@@deriving sexp]

type expr = Atom of atom | Read | Add of atom * atom | Neg of atom
[@@deriving sexp]

type statement = Assign of var * expr [@@deriving sexp]
type tail = Return of expr | Seq of statement * tail [@@deriving sexp]
type info = { locals : var list } [@@deriving sexp]
type program = (info * tail) list [@@deriving sexp]
