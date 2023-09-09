open C_var

exception Cannot_turn_expr_to_atom of L_mon_var.expr

module Env : sig
  type t

  val make : unit -> t
  val vars : t -> var list
  val add_var : t -> var -> t
end = struct
  type t = (var, unit) Hashtbl.t

  let make () = Hashtbl.create 128
  let vars t = Hashtbl.to_seq_keys t |> List.of_seq |> List.rev

  let add_var t v =
    Hashtbl.replace t v ();
    t
end

let to_atom ast =
  match ast with L_mon_var.Int x -> Int x | L_mon_var.Var v -> Var v

let rec to_tail ~env ast =
  match ast with
  | L_mon_var.Atom (L_mon_var.Int n) -> Return (Atom (Int n))
  | L_mon_var.Atom (L_mon_var.Var v) -> Return (Atom (Var v))
  | L_mon_var.Prim L_mon_var.Read -> Return Read
  | L_mon_var.Prim (L_mon_var.Add (x, y)) -> Return (Add (to_atom x, to_atom y))
  | L_mon_var.Prim (L_mon_var.Neg x) -> Return (Neg (to_atom x))
  | L_mon_var.Let (name, rhs, body) ->
      to_assign ~env name rhs (to_tail ~env body)

and to_assign ~env var rhs tail =
  let env = Env.add_var env var in
  match rhs with
  | L_mon_var.Atom atom -> Seq (Assign (var, Atom (to_atom atom)), tail)
  | L_mon_var.Prim L_mon_var.Read -> Seq (Assign (var, Read), tail)
  | L_mon_var.Prim (L_mon_var.Add (x, y)) ->
      Seq (Assign (var, Add (to_atom x, to_atom y)), tail)
  | L_mon_var.Prim (L_mon_var.Neg x) -> Seq (Assign (var, Neg (to_atom x)), tail)
  | L_mon_var.Let (var', rhs, _body) ->
      let tail = Seq (Assign (var, Atom (Var var')), tail) in
      to_assign ~env var' rhs tail

let run : L_mon_var.expr -> C_var.program =
 fun ast ->
  let env = Env.make () in
  let block = to_tail ~env ast in
  let locals = Env.vars env in
  [ ({ locals }, block) ]
