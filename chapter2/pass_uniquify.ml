open L_var

module Env : sig
  type t

  val make : unit -> t
  val get : t -> var -> var
  val gensym : t -> var -> t * var
end = struct
  let var_id = ref 0

  type t = (var, var) Hashtbl.t

  let make () = Hashtbl.create 128
  let get t n = Hashtbl.find t n

  let gensym t v =
    var_id := !var_id + 1;
    let v' = v ^ "/" ^ (!var_id |> string_of_int) in
    Hashtbl.add t v v';
    (t, v')
end

let rec do_run ast ~env =
  match ast with
  | Int _ -> ast
  | Prim Read -> ast
  | Prim (Add (x, y)) -> Prim (Add (do_run ~env x, do_run ~env y))
  | Prim (Neg x) -> Prim (Neg (do_run ~env x))
  | Var var -> Var (Env.get env var)
  | Let (var, expr, body) ->
      let env, var = Env.gensym env var in
      let expr = do_run ~env expr in
      let body = do_run ~env body in
      Let (var, expr, body)

let run (ast : L_var.expr) = do_run ast ~env:(Env.make ())
