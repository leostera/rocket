open L_mon_var

exception Cannot_turn_expr_to_atom of L_var.expr

module Env : sig
  val tmp : unit -> var
end = struct
  let var_id = ref 0

  let tmp () =
    var_id := !var_id + 1;
    "tmp/" ^ (!var_id |> string_of_int)
end

let is_complex_operand (expr : L_var.expr) =
  match expr with L_var.Int _ -> false | L_var.Var _ -> false | _ -> true

let to_atom (ast : L_var.expr) : atom =
  match ast with
  | L_var.Int x -> Int x
  | L_var.Var v -> Var v
  | _ -> raise (Cannot_turn_expr_to_atom ast)

let rec to_expr (ast : L_var.expr) : expr =
  match ast with
  | L_var.Int _ -> Atom (to_atom ast)
  | L_var.Prim L_var.Read -> Prim Read
  | L_var.Prim (L_var.Add (x, y)) ->
      if is_complex_operand x then
        let tmp_x = Env.tmp () in
        Let
          ( tmp_x,
            to_expr x,
            to_expr (L_var.Prim (L_var.Add (L_var.Var tmp_x, y))) )
      else if is_complex_operand y then
        let tmp_y = Env.tmp () in
        Let
          ( tmp_y,
            to_expr y,
            to_expr (L_var.Prim (L_var.Add (x, L_var.Var tmp_y))) )
      else Prim (Add (to_atom x, to_atom y))
  | L_var.Prim (L_var.Neg x) ->
      if is_complex_operand x then
        let tmp_x = Env.tmp () in
        Let (tmp_x, to_expr x, Prim (Neg (Var tmp_x)))
      else Prim (Neg (to_atom x))
  | L_var.Var var -> Atom (Var var)
  | L_var.Let (name, expr, body) -> Let (name, to_expr expr, to_expr body)

let run : L_var.expr -> L_mon_var.expr = fun ast -> to_expr ast
