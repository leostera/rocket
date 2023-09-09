open L_var

type error = Expected_int

exception Eval_error of error

module Env : sig
  type t

  val make : unit -> t
  val get : t -> var -> expr
  val set : t -> var -> expr -> t
end = struct
  type t = (var, expr) Hashtbl.t

  let make () = Hashtbl.create 128
  let get t n = Hashtbl.find t n

  let set t n v =
    Hashtbl.add t n v;
    t
end

let rec do_eval ast ~env =
  match ast with
  | Var v -> Env.get env v
  | Let (v, exp, body) ->
      let env = Env.set env v (do_eval ~env exp) in
      do_eval ~env body
  | Int _ -> ast
  | Prim Read ->
      print_endline "type a number: ";
      Int (read_int ())
  | Prim (Add (left, right)) -> (
      let left = do_eval ~env left in
      let right = do_eval ~env right in
      match (left, right) with
      | Int left, Int right -> Int (left + right)
      | _ -> raise (Eval_error Expected_int))
  | Prim (Neg x) -> (
      match do_eval ~env x with
      | Int x -> Int (-x)
      | _ -> raise (Eval_error Expected_int))

let eval (parsetree : expr) = do_eval parsetree ~env:(Env.make ())
