module M = Map.Make(String)

type var = Var of string

and value = Int of int | Env of env | Close of (var option) * var * code * env

and acc = value

and stack = value list

and env = value M.t

and instr = Add | Sub | Mult | Div | Ldi of int | Push | Test of code * code | Extend of var | Search of var | Pushenv | Popenv | MkClose of (var option) * var * code | Apply

and code = instr list

let eval_env (e: env) (Var x) : value =
  try (M.find x e)
  with Not_found -> failwith "Var undifined"

let update_env e (Var x) v = 
	M.add x v e

let init_env = 
	M.empty
