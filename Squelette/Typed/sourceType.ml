type typ =
  |Nat
  |Arr of typ*typ

type var = string
		  
type term =
  | Const of int
  | Var of var
  | Sum of term * term
  | Minus of term * term
  | Prod of term * term
  | Div of term * term
  | Apply of term * term
  | Fun of var * typ * term 
  | FixFun of var * typ * var * term
  | Let of var * term * term
  | Ifz of term * term * term
