type var = string

type term = Const of int | Var of var | Sum of term * term | Minus of term * term | Prod of term * term | Div of term * term | Apply of term * term | Fun of var * term | FixFun of var * var * term | Let of var * term * term | Ifz of term * term * term
