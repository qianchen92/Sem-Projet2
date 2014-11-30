open CibleType
open SourceType

let rec compile (t: SourceType.term): CibleType.code = 
	match t with
	| Const n -> [Ldi n]
	| Var x -> [Search (Var x)]
	| Sum (t1,t2) -> (compile t1) @ [Push] @ (compile t2) @ [Add]
	| Minus (t1,t2) -> (compile t1) @ [Push] @ (compile t2) @ [Sub]
	| Prod (t1,t2) -> (compile t1) @ [Push] @ (compile t2) @ [Mult]
	| Div (t1,t2) -> (compile t1) @ [Push] @ (compile t2) @ [Div]
	| Apply (t1,t2) -> [Pushenv] @ (compile t2) @ [Push] @ (compile t2) @ [CibleType.Apply] @ [Popenv]
	| Fun (x,t) -> [MkClose(None, Var x, (compile t))]
	| FixFun (f,x,t) -> [MkClose(Some (Var f), Var x, (compile t))]
	| Let (x,t1,t2) -> [Pushenv] @ (compile t1) @ [Extend (Var x)] @ (compile t2) @ [Popenv]
	| Ifz (t0,t1,t2) -> (compile t0) @ [Test (compile t1, compile t2)]

let go_compile file =
  let prog = Utils.parse file in
  compile prog
