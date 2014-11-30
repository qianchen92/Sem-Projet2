open CibleType
open SourceType

let rec type_checker (t: SourceType.term) (e: typ_env): SourceType.typ =
  match t with
  | Const n -> Nat
  | Var x -> eval_typ e x
  | Sum (t1,t2) ->
     if type_checker t1 e = Nat && type_checker t2 e = Nat then
       Nat
     else
       failwith "type_check : program not typed correctly"
  | Minus (t1,t2) -> assert(false)
  | Prod (t1,t2) -> assert(false)
  | Div (t1,t2) -> assert(false)
  | Apply (t1,t2) -> assert(false)
  | Fun (x,ty,t) -> assert(false)
  | FixFun (f,ty,x,t) -> assert(false)
  | Let (x,t1,t2) -> assert(false)
  | Ifz (t0,t1,t2) -> assert(false)


let rec compile (t: SourceType.term): CibleType.code =
  match t with
  | Const n -> [Ldi n]
  | Var x -> [Search (Var x)]
  | Sum (t1,t2) -> (compile t1) @ [Push] @ (compile t2) @ [Add]
  | Minus (t1,t2) -> (compile t1) @ [Push] @ (compile t2) @ [Sub]
  | Prod (t1,t2) -> (compile t1) @ [Push] @ (compile t2) @ [Mult]
  | Div (t1,t2) -> (compile t1) @ [Push] @ (compile t2) @ [Div]
  | Apply (t1,t2) -> [Pushenv] @ (compile t2) @ [Push] @ (compile t1) @ [CibleType.Apply] @ [Popenv]
  | Fun (x,ty,t) -> [MkClose(None, Var x, (compile t))]
  | FixFun (f,ty,x,t) -> [MkClose(Some (Var f), Var x, (compile t))]
  | Let (x,t1,t2) -> [Pushenv] @ (compile t1) @ [Extend (Var x)] @ (compile t2) @ [Popenv]
  | Ifz (t0,t1,t2) -> (compile t0) @ [Test (compile t1, compile t2)]


let go_compile file =
  let prog = Utils.parse file in
  let _ = type_checker prog init_typ_env in
  compile prog
