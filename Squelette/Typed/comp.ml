open CibleType
open SourceType

let rec type_checker (t: SourceType.term) (e: typ_env): SourceType.typ =
  match t with
  | Const n -> Nat
  | Var x -> eval_typ e (Var x)
  | Sum (t1,t2) ->
     if type_checker t1 e = Nat && type_checker t2 e = Nat then
       Nat
     else
       failwith "type_check: program not typed correctly (arithmetic with non-nat type)"
  | Minus (t1,t2) -> 
     if type_checker t1 e = Nat && type_checker t2 e = Nat then
       Nat
     else
       failwith "type_check: program not typed correctly (arithmetic with non-nat type)"
  | Prod (t1,t2) -> 
     if type_checker t1 e = Nat && type_checker t2 e = Nat then
       Nat
     else
       failwith "type_check: program not typed correctly (arithmetic with non-nat type)"
  | Div (t1,t2) -> 
     if type_checker t1 e = Nat && type_checker t2 e = Nat then
       Nat
     else
       failwith "type_check: program not typed correctly (arithmetic with non-nat type)"
  | Apply (t1,t2) ->
     begin
       match t1 with
       | Var x ->
	  begin
	    match eval_typ e (Var x) with
	    | Arr (tau1,tau2) ->
	       if tau1 = type_checker t2 e then
		 tau2
	       else
		 failwith "type_check: program not typed correctly (function's argument has wrong type)"
	    | Nat -> failwith "type_check: program not typed correctly (function with type nat)"
	  end
       | _ -> failwith "apply argument to a non-function type element"
     end
  | Fun (x,ty,t) ->
     type_checker t (update_typ e (Var x) ty)
  | FixFun (f,ty,x,t) -> 		 
     begin
       match ty with
       | Arr (tau1,tau2) ->
	  if tau2 = type_checker t (update_typ (update_typ e (Var f) ty) (Var x) tau1) then
	    ty
	  else
	    failwith "type_check: program not typed correctly (recursif function's return type is not matched)"
       | Nat -> failwith "type_check: program not typed correctly (function with type nat)"
     end      
  | Let (x,t1,t2) ->
     type_checker t2 (update_typ e (Var x) (type_checker t1 e))
  | Ifz (t0,t1,t2) ->
     if type_checker t0 e = Nat then
       let type1 = type_checker t1 e in
       let type2 = type_checker t2 e in
       if type1 = type2 then
	 type1
       else
	 failwith "type_ckeck: program not typed correctly (if 's two branches return two different type)"
     else
       failwith "type_ckeck: program not typed correctly (if 's condition return a non-nat element)"


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
  ignore(type_checker prog init_typ_env);
  print_endline "Type checked";
  let _ = type_checker prog init_typ_env in
  compile prog
