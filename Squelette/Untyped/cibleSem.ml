open CibleType
open List

exception Finished of value

let add v1 v2 =
	match (v1,v2) with
	| (Int n1, Int n2) -> n2 + n1
	| _ -> failwith "Fail: Add"

let sub v1 v2 =
	match (v1,v2) with
	| (Int n1, Int n2) -> n2 - n1
	| _ -> failwith "Fail: Sub"

let mult v1 v2 =
	match (v1,v2) with
	| (Int n1, Int n2) -> n2 * n1
	| _ -> failwith "Fail: Mult"

let div v1 v2 =
	match (v1,v2) with
	| (Int n1, Int n2) -> n2 / n1
	| _ -> failwith "Fail: Div"

let step ((a: acc), (p: stack), (e: env), (c: code)): (acc * stack * env * code) =
	match c with
	| [] -> raise (Finished a)
	| instr :: r ->
		begin
		match instr with
		| Add -> (Int (add a (hd p)), tl p, e, r)
		| Sub -> (Int (sub a (hd p)), tl p, e, r)
		| Mult -> (Int (mult a (hd p)), tl p, e, r)
		| Div -> (Int (div a (hd p)), tl p, e, r)
		| Ldi n -> (Int n, p, e, r)
		| Push -> (a, a :: p, e, r)
		| Test (i, j) -> 
			if a = Int 0
				then (a, p, e, i @ r)
				else (a, p, e, j @ r)
		| Extend x -> (a, p, update_env e x a, r)
		| Search x -> (eval_env e x, p, e, r)
		| Pushenv -> (a, (Env e) :: p, e, r)
		| Popenv -> 
			begin
			match (hd p) with
			| Env e -> (a, tl p, e, r)
			| _ -> failwith "Fail: Popenv"
			end
		| MkClose (f, x, i) -> (Close(f,x,i,e), p, e, r)
		| Apply -> 
			begin
			match a with
			| Close (Some f, x, i, e) ->
				let e' = update_env (update_env e x (hd p)) f a in
				(a, tl p, e', i @ r)
			| Close (None, x, i, e) ->
				let e' = update_env e x (hd p) in
				(a, tl p, e', i @ r)
			| _ -> failwith "Fail: Apply"
			end
		end
 
let rec run c =
	let quad = (Int 0, [], init_env, c) in
	let rec iter quad =
		let (a,p,e,c) = step quad in
		(*CiblePrinter.print_val a ;
		print_string " : " ;
		begin
		match p with
		| [] -> print_string "void"
		| v :: r -> CiblePrinter.print_val v ;
		end ;
		print_string " : " ;
		begin
		match c with
		| [] -> print_string "void"
		| i :: r -> CiblePrinter.print_instr_short i ;
		end ;
		print_newline() ;*)
		iter (a,p,e,c)
	in
	try
		iter quad
	with
		Finished a -> a

let top t = run t
