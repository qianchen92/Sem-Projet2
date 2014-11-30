open SourceType

let rec print_source c =
	match c with
	| Const n -> 
		print_string "Const " ;
		print_int n ;
	| Var x -> 
		print_string "Var " ;
		print_string x ;
	| Sum (t1,t2) -> 
		print_string "Sum (" ;
		print_source t1 ;
		print_string ", " ;
		print_source t2 ;
		print_string ")" ;
	| Minus (t1,t2) -> 
		print_string "Minus (" ;
		print_source t1 ;
		print_string ", " ;
		print_source t2 ;
		print_string ")" ;
	| Prod (t1,t2) -> 
		print_string "Prod (" ;
		print_source t1 ;
		print_string ", " ;
		print_source t2 ;
		print_string ")" ;
	| Div (t1,t2) -> 
		print_string "Div (" ;
		print_source t1 ;
		print_string ", " ;
		print_source t2 ;
		print_string ")" ;
	| Apply (t1,t2) -> 
		print_string "Apply (" ;
		print_source t1 ;
		print_string ", " ;
		print_source t2 ;
		print_string ")" ;
	| Fun (x,t) -> 
		print_string "Fun (" ;
		print_string x ;
		print_string ", " ;
		print_source t ;
		print_string ")" ;
	| FixFun (f,x,t) -> 
		print_string "FixFun (" ;
		print_string f ;
		print_string ", " ;
		print_string x ;
		print_string ", " ;
		print_source t ;
		print_string ")" ;
	| Let (x,t1,t2) -> 
		print_string "Let (" ;
		print_string x ;
		print_string ", " ;
		print_source t1 ;
		print_string ", " ;
		print_source t2 ;
		print_string ")" ;
	| Ifz (t0,t1,t2) -> 
		print_string "Ifz (" ;
		print_source t0 ;
		print_string ", " ;
		print_source t1 ;
		print_string ", " ;
		print_source t2 ;
		print_string ")" ;
