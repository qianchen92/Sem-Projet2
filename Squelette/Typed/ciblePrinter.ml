open CibleType
open CibleSem

open CibleType

let print_var (Var x) =
	print_string x
	
let print_instr_short instr =
	match instr with 
	| Add -> print_string "Add"
	| Sub -> print_string "Sub"
	| Mult -> print_string "Mult"
	| Div -> print_string "Div"
	| Ldi n -> 
		print_string "Ldi " ;
		print_int n
	| Push -> print_string "Push"
	| Test (i, j) -> print_string "Test"
	| Extend x -> 
		print_string "Extend " ; 
		print_var x 
	| Search x -> 
		print_string "Search " ; 
		print_var x 
	| Pushenv -> print_string "Pushenv"
	| Popenv -> print_string "Popenv"
	| MkClose (f, x, i) -> 
		print_string "MkClose " ; 
		begin
		match f with
		| Some (Var name) -> print_string name
		| None -> print_string "_"
		end ;
		print_string " " ;
		print_var x ;
	| Apply -> print_string "Apply"

let rec print_code code =
	match code with
	| [] -> ()
	| i :: r ->
		print_instr i ;
		print_code_suite r

and print_code_suite code =
	match code with
	| [] -> ()
	| i :: r ->
		print_string " ; " ;
		print_instr i ;
		print_code_suite r
		
and print_instr instr =
	match instr with 
	| Add -> print_string "Add"
	| Sub -> print_string "Sub"
	| Mult -> print_string "Mult"
	| Div -> print_string "Div"
	| Ldi n -> 
		print_string "Ldi " ;
		print_int n
	| Push -> print_string "Push"
	| Test (i, j) -> 
		print_string "Test (" ; 
		print_code i ;
		print_string ") (" ; 
		print_code j ;
		print_string ")"
	| Extend x -> 
		print_string "Extend " ; 
		print_var x 
	| Search x -> 
		print_string "Search " ; 
		print_var x 
	| Pushenv -> print_string "Pushenv"
	| Popenv -> print_string "Popenv"
	| MkClose (f, x, i) -> 
		print_string "MkClose " ; 
		begin
		match f with
		| Some (Var name) -> print_string name
		| None -> print_string "_"
		end ;
		print_string " " ;
		print_var x ;
		print_string " (" ; 
		print_code i ;
		print_string ")"
	| Apply -> print_string "Apply"

let print_val value =
	match value with
	| Int n -> 
		print_string "Entier " ; 
		print_int n
	| Env _ -> print_string "Environnement"
	| Close _ -> 
		print_string "Fonction"
