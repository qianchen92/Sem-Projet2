open SourceType
open List 

let count = ref 0

let fresh_var: typ = count := !count + 1; VarT (string_of_int !count)

let rec gen_constraints (t: term) (e: typ_env): (typ * constraints list) = TODO

let rec solve_constraints (l: constraints list) : constraints list = TODO

let type_checker (t: SourceType.term) (e: typ_env): typ = TODO
