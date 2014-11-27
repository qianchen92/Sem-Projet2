open CibleType
open SourceType
open Type_inf

let rec compile (t: SourceType.term): CibleType.code = TODO 

let go_compile file =
  let prog = Utils.parse file in
  let _ = type_checker prog init_typ_env in
  compile prog
