open CibleType
open SourceType

let rec type_checker (t: SourceType.term) (e: typ_env): SourceType.typ = TODO

let rec compile (t: SourceType.term): CibleType.code = TODO 

let go_compile file =
  let prog = Utils.parse file in
  let _ = type_checker prog init_typ_env in
  compile prog
