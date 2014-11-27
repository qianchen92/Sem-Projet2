open CibleType
open SourceType

let rec compile (t: SourceType.term): CibleType.code = TODO.

let go_compile file =
  let prog = Utils.parse file in
  compile prog
