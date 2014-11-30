open CibleType

let () =
  let file = try Sys.argv.(1) with _ -> "exemples/fact.pcf" in
  let source = Utils.parse file in
  let prog = Comp.compile source in
  print_endline "Source program:" ;
  SourcePrinter.print_source source ;
  print_newline() ;
  print_endline "Compiled program:" ;
  CiblePrinter.print_code prog ;
  print_newline() ;
  print_endline "Interpretation:" ;
  CiblePrinter.print_val (CibleSem.top prog) ;
  print_newline()
