let () =
  let file = try Sys.argv.(1) with _ -> "exemples/fact.pcf" in
  let prog = Comp.go_compile file in
  print_endline "Compiled program:";
  print_endline (CiblePrinter.print_code prog);
  print_endline "Interpretation:";
  print_endline (CiblePrinter.print_val (CibleSem.top prog))
