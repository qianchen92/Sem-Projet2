let () =
  let file = try Sys.argv.(1) with _ -> "exemples/fact.pcf" in
  let prog = Comp.go_compile file in
  print_endline "Compiled program:";
  CiblePrinter.print_code prog;
  print_newline();
  print_endline "Interpretation:";
  CiblePrinter.print_val (CibleSem.top prog);
  print_newline()
