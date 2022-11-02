open Nox

let () =
  let source = Source.make "let x = lam(a, b) {a + b}" in
  try
    let stmts = Parser.parse source in
    print_endline (Printer.string_of_stmts stmts)
  with
    Parser.SyntaxError diagnostics ->
      List.iter 
        (fun diagnostic -> print_endline (Printer.string_of_diagnostic diagnostic)) 
        diagnostics
