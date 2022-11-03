open Nox

let () =
  let source = Source.make "10 < <x> {x} (100)" in
  try
    let stmts = Parser.parse source in
    print_endline (Printer.string_of_stmts stmts)
  with
    Parser.SyntaxError diagnostics ->
      List.iter 
        (fun diagnostic -> print_endline (Printer.string_of_diagnostic diagnostic)) 
        diagnostics
