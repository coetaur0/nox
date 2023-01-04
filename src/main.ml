open Nox

let () =
  let source = Source.make "<x> {let y = <z> {x(z)}; y}" in
  try
    let stmts = Parser.parse source in
    let ty = Typechecker.infer stmts in
    print_endline (Printer.type_repr ty)
  with
  | Parser.SyntaxError diagnostics ->
    List.iter (fun diagnostic -> print_endline (Printer.diagnostic_repr diagnostic)) diagnostics
  | Typechecker.TypeError diagnostic -> print_endline (Printer.diagnostic_repr diagnostic)
