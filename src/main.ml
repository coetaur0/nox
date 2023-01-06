open Nox

let () =
  let source = Source.make "fn fact(n) {if n <= 0 {1} else {n * fact(n - 1)}}; fact(5)" in
  try
    let stmts = Parser.parse source in
    let ty = Typechecker.infer stmts in
    print_endline (Printer.type_repr ty);
    let ir = Lowerer.lower stmts in
    print_endline (Printer.ir_repr ir)
  with
  | Parser.SyntaxError diagnostics ->
    List.iter (fun diagnostic -> print_endline (Printer.diagnostic_repr diagnostic)) diagnostics
  | Typechecker.TypeError diagnostic -> print_endline (Printer.diagnostic_repr diagnostic)
