open Nox

let () =
  let source = Source.make "fn f(x) {fn g(x) {x + 1}; g(x)}; fn g(x) {x * 2}; f(3)" in
  try
    let stmts = Parser.parse source in
    let ty = Typechecker.infer stmts in
    print_endline ("Type: " ^ Printer.type_repr ty ^ "\n");
    let ir = Lowerer.lower stmts in
    print_endline ("IR representation:\n" ^ Printer.ir_repr ir ^ "\n");
    let lua = Lua.emit ir in
    print_endline ("Lua code:\n" ^ lua)
  with
  | Parser.SyntaxError diagnostics ->
    List.iter (fun diagnostic -> print_endline (Printer.diagnostic_repr diagnostic)) diagnostics
  | Typechecker.TypeError diagnostic -> print_endline (Printer.diagnostic_repr diagnostic)
