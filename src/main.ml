open Nox

let () =
  let source = Source.make "fn g() {f()} fn f() {42}; g()" in
  try
    let stmts = Parser.parse source in
    let (_, ty) = Typechecker.infer Environment.empty stmts in
    print_endline ("Type: " ^ Printer.type_repr ty ^ "\n");
    let ir = Lowerer.lower stmts in
    print_endline ("IR representation:\n" ^ Printer.ir_repr ir ^ "\n");
    let lua = Lua.emit ir in
    print_endline ("Lua code:\n" ^ lua)
  with
  | Parser.SyntaxError diagnostics ->
    List.iter (fun diagnostic -> print_endline (Printer.diagnostic_repr diagnostic)) diagnostics
  | Typechecker.TypeError diagnostic -> print_endline (Printer.diagnostic_repr diagnostic)
