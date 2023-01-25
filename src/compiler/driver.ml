(* ----- Utility functions ---------------------------------------------------------------------- *)

let rec read_input start =
  if start then
    print_string "> "
  else
    print_string "| ";
  let input = Stdlib.read_line () in
  let length = String.length input in
  if length < 2 || String.sub input (length - 2) 2 <> ";;" then
    input ^ read_input false
  else
    String.sub input 0 (length - 2)

let print_syntax_errors source diagnostics =
  print_endline (Printf.sprintf "Encountered syntax errors in %s:" source);
  List.iter
    (fun diagnostic -> print_endline (Printf.sprintf "  * %s" (Printer.diagnostic_repr diagnostic)))
    diagnostics

let print_type_error source diagnostic =
  print_endline (Printf.sprintf "Encountered a type error in %s:" source);
  print_endline (Printf.sprintf "  * %s" (Printer.diagnostic_repr diagnostic))

(* ----- REPL functions ------------------------------------------------------------------------- *)

let run_repl () =
  print_endline "Welcome to the Nox REPL.";
  let type_env = ref Typechecker.init_env in
  let runtime_env = ref Interpreter.init_env in
  let continue = ref true in
  while !continue do
    try
      let stmts = read_input true |> Source.make |> Parser.parse in
      let (type_env', ty) = Typechecker.infer !type_env stmts in
      let (runtime_env', value) = Interpreter.run !runtime_env stmts in
      type_env := type_env';
      runtime_env := runtime_env';
      print_endline (Printf.sprintf " %s : %s" (Printer.value_repr value) (Printer.type_repr ty))
    with
    | End_of_file ->
      print_endline "";
      continue := false
    | Parser.SyntaxError diagnostics -> print_syntax_errors "input" diagnostics
    | Typechecker.TypeError diagnostic -> print_type_error "input" diagnostic
  done

(* ----- Compilation functions ------------------------------------------------------------------ *)

let compile path =
  try
    let stmts = Source.from_file path |> Parser.parse in
    ignore (Typechecker.infer Typechecker.init_env stmts);
    let lua = stmts |> Lowerer.lower Lowerer.init_env |> Lua.emit in
    let output = Out_channel.open_text (Filename.remove_extension path ^ ".lua") in
    Out_channel.output_string output lua;
    Out_channel.close output
  with
  | Sys_error error -> print_endline error
  | Parser.SyntaxError diagnostics -> print_syntax_errors path diagnostics
  | Typechecker.TypeError diagnostic -> print_type_error path diagnostic
