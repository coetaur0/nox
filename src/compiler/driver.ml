(* ----- Utility functions ---------------------------------------------------------------------- *)

let rec read_input start =
  if start then
    print_string "> "
  else
    print_string "| ";
  let input = Stdlib.read_line () in
  let length = String.length input in
  if length < 2 || String.sub input (length - 2) 2 <> ";;" then
    input ^ "\n" ^ read_input false
  else
    String.sub input 0 (length - 2)

let interpret_file path type_env runtime_env =
  let stmts = path |> Source.from_file |> Parser.parse in
  let (_, ty) = Typechecker.infer type_env stmts in
  let (_, value) = Interpreter.run runtime_env stmts in
  (ty, value)

let print_syntax_errors source diagnostics =
  print_endline (Printf.sprintf "Encountered syntax errors in %s:" source);
  List.iter
    (fun diagnostic -> print_endline (Printf.sprintf "  * %s" (Printer.diagnostic_repr diagnostic)))
    diagnostics

let print_type_error source diagnostic =
  print_endline (Printf.sprintf "Encountered a type error in %s:" source);
  print_endline (Printf.sprintf "  * %s" (Printer.diagnostic_repr diagnostic))

(* ----- REPL functions ------------------------------------------------------------------------- *)

let run_command input type_env runtime_env continue =
  let source = Source.make input in
  let lexer = Lexer.make source in
  match Source.read source (Lexer.next lexer).span with
  | "load" -> (
    let token = Lexer.next lexer in
    match token.kind with
    | Token.String -> (
      let string = Source.read source token.span in
      let file = String.sub string 1 (String.length string - 2) in
      let module_name = "\"" ^ (file |> Filename.basename |> Filename.remove_extension) ^ "\"" in
      try
        let (ty, value) = interpret_file file !type_env !runtime_env in
        type_env := Environment.add module_name ty !type_env;
        runtime_env := Environment.add module_name value !runtime_env;
        print_endline (Printf.sprintf " %s : %s" (Printer.value_repr value) (Printer.type_repr ty))
      with
      | Sys_error error -> print_endline error
      | Parser.SyntaxError diagnostics -> print_syntax_errors file diagnostics
      | Typechecker.TypeError diagnostic -> print_type_error file diagnostic )
    | _ -> print_endline "Invalid module name." )
  | "quit" -> continue := false
  | _ -> print_endline "Invalid REPL command."

let run_repl () =
  print_endline "Welcome to the Nox REPL.";
  let type_env = ref Typechecker.init_env in
  let runtime_env = ref Interpreter.init_env in
  let continue = ref true in
  while !continue do
    try
      let input = read_input true in
      if String.starts_with ~prefix:"$" input then
        run_command (String.sub input 1 (String.length input - 1)) type_env runtime_env continue
      else (
        try
          let stmts = input |> Source.make |> Parser.parse in
          let (type_env', ty) = Typechecker.infer !type_env stmts in
          let (runtime_env', value) = Interpreter.run !runtime_env stmts in
          type_env := type_env';
          runtime_env := runtime_env';
          print_endline
            (Printf.sprintf " %s : %s" (Printer.value_repr value) (Printer.type_repr ty))
        with
        | Parser.SyntaxError diagnostics -> print_syntax_errors "input" diagnostics
        | Typechecker.TypeError diagnostic -> print_type_error "input" diagnostic
        | Failure message | Invalid_argument message ->
          print_endline (Printf.sprintf "Runtime error: %s." message)
      )
    with End_of_file ->
      print_endline "";
      continue := false
  done

(* ----- Interpreter functions ------------------------------------------------------------------ *)

let interpret files =
  let type_env = ref Typechecker.init_env in
  let runtime_env = ref Interpreter.init_env in
  let rec interpret' = function
    | file :: rest -> (
      try
        let (ty, value) = interpret_file file !type_env !runtime_env in
        let module_name = "\"" ^ (file |> Filename.basename |> Filename.remove_extension) ^ "\"" in
        type_env := Environment.add module_name ty !type_env;
        runtime_env := Environment.add module_name value !runtime_env;
        interpret' rest
      with
      | Sys_error error -> print_endline error
      | Parser.SyntaxError diagnostics -> print_syntax_errors file diagnostics
      | Typechecker.TypeError diagnostic -> print_type_error file diagnostic )
    | [] -> ()
  in
  ignore (interpret' files)

(* ----- Compilation functions ------------------------------------------------------------------ *)

let compile files directory =
  let type_env = ref Typechecker.init_env in
  let rec compile' = function
    | file :: rest -> (
      try
        let stmts = file |> Source.from_file |> Parser.parse in
        let (_, ty) = Typechecker.infer !type_env stmts in
        let lua = stmts |> Lowerer.lower Lowerer.init_env |> Lua.emit in
        let module_name = file |> Filename.basename |> Filename.remove_extension in
        type_env := Environment.add ("\"" ^ module_name ^ "\"") ty !type_env;
        let output = Out_channel.open_text (directory ^ module_name ^ ".lua") in
        Out_channel.output_string output lua;
        Out_channel.close output;
        compile' rest
      with
      | Sys_error error -> print_endline error
      | Parser.SyntaxError diagnostics -> print_syntax_errors file diagnostics
      | Typechecker.TypeError diagnostic -> print_type_error file diagnostic )
    | [] -> ()
  in
  ignore (compile' files)
