open Nox

let usage_message = "nox [-compile] [<file1>] [<file2>] ... [-output <directory>]"

let compile = ref false

let output = ref ""

let files = ref []

let anon_fun file = files := file :: !files

let speclist =
  [ ("-compile", Arg.Set compile, "Compile to Lua source code");
    ("-output", Arg.Set_string output, "Output directory for compilation") ]

let () =
  Arg.parse speclist anon_fun usage_message;
  match (!compile, !files) with
  | (false, []) -> Driver.run_repl ()
  | (false, _) -> Driver.interpret (List.rev !files)
  | (true, _) -> (
    match !output with
    | "" -> print_endline "Please specify an output directory for compilation"
    | _ -> Driver.compile (List.rev !files) !output )
