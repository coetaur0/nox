open Nox

let usage_message = "nox [-emit-lua] <file>"

let emit_lua = ref false

let filepath = ref ""

let anon_fun path = if !filepath = "" then filepath := path

let speclist = [("-emit-lua", Arg.Set emit_lua, "Emit Lua source code")]

let () =
  Arg.parse speclist anon_fun usage_message;
  match (!emit_lua, !filepath) with
  | (true, "") -> prerr_endline "Error: expect a .nox file to compile as argument."
  | (false, "") -> Driver.run_repl ()
  | (true, _) -> Driver.compile !filepath
  | (false, _) -> Driver.interpret !filepath
