open Nox

let usage_message = "nox <file>"

let filepath = ref ""

let anon_fun path = if !filepath = "" then filepath := path

let () =
  Arg.parse [] anon_fun usage_message;
  if !filepath <> "" then
    Driver.compile !filepath
  else
    Driver.run_repl ()
