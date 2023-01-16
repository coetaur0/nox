(** Compiler driver module. *)

(** [run_repl ()] runs the interactive interpreter (REPL). *)
val run_repl : unit -> unit

(** [compile path] compiles the contents of a file at some [path] and writes the result in a new Lua
    file with the same name. *)
val compile : string -> unit
