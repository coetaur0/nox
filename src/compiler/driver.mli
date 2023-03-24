(** Compiler driver module. *)

(** [run_repl ()] runs the interactive interpreter (REPL). *)
val run_repl : unit -> unit

(** [interpret files] runs the interpreter on a sequence of modules in a list of [files]. *)
val interpret : string list -> unit

(** [compile files directory] compiles the modules stored in a list of [files] and writes the
    resulting Lua modules in '.lua' files with the same name, in some output [directory]. *)
val compile : string list -> string -> unit
