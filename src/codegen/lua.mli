(** Lua code generation module. *)

(** [emit stmts] is the source code obtained after transforming a sequence of IR statements into
    Lua statements. *)
val emit : Ir.stmt list -> string
