(** Interpreter module. *)

(** [run env stmts] is the new environment and the runtime value obtained after evaluating a 
    sequence of statements in some initial runtime environment. *)
val run : Values.env -> Ast.stmt Ast.node list -> Values.env * Values.t
