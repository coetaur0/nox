(** Type checking module. *)

(** An exception raised by the type checker when it encounters a type error. *)
exception TypeError of Diagnostic.t

(** A typing environment mapping variable names to their types. *)
type env = Types.t Environment.t

(** The initial typing environment for the type checker. *)
val init_env : env

(** [infer env stmts] is the new environment and the type obtained by the type checker after
    performing type inference on a sequence of statements in some initial typing environment.
    This function raises a [TypeError] exception when it encounters a type error. *)
val infer : env -> Ast.stmt Ast.node list -> env * Types.t
