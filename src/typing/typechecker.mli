(** Type checking module. *)

(** An exception raised by the type checker when it encounters a type error. *)
exception TypeError of Diagnostic.t

(** A typing environment mapping variable names to their types. *)
type env = Types.t Environment.t

(** [infer env stmts] is the type inferred by the type checker for a sequence of statements in some
    initial environment, along with the new environment obtained after doing so.
    This function raises a [TypeError] exception when it encounters a type error. *)
val infer : env -> Ast.stmt Ast.node list -> env * Types.t
