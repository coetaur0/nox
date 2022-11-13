(** Type checking module. *)

(** An exception raised by the type checker when it encounters a type error. *)
exception TypeError of Diagnostic.t

(** [infer stmts] is the type inferred by the type checker for a sequence of statements.
    This function raises a [TypeError] exception when it encounters a type error. *)
val infer : Ast.stmt list -> Types.t
