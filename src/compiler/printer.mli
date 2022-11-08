(** Pretty printing module. *)

(** [string_of_span span] is a string representation for a [span]. *)
val string_of_span : Source.span -> string

(** [string_of_diagnostic diagnostic] is a string representation for a [diagnostic]. *)
val string_of_diagnostic : Diagnostic.t -> string

(** [string_of_stmts stmts] is a string representation for a sequence of statements. *)
val string_of_stmts : Ast.stmt list -> string

(** [string_of_stmt stmt] is a string representation for a statement. *)
val string_of_stmt : Ast.stmt -> string

(** [string_of_decl decl] is a string representation for a declaration.  *)
val string_of_decl : Ast.decl Ast.node -> string

(** [string_of_expr expr] is a string representation for an expression. *)
val string_of_expr : Ast.expr Ast.node -> string

(** [string_of_type ty] is a string representation for a type. *)
val string_of_type : Types.t -> string
