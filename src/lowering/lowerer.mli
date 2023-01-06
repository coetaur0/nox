(** AST lowering module. *)

(** [lower stmts] is the result obtained after lowering a sequence of AST statements into IR
    statements. *)
val lower : Ast.stmt Ast.node list -> Ir.stmt list
