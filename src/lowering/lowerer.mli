(** AST lowering module. *)

(** A lowering environment mapping source names to IR names. *)
type env = string Environment.t

(** The initial lowering environment for the AST lowerer. *)
val init_env : env

(** [lower stmts] is the result obtained after lowering a sequence of AST statements into IR
    statements. *)
val lower : env -> Ast.stmt Ast.node list -> Ir.stmt list
