(** Pretty printing module. *)

(** [list_repr list string_fn separator] is a string representation for a list of values.
    [string_fn] is the function used to transform the values in the list into strings.
    [separator] is the string used to separate the elements of the list in the output. *)
val list_repr : 'a list -> ('a -> string) -> string -> string

(** [span_repr span] is a string representation for a [span]. *)
val span_repr : Source.span -> string

(** [diagnostic_repr diagnostic] is a string representation for a [diagnostic]. *)
val diagnostic_repr : Diagnostic.t -> string

(** [ast_repr stmts] is a string representation for a sequence of AST statements. *)
val ast_repr : Ast.stmt Ast.node list -> string

(** [type_repr ty] is a string representation for a type. *)
val type_repr : Types.t -> string

(** [value_repr value] is a string representation for a runtime value. *)
val value_repr : Values.t -> string

(** [ir_repr stmts] is a string representation for a sequence of IR statements. *)
val ir_repr : Ir.stmt list -> string
