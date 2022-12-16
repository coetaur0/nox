(** Parsing module. *)

(** An exception raised by the parser when it encounters syntax errors. *)
exception SyntaxError of Diagnostic.t list

(** [parse source] is the result obtained after parsing some [source].
    This function raises a [SyntaxError] exception when it encounters errors during parsing. *)
val parse : Source.t -> Ast.stmt Ast.node list
