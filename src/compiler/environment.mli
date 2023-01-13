(** Compiler environment. *)

(** A compiler environment maps variable names to some type of information. *)
include Map.S with type key = Ast.name
