(** Runtime values. *)

(** A runtime environment mapping variable names to their values. *)
type env = t Environment.t

(** A runtime value manipulated by the interpreter. *)
and t =
  | Closure of env ref * Ast.name list * Ast.expr Ast.node
  | Number of float
  | Boolean of bool
  | Unit
