(** Intermediate representation (IR). *)

(** An IR statement. *)
type stmt =
  | Fun of (Ast.name * Ast.name list * stmt list) list
  | Decl of Ast.name
  | Assign of expr * expr
  | CopyRecord of Ast.name * expr
  | If of expr * stmt list * stmt list
  | Return of expr

(** An IR expression. *)
and expr =
  | Binary of Ast.binop * expr * expr
  | Unary of Ast.unop * expr
  | App of expr * expr list
  | Select of expr * Ast.name
  | Lambda of Ast.name list * stmt list
  | Var of Ast.name
  | Number of float
  | Boolean of bool
  | String of string
  | EmptyRecord
  | Unit
