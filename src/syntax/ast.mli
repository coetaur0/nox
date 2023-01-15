(** Abstract syntax tree (AST). *)

(** A name in the AST. *)
type name = string

(** An AST node. *)
type 'a node =
  { value : 'a;
    span : Source.span }

(** A binary operator. *)
type binop =
  | Or
  | And
  | Eq
  | Ne
  | Le
  | Ge
  | Lt
  | Gt
  | Add
  | Sub
  | Mul
  | Div

(** A unary operator. *)
type unop =
  | Not
  | Neg

(** An AST statement. *)
type stmt =
  | Fn of (name * name list * expr node) list
  | Let of name * expr node
  | Expr of expr

(** An AST expression. *)
and expr =
  | Binary of binop * expr node * expr node
  | Unary of unop * expr node
  | Block of stmt node list
  | If of expr node * expr node * expr node
  | App of expr node * expr node list
  | Lambda of name list * expr node
  | Var of name
  | Number of float
  | Boolean of bool
  | Unit
  | Invalid
