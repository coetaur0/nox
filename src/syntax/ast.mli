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
  | Concat

(** A unary operator. *)
type unop =
  | Not
  | Neg
  | Ref
  | Deref

(** An AST statement. *)
type stmt =
  | Fun of (name * name list * expr node) list
  | Let of name * expr node
  | Update of expr node * expr node
  | Expr of expr

(** An AST expression. *)
and expr =
  | Binary of binop * expr node * expr node
  | Unary of unop * expr node
  | Block of stmt node list
  | If of expr node * expr node * expr node
  | App of expr node * expr node list
  | Record of expr node Environment.t * expr node
  | Select of expr node * name node
  | Lambda of name list * expr node
  | Var of name
  | Number of float
  | Boolean of bool
  | String of string
  | EmptyRecord
  | Unit
  | Invalid
