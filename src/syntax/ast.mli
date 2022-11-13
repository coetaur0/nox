(** Abstract syntax tree (AST). *)

(** A name in the AST. *)
type name = string

(** An AST node. *)
type 'a node = {kind : 'a; span : Source.span}

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

(** A statement. *)
type stmt =
  | Decl of decl node
  | Expr of expr node

(** A declaration. *)
and decl =
  | Fn of name * name list * expr node
  | Let of name * expr node

(** An expression. *)
and expr =
  | Binary of binop * expr node * expr node
  | Unary of unop * expr node
  | Block of stmt list
  | If of expr node * expr node * expr node option
  | App of expr node * expr node list
  | Lambda of name list * expr node
  | Var of name
  | Number of float
  | Boolean of bool
  | Unit
  | Invalid
