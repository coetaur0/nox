(** Token definition module. *)

(** A lexeme is a unit of syntax in a source. *)
type lexeme =
  | Number
  | Boolean
  | Name
  | Fn
  | Let
  | If
  | Else
  | Assign
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
  | Not
  | LParen
  | RParen
  | LBrace
  | RBrace
  | Pipe
  | Comma
  | Semicolon
  | Eof
  | Unknown

(** A lexical token is represented by a lexeme and a span in a source. *)
type t = {kind : lexeme; span : Source.span}
