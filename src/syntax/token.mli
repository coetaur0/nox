(** Lexical tokens. *)

(** A lexeme is a unit of syntax in a source. *)
type lexeme =
  | Number
  | Boolean
  | String
  | Name
  | Case
  | Open
  | Fun
  | Let
  | If
  | Else
  | Match
  | Assign
  | Update
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
  | Not
  | Ref
  | Deref
  | LParen
  | RParen
  | LBrace
  | RBrace
  | LBracket
  | RBracket
  | Comma
  | Semicolon
  | Pipe
  | Dot
  | Arrow
  | Eof
  | BadString
  | Unknown

(** A lexical token is represented by a lexeme and a span in a source. *)
type t =
  { kind : lexeme;
    span : Source.span }
