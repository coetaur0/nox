(** Type definition module. *)

(** A type variable name. *)
type name = string

(** A type. *)
type t =
  | Fn of t list * t
  | Generic of name
  | Var of var ref
  | Number
  | Boolean
  | Unit

(** A type variable. *)
and var =
  | Bound of t
  | Free of name
