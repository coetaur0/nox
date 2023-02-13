(** Data types. *)

(** A type variable name. *)
type name = string

(** A type. *)
type t =
  | Fun of t list * t
  | Generic of name
  | Var of var ref
  | Ref of t
  | Record of t Environment.t * t
  | Number
  | Boolean
  | String
  | EmptyRecord
  | Unit

(** A type variable. *)
and var =
  | Bound of t
  | Free of name * int
