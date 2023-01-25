(** Compiler environment. *)

include Map.S with type key = Ast.name

(** [of_list list] is a new environment built from a list of pairs of strings and values. *)
val of_list : (string * 'a) list -> 'a t
