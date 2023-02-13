(** Compiler environment. *)

include Map.S with type key = string

(** [of_list list] is a new environment built from a list of pairs of strings and values. *)
val of_list : (string * 'a) list -> 'a t

(** [merge lhs rhs] is the environment obtained after merging [lhs] and [rhs]. When a key appears in
    both [lhs] and [rhs], the value in [lhs] is kept. *)
val merge : 'a t -> 'a t -> 'a t
