(** Compiler diagnostic definition module. *)

(** A compiler diagnostic. *)
type t = {message : string; span : Source.span}
