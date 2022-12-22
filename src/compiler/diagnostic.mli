(** Compiler diagnostics. *)

(** A compiler diagnostic. *)
type t =
  { message : string;
    span : Source.span }
