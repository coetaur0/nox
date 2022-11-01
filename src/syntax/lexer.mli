(** Lexing module. *)

(** A lexical analyser (lexer).
    A lexer acts as a stream from which tokens can be pulled. *)
type t

(** [make source] is a new lexer for some [source]. *)
val make : Source.t -> t

(** [next lexer] is the next token in the [lexer]'s stream. *)
val next : t -> Token.t
