(** Source manipulation module. *)

(** A source of code. *)
type t = {path : string option; content : string}

(** A position in a source. *)
type position = {line : int; column : int; offset : int}

(** A span between two locations in a source. *)
type span = {left : position; right : position}

(** [make string] is a new source containing some [string]. *)
val make : string -> t

(** [from_file path] is a new source loaded from a file at some given [path].
    This function raises a [Sys_error] exception if it fails to open the file at
    the specified [path]. *)
val from_file : string -> t

(** [length source] is the length of a [source] in bytes. *)
val length : t -> int

(** [at source offset] is the character at some [offset] in a [source], or
    [None] if the [offset] is out of bounds. *)
val at : t -> int -> char option

(** [read source span] is the contents of a [source] covered by some [span]. *)
val read : t -> span -> string
