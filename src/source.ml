(* ----- Source, position and span ------------------------------------------ *)

type t = {path : string option; content : string}

type position = {line : int; column : int; offset : int}

type span = {left : position; right : position}

(* ----- Source manipulation functions -------------------------------------- *)

let make string =
  {path = None; content = string}

let from_file path =
  let file = In_channel.open_text path in
  let content = In_channel.input_all file in
  In_channel.close file;
  {path = Some path; content}

let length source =
  String.length source.content

let at source offset =
  try Some source.content.[offset]
  with Invalid_argument _ -> None

let read source span =
  let start = max 0 (min span.left.offset (length source)) in
  let length = (min span.right.offset (length source)) - start in
  String.sub source.content start length
