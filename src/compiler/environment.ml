(* ----- Compiler environment ------------------------------------------------------------------- *)

include Map.Make (String)

let of_list list = list |> List.to_seq |> of_seq
